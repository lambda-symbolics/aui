(in-package #:aui)

;;;; -- Record Projection Tests --

(fiveam:def-suite records
  :description "Tests for turning the records and live events of Autolith into transcript entries."
  :in          aui)

(fiveam:in-suite records)

(-> test-item-record (integer string) list)
(defun test-item-record (sequence wire-json)
  "Return a durable provider item record.
The sequence number of the record is SEQUENCE, and the wire JSON is WIRE-JSON."
  (list :provider-item :seq sequence :time 4000000000 :wire-json wire-json))

(defmacro with-test-projector ((transcript projector) &body body)
  "Run BODY with a fresh transcript and projector."
  `(let* ((,transcript (make-transcript))
          (,projector (make-projector ,transcript)))
     ,@body))

(defparameter *test-history*
  (list (list :message :seq 1 :time 4000000000 :role :user :content "Fix the build.")
        (test-item-record 2 "{\"type\":\"reasoning\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"Look at the Makefile.\"}]}")
        (test-item-record 3 "{\"type\":\"function_call\",\"call_id\":\"call-a\",\"name\":\"shell_run\",\"arguments\":\"{\\\"command\\\":\\\"make\\\"}\"}")
        (list :tool-result :seq 4 :time 4000000001 :call-id "call-a" :tool "shell.run" :status :ok
                           :output "built" :real-microseconds 2500000)
        (test-item-record 5 "{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"It builds now.\"}]}")
        (list :message :seq 6 :time 4000000002 :role :user :automatic-p t
                       :content "x" :job-completion (list :outcome :failure :job-id "job-7" :summary "exit 2"))
        (list :summary :seq 7 :time 4000000003 :through-seq 6 :content "summary"))
  "A short durable history in the format written by Autolith.")

(fiveam:test history-becomes-entries-in-order
  (let* ((projector (make-projector (make-transcript)))
         (entries   (records->entries projector *test-history*)))
    (fiveam:is (equal '(user-entry reasoning-entry tool-entry assistant-entry notice-entry marker-entry)
                      (mapcar #'type-of entries)))
    (fiveam:is (string= "Fix the build." (user-entry-text (first entries))))
    (fiveam:is (string= "Look at the Makefile." (reasoning-entry-text (second entries))))
    (fiveam:is (string= "It builds now." (assistant-entry-text (fourth entries))))))

(fiveam:test a-result-settles-its-call
  (let* ((projector (make-projector (make-transcript)))
         (tool      (third (records->entries projector *test-history*))))
    (fiveam:is (eq ':completed (tool-entry-status tool)))
    (fiveam:is (string= "built" (tool-entry-output tool)))
    (fiveam:is (= 5/2 (tool-entry-seconds tool)))
    (fiveam:is (equal '(("command" . "make")) (tool-entry-arguments tool)))))

(fiveam:test a-failed-job-reads-as-a-failure
  (let* ((projector (make-projector (make-transcript)))
         (notice    (fifth (records->entries projector *test-history*))))
    (fiveam:is (eq ':failure (notice-entry-tone notice)))
    (fiveam:is (search "job-7" (notice-entry-text notice)))
    (fiveam:is (string= "exit 2" (notice-entry-detail notice)))))

(fiveam:test streamed-text-grows-one-answer
    (with-test-projector (transcript projector)
     (projector-apply projector '(:status :status :provider-request-started :details nil))
     (projector-apply projector '(:text :delta "Hello"))
     (projector-apply projector '(:text :delta ", world."))
     (fiveam:is (= 1 (transcript-length transcript)))
     (fiveam:is (string= "Hello, world." (assistant-entry-text (transcript-last transcript))))
     (fiveam:is (assistant-entry-streaming-p (transcript-last transcript)))
     (projector-apply projector '(:status :status :assistant-response-persisted
                                  :details (:text "Hello, world.")))
     (fiveam:is (= 1 (transcript-length transcript)))
     (fiveam:is (not (assistant-entry-streaming-p (transcript-last transcript))))))

(fiveam:test an-answer-that-does-not-stream-arrives-whole
    (with-test-projector (transcript projector)
     (projector-apply projector '(:status :status :assistant-response-persisted :details (:text "All at once.")))
     (fiveam:is (string= "All at once." (assistant-entry-text (transcript-last transcript))))))

(fiveam:test a-live-tool-call-runs-then-settles
    (with-test-projector (transcript projector)
     (projector-apply projector '(:text :delta "Checking."))
     (projector-apply projector '(:status :status :tool-call-started
                                  :details (:call-id "call-b" :tool "resource.read"
                                            :arguments "{\"uri\":\"workspace:README.md\"}")))
     (let ((tool (transcript-last transcript)))
       (fiveam:is (eq ':running (tool-entry-status tool)))
       (fiveam:is (equal '(("uri" . "workspace:README.md")) (tool-entry-arguments tool)))
       (projector-apply projector '(:status :status :tool-call-completed
                                    :details (:call-id "call-b" :tool "resource.read" :success-p nil
                                              :output "no such file")))
       (fiveam:is (eq ':failed (tool-entry-status tool)))
       (fiveam:is (string= "no such file" (tool-entry-output tool))))
    ;; Text that arrives after the call is a new answer below the call.
     (projector-apply projector '(:text :delta "It is missing."))
     (fiveam:is (= 3 (transcript-length transcript)))))

(fiveam:test live-records-do-not-repeat-the-turn
    (with-test-projector (transcript projector)
     (setf (projector-boundary projector) 4)
    ;; This record is already in the history.
     (projector-apply projector (list :record :record (first *test-history*)))
     (fiveam:is (zerop (transcript-length transcript)))
    ;; The answer in this record is shown from the events of the turn.
     (projector-apply projector (list :record :record (fifth *test-history*)))
     (fiveam:is (zerop (transcript-length transcript)))
    ;; A new message from the person is taken from the record.
     (projector-apply projector
                      (list :record :record (list :message :seq 9 :role :user :content "And the tests?")))
     (fiveam:is (string= "And the tests?" (user-entry-text (transcript-last transcript))))))

(fiveam:test catching-up-takes-records-until-the-next-request
    (with-test-projector (transcript projector)
     (setf (projector-catch-up-p projector) t)
     (projector-apply projector '(:text :delta "half an answer"))
     (fiveam:is (zerop (transcript-length transcript)))
     (projector-apply projector (list :record :record (fifth *test-history*)))
     (fiveam:is (string= "It builds now." (assistant-entry-text (transcript-last transcript))))
     (projector-apply projector '(:status :status :provider-request-started :details nil))
     (projector-apply projector '(:text :delta "live again"))
     (fiveam:is (string= "live again" (assistant-entry-text (transcript-last transcript))))))

(fiveam:test command-output-joins-under-its-form
    (with-test-projector (transcript projector)
     (projector-apply projector '(:work-started :kind :lisp))
     (projector-apply projector (list :present :text (format nil "* (status)~2%")))
     (projector-apply projector (list :present :text (format nil "autolith v1~%  model x~2%")))
     (projector-apply projector (list :present :text (format nil "⇒ NIL~2%")))
     (projector-apply projector '(:work-finished :kind :lisp :outcome :done))
     (fiveam:is (= 1 (transcript-length transcript)))
     (let ((output (transcript-last transcript)))
       (fiveam:is (string= "(status)" (output-entry-invocation output)))
       (fiveam:is (search "model x" (output-entry-text output)))
       (fiveam:is (null (search "⇒ NIL" (output-entry-text output)))))
     (projector-apply projector (list :present :text "settings"))
     (fiveam:is (= 2 (transcript-length transcript)))))

(fiveam:test a-form-value-is-kept
    (with-test-projector (transcript projector)
     (projector-apply projector (list :present :text (format nil "* (+ 1 2)~2%")))
     (projector-apply projector (list :present :text (format nil "⇒ 3~2%")))
     (fiveam:is (string= "⇒ 3" (output-entry-text (transcript-last transcript))))
     (projector-apply projector '(:work-finished :kind :lisp :outcome :done))
     (projector-apply projector (list :present :text (format nil "* (values)~2%")))
     (projector-apply projector (list :present :text (format nil "⇒ NIL~2%")))
     (fiveam:is (string= "⇒ NIL" (output-entry-text (transcript-last transcript))))))

(fiveam:test one-line-notices-read-as-notices
    (with-test-projector (transcript projector)
     (projector-apply projector (list :present :text (format nil "∙ continuing with job results~%")))
     (projector-apply projector (list :present :text (format nil "! provider request failed~%")))
     (let ((entries (coerce (transcript-entries transcript) 'list)))
       (fiveam:is (equal '(notice-entry notice-entry) (mapcar #'type-of entries)))
       (fiveam:is (string= "continuing with job results" (notice-entry-text (first entries))))
       (fiveam:is (eq ':quiet (notice-entry-tone (first entries))))
       (fiveam:is (eq ':failure (notice-entry-tone (second entries)))))
     (projector-apply projector (list :present :text (format nil "* (tasks)~2%")))
     (projector-apply projector (list :present :text (format nil "∙ no jobs~%")))
     (fiveam:is (= 3 (transcript-length transcript)))
     (fiveam:is (string= "∙ no jobs" (output-entry-text (transcript-last transcript))))))

(fiveam:test portable-values-read-back
  (let ((object (portable-object "name" "model" "value" nil "list" '(:array 1 2))))
    (fiveam:is (string= "model" (portable-get object "name")))
    (fiveam:is (null (portable-get object "value")))
    (fiveam:is (eq ':missing (portable-get object "absent" ':missing)))
    (fiveam:is (equal '(1 2) (portable-items (portable-get object "list"))))
    (fiveam:is (portable-true-p t))
    (fiveam:is (not (portable-true-p ':false)))))

(fiveam:test command-text-names-its-command
  (dolist (case '(("/model" nil nil)
                  ("(model)" "model" nil)
                  ("  (settings )" "settings" nil)
                  ("(model \"gpt-6\")" "model" t)
                  ("(open \"b-6\")" "open" t)
                  ("hello there" nil nil)
                  ("(" nil nil)))
    (destructuring-bind (text name arguments-p) case
      (multiple-value-bind (found found-arguments-p) (command-text-name text)
        (fiveam:is (equal name found) "~S was read as the command name ~S" text found)
        (fiveam:is (eq arguments-p found-arguments-p) "~S has the arguments flag ~S" text found-arguments-p)))))

(fiveam:test a-stopped-turn-is-not-a-failure
  (let* ((projector (make-projector (make-transcript)))
         (entries   (records->entries
                     projector
                     (list '(:turn-aborted :seq 1 :reason :cancelled
                             :message "#<APPLICATION-TURN-CANCELLED {1}>")
                           '(:turn-aborted :seq 2 :reason :application-error :message "the provider refused")))))
    (fiveam:is (equal '(notice-entry notice-entry) (mapcar #'type-of entries)))
    (fiveam:is (string= "stopped" (notice-entry-text (first entries))))
    (fiveam:is (eq ':quiet (notice-entry-tone (first entries))))
    (fiveam:is (null (notice-entry-detail (first entries))))
    (fiveam:is (eq ':failure (notice-entry-tone (second entries))))
    (fiveam:is (string= "the provider refused" (notice-entry-detail (second entries))))))

(fiveam:test structured-shell-details-and-legacy-output
  (let* ((details   '(:output-streams ((:stream :stdout :head "hello" :tail " world"
                                                :complete-p t))))
         (projector (make-projector (make-transcript))))
    (projector-apply projector
                     '(:status :status :tool-call-started
                       :details (:call-id "shell-1" :tool "shell.run" :arguments "{}")))
    (projector-apply projector
                     (list :status :status :tool-call-completed
                           :details (list :call-id "shell-1" :success-p t
                                           :output  "legacy"
                                           :details details)))
    (let ((tool (transcript-last (projector-transcript projector))))
      (flet ((result-text ()
               (paragraph-text (code-block-paragraph (first (entries--result-blocks tool (tool-entry-output tool)))))))
        (fiveam:is (string= "hello world" (result-text)))
        (tool-entry-settle tool :success-p t :output "legacy")
        (fiveam:is (string= "legacy" (result-text)))))))
(fiveam:test a-presented-reasoning-summary-is-a-thought-shown-once
    (with-test-projector (transcript projector)
     (projector-apply projector (list ':reasoning :delta "**Checking model limit defaults**"))
     (projector-apply projector
                      (list ':present
                            :text (format nil "◇ reasoning summary~% │ Checking model limit defaults")))
     (projector-apply projector
                      (list ':present
                            :text (format nil "◇ reasoning summary~% │ Reading the provider file")))
     (let ((entries (coerce (transcript-entries transcript) 'list)))
       (fiveam:is (equal '(reasoning-entry reasoning-entry) (mapcar #'type-of entries)))
       (fiveam:is (string= "Reading the provider file" (reasoning-entry-text (second entries)))))))

(fiveam:test a-notice-does-not-join-output-nobody-asked-for
    (with-test-projector (transcript projector)
     (projector-apply projector (list ':present :text "something the channel printed"))
     (projector-apply projector
                      (list ':present :text "! cache stalled · 153.0K / 191.2K input tokens cached"))
     (let ((entries (coerce (transcript-entries transcript) 'list)))
       (fiveam:is (equal '(output-entry notice-entry) (mapcar #'type-of entries)))
       (fiveam:is (search "cache stalled" (notice-entry-text (second entries)))))))

(fiveam:test side-status-diagnostics-match-live-and-history
  (let ((records (list
                  '(:side-status :seq 1 :time 4000000000 :source "prompt-cache"
                    :spans ((:failure . "! ") (:notice . "cache miss ")
                            (:code . "259.8K") (:dim . " cached tokens lost · ")
                            (:code . "0") (:dim . " / ") (:code . "278.7K")
                            (:dim . " input tokens cached") (:dim . "") (:dim . " · request prefix intact")))
                  '(:side-status :seq 2 :time 4000000001 :source "prompt-cache"
                    :spans ((:failure . "! ") (:notice . "cache stalled · ")
                            (:code . "153.0K") (:dim . " / ") (:code . "191.2K")
                            (:dim . " input tokens cached") (:dim . " · shared history grew 319.3K characters")
                            (:dim . " · request cache markers moved")))
                  (list :side-status :seq 3 :time 4000000002 :source "mission failed"
                        :spans (list (cons ':failure "■ mission failed")
                                     (cons ':dim (format nil "~%  No worker reported success."))))
                  '(:side-status :seq 4 :time 4000000003 :source "prompt-cache"
                    :spans ((:notice . "· Cache metrics are not available."))))))
    (with-test-projector (transcript projector)
      (let ((history (records->entries projector records)))
        (dolist (record records)
          (projector-apply projector (list :record :record record)))
        (fiveam:is (= 4 (transcript-length transcript)))
        (loop for saved in history for live across (transcript-entries transcript)
              do (fiveam:is (string= (notice-entry-text saved) (notice-entry-text live)))
                 (fiveam:is (equal (notice-entry-body saved) (notice-entry-body live)))
                 (fiveam:is (eql (notice-entry-tone saved) (notice-entry-tone live)))
                 (fiveam:is (= (entry-time saved) (entry-time live))))
        (destructuring-bind (miss stalled mission fallback) history
          (fiveam:is (eq :warning (notice-entry-tone miss)))
          (fiveam:is (search "259.8K cached tokens lost" (notice-entry-body miss)))
          (fiveam:is (search "0 / 278.7K input tokens cached" (notice-entry-body miss)))
          (fiveam:is (search "request prefix intact" (notice-entry-body miss)))
          (fiveam:is (search "shared history grew 319.3K characters" (notice-entry-body stalled)))
          (fiveam:is (search "request cache markers moved" (notice-entry-body stalled)))
          (fiveam:is (eq :failure (notice-entry-tone mission)))
          (fiveam:is (string= "No worker reported success." (notice-entry-body mission)))
          (fiveam:is (search "metrics are not available" (notice-entry-text fallback))))
        (setf (projector-boundary projector) 4)
        (projector-apply projector (list :record :record (first records)))
        (fiveam:is (= 4 (transcript-length transcript)))))))

(fiveam:test output-that-begins-like-a-notice-keeps-its-command
  (with-test-projector (transcript projector)
    (setf (projector-invocation projector) "/cache")
    (projector-apply projector '(:present :text "! cache stalled · example output"))
    (let ((entry (transcript-entry transcript 0)))
      (fiveam:is (string= "/cache" (output-entry-invocation entry)))
      (fiveam:is (search "example output" (output-entry-text entry))))))
(fiveam:test a-wrapped-reasoning-summary-is-unwrapped
  (fiveam:is (string= (format nil "Planning the deadline capture~2%Checking Lisp apropos")
                      (projector--reasoning-summary
                       (format nil "◇ reasoning summary~%  │ Planning the deadline~%  │ capture~%  │~%  │ Checking Lisp apropos")))))

(fiveam:test a-notice-keeps-the-calls-it-lists-behind-its-line
    (with-test-projector (transcript projector)
     (projector-apply projector
                      (list ':present
                            :text "∙ 2 relevant memories exist · (memory \"w-1\") · (memory \"Q-2\")"))
     (let ((notice (first (coerce (transcript-entries transcript) 'list))))
       (fiveam:is (string= "2 relevant memories exist" (notice-entry-text notice)))
       (fiveam:is (string= (format nil "(memory \"w-1\")~%(memory \"Q-2\")") (notice-entry-detail notice))))))

(fiveam:test schema-lookups-that-follow-one-another-are-one-entry
  (let* ((projector (make-projector (make-transcript)))
         (entries   (records->entries
                     projector
                     (list (list :message :seq 1 :time 4000000000 :role :user :content "Look around.")
                           (test-item-record 2 "{\"type\":\"tool_search_output\",\"tools\":[{\"type\":\"namespace\",\"name\":\"shell\"}]}")
                           (test-item-record 3 "{\"type\":\"tool_search_output\",\"tools\":[{\"type\":\"namespace\",\"name\":\"lisp\"},{\"type\":\"namespace\",\"name\":\"shell\"}]}")
                           (test-item-record 4 "{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"Looked.\"}]}")
                           (test-item-record 5 "{\"type\":\"tool_search_output\",\"tools\":[{\"type\":\"namespace\",\"name\":\"plan\"}]}")))))
    (fiveam:is (equal '(user-entry schemas-entry assistant-entry schemas-entry) (mapcar #'type-of entries)))
    (fiveam:is (equal '("shell" "lisp") (schemas-entry-names (second entries))))
    (fiveam:is (equal '("plan") (schemas-entry-names (fourth entries))))))

(fiveam:test a-live-schema-lookup-is-taken-from-its-record
    (with-test-projector (transcript projector)
     (projector-apply projector (list ':status :status ':provider-request-started))
     (dolist (name '("shell" "lisp"))
       (projector-apply projector
                        (list ':record
                              :record (test-item-record 9 (format nil "{\"type\":\"tool_search_output\",\"tools\":[{\"type\":\"namespace\",\"name\":\"~A\"}]}" name)))))
     (projector-apply projector
                      (list ':record
                            :record (test-item-record 10 "{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"Done.\"}]}")))
     (let ((entries (coerce (transcript-entries transcript) 'list)))
       (fiveam:is (equal '(schemas-entry) (mapcar #'type-of entries)))
       (fiveam:is (equal '("shell" "lisp") (schemas-entry-names (first entries)))))))

(fiveam:test a-message-from-another-channel-keeps-where-it-was-written
  (flet ((message (source)
           (list :message :seq 1 :time 4000000000 :role :user :content "x"
                          :operation-request (list :operation "channel"
                                                   :summary   "The Makefile reads well."
                                                   :message   (list :sender-name "review"
                                                                    :source      source
                                                                    :target      "B2Qweni"
                                                                    :kind        ':direct)))))
    (let ((projector (make-projector (make-transcript))))
      (setf (projector-channel projector) "B2Qweni")
      (let ((from-child (first (records->entries projector (list (message "C4puSYz")))))
            (from-here  (first (records->entries projector (list (message "B2Qweni"))))))
        (fiveam:is (string= "review" (user-entry-sender from-child)))
        (fiveam:is (string= "The Makefile reads well." (user-entry-text from-child)))
        (fiveam:is (equal "C4puSYz" (user-entry-origin from-child)))
        (fiveam:is (null (user-entry-origin from-here)))))))

(fiveam:test select-lisp-parenthesis-highlighting
  (fiveam:is (highlight--lisp-p "lisp" nil))
  (fiveam:is (highlight--lisp-p nil "src/view/window.lisp"))
  (fiveam:is (not (highlight--lisp-p "bash" "src/view/window.lisp"))))

(fiveam:test a-goal-record-states-the-goal-or-its-clearing
  (let ((goal (record-goal (list :goal :seq 9 :objective "Ship it." :status :paused :continuations 2
                                       :created-at 4000000000))))
    (fiveam:is (equal "Ship it." (getf goal :objective)))
    (fiveam:is (eq ':paused (getf goal :status)))
    (fiveam:is (eq ':goal (getf goal :kind)))
    (fiveam:is (eql 4000000000 (getf goal :created-at))))
  (fiveam:is (eq ':mission
                 (getf (record-goal (list :goal :seq 3 :kind :mission :objective "Port it." :status :active))
                       :kind)))
  ;; The record that Autolith writes for a cleared goal contains no goal.
  (fiveam:is (null (record-goal (list :goal :seq 10 :objective nil :status :cleared :continuations 0
                                            :created-at nil))))
  (fiveam:is (null (record-goal (list :goal :seq 11 :objective "" :status :active))))
  (fiveam:is (null (record-goal (list :goal :seq 12 :objective "Ship it." :status 7)))))

(fiveam:test the-newest-goal-record-is-the-one-with-the-highest-sequence
  (let ((early (list :goal :seq 4 :objective "First." :status :active))
        (late  (list :goal :seq 30 :objective "Second." :status :active))
        (other (list :message :seq 40 :role :user :content "x")))
    (fiveam:is (eq late (records-newest-goal (list early other late) nil)))
    ;; The newer goal is kept when a page of older history is read.
    (fiveam:is (eq late (records-newest-goal (list early other) late)))
    (fiveam:is (eq late (records-newest-goal (list late) early)))
    (fiveam:is (null (records-newest-goal (list other nil) nil)))))

(fiveam:test a-state-names-its-goal
  (let ((goal (portable-goal (portable-object "objective" "Ship it." "status" "active" "kind" "mission"
                                              "createdAt" 4000000000 "continuations" 1))))
    (fiveam:is (equal "Ship it." (getf goal :objective)))
    (fiveam:is (eq ':active (getf goal :status)))
    (fiveam:is (eq ':mission (getf goal :kind))))
  (fiveam:is (null (portable-goal ':null)))
  (fiveam:is (null (portable-goal nil)))
  ;; A state that has a goal member set to nil is different from a state without a goal member.
  (fiveam:is (portable-member-p (portable-object "goal" nil) "goal"))
  (fiveam:is (not (portable-member-p (portable-object "title" "x") "goal")))
  (fiveam:is (not (portable-member-p nil "goal"))))

(fiveam:test channels-beneath-a-channel-are-listed-under-their-parents
  (let ((rows (list (list :id "a" :parent "home")
                    (list :id "c" :parent "b" :title "grandchild")
                    (list :id "b" :parent "a" :title "child")
                    (list :id "d" :parent "a")
                    (list :id "e" :parent "other"))))
    (fiveam:is (equal '(("b" . 0) ("c" . 1) ("d" . 0))
                      (mapcar (lambda (row)
                                (cons (getf row :id) (getf row :depth)))
                              (channel-descendants "a" rows))))
    (fiveam:is (equal "grandchild" (getf (second (channel-descendants "a" rows)) :title)))
    (fiveam:is (null (channel-descendants "c" rows)))
    (fiveam:is (null (channel-descendants nil rows))))
  ;; Channels that are parents of each other are listed once.
  (fiveam:is (equal '("y")
                    (mapcar (lambda (row)
                              (getf row :id))
                            (channel-descendants "x" (list (list :id "x" :parent "y")
                                                           (list :id "y" :parent "x"))))))
  ;; Working channels are listed first among siblings.
  ;; Channels that are absent from the server are listed last.
  (fiveam:is (equal '("busy" "under-busy" "waits" "rests" "gone")
                    (mapcar (lambda (row)
                              (getf row :id))
                            (channel-descendants
                             "top"
                             (list (list :id "gone" :parent "top" :state ':absent)
                                   (list :id "rests" :parent "top" :state ':idle)
                                   (list :id "waits" :parent "top" :state ':idle :waiting-for-jobs-p t)
                                   (list :id "under-busy" :parent "busy" :state ':absent)
                                   (list :id "busy" :parent "top" :state ':working))))))
  (fiveam:is (channel-row-working-p (list :id "a" :state ':working)))
  (fiveam:is (not (channel-row-working-p (list :id "a" :state ':idle)))))

(fiveam:test a-result-read-before-its-call-settles-it-on-the-page-before
  (let* ((projector (make-projector (make-transcript)))
         (newer     (records->entries projector (nthcdr 3 *test-history*)))
         (older     (records->entries projector (subseq *test-history* 0 3))))
    (fiveam:is (equal '(assistant-entry notice-entry marker-entry) (mapcar #'type-of newer)))
    (fiveam:is (eq ':completed (tool-entry-status (third older))))
    (fiveam:is (string= "built" (tool-entry-output (third older))))
    (fiveam:is (null (projector--unsettled projector)))))

(fiveam:test an-answer-a-notice-parted-is-not-repeated-when-it-persists
    (with-test-projector (transcript projector)
     (projector-apply projector '(:status :status :provider-request-started :details nil))
     (projector-apply projector '(:text :delta "First part. "))
     (projector-apply projector '(:present :text "∙ a memory was saved"))
     (projector-apply projector '(:text :delta "Second part."))
     (projector-apply projector '(:status :status :assistant-response-persisted
                                  :details (:text "First part. Second part.")))
     (fiveam:is (equal '(assistant-entry notice-entry assistant-entry)
                       (map 'list #'type-of (transcript-entries transcript))))
     (fiveam:is (string= "Second part." (assistant-entry-text (transcript-last transcript))))))

(fiveam:test a-compaction-told-twice-is-marked-once
    (with-test-projector (transcript projector)
     (projector-apply projector '(:status :status :compaction-completed :details nil))
     (projector-apply projector (list :record :record (list :summary :seq 9 :time 4000000003 :through-seq 6
                                                                     :content "summary")))
     (fiveam:is (equal '(marker-entry) (map 'list #'type-of (transcript-entries transcript))))))

(fiveam:test edit-history-reconstructs-an-anchored-multiple-edit
  (let* ((uri     "workspace:sample")
         (header  (format nil "URI: ~A~%Revision: R1~%Visible lines: 4-6 of 20~%Elided: none~%Content:~%" uri))
         (body    (format nil "~6D:~A  old~%~6D:~A  ~%~6D:~A    next" 4 (cl-hashline:line-anchor "old")
                          5 (cl-hashline:line-anchor "") 6 (cl-hashline:line-anchor "  next")))
         (read    (make-instance 'tool-entry :name "resource.read" :status ':completed :output (concatenate 'string header body)))
         (edit    (make-instance 'tool-entry :name "resource.edit" :status ':completed
                                 :output    (format nil "Applied replace-lines 4-5; delete-lines 6-6.~%URI: ~A~%Revision: R2~%" uri)
                                 :arguments `(("base-revision" . "R1")
                                              ("operations" (("op" . "delete-lines") ("start-line" . 6) ("end-line" . 6))
                                                            (("op" . "replace-lines") ("start-line" . 3)
                                                             ("start-anchor" . ,(cl-hashline:line-anchor "old"))
                                                             ("end-line" . 5) ("content" . "new"))))))
         (entries (vector read edit)))
    (fiveam:is (equal (format nil "--- ~A~%+++ ~A~%@@ -4,2 +4,1 @@~%-old~%-~%+new~%@@ -6,1 +4,0 @@~%-  next~%" uri uri)
                      (edit-history--reconstruct edit entries 1)))
    (setf (tool-entry-patch edit) nil)
    (flet ((unavailable () (null (edit-history--reconstruct edit entries 1))))
      (setf (tool-entry-status edit) ':failed)
      (fiveam:is (unavailable))
      (setf (tool-entry-status edit) ':completed (tool-entry-output read) (concatenate 'string header (subseq body 0 5)))
      (fiveam:is (unavailable))
      (setf (tool-entry-output read) (concatenate 'string (substitute #\2 #\1 header) body))
      (fiveam:is (unavailable))
      (setf (tool-entry-output read) (concatenate 'string (substitute #\z #\w header) body))
      (fiveam:is (unavailable)))))
