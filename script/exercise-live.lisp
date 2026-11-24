;;;; -- Live exercise --

;;; Drive real turns through a temporary world and scripted model.
;;;
;;; Usage: script/exercise-live

(load (merge-pathnames "exercise-support.lisp" *load-truename*))

(in-package #:aui)


;;;; -- A Scripted Model --

(defparameter *mock-title*
  "{\"title\": \"A short title\", \"agent_name\": \"bridge-builder\"}"
  "Mock channel title response.")

(defparameter *mock-answer*
  "## What I found

The bridge builds on both systems now. Three things changed:

1. The **Makefile** asks `fltk-config` for its flags.
2. Fonts are found through *CoreText* on a Mac.
3. The smoke tests run in CI.

```lisp
(defun bridge-library-name ()
  \"Return the file name of the bridge.\"
  #+darwin \"liblightfast.dylib\"
  #-darwin \"liblightfast.so\")
```

| System | Library | Fonts |
|--------|---------|-------|
| Linux | `.so` | Pango |
| macOS | `.dylib` | CoreText |

That is all of it.
"
  "Mock Markdown response.")

(-> mock--octets (string) (simple-array (unsigned-byte 8) (*)))
(defun mock--octets (string)
  "Encode STRING as UTF-8."
  (sb-ext:string-to-octets string :external-format ':utf-8))

(-> mock--pieces (string integer) list)
(defun mock--pieces (text size)
  "Split TEXT into pieces of SIZE characters."
  (loop for start from 0 below (length text) by size
        collect (subseq text start (min (length text) (+ start size)))))

(-> mock--read-request (stream) (values string string))
(defun mock--read-request (stream)
  "Read an HTTP method and body from STREAM."
  (let ((header (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (loop for octet = (read-byte stream nil nil)
          while octet
          do (vector-push-extend octet header)
          until (let ((length (length header)))
                  (and (>= length 4)
                       (= (aref header (- length 1)) 10)
                       (= (aref header (- length 2)) 13)
                       (= (aref header (- length 3)) 10)
                       (= (aref header (- length 4)) 13))))
    (let* ((text   (sb-ext:octets-to-string (coerce header '(simple-array (unsigned-byte 8) (*)))
                                            :external-format ':latin-1))
           (method (subseq text 0 (or (position #\Space text) 0)))
           (marker (search "content-length:" text :test #'char-equal))
           (length (if marker
                       (parse-integer text :start (+ marker 15) :junk-allowed t)
                       0))
           (body   (make-array (or length 0) :element-type '(unsigned-byte 8))))
      (read-sequence body stream)
      (values method (sb-ext:octets-to-string body :external-format ':utf-8)))))

(-> mock--send (stream string) null)
(defun mock--send (stream string)
  "Write STRING to STREAM."
  (write-sequence (mock--octets string) stream)
  (finish-output stream)
  nil)

(-> mock--send-json (stream t) null)
(defun mock--send-json (stream value)
  "Send JSON VALUE on STREAM."
  (let ((body (mock--octets (autolith::json-encode value))))
    (mock--send stream (format nil "HTTP/1.1 200 OK~C~CContent-Type: application/json~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
                               #\Return #\Newline #\Return #\Newline (length body)
                               #\Return #\Newline #\Return #\Newline #\Return #\Newline))
    (write-sequence body stream)
    (finish-output stream))
  nil)

(-> mock--send-event (stream string) null)
(defun mock--send-event (stream data)
  "Send DATA as one chunked server-sent event."
  (let ((octets (mock--octets (format nil "data: ~A~C~C" data #\Newline #\Newline))))
    (mock--send stream (format nil "~X~C~C" (length octets) #\Return #\Newline))
    (write-sequence octets stream)
    (mock--send stream (format nil "~C~C" #\Return #\Newline)))
  nil)

(-> mock--chunk (t &key (:finish (or null string))) string)
(defun mock--chunk (delta &key finish)
  "Build a streamed completion chunk for DELTA."
  (autolith::json-encode
   (autolith::json-object
    "id"      "chatcmpl-mock"
    "object"  "chat.completion.chunk"
    "model"   "mock-1"
    "choices" (vector (autolith::json-object "index"         0
                                             "delta"         delta
                                             "finish_reason" (or finish ':null))))))

(-> mock--text (t) string)
(defun mock--text (content)
  "Extract text from CONTENT."
  (cond
    ((stringp content)
     content)
    ((vectorp content)
     (format nil "~{~A~^ ~}"
             (loop for part across content
                   when (and (autolith::json-object-p part) (stringp (autolith::json-get part "text")))
                     collect (autolith::json-get part "text"))))
    (t
     "")))

(-> mock--spoken (t) list)
(defun mock--spoken (request)
  "Return request messages without temporary context."
  (let ((messages (autolith::json-get request "messages")))
    (remove-if (lambda (message)
                 (let ((text (mock--text (autolith::json-get message "content"))))
                   (and (>= (length text) 17) (string= "Temporary context" text :end2 17))))
               (coerce (if (vectorp messages) messages #()) 'list))))

(-> mock--stream (stream t) null)
(defun mock--stream (stream request)
  "Stream an answer to REQUEST on STREAM, as a model would.

Requests without tools come from Autolith itself, for a title or a
classification. They are answered with *MOCK-TITLE*. A tool result is answered
with a closing sentence. If a goal is active, the answer contains the marker for
a met goal. A message that contains the word shell is answered with a call of
the shell tool. Other messages are answered with a thought and *MOCK-ANSWER*.
The answer is streamed slowly if the message contains the word slowly."
  (let* ((tools     (let ((value (autolith::json-get request "tools")))
                      (if (vectorp value) (coerce value 'list) nil)))
         (spoken    (mock--spoken request))
         (last      (first (last spoken)))
         (last-user (mock--text
                     (autolith::json-get
                      (find "user" spoken
                            :key      (lambda (message)
                                        (autolith::json-get message "role"))
                            :test     #'equal
                            :from-end t)
                      "content")))
         (slow-p    (search "slowly" last-user :test #'char-equal))
         ;; If a goal is active, each request from Autolith contains the marker
         ;; for a met goal.
         (goal-p    (some (lambda (message)
                            (and (not (equal (autolith::json-get message "role") "assistant"))
                                 (search autolith::*application-goal-complete-marker*
                                         (mock--text (autolith::json-get message "content")))))
                          (let ((messages (autolith::json-get request "messages")))
                            (coerce (if (vectorp messages) messages #()) 'list)))))
    (flet ((say (text &key (member "content") (pause 0.02))
             (dolist (piece (mock--pieces text 14))
               (mock--send-event stream (mock--chunk (autolith::json-object member piece)))
               (sleep pause))))
      (mock--send stream (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CTransfer-Encoding: chunked~C~CConnection: close~C~C~C~C"
                                 #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                 #\Return #\Newline #\Return #\Newline))
      (mock--send-event stream (mock--chunk (autolith::json-object "role" "assistant" "content" "")))
      (cond
        ((null tools)
         (say *mock-title*)
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop")))
        ((equal (autolith::json-get last "role") "tool")
         (say "The command ran and printed what you see above.")
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop")))
        ;; A turn toward a goal. The answer contains the marker for a met goal.
        (goal-p
         (say (format nil "The bridge builds. ~A" autolith::*application-goal-complete-marker*))
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop")))
        ((search "shell" last-user :test #'char-equal)
         (let ((name (or (find-if (lambda (name)
                                    (and (stringp name) (search "shell" name)))
                                  (mapcar (lambda (tool)
                                            (autolith::json-get (autolith::json-get tool "function") "name"))
                                          tools))
                         "shell_run")))
           (say "I will run one command to look.")
           (mock--send-event
            stream
            (mock--chunk
             (autolith::json-object
              "tool_calls" (vector (autolith::json-object
                                    "index"    0
                                    "id"       "call_mock_1"
                                    "type"     "function"
                                    "function" (autolith::json-object
                                                "name"      name
                                                "arguments" "{\"command\": \"echo hello from the mock\"}"))))))
           (mock--send-event stream (mock--chunk (autolith::json-object) :finish "tool_calls"))))
        (t
         (say "The person asks about the bridge. I should say what changed and show it."
              :member "reasoning_content")
         (say *mock-answer* :pause (if slow-p 0.3 0.02))
         (mock--send-event stream (mock--chunk (autolith::json-object) :finish "stop"))))
      (mock--send-event
       stream
       (autolith::json-encode
        (autolith::json-object
         "id"      "chatcmpl-mock"
         "object"  "chat.completion.chunk"
         "model"   "mock-1"
         "choices" (vector)
         "usage"   (autolith::json-object "prompt_tokens" 1200 "completion_tokens" 180 "total_tokens" 1380))))
      (mock--send-event stream "[DONE]")
      (mock--send stream (format nil "0~C~C~C~C" #\Return #\Newline #\Return #\Newline))))
  nil)

(-> mock--answer (stream) null)
(defun mock--answer (stream)
  "Answer the request on STREAM."
  (multiple-value-bind (method body) (mock--read-request stream)
    (cond
      ((string= method "GET")
       (mock--send-json stream
                        (autolith::json-object
                         "object" "list"
                         "data"   (vector (autolith::json-object "id" "mock-1" "object" "model")))))
      (t
       (let ((request (autolith::json-decode body)))
         (if (eq (autolith::json-get request "stream") t)
             (mock--stream stream request)
             (mock--send-json
              stream
              (autolith::json-object
               "id"      "chatcmpl-mock"
               "object"  "chat.completion"
               "model"   "mock-1"
               "choices" (vector (autolith::json-object
                                  "index"         0
                                  "finish_reason" "stop"
                                  "message"       (autolith::json-object "role"    "assistant"
                                                                         "content" *mock-title*)))
               "usage"   (autolith::json-object "prompt_tokens" 10 "completion_tokens" 3 "total_tokens" 13))))))))
  nil)

(-> mock-start () integer)
(defun mock-start ()
  "Start the mock model on a free local port. Returns the port.

Each request is answered on a separate thread. If a client disconnects in the
middle of an answer, only that thread ends."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 16)
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((client (sb-bsd-sockets:socket-accept socket)))
           (sb-thread:make-thread
            (lambda ()
              (let ((stream (sb-bsd-sockets:socket-make-stream client
                                                               :input        t
                                                               :output       t
                                                               :element-type '(unsigned-byte 8)
                                                               :buffering    ':full)))
                (unwind-protect
                     (handler-case (mock--answer stream)
                       (error ()
                         nil))
                  (ignore-errors (close stream :abort t)))))
            :name "mock model answer"))))
     :name "mock model")
    (nth-value 1 (sb-bsd-sockets:socket-name socket))))


;;;; -- A Separate World --

(-> exercise-live--world-root () string)
(defun exercise-live--world-root ()
  "Return the directory of the temporary world from AUI_EXERCISE_WORLD.

Exit if the variable is empty. Also exit if a directory for the state or the
logins of Autolith is outside the world directory. The world of a person must
not be changed by the exercise."
  (let ((root (uiop:getenv "AUI_EXERCISE_WORLD")))
    (unless (and root
                 (plusp (length root))
                 (every (lambda (name)
                          (let ((value (uiop:getenv name)))
                            (and value (uiop:string-prefix-p root value))))
                        '("XDG_DATA_HOME" "XDG_STATE_HOME" "XDG_CONFIG_HOME" "CODEX_HOME" "GROK_HOME")))
      (format t "~&Run script/exercise-live. The exercise needs the separate world created by that script.~%")
      (finish-output)
      (sb-ext:exit :code 64 :abort t))
    root))

(-> exercise-live--prepare (string integer) null)
(defun exercise-live--prepare (root port)
  "Register the mock model on PORT as the only provider of the world under ROOT. The API key of the provider is fake."
  (let* ((configuration (autolith::configuration-create :immutable-p t :defer-provider-validation-p t))
         (init          (merge-pathnames "init.lisp" (autolith::config :config-root configuration)))
         (keys          (autolith::configuration-api-keys-path configuration)))
    (unless (and (uiop:string-prefix-p root (namestring init))
                 (uiop:string-prefix-p root (namestring keys)))
      (format t "~&The configuration of Autolith is not under ~A. It is left as it is.~%" root)
      (finish-output)
      (sb-ext:exit :code 64 :abort t))
    ;; script/exercise-live uses this file to stop the server if this process dies first.
    (with-open-file (stream (format nil "~A/autolith-program" root) :direction ':output :if-exists ':supersede)
      (write-line (installation-program) stream))
    (ensure-directories-exist init)
    (with-open-file (stream init :direction ':output :if-exists ':supersede)
      (format stream "(register-openai-compatible-provider~% :name        \"mock\"~% :description \"The live exercise's model\"~% :endpoint    \"http://127.0.0.1:~D/v1/chat/completions\"~% :models      '(\"mock-1\"))~%"
              port))
    (ensure-directories-exist keys)
    (autolith::api-key-credential-manager-save-key
     (autolith::api-key-credential-manager-create :provider-name "mock" :pathname keys)
     "a-key-that-opens-nothing"))
  nil)


;;;; -- The Exercise --

(-> exercise-live--until (workbench function &key (:seconds real)) boolean)
(defun exercise-live--until (workbench predicate &key (seconds 60))
  "Apply the events from the channel until PREDICATE is true. Returns T if PREDICATE became true within SECONDS."
  (let ((deadline (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second)))))
    (loop
      (when (funcall predicate)
        (return t))
      (when (> (get-internal-real-time) deadline)
        (return nil))
      (main--settle workbench 0.2))))

(-> exercise-live--type (workbench string integer) null)
(defun exercise-live--type (workbench text code)
  "Put TEXT in the composer and press the key with the code CODE in the composer."
  (setf (composer-text workbench) text)
  (window--editor-key workbench (make-key-event :code code))
  nil)

(-> exercise-live--entries (workbench symbol) list)
(defun exercise-live--entries (workbench class)
  "Return the entries of CLASS in the transcript, oldest first."
  (remove-if-not (lambda (entry)
                   (typep entry class))
                 (coerce (transcript-entries (workbench-transcript workbench)) 'list)))

(-> exercise-live--picture (workbench string) null)
(defun exercise-live--picture (workbench name)
  "Write a picture of the window as NAME.png into the directory from AUI_EXERCISE_SHOTS, if the variable is set."
  (let ((directory (uiop:getenv "AUI_EXERCISE_SHOTS")))
    (when (and directory (plusp (length directory)))
      (window-arrange workbench)
      (save-window-picture
       (workbench-window workbench)
       (uiop:native-namestring
        (merge-pathnames (format nil "~A.png" name)
                         (uiop:ensure-directory-pathname directory))))))
  nil)

(-> exercise-live--idle-p (workbench) boolean)
(defun exercise-live--idle-p (workbench)
  "Return T if the channel is not working and has no pending approval or pending messages."
  (and (not (workbench-working-p workbench))
       (null (workbench-approval workbench))
       (null (workbench-pending workbench))))

(-> exercise-live--goal-met-p (workbench) boolean)
(defun exercise-live--goal-met-p (workbench)
  "Check the terminal status of the mock goal on mission and legacy servers."
  (let ((goal (workbench-goal workbench)))
    (eq (getf goal :status) (if (eq (getf goal :kind) ':mission) ':verified ':complete))))

(-> exercise-live--current-p (workbench) boolean)
(defun exercise-live--current-p (workbench)
  "Check that the attached channel has a selected sidebar row."
  (and (workbench-attached-p workbench)
       (some (lambda (row)
               (and (equal (workbench-channel workbench) (getf row :id)) (getf row :current-p)))
             (sidebar-entries workbench))))

(-> exercise-live--kill-server (workbench string) null)
(defun exercise-live--kill-server (workbench root)
  "Kill the mock server after locating its process lease inside ROOT."
  (let* ((world (workbench-world workbench))
         (pid   (getf (nthcdr 2 (world--entry world)) :pid))
         (lease (autolith::world-server-lock-pathname (world-configuration world))))
    (assert (and (integerp pid) (> pid 1)
                 (uiop:string-prefix-p (format nil "~A/" root) (namestring (truename lease)))))
    (uiop:run-program (list "kill" "-KILL" (princ-to-string pid))))
  nil)

(-> exercise-live-run () integer)
(defun exercise-live-run ()
  "Exercise live turns against the mock model. Returns the number of failed expectations."
  (let ((root (exercise-live--world-root)))
    (exercise-live--prepare root (mock-start))
    (lightfast:load-library)
    (lightfast:enable-thread-wakeups)
    (theme-install)
    (let* ((directory                (uiop:native-namestring
                                      (ensure-directories-exist
                                       (uiop:ensure-directory-pathname (format nil "~A/work/bridge" root)))))
           (launch                   (launch-parse (list "-C" directory "--model" "mock/mock-1"
                                                         "--size" "1200x800" "--shot" "unused.png")))
           (workbench                (make-instance 'workbench
                                                    :launch launch
                                                    :world  (launch-world launch)
                                                    :queue  (make-ui-queue :wakeup #'lightfast:awake)))
           (*stage-failure-function* (lambda (report)
                                       (incf *exercise-failures*)
                                       (format t "~&  FAIL  ~A~%" report))))
      (workbench--reset-channel workbench)
      (window-build workbench)
      (main--start workbench)
      (window-show workbench)
      (unwind-protect
           (progn
             ;; The world server is started by the first open. The start can take a long time.
             (let ((command-p nil))
               (exercise-expect "a world server is started and a channel is opened on the first start"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (when (equal (second (workbench-progress workbench))
                                                                     "starting the world server")
                                                          (setf command-p t))
                                                        (workbench-attached-p workbench))
                                                      :seconds 600))
               (exercise-expect "the world server was started by the autolith command" command-p))
             (main--settle workbench 1)
             (exercise-expect "the model of the channel is the model given on the command line"
                              (search "mock-1" (or (workbench-model workbench) "")))
             ;; A turn that is watched while it streams.
             (exercise-live--type workbench "Tell me about the bridge, slowly" #xff0d)
             (exercise-expect "pressing Enter sends the message, and the agent starts to work"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (workbench-working-p workbench))))
             (exercise-expect "the answer is streamed while the agent works"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (and (workbench-working-p workbench)
                                      (some (lambda (entry)
                                              (search "What I found" (assistant-entry-text entry)))
                                            (exercise-live--entries workbench 'assistant-entry))))))
             (exercise-expect "the reasoning of the model is shown as a thought"
                              (exercise-live--entries workbench 'reasoning-entry))
             (exercise-live--picture workbench "live-streaming")
             ;; A follow-up message queued while the agent works.
             (exercise-live--type workbench "Now use a shell tool to look around" #xff09)
             (main--settle workbench 0.5)
             (exercise-expect "pressing Tab queues a follow-up message"
                              (find "Now use a shell tool to look around" (workbench-pending workbench)
                                    :key #'second :test #'equal))
             (exercise-expect "the waiting messages are read from the queue of the channel"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (workbench-poll-waiting workbench :now-p t)
                                 (find "Now use a shell tool to look around" (workbench--waiting workbench)
                                       :key #'first :test #'equal))
                               :seconds 20))
             ;; The follow-up message leads to a command. The command needs an approval.
             (exercise-expect "an approval is requested before the command runs"
                              (and (exercise-live--until workbench
                                                         (lambda ()
                                                           (workbench-approval workbench))
                                                         :seconds 120)
                                   (progn
                                     (exercise--draw workbench)
                                     (exercise--region workbench '(:approval "once")))))
             (exercise-live--picture workbench "live-approval")
             (exercise--click workbench '(:approval "once"))
             (exercise-expect "after an approval for one run, the command runs and the turn ends"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (exercise-live--idle-p workbench))
                                                    :seconds 120))
             (main--settle workbench 1)
             (let ((tool (first (exercise-live--entries workbench 'tool-entry))))
               (exercise-expect "the tool call contains the output of the command"
                                (and tool
                                     (search "hello from the mock" (princ-to-string (tool-entry-output tool)))))
               (when tool
                 (setf (entry-expanded-p tool) t)
                 (workbench-redraw workbench)
                 (exercise-live--picture workbench "live-tool")
                 (setf (entry-expanded-p tool) nil)))
             ;; Stopping a turn in the middle.
             (exercise-live--type workbench "Tell me again, slowly" #xff0d)
             (exercise-live--until workbench
                                   (lambda ()
                                     (and (workbench-working-p workbench)
                                          (search "What I found"
                                                  (assistant-entry-text
                                                   (first (last (exercise-live--entries workbench
                                                                                        'assistant-entry))))))))
             (window--editor-key workbench (make-key-event :code #xff1b))
             (exercise-expect "pressing Escape stops the turn"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (not (workbench-working-p workbench)))
                                                    :seconds 60))
             (main--settle workbench 1)
             (exercise-expect "a quiet notice is shown after a stopped turn"
                              (let ((notice (first (last (exercise-live--entries workbench 'notice-entry)))))
                                (and notice
                                     (string= "stopped" (notice-entry-text notice))
                                     (eq ':quiet (notice-entry-tone notice)))))
             ;; Steering a running turn.
             (exercise-live--type workbench "Tell me once more, slowly" #xff0d)
             (exercise-live--until workbench
                                   (lambda ()
                                     (workbench-working-p workbench)))
             (main--settle workbench 1)
             (exercise-live--type workbench "Also say which fonts" #xff0d)
             (main--settle workbench 0.5)
             (exercise-expect "pressing Enter while the agent works sends the message as steering"
                              (third (find "Also say which fonts" (workbench-pending workbench)
                                           :key #'second :test #'equal)))
             (exercise-expect "the agent takes up the steering message"
                              (and (exercise-live--until workbench
                                                         (lambda ()
                                                           (exercise-live--idle-p workbench))
                                                         :seconds 180)
                                   (find "Also say which fonts" (exercise-live--entries workbench 'user-entry)
                                         :key #'user-entry-text :test #'equal)))
             ;; A command.
             (exercise-live--type workbench "(status)" #xff0d)
             (exercise-expect "the output of a command is printed in the conversation under the name of the command"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (some (lambda (entry)
                                         (and (search "mock-1" (output-entry-text entry))
                                              (equal "(status)" (output-entry-invocation entry))))
                                       (exercise-live--entries workbench 'output-entry)))))
             (exercise-expect "the channel has the title given by the model"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (search "short title" (workbench-title workbench)))))
             (exercise-expect "the channel list contains the agent name given by the model"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (equal "bridge-builder"
                                        (getf (find (workbench-channel workbench)
                                                    (world-entries (workbench-world workbench))
                                                    :key  (lambda (entry)
                                                            (getf entry :id))
                                                    :test #'equal)
                                              :name)))))
             ;; The chips under the composer are used to change the settings of the channel.
             (workbench-set-setting workbench "reasoning-effort" "low")
             (exercise-expect "the effort of the channel can be changed"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (equal "low" (workbench-effort workbench)))
                                                    :seconds 30))
             ;; Full access is used because it is available on all platforms.
             (workbench-set-mode workbench ':full-access)
             (exercise-expect "the permission mode of the channel can be changed"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (eq ':full-access (workbench-permission-mode workbench)))
                                                    :seconds 30))
             ;; Settings with keyword values on the server are set by the spelling of the value.
             (workbench-set-setting workbench "permission-mode" "auto")
             (exercise-expect "a setting with keyword values can be changed"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (let ((row (find "permission-mode" (workbench-settings workbench)
                                                  :key  (lambda (row)
                                                          (portable-get row "name"))
                                                  :test #'equal)))
                                   (and row (equal "auto" (portable-get row "value")))))
                               :seconds 30))
             ;; The folder of the channel can be changed by the person.
             (let ((folder (namestring (uiop:ensure-directory-pathname (format nil "~A/work/elsewhere" root)))))
               (ensure-directories-exist folder)
               (workbench-submit workbench (format nil "(cwd ~S)" folder))
               (exercise-expect "the folder of a channel can be changed, and the new folder is shown in the window"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (equal folder (workbench-directory workbench)))
                                                      :seconds 30)))
             ;; A goal is set in the channel and read by the window.
             ;; The goal is met if the agent says so.
             (workbench-submit workbench "(goal \"Keep the bridge building.\")")
             (exercise-expect "a goal set in the channel is read by the window"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (equal "Keep the bridge building." (getf (workbench-goal workbench) :objective)))
                               :seconds 60))
             (exercise-expect "the goal is complete after the agent says that it is met"
                              (exercise-live--until
                               workbench
                               (lambda ()
                                 (and (exercise-live--goal-met-p workbench)
                                      (exercise-live--idle-p workbench)))
                               :seconds 180))
             ;; From Autolith 0.67.57 on, the state of the channel contains the goal.
             (let ((state (workbench-state workbench)))
               (format t "~&  note  the goal is read from ~:[the newest goal record~;the state of the channel~]~%"
                       (portable-member-p state "goal"))
               (exercise-expect "if the state contains a goal, it is the goal shown in the window"
                                (or (not (portable-member-p state "goal"))
                                    (equal "Keep the bridge building."
                                           (portable-get (portable-get state "goal") "objective")))))
             (stage-resize (workbench-stage workbench) 1500 900)
             (exercise--draw workbench)
             (exercise-expect "the completed goal can be cleared, but only a legacy goal can be edited"
                              (and (exercise--region workbench '(:console-section :goal))
                                  (eq (null (exercise--region workbench '(:console-goal :update)))
                                      (eq (getf (workbench-goal workbench) :kind) ':mission))
                                  (exercise--region workbench '(:console-goal :clear))))
             (exercise-live--picture workbench "live-goal")
             (stage-resize (workbench-stage workbench) 1200 800)
             (exercise--draw workbench)
             ;; The same channel is opened again from its history.
             (let ((channel (workbench-channel workbench)))
               (workbench-open workbench :channel channel)
               (exercise-expect "the channel is reopened from its history"
                                (and (exercise-live--until workbench
                                                           (lambda ()
                                                             (workbench-attached-p workbench))
                                                           :seconds 120)
                                     (progn
                                       (main--settle workbench 1.5)
                                       (equal channel (workbench-channel workbench)))))
               (exercise-expect "the history contains the messages, the tool call and the output of the call"
                                (and (>= (length (exercise-live--entries workbench 'user-entry)) 4)
                                     (let ((tool (first (exercise-live--entries workbench 'tool-entry))))
                                       (and tool
                                            (search "hello from the mock"
                                                    (princ-to-string (tool-entry-output tool)))))))
               (exercise-live--picture workbench "live-history")
               (exercise-expect "the goal is read from the history of the channel"
                                (and (equal "Keep the bridge building."
                                            (getf (workbench-goal workbench) :objective))
                                     (exercise-live--goal-met-p workbench)))
               (workbench-goal-request workbench ':clear)
               (exercise-expect "a goal cleared from the window is removed"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (null (workbench-goal workbench)))
                                                      :seconds 30)))
             (exercise-expect "no failure was reported" (null (workbench-notice workbench)))
             (let ((channel (workbench-channel workbench)))
               (workbench-stop-server workbench)
               (exercise-expect "the attachment closes when the server stops"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (and (eq (workbench-attachment workbench) ':closed)
                                                             (not (world-served-p (workbench-world workbench)))))
                                                      :seconds 90))
               (exercise--draw workbench)
               (exercise-live--picture workbench "live-disconnected")
               (exercise-expect "the disconnect report can be copied before retrying"
                                (exercise--click workbench ':copy-error))
               (exercise-expect "Try again is available after shutdown" (exercise--click workbench ':retry))
               (exercise-expect "the reopened channel is current in the sidebar after restart"
                                (exercise-live--until
                                 workbench
                                 (lambda ()
                                  (and (equal (workbench-channel workbench) channel)
                                       (exercise-live--current-p workbench)))
                                 :seconds 120))
               (exercise-live--picture workbench "live-restart"))
             ;; Open another channel through the server, without changing this window's session.
             (let* ((world (workbench-world workbench))
                    (peer  (autolith::world-client-open (world-configuration world) :directory directory
                                                        :new-p t :attach-expected-p nil)))
               (exercise-expect "a channel opened elsewhere appears beside the current channel after reconnect"
                                (exercise-live--until workbench
                                                      (lambda ()
                                                        (and (exercise-live--current-p workbench)
                                                             (find peer (sidebar-entries workbench) :test #'equal
                                                                   :key                                   (lambda (row) (getf row :id)))))
                                                      :seconds 60)))
             ;; Deleting the channel closes it and removes it from the world.
             (let ((channel (workbench-channel workbench)))
               (workbench-delete-channel workbench channel)
               (exercise-expect "a channel can be deleted"
                                (exercise-live--until
                                 workbench
                                 (lambda ()
                                   (and (null (workbench-channel workbench))
                                        (null (workbench-notice workbench))
                                        (not (find channel (world-entries (workbench-world workbench))
                                                   :key  (lambda (entry)
                                                           (getf entry :id))
                                                   :test #'equal))))
                                 :seconds 60)))
             (workbench-open workbench :directory directory :new-p t)
             (exercise-expect "a new channel is selected before its first turn"
                              (exercise-live--until workbench (lambda () (exercise-live--current-p workbench))
                                                    :seconds 90))
             (let ((channel (workbench-channel workbench)))
               (exercise-live--kill-server workbench root)
               (exercise-expect "the unsaved channel closes when the server dies"
                                (exercise-live--until workbench (lambda () (eq (workbench-attachment workbench) ':closed))
                                                      :seconds 90))
               (exercise--draw workbench)
               (exercise-expect "Try again opens a fresh channel in the same folder with a selected row"
                                (and (exercise--click workbench ':retry)
                                     (exercise-live--until workbench
                                                           (lambda ()
                                                             (and (exercise-live--current-p workbench)
                                                                  (not (equal channel (workbench-channel workbench)))
                                                                  (equal directory (workbench-directory workbench))))
                                                           :seconds 120)))
               (exercise-live--picture workbench "live-restart-unsaved"))
             ;; The world server was started only for this exercise. It is stopped from the window.
             (workbench-stop-server workbench)
             (exercise-expect "the world server is stopped from the window"
                              (exercise-live--until workbench
                                                    (lambda ()
                                                      (not (world-served-p (workbench-world workbench))))
                                                    :seconds 60)))
        (workbench-close workbench)
        ;; Stop the world server here if the exercise did not stop it.
        (uiop:run-program (list (installation-program) "stop")
                          :ignore-error-status t))))
  *exercise-failures*)

(exercise-live-run)
(exercise--finish)
