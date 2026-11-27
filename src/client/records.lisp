(in-package #:aui)

(defclass projector ()
  ((transcript
    :initarg       :transcript
    :reader        projector-transcript
    :type          transcript
    :documentation "The transcript to which the entries are written.")
   (image-root
    :initarg       :image-root
    :initform      nil
    :accessor      projector-image-root
    :type          (or null pathname)
    :documentation "The directory that contains the image artifacts of the conversation, or NIL.")
   (boundary
    :initform      0
    :accessor      projector-boundary
    :type          integer
    :documentation "The sequence number of the last record read from history.
Live records at or below this number were already shown.")
   (catch-up-p
    :initform      nil
    :accessor      projector-catch-up-p
    :type          boolean
    :documentation "True if a provider request was already running at attachment.
In that case, the records of the request are used instead of the events.")
   (answer
    :initform      nil
    :accessor      projector--answer
    :type          (or null assistant-entry)
    :documentation "The answer that is streaming now, or NIL.")
   (reasoning
    :initform      nil
    :accessor      projector--reasoning
    :type          (or null reasoning-entry)
    :documentation "The reasoning that is streaming now, or NIL.")
   (output
    :initform      nil
    :accessor      projector--output
    :type          (or null output-entry)
    :documentation "The open command output entry, to which more text can be added, or NIL.")
   (streamed
    :initform      0
    :accessor      projector--streamed
    :type          integer
    :documentation "The number of characters streamed for the answer to the current provider request.
The count includes all entries of the answer.")
   (unsettled
    :initform      nil
    :accessor      projector--unsettled
    :type          list
    :documentation "The tool results of the last history page for which the calls are on another page.")
   (invocation
    :initform      nil
    :accessor      projector-invocation
    :type          (or null string)
    :documentation "The command that the person just sent, or NIL.
The output that follows is labelled with this command.")
   (channel
    :initform      nil
    :accessor      projector-channel
    :type          (or null string)
    :documentation "The identifier of the channel of the records, or NIL before it is known."))
  (:documentation "The state used to convert the records and events of one channel into a transcript."))

(-> make-projector (transcript &key (:image-root (or null pathname))) projector)
(defun make-projector (transcript &key image-root)
  "Return a projector that writes to TRANSCRIPT. Image artifacts are looked up below IMAGE-ROOT."
  (make-instance 'projector :transcript transcript :image-root image-root))

;;;; -- JSON --

(-> json->data (t) t)
(defun json->data (value)
  "Return the decoded JSON VALUE as plain Lisp data.

Objects become association lists of string keys in the written order.
Arrays become lists, and false becomes :FALSE. Null is :NULL. Strings,
numbers and T are returned as they are."
  (cond
    ((autolith::json-object-p value)
     (loop for key being the hash-keys of value using (hash-value member)
           collect (cons key (json->data member))))
    ((autolith::json-false-p value)
     ':false)
    ((stringp value)
     value)
    ((vectorp value)
     (map 'list #'json->data value))
    (t
     value)))

(-> json-arguments (t) list)
(defun json-arguments (source)
  "Return the tool arguments SOURCE as an association list.
SOURCE is a JSON object or the text of one.

If SOURCE does not decode to an object, return a list with one \"arguments\"
member that contains the text."
  (handler-case
      (let ((decoded (if (stringp source)
                         (if (plusp (length source))
                             (autolith::json-decode source)
                             (autolith::json-object))
                         source)))
        (if (autolith::json-object-p decoded)
            (json->data decoded)
            (list (cons "arguments" (princ-to-string source)))))
    (error ()
      (list (cons "arguments" (princ-to-string source))))))


;;;; -- Records --

(-> record-sequence (list) (or null integer))
(defun record-sequence (record)
  "Return the durable sequence number of RECORD, or NIL if RECORD has none."
  (let ((sequence (getf (rest record) :seq)))
    (and (integerp sequence) sequence)))

(-> record--time (list) (or null integer))
(defun record--time (record)
  "Return the universal time of RECORD, or NIL if RECORD has no time in that form."
  (let ((time (getf (rest record) :time)))
    (and (integerp time) time)))

(defparameter *compaction-label* "context compacted"
  "The label of the marker at the place where the context of the conversation was compacted.")

(defparameter *goal-statuses* '(:active :paused :complete :verified :blocked :exhausted :cancelled :failed)
  "A list of the statuses that the goal of a channel can have.")

(-> goal--create (t t &key (:kind t) (:created-at t)) list)
(defun goal--create (objective status &key kind created-at)
  "Return a goal as a property list of :OBJECTIVE, :STATUS, :KIND and :CREATED-AT.
Return NIL if OBJECTIVE and STATUS do not describe a goal.

STATUS is one of `*goal-statuses*', as a keyword or as a string. :KIND is
:MISSION if KIND is \"mission\" and :GOAL otherwise."
  (let ((status (and (or (stringp status) (symbolp status))
                     (find status *goal-statuses* :test #'string-equal))))
    (and status
         (stringp objective)
         (plusp (length objective))
         (list :objective  objective
               :status     status
               :kind       (if (and (or (stringp kind) (symbolp kind)) (string-equal kind "mission"))
                               ':mission
                               ':goal)
               :created-at (and (integerp created-at) created-at)))))

(-> record-goal (list) list)
(defun record-goal (record)
  "Return the goal stated in the durable goal RECORD.
Return NIL if RECORD is the record of a cleared goal."
  (let ((properties (rest record)))
    (goal--create (getf properties :objective) (getf properties :status)
                  :kind       (getf properties :kind)
                  :created-at (getf properties :created-at))))

(-> portable-goal (t) list)
(defun portable-goal (object)
  "Return the goal in the portable object OBJECT of the state of a channel.
Return NIL if OBJECT contains no goal."
  (goal--create (portable-get object "objective") (portable-get object "status")
                :kind       (portable-get object "kind")
                :created-at (portable-get object "createdAt")))

(-> records-newest-goal (list list) list)
(defun records-newest-goal (records known)
  "Return the goal record with the highest sequence number among RECORDS and KNOWN.
KNOWN is a goal record or NIL."
  (dolist (record records known)
    (when (and (eq (first record) ':goal)
               (or (null known)
                   (>= (or (record-sequence record) 0) (or (record-sequence known) 0))))
      (setf known record))))

(-> record--image-pathnames (projector list) list)
(defun record--image-pathnames (projector descriptors)
  "Return the namestrings of the image artifacts in DESCRIPTORS that exist below the image root of PROJECTOR."
  (let ((root (projector-image-root projector)))
    (when root
      (loop for descriptor in descriptors
            for artifact = (and (listp descriptor) (getf descriptor :artifact))
            for pathname = (and (stringp artifact) (merge-pathnames artifact root))
            when (and pathname (probe-file pathname))
              collect (namestring pathname)))))

(-> record--job-notice (list (or null integer)) notice-entry)
(defun record--job-notice (completion time)
  "Return the notice for the COMPLETION of a background job, recorded at TIME."
  (let ((outcome (getf completion :outcome))
        (summary (getf completion :summary)))
    (make-instance 'notice-entry
                   :time   time
                   :tone   (if (eq outcome ':failure) ':failure ':quiet)
                   :text   (format nil "job ~A~@[ · ~A~]"
                                   (case outcome
                                     (:cancellation-pending
                                      "cancellation pending")
                                     (:success
                                      "completed")
                                     (:failure
                                      "failed")
                                     (:cancelled
                                      "cancelled")
                                     (otherwise
                                      "result received"))
                                   (getf completion :job-id))
                   :detail (and (stringp summary) (plusp (length summary)) summary))))

(-> record--request-entry (list (or null integer) (or null string)) entry)
(defun record--request-entry (request time channel)
  "Return the entry for the operation REQUEST that started a turn.
The entry is a message, a mention notice or a notice with the name of the operation.

Messages keep the channel from which the sender wrote, unless that channel is CHANNEL."
  (let ((message (getf request :message)))
    (cond
      ((and message (member (getf message :kind) '(:mention :subscription)))
       (make-instance 'notice-entry
                      :time    time
                      :channel (getf message :target)
                      :text    (format nil "@~A mentioned from ~A"
                                       (or (getf message :sender-name) "participant")
                                       (channel-display-name (getf message :target)))))
      (message
       (make-instance 'user-entry
                      :time   time
                      :text   (or (getf message :text) (getf request :summary) "")
                      :sender (or (getf message :sender-name) "participant")
                      :origin (let ((origin (or (getf message :source) (getf message :target))))
                                (and (stringp origin) (not (equal origin channel)) origin))))
      (t
       (make-instance 'notice-entry
                      :time   time
                      :text   (format nil "~A" (getf request :operation))
                      :detail (getf request :summary))))))

(-> record--continuation-text (string) string)
(defun record--continuation-text (content)
  "Return the notice text for the automatic continuation prompt CONTENT."
  (cond
    ((string= content autolith::*application-goal-continuation-prompt*)
     "goal continues")
    ((string= content autolith::*application-job-completion-continuation-prompt*)
     "continuing with job results")
    (t
     (format nil "mission wakeup: ~A" content))))

(-> record--message-entry (projector list) (or null entry))
(defun record--message-entry (projector record)
  "Return the entry for the user message RECORD. Return NIL if there is nothing to show."
  (let* ((properties (rest record))
         (content    (getf properties :content))
         (time       (record--time record)))
    (when (eq (getf properties :role) ':user)
      (cond
        ((getf properties :job-completion)
         (record--job-notice (getf properties :job-completion) time))
        ((getf properties :operation-request)
         (record--request-entry (getf properties :operation-request) time (projector-channel projector)))
        ((getf properties :automatic-p)
         (make-instance 'notice-entry
                        :time time
                        :text (record--continuation-text (if (stringp content) content ""))))
        (t
         (make-instance 'user-entry
                        :time   time
                        :text   (if (stringp content) content "")
                        :images (record--image-pathnames projector (getf properties :images))))))))

(-> record--item-entry (list) (or null entry))
(defun record--item-entry (record)
  "Return the entry for the provider item RECORD, or NIL.
The entry is an answer, reasoning, a tool call or a lookup of tool schemas."
  (let ((wire-json (getf (rest record) :wire-json))
        (time      (record--time record)))
    (handler-case
        (let* ((item (and (stringp wire-json) (autolith::json-decode wire-json)))
               (type (and (autolith::json-object-p item) (autolith::json-get item "type"))))
          (cond
            ((and (equal type "message") (equal (autolith::json-get item "role") "assistant"))
             (let ((text (autolith::application--visible-assistant-text item)))
               (and text (make-instance 'assistant-entry :time time :text text))))
            ((equal type "reasoning")
             (let ((summary (autolith::response-item-reasoning-summary item)))
               (and summary (make-instance 'reasoning-entry :time time :text summary))))
            ((equal type "function_call")
             (make-instance 'tool-entry
                            :time            time
                            :call-identifier (autolith::json-get item "call_id")
                            :name            (or (autolith::function-call-canonical-name item) "tool")
                            :arguments       (json-arguments (autolith::json-get item "arguments"))))
            ((equal type "tool_search_output")
             (let ((names (autolith::application--tool-search-namespaces item)))
               (and names (make-instance 'schemas-entry :time time :names names))))
            ((equal type "web_search_call")
             (make-instance 'tool-entry
                            :time      time
                            :name      "web.search"
                            :status    ':completed
                            :arguments (list (cons "query" (autolith::web-search-call-detail item)))))
            (t
             nil)))
      (error ()
        nil))))

(-> record--schemas-entry (list) (or null entry))
(defun record--schemas-entry (record)
  "Return the entry of RECORD if RECORD is a provider item for a lookup of tool schemas.
Otherwise, return NIL."
  (let ((wire-json (getf (rest record) :wire-json)))
    (and (eq (first record) ':provider-item)
         (stringp wire-json)
         ;; Most items have other types. Only items that contain the type are decoded.
         (search "tool_search_output" wire-json)
         (let ((entry (record--item-entry record)))
           (and (typep entry 'schemas-entry) entry)))))

(-> record--join-schemas (entry (or null entry)) boolean)
(defun record--join-schemas (entry previous)
  "Add the names of the schema lookup ENTRY to PREVIOUS if both are lookups.
Return T if the names were added."
  (when (and (typep entry 'schemas-entry) (typep previous 'schemas-entry))
    (setf (schemas-entry-names previous)
          (remove-duplicates (append (schemas-entry-names previous) (schemas-entry-names entry))
                             :test     #'string=
                             :from-end t))
    t))

(-> record--seconds (list) (or null real))
(defun record--seconds (properties)
  "Return the wall-clock seconds in the tool result PROPERTIES, or NIL."
  (let ((microseconds (getf properties :real-microseconds)))
    (and (realp microseconds) (/ microseconds 1000000))))

(-> tool-entry-settle (tool-entry &key (:success-p boolean) (:output t)
                                      (:details t) (:seconds (or null real))) null)
(defun tool-entry-settle (entry &key success-p output details seconds)
  "Set the status, output, structured details and duration of ENTRY."
  (setf (tool-entry-status entry)  (if success-p ':completed ':failed)
        (tool-entry-output entry)  (and output (princ-to-string output))
        (tool-entry-details entry) details
        (tool-entry-patch entry)   nil
        (tool-entry-seconds entry) seconds)
  nil)

(-> record--result (list) list)
(defun record--result (properties)
  "Return the outcome in tool result PROPERTIES as tool-entry-settle arguments."
  (list :success-p (and (member (getf properties :status) '(:ok :success)) t)
        :output    (getf properties :output)
        :details   (getf properties :details)
        :seconds   (record--seconds properties)))

(-> projector--settle-tool
    (projector (or null string) &rest t &key (:success-p boolean) (:output t)
                                      (:details t) (:seconds (or null real))) null)
(defun projector--settle-tool (projector call-identifier &rest result
                               &key success-p output details seconds)
  "Store RESULT of tool call CALL-IDENTIFIER in PROJECTOR's transcript."
  (declare (ignore success-p output details seconds))
  (let* ((transcript (projector-transcript projector))
         (entry      (and call-identifier (transcript-find-tool transcript call-identifier))))
    (when entry
      (apply #'tool-entry-settle entry result)
      (when (equal (tool-entry-name entry) "resource.edit")
        (let ((entries (transcript-entries transcript)))
          (edit-history--reconstruct entry entries (position entry entries :from-end t))))
      (transcript-touch transcript entry)))
  nil)

(-> record--side-status-text (list) string)
(defun record--side-status-text (record)
  "Return the text of the spans in the side status RECORD."
  (with-output-to-string (stream)
    (dolist (span (getf (rest record) :spans))
      (let ((text (if (consp span) (autolith::terminal-span-text span) span)))
        (when (stringp text)
          (write-string text stream))))))

(-> record--cache-notice (string (or null integer)) (or null notice-entry))
(defun record--cache-notice (text time)
  "Return a cache notice for TEXT, or NIL if the wording is not recognized."
  (let ((prefix (find-if (lambda (prefix) (uiop:string-prefix-p prefix text))
                         '("cache miss " "cache stalled · "))))
    (when prefix
      (let ((clauses (loop with separator = " · "
                           for start = (length prefix) then (+ end (length separator))
                           for end = (search separator text :start2 start)
                           collect (subseq text start end)
                           while end)))
        (make-instance 'notice-entry
                       :time time
                       :tone ':warning
                       :text (if (string= prefix "cache miss ") "prompt cache miss" "prompt cache stalled")
                       :body (format nil "~{~A~^~%~}" clauses))))))

(-> record--side-status (list) (or null notice-entry))
(defun record--side-status (record)
  "Return the diagnostic described by the stored display spans of RECORD."
  (let* ((fields (rest record))
         (text   (string-trim '(#\Space #\Tab #\Newline) (record--side-status-text record)))
         (time   (record--time record))
         (style  (let ((span (first (getf fields :spans))))
                   (and (consp span) (first span))))
         (words  (if (and (> (length text) 2)
                          (char= (char text 1) #\Space)
                          (find (char text 0) "!·∙✓■"))
                     (subseq text 2)
                     text))
         (break  (position #\Newline words)))
    (when (plusp (length words))
      (or (and (equal (getf fields :source) "prompt-cache") (record--cache-notice words time))
          (make-instance 'notice-entry
                         :time time
                         :tone (case style
                                 (:failure
                                  ':failure)
                                 (:success
                                  ':success)
                                 (otherwise
                                  ':quiet))
                         :text (string-right-trim '(#\Space #\Tab) (subseq words 0 break))
                         :body (and break (string-trim '(#\Space #\Tab #\Newline) (subseq words (1+ break)))))))))

(-> record->entry (projector list) (or null entry))
(defun record->entry (projector record)
  "Return the transcript entry for RECORD, or NIL if RECORD has no entry.

Tool results have no entry of their own. The result is stored in the entry
of the call in the transcript of PROJECTOR."
  (case (first record)
    (:message
     (record--message-entry projector record))
    (:provider-item
     (record--item-entry record))
    (:tool-result
     (apply #'projector--settle-tool projector (getf (rest record) :call-id) (record--result (rest record)))
     nil)
    (:side-status
     (record--side-status record))
    (:assistant-yield
     (let ((count (getf (rest record) :job-count)))
       (when (and (integerp count) (plusp count))
         (make-instance 'notice-entry
                        :time (record--time record)
                        :text (format nil "yielded: the agent waits for ~D job~:P" count)))))
    ((:summary :native-compaction)
     (make-instance 'marker-entry :time (record--time record) :label *compaction-label*))
    (:turn-aborted
     ;; A turn stopped by a person gets a quiet notice without the condition.
     (if (eq (getf (rest record) :reason) ':cancelled)
         (make-instance 'notice-entry
                        :time (record--time record)
                        :text "stopped")
         (make-instance 'notice-entry
                        :time   (record--time record)
                        :tone   ':failure
                        :text   "the turn stopped"
                        :detail (let ((message (getf (rest record) :message)))
                                  (and (stringp message) message)))))
    (otherwise
     nil)))

(-> records->entries (projector list) list)
(defun records->entries (projector records)
  "Return the entries of the history page RECORDS, oldest first.

Tool results are stored in the call entries of the same page. Results for
which the call is not on the page are kept in PROJECTOR. Results kept from
the previous call are stored in the call entries of this page."
  (let ((entries   nil)
        (calls     (make-hash-table :test #'equal))
        (unsettled nil))
    (flet ((settle (properties)
             (let ((entry (gethash (getf properties :call-id) calls)))
               (when entry
                 (apply #'tool-entry-settle entry (record--result properties))
                 t))))
      (dolist (record records)
        (if (eq (first record) ':tool-result)
            (unless (settle (rest record))
              (push (rest record) unsettled))
            (let ((entry (record->entry projector record)))
              (when (and entry (not (record--join-schemas entry (first entries))))
                (when (and (typep entry 'tool-entry) (tool-entry-call-identifier entry))
                  (setf (gethash (tool-entry-call-identifier entry) calls) entry))
                (push entry entries)))))
      (mapc #'settle (shiftf (projector--unsettled projector) unsettled)))
    (let* ((page        (nreverse entries))
           (page-length (length page))
           (transcript  (projector-transcript projector))
           (later       (transcript-entries transcript))
           (window      (concatenate (quote vector) page (subseq later 0 (min 128 (length later))))))
      (loop for entry across window for index from 0
            when (edit-history--reconstruct entry window index)
              do (when (>= index page-length) (transcript-touch transcript entry)))
      page)))


;;;; -- Live Events --

(-> projector--close-stream (projector) null)
(defun projector--close-stream (projector)
  "Mark the streaming answer and reasoning of PROJECTOR as finished, and clear them from PROJECTOR."
  (let ((transcript (projector-transcript projector))
        (answer     (projector--answer projector))
        (reasoning  (projector--reasoning projector)))
    (when answer
      (setf (assistant-entry-streaming-p answer) nil)
      (transcript-touch transcript answer))
    (when reasoning
      (setf (reasoning-entry-streaming-p reasoning) nil)
      (transcript-touch transcript reasoning))
    (setf (projector--answer projector)    nil
          (projector--reasoning projector) nil))
  nil)

(-> projector--append (projector entry) entry)
(defun projector--append (projector entry)
  "Append ENTRY to the transcript of PROJECTOR and return ENTRY.
Close the open command output, unless ENTRY is an output entry."
  (unless (typep entry 'output-entry)
    (setf (projector--output projector) nil))
  (transcript-append (projector-transcript projector) entry))

(-> projector--text (projector string) null)
(defun projector--text (projector delta)
  "Append the streamed answer text DELTA to the answer of PROJECTOR."
  (when (plusp (length delta))
    (let ((answer (projector--answer projector)))
      (unless answer
        (let ((reasoning (projector--reasoning projector)))
          (when reasoning
            (setf (reasoning-entry-streaming-p reasoning) nil
                  (projector--reasoning projector)        nil)
            (transcript-touch (projector-transcript projector) reasoning)))
        (setf answer                         (projector--append projector
                                                                (make-instance 'assistant-entry :streaming-p t))
              (projector--answer projector) answer))
      (setf (assistant-entry-text answer)
            (concatenate 'string (assistant-entry-text answer) delta))
      (incf (projector--streamed projector) (length delta))
      (transcript-touch (projector-transcript projector) answer)))
  nil)

(-> projector--reasoning-text (projector string) null)
(defun projector--reasoning-text (projector delta)
  "Append the streamed reasoning text DELTA to the reasoning of PROJECTOR."
  (when (plusp (length delta))
    (let ((reasoning (projector--reasoning projector)))
      (unless reasoning
        (setf reasoning                         (projector--append projector
                                                                   (make-instance 'reasoning-entry :streaming-p t))
              (projector--reasoning projector) reasoning))
      (setf (reasoning-entry-text reasoning)
            (concatenate 'string (reasoning-entry-text reasoning) delta))
      (transcript-touch (projector-transcript projector) reasoning)))
  nil)

(-> projector--persisted-answer (projector t) null)
(defun projector--persisted-answer (projector text)
  "Complete the streamed answer with the part of the persisted TEXT that was not streamed.

For providers that do not stream, this part is the whole text."
  (when (stringp text)
    (let ((streamed (projector--streamed projector)))
      (when (< streamed (length text))
        (projector--text projector (subseq text streamed)))))
  (setf (projector--streamed projector) 0)
  (projector--close-stream projector)
  nil)

(-> projector--append-marker (projector string) null)
(defun projector--append-marker (projector label)
  "Append a marker labelled LABEL to the transcript of PROJECTOR.
Do nothing if the last entry is already that marker."
  (let* ((transcript (projector-transcript projector))
         (count      (transcript-length transcript))
         (last       (and (plusp count) (transcript-entry transcript (1- count)))))
    (unless (and (typep last 'marker-entry) (equal (marker-entry-label last) label))
      (projector--close-stream projector)
      (projector--append projector (make-instance 'marker-entry :label label))))
  nil)

(-> projector--status (projector t list) null)
(defun projector--status (projector status details)
  "Apply the provider or tool lifecycle STATUS and DETAILS to PROJECTOR."
  (case status
    (:provider-request-started
     (setf (projector-catch-up-p projector) nil
           (projector--streamed projector)  0)
     (projector--close-stream projector))
    (:assistant-response-persisted
     (projector--persisted-answer projector (getf details :text)))
    (:tool-call-started
     (projector--close-stream projector)
     (projector--append projector
                        (make-instance 'tool-entry
                                       :call-identifier (getf details :call-id)
                                       :name            (or (getf details :tool) "tool")
                                       :status          ':running
                                       :arguments       (json-arguments (or (getf details :arguments) "")))))
    (:tool-call-completed
     (projector--settle-tool projector (getf details :call-id)
                             :success-p (and (getf details :success-p) t)
                             :output    (getf details :output)
                             :details   (getf details :details)
                             :seconds   (record--seconds details)))
    (:provider-request-completed
     (projector--close-stream projector))
    (:compaction-completed
     (projector--append-marker projector *compaction-label*))
    (otherwise
     nil))
  nil)

(defparameter *notice-glyphs* '((#\∙ . :quiet) (#\· . :quiet) (#\✓ . :success)
                                (#\! . :failure) (#\@ . :quiet) (#\◆ . :quiet) (#\↪ . :quiet))
  "A list of the glyphs at the start of one-line notices from Autolith, associated with their tones.")

(-> projector--notice-line (string) (values (or null string) (or null keyword)))
(defun projector--notice-line (text)
  "Return the words and the tone of TEXT if TEXT is a one-line notice from Autolith.
Otherwise, return NIL.

These notices start with a glyph and a space. The words are returned without
the glyph."
  (let ((tone (and (> (length text) 2)
                   (char= (char text 1) #\Space)
                   (null (position #\Newline text))
                   (rest (assoc (char text 0) *notice-glyphs*)))))
    (if tone
        (values (string-trim " " (subseq text 2)) tone)
        (values nil nil))))

(-> projector--reasoning-summary (string) (or null string))
(defun projector--reasoning-summary (text)
  "Return the reasoning summary in TEXT if TEXT is a summary printed by the terminal.
Otherwise, return NIL.

These summaries are printed under a heading, with a bar in front of each
line. The heading and the bars are removed."
  (let* ((lines (markdown-lines text))
         (head  (string-trim '(#\Space) (or (first lines) ""))))
    (and (> (length head) 2)
         (search "reasoning summary" head)
         (not (alphanumericp (char head 0)))
         ;; The summary was wrapped to the width of the terminal. Consecutive
         ;; rows form one paragraph, and an empty row separates two paragraphs.
         (let ((paragraphs nil)
               (words      nil))
           (dolist (line (rest lines))
             (let ((row (string-trim '(#\Space) (string-left-trim '(#\Space #\│ #\|) line))))
               (cond
                 ((plusp (length row))
                  (push row words))
                 (words
                  (push (format nil "~{~A~^ ~}" (nreverse words)) paragraphs)
                  (setf words nil)))))
           (when words
             (push (format nil "~{~A~^ ~}" (nreverse words)) paragraphs))
           (format nil "~{~A~^~2%~}" (nreverse paragraphs))))))

(-> projector--letters (string) string)
(defun projector--letters (text)
  "Return the letters and digits of TEXT in lowercase.
Two renderings of one summary have the same letters and digits."
  (string-downcase (remove-if-not #'alphanumericp text)))

(-> projector--note-reasoning (projector string) null)
(defun projector--note-reasoning (projector summary)
  "Append the reasoning SUMMARY as a thought, unless a recent thought already contains it."
  (let* ((transcript (projector-transcript projector))
         (entries    (transcript-entries transcript))
         (recent     (find-if (lambda (entry)
                                (typep entry 'reasoning-entry))
                              entries
                              :from-end t
                              :start    (max 0 (- (length entries) 6))))
         (letters    (projector--letters summary)))
    (unless (or (zerop (length letters))
                (and recent (search letters (projector--letters (reasoning-entry-text recent)))))
      (projector--close-stream projector)
      (projector--append projector (make-instance 'reasoning-entry :text summary))))
  nil)

(-> projector--present (projector t) null)
(defun projector--present (projector text)
  "Append the command or Lisp output TEXT to the transcript of PROJECTOR.
The parts printed by one command are joined in one entry.

Autolith echoes local Lisp forms on a line that starts with an asterisk,
before the output. That line becomes the invocation of the entry."
  (when (stringp text)
    (let* ((trimmed (string-trim '(#\Newline) text))
           (echo-p  (and (> (length trimmed) 2)
                         (string= "* " trimmed :end2 2)
                         (null (position #\Newline trimmed))))
           (open    (projector--output projector))
           (cache   (and (or (null open) (null (output-entry-invocation open)))
                         (null (projector-invocation projector))
                         (uiop:string-prefix-p "! cache " trimmed)
                         (record--cache-notice (subseq trimmed 2) nil))))
      (cond
        ((zerop (length (string-trim '(#\Newline #\Space) trimmed)))
         nil)
        ;; A reasoning summary printed by the terminal is shown once, as a thought.
        ((projector--reasoning-summary trimmed)
         (projector--note-reasoning projector (projector--reasoning-summary trimmed)))
        (cache
         (projector--close-stream projector)
         (projector--append projector cache))
        ;; The NIL returned by a command is dropped after the output of the command.
        ((and open
              (plusp (length (output-entry-text open)))
              (string= (string-trim '(#\Space) trimmed) "⇒ NIL"))
         nil)
        (echo-p
         (setf (projector-invocation projector) nil
               (projector--output projector)
               (projector--append projector
                                  (make-instance 'output-entry :invocation (subseq trimmed 2)))))
        ;; A notice line becomes a notice entry, unless a command run by the
        ;; person is printing. In that case, the line is part of the output.
        ((and (or (null open) (null (output-entry-invocation open)))
              (null (projector-invocation projector))
              (projector--notice-line trimmed))
         (multiple-value-bind (words tone) (projector--notice-line trimmed)
           (projector--close-stream projector)
           ;; Some notices, such as the notice about memories, list calls after
           ;; the statement. The statement becomes the text, and the calls
           ;; become the detail.
           (let ((calls (search " · (" words)))
             (projector--append
              projector
              (make-instance 'notice-entry
                             :tone   tone
                             :text   (if calls (subseq words 0 calls) words)
                             :detail (and calls
                                          (format nil "~{~A~^~%~}"
                                                  (loop with rest = (subseq words (+ calls 3))
                                                        for cut = (search " · " rest)
                                                        collect (subseq rest 0 cut)
                                                        while cut
                                                        do (setf rest (subseq rest (+ cut 3)))))))))))
        (open
         (setf (output-entry-text open)
               (if (plusp (length (output-entry-text open)))
                   (format nil "~A~%~A" (output-entry-text open) trimmed)
                   trimmed))
         (transcript-touch (projector-transcript projector) open))
        ;; Slash commands are not echoed. The output is labelled with the text sent
        ;; by the person.
        (t
         (setf (projector--output projector)
               (projector--append projector
                                  (make-instance 'output-entry
                                                 :text       trimmed
                                                 :invocation (shiftf (projector-invocation projector) nil))))))))
  nil)

(-> projector--live-record (projector list) null)
(defun projector--live-record (projector record)
  "Append the entry of the live RECORD to the transcript of PROJECTOR.
Do nothing if RECORD was read from history or if the events of the turn cover it."
  (let ((sequence (record-sequence record)))
    (when (or (null sequence) (> sequence (projector-boundary projector)))
      (let* ((entry      (if (or (projector-catch-up-p projector)
                                 (not (member (first record) '(:provider-item :tool-result))))
                             (record->entry projector record)
                             (record--schemas-entry record)))
             (transcript (projector-transcript projector))
             (count      (transcript-length transcript))
             (last       (and (plusp count) (transcript-entry transcript (1- count)))))
        (cond
          ((null entry)
           nil)
          ((record--join-schemas entry last)
           (transcript-touch transcript last))
          ((typep entry 'marker-entry)
           (projector--append-marker projector (marker-entry-label entry)))
          (t
           (projector--close-stream projector)
           (projector--append projector entry))))))
  nil)

(-> projector-apply (projector list) null)
(defun projector-apply (projector event)
  "Apply EVENT of the structured client protocol to the transcript of PROJECTOR.

Events unrelated to the transcript, such as approvals and state, are ignored."
  (let ((fields (rest event)))
    (case (first event)
      (:text
       (unless (projector-catch-up-p projector)
         (projector--text projector (or (getf fields :delta) ""))))
      (:reasoning
       (unless (projector-catch-up-p projector)
         (projector--reasoning-text projector (or (getf fields :delta) ""))))
      (:status
       (let ((status (getf fields :status)))
         (unless (and (projector-catch-up-p projector)
                      (member status '(:assistant-response-persisted :tool-call-started
                                       :tool-call-completed)))
           (projector--status projector status (getf fields :details)))))
      (:present
       (projector--present projector (getf fields :text)))
      (:record
       (projector--live-record projector (getf fields :record)))
      (:work-started
       (setf (projector-catch-up-p projector) nil
             (projector--output projector)    nil))
      (:work-finished
       (projector--close-stream projector)
       (setf (projector--output projector)    nil
             (projector-invocation projector) nil))
      (otherwise
       nil)))
  nil)

(-> channel-display-name (t) string)
(defun channel-display-name (identifier)
  "Return the display form of the channel IDENTIFIER. The display form contains the visual hyphen."
  (if (stringp identifier)
      (autolith::conversation-identifier-display identifier)
      "?"))
