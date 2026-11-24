(in-package #:aui)

;;;; -- Transcript Model --

;;; Ordered channel history. Client records become entry mutations; the transcript is interface-thread state.

(deftype tool-status ()
  "The progress of a tool call."
  '(member :pending :running :completed :failed))

(deftype notice-tone ()
  "The tone of a notice: a quiet aside, a success, a warning or a failure."
  '(member :quiet :success :warning :failure))


;;;; -- Entries --

(defclass entry ()
  ((identifier
    :initarg       :identifier
    :initform      nil
    :accessor      entry-identifier
    :type          (or null string)
    :documentation "The identifier of the entry in the transcript.
If it is NIL, an identifier is assigned on insertion.")
   (time
    :initarg       :time
    :initform      nil
    :reader        entry-time
    :type          (or null integer)
    :documentation "The universal time recorded by Autolith for the entry, or NIL.")
   (revision
    :initform      0
    :accessor      entry-revision
    :type          integer
    :documentation "The revision of the entry. It is incremented on each change.")
   (expanded-p
    :initarg       :expanded-p
    :initform      nil
    :accessor      entry-expanded-p
    :type          boolean
    :documentation "True if the person opened the detail of the entry."))
  (:documentation "One unit of the history of a channel."))

(defclass user-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :reader        user-entry-text
    :type          string
    :documentation "The text of the message.")
   (images
    :initarg       :images
    :initform      nil
    :reader        user-entry-images
    :type          list
    :documentation "The namestrings of the images sent with the message.")
   (sender
    :initarg       :sender
    :initform      nil
    :reader        user-entry-sender
    :type          (or null string)
    :documentation "The participant who wrote the message, or NIL if the person wrote it.
Messages from other channels have a sender.")
   (origin
    :initarg       :origin
    :initform      nil
    :reader        user-entry-origin
    :type          (or null string)
    :documentation "The channel from which the message came, or NIL."))
  (:documentation "A message to the agent of the channel."))

(defclass assistant-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :accessor      assistant-entry-text
    :type          string
    :documentation "The prose of the agent as Markdown. The text grows while it streams.")
   (streaming-p
    :initarg       :streaming-p
    :initform      nil
    :accessor      assistant-entry-streaming-p
    :type          boolean
    :documentation "True if more text is still arriving."))
  (:documentation "One contiguous answer from the agent."))

(defclass reasoning-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :accessor      reasoning-entry-text
    :type          string
    :documentation "The visible reasoning summary. The text grows while it streams.")
   (streaming-p
    :initarg       :streaming-p
    :initform      nil
    :accessor      reasoning-entry-streaming-p
    :type          boolean
    :documentation "True if more reasoning is still arriving."))
  (:documentation "A part of the visible reasoning of the agent. It is closed by default."))

(defclass tool-entry (entry)
  ((call-identifier
    :initarg       :call-identifier
    :initform      nil
    :reader        tool-entry-call-identifier
    :type          (or null string)
    :documentation "The identifier of the call from the provider.
The result of the call contains the same identifier.")
   (name
    :initarg       :name
    :initform      "tool"
    :reader        tool-entry-name
    :type          string
    :documentation "The canonical name of the tool, such as shell.run.")
   (arguments
    :initarg       :arguments
    :initform      nil
    :accessor      tool-entry-arguments
    :type          list
    :documentation "The arguments of the call as an association list of names and values, in the given order.")
   (status
    :initarg       :status
    :initform      ':pending
    :accessor      tool-entry-status
    :type          tool-status
    :documentation "The progress of the call.")
   (patch
    :initarg       :patch
    :initform      nil
    :accessor      tool-entry-patch
    :type          (or null string)
    :documentation "The logical-line patch reconstructed from this channel, or NIL.")
   (output
    :initarg       :output
    :initform      nil
    :accessor      tool-entry-output
    :type          (or null string)
    :documentation "The output of the tool, or NIL before the tool returned.")
   (details
     :initarg       :details
     :initform      nil
     :accessor      tool-entry-details
     :type          t
     :documentation "The structured details of the result, or NIL for legacy results.")
   (seconds
    :initarg       :seconds
    :initform      nil
    :accessor      tool-entry-seconds
    :type          (or null real)
    :documentation "The wall-clock duration of the call in seconds, or NIL before it is known."))
  (:documentation "One tool call and its result."))

(defclass notice-entry (entry)
  ((tone
    :initarg       :tone
    :initform      ':quiet
    :reader        notice-entry-tone
    :type          notice-tone
    :documentation "The tone of the notice.")
   (text
    :initarg       :text
    :initform      ""
    :reader        notice-entry-text
    :type          string
    :documentation "The one-line statement of the notice.")
   (detail
    :initarg       :detail
    :initform      nil
    :reader        notice-entry-detail
    :type          (or null string)
    :documentation "A longer text shown beneath the statement, or NIL.")
   (channel
    :initarg       :channel
    :initform      nil
    :reader        notice-entry-channel
    :type          (or null string)
    :documentation "The channel that is opened by a click on the notice, or NIL."))
  (:documentation "A statement from Autolith about the conversation."))

(defclass output-entry (entry)
  ((text
    :initarg       :text
    :initform      ""
    :accessor      output-entry-text
    :type          string
    :documentation "The output, laid out by Autolith for a fixed-pitch face.")
   (invocation
    :initarg       :invocation
    :initform      nil
    :accessor      output-entry-invocation
    :type          (or null string)
    :documentation "The command or form that printed the output, or NIL."))
  (:documentation "The output printed by a command or a local Lisp form."))

(defclass schemas-entry (entry)
  ((names
    :initarg       :names
    :initform      nil
    :accessor      schemas-entry-names
    :type          list
    :documentation "The names of the groups of tools, in the order of the first lookup of each group."))
  (:documentation "A lookup of the schemas of groups of tools by the agent. Consecutive lookups are one entry."))

(defclass marker-entry (entry)
  ((label
    :initarg       :label
    :initform      ""
    :reader        marker-entry-label
    :type          string
    :documentation "The text on the horizontal rule."))
  (:documentation "A horizontal rule across the transcript, such as at the point where the context was compacted."))


;;;; -- Transcript --

(defclass transcript ()
  ((entries
    :initform      (make-array 64 :adjustable t :fill-pointer 0)
    :reader        transcript-entries
    :type          vector
    :documentation "The entries, oldest first.")
   (index
    :initform      (make-hash-table :test #'equal)
    :reader        transcript--index
    :type          hash-table
    :documentation "A table of the entries by identifier.")
   (tools
    :initform      (make-hash-table :test #'equal)
    :reader        transcript--tools
    :type          hash-table
    :documentation "A table of the tool entries by call identifier.")
   (counter
    :initform      0
    :accessor      transcript--counter
    :type          integer
    :documentation "The last generated entry identifier.")
   (revision
    :initform      0
    :accessor      transcript-revision
    :type          integer
    :documentation "The revision of the transcript.
It is incremented on each change to the sequence or to an entry."))
  (:documentation "The ordered history of one channel."))

(-> make-transcript () transcript)
(defun make-transcript ()
  "Return an empty transcript."
  (make-instance 'transcript))

(-> transcript-length (transcript) integer)
(defun transcript-length (transcript)
  "Return the number of entries in TRANSCRIPT."
  (length (transcript-entries transcript)))

(-> transcript-entry (transcript integer) entry)
(defun transcript-entry (transcript position)
  "Return the entry at POSITION in TRANSCRIPT. Position 0 is the oldest entry."
  (aref (transcript-entries transcript) position))

(-> transcript-find (transcript string) (or null entry))
(defun transcript-find (transcript identifier)
  "Return the entry of TRANSCRIPT with the identifier IDENTIFIER, or NIL."
  (values (gethash identifier (transcript--index transcript))))

(-> transcript-find-tool (transcript string) (or null tool-entry))
(defun transcript-find-tool (transcript call-identifier)
  "Return the tool entry of TRANSCRIPT for CALL-IDENTIFIER, or NIL."
  (values (gethash call-identifier (transcript--tools transcript))))

(-> transcript-last (transcript) (or null entry))
(defun transcript-last (transcript)
  "Return the newest entry of TRANSCRIPT, or NIL if TRANSCRIPT is empty."
  (let ((entries (transcript-entries transcript)))
    (when (plusp (length entries))
      (aref entries (1- (length entries))))))

(-> transcript--register (transcript entry) entry)
(defun transcript--register (transcript entry)
  "Give ENTRY an identifier if it has none, and add ENTRY to the tables of TRANSCRIPT.
Return ENTRY."
  (unless (entry-identifier entry)
    (setf (entry-identifier entry)
          (format nil "entry-~D" (incf (transcript--counter transcript)))))
  (setf (gethash (entry-identifier entry) (transcript--index transcript)) entry)
  (when (and (typep entry 'tool-entry) (tool-entry-call-identifier entry))
    (setf (gethash (tool-entry-call-identifier entry) (transcript--tools transcript)) entry))
  entry)

(-> transcript-append (transcript entry) entry)
(defun transcript-append (transcript entry)
  "Add ENTRY as the newest entry of TRANSCRIPT and return ENTRY."
  (transcript--register transcript entry)
  (vector-push-extend entry (transcript-entries transcript))
  (incf (transcript-revision transcript))
  entry)

(-> transcript-prepend (transcript list) integer)
(defun transcript-prepend (transcript entries)
  "Put ENTRIES, oldest first, before all entries of TRANSCRIPT.
Return the number of added entries."
  (let* ((existing (coerce (transcript-entries transcript) 'list))
         (vector   (transcript-entries transcript)))
    (setf (fill-pointer vector) 0)
    (dolist (entry entries)
      (vector-push-extend (transcript--register transcript entry) vector))
    (dolist (entry existing)
      (vector-push-extend entry vector))
    (incf (transcript-revision transcript))
    (length entries)))

(-> transcript-touch (transcript entry) entry)
(defun transcript-touch (transcript entry)
  "Mark ENTRY as changed and return ENTRY.
The revisions of ENTRY and TRANSCRIPT are incremented."
  (incf (entry-revision entry))
  (incf (transcript-revision transcript))
  entry)

(-> transcript-clear (transcript) transcript)
(defun transcript-clear (transcript)
  "Remove all entries from TRANSCRIPT and return TRANSCRIPT."
  (setf (fill-pointer (transcript-entries transcript)) 0)
  (clrhash (transcript--index transcript))
  (clrhash (transcript--tools transcript))
  (incf (transcript-revision transcript))
  transcript)

(-> transcript-plain-text (transcript) string)
(defun transcript-plain-text (transcript)
  "Return the messages and the answers of TRANSCRIPT as plain text."
  (with-output-to-string (stream)
    (loop for entry across (transcript-entries transcript)
          do (typecase entry
               (user-entry
                (format stream "You: ~A~2%" (user-entry-text entry)))
               (assistant-entry
                (format stream "~A~2%" (assistant-entry-text entry)))))))
