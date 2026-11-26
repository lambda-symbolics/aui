(in-package #:aui)

;;;; -- Entries as Blocks --

(defparameter *tool-preview-keys*
  '("command" "forms" "form" "uri" "path" "query" "pattern" "patterns" "url" "system" "name" "task" "text")
  "A list of the argument names that are tried in order for the one-line preview of a tool call.")

(defparameter *output-preview-lines* 14
  "The number of lines shown from the detail of an opened notice.")

(defparameter *output-open-lines* 200
  "The number of lines shown from the result of an opened tool call.")

(defgeneric entry-blocks (entry toggle)
  (:documentation "Return the blocks that draw ENTRY. TOGGLE is a function of no arguments that opens or closes the detail of the entry."))

(-> entries--value-text (t) string)
(defun entries--value-text (value)
  "Return the decoded JSON VALUE as text on one line."
  (cond
    ((stringp value)
     value)
    ((eq value ':false)
     "false")
    ((eq value ':null)
     "null")
    ((eq value t)
     "true")
    ((and (consp value) (consp (first value)) (stringp (first (first value))))
     (format nil "{~{~A~^, ~}}"
             (mapcar (lambda (member)
                       (format nil "~A: ~A" (first member) (entries--value-text (rest member))))
                     value)))
    ((listp value)
     (format nil "~{~A~^, ~}" (mapcar #'entries--value-text value)))
    (t
     (princ-to-string value))))

(-> entries--one-line (string) string)
(defun entries--one-line (text)
  "Return TEXT with every run of whitespace replaced by one space. Line breaks count as whitespace."
  (let ((result  (make-string-output-stream))
        (space-p nil))
    (loop for character across (string-trim '(#\Space #\Newline #\Tab #\Return) text)
          do (if (member character '(#\Space #\Newline #\Tab #\Return))
                 (setf space-p t)
                 (progn
                   (when space-p
                     (write-char #\Space result)
                     (setf space-p nil))
                   (write-char character result))))
    (get-output-stream-string result)))

(-> tool-entry-preview (tool-entry) string)
(defun tool-entry-preview (entry)
  "Return the one-line preview of the tool call ENTRY. The preview is the command, path, query or first argument."
  (let* ((arguments (tool-entry-arguments entry))
         (member    (or (loop for key in *tool-preview-keys*
                              for found = (assoc key arguments :test #'string=)
                              when found
                                do (return found))
                        (first arguments))))
    (if member
        (entries--one-line (entries--value-text (rest member)))
        "")))

(-> entries--duration (real) string)
(defun entries--duration (seconds)
  "Return SECONDS as a readable duration, such as 0.4s, 12s or 3m 05s."
  (cond
    ((< seconds 10)
     (format nil "~,1Fs" seconds))
    ((< seconds 60)
     (format nil "~Ds" (round seconds)))
    (t
     (multiple-value-bind (minutes rest) (floor (round seconds) 60)
       (format nil "~Dm ~2,'0Ds" minutes rest)))))

(-> entries--clock (integer) string)
(defun entries--clock (universal-time)
  "Return the local hour and minute of UNIVERSAL-TIME."
  (multiple-value-bind (second minute hour) (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))


;;;; -- Messages --

(defvar *entry-channel-opener* nil
  "A function that takes the identifier of a channel and shows that channel, or NIL.

If it is NIL, messages and notices do not lead to the channel they came from.")

(-> entries--message-body (user-entry) list)
(defun entries--message-body (entry)
  "Return the blocks of the message ENTRY. The images come first, then the text.

The text is Markdown, and the line breaks in it are kept."
  (let ((text (user-entry-text entry)))
    (append
     (mapcar (lambda (pathname)
               (make-instance 'image-block :pathname pathname :margin-bottom 8))
             (user-entry-images entry))
     (when (plusp (length text))
       (let ((blocks (prose-blocks text :breaks-p t)))
         (when blocks
           (setf (block-margin-top (first blocks))           0
                 (block-margin-bottom (first (last blocks))) 0))
         blocks)))))

(-> entries--participant-blocks (user-entry list) list)
(defun entries--participant-blocks (entry body)
  "Return the blocks of the message ENTRY from another participant.

The blocks are the name of the sender in large letters, BODY, and a button that
leads to the channel the message came from."
  (let ((origin (user-entry-origin entry))
        (opener *entry-channel-opener*)
        (name   (text-style ':heading-2)))
    (list (make-instance
           'box-block
           :children      (append
                           (list (make-text-block (spans (list (user-entry-sender entry) name))
                                                  :style name :margin-bottom 4))
                           body
                           (when (and origin opener)
                             (list (make-instance 'button-block
                                                  :identifier (list ':entry-channel (entry-identifier entry))
                                                  :label      (format nil "go to ~A" (channel-display-name origin))
                                                  :action     (lambda () (funcall opener origin))
                                                  :margin-top 10))))
           :frame-p       nil
           :padding-x     0
           :padding-y     0
           :margin-top    22
           :margin-bottom 22))))

(defmethod entry-blocks ((entry user-entry) toggle)
  "Draw a message from the person in a framed box at the right.

Draw a message from another participant at the left without a frame, under the name of the sender."
  (declare (ignore toggle))
  (let ((body (entries--message-body entry)))
    (if (user-entry-sender entry)
        (entries--participant-blocks entry body)
        (list (make-instance 'box-block
                             :children      (or body (list (make-text-block (list (make-span "" nil)))))
                             :align         ':right
                             :fraction      85/100
                             :padding-x     16
                             :padding-y     10
                             :margin-top    22
                             :margin-bottom 22)))))

(defvar *answers-under-way* (make-hash-table :test #'eq :weakness ':key)
  "A table of the answers that are still streaming. The value for each answer is (END . BLOCKS).

BLOCKS are the finished blocks of the answer, and END is the position in the text where they end.")

(-> entries--streaming-blocks (assistant-entry) list)
(defun entries--streaming-blocks (entry)
  "Return the blocks of the text of ENTRY so far.

The blocks of the settled part of the text are kept between calls. Only the text
after the settled part is typeset again."
  (let* ((text    (assistant-entry-text entry))
         (known   (gethash entry *answers-under-way*))
         (from    (if known (first known) 0))
         (end     (markdown-settled-end text :start from))
         (settled (append (rest known) (and (> end from) (prose-blocks (subseq text from end))))))
    (setf (gethash entry *answers-under-way*) (cons end settled))
    (append settled (prose-blocks (subseq text end)))))

(defmethod entry-blocks ((entry assistant-entry) toggle)
  "Draw the answer of the agent as prose. A finished answer is typeset whole."
  (declare (ignore toggle))
  (let ((blocks (if (assistant-entry-streaming-p entry)
                    (entries--streaming-blocks entry)
                    (progn
                      (remhash entry *answers-under-way*)
                      (prose-blocks (assistant-entry-text entry))))))
    (when blocks
      (setf (block-margin-top (first blocks)) (max 8 (block-margin-top (first blocks)))))
    blocks))

(defmethod entry-blocks ((entry reasoning-entry) toggle)
  "Draw the reasoning of the agent as a closed row. The reasoning is shown in small italics in the opened row."
  (let* ((text       (reasoning-entry-text entry))
         (expanded-p (entry-expanded-p entry))
         (label      (text-style ':label :face ':mono-italic))
         (row        (make-instance 'row-block
                                    :identifier (list ':entry (entry-identifier entry))
                                    :mark       (cond
                                                  ((reasoning-entry-streaming-p entry)
                                                   ':spinner)
                                                  (expanded-p
                                                   ':open)
                                                  (t
                                                   ':closed))
                                    ;; The emphasis of the summary is drawn without the Markdown markers.
                                    :title      (cons (make-span (if (reasoning-entry-streaming-p entry)
                                                                     "thinking  "
                                                                     "thought  ")
                                                                 label)
                                                      (prose-spans
                                                       (markdown-inline
                                                        (entries--one-line
                                                         (subseq text 0 (min 200 (length text)))))
                                                       (text-style ':label)))
                                    :style      label
                                    :action     toggle
                                    :margin-top 2)))
    (if expanded-p
        (list row
              (make-instance 'box-block
                             :children      (prose-blocks text :style (text-style ':small :face ':serif-italic))
                             :frame-p       nil
                             :padding-x     28
                             :padding-y     2
                             :margin-bottom 8))
        (list row))))


;;;; -- Tools --

(-> entries--plan-steps (tool-entry) list)
(defun entries--plan-steps (entry)
  "Return the blocks of the plan update ENTRY.

The explanation comes first. The steps follow as a checklist, and the current step is bold blue."
  (let* ((arguments   (tool-entry-arguments entry))
         (explanation (rest (assoc "explanation" arguments :test #'string=)))
         (steps       (rest (or (assoc "plan" arguments :test #'string=)
                                (assoc "steps" arguments :test #'string=))))
         (style       (text-style ':list)))
    (append
     (when (and (stringp explanation) (plusp (length explanation)))
       (list (make-text-block (list (make-span explanation nil))
                              :style (text-style ':small :face ':serif-italic) :margin-bottom 6)))
     (loop for step in (and (listp steps) steps)
           for text = (and (consp step)
                           (rest (or (assoc "step" step :test #'string=)
                                     (assoc "text" step :test #'string=))))
           for status = (and (consp step) (rest (assoc "status" step :test #'string=)))
           when (stringp text)
             collect (make-text-block
                      (list (make-span text
                                       (cond
                                         ((member status '("in_progress" "doing") :test #'equal)
                                          (text-style ':list-bold :color *blue*))
                                         ((member status '("completed" "done") :test #'equal)
                                          (text-style ':list :strike-p t))
                                         (t
                                          nil))))
                      :style  style
                      :indent 26
                      :marker (if (member status '("completed" "done") :test #'equal) ':done ':open)
                      :margin-top 2 :margin-bottom 2)))))

(-> entries--plan-blocks (tool-entry) list)
(defun entries--plan-blocks (entry)
  "Return the blocks of the plan update ENTRY, which are a framed box with the heading plan."
  (list
   (make-instance
    'box-block
    :padding-x     14
    :padding-y     10
    :margin-top    10
    :margin-bottom 10
    :children      (cons (make-text-block (spans (list "plan" (text-style ':label-bold)))
                                          :style (text-style ':label-bold) :margin-bottom 4)
                         (entries--plan-steps entry)))))

(defparameter *diff-preview-lines* 40
  "The number of lines shown from each side of an edit in an opened row.")

(-> entries--diff-spans (string string) list)
(defun entries--diff-spans (old new)
  "Return the spans of an edit as a diff. The lines of OLD are marked as removed, then the lines of NEW are marked as added.

Removed lines are red and added lines are green."
  (let ((removed (text-style ':code :color *red*))
        (added   (text-style ':code :color *green*))
        (spans   nil))
    (flet ((side (text mark style)
             (let ((lines (markdown-lines (string-right-trim '(#\Newline) text))))
               (loop for line in lines
                     for index from 0
                     while (< index *diff-preview-lines*)
                     do (push (make-span (format nil "~A~A~%" mark line) style) spans))
               (when (> (length lines) *diff-preview-lines*)
                 (push (make-span (format nil "  … ~D more lines~%" (- (length lines) *diff-preview-lines*))
                                  (text-style ':code :face ':mono-italic))
                       spans)))))
      (when (plusp (length old))
        (side old "- " removed))
      (when (plusp (length new))
        (side new "+ " added)))
    (nreverse spans)))

(-> entries--argument (tool-entry &rest string) t)
(defun entries--argument (entry &rest names)
  "Return the value of the first argument of ENTRY that has one of NAMES, or NIL."
  (loop for name in names
        for member = (assoc name (tool-entry-arguments entry) :test #'string=)
        when member
          do (return (rest member))))

(-> entries--preview (string &key (:lines integer)) string)
(defun entries--preview (text &key (lines *output-preview-lines*))
  "Show the beginning and end of TEXT, marking material omitted for display."
  (let* ((length (length text))
         (sample (if (> length 16000)
                     (format nil "~A~%… ~D characters omitted …~%~A" (subseq text 0 12000) (- length 16000) (subseq text (- length 4000)))
                     text))
         (rows   (markdown-lines (string-right-trim '(#\Newline) sample)))
         (count  (length rows))
         (tail   (max 1 (floor lines 4))))
    (if (<= count lines) sample
        (format nil "~{~A~%~}… ~D lines omitted …~%~{~A~^~%~}"
                (subseq rows 0 (- lines tail)) (- count lines) (last rows tail)))))

(defparameter *lisp-argument-names* '("forms" "form" "definition" "source" "expression" "code")
  "A list of the argument names that hold the source for tools that evaluate Lisp.")

(-> entries--lisp-tool-p (tool-entry) boolean)
(defun entries--lisp-tool-p (entry)
  "Return T if the tool of ENTRY takes and returns Lisp. These tools are the evaluators of the workspace image and of Autolith."
  (let ((name (tool-entry-name entry)))
    (and (or (uiop:string-prefix-p "lisp." name)
             (uiop:string-prefix-p "self." name))
         t)))

(-> entries--lisp-source (tool-entry) (values (or null string) (or null string)))
(defun entries--lisp-source (entry)
  "Return the Lisp source of the tool call ENTRY and the name of the argument that contains it.

The source has one form on each line, and long one-line forms are laid out on
several lines. Return NIL twice if the call has no Lisp source."
  (loop for name in *lisp-argument-names*
        for value = (entries--argument entry name)
        when value
          do (return (values (lisp-format (if (and (listp value) (every #'stringp value))
                                              (format nil "~{~A~^~%~}" value)
                                              (entries--value-text value)))
                             name))
        finally (return (values nil nil))))

(-> entries--lisp-output (string) string)
(defun entries--lisp-output (text)
  "Return TEXT, the output of a Lisp tool, with each long one-line form laid out on several lines."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (line)
                    (let ((trimmed (string-left-trim '(#\Space) line)))
                      (if (and (plusp (length trimmed)) (find (char trimmed 0) "(#'"))
                          (lisp-format line)
                          line)))
                  (markdown-lines text))))

(-> entries--result-blocks (tool-entry string) list)
(defun entries--result-blocks (entry output)
  "Render tool output, with separate blocks for captured shell streams."
  (let* ((raw      (tool-entry-details entry))
         (details  (and (equal (tool-entry-name entry) "shell.run") (job-view--fields-p raw) raw))
         (sections (getf details :output-streams))
         (streams  (and (alexandria:proper-list-p sections)
                        (every (lambda (section)
                                 (and (job-view--fields-p section)
                                      (member (getf section :stream) '(:combined :stdout :stderr))
                                      (typep (getf section :head) '(or null string))
                                      (typep (getf section :tail) '(or null string)))) sections)
                        sections))
         (logs     (getf details :shell-logs))
         (log      (and (consp logs) (job-view--fields-p (first logs)) (first logs)))
         (exit     (or (getf details :exit-code) (getf log :exit-code)))
         (status   (remove nil (list (and (integerp exit) (not (zerop exit)) (format nil "exit ~D" exit))
                                     (and (or (getf details :timed-out-p) (getf log :timed-out-p)) "timed out")
                                     (and (or (getf details :cancelled-p) (getf log :cancelled-p)) "cancelled")
                                     (and (getf details :capture-diagnostic) (format nil "capture: ~A" (getf details :capture-diagnostic))))))
         (lisp-p   (and (not (eq (tool-entry-status entry) ':failed))
                        (member (tool-entry-name entry) '("lisp.eval" "self.eval" "lisp.source") :test #'equal))))
    (flet ((block-for (text label)
             (let ((shown (entries--preview (if lisp-p (entries--lisp-output text) text)
                                            :lines *output-open-lines*)))
               (make-code-block shown :label label :margin-bottom 8
                                :spans (cond (lisp-p
                                              (code-spans shown "lisp"))
                                             ((patch-p shown)
                                              (patch-spans shown)))))))
      (append (when status (list (make-text-block (list (make-span (format nil "~{~A~^ · ~}" status) nil))
                                                  :style         (text-style ':label-bold :color (if (eq (tool-entry-status entry) ':failed) *red* *ink*))
                                                  :margin-bottom 6)))
              (if streams
                  (loop for section in streams
                        for stream = (getf section :stream)
                        for label = (if (member stream '(:stdout :stderr)) (string-downcase stream) "output")
                        unless (and (getf section :complete-p) (not (getf section :unavailable-p))
                                    (not (getf section :omitted-p)) (zerop (length (getf section :head)))
                                    (zerop (length (getf section :tail))))
                        collect (block-for (concatenate 'string (or (getf section :head) "")
                                                        (if (getf section :omitted-p) (format nil "~%…~%") "")
                                                        (or (getf section :tail) ""))
                                           (cond ((getf section :unavailable-p)
                                                  (format nil "~A · unavailable" label))
                                                 ((not (getf section :complete-p))
                                                  (format nil "~A · incomplete" label))
                                                 (t
                                                  (if (eq stream ':combined) nil label)))))
                  (when (plusp (length output))
                    (list (block-for output (if (eq (tool-entry-status entry) ':failed) "Failed" "Result")))))))))

(-> entries--ordinary-input (list) list)
(defun entries--ordinary-input (arguments)
  "Show the human-authored part of the request without its transport fields."
  (loop for (key . value) in arguments
        when (and (stringp value) (member key '("task" "text" "content" "title") :test #'equal))
          collect (make-text-block (list (make-span (entries--preview value :lines 40) nil))
                                   :style (text-style ':small) :margin-bottom 8)))

(-> entries--argument-blocks (list) list)
(defun entries--argument-blocks (arguments)
  "Lay out ordinary arguments as labeled fields. Indent nested objects and array members."
  (when arguments
    (let ((remaining 100))
      (labels ((row (out key value depth)
                 (when (plusp remaining)
                   (decf remaining)
                   (format out "~&~V@T~A:" (* 2 depth) key)
                   (cond ((and (consp value) (< depth 6))
                          (if (and (consp (first value)) (stringp (first (first value))))
                              (loop for (label . item) in value while (plusp remaining) do (row out label item (1+ depth)))
                              (loop for item in value for index from 1 while (plusp remaining) do (row out index item (1+ depth)))))
                         ((consp value)
                          (write-string " … nested value" out))
                         (t
                          (format out " ~A" (entries--preview (entries--value-text value) :lines 20)))))))
        (list (make-code-block (entries--preview
                                (with-output-to-string (out)
                                  (loop for (key . value) in arguments while (plusp remaining) do (row out key value 0))
                                  (when (zerop remaining) (format out "~%… field limit reached"))) :lines 100)
                               :label "Input" :margin-bottom 8))))))

(-> entries--edit-operation (list) string)
(defun entries--edit-operation (operation)
  "Describe the requested change without revision tokens or line anchors."
  (flet ((field (key) (rest (assoc key operation :test #'equal))))
    (let* ((op (field "op")) (start (field "start-line")) (end (field "end-line"))
           (action (substitute #\Space #\- (or (portable-string op) "edit"))))
      (with-output-to-string (out)
        (cond ((and (integerp start) (integerp end))
               (format out "~A ~D~:[~;–~D~]" action start (/= start end) end))
              ((integerp (field "line"))
               (format out "~A line ~D" action (field "line")))
              (t
               (write-string action out)))
        (dolist (pair operation)
          (unless (member (first pair) '("op" "line" "anchor" "start-line" "end-line" "start-anchor" "end-anchor") :test #'equal)
            (format out "~%~A~A" (if (equal (first pair) "content") "" (format nil "~A: " (first pair)))
                    (entries--value-text (rest pair)))))))))

(-> entries--resource-edit-blocks (tool-entry boolean) list)
(defun entries--resource-edit-blocks (entry expanded-p)
  "Draw a compact patch inline and the full change when the file row is opened."
  (let* ((patch      (tool-entry-patch entry))
         (output     (tool-entry-output entry))
         (details    (getf (tool-entry-details entry) :resource-edit))
         (operations (entries--argument entry "operations")))
    (cond ((and (eq (tool-entry-status entry) ':completed) (stringp patch))
           (let ((text (if expanded-p patch (entries--preview patch :lines *diff-preview-lines*))))
             (append (list (make-code-block text :spans   (patch-spans text :pathname (highlight-resource-path (entries--argument entry "uri")))
                                           :margin-bottom 8))
                     (when (and expanded-p output (getf details :delimiter-warning-p))
                       (entries--result-blocks entry output)))))
          ((not expanded-p)
           nil)
          ((and (eq (tool-entry-status entry) ':failed) output)
           (entries--result-blocks entry output))
          (t
           (append (loop for operation in (and (listp operations) operations)
                         when (and (consp operation) (every #'consp operation))
                           collect (make-code-block (entries--edit-operation operation)
                                                    :label (if (and (eq (tool-entry-status entry) ':completed)
                                                                    (member (rest (assoc "op" operation :test #'equal))
                                                                            '("replace-lines" "delete-lines") :test #'equal))
                                                               "Original lines unavailable"
                                                               "Requested edit")))
                   (when (and output (or (getf details :delimiter-warning-p) (null operations)))
                     (entries--result-blocks entry output)))))))

(-> entries--tool-detail (tool-entry &optional function) flow-block)
(defun entries--tool-detail (entry &optional (toggle (constantly nil)))
  "Return the opened detail of the tool call ENTRY.

The detail contains the input of the call and the result. The input is shown as a
command, Lisp source, a diff or a list of arguments."
  (let* ((arguments (tool-entry-arguments entry))
         (output    (tool-entry-output entry))
         (command   (and (equal (tool-entry-name entry) "shell.run") (entries--argument entry "command")))
         (old-text  (entries--argument entry "old-text" "old_text" "old"))
         (new-text  (entries--argument entry "new-text" "new_text" "new" "content")))
    (multiple-value-bind (source source-name) (if (entries--lisp-tool-p entry)
                                                  (entries--lisp-source entry)
                                                  (values nil nil))
      (let* ((parameters (unless command
                           (remove-if (lambda (pair)
                                        (or (equal (first pair) source-name)
                                            (and (stringp (rest pair)) (member (first pair) '("task" "text" "content" "title") :test #'equal))))
                                      arguments)))
             (evaluation (and (job-view--fields-p (tool-entry-details entry)) (getf (tool-entry-details entry) :evaluation)))
             (trace      (and (equal (tool-entry-status entry) ':failed) (job-view--fields-p evaluation)
                              (portable-string (getf evaluation :backtrace)))))
       (make-instance
        'box-block
        :frame-p       nil
        :padding-x     28
        :padding-y     2
        :margin-bottom 10
        :children
        (or (entries--file-tool-blocks entry)
            (append
             (cond
               ((stringp command)
                (list (make-text-block (code-spans command "bash") :style (text-style ':code) :margin-bottom 6)))
               (source
                (list (make-text-block (code-spans source "lisp") :style (text-style ':code) :margin-bottom 6)))
               ((and (equal (tool-entry-name entry) "fs.edit") (or (stringp old-text) (stringp new-text)))
                (let* ((path (entries--argument entry "path" "uri"))
                       (old  (if (stringp old-text) old-text ""))
                       (new  (if (stringp new-text) new-text "")))
                  (list (make-code-block ""
                                         :label         (if (stringp path) path "change")
                                         :spans         (or (edit-spans old new :pathname (and (stringp path) path))
                                                            (entries--diff-spans old new))
                                         :margin-bottom 8))))
               (t
                (entries--ordinary-input arguments)))
             (or (entries--job-tool-blocks entry) (entries--evaluation-result-blocks entry) (entries--mcp-result-blocks entry)
                 (when (or output (tool-entry-details entry)) (entries--result-blocks entry (or output ""))))
             (when (or parameters trace)
               (cons (make-instance 'row-block :identifier (list ':call-details (entry-identifier entry))
                                    :mark                  (if (tool-entry-show-details-p entry) ':open ':closed)
                                    :title (list (make-span "Details" nil)) :style (text-style ':meta)
                                    :action                (lambda () (funcall toggle t)))
                     (when (tool-entry-show-details-p entry)
                       (append (entries--argument-blocks parameters)
                               (when trace (list (make-code-block (entries--preview trace :lines 60) :label "Backtrace"))))))))))))))

(-> entries--preview-spans (tool-entry) list)
(defun entries--preview-spans (entry)
  "Return the spans of the one-line preview of ENTRY.

Commands are highlighted as shell, Lisp is highlighted as Lisp, and the rest is plain."
  (let* ((preview  (tool-entry-preview entry))
         (language (cond
                     ((stringp (entries--argument entry "command"))
                      "bash")
                     ((entries--lisp-tool-p entry)
                      "lisp")
                     (t
                      nil)))
         (parts    (and language (source-highlight preview :language language))))
    (if parts
        (loop for (role . text) in parts
              collect (let ((style (code-role-style role)))
                        ;; The row is smaller than a code block. However, it has the same inks.
                        (make-span text (if style
                                            (text-style ':label
                                                        :face  (text-style-face style)
                                                        :color (text-style-color style))
                                            (text-style ':label)))))
        (list (make-span preview (text-style ':label))))))

(defmethod entry-blocks ((entry tool-entry) toggle)
  "Draw a tool call as one row that contains the name of the tool and the preview.

The arguments and the result are shown in the opened row. A plan update is drawn as the plan."
  (if (and (string= (tool-entry-name entry) "plan.update") (eq (tool-entry-status entry) ':completed))
      (entries--plan-blocks entry)
      (let* ((status     (tool-entry-status entry))
             (expanded-p (entry-expanded-p entry))
             (seconds    (tool-entry-seconds entry))
             (row        (make-instance
                          'row-block
                          :identifier (list ':entry (entry-identifier entry))
                          :mark       (ecase status
                                        ((:pending :running)
                                         ':spinner)
                                        (:completed
                                         (if expanded-p ':open ':closed))
                                        (:failed
                                         ':cross))
                          :mark-color (and (eq status ':failed) *red*)
                          :title      (cons (make-span (format nil "~A  " (tool-entry-name entry))
                                                       (text-style ':label-bold
                                                                   :color (if (eq status ':failed) *red* *amber*)))
                                            (if (and expanded-p (equal (tool-entry-name entry) "shell.run")
                                                     (stringp (entries--argument entry "command")))
                                                nil
                                                (entries--preview-spans entry)))
                          :trailing   (and seconds (entries--duration seconds))
                          :style      (text-style ':label)
                          :action     toggle)))
          (cond ((equal (tool-entry-name entry) "resource.edit")
                 (cons row (entries--resource-edit-blocks entry expanded-p)))
                (expanded-p
                 (list row (entries--tool-detail entry toggle)))
                (t
                 (list row))))))


;;;; -- Notices, Output and Markers --

(-> entries--detail-sections (string) (values list (or null string)))
(defun entries--detail-sections (detail)
  "Return DETAIL as one raw section for legacy notices."
  (values (list (cons "detail" detail)) nil))

(-> entries--last-line (string) string)
(defun entries--last-line (text)
  "Return the last nonblank line of TEXT."
  (or (find-if (lambda (line)
                 (plusp (length (string-trim '(#\Space #\Tab) line))))
               (markdown-lines text)
               :from-end t)
      ""))

(defmethod entry-blocks ((entry notice-entry) toggle)
  "Draw a notice as a quiet row in the voice of the window.

If the notice has detail, the last line of the detail is shown in the closed row. The row opens on a click."
  (let* ((tone       (notice-entry-tone entry))
         (detail     (notice-entry-detail entry))
         (expanded-p (and detail (entry-expanded-p entry)))
         (color      (ecase tone
                       (:quiet
                        nil)
                       (:success
                        *green*)
                       (:warning
                        *amber*)
                       (:failure
                        *red*))))
    (multiple-value-bind (sections status) (if detail (entries--detail-sections detail) (values nil nil))
      (let* ((preview (and detail
                           (not expanded-p)
                           (format nil "~@[~A  ~]~A" status
                                   (entries--one-line
                                    (entries--last-line (or (rest (first (last sections))) ""))))))
             (row     (make-instance 'row-block
                                     :identifier (list ':entry (entry-identifier entry))
                                     :mark       (cond
                                                   (expanded-p
                                                    ':open)
                                                   (detail
                                                    ':closed)
                                                   (t
                                                    (ecase tone
                                                      (:quiet
                                                       ':point)
                                                      (:success
                                                       ':check)
                                                      ((:warning :failure)
                                                       ':cross))))
                                     :mark-color color
                                     :title      (spans (list (notice-entry-text entry)
                                                              (text-style ':label :color color))
                                                        (and preview
                                                             (list (format nil "  ~A" preview)
                                                                   (text-style ':label :face ':mono-italic))))
                                     :trailing   (and (entry-time entry) (entries--clock (entry-time entry)))
                                     :style      (text-style ':label)
                                     ;; A notice with detail opens. A notice about
                                     ;; another channel leads to that channel.
                                     :action     (cond
                                                   (detail
                                                    toggle)
                                                   ((and (notice-entry-channel entry) *entry-channel-opener*)
                                                    (let ((opener  *entry-channel-opener*)
                                                          (channel (notice-entry-channel entry)))
                                                      (lambda () (funcall opener channel)))))
                                     :margin-top 4)))
        (if expanded-p
            (list row
                  (make-instance 'box-block
                                 :children      (loop for (label . text) in sections
                                                      for first-p = t then nil
                                                      collect (make-code-block
                                                               (entries--preview text)
                                                               :label         (if (and first-p status)
                                                                                  (format nil "~A · ~A" label status)
                                                                                  label)
                                                               :margin-bottom 4))
                                 :frame-p       nil
                                 :padding-x     28
                                 :padding-y     2
                                 :margin-bottom 8))
            (list row))))))

(defmethod entry-blocks ((entry schemas-entry) toggle)
  "Draw a quiet row that lists the groups of tools looked up by the agent."
  (declare (ignore toggle))
  (list (make-instance 'row-block
                       :identifier (list ':entry (entry-identifier entry))
                       :mark       ':point
                       :title      (cons (make-span "tool schemas  " (text-style ':label))
                                         (loop for name in (schemas-entry-names entry)
                                               for first-p = t then nil
                                               unless first-p
                                                 collect (make-span ", " (text-style ':label))
                                               collect (make-span name (text-style ':label :color *blue*))))
                       :style      (text-style ':label)
                       :margin-top 4)))

(defmethod entry-blocks ((entry output-entry) toggle)
  "Draw the output of a command in the typewriter face, under the form that printed it."
  (declare (ignore toggle))
  (list (make-code-block (output-entry-text entry)
                         :label         (output-entry-invocation entry)
                         :margin-top    12
                         :margin-bottom 12)))

(defmethod entry-blocks ((entry marker-entry) toggle)
  "Draw the marker as a rule across the column, with the label of the marker."
  (declare (ignore toggle))
  (list (make-instance 'rule-block
                       :label         (string-downcase (marker-entry-label entry))
                       :margin-top    18
                       :margin-bottom 18)))
