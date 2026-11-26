(in-package #:aui)

;;;; -- Evaluation Results --

(-> evaluation--preview (string) string)
(defun evaluation--preview (text)
  "Return a literal preview of TEXT, with a count when the display omits characters."
  (let ((end (loop with limit = (min (length text) 16000)
                   for index below limit
                   count (char= (char text index) #\Newline) into lines
                   when (= lines 200) return index
                   finally (return limit))))
    (if (< end (length text))
        (format nil "~A~%… ~D more characters" (subseq text 0 end) (- (length text) end))
        text)))

(-> evaluation--sections-p (t) boolean)
(defun evaluation--sections-p (sections)
  "Return T if SECTIONS contains the text and restart pairs of an evaluation record."
  (and (alexandria:proper-list-p sections)
       (evenp (length sections))
       (loop for (key value) on sections by (partial #'nthcdr 2)
             always (case key
                      ((:condition :output :values :backtrace :retry)
                       (stringp value))
                      (:restarts
                       (and (alexandria:proper-list-p value) (<= (length value) 16)
                            (every (lambda (pair) (typep pair '(cons string (cons string null)))) value)))
                      (otherwise
                       nil)))))

(-> entries--evaluation-result-blocks (tool-entry) list)
(defun entries--evaluation-result-blocks (entry)
  "Return result blocks for a typed evaluation in ENTRY, or NIL for the ordinary result view."
  (let* ((details  (tool-entry-details entry))
         (sections (and (alexandria:proper-list-p details) (evenp (length details))
                        (getf details :evaluation))))
    (when (and (evaluation--sections-p sections)
               (if (eq (tool-entry-status entry) ':failed)
                   (portable-string (getf sections :condition))
                   (stringp (getf sections :values))))
      (let ((restarts (getf sections :restarts)))
        (flet ((section (label text &key lisp-p)
                 (when (portable-string text)
                   (let ((shown (evaluation--preview text)))
                     (make-code-block shown :label label :margin-bottom 8
                                      :spans (and lisp-p (code-spans shown "lisp")))))))
          (remove nil
                  (list (section "condition" (getf sections :condition))
                        (section (if (equal (tool-entry-name entry) "lisp.describe") "description" "output")
                                 (getf sections :output))
                        (when restarts
                          (section "available restarts"
                                   (format nil "~:{~A: ~A~:^~%~}"
                                           (mapcar (lambda (pair) (mapcar #'evaluation--preview pair)) restarts))))
                        (section "retry" (getf sections :retry))
                        (when (and (getf sections :values)
                                   (or (portable-string (getf sections :values))
                                       (not (portable-string (getf sections :output)))))
                          (section "values" (or (portable-string (getf sections :values)) "(no values)")
                                   :lisp-p (plusp (length (getf sections :values))))))))))))
