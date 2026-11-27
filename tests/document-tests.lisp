(in-package #:aui)

;;;; -- Document Tests --

(fiveam:def-suite document
  :description "Tests for the document, in which only the items near the view are laid out."
  :in          aui)

(fiveam:in-suite document)

(defclass test-block (flow-block)
  ()
  (:documentation "A block without text.
The height of the block only depends on the width.
The block can be laid out without a display."))

(defmethod block-layout ((block test-block) width)
  "Return a height of a hundred units at a WIDTH of four hundred.
The height grows as WIDTH gets smaller."
  (floor 40000 width))

(defmethod block-paint ((block test-block) x y)
  "Draw nothing."
  (declare (ignore x y))
  nil)

(defclass test-entry (entry)
  ()
  (:documentation "An entry that is typeset as one test block."))

(defmethod entry-blocks ((entry test-entry) toggle)
  "Return a list of one test block."
  (declare (ignore toggle))
  (list (make-instance 'test-block)))

(-> test-document (integer) document)
(defun test-document (count)
  "Return a document of COUNT test entries.
The view is four hundred units tall, and the column is four hundred units wide.
The estimated height of an entry is equal to the real height."
  (let* ((transcript               (make-transcript))
         (document                 (make-document transcript))
         (*document-item-estimate* 100))
    (dotimes (index count)
      (transcript-append transcript (make-instance 'test-entry)))
    (setf (document--viewport document) (list 0 0 400 400))
    (document-sync document 400)
    document))

(-> test-laid-out (document) list)
(defun test-laid-out (document)
  "Return the indices of the items in DOCUMENT that have blocks."
  (loop for item across (document--items document)
        for index from 0
        when (item-blocks item)
          collect index))

(fiveam:test only-what-is-near-the-view-is-laid-out
  (let ((document (test-document 1000)))
    (fiveam:is (null (test-laid-out document)))
    (document--bring-near document)
    (fiveam:is (equal '(0 1 2 3 4 5 6 7) (test-laid-out document)))))

(fiveam:test items-far-from-the-view-keep-their-height-and-nothing-else
  (let ((document (test-document 1000)))
    (fiveam:is (= 100000 (document-content-height document)))
    (setf (document-scroll document) 50000)
    (document--bring-near document)
    (let ((laid (test-laid-out document)))
      (fiveam:is (member 500 laid))
      (fiveam:is (< (length laid) (+ 20 (* 2 *document-kept-items*))))
      (fiveam:is (null (member 0 laid)))
      (fiveam:is (null (member 999 laid))))
    (fiveam:is (= 100000 (document-content-height document)))
    ;; Scroll back to the start. The items there are laid out again.
    (setf (document-scroll document) 0)
    (document--bring-near document)
    (fiveam:is (member 0 (test-laid-out document)))
    (fiveam:is (null (member 500 (test-laid-out document))))))

(fiveam:test a-new-width-is-met-by-what-is-near-and-the-view-stays-on-what-it-showed
  (let ((document (test-document 1000)))
    (setf (document-following-p document) nil
          (document-scroll document)      50000)
    (document--bring-near document)
    ;; At half the width, the items that are laid out again are twice as tall.
    (document-sync document 200)
    (document--bring-near document)
    (let ((items (document--items document)))
      (fiveam:is (= 200 (item-height (aref items 500))))
      (fiveam:is (= 100 (item-height (aref items 0))))
      ;; The same entry is at the top of the view as before.
      (fiveam:is (= (item-top (aref items 500)) (document-scroll document))))))

(fiveam:test prose-input-is-not-a-file-addition
  (let* ((entry  (make-instance 'tool-entry :name "papercut.report" :status ':completed
                                :arguments '(("content" . "Clicking Retry closed the window.") ("tags" "UI" "Linux"))
                                :output    "Recorded."))
         (blocks (box-block-children (entries--tool-detail entry))))
    (fiveam:is (typep (first blocks) 'text-block))
    (fiveam:is (search "Clicking Retry" (paragraph-text (text-block-paragraph (first blocks)))))
    (fiveam:is (typep (third blocks) 'row-block))
    (let* ((document (make-document (make-transcript))))
      (setf (entry-expanded-p entry) t)
      (transcript-append (document-transcript document) entry)
      (funcall (row-block-action (third (box-block-children (second (document--entry-blocks document entry))))))
      (let ((fields (first (last (box-block-children (entries--tool-detail entry))))))
        (fiveam:is (search "Linux" (paragraph-text (code-block-paragraph fields))))))))

(fiveam:test a-failed-plan-shows-the-error-instead-of-a-checklist
  (let* ((entry  (make-instance 'tool-entry :name "plan.update" :status ':failed :expanded-p t
                                :output "Unknown step."))
         (detail (second (entry-blocks entry (constantly nil)))))
    (fiveam:is (equal "Unknown step." (paragraph-text (code-block-paragraph (first (box-block-children detail))))))))

(fiveam:test bounded-tool-output-retains-the-final-diagnostic
  (let* ((message (format nil "Start~%~A~%Disk full" (make-string 20000 :initial-element #\a)))
         (preview (entries--preview message :lines 3)))
    (fiveam:is (uiop:string-prefix-p "Start" preview))
    (fiveam:is (uiop:string-suffix-p preview "Disk full"))
    (fiveam:is (search "omitted" preview))
    (fiveam:is (< (length preview) (length message)))))

(fiveam:test edited-source-has-syntax-inside-the-diff-gutters
  (let* ((patch (format nil "--- workspace:x.lisp~%+++ workspace:x.lisp~%@@ -1,1 +1,2 @@~%-(defun old () 1)~%+(defun new ()~%+  2)~%"))
         (parts (patch-spans patch :pathname "x.lisp")))
    (fiveam:is (equal patch (format nil "~{~A~}" (mapcar #'span-text parts))))
    (dolist (change (list (cons "-" *red*) (cons "+" *green*)))
      (let ((mark (find (first change) parts :key #'span-text :test #'equal)))
        (fiveam:is (and mark (equal (text-style-color (span-style mark)) (rest change))))))
    (fiveam:is (= 2 (count-if (lambda (part)
                                (and (search "defun" (span-text part)) (span-style part)
                                     (equal *plum* (text-style-color (span-style part))))) parts)))))

(fiveam:test shell-detail-is-command-and-output-not-capture-bookkeeping
  (let* ((entry  (make-instance 'tool-entry :name "shell.run" :status ':completed
                                :arguments '(("command" . "make test") ("description" . "test") ("async" . :false))
                                :output    "capture summary"
                                :details   '(:exit-code 0 :output-streams ((:stream :combined :complete-p t :head "ok")
                                                                           (:stream :stderr :complete-p t :head ""))
                                             :shell-logs ((:captures ((:reference "shell-log:example")))))))
         (blocks (box-block-children (entries--tool-detail entry))))
    (fiveam:is (= 2 (length blocks)))
    (fiveam:is (equal "make test" (paragraph-text (text-block-paragraph (first blocks)))))
    (fiveam:is (equal "ok" (paragraph-text (code-block-paragraph (second blocks)))))
    (setf (tool-entry-status entry) ':failed (getf (tool-entry-details entry) :exit-code) 2)
    (let ((failure (second (box-block-children (entries--tool-detail entry)))))
      (fiveam:is (search "2" (paragraph-text (text-block-paragraph failure)))))))

(fiveam:test a-protected-folder-refusal-is-distinguished-from-other-tool-failures
  (let* ((report "resource.read failed: Path /example/private/notes.txt is denied by the folder blacklist.")
         (entry  (make-instance 'tool-entry :name "resource.read" :status ':failed :output report))
         (row    (first (entry-blocks entry (constantly nil)))))
    (fiveam:is (find "Blocked by folder policy" (row-block-title row) :key #'span-text :test #'equal))
    (setf (tool-entry-output entry) "resource.read failed: Permission denied.")
    (fiveam:is (not (entries--protected-path-refusal-p entry)))
    (setf (tool-entry-output entry) report (tool-entry-status entry) ':completed)
    (fiveam:is (not (entries--protected-path-refusal-p entry)))))
