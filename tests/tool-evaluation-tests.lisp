(in-package #:aui)

;;;; -- Evaluation Display Tests --

(fiveam:def-suite tool-evaluation
  :description "Evaluation sections in expanded tool calls."
  :in          aui)

(fiveam:in-suite tool-evaluation)

(fiveam:test evaluation-results
  (flet ((sections (name status details)
           (loop for block in (entries--evaluation-result-blocks
                               (make-instance 'tool-entry :name name :status status :details details))
                 collect (list (code-block-label block) (paragraph-text (code-block-paragraph block))))))
    (fiveam:is (equal '(("description" "(printed, not Lisp)") ("values" "(no values)"))
                      (sections "lisp.describe" ':completed
                                '(:evaluation (:output "(printed, not Lisp)" :values "")))))
    (fiveam:is (equal '(("values" "NIL"))
                      (sections "procedure.run" ':completed '(:evaluation (:values "NIL")))))
    (fiveam:is (equal (list '("condition" "unbound variable") '("output" "before failure")
                            (list "available restarts" (format nil "USE: supply value~%ABORT: stop"))
                            '("retry" "supply :restart") '("backtrace" "0: frame"))
                      (sections "self.eval" ':failed
                                '(:evaluation (:condition "unbound variable" :output "before failure"
                                               :restarts (("USE" "supply value") ("ABORT" "stop"))
                                               :retry "supply :restart" :backtrace "0: frame")))))
    (fiveam:is (null (sections "lisp.eval" ':failed '(:evaluation (:condition "error" :restarts (broken))))))
    (let ((text (second (first (sections "lisp.eval" ':completed
                                         (list :evaluation (list :values (make-string 20000 :initial-element #\a))))))))
      (fiveam:is (< (length text) 20000))
      (fiveam:is (search "4000 more characters" text)))))
