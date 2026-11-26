(in-package #:aui)

;;;; -- MCP display tests --

(fiveam:def-suite tool-mcp
  :description "MCP sections in expanded tool results."
  :in          aui)

(fiveam:in-suite tool-mcp)

(-> mcp-view-test--blocks (string t) list)
(defun mcp-view-test--blocks (output details)
  "Render OUTPUT and DETAILS from a recorded MCP failure."
  (entries--mcp-result-blocks
   (make-instance 'tool-entry :name "test.lookup" :status ':failed :details details :output output)))

(fiveam:test mcp-text-and-structured-fields
  (let* ((message "lookup failed: #.(example) {\"code\":2}")
         (json    "{\"flags\":[false,true]}")
         (output  (format nil "~A~2%Structured content:~%~A" message json))
         (details (list ':mcp-content :sections (list (list ':text 0 (length message))
                                                      (list ':structured (- (length output) (length json)) (length output)))
                        :structured-content     '(:mcp-object ("flags" (:mcp-array :mcp-false t)))))
         (blocks  (mcp-view-test--blocks output details)))
    (fiveam:is (equal '(nil "Structured fields") (mapcar #'code-block-label blocks)))
    (fiveam:is (equal (list message (format nil "flags: ~%  • false~%  • true"))
                      (mapcar (lambda (block) (paragraph-text (code-block-paragraph block))) blocks)))))

(fiveam:test mcp-literal-fallback
  (dolist (details '((:mcp-content :sections ((:text -1 4))) (:mcp-content :sections ((:text 0 5)))
                     (:mcp-content :sections ((:text 0 3) (:text 2 4))) (:mcp-content :sections ((:text 2 4)))
                     (:mcp-content :sections ((:text 0 2))) (:mcp-content :sections ((:structured 0 4)))
                     (:mcp-content :sections ((:structured 0 4)) :structured-content (:mcp-object . 1))
                     (:mcp-content :sections . 3) (:mcp-content :sections ((:text 0 4) . 2))
                     (:mcp-content :sections ((:other 0 4)))))
    (fiveam:is (null (mcp-view-test--blocks "oops" details)))))

(fiveam:test mcp-display-bounds
  (let* ((output (concatenate 'string (make-string 4096 :initial-element #\a) "error at end"))
         (block  (first (mcp-view-test--blocks output (list ':mcp-content :sections (list (list ':text 0 (length output)))))))
         (shown  (paragraph-text (code-block-paragraph block))))
    (fiveam:is (< (length shown) (length output)))
    (fiveam:is (search "omitted" shown))
    (fiveam:is (uiop:string-suffix-p shown "error at end")))
  (let* ((sections (loop for i below 20 collect (list ':text i (1+ i))))
         (blocks   (mcp-view-test--blocks "abcdefghijklmnopqrst" (list ':mcp-content :sections sections))))
    (fiveam:is (= 13 (length blocks)))
    (fiveam:is (typep (nth 8 blocks) 'text-block))
    (fiveam:is (string= "t" (paragraph-text (code-block-paragraph (first (last blocks))))))))
