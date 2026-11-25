(in-package #:aui)

;;;; -- Expanded File Tool Tests --

(fiveam:def-suite tool-files
  :description "Presentation of persisted file windows and search pages."
  :in          aui)

(fiveam:in-suite tool-files)

(-> file-test--texts (tool-entry) list)
(defun file-test--texts (entry)
  "Return the displayed and copyable text of each expanded block for ENTRY."
  (loop for block in (entries--file-tool-blocks entry)
        collect (paragraph-text (code-block-paragraph block))))

(fiveam:test a-read-uses-its-retained-window
  (let* ((entry (make-instance 'tool-entry :name "resource.read" :status ':completed
                               :arguments '(("uri" . "workspace:sample.lisp") ("start-line" . 10))
                               :details   '(:resource-read (:uri      "workspace:sample.lisp"
                                                            :lines    ((10 "02AC" "(print :hello)"))
                                                            :elisions ("1-9 before")))))
         (texts (file-test--texts entry)))
    (fiveam:is (= 2 (length texts)))
    (fiveam:is (search "start-line" (first texts)))
    (fiveam:is (not (search "workspace:" (first texts))))
    (fiveam:is (search "10  (print :hello)" (second texts)))
    (fiveam:is (search "1-9 before" (second texts)))
    (setf (tool-entry-details entry) nil)
    (fiveam:is (null (entries--file-tool-blocks entry)))))

(fiveam:test a-failed-read-displays-the-retained-cause
  (let ((entry (make-instance 'tool-entry :name "resource.read" :status ':failed
                              :output "read failed" :details '(:resource-read (:cause "File not found.")))))
    (fiveam:is (equal '("File not found.") (file-test--texts entry)))))

(fiveam:test a-search-retains-headings-context-and-metadata
  (let* ((output (format nil "1 match~%C:/project/sample.lisp:7:1 [git:clean]~%  | 6  ~%  > 7  (print :ok)~%next-file-offset: 2"))
         (entry  (make-instance 'tool-entry :name "search.content" :status ':completed :output output
                                :arguments '(("query" . "print") ("mode" . "regex"))))
         (texts  (file-test--texts entry)))
    (fiveam:is (search "regex" (first texts)))
    (fiveam:is (not (search "print" (first texts))))
    (fiveam:is (equal output (second texts)))
    (fiveam:is (equal "C:/project/sample.lisp" (file-tool--search-path "C:/project/sample.lisp:7:1 [git:clean]")))
    (fiveam:is (null (file-tool--search-path "src/a:bad:1")))
    (fiveam:is (span-style (first (file-tool--snippet "  > 7  (print :ok)" "sample.lisp"))))))

(fiveam:test file-searches-keep-the-cursor-when-the-page-is-clipped
  (let* ((*file-tool-row-limit* 2)
         (output                (format nil "3 results shown, 8 matched, 10 indexed; page 0.~%src/a [git:clean] [12 bytes]~%src/b~%src/c~%next-page: 1")))
    (dolist (name '("search.files" "search.glob"))
      (let ((text (first (file-test--texts (make-instance 'tool-entry :name name :status ':completed :output output)))))
        (fiveam:is (search "src/a [git:clean] [12 bytes]" text))
        (fiveam:is (search "2 more rows" text))
        (fiveam:is (search "next-page: 1" text))
        (fiveam:is (not (search "src/b" text)))))))

(fiveam:test a-non-file-read-shows-the-resource-body-once
  (let ((entry (make-instance 'tool-entry :name "resource.read" :status ':completed
                              :arguments '(("uri" . "memory:personal"))
                              :output    (format nil "URI: memory:personal~%An observed fact"))))
    (fiveam:is (equal '("An observed fact") (file-test--texts entry)))))

(fiveam:test malformed-resource-metadata-falls-back-to-its-output
  (dolist (details '(:unavailable (:resource-read) (:resource-read :unavailable)
                     (:resource-read (:lines)) (:resource-read (:lines ((1 "ABCD" "text") . 2)))))
    (let ((entry (make-instance 'tool-entry :name "resource.read" :status ':completed
                                :details details :output "The retained text")))
      (fiveam:is (equal '("The retained text") (file-test--texts entry))))))
