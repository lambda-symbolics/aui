(in-package #:aui)

;;;; -- Resource Edit History --

(-> edit-history--field (list string) t)
(defun edit-history--field (arguments field)
  "Return FIELD from the decoded JSON ARGUMENTS."
  (rest (assoc field arguments :test #'equal)))

(-> edit-history--header (tool-entry) (values (or null string) (or null string) (or null integer) &optional))
(defun edit-history--header (entry)
  "Read the canonical URI and revision from the result header of ENTRY, never from its content."
  (let ((output (tool-entry-output entry)))
    (when (and output (or (eq (tool-entry-status entry) ':completed)
                          (and (equal (tool-entry-name entry) "resource.read")
                               (eq (tool-entry-status entry) ':failed)
                               (uiop:string-prefix-p "No file exists at " output)))
               (member (tool-entry-name entry) '("resource.read" "resource.edit") :test #'equal))
      (multiple-value-bind (start end groups ends)
          (cl-ppcre:scan "(?m)^URI: ([^\\n]+)\\nRevision: ([^\\n]+)\\n" output :end (min 4096 (length output)))
        (when (and start (or (zerop start)
                             (and (equal (tool-entry-name entry) "resource.read")
                                  (uiop:string-prefix-p "Kind: missing" (subseq output end))
                                  (equal (subseq output 0 start)
                                         (format nil "No file exists at ~A; resource.edit can create it at the revision below.~%"
                                                 (subseq output (aref groups 0) (aref ends 0)))))
                             (and (equal (tool-entry-name entry) "resource.edit")
                                  (uiop:string-prefix-p "Applied " output)
                                  (not (search "Content:" output :end2 start)))))
          (return-from edit-history--header
            (values (subseq output (aref groups 0) (aref ends 0))
                    (subseq output (aref groups 1) (aref ends 1)) end))))))
  (values nil nil nil))

(-> edit-history--collect (tool-entry integer hash-table) boolean)
(defun edit-history--collect (entry header-end rows)
  "Collect hash-checked, numbered lines from ENTRY in ROWS. Return true for an observed empty file."
  (let* ((output (tool-entry-output entry))
         (marker (format nil "~%Content:~%"))
         (body   (search marker output :start2 header-end)))
    (when (equal (string-right-trim '(#\Newline) (subseq output header-end)) "Kind: missing")
      (return-from edit-history--collect t))
    (when (and body (<= (length output) 32768))
      (with-input-from-string (stream output :start (+ body (length marker)))
        (loop for line = (read-line stream nil) while line do
          (cl-ppcre:register-groups-bind (number anchor text)
              ("^ *([0-9]{1,12}):([A-F0-9]{4})  (.*)\\z" line)
            (when (string= anchor (cl-hashline:line-anchor text))
              (setf (gethash (parse-integer number) rows) (cons anchor text))))))
      (and (search (format nil "~%Visible lines: none of 0~%") output :end2 body) t))))

(-> edit-history--line (list hash-table string &key (:anchor string)) (or null integer))
(defun edit-history--line (operation rows field &key anchor)
  "Resolve FIELD against observed ROWS, allowing Autolith's three-line anchor correction."
  (let ((line (edit-history--field operation field))
        (hash (edit-history--field operation anchor)))
    (when (typep line '(integer 1))
      (if hash
          (let ((matches (loop for n from (max 1 (- line 3)) to (+ line 3)
                               when (equalp hash (first (gethash n rows))) collect n)))
            (and (null (rest matches)) (first matches)))
          (and (gethash line rows) line)))))

(-> edit-history--hunk (list hash-table boolean) list)
(defun edit-history--hunk (operation rows empty-p)
  "Return (first-line removed added) for OPERATION, or NIL without its exact preimage."
  (let* ((kind    (edit-history--field operation "op"))
         (content (edit-history--field operation "content"))
         (added   (when (and (stringp content) (<= (length content) 65536))
                    (coerce (autolith::text--split-lines content) 'list))))
    (unless (or (equal kind "delete-lines") (and (stringp content) (<= (length content) 65536)))
      (return-from edit-history--hunk nil))
    (cond
      ((member kind '("delete-lines" "replace-lines") :test #'equal)
       (let ((start (edit-history--line operation rows "start-line" :anchor "start-anchor"))
             (end   (edit-history--line operation rows "end-line" :anchor "end-anchor")))
         (when (and start end (<= start end))
           (list start (loop for n from start to end for row = (gethash n rows)
                             unless row do (return-from edit-history--hunk nil)
                             collect (rest row)) added start (format nil "~A ~D-~D" kind start end)))))
      ((member kind '("insert-before" "insert-after") :test #'equal)
       (let ((line (edit-history--line operation rows "line" :anchor "anchor")))
         (and line (list (+ line (if (equal kind "insert-after") 1 0)) nil added line (format nil "~A ~D" kind line)))))
      ((and empty-p (equal kind "replace-empty"))
       (list 1 nil added 0 "replace-empty")))))

(-> edit-history--patch (string list) string)
(defun edit-history--patch (uri hunks)
  "Format original-coordinate HUNKS as a zero-context logical-line diff for URI."
  (with-output-to-string (out)
    (format out "--- ~A~%+++ ~A~%" uri uri)
    (loop with delta = 0 for (start old new) in hunks
          for minus = (length old) for plus = (length new) do
            (format out "@@ -~D,~D +~D,~D @@~%" (if old start (1- start)) minus
                    (+ delta (if new start (1- start))) plus)
            (dolist (line old) (format out "-~A~%" line))
            (dolist (line new) (format out "+~A~%" line))
            (incf delta (- plus minus)))))

(-> edit-history--reconstruct (entry vector integer) (or null string))
(defun edit-history--reconstruct (entry entries position)
  "Try the preceding 128 transcript entries for an exact base of this completed edit."
  (when (and (typep entry 'tool-entry) (equal (tool-entry-name entry) "resource.edit")
             (eq (tool-entry-status entry) ':completed) (not (tool-entry-patch entry)))
    (let* ((args       (tool-entry-arguments entry))
           (base       (edit-history--field args "base-revision"))
           (operations (edit-history--field args "operations"))
           (uri        (edit-history--header entry))
           (rows       (make-hash-table))
           (empty-p nil))
      (when (and uri (stringp base) operations (listp operations) (<= (length operations) 100)
                 (every (lambda (op) (and (listp op) (every #'consp op))) operations))
        (loop for n downfrom (1- position) to (max 0 (- position 128))
              for previous = (aref entries n) when (typep previous 'tool-entry) do
                (multiple-value-bind (path revision end) (edit-history--header previous)
                  (when (and (equal path uri) (equal revision base))
                    (when (edit-history--collect previous end rows) (setf empty-p t)))))
        (let ((hunks (loop for op in operations for hunk = (edit-history--hunk op rows empty-p)
                           unless hunk do (return nil) collect hunk)))
          (when (and hunks (uiop:string-prefix-p
                            (format nil "Applied ~{~A~^; ~}."
                                    (mapcar #'fifth (setf hunks (sort hunks #'< :key #'fourth))))
                            (tool-entry-output entry)))
            (setf (tool-entry-patch entry) (edit-history--patch uri hunks))))))))
