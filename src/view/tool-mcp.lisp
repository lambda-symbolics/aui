(in-package #:aui)

;;;; -- MCP results --

(-> mcp-view--slice (string integer integer) string)
(defun mcp-view--slice (text start end)
  "Return the section between START and END in TEXT, with both ends of long sections."
  (let ((head (loop for i from start below (min end (+ start 1536))
                    count (char= (char text i) #\Newline) into lines
                    when (= lines 24) return i
                    finally (return (min end (+ start 1536)))))
        (tail (loop for i downfrom (1- end) to (max start (- end 512))
                    count (char= (char text i) #\Newline) into lines
                    when (= lines 8) return (1+ i)
                    finally (return (max start (- end 512))))))
    (if (<= tail head)
        (subseq text start end)
        (format nil "~A~%… ~D characters omitted from view …~%~A"
                (subseq text start head) (- tail head) (subseq text tail end)))))

(-> mcp-view--fields (t) (or null string))
(defun mcp-view--fields (value)
  "Lay out the recorded fields of VALUE. Return NIL for an unfamiliar or oversized tree."
  (let ((count 0))
    (with-output-to-string (out)
      (labels ((write-node (node depth prefix)
                 (when (or (> (incf count) 96) (> depth 8)) (return-from mcp-view--fields nil))
                 (when prefix
                   (fresh-line out)
                   (format out "~V@T~A" (* 2 (max 0 (1- depth))) prefix))
                 (cond ((member node '(nil t :mcp-false))
                        (write-string (case node ((nil)
                                                  "null")
                                                 ((t)
                                                  "true")
                                                 (otherwise
                                                  "false")) out))
                       ((and (stringp node) (<= (length node) 256))
                        (princ node out))
                       ((or (floatp node) (typep node '(signed-byte 256)))
                        (princ node out))
                       ((and (consp node) (member (first node) '(:mcp-object :mcp-array)))
                        (let ((object-p (eq (first node) ':mcp-object)))
                          (unless (rest node) (write-string (if object-p "{}" "[]") out))
                          (loop for tail = (rest node) then (rest tail)
                                while tail
                                for i from 1
                                unless (and (consp tail) (<= i 12)) do (return-from mcp-view--fields nil)
                                do (let ((item (first tail)))
                                     (if object-p
                                         (progn
                                           (unless (and (typep item '(cons string (cons t null)))
                                                        (<= (length (first item)) 96))
                                             (return-from mcp-view--fields nil))
                                           (write-node (second item) (1+ depth) (format nil "~A: " (first item))))
                                         (write-node item (1+ depth) "• "))))))
                       (t
                        (return-from mcp-view--fields nil)))))
        (write-node value 0 nil)))))

(-> mcp-view--sections (t string) list)
(defun mcp-view--sections (details output)
  "Check the ordered section ranges in DETAILS against OUTPUT, including inter-section labels."
  (when (and (consp details) (eq (first details) ':mcp-content)
             (alexandria:proper-list-p (rest details)) (evenp (length (rest details))))
    (let ((sections (getf (rest details) :sections))
          (end      0))
      (loop for tail = sections then (rest tail)
            while tail
            for i from 1
            do (unless (and (<= i 257) (consp tail)
                            (typep (first tail) '(cons (member :text :structured) (cons integer (cons integer null)))))
                 (return-from mcp-view--sections nil))
               (destructuring-bind (kind from to) (first tail)
                 (let ((gap (concatenate 'string (if (= i 1) "" (format nil "~2%"))
                                         (if (eq kind ':structured) (format nil "Structured content:~%") ""))))
                   (unless (and (<= end from to (length output))
                                (or (= end from) (string= gap output :start2 end :end2 from))
                                (or (eq kind ':text) (null (rest tail))))
                     (return-from mcp-view--sections nil)))
                 (setf end to)))
      (when (= end (length output)) sections))))

(-> entries--mcp-result-blocks (tool-entry) list)
(defun entries--mcp-result-blocks (entry)
  "Return MCP text and structured fields for ENTRY, or NIL to show the literal result."
  (let* ((output   (tool-entry-output entry))
         (details  (tool-entry-details entry))
         (sections (and output (mcp-view--sections details output)))
         (count    (length sections)))
    (loop for (kind start end) in sections
          for index from 1
          when (or (<= index 8) (> index (- count 4)))
            collect (let* ((structured-p (eq kind ':structured))
                           (fields       (and structured-p (getf (rest details) :structured-content)
                                              (mcp-view--fields (getf (rest details) :structured-content)))))
                      (when (and structured-p (not fields)) (return-from entries--mcp-result-blocks nil))
                      (make-code-block (or fields (mcp-view--slice output start end))
                                       :label (if fields "structured fields" "text") :margin-bottom 8))
          else when (= index 9)
                 collect (make-text-block (list (make-span (format nil "… ~D sections omitted from view" (- count 12)) nil))
                                          :margin-bottom 8))))
