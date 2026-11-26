(in-package #:aui)

;;;; -- Retained File Windows and Searches --

(defparameter *file-tool-row-limit* 80
  "The most source or search rows drawn in an expanded file tool result.")

(-> file-tool--clip (string) string)
(defun file-tool--clip (text)
  "Limit one retained row before passing it to the highlighter or layout."
  (if (> (length text) 512)
      (format nil "~A… [~D more characters]" (subseq text 0 512) (- (length text) 512))
      text))

(-> file-tool--note (string) span)
(defun file-tool--note (text)
  "Set an annotation apart from the source it describes."
  (make-span text (text-style ':code :face ':mono-italic)))

(-> file-tool--source (string (or null string)) list)
(defun file-tool--source (text path)
  "Color retained TEXT by PATH, without opening a file."
  (let* ((shown (file-tool--clip text))
         (parts (and path (source-highlight shown :pathname path))))
    (if parts (code--role-spans parts) (list (make-span shown nil)))))

(-> file-tool--window (list) list)
(defun file-tool--window (details)
  "Return annotated source spans for the line triples of a resource.read result."
  (let* ((uri      (getf details :uri))
         (lines    (getf details :lines))
         (elisions (getf details :elisions))
         (path     (highlight-resource-path uri)))
    (when (and lines (alexandria:proper-list-p lines)
               (every (lambda (row) (typep row '(cons integer (cons string (cons string null))))) lines)
               (alexandria:proper-list-p elisions) (every #'stringp elisions))
      (append
       (loop for row in lines for number = (first row) for text = (third row)
             for index below *file-tool-row-limit*
             append (append (when (plusp index) (list (make-span (string #\Newline) nil)))
                            (list (file-tool--note (format nil "~D  " number)))
                            (file-tool--source text path)))
       (when (> (length lines) *file-tool-row-limit*)
         (list (file-tool--note (format nil "~%… ~D more source lines" (- (length lines) *file-tool-row-limit*)))))
       (when elisions
         (list (file-tool--note (format nil "~%Elided: ~{~A~^; ~}" elisions))))))))

(-> file-tool--search-path (string) (or null string))
(defun file-tool--search-path (line)
  "Read the path from a PATH:LINE:COLUMN heading, which may have a git annotation."
  (let* ((end    (or (search " [git:" line :from-end t) (length line)))
         (column (position #\: line :end end :from-end t))
         (number (and column (position #\: line :end column :from-end t))))
    (when (and number (plusp number) (< (1+ number) column) (< (1+ column) end)
               (every #'digit-char-p (subseq line (1+ number) column))
               (every #'digit-char-p (subseq line (1+ column) end)))
      (subseq line 0 number))))

(-> file-tool--snippet (string string) list)
(defun file-tool--snippet (line path)
  "Separate the context or match marker and line number from a search snippet."
  (let* ((match-p (uiop:string-prefix-p "  > " line))
         (end     (and (or match-p (uiop:string-prefix-p "  | " line))
                       (position-if-not #'digit-char-p line :start 4))))
    (when (and end (> end 4) (<= (+ end 2) (length line)) (string= "  " line :start2 end :end2 (+ end 2)))
      (cons (make-span (subseq line 0 (+ end 2))
                       (if match-p (text-style ':code :face ':mono-bold :color *blue*)
                           (text-style ':code :face ':mono-italic)))
            (file-tool--source (subseq line (+ end 2)) path)))))

(-> file-tool--search-spans (string boolean) list)
(defun file-tool--search-spans (output content-p)
  "Lay out the retained fff page, preserving its summary and continuation cursor."
  (let* ((lines (markdown-lines (string-right-trim '(#\Newline) output)))
         (count (length lines))
         (path  nil))
    (loop for line in lines
          for i from 0
          for shown = (file-tool--clip line)
          when (or (< i *file-tool-row-limit*) (= i (1- count)))
            append
            (let* ((snippet (and content-p path (file-tool--snippet shown path)))
                   (heading (and content-p (not snippet) (file-tool--search-path shown)))
                   (tags    (and (not content-p) (search " [git:" shown :from-end t))))
              (unless snippet (setf path heading))
              (append
               (when (plusp i) (list (make-span (string #\Newline) nil)))
               (when (> i *file-tool-row-limit*)
                 (list (file-tool--note (format nil "… ~D more rows~%" (- i *file-tool-row-limit*)))))
               (cond
                 (snippet
                  snippet)
                 (heading
                  (list (make-span shown (text-style ':code :face ':mono-bold))))
                 (tags
                  (list (make-span (subseq shown 0 tags) (text-style ':code :face ':mono-bold))
                        (file-tool--note (subseq shown tags))))
                 (t
                  (list (file-tool--note shown)))))))))

(-> file-tool--read-output (string (or null string)) list)
(defun file-tool--read-output (output uri)
  "Show a non-source resource or an older read from its retained output, omitting only a repeated URI heading."
  (let* ((lines (markdown-lines (string-right-trim '(#\Newline) output)))
         (body  (if (and uri (equal (first lines) (format nil "URI: ~A" uri))) (rest lines) lines)))
    (when body
      (list (make-span (format nil "~{~A~^~%~}~@[~%… ~D more rows~]"
                               (loop for line in body repeat *file-tool-row-limit* collect (file-tool--clip line))
                               (and (> (length body) *file-tool-row-limit*) (- (length body) *file-tool-row-limit*)))
                       nil)))))

(-> file-tool--property-list (t) list)
(defun file-tool--property-list (value)
  "Return VALUE if it can be safely inspected as a property list."
  (and (alexandria:proper-list-p value) (evenp (length value)) value))

(-> entries--file-tool-blocks (tool-entry) list)
(defun entries--file-tool-blocks (entry)
  "Return expanded blocks for a retained file window or search page, or NIL for generic rendering."
  (let* ((name        (tool-entry-name entry))
         (read-p      (equal name "resource.read"))
         (content-p   (equal name "search.content"))
         (details     (and read-p (file-tool--property-list
                                   (getf (file-tool--property-list (tool-entry-details entry)) :resource-read))))
         (output      (tool-entry-output entry))
         (failed-p    (eq (tool-entry-status entry) ':failed))
         (completed-p (eq (tool-entry-status entry) ':completed)))
    (when (and (or read-p content-p (member name '("search.files" "search.glob") :test #'equal))
               (or completed-p failed-p))
      (let* ((cause (and failed-p (listp details) (getf details :cause)))
             (spans (cond
                      (failed-p
                       (let ((reason (if (and (stringp cause) (plusp (length cause))) cause output)))
                         (when reason
                           (list (make-span (file-tool--clip (entries--preview reason :lines 10))
                                            (text-style ':code :color *red*))))))
                      (read-p
                       (or (and (listp details) (file-tool--window details))
                           (and output (plusp (length output))
                                (file-tool--read-output output (let ((uri (entries--argument entry "uri")))
                                                                 (and (stringp uri) uri))))))
                      ((and output (plusp (length output)))
                       (file-tool--search-spans output content-p))))
             (label (cond (failed-p
                           "Error")
                          (read-p
                           (if (and (listp details) (getf details :lines)) "Source" "Resource"))
                          (content-p
                           "Matches")
                          (t
                           "Files"))))
        (when spans
          (list (make-code-block "" :spans spans :label label :margin-bottom 8)))))))
