(in-package #:aui)

;;;; -- Prose --

(defparameter *prose-paragraph-gap* 12
  "The space between two paragraphs of an answer.")

(defparameter *prose-item-indent* 26
  "The indentation of one level of a list.")

(-> prose--inline-style (text-style list) (or null text-style))
(defun prose--inline-style (base properties)
  "Return the style of an inline node that has PROPERTIES inside text set in BASE, or NIL if the style is BASE."
  (let ((bold-p   (getf properties :bold))
        (italic-p (getf properties :italic))
        (code-p   (getf properties :code))
        (link     (getf properties :link))
        (strike-p (getf properties :strike)))
    (if (or bold-p italic-p code-p link strike-p)
        (let ((style (copy-text-style base)))
          (cond
            (code-p
             (let ((code (text-style ':code)))
               (setf (text-style-face style) (face-variant ':mono :bold-p bold-p :italic-p italic-p)
                     ;; Inline code is a little smaller than the text and has the same leading.
                     (text-style-size style) (max 11 (- (text-style-size base) 3))
                     (text-style-line-height style) (max (text-style-line-height base)
                                                         (text-style-line-height code)))))
            (t
             (let* ((face          (text-style-face base))
                    (base-bold-p   (member face '(:serif-bold :serif-bold-italic :mono-bold :mono-bold-italic)))
                    (base-italic-p (member face '(:serif-italic :serif-bold-italic :mono-italic
                                                  :mono-bold-italic))))
               (setf (text-style-face style)
                     (face-variant face
                                   :bold-p   (and (or bold-p base-bold-p) t)
                                   :italic-p (and (or italic-p base-italic-p) t))))))
          (when (or link code-p)
            (setf (text-style-color style) *blue*))
          (when link
            (setf (text-style-underline-p style) t))
          (when strike-p
            (setf (text-style-strike-p style) t))
          style)
        nil)))

(-> prose-spans (list text-style) list)
(defun prose-spans (inlines base)
  "Return the spans of the inline nodes INLINES for text set in BASE."
  (loop for (text . properties) in inlines
        collect (make-span text (prose--inline-style base properties) :link (getf properties :link))))

(-> prose--table (list list) table-block)
(defun prose--table (header rows)
  "Return the table block made from HEADER and ROWS. HEADER is a list of cells, ROWS is a list of
lists of cells, and a cell is a list of inline nodes."
  (let ((bold (text-style ':list-bold))
        (body (text-style ':list)))
    (make-instance 'table-block
                   :rows          (cons (mapcar (lambda (cell)
                                                  (make-paragraph (prose-spans cell bold) :style bold))
                                                header)
                                        (mapcar (lambda (row)
                                                  (mapcar (lambda (cell)
                                                            (make-paragraph (prose-spans cell body) :style body))
                                                          row))
                                                rows))
                   :margin-top    *prose-paragraph-gap*
                   :margin-bottom *prose-paragraph-gap*)))

(-> prose-blocks (string &key (:style text-style) (:breaks-p boolean)) list)
(defun prose-blocks (text &key (style (text-style ':body)) breaks-p)
  "Return the blocks of the Markdown TEXT, with the running text set in STYLE.

If BREAKS-P is true, the line breaks inside a paragraph are kept."
  (let ((blocks nil))
    (dolist (node (markdown-blocks text :breaks-p breaks-p))
      (push
       (ecase (first node)
         (:heading
          (destructuring-bind (level inlines) (rest node)
            (let ((heading (text-style (case level
                                         (1
                                          ':heading-1)
                                         (2
                                          ':heading-2)
                                         (otherwise
                                          ':heading-3)))))
              (make-text-block (prose-spans inlines heading)
                               :style         heading
                               :margin-top    (if (= level 1) 26 20)
                               :margin-bottom 6))))
         (:paragraph
          (make-text-block (prose-spans (second node) style)
                           :style         style
                           :margin-top    *prose-paragraph-gap*
                           :margin-bottom *prose-paragraph-gap*))
         (:item
          (destructuring-bind (depth marker inlines) (rest node)
            (make-text-block (prose-spans inlines style)
                             :style         style
                             :indent        (* *prose-item-indent* (1+ depth))
                             :marker        marker
                             :margin-top    4
                             :margin-bottom 4)))
         (:quote
          (let ((quoted (copy-text-style style)))
            (setf (text-style-face quoted) (face-variant (text-style-face style) :italic-p t))
            (make-text-block (prose-spans (second node) quoted)
                             :style         quoted
                             :indent        *prose-item-indent*
                             :margin-top    *prose-paragraph-gap*
                             :margin-bottom *prose-paragraph-gap*)))
         (:code
          (destructuring-bind (language code) (rest node)
            (make-code-block code
                             :label         language
                             :spans         (code-spans code language)
                             :margin-top    *prose-paragraph-gap*
                             :margin-bottom *prose-paragraph-gap*)))
         (:table
          (prose--table (second node) (third node)))
         (:rule
          (make-instance 'rule-block :margin-top 14 :margin-bottom 14)))
       blocks))
    (nreverse blocks)))


;;;; -- Code Highlighting --

;;; Code is colored by the highlighter of Autolith if Autolith has one for the language.
;;; Each role gets one of the inks of the theme. Other languages get a simpler
;;; pass by AUI: defining words are bold plum, strings green and comments italic.

(-> code-role-style (keyword) (or null text-style))
(defun code-role-style (role)
  "Return the style of code that has the highlighter role ROLE, or NIL for the plain code style."
  (case role
    ((:syntax-keyword :syntax-heading)
     (text-style ':code :face ':mono-bold :color *plum*))
    ((:syntax-function :syntax-type :syntax-property)
     (text-style ':code :color *blue*))
    (:syntax-string
     (text-style ':code :color *green*))
    ((:syntax-number :syntax-escape)
     (text-style ':code :color *amber*))
    (:syntax-comment
     (text-style ':code :face ':mono-italic))
    (:removed-gutter
     (text-style ':code :color *red*))
    ((:added-gutter :source-gutter)
     (text-style ':code :color *green*))
    ((:elision :prompt)
     (text-style ':code :face ':mono-italic))
    ;; Parentheses are colored by depth. The outermost are ink, deeper ones are accents.
    (:syntax-parenthesis-2
     (text-style ':code :color *blue*))
    (:syntax-parenthesis-3
     (text-style ':code :color *plum*))
    (:syntax-parenthesis-4
     (text-style ':code :color *green*))
    (:syntax-parenthesis-5
     (text-style ':code :color *amber*))
    (:syntax-parenthesis-6
     (text-style ':code :color *red*))
    (otherwise
     nil)))

(-> code--role-spans (list &key (:plain (or null text-style))) list)
(defun code--role-spans (parts &key plain)
  "Return the spans of PARTS, a list of (ROLE . TEXT) pairs.

Each part is set in the style of its role, or in PLAIN if the role has no style.
Neighbouring parts of the same role are joined into one span."
  (let ((spans nil)
        (roles nil))
    (loop for (role . text) in parts
          do (let ((style (or (code-role-style role) plain)))
               (if (and spans (eq role (first roles)))
                   (setf (span-text (first spans)) (concatenate 'string (span-text (first spans)) text))
                   (progn
                     (push (make-span text style) spans)
                     (push role roles)))))
    (nreverse spans)))

(defparameter *code-defining-words*
  '("defun" "defmacro" "defgeneric" "defmethod" "defclass" "defstruct" "defvar" "defparameter"
    "defpackage" "deftype" "define-condition" "lambda" "let" "let*" "flet" "labels" "if" "when"
    "unless" "cond" "case" "loop" "progn" "function" "def" "class" "return" "import" "from" "fn"
    "func" "const" "var" "for" "while" "else" "struct" "enum" "impl" "pub" "use" "match" "export"
    "async" "await" "static" "void" "public" "private" "interface" "type" "package" "elif" "try"
    "except" "raise" "with" "as" "in" "not" "and" "or" "then" "fi" "do" "done" "end")
  "A list of the words set bold in code, taken from the languages that agents write most often.")

(-> code--comment-start (string (or null string)) (or null integer))
(defun code--comment-start (line language)
  "Return the position of the start of a comment on LINE of LANGUAGE, or NIL if there is none.

Lisp comments start at a semicolon. Comments of most other languages start at // or #."
  (let ((lisp-p (member language '("lisp" "common-lisp" "cl" "elisp" "emacs-lisp" "scheme" "clojure")
                        :test #'equal)))
    (cond
      (lisp-p
       (position #\; line))
      ((member language '("python" "py" "sh" "bash" "shell" "zsh" "ruby" "yaml" "toml" "nix" "make")
               :test #'equal)
       (let ((hash (position #\# line)))
         (and hash (or (zerop hash) (char= (char line (1- hash)) #\Space)) hash)))
      (t
       (search "//" line)))))

(-> code--string-end (string integer) integer)
(defun code--string-end (source start)
  "Return the position after the string literal that starts at START in SOURCE.

If the string literal is not closed, return the length of SOURCE."
  (let ((quote (char source start)))
    (loop for index from (1+ start) below (length source)
          do (cond
               ((char= (char source index) #\\)
                (incf index))
               ((char= (char source index) quote)
                (return (1+ index))))
          finally (return (length source)))))

(-> code-spans (string (or null string)) list)
(defun code-spans (code language)
  "Return the colored spans of CODE in LANGUAGE.

The code is colored by the highlighter of Autolith if Autolith has one for LANGUAGE.
Otherwise, the code is colored by the simpler pass of AUI."
  (let ((parts (and language (source-highlight code :language language))))
    (if parts
        (code--role-spans parts)
        (code--light-spans code language))))

(-> edit-spans (string string &key (:pathname (or null string))) list)
(defun edit-spans (old new &key pathname)
  "Return the spans of a numbered diff that replaces OLD with NEW, or NIL if Autolith cannot render the edit.

The gutter and the plain text of removed lines are red. The gutter and the plain
text of added lines are green. Highlighted code has the ink of its role."
  (let ((rows (edit-highlight old new :pathname pathname)))
    (and rows
         (loop for (row . more) on rows
               append (code--role-spans row
                                        :plain (case (first (first row))
                                                 (:removed-gutter
                                                  (text-style ':code :color *red*))
                                                 (:added-gutter
                                                  (text-style ':code :color *green*))
                                                 (otherwise
                                                  nil)))
               when more
                 collect (make-span (string #\Newline) nil)))))

(-> patch-p (string) boolean)
(defun patch-p (text)
  "Return T if TEXT is a unified diff. A unified diff has a hunk header and added or removed lines."
  (let ((lines (markdown-lines text)))
    (and (some (lambda (line)
                 (and (> (length line) 3) (string= "@@ " line :end2 3)))
               lines)
         (some (lambda (line)
                 (and (plusp (length line)) (find (char line 0) "+-")))
               lines)
         t)))

(-> patch--source-spans (list string text-style) list)
(defun patch--source-spans (lines pathname gutter)
  "Highlight one run of added or removed lines together, without feeding the diff markers to the lexer."
  (let* ((mark  (subseq (first lines) 0 1))
         (body  (format nil "~{~A~^~%~}" (mapcar (lambda (line) (subseq line 1)) lines)))
         (parts (source-highlight body :pathname pathname))
         (spans (list (make-span mark gutter))))
    (unless (and parts (string= body (format nil "~{~A~}" (mapcar #'rest parts))))
      (setf parts (list (cons ':plain body))))
    (loop for (role . text) in parts for style = (or (code-role-style role) gutter) do
      (loop for start = 0 then (1+ end)
            for end = (position #\Newline text :start start)
            do (when (< start (or end (length text)))
                 (push (make-span (subseq text start end) style) spans))
               (when end (push (make-span (format nil "~%~A" mark) gutter) spans))
            while end))
    (nreverse spans)))

(-> patch-spans (string &key (:pathname (or null string))) list)
(defun patch-spans (text &key pathname)
  "Style unified diff headers and markers. For a file edit, color the changed source by PATHNAME."
  (let ((removed   (text-style ':code :color *red*))
        (added     (text-style ':code :color *green*))
        (hunk      (text-style ':code :color *blue*))
        (bold      (text-style ':code :face ':mono-bold))
        (in-hunk-p nil)
        (lines     (uiop:split-string text :separator '(#\Newline))))
    (loop while lines for line = (pop lines)
          for mark = (and (plusp (length line)) (char line 0))
          for change = (case mark (#\+
                                   added)
                                  (#\-
                                   removed))
          append (cond ((uiop:string-prefix-p "@@" line)
                        (setf in-hunk-p t)
                        (list (make-span line hunk)))
                       ((and pathname in-hunk-p change)
                        (patch--source-spans (cons line (loop while (and lines (plusp (length (first lines)))
                                                                        (char= mark (char (first lines) 0)))
                                                               collect (pop lines))) pathname change))
                       ((or (uiop:string-prefix-p "+++" line) (uiop:string-prefix-p "---" line)
                            (uiop:string-prefix-p "diff " line) (uiop:string-prefix-p "index " line))
                        (list (make-span line bold)))
                       (t
                        (list (make-span line change))))
          when lines collect (make-span (string #\Newline) nil))))

(-> code--light-spans (string (or null string)) list)
(defun code--light-spans (code language)
  "Return the spans of CODE in LANGUAGE.

Comments are italic, defining words are bold plum, strings are green and the rest is plain."
  (let ((bold   (text-style ':code :face ':mono-bold :color *plum*))
        (italic (text-style ':code :face ':mono-italic))
        (quoted (text-style ':code :color *green*))
        (spans  nil))
    (flet ((emit (text style)
             ;; Neighbouring text of the same style is joined into one span.
             (when (plusp (length text))
               (let ((previous (first spans)))
                 (if (and previous (eq (span-style previous) style))
                     (setf (span-text previous) (concatenate 'string (span-text previous) text))
                     (push (make-span text style) spans))))))
      (loop for (line . more) on (markdown-lines code)
            do (let* ((comment (code--comment-start line language))
                      (source  (subseq line 0 comment))
                      (index   0))
                 (loop while (< index (length source))
                       do (let* ((next       (position-if (lambda (character)
                                                            (or (alpha-char-p character) (char= character #\")))
                                                          source :start index))
                                 (string-p   (and next (char= (char source next) #\")))
                                 (word-start (and next (not string-p) next))
                                 (word-end   (and word-start
                                                  (or (position-if-not
                                                       (lambda (character)
                                                         (or (alphanumericp character) (find character "-*_")))
                                                       source :start word-start)
                                                      (length source)))))
                            (cond
                              ((null next)
                               (emit (subseq source index) nil)
                               (setf index (length source)))
                              (string-p
                               (let ((end (code--string-end source next)))
                                 (emit (subseq source index next) nil)
                                 (emit (subseq source next end) quoted)
                                 (setf index end)))
                              (t
                               (emit (subseq source index word-start) nil)
                               (let ((word (subseq source word-start word-end)))
                                 (emit word (and (member word *code-defining-words* :test #'string=)
                                                 (or (zerop word-start)
                                                     (not (alphanumericp (char source (1- word-start)))))
                                                 bold)))
                               (setf index word-end)))))
                 (when comment
                   (emit (subseq line comment) italic))
                 (when more
                   (emit (string #\Newline) nil)))))
    (or (nreverse spans) (list (make-span "" nil)))))
