(in-package #:aui)

(defvar *highlight-library-sought-p* nil
  "True if this process has already looked for the native highlighter library of a release.")

(-> highlight--seek-library () null)
(defun highlight--seek-library ()
  "Look for the native highlighter library of a release. Do nothing after the first call."
  (unless *highlight-library-sought-p*
    (setf *highlight-library-sought-p* t)
    (installation-adopt))
  nil)

(defparameter *lisp-languages* '("lisp" "common-lisp" "cl" "elisp" "emacs-lisp" "scheme" "clojure")
  "The code fence names of the languages written in parentheses.")

(defparameter *lisp-file-types* '("lisp" "asd" "cl" "el" "scm" "clj" "sexp")
  "The file types of the languages written in parentheses.")

(-> highlight--lisp-p ((or null string) (or null string)) boolean)
(defun highlight--lisp-p (language pathname)
  "Return T if LANGUAGE is written in parentheses.
If LANGUAGE is NIL, use the file type of PATHNAME instead."
  (and (if language
           (member language *lisp-languages* :test #'string-equal)
           (and pathname
                (member (pathname-type (pathname pathname)) *lisp-file-types* :test #'equal)))
       t))

(-> highlight-resource-path (t) (or null string))
(defun highlight-resource-path (uri)
  "Return the path used to classify a file resource's syntax, without reading the file."
  (when (stringp uri)
    (loop for prefix in '("workspace:" "scratchpad:" "skill-source:")
          when (uiop:string-prefix-p prefix uri) return (subseq uri (length prefix))
          finally (return (and (uiop:absolute-pathname-p uri) uri)))))

(-> source-highlight (string &key (:language (or null string)) (:pathname (or null string))) list)
(defun source-highlight (source &key language pathname)
  "Return (ROLE . TEXT) spans for SOURCE, or NIL if highlighting is unavailable.
Use LANGUAGE for a fenced block or infer it from PATHNAME. Color Lisp parentheses by depth."
  (highlight--seek-library)
  (handler-case
      (let* ((found (and language
                         (autolith::language-find (string-downcase language) :errorp nil)))
             (spans (and (or found pathname)
                         (autolith::syntax--highlight-spans
                          source :language found :pathname pathname
                          :rainbow-parentheses-p (highlight--lisp-p language pathname)))))
        (mapcar (lambda (span)
                  (cons (autolith::terminal-span-style span) (autolith::terminal-span-text span)))
                spans))
    (error ()
      nil)))

(-> edit-highlight (string string &key (:pathname (or null string)) (:limit integer)) list)
(defun edit-highlight (old new &key pathname (limit 60))
  "Return the rows of the edit that replaced OLD with NEW, or NIL if the edit cannot be rendered.
Each row is a list of (ROLE . TEXT).

Each row starts with a gutter of the role :CONTEXT-GUTTER, :REMOVED-GUTTER or
:ADDED-GUTTER. The gutter contains the line number and the kind of change. The
rest of the row is the source of the line, colored for the language of the
file PATHNAME. At most LIMIT rows of the change are returned."
  (highlight--seek-library)
  (handler-case
      (autolith::render-diff :removed-content (and (plusp (length old)) old)
                             :added-content   (and (plusp (length new)) new)
                             :source-path     pathname
                             :line-limit      limit
                             :span-function   #'cons)
    (error ()
      nil)))
