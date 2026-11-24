(in-package #:aui)

;;;; -- Completion --

(defparameter *completion-rows* 7
  "The number of completions shown at once.")

(defparameter *completion-row-height* 26
  "The height of a row of completions.")

(defstruct completion
  "One completion. LABEL is the text shown, and DETAIL is shown beside it. TEXT replaces the word that is typed."
  (label  "" :type string)
  (detail "" :type string)
  (text   "" :type string))

(-> completion--word (string) (values string integer))
(defun completion--word (text)
  "Return the word at the end of TEXT and the start position of the word."
  (let ((start (1+ (or (position-if (lambda (character)
                                      (member character '(#\Space #\Newline #\Tab)))
                                    text :from-end t)
                       -1))))
    (values (subseq text start) start)))

(-> completion--command-prefix (string integer) (or null string))
(defun completion--command-prefix (text caret)
  "Return the name typed after the open paren before CARET in TEXT, or NIL.

Only the text before the caret counts, so the close paren that the composer
inserts after an open paren does not end the command."
  (multiple-value-bind (word start) (completion--word (subseq text 0 caret))
    (when (and (plusp (length word))
               (zerop start)
               (char= (char word 0) #\()
               (not (find #\) word)))
      (subseq word 1))))

(-> completion--caret (workbench) integer)
(defun completion--caret (workbench)
  "Return the caret of the composer as a character index, or the end of the draft when there is no editor."
  (or (and (workbench-editor workbench) (composer--caret workbench))
      (length (composer-text workbench))))

(-> completion--typed-text (string integer) string)
(defun completion--typed-text (text caret)
  "Return what the person has typed in TEXT with the caret at CARET.

The close brace under the caret belongs to the pair that the composer inserts
after an open brace, so it is part of the typed text."
  (let ((head (subseq text 0 caret)))
    (multiple-value-bind (word start) (completion--word head)
      (declare (ignore word))
      (let ((opener (and (< start (length head)) (char head start))))
        (if (and opener
                 (< caret (length text))
                 (eql (char text caret) (composer--closing-brace opener)))
            (subseq text 0 (1+ caret))
            head)))))

(-> completion--commands (workbench string) list)
(defun completion--commands (workbench prefix)
  "Return the completions for the commands whose names begin with PREFIX. Each completion is a Lisp form."
  (loop for row in (workbench-operations workbench)
        for name = (portable-get row "name")
        when (and (stringp name)
                  (equal (portable-get row "kind") "command")
                  (not (member name *terminal-only-commands* :test #'string=))
                  (>= (length name) (length prefix))
                  (string-equal prefix name :end2 (length prefix)))
          collect (make-completion
                   :label  (format nil "(~A" name)
                   :detail (entries--one-line (or (portable-get row "description") ""))
                   :text   (if (portable-string (portable-get row "argumentHint"))
                               (format nil "(~A " name)
                               (format nil "(~A)" name)))))

(-> completion--participants (workbench string) list)
(defun completion--participants (workbench prefix)
  "Return the mentions of the participants of the world whose names begin with PREFIX."
  (unless (workbench-demo-p workbench)
    (loop for participant in (workbench-participants workbench)
          for name = (getf participant :name)
          when (and (stringp name)
                    (>= (length name) (length prefix))
                    (string-equal prefix name :end2 (length prefix)))
            collect (make-completion
                     :label  (format nil "@~A" name)
                     :detail (cond
                               ((eq (getf participant :kind) ':human)
                                "you")
                               ((getf participant :residence)
                                (format nil "agent in ~A" (channel-display-name (getf participant :residence))))
                               (t
                                "parked agent"))
                     :text   (format nil "@~A " name)))))

(-> completion-candidates (workbench string integer) list)
(defun completion-candidates (workbench text caret)
  "Return the completions for the draft TEXT with the caret at CARET, or NIL if there are none.

Only the text before the caret is completed, so the pair of parens that the
composer inserts is complete as soon as it is empty."
  (let ((prefix (completion--command-prefix text caret)))
    (if prefix
        (completion--commands workbench prefix)
        (multiple-value-bind (word start) (completion--word (subseq text 0 caret))
          (declare (ignore start))
          (when (and (plusp (length word))
                     (char= (char word 0) #\@)
                     (not (find #\/ word)))
            (completion--participants workbench (subseq word 1)))))))

(defvar *completion-offer* nil
  "The last completions and their inputs, as a list (TEXT CARET OPERATIONS PARTICIPANTS . CANDIDATES).")

(-> completion--offer (workbench string integer) list)
(defun completion--offer (workbench text caret)
  "Return the completions for the draft TEXT with the caret at CARET. They are only computed again if the draft, the caret, the operations or the participants changed."
  (let ((operations   (workbench-operations workbench))
        (participants (workbench--participants workbench)))
    (destructuring-bind (&optional known-text known-caret known-operations known-participants &rest candidates)
        *completion-offer*
      (if (and *completion-offer*
               (string= text known-text)
               (eql caret known-caret)
               (eq operations known-operations)
               (eq participants known-participants))
          candidates
          (let ((candidates (completion-candidates workbench text caret)))
            (setf *completion-offer* (list* text caret operations participants candidates))
            candidates)))))

(-> completion-current (workbench) list)
(defun completion-current (workbench)
  "Return the completions for the current draft, or NIL if the list was closed for this draft."
  (let* ((text  (composer-text workbench))
         (caret (completion--caret workbench)))
    (if (equal (list text caret) (workbench-completion-dismissed workbench))
        nil
        (let ((candidates (completion--offer workbench text caret)))
          ;; Offer nothing if the only completion is the text that is already typed.
          (if (and candidates
                   (null (rest candidates))
                   (string= (string-right-trim " " (completion-text (first candidates)))
                            (string-right-trim " " (completion--typed-text text caret))))
              nil
              candidates)))))

(-> completion-accept (workbench completion) null)
(defun completion-accept (workbench completion)
  "Replace the word that is typed with the text of COMPLETION.

The text after the caret is kept, except for the close brace of an empty pair,
which the accepted text replaces."
  (let* ((text     (composer-text workbench))
         (caret    (completion--caret workbench))
         (typed    (completion--typed-text text caret))
         (inserted (completion-text completion)))
    (multiple-value-bind (word start) (completion--word (subseq text 0 caret))
      (declare (ignore word))
      (setf (composer-text workbench)
            (concatenate 'string (subseq text 0 start) inserted (subseq text (length typed))))
      (composer--move workbench (+ start (length inserted)))))
  (setf (workbench-completion-index workbench) 0)
  nil)

(-> completion-key (workbench key-event) boolean)
(defun completion-key (workbench event)
  "Handle the key EVENT for the list of completions. Return T if the key was used."
  (let* ((candidates (completion-current workbench))
         (count      (length candidates)))
    (when candidates
      (case (key-event-key event)
        (:down
         (setf (workbench-completion-index workbench)
               (mod (1+ (workbench-completion-index workbench)) count))
         (workbench-redraw workbench)
         t)
        (:up
         (setf (workbench-completion-index workbench)
               (mod (1- (workbench-completion-index workbench)) count))
         (workbench-redraw workbench)
         t)
        (:tab
         (completion-accept workbench
                            (nth (min (workbench-completion-index workbench) (1- count)) candidates))
         t)
        (:escape
         (setf (workbench-completion-dismissed workbench)
               (list (composer-text workbench) (completion--caret workbench)))
         (workbench-redraw workbench)
         t)
        (otherwise
         nil)))))

(-> completion-height (workbench) integer)
(defun completion-height (workbench)
  "Return the height of the list of completions, or zero if there are no completions."
  (let ((count (length (completion-current workbench))))
    (if (zerop count)
        0
        (+ 2 (* *completion-row-height* (min count *completion-rows*))))))

(-> completion-paint (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun completion-paint (workbench stage &key x y width)
  "Draw the completions in a framed list with the top left corner at X, Y."
  (let* ((candidates (completion-current workbench))
         (count      (length candidates))
         (index      (min (workbench-completion-index workbench) (max 0 (1- count))))
         (first      (max 0 (min (- index (1- *completion-rows*)) (- count *completion-rows*))))
         (first      (max 0 (if (< index first) index first)))
         (height     (completion-height workbench))
         (bold       (text-style ':label-bold))
         (meta       (text-style ':meta))
         ;; Draw the labels in one column and the details in another.
         (label-w    (min (floor (* width 2/5))
                          (loop for completion in candidates
                                maximize (text-width (completion-label completion) bold)))))
    (when candidates
      (paint-fill x y :width width :height height :color *paper*)
      (paint-frame x y :width width :height height)
      (loop for completion in (nthcdr first candidates)
            for row from first
            for row-y from (1+ y) by *completion-row-height*
            repeat *completion-rows*
            do (let* ((chosen completion)
                      (region (list ':completion row))
                      (lit-p  (or (= row index) (stage-hot-p stage region))))
                 (flet ((content ()
                          (paint-text-in (completion-label completion) bold
                                         :x      (+ x 10)
                                         :y      row-y
                                         :width  label-w
                                         :height *completion-row-height*)
                          (paint-text-in (completion-detail completion) meta
                                         :x      (+ x 10 label-w 18)
                                         :y      row-y
                                         :width  (max 0 (- width label-w 40))
                                         :height *completion-row-height*)))
                   (if lit-p
                       (with-inversion ((1+ x) row-y (- width 2) *completion-row-height*)
                         (content))
                       (content)))
                 (stage-region stage region
                               :x      x
                               :y      row-y
                               :width  width
                               :height *completion-row-height*
                               :action (lambda ()
                                         (completion-accept workbench chosen)
                                         (window-focus-composer workbench)))))))
  nil)
