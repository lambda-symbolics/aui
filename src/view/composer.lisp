(in-package #:aui)

;;;; -- Composer --

(defparameter *composer-line-height* 29
  "The height of a line of text in the composer.")

(defparameter *composer-maximum-lines* 9
  "The largest number of lines shown in the composer before it scrolls.")

(defparameter *composer-toolbar-height* 34
  "The height of the row of chips and the send button.")

(defparameter *permission-modes*
  '((:ask         "Ask first"   "Ask me before running a command that is not recognized")
    (:auto        "Auto"        "Choose the sandbox, full access or refusal for each command")
    (:sandboxed   "Sandbox"     "Run commands inside the workspace sandbox")
    (:full-access "Full access" "Run commands with your full privileges"))
  "A list of the permission modes as (MODE LABEL EXPLANATION), in the order of the menu.")

(-> composer-text (workbench) string)
(defun composer-text (workbench)
  "Return the text in the composer."
  (let ((editor (workbench-editor workbench)))
    (if editor (lightfast:value editor) "")))

(-> (setf composer-text) (string workbench) string)
(defun (setf composer-text) (text workbench)
  "Replace the text in the composer with TEXT."
  (let ((editor (workbench-editor workbench)))
    (when editor
      (setf (lightfast:value editor) text)
      (workbench-redraw workbench)))
  text)

(-> composer-caret-to-end (workbench) null)
(defun composer-caret-to-end (workbench)
  "Put the caret of the composer after the last character."
  (let ((editor (workbench-editor workbench)))
    (when editor
      (setf (lightfast:text-insert-position editor) (lightfast:text-length editor))))
  nil)

(-> composer-recall (workbench integer) boolean)
(defun composer-recall (workbench step)
  "Show the sent text that is STEP places earlier in the history, or later if STEP is negative. Return T if there was a text to show.

A step past the newest text empties the composer."
  (let* ((history (workbench-history workbench))
         (index   (workbench-history-index workbench))
         (target  (if index (+ index step) (if (plusp step) 0 -1))))
    (cond
      ((null history)
       nil)
      ((minusp target)
       (when index
         (setf (workbench-history-index workbench) nil
               (composer-text workbench)           ""))
       (and index t))
      ((>= target (length history))
       nil)
      (t
       (setf (workbench-history-index workbench) target
             (composer-text workbench)           (nth target history))
       (composer-caret-to-end workbench)
       t))))

(-> composer-text-height (workbench integer) integer)
(defun composer-text-height (workbench width)
  "Return the height of the text in the composer at WIDTH. The height is between one line and the largest number of lines."
  (let* ((text      (composer-text workbench))
         (paragraph (make-paragraph (list (make-span text nil)) :style (text-style ':body)))
         (height    (paragraph-layout paragraph (max 40 (- width 8)))))
    (max *composer-line-height*
         (min (* *composer-maximum-lines* *composer-line-height*) height))))

(-> composer-height (workbench integer) integer)
(defun composer-height (workbench width)
  "Return the height of the composer frame at WIDTH, including the attachments, the text and the toolbar."
  (+ 10
     (if (workbench-attachments workbench) 30 0)
     (composer-text-height workbench (- width 28))
     6
     *composer-toolbar-height*
     4))

(-> composer-send (workbench &key (:steer-p boolean)) null)
(defun composer-send (workbench &key (steer-p (workbench-working-p workbench)))
  "Send the text and the attachments in the composer. Empty the composer if the message was sent."
  (when (workbench-submit workbench (composer-text workbench)
                          :steer-p steer-p
                          :images  (workbench-attachments workbench))
    (setf (composer-text workbench)         ""
          (workbench-attachments workbench) nil))
  nil)

(-> composer-attach (workbench) null)
(defun composer-attach (workbench)
  "Let the person choose image files to send with the next message."
  (let ((files (lightfast:choose-files :title  "Attach images"
                                       :filter (format nil "Images~C*.{png,jpg,jpeg,gif,webp}" #\Tab))))
    (when files
      (setf (workbench-attachments workbench)
            (remove-duplicates (append (workbench-attachments workbench) files) :test #'string=))
      (workbench-redraw workbench)))
  nil)

(-> composer--offered-models (workbench) list)
(defun composer--offered-models (workbench)
  "Return the models of the providers that the person has signed in to. Return all models if the person has not signed in to any provider."
  (let* ((models (workbench-models workbench))
         (usable (remove-if-not (lambda (model)
                                  (portable-true-p (portable-get model "credentialStored")))
                                models)))
    (or usable models)))

(-> composer--model-choices (workbench) list)
(defun composer--model-choices (workbench)
  "Return the choices of models, with the provider of each model if it is known.

The choices come from the list of models of the channel. If that list is
missing, the choices come from the options of the model setting. The list is
missing, for example, if a provider cannot be reached."
  (let ((current (workbench-model workbench)))
    (or (mapcar (lambda (model)
                  (make-choice :label     (or (portable-get model "id") "")
                               :detail    (portable-string (portable-get model "provider"))
                               :value     (portable-get model "id")
                               :current-p (equal (portable-get model "id") current)))
                (composer--offered-models workbench))
        (mapcar (lambda (name)
                  (make-choice :label name :value name :current-p (equal name current)))
                (composer--setting-options workbench "model")))))

(-> composer--choose-model (workbench) null)
(defun composer--choose-model (workbench)
  "Show the models in a picker, and select the model chosen."
  (let ((choices (composer--model-choices workbench)))
    (if (null choices)
        (progn
          (setf (workbench-notice workbench) "the list of models is not loaded yet")
          (workbench-redraw workbench))
        (workbench-pick workbench choices
                        (lambda (model)
                          (workbench-set-setting workbench "model" model))
                        :title "Model")))
  nil)

(-> composer--setting-options (workbench string) list)
(defun composer--setting-options (workbench name)
  "Return the options of the setting NAME of the channel, as strings."
  (let ((row (find name (workbench-settings workbench)
                   :key (lambda (row) (portable-get row "name")) :test #'equal)))
    (loop for option in (portable-items (portable-get row "options"))
          collect (if (portable-object-p option)
                      (princ-to-string (or (portable-get option "value") (portable-get option "name")))
                      (princ-to-string option)))))

(-> composer--offer (workbench list function &key (:title string) (:x (or null integer)) (:y integer)) null)
(defun composer--offer (workbench choices action &key (title "Choose") x (y 0))
  "Show CHOICES and call ACTION with the value of the choice taken. If X is given, show a menu that grows upward from X, Y. Otherwise, show a picker titled TITLE."
  (if x
      (workbench-menu workbench choices :x x :y y :above-p t :action action)
      (workbench-pick workbench choices action :title title))
  nil)

(-> composer--choose-effort (workbench &key (:x (or null integer)) (:y integer)) null)
(defun composer--choose-effort (workbench &key x (y 0))
  "Show the reasoning efforts supported by the model, and select the effort chosen."
  (let ((current (workbench-effort workbench)))
    (composer--offer workbench
                     (mapcar (lambda (option)
                               (make-choice :label option :value option :current-p (equal option current)))
                             (or (composer--setting-options workbench "reasoning-effort")
                                 '("low" "medium" "high")))
                     (lambda (effort)
                       (workbench-set-setting workbench "reasoning-effort" effort))
                     :title "Reasoning effort"
                     :x     x
                     :y     y))
  nil)

(-> composer--choose-mode (workbench &key (:x (or null integer)) (:y integer)) null)
(defun composer--choose-mode (workbench &key x (y 0))
  "Show the permission modes with an explanation of each, and select the mode chosen."
  (let ((current (workbench-permission-mode workbench)))
    (composer--offer workbench
                     (mapcar (lambda (mode)
                               (make-choice :label     (second mode)
                                            :detail    (third mode)
                                            :value     (first mode)
                                            :current-p (eq (first mode) current)))
                             *permission-modes*)
                     (lambda (mode)
                       (workbench-set-mode workbench mode))
                     :title "Permissions"
                     :x     x
                     :y     y))
  nil)


;;;; -- Editing Keys --

;;; The editing keys of the line editor of Autolith, which also work in the
;;; composer:
;;;
;;;   Ctrl-A, Ctrl-E                         move to the start or the end of the line
;;;   Ctrl-U, Ctrl-K                         delete to the start or the end of the line
;;;   Ctrl-W, Ctrl-Backspace, Alt-Backspace  delete the word before the caret
;;;   Alt-D, Ctrl-Delete                     delete the word after the caret
;;;   Alt-B, Alt-F                           move by words

(-> composer--caret (workbench) integer)
(defun composer--caret (workbench)
  "Return the index of the character after the caret of the composer."
  (let* ((editor (workbench-editor workbench))
         (text   (composer-text workbench))
         (octets (lightfast:text-insert-position editor))
         (passed 0))
    (loop for index from 0 below (length text)
          do (when (>= passed octets)
               (return index))
             (incf passed (lightfast:character-byte-length (char text index)))
          finally (return (length text)))))

(-> composer--octets (string integer) integer)
(defun composer--octets (text end)
  "Return the number of octets of the first END characters of TEXT in the editor."
  (lightfast:string-byte-length (subseq text 0 end)))

(-> composer--delete (workbench integer integer) null)
(defun composer--delete (workbench start end)
  "Delete the characters of the composer from START up to END, and put the caret at START."
  (let ((editor (workbench-editor workbench))
        (text   (composer-text workbench)))
    (when (< start end)
      (lightfast:text-replace editor (composer--octets text start) (composer--octets text end) "")
      (setf (lightfast:text-insert-position editor) (composer--octets text start))
      (workbench-redraw workbench)))
  nil)

(-> composer--move (workbench integer) null)
(defun composer--move (workbench index)
  "Put the caret of the composer before the character at INDEX."
  (setf (lightfast:text-insert-position (workbench-editor workbench))
        (composer--octets (composer-text workbench) index))
  nil)

(-> composer--selected-p (workbench) boolean)
(defun composer--selected-p (workbench)
  "Return T when the composer has a selection, which a typed character replaces."
  (let ((editor (workbench-editor workbench)))
    (and editor (plusp (length (lightfast:text-selection editor))))))

(-> composer--closing-brace (character) (or null character))
(defun composer--closing-brace (character)
  "Return the brace that closes CHARACTER, or NIL when CHARACTER opens none."
  (case character
    (#\( #\))
    (#\[ #\])
    (#\{ #\})))

(-> composer--escaped-p (string integer) boolean)
(defun composer--escaped-p (text index)
  "Return T when an odd run of backslashes precedes INDEX in TEXT."
  (oddp (loop for position downfrom (1- index) to 0
              while (char= (char text position) #\\)
              count t)))

(-> composer--brace-step (string integer character &key (:selected-p boolean)) (or null keyword))
(defun composer--brace-step (text caret character &key selected-p)
  "Return the step for typing CHARACTER, a brace, with the caret at CARET in TEXT.

:PAIR inserts the pair and leaves the caret between the braces. :STEP moves the
caret over the brace that is already at CARET, so that a close brace typed after
its pair does not double it. A backslash before the caret, or a selection that
the editor replaces, takes no step."
  (unless selected-p
    (let ((closing (composer--closing-brace character)))
      (cond
        ((composer--escaped-p text caret)
         nil)
        (closing
         :pair)
        ((and (< caret (length text))
              (char= character (char text caret)))
         :step)))))

(-> composer--brace-pair-around-p (string integer) boolean)
(defun composer--brace-pair-around-p (text caret)
  "Return T when CARET in TEXT sits between an empty pair of braces."
  (and (plusp caret)
       (< caret (length text))
       (let ((closing (composer--closing-brace (char text (1- caret)))))
         (and closing
              (char= closing (char text caret))
              (not (composer--escaped-p text (1- caret)))))))

(-> composer--insert (workbench integer string) null)
(defun composer--insert (workbench index text)
  "Insert TEXT at INDEX in the composer and put the caret after it."
  (let ((editor (workbench-editor workbench))
        (source (composer-text workbench)))
      (when editor
        ;; Typing leaves the history recall, as it does in the line editor of the terminal.
        (setf (workbench-history-index workbench) nil)
        (lightfast:text-replace editor
                                (composer--octets source index)
                                (composer--octets source index)
                                text)
        (setf (lightfast:text-insert-position editor)
              (composer--octets (composer-text workbench) (+ index (length text))))
        (workbench-redraw workbench)))
  nil)

(-> composer--pair-braces (workbench integer character) null)
(defun composer--pair-braces (workbench caret character)
  "Insert CHARACTER and the brace that closes it at CARET and leave the caret between them."
  (let ((closing (composer--closing-brace character)))
    (when (and closing (workbench-editor workbench))
      (composer--insert workbench caret (coerce (list character closing) 'string))
      (composer--move workbench (1+ caret))))
  nil)

(-> composer--word-start (string integer) integer)
(defun composer--word-start (text index)
  "Return the start of the word before INDEX in TEXT. Whitespace before INDEX is skipped."
  (let ((start index))
    (loop while (and (plusp start) (member (char text (1- start)) '(#\Space #\Tab #\Newline)))
          do (decf start))
    (loop while (and (plusp start) (not (member (char text (1- start)) '(#\Space #\Tab #\Newline))))
          do (decf start))
    start))

(-> composer--word-end (string integer) integer)
(defun composer--word-end (text index)
  "Return the end of the word after INDEX in TEXT. Whitespace after INDEX is skipped."
  (let ((end index))
    (loop while (and (< end (length text)) (member (char text end) '(#\Space #\Tab #\Newline)))
          do (incf end))
    (loop while (and (< end (length text)) (not (member (char text end) '(#\Space #\Tab #\Newline))))
          do (incf end))
    end))

(-> composer-edit-key (workbench key-event) boolean)
(defun composer-edit-key (workbench event)
  "Handle the key EVENT in the composer as the line editor of Autolith does. Return T if the key was used.

A typed brace pairs with its close brace or steps over a brace that is already
under the caret, and backspace removes an empty pair, as the line editor of the
terminal does. A selection is replaced by the editor, and a key that types
several characters is inserted as typed, so neither pairs. Ctrl-K is only used
if there is text after the caret on the line. At the end of a line, Ctrl-K is
left unused, so that it can open the list of commands."
    (let* ((key         (key-event-key event))
           (character   (or (key-event-character event) key))
           (control-p   (key-event-control-p event))
           (alt-p       (key-event-alt-p event))
           (command-p   (key-event-command-p event))
           (brace-p     (and (characterp character)
                             (member character '(#\( #\) #\[ #\] #\{ #\}) :test #'char=)
                             (not control-p)
                             (not alt-p)
                             (not command-p)))
         (backspace-p (and (eq key ':backspace)
                           (not control-p)
                           (not alt-p)
                           (not command-p)))
         (text        (composer-text workbench))
         (caret       (and (or control-p alt-p brace-p backspace-p)
                           (workbench-editor workbench)
                           (composer--caret workbench)))
           (line        (and caret (1+ (or (position #\Newline text :end caret :from-end t) -1))))
           (line-end    (and caret (or (position #\Newline text :start caret) (length text))))
           (selected-p  (and caret (or brace-p backspace-p) (composer--selected-p workbench)))
           (step        (and brace-p
                             caret
                             (composer--brace-step text caret character :selected-p selected-p)))
         (pair-p      (and backspace-p
                           caret
                           (not selected-p)
                           (composer--brace-pair-around-p text caret))))
    (cond
      ((null caret)
       nil)
      ((eq step :pair)
         (composer--pair-braces workbench caret character)
       t)
      ((eq step :step)
       (composer--move workbench (1+ caret))
       t)
      (pair-p
       (composer--delete workbench (1- caret) (1+ caret))
       t)
      ((or (and control-p (member key '(:backspace #\w)))
           (and alt-p (eq key ':backspace)))
       (composer--delete workbench (composer--word-start text caret) caret)
       t)
      ((or (and alt-p (eql key #\d))
           (and control-p (eq key ':delete)))
       (composer--delete workbench caret (composer--word-end text caret))
       t)
      ((and control-p (eql key #\u))
       (composer--delete workbench line caret)
       t)
      ((and control-p (eql key #\k) (< caret line-end))
       (composer--delete workbench caret line-end)
       t)
      ((and control-p (eql key #\a))
       (composer--move workbench line)
       t)
      ((and control-p (eql key #\e))
       (composer--move workbench line-end)
       t)
      ((and alt-p (or (eql key #\b) (eq key ':left)))
       (composer--move workbench (composer--word-start text caret))
       t)
      ((and alt-p (or (eql key #\f) (eq key ':right)))
       (composer--move workbench (composer--word-end text caret))
       t)
      (t
       nil))))

(-> composer-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun composer-paint (workbench stage &key x y width height)
  "Draw the frame, the attachments, the chips and the send button of the composer. The editor is a native widget inside the frame."
  (let* ((middle    (- (+ y height) 8 16))
         (chip-y    (- middle 12))
         (cursor    (+ x 8))
         (working-p (workbench-working-p workbench))
         (ready-p   (workbench-attached-p workbench)))
    (paint-fill x y :width width :height height :color *paper*)
    (paint-frame x y :width width :height height)
    (paint-frame (1+ x) (1+ y) :width (- width 2) :height (- height 2))
    ;; Draw the attachments. Each attachment can be removed.
    (let ((chip-x (+ x 10)))
      (loop for pathname in (workbench-attachments workbench)
            do (let ((attached pathname))
                 (incf chip-x
                       (+ 6 (paint-chip stage (list ':attachment attached) (format nil "~A  ×" (file-namestring attached))
                                        :x       chip-x
                                        :y       (+ y 8)
                                        :height  22
                                        :action  (lambda ()
                                                   (setf (workbench-attachments workbench)
                                                         (remove attached (workbench-attachments workbench)
                                                                 :test #'string=))
                                                   (workbench-redraw workbench))
                                        :tooltip "Remove this image"))))))
    ;; Draw the toolbar, with the attach button and the settings of the next
    ;; turn. All parts of the toolbar are centered on one horizontal line.
    (paint-button stage ':attach ""
                  :x         cursor
                  :y         (- middle 14)
                  :width     28
                  :height    28
                  :mark      ':plus
                  :mark-size 12
                  :action    (lambda () (composer-attach workbench))
                  :tooltip   "Attach images")
    (incf cursor 36)
    (when ready-p
      (incf cursor (+ 2 (paint-chip stage ':model (or (workbench-model workbench) "model")
                                    :x       cursor
                                    :y       chip-y
                                    :height  24
                                    :menu-p  t
                                    :action  (lambda () (composer--choose-model workbench))
                                    :tooltip "Model")))
      (when (workbench-effort workbench)
        (let ((chip-x cursor))
          (incf cursor (+ 2 (paint-chip stage ':effort (workbench-effort workbench)
                                        :x       cursor
                                        :y       chip-y
                                        :height  24
                                        :menu-p  t
                                        :action  (lambda ()
                                                   (composer--choose-effort workbench :x chip-x :y (- chip-y 4)))
                                        :tooltip "Reasoning effort")))))
      (let* ((mode   (find (workbench-permission-mode workbench) *permission-modes* :key #'first))
             (label  (if mode (second mode) "Ask first"))
             (chip-x cursor))
        ;; Only show the mode if there is room for it before the send button.
        (when (<= (+ cursor (chip-width label :menu-p t)) (- (+ x width) 48))
          (paint-chip stage ':mode label
                      :x       cursor
                      :y       chip-y
                      :height  24
                      :menu-p  t
                      :action  (lambda ()
                                 (composer--choose-mode workbench :x chip-x :y (- chip-y 4)))
                      :tooltip (and mode (third mode))))))
    ;; Draw the send button, or the stop button while the agent works. The
    ;; distance to the right edge is the same as the distance to the bottom.
    (let ((button-x (- (+ x width) 32 8)))
      (if (and working-p (zerop (length (string-trim '(#\Space #\Newline) (composer-text workbench)))))
          (paint-button stage ':stop ""
                        :x         button-x
                        :y         (- middle 16)
                        :width     32
                        :height    32
                        :mark      ':square
                        :primary-p t
                        :action    (lambda () (workbench-stop workbench))
                        :tooltip   "Stop the agent (Esc)")
          (paint-button stage ':send ""
                        :x         button-x
                        :y         (- middle 16)
                        :width     32
                        :height    32
                        :mark      ':arrow-up
                        :primary-p t
                        :enabled-p ready-p
                        :action    (lambda () (composer-send workbench))
                        :tooltip   (if working-p
                                       "Steer the running turn (Enter), or queue a follow-up (Tab)"
                                       "Send (Enter)")))))
  nil)

(-> composer-editor-box (workbench &key (:x integer) (:y integer) (:width integer) (:height integer)) list)
(defun composer-editor-box (workbench &key x y width height)
  "Return the box (X Y WIDTH HEIGHT) of the editor inside a composer frame with the given box."
  (let ((top (+ y 8 (if (workbench-attachments workbench) 30 0))))
    (list (+ x 12)
          top
          (- width 24)
          (max *composer-line-height*
               (- (+ y height) top *composer-toolbar-height* 8)))))
