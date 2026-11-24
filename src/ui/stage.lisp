(in-package #:aui)

;;;; -- Stage --

;;; The window's drawing surface. Pointer regions are rebuilt after each draw.

(defstruct region
  "A rectangle of the stage that responds to the pointer."
  (identifier nil)
  (x          0   :type integer)
  (y          0   :type integer)
  (width      0   :type integer)
  (height     0   :type integer)
  (action     nil :type (or null function))
  (menu       nil :type (or null function))
  (wheel      nil :type (or null function))
  (drag       nil :type (or null function))
  (cursor     ':default :type keyword)
  (tooltip    nil :type (or null string)))

(defclass stage ()
  ((window
    :initarg       :window
    :reader        stage-window
    :documentation "The window filled by the stage.")
   (canvas
    :initform      nil
    :accessor      stage-canvas
    :documentation "The canvas everything is drawn on.")
   (width
    :initform      0
    :accessor      stage-width
    :type          integer
    :documentation "The current width of the stage in FLTK units.")
   (height
    :initform      0
    :accessor      stage-height
    :type          integer
    :documentation "The current height of the stage in FLTK units.")
   (painter
    :initarg       :painter
    :accessor      stage-painter
    :type          function
    :documentation "The function that draws the stage and declares the regions. It is called with the stage.")
   (key-handler
    :initarg       :key-handler
    :initform      nil
    :accessor      stage-key-handler
    :type          (or null function)
    :documentation "The function called with a key event if the stage has the keyboard focus. It returns true if the key was handled.")
   (drop-handler
    :initarg       :drop-handler
    :initform      nil
    :accessor      stage-drop-handler
    :type          (or null function)
    :documentation "The function called with the text dropped on the stage. The text is the dragged text or file names, one on each line.")
   (regions
    :initform      nil
    :accessor      stage--regions
    :type          list
    :documentation "The regions declared during the last drawing. The topmost region is first.")
   (drawing
    :initform      nil
    :accessor      stage--drawing
    :type          list
    :documentation "The regions declared so far during the current drawing.")
   (hot
    :initform      nil
    :accessor      stage-hot
    :documentation "The identifier of the region under the pointer, or NIL.")
   (active
    :initform      nil
    :accessor      stage-active
    :documentation "The identifier of the region where a pointer button was pressed, until the button is released.")
   (pointer-x
    :initform      -1
    :accessor      stage-pointer-x
    :type          integer
    :documentation "The last known X of the pointer, or -1 if the pointer is outside the stage.")
   (pointer-y
    :initform      -1
    :accessor      stage-pointer-y
    :type          integer
    :documentation "The last known Y of the pointer, or -1 if the pointer is outside the stage.")
   (cursor
    :initform      ':default
    :accessor      stage--cursor
    :type          keyword
    :documentation "The cursor shape that was set last.")
   (dirty-p
    :initform      t
    :accessor      stage-dirty-p
    :type          boolean
    :documentation "True if a redraw was requested and is not yet passed to FLTK.")
   (natives
    :initform      nil
    :accessor      stage-natives
    :type          list
    :documentation "The native widgets that lie over the canvas. They are redrawn after the canvas, so that the canvas does not cover them.")
   (animating-p
    :initform      nil
    :accessor      stage-animating-p
    :type          boolean
    :documentation "True if the last drawing contained something that moves.")
   (phase
    :initform      0
    :accessor      stage-phase
    :type          integer
    :documentation "A counter that advances while something moves. The frame of animated marks is computed from this counter."))
  (:documentation "The drawing surface of a window. Pointer and key events on the stage are passed to their handlers."))


;;;; -- Guarding --

(defvar *stage-failure-function* nil
  "The function called with a report string if a drawing or input handler signals an error. If it is NIL, the error is only logged.")

(defmacro with-stage-guard ((what) &body body)
  "Run BODY and return the values of BODY. If BODY signals a serious condition, report that WHAT failed and return NIL.

The report is written to standard error and passed to *STAGE-FAILURE-FUNCTION*."
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       (serious-condition (,condition)
         (let ((report (format nil "~A failed: ~A" ,what ,condition)))
           (format *error-output* "~&aui: ~A~%" report)
           (when *stage-failure-function*
             (funcall *stage-failure-function* report)))
         nil))))


;;;; -- Clipping --

(defvar *clip* nil
  "The box (X Y WIDTH HEIGHT) used to clip the current drawing, or NIL if there is no clip.")

(-> clip--within (list (or null list)) list)
(defun clip--within (box clip)
  "Return the part of BOX that lies within CLIP. BOX is an (X Y WIDTH HEIGHT) list. CLIP is a box of the same form, or NIL for no clip."
  (if (null clip)
      box
      (destructuring-bind (x y width height) box
        (destructuring-bind (clip-x clip-y clip-width clip-height) clip
          (let ((left (max x clip-x))
                (top  (max y clip-y)))
            (list left
                  top
                  (max 0 (- (min (+ x width) (+ clip-x clip-width)) left))
                  (max 0 (- (min (+ y height) (+ clip-y clip-height)) top))))))))

(defmacro with-clip ((x y width height) &body body)
  "Run BODY and clip the drawing of BODY to the box X, Y, WIDTH, HEIGHT.

The regions declared by BODY are clipped to the box too, so that hidden
regions do not respond to the pointer. The four forms are evaluated once, in
order."
  `(let ((*clip* (clip--within (list ,x ,y ,width ,height) *clip*)))
     (lightfast:with-clip ((first *clip*) (second *clip*) (third *clip*) (fourth *clip*))
       ,@body)))


;;;; -- Regions --

(-> stage-region
    (stage t
           &key (:x integer) (:y integer) (:width integer) (:height integer)
                (:action (or null function)) (:menu (or null function)) (:wheel (or null function))
                (:drag (or null function)) (:cursor keyword) (:tooltip (or null string)))
    boolean)
(defun stage-region (stage identifier
                     &key (x 0) (y 0) (width 0) (height 0) action menu wheel drag (cursor ':default) tooltip)
    "Declare a pointer region in STAGE and return whether the pointer is inside it.

ACTION handles clicks, MENU secondary clicks, WHEEL scrolls, and DRAG pointer motion.
Later regions are on top; an actionable default-cursor region uses a hand cursor."
  (destructuring-bind (x y width height) (clip--within (list x y width height) *clip*)
    ;; Regions that lie outside the clip are left out.
    (when (or (null *clip*) (and (plusp width) (plusp height)))
      (push (make-region :identifier identifier :x x :y y :width width :height height
                         :action action :menu menu :wheel wheel :drag drag
                         :cursor  (if (and action (eq cursor ':default)) ':hand cursor)
                         :tooltip tooltip)
            (stage--drawing stage))))
  (and (equal identifier (stage-hot stage)) t))

(-> stage-hot-p (stage t) boolean)
(defun stage-hot-p (stage identifier)
  "Return T if the pointer is over the region IDENTIFIER."
  (and (stage-hot stage) (equal identifier (stage-hot stage)) t))

(-> stage-pressed-p (stage t) boolean)
(defun stage-pressed-p (stage identifier)
  "Return T if a pointer button is held down in the region IDENTIFIER."
  (and (stage-active stage) (equal identifier (stage-active stage)) t))

(-> stage--region-at (stage integer integer &key (:test (or null function))) (or null region))
(defun stage--region-at (stage x y &key test)
  "Return the topmost region containing X, Y that satisfies TEST, or NIL."
  (find-if (lambda (region)
             (and (<= (region-x region) x)
                  (< x (+ (region-x region) (region-width region)))
                  (<= (region-y region) y)
                  (< y (+ (region-y region) (region-height region)))
                  (or (null test) (funcall test region))))
           (stage--regions stage)))

(-> stage--find-region (stage t) (or null region))
(defun stage--find-region (stage identifier)
  "Return the region IDENTIFIER declared during the last drawing, or NIL."
  (find identifier (stage--regions stage) :key #'region-identifier :test #'equal))


;;;; -- Redrawing --

(-> stage-redraw (stage) null)
(defun stage-redraw (stage)
  "Request a redraw of the stage."
  (setf (stage-dirty-p stage) t)
  nil)

(-> stage-flush (stage) null)
(defun stage-flush (stage)
  "Pass a pending redraw to FLTK. The native widgets are redrawn after the canvas."
  (when (and (stage-dirty-p stage) (stage-canvas stage))
    (setf (stage-dirty-p stage) nil)
    (lightfast:redraw (stage-canvas stage))
    (dolist (native (stage-natives stage))
      (lightfast:redraw native)))
  nil)

(-> stage--paint (stage) null)
(defun stage--paint (stage)
  "Draw the stage. Fill it with paper, call the painter and keep the regions declared by the painter."
  (setf (stage--drawing stage)   nil
        (stage-animating-p stage) nil)
  (let ((*on-ink-p* nil))
    (paint-fill 0 0 :width (stage-width stage) :height (stage-height stage) :color *paper*)
    (funcall (stage-painter stage) stage))
  (setf (stage--regions stage) (stage--drawing stage)
        (stage--drawing stage) nil)
  ;; The pointer has not moved. However, the region under it can be different.
  (stage--track stage (stage-pointer-x stage) (stage-pointer-y stage) :redraw-p nil)
  nil)

(-> stage--track (stage integer integer &key (:redraw-p boolean)) null)
(defun stage--track (stage x y &key (redraw-p t))
  "Record the pointer at X, Y, and update the hot region and the cursor."
  (setf (stage-pointer-x stage) x
        (stage-pointer-y stage) y)
  (let* ((region (and (>= x 0) (>= y 0) (stage--region-at stage x y)))
         (hot    (and region (region-identifier region)))
         (cursor (if region (region-cursor region) ':default)))
    (unless (equal hot (stage-hot stage))
      (setf (stage-hot stage) hot)
      (lightfast:set-tooltip (stage-canvas stage) (and region (region-tooltip region)))
      (when redraw-p
        (stage-redraw stage)))
    (unless (eq cursor (stage--cursor stage))
      (setf (stage--cursor stage) cursor)
      (lightfast:set-cursor (stage-canvas stage) cursor)))
  nil)


;;;; -- Events --

(-> stage--numbers (string) list)
(defun stage--numbers (value)
  "Return the integers in the space-separated event VALUE."
  (let ((numbers nil)
        (start   0))
    (loop
      (let ((end (position #\Space value :start start)))
        (let ((number (parse-integer value :start start :end end :junk-allowed t)))
          (when number
            (push number numbers)))
        (if end
            (setf start (1+ end))
            (return))))
    (nreverse numbers)))

(-> stage--pointer (stage keyword string) null)
(defun stage--pointer (stage kind value)
  "Handle the pointer event KIND. VALUE contains the position, the button, the wheel steps and the modifier state."
  (destructuring-bind (&optional (x -1) (y -1) (button 0) (dx 0) (dy 0) &rest state) (stage--numbers value)
    (declare (ignore state))
    (ecase kind
      ((:move :enter)
       (stage--track stage x y))
      (:leave
       (stage--track stage -1 -1))
      (:push
       (stage--track stage x y)
       (let ((region (stage--region-at stage x y)))
         (setf (stage-active stage) (and region (region-identifier region)))
         (when (and region (region-drag region) (= button 1))
           (funcall (region-drag region) ':start x y))
         (stage-redraw stage)))
      (:drag
       (setf (stage-pointer-x stage) x
             (stage-pointer-y stage) y)
       (let ((region (and (stage-active stage) (stage--find-region stage (stage-active stage)))))
         (when (and region (region-drag region))
           (funcall (region-drag region) ':move x y)
           (stage-redraw stage))))
      (:release
       (let* ((active (stage-active stage))
              (region (and active (stage--find-region stage active)))
              (under  (stage--region-at stage x y)))
         (setf (stage-active stage) nil)
         (when region
           (when (region-drag region)
             (funcall (region-drag region) ':end x y))
           (when (and under (equal (region-identifier under) active))
             (cond
               ((and (= button 3) (region-menu region))
                (funcall (region-menu region)))
               ((and (= button 1) (region-action region))
                (funcall (region-action region))))))
         (stage--track stage x y)
         (stage-redraw stage)))
      (:wheel
       (let ((region (stage--region-at stage x y :test #'region-wheel)))
         (when region
           (funcall (region-wheel region) dy dx)
           (stage-redraw stage))))))
  nil)

(defstruct key-event
  "A key press, which consists of the FLTK key code, the modifier state bits and the typed text."
  (code  0  :type integer)
  (state 0  :type integer)
  (text  "" :type string))

(-> parse-key-event (string) key-event)
(defun parse-key-event (value)
  "Return the key event described by the callback VALUE. VALUE contains a key code, a modifier state and the typed text."
  (let* ((first-space  (position #\Space value))
         (second-space (and first-space (position #\Space value :start (1+ first-space)))))
    (make-key-event :code  (or (parse-integer value :end first-space :junk-allowed t) 0)
                    :state (or (and first-space
                                    (parse-integer value :start (1+ first-space) :end second-space
                                                         :junk-allowed t))
                               0)
                    :text  (if second-space (subseq value (1+ second-space)) ""))))

(defparameter *key-codes*
  '(:escape #xff1b :enter #xff0d :keypad-enter #xff8d :tab #xff09 :backspace #xff08 :delete #xffff
    :up #xff52 :down #xff54 :left #xff51 :right #xff53 :home #xff50 :end #xff57
    :page-up #xff55 :page-down #xff56)
  "A property list of named keys associated with their FLTK key codes.")

(-> key-event-key (key-event) (or keyword character null))
(defun key-event-key (event)
  "Return the key of EVENT. Named keys are returned as keywords. Letters, digits and signs are returned as lowercase characters. For other keys, NIL is returned."
  (let ((code (key-event-code event)))
    (or (loop for (name value) on *key-codes* by (partial #'nthcdr 2)
              when (= value code)
                do (return name))
        (and (< 31 code 127) (char-downcase (code-char code))))))

(-> key-event-character (key-event) (or null character))
(defun key-event-character (event)
    "Return the character that EVENT typed, or NIL when it typed no single character.

A shifted character arrives with the key of the unshifted key, so the typed
text is the character that was entered: `(` reports the key of `9`. Named keys
such as Return and Tab also carry one character of text and are returned here;
callers that handle them check the key first."
  (let ((text (key-event-text event)))
    (and (= (length text) 1) (char text 0))))

(-> key-event-shift-p (key-event) boolean)
(defun key-event-shift-p (event)
  "Return T if Shift was held."
  (logtest lightfast:+modifier-shift+ (key-event-state event)))

(-> key-event-control-p (key-event) boolean)
(defun key-event-control-p (event)
  "Return T if Control was held."
  (logtest lightfast:+modifier-control+ (key-event-state event)))

(-> key-event-alt-p (key-event) boolean)
(defun key-event-alt-p (event)
  "Return T if Alt (Option on a Mac) was held."
  (logtest lightfast:+modifier-alt+ (key-event-state event)))

(-> key-event-command-p (key-event) boolean)
(defun key-event-command-p (event)
  "Return T if the command modifier of the platform was held. It is Command on a Mac and Control on other platforms."
  (logtest lightfast:+modifier-command+ (key-event-state event)))

(-> make-stage
    (t &key (:painter function) (:key-handler (or null function)) (:drop-handler (or null function))) stage)
(defun make-stage (window &key painter key-handler drop-handler)
  "Fill WINDOW with a stage drawn by PAINTER and return the stage. Keys are passed to KEY-HANDLER, and drops are passed to DROP-HANDLER."
  (let* ((stage  (make-instance 'stage :window window :painter painter :key-handler key-handler
                                       :drop-handler drop-handler))
         (canvas (lightfast:make-canvas
                  :parent   window
                  :x        0
                  :y        0
                  :width    (lightfast:widget-width window)
                  :height   (lightfast:widget-height window)
                  :callback (lambda (widget event value)
                              (declare (ignore widget event value))
                              ;; A failed drawing does not request another drawing.
                              (unless (with-stage-guard ("Drawing")
                                        (stage--paint stage)
                                        t)
                                (setf (stage-dirty-p stage) nil))))))
    (setf (stage-canvas stage) canvas
          (stage-width stage)  (lightfast:widget-width window)
          (stage-height stage) (lightfast:widget-height window))
    (lightfast:set-box canvas lightfast:+box-no-box+)
    (lightfast:canvas-set-focusable canvas t)
    (flet ((pointer (kind)
             (lambda (widget event value)
               (declare (ignore widget event))
               (with-stage-guard ("A pointer action")
                 (stage--pointer stage kind value)))))
      (lightfast:on canvas (pointer ':push) :event lightfast:+event-push+)
      (lightfast:on canvas (pointer ':drag) :event lightfast:+event-drag+)
      (lightfast:on canvas (pointer ':release) :event lightfast:+event-release+)
      (lightfast:on canvas (pointer ':wheel) :event lightfast:+event-wheel+)
      (lightfast:on canvas (pointer ':move) :event lightfast:+event-move+)
      (lightfast:on canvas (pointer ':enter) :event lightfast:+event-enter+)
      (lightfast:on canvas (pointer ':leave) :event lightfast:+event-leave+))
    (lightfast:on canvas
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (let ((handler (stage-drop-handler stage)))
                      (when handler
                        (with-stage-guard ("A drop")
                          (funcall handler value)))))
                  :event lightfast:+event-drop+)
    (lightfast:on canvas
                  (lambda (widget event value)
                    (declare (ignore widget event))
                    (let ((handler (stage-key-handler stage)))
                      (when handler
                        (with-stage-guard ("A key")
                          (funcall handler (parse-key-event value))))))
                  :event lightfast:+event-key+)
    stage))

(-> stage-resize (stage integer integer) null)
(defun stage-resize (stage width height)
  "Set the size of the stage and of the canvas to WIDTH by HEIGHT."
  (setf (stage-width stage)  width
        (stage-height stage) height)
  (lightfast:resize-widget (stage-canvas stage) :x 0 :y 0 :width width :height height)
  (stage-redraw stage)
  nil)
