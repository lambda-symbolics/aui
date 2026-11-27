(in-package #:aui)

;;;; -- Blocks --

;;; Flow blocks are laid out, painted, and exposed as selectable paragraph leaves.

(defvar *paint-stage* nil
  "The stage that is being painted. Blocks declare their pointer regions on this stage.")

(defvar *paint-top* most-negative-fixnum
  "The top of the visible area. Parts that lie above it are skipped.")

(defvar *paint-bottom* most-positive-fixnum
  "The bottom of the visible area. Parts that lie below it are skipped.")

(defvar *paint-selection* nil
  "A table of paragraphs associated with their selected (START . END) character ranges, or NIL.")

(defclass flow-block ()
  ((margin-top
    :initarg       :margin-top
    :initform      0
    :accessor      block-margin-top
    :type          integer
    :documentation "The space above the block.")
   (margin-bottom
    :initarg       :margin-bottom
    :initform      0
    :accessor      block-margin-bottom
    :type          integer
    :documentation "The space below the block.")
   (height
    :initform      0
    :accessor      block-height
    :type          integer
    :documentation "The height of the block at the width of the last layout, without the margins.")
   (width
    :initform      0
    :accessor      block-width
    :type          integer
    :documentation "The width of the last layout."))
  (:documentation "A part of a stacked document."))

(defgeneric block-layout (block width)
  (:documentation "Lay BLOCK out to WIDTH. Record the height of BLOCK and return it."))

(defgeneric block-paint (block x y)
  (:documentation "Draw BLOCK on the stage that is being painted. The top-left corner of BLOCK is at X, Y."))

(defgeneric block-leaves (block)
  (:documentation "Return the selectable text of BLOCK as a list of (PARAGRAPH X Y). The offsets are relative to the top-left corner of BLOCK.")
  (:method ((block flow-block))
    "Return NIL for a block without text."
    nil))

(defmethod block-layout :around ((block flow-block) width)
  "Record the width and the height of the layout."
  (setf (block-width block)  width
        (block-height block) (call-next-method)))

(-> block--selection (paragraph) (or null cons))
(defun block--selection (paragraph)
  "Return the selected range of PARAGRAPH in the current paint, or NIL."
  (and *paint-selection* (gethash paragraph *paint-selection*)))

(-> block--paint-paragraph (paragraph integer integer) null)
(defun block--paint-paragraph (paragraph x y)
  "Draw PARAGRAPH and the selection of PARAGRAPH at X, Y. Declare a region for each link in PARAGRAPH."
  (paragraph-paint paragraph x y
                   :selection (block--selection paragraph)
                   :top       *paint-top*
                   :bottom    *paint-bottom*)
  (when *paint-stage*
    (dolist (line (paragraph-lines paragraph))
      (dolist (fragment (text-line-fragments line))
        (let ((link (and (fragment-span fragment) (span-link (fragment-span fragment)))))
          (when link
            (stage-region *paint-stage* (list ':link link (fragment-start fragment))
                          :x       (+ x (fragment-x fragment))
                          :y       (+ y (text-line-y line))
                          :width   (fragment-width fragment)
                          :height  (text-line-height line)
                          :action  (if (functionp link)
                                       link
                                       (lambda () (open-link link)))
                          :tooltip (and (stringp link) link)))))))
  nil)

(defvar *link-handler* nil
  "The function called with the target of a link if the link is clicked, or NIL. If it is NIL, clicks on links are ignored.")

(-> open-link (string) null)
(defun open-link (link)
  "Open the target LINK by calling *LINK-HANDLER*."
  (when *link-handler*
    (funcall *link-handler* link))
  nil)


;;;; -- Text --

(defclass text-block (flow-block)
  ((paragraph
    :initarg       :paragraph
    :reader        text-block-paragraph
    :type          paragraph
    :documentation "The text.")
   (indent
    :initarg       :indent
    :initform      0
    :reader        text-block-indent
    :type          integer
    :documentation "The distance of the text from the left edge of the block.")
   (marker
    :initarg       :marker
    :initform      nil
    :reader        text-block-marker
    :documentation "The marker in the indent before the first line. It is a string, a mark keyword or NIL."))
  (:documentation "A paragraph that can be indented after a marker, as in a list item."))

(-> make-text-block (list &key (:style text-style) (:indent integer) (:marker t) (:align keyword)
                               (:margin-top integer) (:margin-bottom integer))
    text-block)
(defun make-text-block (spans &key (style (text-style ':body)) (indent 0) marker (align ':left)
                                   (margin-top 0) (margin-bottom 0))
  "Return a text block of SPANS in STYLE."
  (make-instance 'text-block
                 :paragraph     (make-paragraph spans :style style :align align)
                 :indent        indent
                 :marker        marker
                 :margin-top    margin-top
                 :margin-bottom margin-bottom))

(defmethod block-layout ((block text-block) width)
  "Lay out the paragraph inside the indent and return the height of the lines."
  (paragraph-layout (text-block-paragraph block) (max 1 (- width (text-block-indent block)))))

(defmethod block-paint ((block text-block) x y)
  "Draw the marker in the indent and the text beside it."
  (let* ((paragraph (text-block-paragraph block))
         (style     (paragraph-style paragraph))
         (indent    (text-block-indent block))
         (marker    (text-block-marker block)))
    (cond
      ((stringp marker)
       (paint-text marker (+ x (max 0 (- indent (text-width marker style) 8))) (+ y (text-baseline style))
                   :style style))
      (marker
       (let ((size 14))
         (paint-frame (+ x (- indent size 9)) (+ y (floor (- (text-style-line-height style) size) 2))
                      :width  size
                      :height size)
         (when (eq marker ':done)
           (paint-mark ':check (+ x (- indent size 9)) (+ y (floor (- (text-style-line-height style) size) 2))
                       :size size)))))
    (block--paint-paragraph paragraph (+ x indent) y)))

(defmethod block-leaves ((block text-block))
  "Return the paragraph, offset by the indent."
  (list (list (text-block-paragraph block) (text-block-indent block) 0)))


;;;; -- Code --

(defparameter *code-padding* 12
  "The space between the frame of a code block and the code.")

(defclass code-block (flow-block)
  ((paragraph
    :initarg       :paragraph
    :reader        code-block-paragraph
    :type          paragraph
    :documentation "The code. Lines are broken where the width ends.")
   (label
    :initarg       :label
    :initform      nil
    :reader        code-block-label
    :type          (or null string)
    :documentation "The label above the code, such as the language, or NIL.")
   (identifier
    :initarg       :identifier
    :initform      (gensym "CODE")
    :reader        code-block-identifier
    :documentation "The identifier used for the copy region of the block."))
  (:documentation "Fixed-pitch text in a hairline frame. A copy action is drawn under the frame."))

(-> make-code-block (string &key (:label (or null string)) (:spans (or null list)) (:margin-top integer)
                                 (:margin-bottom integer))
    code-block)
(defun make-code-block (text &key label spans (margin-top 0) (margin-bottom 0))
  "Return a code block of TEXT under LABEL. If styled SPANS are given, they are used instead of TEXT."
  (make-instance 'code-block
                 :paragraph     (make-paragraph (or spans (list (make-span text nil)))
                                                :style   (text-style ':code)
                                                :wrap    ':anywhere
                                                :hanging 18)
                 :label         label
                 :margin-top    margin-top
                 :margin-bottom margin-bottom))

(-> code-block--header-height (code-block) integer)
(defun code-block--header-height (block)
  "Return the height of the header row of BLOCK, or zero if BLOCK has no label."
  (if (code-block-label block)
      (+ 6 (text-style-line-height (text-style ':meta)))
      0))

(-> code-block--copy-height () integer)
(defun code-block--copy-height ()
  "Return the height of the row under a code frame that contains the copy action."
  (+ 2 (text-style-line-height (text-style ':meta))))

(defmethod block-layout ((block code-block) width)
  "Lay out the code inside the frame. The frame contains the label (if there is one) and the code. The copy action is under the frame."
  (+ (* 2 *code-padding*)
     (code-block--header-height block)
     (paragraph-layout (code-block-paragraph block) (max 1 (- width (* 2 *code-padding*))))
     (code-block--copy-height)))

(defmethod block-paint ((block code-block) x y)
  "Draw the frame, the label, the code and the copy action. The copy action is under the right corner of the frame."
  (let* ((width   (block-width block))
         (meta    (text-style ':meta))
         (header  (code-block--header-height block))
         (frame-h (- (block-height block) (code-block--copy-height)))
         (copy-w  (+ (text-width "copy" meta) 12))
         (copy-h  (code-block--copy-height))
         (copy-x  (- (+ x width) copy-w))
         (copy-y  (+ y frame-h))
         (copy-id (list ':copy (code-block-identifier block)))
         (hot-p   (and *paint-stage* (stage-hot-p *paint-stage* copy-id))))
    (paint-frame x y :width width :height frame-h)
    (when (code-block-label block)
      (paint-text-in (code-block-label block) (text-style ':meta-bold)
                     :x      (+ x *code-padding*)
                     :y      (+ y 7)
                     :width  (- width (* 2 *code-padding*))
                     :height (- header 6)))
    (if hot-p
        (with-inversion (copy-x copy-y copy-w copy-h)
          (paint-text-in "copy" meta :x copy-x :y copy-y :width copy-w :height copy-h :align ':center))
        (paint-text-in "copy" meta :x copy-x :y copy-y :width copy-w :height copy-h :align ':center))
    (when *paint-stage*
      (stage-region *paint-stage* copy-id
                    :x       copy-x
                    :y       copy-y
                    :width   copy-w
                    :height  copy-h
                    :action  (lambda ()
                               (lightfast:copy-text (paragraph-text (code-block-paragraph block))))
                    :tooltip "Copy this block"))
    (block--paint-paragraph (code-block-paragraph block)
                            (+ x *code-padding*) (+ y *code-padding* header))))

(defmethod block-leaves ((block code-block))
  "Return the code, offset by the padding of the frame and by the label."
  (list (list (code-block-paragraph block)
              *code-padding*
              (+ *code-padding* (code-block--header-height block)))))


;;;; -- Table --

(defclass table-block (flow-block)
  ((rows
    :initarg       :rows
    :reader        table-block-rows
    :type          list
    :documentation "The rows. Each row is a list of cell paragraphs. The first row is the header.")
   (columns
    :initform      nil
    :accessor      table-block--columns
    :type          list
    :documentation "The laid-out column widths.")
   (row-heights
    :initform      nil
    :accessor      table-block--row-heights
    :type          list
    :documentation "The laid-out row heights."))
  (:documentation "A grid of cells that are separated by hairlines."))

(defparameter *cell-padding-x* 10
  "The horizontal space between the edge of a table cell and the text.")

(defparameter *cell-padding-y* 5
  "The vertical space between the edge of a table cell and the text.")

(defmethod block-layout ((block table-block) width)
  "Give each column the natural width. If the table is too wide, all columns are shrunk in proportion."
  (let* ((rows    (table-block-rows block))
         (count   (reduce #'max rows :key #'length :initial-value 0))
         (natural (loop for column from 0 below count
                        collect (loop for row in rows
                                      for cell = (nth column row)
                                      maximize (if cell
                                                   (progn (paragraph-layout cell 4000)
                                                          (paragraph-extent cell))
                                                   0))))
         (padding (* 2 *cell-padding-x*))
         (total   (+ 1 (reduce #'+ natural) (* count (1+ padding))))
         (scale   (if (and (> total width) (plusp (reduce #'+ natural)))
                      (/ (max 1 (- width 1 (* count (1+ padding)))) (reduce #'+ natural))
                      1))
         (columns (mapcar (lambda (natural-width) (max 24 (floor (* natural-width scale)))) natural))
         (heights (loop for row in rows
                        collect (+ (* 2 *cell-padding-y*)
                                   (loop for cell in row
                                         for column-width in columns
                                         maximize (paragraph-layout cell column-width))))))
    (setf (table-block--columns block)     columns
          (table-block--row-heights block) heights)
    (+ 1 (reduce #'+ heights) (length heights))))

(defmethod block-paint ((block table-block) x y)
  "Draw the text of all cells and the grid."
  (let* ((columns (table-block--columns block))
         (padding (* 2 *cell-padding-x*))
         (width   (+ 1 (reduce #'+ columns) (* (length columns) (1+ padding))))
         (row-y   y))
    (paint-line x y :x2 (+ x width -1) :y2 y)
    (loop for row in (table-block-rows block)
          for height in (table-block--row-heights block)
          do (let ((cell-x x))
               (loop for cell in row
                     for column-width in columns
                     do (block--paint-paragraph cell (+ cell-x 1 *cell-padding-x*) (+ row-y 1 *cell-padding-y*))
                        (incf cell-x (+ column-width padding 1))))
             (incf row-y (1+ height))
             (paint-line x row-y :x2 (+ x width -1) :y2 row-y))
    (let ((line-x x))
      (paint-line line-x y :x2 line-x :y2 row-y)
      (dolist (column-width columns)
        (incf line-x (+ column-width padding 1))
        (paint-line line-x y :x2 line-x :y2 row-y)))))

(defmethod block-leaves ((block table-block))
  "Return all cells at their places in the grid."
  (let ((leaves  nil)
        (padding (* 2 *cell-padding-x*))
        (row-y   0))
    (loop for row in (table-block-rows block)
          for height in (table-block--row-heights block)
          do (let ((cell-x 0))
               (loop for cell in row
                     for column-width in (table-block--columns block)
                     do (push (list cell (+ cell-x 1 *cell-padding-x*) (+ row-y 1 *cell-padding-y*)) leaves)
                        (incf cell-x (+ column-width padding 1))))
             (incf row-y (1+ height)))
    (nreverse leaves)))


;;;; -- Rule --

(defclass rule-block (flow-block)
  ((label
    :initarg       :label
    :initform      nil
    :reader        rule-block-label
    :type          (or null string)
    :documentation "The label centered on the rule, or NIL for a rule without a label."))
  (:documentation "A band of dither across the column. A label can be centered on the band."))

(defmethod block-layout ((block rule-block) width)
  "Return the height of one line in the meta style."
  (declare (ignore width))
  (text-style-line-height (text-style ':meta)))

(defmethod block-paint ((block rule-block) x y)
  "Draw the dither band. Draw the label in bold on a strip of the ground."
  (let* ((style  (text-style ':meta-bold))
         (height (block-height block))
         (width  (block-width block))
         (band-y (+ y (floor (- height 5) 2)))
         (label  (rule-block-label block)))
    (paint-dither x band-y :width width :height 5 :step 3)
    (when label
      (let* ((label-width (+ (text-width label style) 24))
             (label-x     (+ x (floor (- width label-width) 2))))
        (paint-fill label-x y :width label-width :height height :color (ground-color))
        (paint-text-in label style :x label-x :y y :width label-width :height height :align ':center)))))


;;;; -- Row --

(defclass row-block (flow-block)
  ((identifier
    :initarg       :identifier
    :reader        row-block-identifier
    :documentation "The identifier used for the region of the row.")
   (mark
    :initarg       :mark
    :initform      nil
    :accessor      row-block-mark
    :documentation "The mark before the title. It is a mark keyword, :SPINNER or NIL.")
   (mark-color
    :initarg       :mark-color
    :initform      nil
    :accessor      row-block-mark-color
    :type          (or null list)
    :documentation "The color of the mark. If it is NIL, the ink of the ground is used.")
   (title
    :initarg       :title
    :reader        row-block-title
    :type          list
    :documentation "The text spans of the row.")
   (paragraph
    :initarg       :paragraph
    :initform      nil
    :reader        row-block-paragraph
    :type          (or null paragraph)
    :documentation "The selectable, wrapping title of the row, or NIL for a single line.")
   (trailing
    :initarg       :trailing
    :initform      nil
    :reader        row-block-trailing
    :type          (or null string)
    :documentation "Small text at the right edge of the row, such as a duration.")
   (action
    :initarg       :action
    :initform      nil
    :reader        row-block-action
    :type          (or null function)
    :documentation "The function called if the row is clicked.")
   (style
    :initarg       :style
    :reader        row-block-style
    :type          text-style
    :documentation "The style of the title for spans without a style."))
  (:documentation "A row that summarizes something and can respond to clicks."))

(defparameter *row-mark-size* 14
  "The side length of the mark at the start of a row.")

(-> row-block--title-offset (row-block) integer)
(defun row-block--title-offset (block)
  "Return the space to the left of the title of BLOCK."
  (+ 6 (if (row-block-mark block) (+ *row-mark-size* 8) 0)))

(-> row-block--trailing-width (row-block) integer)
(defun row-block--trailing-width (block)
  "Return the space reserved for the right-aligned label of BLOCK."
  (let ((label (row-block-trailing block)))
    (if label (+ (text-width label (text-style ':meta)) 10) 0)))

(defmethod block-layout ((block row-block) width)
  "Return the height of the title, with wrapping when the row has a paragraph."
  (let ((paragraph (row-block-paragraph block)))
    (+ 4 (if paragraph
             (paragraph-layout paragraph (max 1 (- width (row-block--title-offset block) (row-block--trailing-width block) 6)))
             (text-style-line-height (row-block-style block))))))

(defmethod block-leaves ((block row-block))
  "Return the selectable title of a wrapping row."
  (let ((paragraph (row-block-paragraph block)))
    (when paragraph
      (list (list paragraph (row-block--title-offset block) 2)))))

(defmethod block-paint ((block row-block) x y)
  "Draw the mark, the title and the trailing text. Invert the row when the pointer is over its action."
  (let* ((stage     *paint-stage*)
         (style     (row-block-style block))
         (meta      (text-style ':meta))
         (width     (block-width block))
         (height    (block-height block))
         (action    (row-block-action block))
         (hot-p     (and stage action (stage-hot-p stage (row-block-identifier block))))
         (trailing  (row-block-trailing block))
         (trail-w   (row-block--trailing-width block))
         (text-x    (+ x (row-block--title-offset block)))
         (line-h    (+ 4 (text-style-line-height style)))
         (paragraph (row-block-paragraph block)))
    (flet ((content ()
             (let ((mark   (row-block-mark block))
                   (mark-y (+ y (floor (- line-h *row-mark-size*) 2))))
               (cond
                 ((eq mark ':spinner)
                  (setf (stage-animating-p stage) t)
                  (paint-spinner (+ x 6) mark-y :size *row-mark-size* :phase (stage-phase stage)))
                 (mark
                  (paint-mark mark (+ x 6) mark-y
                              :size  *row-mark-size*
                              :color (and (not hot-p) (row-block-mark-color block))))))
             (if paragraph
               (block--paint-paragraph paragraph text-x (+ y 2))
               (let ((cursor   text-x)
                     (limit    (- (+ x width) trail-w 6))
                     (baseline (+ y 2 (text-baseline style))))
                 (dolist (span (row-block-title block))
                   (let* ((span-style (or (span-style span) style))
                          (fitted     (text-fit (span-text span) span-style (max 0 (- limit cursor)))))
                     (when (and (plusp (- limit cursor)) (plusp (length fitted)))
                       (setf cursor (paint-text fitted cursor baseline :style span-style)))))))
             (when trailing
               (paint-text-in trailing meta
                              :x      (- (+ x width) trail-w 6)
                              :y      y
                              :width  trail-w
                              :height line-h
                              :align  ':right))))
      (if hot-p
          (with-inversion (x y width height)
            (content))
          (content)))
    (when (and stage action)
      (stage-region stage (row-block-identifier block) :x x :y y :width width :height height :action action))))


;;;; -- Box --

(defclass box-block (flow-block)
  ((children
    :initarg       :children
    :accessor      box-block-children
    :type          list
    :documentation "The blocks inside the box, from top to bottom.")
   (padding-x
    :initarg       :padding-x
    :initform      14
    :reader        box-block-padding-x
    :type          integer
    :documentation "The space between the sides of the box and the children.")
   (padding-y
    :initarg       :padding-y
    :initform      10
    :reader        box-block-padding-y
    :type          integer
    :documentation "The space between the top and bottom of the box and the children.")
   (frame-p
    :initarg       :frame-p
    :initform      t
    :reader        box-block-frame-p
    :type          boolean
    :documentation "True if a hairline is drawn around the box.")
   (fill
    :initarg       :fill
    :initform      ':none
    :reader        box-block-fill
    :type          (member :none :ink :quiet)
    :documentation "The fill of the box. :NONE is no fill, :INK is ink with inverted content, and :QUIET is light dither.")
   (shadow-p
    :initarg       :shadow-p
    :initform      nil
    :reader        box-block-shadow-p
    :type          boolean
    :documentation "True if a dither shadow is drawn below and to the right of the box.")
   (align
    :initarg       :align
    :initform      ':stretch
    :reader        box-block-align
    :type          (member :stretch :left :right)
    :documentation "With :STRETCH, the box fills the whole width. With :LEFT or :RIGHT, the box is shrunk to the content at that side.")
   (fraction
    :initarg       :fraction
    :initform      1
    :reader        box-block-fraction
    :type          real
    :documentation "The largest fraction of the available width for a box that is shrunk to the content.")
   (offset
    :initform      0
    :accessor      box-block--offset
    :type          integer
    :documentation "The start of the laid-out box within the available width.")
   (inner-width
    :initform      0
    :accessor      box-block--inner-width
    :type          integer
    :documentation "The width used for the layout of the children."))
  (:documentation "A frame, a fill or padding around other blocks."))

(defparameter *shadow-offset* 8
  "The offset of the dither shadow of a box, below and to the right of the box.")

(-> box-block--stack-height (list) integer)
(defun box-block--stack-height (children)
  "Return the height of CHILDREN stacked with their margins. Of two adjoining margins, only the larger one is used."
  (let ((height   0)
        (previous 0))
    (loop for child in children
          for first-p = t then nil
          do (incf height (if first-p 0 (max previous (block-margin-top child))))
             (incf height (block-height child))
             (setf previous (block-margin-bottom child)))
    height))

(defmethod block-layout ((block box-block) width)
  "Lay the children out inside the padding. A box with :LEFT or :RIGHT alignment is shrunk to the widest line of text."
  (let* ((children  (box-block-children block))
         (padding   (* 2 (box-block-padding-x block)))
         (shadow    (if (box-block-shadow-p block) *shadow-offset* 0))
         (available (max 1 (- (floor (* width (box-block-fraction block))) padding shadow)))
         (inner     available))
    (dolist (child children)
      (block-layout child available))
    (unless (eq (box-block-align block) ':stretch)
      (let ((extent (loop for child in children
                          maximize (if (typep child 'text-block)
                                       (+ (text-block-indent child)
                                          (paragraph-extent (text-block-paragraph child)))
                                       available))))
        (when (< extent available)
          (setf inner (max 24 extent))
          (dolist (child children)
            (block-layout child inner)))))
    (setf (box-block--inner-width block) inner
          (box-block--offset block)      (if (eq (box-block-align block) ':right)
                                             (- width inner padding shadow)
                                             0))
    (+ (* 2 (box-block-padding-y block)) shadow (box-block--stack-height children))))

(-> box-block--child-positions (box-block) list)
(defun box-block--child-positions (block)
  "Return a list of (CHILD X Y) for the children of BLOCK. The positions are relative to the top-left corner of BLOCK."
  (let ((positions nil)
        (child-x   (+ (box-block--offset block) (box-block-padding-x block)))
        (child-y   (box-block-padding-y block))
        (previous  0))
    (loop for child in (box-block-children block)
          for first-p = t then nil
          do (incf child-y (if first-p 0 (max previous (block-margin-top child))))
             (push (list child child-x child-y) positions)
             (incf child-y (block-height child))
             (setf previous (block-margin-bottom child)))
    (nreverse positions)))

(defmethod block-paint ((block box-block) x y)
  "Draw the shadow, the fill and the frame. Then draw the children on the correct ground."
  (let* ((shadow (if (box-block-shadow-p block) *shadow-offset* 0))
         (box-x  (+ x (box-block--offset block)))
         (box-w  (+ (box-block--inner-width block) (* 2 (box-block-padding-x block))))
         (box-h  (- (block-height block) shadow)))
    (when (plusp shadow)
      (paint-dither (+ box-x shadow) (+ y box-h) :width box-w :height shadow)
      (paint-dither (+ box-x box-w) (+ y shadow) :width shadow :height (- box-h shadow)))
    (flet ((children ()
             (loop for (child child-x child-y) in (box-block--child-positions block)
                   do (when (and (< (+ y child-y) *paint-bottom*)
                                 (> (+ y child-y (block-height child)) *paint-top*))
                        (block-paint child (+ x child-x) (+ y child-y))))))
      (ecase (box-block-fill block)
        (:ink
         (with-inversion (box-x y box-w box-h)
           (children)))
        (:quiet
         (paint-dither box-x y :width box-w :height box-h :step 5)
         (children))
        (:none
         (children))))
    (when (and (box-block-frame-p block) (not (eq (box-block-fill block) ':ink)))
      (paint-frame box-x y :width box-w :height box-h))))

(defmethod block-leaves ((block box-block))
  "Return the leaves of all children, offset by the positions of the children in the box."
  (loop for (child child-x child-y) in (box-block--child-positions block)
        append (loop for (paragraph leaf-x leaf-y) in (block-leaves child)
                     collect (list paragraph (+ child-x leaf-x) (+ child-y leaf-y)))))


;;;; -- Image --

(defparameter *image-limit* 24
  "The number of loaded images that are kept. The least recently used image is released first.")

(defvar *images* nil
  "A list of the loaded images as (NAMESTRING . HANDLE) pairs. The most recently used image is first. The handle is NIL for a file that cannot be read.")

(-> image-handle (string) t)
(defun image-handle (pathname)
  "Return the loaded image of the file PATHNAME, or NIL if the file cannot be read.

Each image is loaded once. If more than *IMAGE-LIMIT* images are loaded, the
least recently used images are released."
  (let ((known (assoc pathname *images* :test #'string=)))
    (if known
        (setf *images* (cons known (delete known *images* :test #'eq)))
        (push (cons pathname (lightfast:load-image pathname)) *images*))
    (let ((spare (nthcdr *image-limit* *images*)))
      (when spare
        (setf *images* (ldiff *images* spare))
        (images--release spare)))
    (rest (first *images*))))

(-> images--release (list) null)
(defun images--release (images)
  "Release the images in IMAGES."
  (loop for (nil . handle) in images
        when handle
          do (lightfast:release-image handle))
  nil)

(-> images-forget () null)
(defun images-forget ()
  "Release all loaded images."
  (images--release (shiftf *images* nil)))

(defclass image-block (flow-block)
  ((pathname
    :initarg       :pathname
    :reader        image-block-pathname
    :type          string
    :documentation "The image file.")
   (extent
    :initform      nil
    :accessor      image-block--extent
    :type          (or null cons)
    :documentation "The (WIDTH . HEIGHT) of the drawn image, or NIL if the file cannot be read."))
  (:documentation "An image, scaled down to fit the column."))

(defparameter *image-maximum-height* 320
  "The maximum height of an inline image.")

(defmethod block-layout ((block image-block) width)
  "Scale the image to fit the width and the maximum height. The aspect ratio is kept."
  (let ((image (image-handle (image-block-pathname block))))
    (if (null image)
        (progn
          (setf (image-block--extent block) nil)
          (text-style-line-height (text-style ':meta)))
        (multiple-value-bind (image-width image-height) (lightfast:image-size image)
          (let* ((scale        (min 1 (/ width (max 1 image-width)) (/ *image-maximum-height* (max 1 image-height))))
                 (drawn-width  (max 1 (floor (* image-width scale))))
                 (drawn-height (max 1 (floor (* image-height scale)))))
            (setf (image-block--extent block) (cons drawn-width drawn-height))
            (+ drawn-height 2))))))

(defmethod block-paint ((block image-block) x y)
  "Draw the image in a hairline frame. If the image cannot be shown, draw the file name."
  (let* ((extent (image-block--extent block))
         (image  (and extent (image-handle (image-block-pathname block)))))
    (if image
        (progn
          (lightfast:draw-image image (1+ x) (1+ y) :width (first extent) :height (rest extent))
          (paint-frame x y :width (+ (first extent) 2) :height (+ (rest extent) 2)))
        (paint-text (format nil "[~A]" (file-namestring (image-block-pathname block))) x (+ y (text-baseline (text-style ':meta)))
                    :style (text-style ':meta)))))
