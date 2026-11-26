(in-package #:aui)

;;;; -- Document View --

(defparameter *document-measure* 760
  "The maximum width of the reading column in FLTK units. It is about seventy characters of the body face.")

(defparameter *document-margin* 28
  "The minimum space at each side of the reading column.")

(defparameter *document-wheel-step* 54
  "The distance of scrolling for one wheel step.")

(defparameter *document-kept-items* 200
  "The number of entries on each side of the view that keep their blocks.")

(defparameter *document-item-estimate* 90
  "The estimated height of an entry that was not laid out yet.")

(defstruct item
  "The layout of one entry. It contains the blocks, the top of the entry in the column and the selectable leaves. WIDTH is the width of the column used to lay out the blocks. WIDTH is zero for an item without blocks, which has the height it last had."
  (entry    nil)
  (revision -1  :type integer)
  (width    0   :type integer)
  (blocks   nil :type list)
  (top      0   :type integer)
  (height   0   :type integer)
  (leaves   nil :type list))

(defclass document ()
  ((transcript
    :initarg       :transcript
    :accessor      document-transcript
    :type          transcript
    :documentation "The transcript shown.")
   (items
    :initform      (make-array 64 :adjustable t :fill-pointer 0)
    :accessor      document--items
    :type          vector
    :documentation "The laid-out entries in the order of the transcript.")
   (laid-revision
    :initform      -1
    :accessor      document--laid-revision
    :type          integer
    :documentation "The revision of the transcript that was used for the last update of the items.")
   (column-width
    :initform      0
    :accessor      document--column-width
    :type          integer
    :documentation "The width used to lay out the entries.")
   (content-height
    :initform      0
    :accessor      document-content-height
    :type          integer
    :documentation "The sum of the heights of all entries.")
   (scroll
    :initform      0
    :accessor      document-scroll
    :type          integer
    :documentation "The distance of scrolling from the top of the column.")
   (following-p
    :initform      t
    :accessor      document-following-p
    :type          boolean
    :documentation "True if the view follows the newest entry as the transcript grows.")
   (viewport
    :initform      (list 0 0 0 0)
    :accessor      document--viewport
    :type          list
    :documentation "The box (X Y WIDTH HEIGHT) used for the last painting of the document.")
   (anchor
    :initform      nil
    :accessor      document--anchor
    :documentation "The start of the selection as (ITEM-INDEX LEAF-INDEX CHARACTER), or NIL.")
   (focus
    :initform      nil
    :accessor      document--focus
    :documentation "The current end of the selection in the form of the anchor, or NIL.")
   (more-p
    :initform      nil
    :accessor      document-more-p
    :type          boolean
    :documentation "True if there is earlier history before the first entry.")
   (load-earlier
    :initarg       :load-earlier
    :initform      nil
    :accessor      document-load-earlier
    :type          (or null function)
    :documentation "A function that fetches the page of history before the first entry, or NIL."))
  (:documentation "A scrolling, selectable view of one transcript."))

(-> make-document (transcript &key (:load-earlier (or null function))) document)
(defun make-document (transcript &key load-earlier)
  "Return a document view of TRANSCRIPT."
  (make-instance 'document :transcript transcript :load-earlier load-earlier))


;;;; -- Layout --

(-> document--stack (list integer) (values integer list))
(defun document--stack (blocks width)
  "Lay out BLOCKS to WIDTH, one under another, with collapsed margins. Returns the height and a list of (BLOCK . TOP) pairs."
  (let ((top      0)
        (previous 0)
        (placed   nil))
    (loop for block in blocks
          for first-p = t then nil
          do (block-layout block width)
             (incf top (if first-p (block-margin-top block) (max previous (block-margin-top block))))
             (push (cons block top) placed)
             (incf top (block-height block))
             (setf previous (block-margin-bottom block)))
    (values (+ top previous) (nreverse placed))))

(-> document--place (document item list) null)
(defun document--place (document item blocks)
  "Lay out BLOCKS to the width of the column and store them in ITEM."
  (multiple-value-bind (height placed) (document--stack blocks (document--column-width document))
    (setf (item-blocks item) placed
          (item-height item) height
          (item-leaves item) (loop for (block . top) in placed
                                   append (loop for (paragraph leaf-x leaf-y) in (block-leaves block)
                                                collect (list paragraph leaf-x (+ top leaf-y))))))
  nil)

(-> document--entry-blocks (document entry) list)
(defun document--entry-blocks (document entry)
  "Typeset ENTRY into blocks. If the entry is opened or closed by a click, the transcript is touched."
  (let ((transcript (document-transcript document)))
    (entry-blocks entry
                  (lambda (&optional details-p)
                    (if details-p
                        (setf (tool-entry-show-details-p entry) (not (tool-entry-show-details-p entry)))
                        (setf (entry-expanded-p entry) (not (entry-expanded-p entry))))
                    (transcript-touch transcript entry)))))

(-> document--current-p (document item) boolean)
(defun document--current-p (document item)
  "Return T if ITEM contains the blocks of the current revision of the entry, laid out to the width of the column."
  (and (= (item-revision item) (entry-revision (item-entry item)))
       (= (item-width item) (document--column-width document))))

(-> document--lay-item (document item) null)
(defun document--lay-item (document item)
  "Update the layout of ITEM to the current entry and the width of the column.

The entry is typeset again if the revision changed or if the item has no blocks.
If only the width changed, the blocks are laid out again. An entry that cannot
be typeset is shown as a line with the error."
  (let ((entry (item-entry item)))
    (handler-case
        (document--place document item
                         (if (and (plusp (item-width item))
                                  (= (item-revision item) (entry-revision entry)))
                             (mapcar #'first (item-blocks item))
                             (document--entry-blocks document entry)))
      (error (condition)
        (format *error-output* "~&aui: an entry could not be shown: ~A~%" condition)
        (document--place document item
                         (list (make-text-block
                                (spans (format nil "! this entry could not be shown: ~A"
                                               (condition-report condition)))
                                :style (text-style ':meta :color *red*))))))
    (setf (item-revision item) (entry-revision entry)
          (item-width item)    (document--column-width document)))
  nil)

(-> document--release-item (item) null)
(defun document--release-item (item)
  "Remove the blocks of ITEM and keep the height."
  (setf (item-blocks item) nil
        (item-leaves item) nil
        (item-width item)  0)
  nil)

(-> document--place-items (document) null)
(defun document--place-items (document)
  "Set the top of each item from the heights of the items before it. Set the content height to the sum of all heights."
  (let ((top 0))
    (loop for item across (document--items document)
          do (setf (item-top item) top)
             (incf top (item-height item)))
    (setf (document-content-height document) top))
  nil)

(-> document--aligned-p (vector vector) boolean)
(defun document--aligned-p (items entries)
  "Return T if ITEMS are the layouts of the first entries of ENTRIES in the same order."
  (and (<= (length items) (length entries))
       (loop for item across items
             for entry across entries
             always (eq (item-entry item) entry))))

(-> document--new-item (entry) item)
(defun document--new-item (entry)
  "Return a new item of ENTRY that has the estimated height."
  (make-item :entry entry :height *document-item-estimate*))

(-> document--items-for (document vector) vector)
(defun document--items-for (document entries)
  "Return the items of ENTRIES in order. Entries that already have an item in DOCUMENT keep it."
  (let ((items (document--items document)))
    (if (document--aligned-p items entries)
        (loop for index from (length items) below (length entries)
              do (vector-push-extend (document--new-item (aref entries index)) items)
              finally (return items))
        (let ((known (make-hash-table :test #'eq))
              (next  (make-array (length entries) :adjustable t :fill-pointer 0)))
          (loop for item across items
                do (setf (gethash (item-entry item) known) item))
          (loop for entry across entries
                do (vector-push (or (gethash entry known) (document--new-item entry)) next))
          next))))

(-> document-sync (document integer) null)
(defun document-sync (document width)
  "Update the items to the entries of the transcript, for a column of WIDTH.

This function does not lay out any entry. If entries are added in front, such
as an earlier page of history, the scroll is adjusted so that the same entries are in view."
  (let* ((transcript (document-transcript document))
         (entries    (transcript-entries transcript)))
    (setf (document--column-width document) width)
    (unless (= (transcript-revision transcript) (document--laid-revision document))
      (let* ((old      (document--items document))
             (first    (and (plusp (length old)) (item-entry (aref old 0))))
             (old-top  (and first (item-top (aref old 0))))
             (selected (document--anchor document))
             (items    (document--items-for document entries)))
        (setf (document--items document) items)
        (document--place-items document)
        ;; If a page was added in front, keep the old first entry at the same place.
        (when (and first old-top (not (document-following-p document)))
          (let ((moved (find first items :key #'item-entry)))
            (when (and moved (/= (item-top moved) old-top))
              (incf (document-scroll document) (- (item-top moved) old-top)))))
        (when (and selected (not (and (plusp (length entries)) (eq (aref entries 0) first))))
          (setf (document--anchor document) nil
                (document--focus document)  nil)))
      (setf (document--laid-revision document) (transcript-revision transcript))))
  nil)

(-> document--bring-near (document) boolean)
(defun document--bring-near (document)
  "Lay out the items that are in the view or near it and are not current. Release the blocks of items far from the view.

Returns T if any item was laid out. If an item above the view changes height, the
scroll is adjusted by the same amount."
  (let* ((items  (document--items document))
         (height (fourth (document--viewport document)))
         (first  (or (document--item-at document (max 0 (- (document-scroll document) height))) 0))
         (last   first)
         (shift  0)
         (laid-p nil))
    (loop for index from first below (length items)
          for item = (aref items index)
          for top = (+ (item-top item) shift)
          while (< top (+ (document-scroll document) (* 2 height)))
          do (setf last index)
             (unless (document--current-p document item)
               (let ((before (item-height item)))
                 (document--lay-item document item)
                 (setf laid-p t)
                 (let ((grown (- (item-height item) before)))
                   (incf shift grown)
                   (when (< top (document-scroll document))
                     (incf (document-scroll document) grown))))))
    (unless (zerop shift)
      (document--place-items document))
    (loop for item across items
          for index from 0
          do (unless (or (zerop (item-width item))
                         (<= (- first *document-kept-items*) index (+ last *document-kept-items*)))
               (document--release-item item)))
    laid-p))

(-> document--maximum-scroll (document) integer)
(defun document--maximum-scroll (document)
  "Return the maximum scroll of DOCUMENT. It is the content height minus the viewport height, plus some space below the last entry."
  (max 0 (- (+ (document-content-height document) 32) (fourth (document--viewport document)))))

(-> document-scroll-by (document integer) null)
(defun document-scroll-by (document amount)
  "Scroll DOCUMENT by AMOUNT units. If AMOUNT is positive, the document is scrolled down. At the bottom, the view follows the newest entry again."
  (let* ((maximum (document--maximum-scroll document))
         (target  (max 0 (min maximum (+ (document-scroll document) amount)))))
    (setf (document-scroll document)      target
          (document-following-p document) (>= target maximum))
    (when (and (<= target 0) (document-more-p document) (document-load-earlier document) (minusp amount))
      (funcall (document-load-earlier document))))
  nil)

(-> document-scroll-to-entry (document entry) null)
(defun document-scroll-to-entry (document entry)
  "Scroll DOCUMENT so that ENTRY is at the top of the view. The view only follows the newest entry again if the scroll is at the bottom."
  (let ((item (find entry (document--items document) :key #'item-entry)))
    (when item
      (let* ((maximum (document--maximum-scroll document))
             (target  (max 0 (min maximum (- (item-top item) 12)))))
        (setf (document-scroll document)      target
              (document-following-p document) (>= target maximum)))))
  nil)

(-> document-scroll-to-end (document) null)
(defun document-scroll-to-end (document)
  "Show the newest entry of DOCUMENT and follow it."
  (setf (document-following-p document) t)
  nil)


;;;; -- Selection --

(-> document--position< (list list) boolean)
(defun document--position< (first second)
  "Return T if the selection position FIRST comes before SECOND in reading order."
  (loop for a in first
        for b in second
        do (cond
             ((< a b)
              (return t))
             ((> a b)
              (return nil)))
        finally (return nil)))

(-> document--item-at (document integer) (or null integer))
(defun document--item-at (document y)
  "Return the index of the item that contains the column height Y. If no item contains Y, return the nearest item."
  (let ((items (document--items document)))
    (when (plusp (length items))
      (let ((low  0)
            (high (1- (length items))))
        (loop while (< low high)
              do (let ((middle (floor (+ low high 1) 2)))
                   (if (<= (item-top (aref items middle)) y)
                       (setf low middle)
                       (setf high (1- middle)))))
        low))))

(-> document--position-at (document integer integer) (or null list))
(defun document--position-at (document x y)
  "Return the selection position nearest to the stage point X, Y, or NIL if the document has no text."
  (destructuring-bind (view-x view-y view-width view-height) (document--viewport document)
    (declare (ignore view-height))
    (let* ((column-x (+ view-x (floor (- view-width (document--column-width document)) 2)))
           (local-x  (- x column-x))
           (local-y  (+ (- y view-y) (document-scroll document)))
           (index    (document--item-at document local-y)))
      (when index
        (let* ((item          (aref (document--items document) index))
               (item-y        (- local-y (item-top item)))
               (best          nil)
               (best-distance nil))
          (loop for (paragraph leaf-x leaf-y) in (item-leaves item)
                for leaf-index from 0
                do (let* ((bottom   (+ leaf-y (paragraph-height paragraph)))
                          (distance (cond
                                      ((< item-y leaf-y)
                                       (- leaf-y item-y))
                                      ((>= item-y bottom)
                                       (- item-y bottom -1))
                                      (t
                                       0))))
                     (when (or (null best-distance) (< distance best-distance))
                       (setf best-distance distance
                             best          (list index leaf-index
                                                 (paragraph-index-at paragraph (- local-x leaf-x)
                                                                     (- item-y leaf-y)))))))
          best)))))

(-> document--selection-range (document) (values (or null list) (or null list)))
(defun document--selection-range (document)
  "Return the two ends of the selection in reading order, or NIL twice if nothing is selected."
  (let ((anchor (document--anchor document))
        (focus  (document--focus document)))
    (cond
      ((or (null anchor) (null focus) (equal anchor focus))
       (values nil nil))
      ((document--position< anchor focus)
       (values anchor focus))
      (t
       (values focus anchor)))))

(-> document--selection-table (document) (or null hash-table))
(defun document--selection-table (document)
  "Return a table that maps each selected paragraph to the selected range (START . END), or NIL if nothing is selected."
  (multiple-value-bind (start end) (document--selection-range document)
    (when start
      (let ((table (make-hash-table :test #'eq))
            (items (document--items document)))
        (loop for index from (first start) to (min (first end) (1- (length items)))
              do (loop for (paragraph) in (item-leaves (aref items index))
                       for leaf-index from 0
                       do (let* ((position (list index leaf-index))
                                 ;; An entry typeset again after the selection can have less text.
                                 (limit    (length (paragraph-text paragraph)))
                                 (from     (cond
                                             ((equal position (butlast start))
                                              (third start))
                                             ((document--position< position start)
                                              nil)
                                             (t
                                              0)))
                                 (to       (cond
                                             ((equal position (butlast end))
                                              (third end))
                                             ((document--position< position end)
                                              (length (paragraph-text paragraph)))
                                             (t
                                              nil))))
                            (when (and from to (< from (min to limit)))
                              (setf (gethash paragraph table) (cons from (min to limit)))))))
        table))))

(-> document--lay-selection (document) null)
(defun document--lay-selection (document)
  "Lay out the selected items that have no current blocks, so that their text can be read."
  (multiple-value-bind (start end) (document--selection-range document)
    (when start
      (let ((items (document--items document)))
        (loop for index from (first start) to (min (first end) (1- (length items)))
              do (unless (document--current-p document (aref items index))
                   (document--lay-item document (aref items index))))
        (document--place-items document))))
  nil)

(-> document-selected-text (document) (or null string))
(defun document-selected-text (document)
  "Return the selected text with each paragraph on its own line, or NIL if nothing is selected."
  (document--lay-selection document)
  (let ((table (document--selection-table document)))
    (when table
      (let ((parts nil))
        (loop for item across (document--items document)
              do (loop for (paragraph) in (item-leaves item)
                       do (let ((range (gethash paragraph table)))
                            (when range
                              (push (subseq (paragraph-text paragraph) (first range) (rest range)) parts)))))
        (and parts (format nil "~{~A~^~%~}" (nreverse parts)))))))

(-> document-clear-selection (document) null)
(defun document-clear-selection (document)
  "Select nothing."
  (setf (document--anchor document) nil
        (document--focus document)  nil)
  nil)

(-> document--drag (document keyword &key (:x integer) (:y integer)) null)
(defun document--drag (document phase &key (x 0) (y 0))
  "Update the selection for the drag PHASE at X, Y. The selection is copied at the end of the drag."
  (ecase phase
    (:start
     (setf (document--anchor document) (document--position-at document x y)
           (document--focus document)  nil))
    (:move
     (setf (document--focus document) (document--position-at document x y))
     ;; If the drag goes past an edge, scroll toward that edge.
     (destructuring-bind (view-x view-y view-width view-height) (document--viewport document)
       (declare (ignore view-x view-width))
       (cond
         ((< y (+ view-y 12))
          (document-scroll-by document (- *document-wheel-step*)))
         ((> y (+ view-y view-height -12))
          (document-scroll-by document *document-wheel-step*)))))
    (:end
     (let ((text (document-selected-text document)))
       (when text
         (lightfast:copy-text text)))))
  nil)


;;;; -- Drawing --

(-> document-paint (document stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun document-paint (document stage &key x y width height)
  "Draw DOCUMENT in the box X, Y, WIDTH, HEIGHT of STAGE."
  (let* ((column (max 120 (min *document-measure* (- width (* 2 *document-margin*)))))
         (left   (+ x (floor (- width column) 2))))
    (setf (document--viewport document) (list x y width height))
    (document-sync document column)
    (flet ((settle ()
             (setf (document-scroll document)
                   (if (document-following-p document)
                       (document--maximum-scroll document)
                       (max 0 (min (document-scroll document) (document--maximum-scroll document)))))))
      (settle)
      ;; A layout can move the view. Then other entries are near the view and need a layout.
      (loop repeat 8
            while (document--bring-near document)
            do (settle)))
    (stage-region stage ':document
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (document-scroll-by document (* vertical *document-wheel-step*)))
                  :drag   (lambda (phase drag-x drag-y)
                            (document--drag document phase :x drag-x :y drag-y))
                  :cursor ':insert)
    (with-clip (x y width height)
      (let* ((scroll            (document-scroll document))
             (items             (document--items document))
             (first             (or (document--item-at document scroll) 0))
             (*paint-stage*     stage)
             (*paint-top*       y)
             (*paint-bottom*    (+ y height))
             (*paint-selection* (document--selection-table document)))
        (loop for index from first below (length items)
              do (let* ((item     (aref items index))
                        (item-top (+ y (- (item-top item) scroll))))
                   (when (>= item-top (+ y height))
                     (return))
                   (loop for (block . top) in (item-blocks item)
                         do (let ((block-top (+ item-top top)))
                              (when (and (< block-top (+ y height))
                                         (> (+ block-top (block-height block)) y))
                                (block-paint block left block-top))))))))
    (document--paint-scrollbar document stage :x x :y y :width width :height height)
    ;; If the view is away from the end, draw a button that goes back to the end.
    (unless (document-following-p document)
      (paint-button stage ':to-newest ""
                    :x       (- (+ x width) 54)
                    :y       (- (+ y height) 44)
                    :width   30
                    :height  30
                    :mark    ':arrow-down
                    :action  (lambda () (document-scroll-to-end document))
                    :tooltip "Show the newest output and follow it")))
  nil)

(-> document--paint-scrollbar
    (document stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun document--paint-scrollbar (document stage &key (x 0) (y 0) (width 0) (height 0))
  "Draw a thin draggable scroll thumb at the right edge if the content is taller than the view."
  (let* ((content (+ (document-content-height document) 32))
         (maximum (document--maximum-scroll document)))
    (when (and (plusp maximum) (> height 40))
      (let* ((thumb-height (max 28 (floor (* height height) content)))
             (track        (- height thumb-height))
             (thumb-y      (+ y (floor (* track (document-scroll document)) maximum)))
             (thumb-x      (- (+ x width) 8))
             (hot-p        (or (stage-hot-p stage ':scrollbar) (stage-pressed-p stage ':scrollbar))))
        (if hot-p
            (paint-fill (- thumb-x 2) thumb-y :width 6 :height thumb-height)
            (paint-dither thumb-x thumb-y :width 4 :height thumb-height :step 2))
        (stage-region stage ':scrollbar
                      :x      (- (+ x width) 14)
                      :y      y
                      :width  14
                      :height height
                      :drag   (lambda (phase drag-x drag-y)
                                (declare (ignore phase drag-x))
                                (when (plusp track)
                                  (let ((fraction (/ (- drag-y y (floor thumb-height 2)) track)))
                                    (setf (document-scroll document)
                                          (max 0 (min maximum (round (* maximum fraction))))
                                          (document-following-p document)
                                          (>= (document-scroll document) maximum)))))))))
  nil)
