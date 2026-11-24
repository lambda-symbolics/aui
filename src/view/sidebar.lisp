(in-package #:aui)

;;;; -- Sidebar --

(defparameter *sidebar-width* 292
  "The width of the docked channel list.")

(defparameter *sidebar-row-height* 30
  "The height of one channel row.")

(defparameter *sidebar-docked-minimum* 980
  "The minimum width of a window that has the channel list beside the conversation.")

(defparameter *sidebar-home-indent* 12
  "The indentation of the workspaces and channels below home.")

(defparameter *sidebar-list-top* 106
  "The distance from the top edge of the channel list to the first row. The heading and the filter are above the rows.")

(defparameter *sidebar-footer-height* 56
  "The height of the space at the bottom of the channel list for the buttons that create and delete channels.")

(-> sidebar--indent (list) integer)
(defun sidebar--indent (entry)
  "Return the indentation of the row of ENTRY if the entry is below home, or zero."
  (if (plusp (or (getf entry :indent) 0))
      *sidebar-home-indent*
      0))

(-> sidebar-entries (workbench) list)
(defun sidebar-entries (workbench)
  "Return the channel tree of the world, or the channel tree of the demonstration."
  (if (workbench-demo-p workbench)
      (workbench-demo-entries workbench)
      (world-entries (workbench-world workbench))))

(-> sidebar--visible-entries (workbench) list)
(defun sidebar--visible-entries (workbench)
  "Return the entries of the list. Without a filter, return the whole tree. With a filter, return the matching channels, with the best match first.

The filter is matched loosely against the agent name, identifier, title and
folder of each channel. The letters of the filter must appear in order.
Channels in open and closed groups are searched."
  (let ((filter (string-trim " " (workbench-filter workbench))))
    (if (zerop (length filter))
        (sidebar-entries workbench)
        (sito:rank filter
                   (mapcar (lambda (row) (list* :depth 0 row))
                           (if (workbench-demo-p workbench)
                               (remove ':channel (sidebar-entries workbench)
                                       :key (lambda (entry) (getf entry :kind)) :test-not #'eq)
                               (world-channel-rows (workbench-world workbench))))
                   :key (lambda (row)
                          (list (getf row :name) (channel-display-name (getf row :id))
                                (getf row :title) (getf row :path)))))))

(-> sidebar--channel-label (list) (values string (or null string)))
(defun sidebar--channel-label (entry)
  "Return the name and the title of the channel ENTRY. The title is NIL if the channel has none."
  (let ((name  (portable-string (getf entry :name)))
        (title (portable-string (getf entry :title))))
    (values (or name (channel-display-name (getf entry :id)))
            title)))

(-> sidebar--entry-label (workbench string) string)
(defun sidebar--entry-label (workbench identifier)
  "Return the label of the channel IDENTIFIER in the list.

The label is the title, or the name of the agent, or the readable identifier."
  (let ((entry (find identifier (sidebar-entries workbench)
                     :key  (lambda (entry)
                             (getf entry :id))
                     :test #'equal)))
    (or (portable-string (getf entry :title))
        (portable-string (getf entry :name))
        (channel-display-name identifier))))

(-> sidebar-confirm-delete (workbench string) null)
(defun sidebar-confirm-delete (workbench identifier)
  "Ask the person whether to delete the channel IDENTIFIER, and delete it if the answer is yes."
  (workbench-pick workbench
                  ;; The first choice is taken on Enter, so the safe choice comes first.
                  (list (make-choice :label "Keep"
                                     :value ':keep)
                        (make-choice :label  "Delete"
                                     :detail "The channel, the channels nested in it and their histories"
                                     :value  ':delete))
                  (lambda (answer)
                    (when (eq answer ':delete)
                      (workbench-delete-channel workbench identifier)))
                  :title (format nil "Delete ~A?" (sidebar--entry-label workbench identifier)))
  nil)

(-> sidebar--close-drawer (workbench) null)
(defun sidebar--close-drawer (workbench)
  "Close the channel list if it fills the whole window."
  (setf (workbench-drawer-open-p workbench) nil)
  nil)

(-> sidebar--open (workbench string) null)
(defun sidebar--open (workbench identifier)
  "Show the channel IDENTIFIER and move the keyboard focus to the composer."
  (sidebar--close-drawer workbench)
  (unless (equal identifier (workbench-channel workbench))
    (workbench-open workbench :channel identifier))
  (window-arrange workbench)
  (window-focus-composer workbench)
  (workbench-redraw workbench)
  nil)

(-> sidebar--home-p (workbench (or null string)) boolean)
(defun sidebar--home-p (workbench identifier)
  "Return T if IDENTIFIER names home. Home is the channel above all other channels, and it cannot be deleted."
  (and identifier
       (find-if (lambda (entry)
                  (and (eq (getf entry :kind) ':inbox)
                       (equal (getf entry :id) identifier)))
                (sidebar-entries workbench))
       t))

(-> sidebar--channel-menu (workbench list) null)
(defun sidebar--channel-menu (workbench entry)
  "Open the menu of the channel ENTRY at the pointer, and do the chosen action."
  (let ((identifier (getf entry :id))
        (stage      (workbench-stage workbench)))
    (workbench-menu
     workbench
     (list (make-choice :label "Open"
                        :value (lambda () (sidebar--open workbench identifier)))
           (make-choice :label "Send a message…"
                        :value (lambda ()
                                 (workbench-ask workbench
                                                (format nil "Message to ~A" (channel-display-name identifier))
                                                (lambda (text)
                                                  (when (plusp (length (string-trim " " text)))
                                                    (workbench-tell workbench identifier text))))))
           (make-choice :label "Copy identifier"
                        :value (lambda () (lightfast:copy-text identifier)))
           (make-choice)
           (make-choice :label "Stop serving"
                        :value (lambda () (workbench-close-channel workbench identifier)))
           (make-choice :label "Delete…"
                        :value (lambda () (sidebar-confirm-delete workbench identifier))))
     :x (stage-pointer-x stage)
     :y (stage-pointer-y stage)))
  nil)

(-> paint-channel-state (t integer integer &key (:waiting-for-jobs-p boolean)) null)
(defun paint-channel-state (state x y &key waiting-for-jobs-p)
  "Draw the mark of a channel in STATE in the 12 by 12 box at X, Y.

A working channel has a green dot. An idle channel has a ring, with a green point
in it if WAITING-FOR-JOBS-P is true. A channel that is absent from the server has a speck."
  (case state
    ((:active :working :cancelling)
     (paint-mark ':dot x y :size 12 :color (accent-color)))
    ((:idle :paused)
     ;; An idle channel that waits for jobs has a green point in the ring.
     (paint-mark ':ring x y :size 12)
     (when waiting-for-jobs-p
       (paint-mark ':point x y :size 12 :color (accent-color))))
    (:absent
     (paint-fill (+ x 5) (+ y 5) :width 2 :height 2))
    (otherwise
     (paint-mark ':ring x y :size 12)))
  nil)

(-> sidebar--paint-channel (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-channel (workbench stage entry &key x y width)
  "Draw one channel row. The row contains the state mark, the name of the agent, the title and the unread counts."
  (let* ((identifier (getf entry :id))
         (region     (list ':channel identifier))
         (current-p  (or (getf entry :current-p)
                         (equal identifier (workbench-channel workbench))))
         (hot-p      (stage-hot-p stage region))
         (state      (getf entry :state))
         (indent     (+ 14 (sidebar--indent entry) (* 16 (or (getf entry :depth) 0))))
         (height     *sidebar-row-height*)
         (unread     (or (getf entry :unread) 0))
         (mentions   (or (getf entry :mentions) 0))
         (pending    (or (getf entry :pending) 0)))
    (flet ((content ()
             (let ((mark-y (+ y (floor (- height 12) 2)))
                   (right  (- (+ x width) 10)))
               (paint-channel-state state (+ x indent) mark-y
                                    :waiting-for-jobs-p (and (getf entry :waiting-for-jobs-p) t))
               (when (plusp mentions)
                 (decf right (+ 6 (paint-badge (format nil "@~D" mentions) :right right :y y :height height))))
               (when (plusp unread)
                 (decf right (+ 6 (paint-badge (princ-to-string unread) :right right :y y :height height))))
               (when (plusp pending)
                 (let ((text (format nil "~D waiting" pending)))
                   (paint-text-in text (text-style ':meta)
                                  :x      (- right (text-width text (text-style ':meta)))
                                  :y      y
                                  :width  (text-width text (text-style ':meta))
                                  :height height)
                   (decf right (+ 8 (text-width text (text-style ':meta))))))
               (multiple-value-bind (name title) (sidebar--channel-label entry)
                 (let* ((text-x (+ x indent 20))
                        (bold   (text-style ':small :face ':serif-bold))
                        (plain  (text-style ':small))
                        (fitted (text-fit name bold (max 0 (- right text-x))))
                        (end    (paint-text fitted text-x (+ y (floor (- height (text-style-line-height bold)) 2)
                                                           (text-baseline bold))
                                            :style bold)))
                   (when (and title (> (- right end) 40))
                     (paint-text (text-fit title plain (- right end 10)) (+ end 10) (+ y (floor (- height (text-style-line-height plain)) 2)
                                                                                     (text-baseline plain))
                                 :style plain)))))))
      (if (or current-p hot-p)
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage region
                  :x       x
                  :y       y
                  :width   width
                  :height  height
                  :action  (lambda () (sidebar--open workbench identifier))
                  :menu    (lambda () (sidebar--channel-menu workbench entry))
                  :tooltip (channel-display-name identifier)))
  nil)

(-> sidebar--paint-workspace (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-workspace (workbench stage entry &key x y width)
  "Draw a workspace heading. The heading contains the open or closed mark, the name, the path and, if it is closed, the count of hidden channels."
  (let* ((key    (getf entry :key))
         (region (list ':workspace key))
         (height *sidebar-row-height*)
         (bold   (text-style ':meta-bold))
         (meta   (text-style ':meta))
         (hot-p  (stage-hot-p stage region))
         (left   (+ x (sidebar--indent entry))))
    (flet ((content ()
             (paint-mark (if (getf entry :open-p) ':open ':closed) (+ left 12) (+ y (floor (- height 10) 2))
                         :size 10)
             (let* ((right (- (+ x width) 10))
                    (end   (paint-text (text-fit (or (getf entry :name) "") bold (- width 60 (- left x)))
                                       (+ left 28)
                                       (+ y (floor (- height (text-style-line-height bold)) 2) (text-baseline bold))
                                       :style bold)))
               (when (and (not (getf entry :open-p)) (getf entry :hidden))
                 (decf right (+ 6 (paint-badge (format nil "~D" (getf entry :hidden))
                                               :right  right
                                               :y      y
                                               :height height))))
               (when (> (- right end) 60)
                 (paint-text-in (text-fit-tail (or (getf entry :path) "") meta (- right end 12)) meta
                                :x      (+ end 12)
                                :y      y
                                :width  (- right end 12)
                                :height height)))))
      (if hot-p
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage region
                  :x      x
                  :y      y
                  :width  width
                  :height height
                  :action (lambda ()
                            (unless (workbench-demo-p workbench)
                              (world-toggle-node (workbench-world workbench) key))
                            (workbench-redraw workbench))))
  nil)

(-> sidebar--paint-home (workbench stage list &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-home (workbench stage entry &key x y width)
  "Draw the row of home with the unread count. Home is the channel above all other channels."
  (let* ((identifier (getf entry :id))
         (region     (list ':channel identifier))
         (height     *sidebar-row-height*)
         (current-p  (or (getf entry :current-p) (equal identifier (workbench-channel workbench))))
         (hot-p      (stage-hot-p stage region))
         (unread     (or (getf entry :unread) 0)))
    (flet ((content ()
             (paint-text-in "Home" (text-style ':small :face ':serif-bold)
                            :x      (+ x 14)
                            :y      y
                            :width  (- width 80)
                            :height height)
             (when (plusp unread)
               (paint-badge (princ-to-string unread) :right (- (+ x width) 10) :y y :height height))))
      (if (or current-p hot-p)
          (with-inversion (x y width height)
            (content))
          (content)))
    (stage-region stage region
                  :x       x
                  :y       y
                  :width   width
                  :height  height
                  :action  (lambda () (sidebar--open workbench identifier))
                  :tooltip "Home, with the messages sent to you"))
  nil)

(-> sidebar--paint-footer (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun sidebar--paint-footer (workbench stage &key x y width)
  "Draw the two buttons at the bottom of the list. The buttons are used to create a new channel and to delete the shown channel."
  (let* ((channel  (workbench-channel workbench))
         (delete-w 104)
         (new-w    (- width 28 8 delete-w)))
    (paint-fill x y :width width :height *sidebar-footer-height* :color *paper*)
    (paint-button stage ':new-channel "New channel"
                  :x         (+ x 14)
                  :y         (+ y 10)
                  :width     new-w
                  :height    34
                  :mark      ':plus
                  :mark-size 13
                  :action    (lambda ()
                               (sidebar--close-drawer workbench)
                               (workbench-new-channel workbench)
                               (window-arrange workbench)
                               (window-focus-composer workbench))
                  :tooltip   (format nil "Create a new channel in ~A (Ctrl+N)"
                                     (short-directory (workbench-directory workbench))))
    (paint-button stage ':delete-channel "Delete…"
                  :x         (+ x 14 new-w 8)
                  :y         (+ y 10)
                  :width     delete-w
                  :height    34
                  :style     (text-style ':label-bold :color *red*)
                  :enabled-p (and channel (not (sidebar--home-p workbench channel)))
                  :action    (lambda ()
                               (sidebar-confirm-delete workbench channel))
                  :tooltip   "Delete the shown channel, the channels nested in it and their histories"))
  nil)

(-> sidebar-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun sidebar-paint (workbench stage &key x y width height)
  "Draw the channel list in the box.

The list contains the wordmark, the button used to hide the list, the filter field,
the rows, and the buttons for a new channel and for deleting the shown channel."
  (let* ((entries     (sidebar--visible-entries workbench))
         (top         (+ y *sidebar-list-top*))
         (bottom      (- (+ y height) *sidebar-footer-height*))
         (list-height (max 0 (- bottom top)))
         (content     (* *sidebar-row-height* (length entries)))
         (scroll      (max 0 (min (workbench-sidebar-scroll workbench) (max 0 (- content list-height))))))
    (setf (workbench-sidebar-scroll workbench) scroll)
    (paint-fill x y :width width :height height :color *paper*)
    (paint-text "Autolith" (+ x 14) (+ y 40) :style (text-style ':title))
    (paint-button stage ':hide-sidebar ""
                  :x       (- (+ x width) 42)
                  :y       (+ y 16)
                  :width   28
                  :height  28
                  :mark    ':panel
                  :action  (lambda () (window-toggle-sidebar workbench))
                  :tooltip "Hide the channels (Ctrl+S)")
    ;; The filter field is a native input over this frame. While an overlay
    ;; is open, the input is hidden and its text is drawn here instead.
    (paint-frame (+ x 14) (+ y 58) :width (- width 28) :height 34)
    (when (workbench-overlay workbench)
      (let ((filter (workbench-filter workbench)))
        (paint-text-in (if (plusp (length filter)) filter "Find a channel")
                       (text-style ':list :face (if (plusp (length filter)) ':serif ':serif-italic))
                       :x      (+ x 20)
                       :y      (+ y 58)
                       :width  (- width 40)
                       :height 34)))
    (stage-region stage ':sidebar-list
                  :x      x
                  :y      top
                  :width  width
                  :height list-height
                  :wheel  (lambda (vertical horizontal)
                            (declare (ignore horizontal))
                            (setf (workbench-sidebar-scroll workbench)
                                  (max 0 (+ (workbench-sidebar-scroll workbench)
                                            (* vertical *sidebar-row-height*))))))
    (with-clip (x top width list-height)
      (loop for entry in entries
            for row-y from (- top scroll) by *sidebar-row-height*
            do (when (and (< row-y bottom) (> (+ row-y *sidebar-row-height*) top))
                 (ecase (getf entry :kind)
                   (:inbox
                    (sidebar--paint-home workbench stage entry :x x :y row-y :width width))
                   (:workspace
                    (sidebar--paint-workspace workbench stage entry :x x :y row-y :width width))
                   (:channel
                    (sidebar--paint-channel workbench stage entry :x x :y row-y :width width)))))
      (when (null entries)
        (paint-text-in (cond ((eq (workbench-attachment workbench) ':opening)
                              "Connecting…")
                             ((eq (workbench-attachment workbench) ':failed)
                              "Connection failed.")
                             ((plusp (length (workbench-filter workbench)))
                              "No matching channels.")
                             (t
                              "No channels yet.")) (text-style ':small :face ':serif-italic)
                       :x      (+ x 14)
                       :y      top
                       :width  (- width 28)
                       :height 40)))
    (sidebar--paint-footer workbench stage :x x :y bottom :width width))
  nil)
