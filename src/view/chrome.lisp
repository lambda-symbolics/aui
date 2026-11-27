(in-package #:aui)

;;;; -- Chrome --

(defparameter *header-height* 62
  "The height of the header above the conversation.")

(defparameter *status-height* 30
  "The height of the status line under the composer.")

(-> workbench-tell (workbench string string) null)
(defun workbench-tell (workbench channel text)
  "Send TEXT to CHANNEL as the person, on another thread. A refusal is reported above the composer."
  (let ((world (workbench-world workbench)))
    (workbench-run-aside workbench (list ':tell channel)
                         (lambda ()
                           (world-tell world channel text))))
  nil)

(-> workbench-close-channel (workbench string) null)
(defun workbench-close-channel (workbench channel)
  "Ask the server to close CHANNEL, on another thread. A refusal is reported above the composer."
  (let ((world (workbench-world workbench)))
    (workbench-run-aside workbench (list ':close-channel channel)
                         (lambda ()
                           (world-close-channel world channel))))
  nil)

(-> chrome--channel-agent (workbench) (or null string))
(defun chrome--channel-agent (workbench)
  "Return the name of the agent that resides in the channel shown, or NIL if there is no such agent."
  (unless (workbench-demo-p workbench)
    (let ((channel (workbench-channel workbench)))
      (loop for participant in (workbench-participants workbench)
            when (and (eq (getf participant :kind) ':agent)
                      (equal (getf participant :residence) channel))
              do (return (getf participant :name))))))

(-> chrome--other-agents (workbench) list)
(defun chrome--other-agents (workbench)
  "Return the names of the agents that reside outside the channel shown, including parked agents."
  (unless (workbench-demo-p workbench)
    (let ((channel (workbench-channel workbench)))
      (loop for participant in (workbench-participants workbench)
            when (and (eq (getf participant :kind) ':agent)
                      (not (equal (getf participant :residence) channel))
                      (stringp (getf participant :name)))
              collect (getf participant :name)))))

(-> chrome--sign-in-to (workbench string) null)
(defun chrome--sign-in-to (workbench name)
  "Sign in to the provider NAME. Ask for an API key if the provider uses one. Otherwise, open the sign-in page in the browser and show a panel about it while Autolith waits for the login."
  (let ((world (workbench-world workbench)))
    (ecase (world-sign-in-kind world name)
      (:key
       (workbench-ask workbench (format nil "API key for ~A" name)
                      (lambda (key)
                        (when (plusp (length (string-trim " " key)))
                          (workbench-show-panel workbench (format nil "Sign in to ~A" name) nil)
                          (workbench-run-aside workbench (list ':sign-in name)
                                               (lambda ()
                                                 (world-sign-in world name :key (string-trim " " key))))))
                      :note "paste the key; Autolith checks and stores it on this machine."))
      (:browser
       (workbench-show-panel
        workbench (format nil "Sign in to ~A" name)
           (panel--empty "finish signing in in your browser"))
       (workbench-run-aside workbench (list ':sign-in name)
                            (lambda ()
                              (world-sign-in world name))))
      ((nil)
       (workbench-show-panel
        workbench (format nil "Sign in to ~A" name)
          (panel--empty (format nil "run autolith auth ~A in a terminal to sign in" name))))))
  nil)

(-> chrome--sign-in (workbench) null)
(defun chrome--sign-in (workbench)
  "Show a picker of the providers to sign in to. The providers come from the list of models of the channel and from the providers known to Autolith. Each provider is shown as signed in or not signed in."
  (let* ((stored (remove-duplicates
                  (loop for model in (workbench-models workbench)
                        when (portable-true-p (portable-get model "credentialStored"))
                          collect (portable-get model "provider"))
                  :test #'equal))
         (names  (sort (remove-duplicates
                        (append (loop for model in (workbench-models workbench)
                                      when (stringp (portable-get model "provider"))
                                        collect (portable-get model "provider"))
                                (world-providers))
                        :test #'equal)
                       #'string<)))
    (workbench-pick workbench
                    (mapcar (lambda (name)
                              (make-choice :label     name
                                           :detail    (if (member name stored :test #'equal)
                                                          "signed in"
                                                          "not signed in")
                                           :value     name
                                           :current-p (and (member name stored :test #'equal) t)))
                            names)
                    (lambda (name)
                      (chrome--sign-in-to workbench name))
                    :title "Sign in to a provider"))
  nil)

(-> chrome--move-agent-here (workbench) null)
(defun chrome--move-agent-here (workbench)
  "Show the agents that reside outside the channel shown, and move the chosen agent into the channel."
  (let ((channel (workbench-channel workbench)))
    (workbench-pick
     workbench
     (loop for participant in (and (not (workbench-demo-p workbench))
                                   (workbench-participants workbench))
           when (and (eq (getf participant :kind) ':agent)
                     (not (equal (getf participant :residence) channel))
                     (stringp (getf participant :name)))
             collect (make-choice :label  (getf participant :name)
                                  :detail (if (getf participant :residence)
                                              (format nil "in ~A" (channel-display-name (getf participant :residence)))
                                              "parked")
                                  :value  (getf participant :name)))
     (lambda (name)
       (workbench-submit workbench (format nil "(move ~S)" name)))
     :title "Move an agent here"))
  nil)

(-> chrome--channel-menu (workbench &key (:x integer) (:y integer)) null)
(defun chrome--channel-menu (workbench &key (x 0) (y 0))
  "Open the menu of the channel shown and of the agent of the channel, with a corner at X, Y."
  (let ((agent (chrome--channel-agent workbench)))
    (flet ((item (label function &optional detail)
             (make-choice :label label :detail detail :value function)))
      (workbench-menu
       workbench
       (append
        (list (item "New channel here"
                    (lambda () (workbench-new-channel workbench)))
              (item "New channel in another folder…"
                    (lambda () (workbench-new-channel-elsewhere workbench)))
              (make-choice)
              (item "Status"
                    (lambda () (panel-show-output workbench "Status" "status")))
              (item "Protected folders"
                    (lambda () (panel-show-protected-folders workbench)))
              (item "Plan"
                    (lambda () (panel-show-plan workbench)))
              (item "Agenda"
                    (lambda () (panel-show-agenda workbench)))
              (item "Memories"
                    (lambda () (panel-show-memories workbench)))
              (item "Jobs"
                    (lambda () (panel-show-output workbench "Jobs" "tasks")))
              (item "Compact the context now"
                    (lambda () (workbench-submit workbench "(compact)")))
              (make-choice)
              (item "Rename the agent…"
                    (lambda ()
                      (workbench-ask workbench "Rename the agent"
                                     (lambda (name)
                                       (when (plusp (length (string-trim " " name)))
                                         (workbench-submit workbench (format nil "(rename ~S)" name))))
                                     :note    "The name of the agent in @mentions."
                                     :initial (or agent "")))))
        (if agent
            (list (item "Park the agent"
                        (lambda () (workbench-submit workbench (format nil "(dismiss ~S)" agent)))))
            (list (item "Create an agent for this channel"
                        (lambda () (workbench-submit workbench "(domestic)")))))
        (when (chrome--other-agents workbench)
          (list (item "Move an agent here…"
                      (lambda () (chrome--move-agent-here workbench)))))
        (list (make-choice)
              (item "Sign in to a model provider…"
                    (lambda () (chrome--sign-in workbench)))
              (item "Stop the world server…"
                    (lambda () (chrome--confirm-stop-server workbench)))
              (item "Copy the channel identifier"
                    (lambda ()
                      (when (workbench-channel workbench)
                        (lightfast:copy-text (workbench-channel workbench)))))
              (item "Copy the conversation"
                    (lambda ()
                      (lightfast:copy-text (transcript-plain-text (workbench-transcript workbench)))))))
       :x x
       :y y)))
  nil)

(-> chrome--confirm-stop-server (workbench) null)
(defun chrome--confirm-stop-server (workbench)
  "Ask the person whether to stop the world server, and stop it if the answer is yes."
  (workbench-pick workbench
                  (list (make-choice :label "Keep it running"
                                     :value ':keep)
                        (make-choice :label  "Stop the server"
                                     :detail "All work is stopped. The channels and histories are kept"
                                     :value  ':stop))
                  (lambda (answer)
                    (when (eq answer ':stop)
                      (workbench-stop-server workbench)))
                  :title "Stop the world server?")
  nil)

(-> chrome-paint-directory
    (workbench stage t &key (:x integer) (:y integer) (:width integer) (:height integer)) integer)
(defun chrome-paint-directory (workbench stage identifier &key x y width height)
  "Draw the folder of the channel shown, cut at the start to fit WIDTH. Return the width of the text drawn.

If the channel is attached, the folder is a region named IDENTIFIER. A click
on the region asks for another folder."
  (let* ((meta      (text-style ':meta))
         (text      (text-fit-tail (short-directory (workbench-directory workbench)) meta (max 0 (- width 8))))
         (wide      (+ 8 (text-width text meta)))
         (enabled-p (workbench-attached-p workbench))
         (hot-p     (and enabled-p (stage-hot-p stage identifier))))
    (flet ((content ()
             (paint-text-in text meta :x (+ x 4) :y y :width (- wide 8) :height height)))
      (if hot-p
          (with-inversion (x y wide height)
            (content))
          (content)))
    (when enabled-p
      (stage-region stage identifier
                    :x       x
                    :y       y
                    :width   wide
                    :height  height
                    :action  (lambda () (workbench-change-directory workbench))
                    :tooltip "Change the folder that the agent works in"))
    wide))

(-> chrome--agent-name (workbench) (or null string))
(defun chrome--agent-name (workbench)
  "Return the name of the agent of the channel shown, as given in the channel list. This is the name used in mentions. Return NIL if the channel list does not contain a name."
  (let ((entry (find (workbench-channel workbench) (sidebar-entries workbench)
                     :key  (lambda (entry)
                             (getf entry :id))
                     :test #'equal)))
    (portable-string (getf entry :name))))

(-> header-paint
    (workbench stage &key (:x integer) (:y integer) (:width integer) (:sidebar-toggle-p boolean)
               (:band-p boolean))
    null)
(defun header-paint (workbench stage &key x y width sidebar-toggle-p (band-p t))
  "Draw the header. It contains the title of the channel, a line with the name of the agent, the identifier and the folder of the channel, and the buttons for the settings, the linked files, the commands and the menu of the channel.

If BAND-P is true, a band of dither is drawn under the header."
  (let* ((left  (+ x 24 (if sidebar-toggle-p 38 0)))
         (right (- (+ x width) 20))
         (title (text-style ':heading-2))
         (meta  (text-style ':meta)))
    (paint-fill x y :width width :height *header-height* :color *paper*)
    (when sidebar-toggle-p
      (paint-button stage ':toggle-sidebar ""
                    :x       (+ x 18)
                    :y       (+ y 15)
                    :width   30
                    :height  30
                    :mark    ':panel
                    :action  (lambda () (window-toggle-sidebar workbench))
                    :tooltip "Show the channels (Ctrl+S)"))
    (flet ((button (identifier mark action tooltip)
             (paint-button stage identifier ""
                           :x       (- right 30)
                           :y       (+ y 15)
                           :width   30
                           :height  30
                           :mark    mark
                           :action  action
                           :tooltip tooltip)
             (decf right 36)))
      (when (workbench-channel workbench)
        (let ((menu-x (- right 30)))
          (button ':channel-menu ':more
                  (lambda ()
                    (chrome--channel-menu workbench :x menu-x :y (+ y 47)))
                  "This channel")))
      (button ':open-commands ':slash
              (lambda () (workbench-show-overlay workbench ':commands))
              "Commands (Ctrl+K)")
      (when (workbench-channel workbench)
        (button ':open-files ':page
                (lambda () (files-show workbench))
                "Files linked in this conversation"))
      (button ':open-settings ':sliders
              (lambda () (workbench-show-overlay workbench ':settings))
              "Settings (Ctrl+,)")
      (decf right 8))
    (paint-text (text-fit (workbench-title workbench) title (- right left)) left (+ y 30) :style title)
    (let ((cursor (if (workbench-channel workbench)
                      (paint-text (text-fit (format nil "~@[~A · ~]~A · "
                                                    (chrome--agent-name workbench)
                                                    (channel-display-name (workbench-channel workbench)))
                                            meta (- right left))
                                  left (+ y 50)
                                  :style meta)
                      left)))
      (chrome-paint-directory workbench stage ':header-directory
                              :x      (- cursor 4)
                              :y      (- (+ y 50) (text-baseline meta))
                              :width  (max 0 (- right cursor -4))
                              :height (text-style-line-height meta)))
    ;; Draw a band of dither under the header, which separates it from the conversation.
    (when band-p
      (paint-dither x (+ y *header-height* -3) :width width :height 3 :step 3)))
  nil)

(-> chrome--count (integer) string)
(defun chrome--count (tokens)
  "Return the token count TOKENS as short text, such as 950, 40.1K or 1.2M."
  (cond
    ((< tokens 1000)
     (princ-to-string tokens))
    ((< tokens 1000000)
     (format nil "~,1FK" (/ tokens 1000)))
    (t
     (format nil "~,1FM" (/ tokens 1000000)))))

(-> chrome--elapsed (integer) string)
(defun chrome--elapsed (seconds)
  "Return SECONDS as minutes and seconds."
  (multiple-value-bind (minutes rest) (floor seconds 60)
    (format nil "~D:~2,'0D" minutes rest)))

(-> status-paint (workbench stage &key (:x integer) (:y integer) (:width integer)) null)
(defun status-paint (workbench stage &key x y width)
  "Draw the status line under the composer. While the agent works, the line contains the time since the turn started. Otherwise, it contains the folder of the channel. The use of the context is drawn at the right.

Context counts estimated by Autolith start with a tilde."
  (let* ((meta   (text-style ':meta))
         (height *status-height*)
         (right  (+ x width)))
    (multiple-value-bind (used limit estimated-p) (workbench-context workbench)
      (when (and used limit (plusp limit))
        (let* ((label   (format nil "~:[~;~~~]~A / ~A" estimated-p (chrome--count used) (chrome--count limit)))
               (label-w (text-width label meta))
               (bar-w   64)
               (bar-x   (- right label-w 10 bar-w))
               (bar-y   (+ y (floor (- height 8) 2)))
               (filled  (floor (* bar-w (min 1 (/ used limit))))))
          (paint-text-in label meta :x (- right label-w) :y y :width label-w :height height)
          (paint-frame bar-x bar-y :width bar-w :height 8)
          (paint-fill bar-x bar-y :width filled :height 8)
          (stage-region stage ':context-meter
                        :x       bar-x
                        :y       y
                        :width   (- right bar-x)
                        :height  height
                        :tooltip (format nil "Context: ~:[~;about ~]~D of ~D tokens~:[~;, estimated~]"
                                         estimated-p used limit estimated-p))
          (setf right (- bar-x 14)))))
    (cond
      ((workbench-working-p workbench)
       (setf (stage-animating-p stage) t)
       (paint-spinner x (+ y (floor (- height 14) 2)) :size 14 :phase (stage-phase stage) :color *green*)
       (paint-text-in (format nil "working~@[ ~A~]"
                              (and (workbench-working-since workbench)
                                   (chrome--elapsed (- (get-universal-time)
                                                       (workbench-working-since workbench))))) meta
                      :x      (+ x 22)
                      :y      y
                      :width  (max 0 (- right x 22))
                      :height height))
      (t
       (chrome-paint-directory workbench stage ':status-directory
                               :x      (- x 4)
                               :y      (+ y (floor (- height (text-style-line-height meta)) 2))
                               :width  (max 0 (- right x -4))
                               :height (text-style-line-height meta)))))
  nil)


;;;; -- Approval --

(-> approval--label (list) string)
(defun approval--label (option)
  "Return the label of the button for the approval OPTION."
  (let ((value (getf option :value)))
    (cond
      ((equal value "once")
       "Allow once")
      ((equal value "always")
       "Always allow")
      ((equal value "auto")
       "Decide automatically")
      ((equal value "sandbox")
       "In the sandbox")
      ((equal value "full")
       "Full access")
      ((equal value "deny")
       "Deny")
      (t
       (string-capitalize (substitute #\Space #\- (or value "choose")))))))

(defparameter *approval-row-height* 28
  "The height of a choice in the approval card.")

(-> approval--sentence (string) string)
(defun approval--sentence (text)
  "Return TEXT with a capital first letter."
  (if (plusp (length text))
      (concatenate 'string (string (char-upcase (char text 0))) (subseq text 1))
      text))

(-> approval-height (workbench integer) integer)
(defun approval-height (workbench width)
  "Return the height of the approval card at WIDTH, or zero if there is no request for approval."
  (let ((approval (workbench-approval workbench)))
    (if (null approval)
        0
        (let ((entry (make-paragraph (list (make-span (or (getf approval :entry) "") nil))
                                     :style (text-style ':code) :wrap ':anywhere)))
          (+ 28 12
             (min 120 (paragraph-layout entry (- width 44)))
             10
             (* *approval-row-height* (length (getf approval :options)))
             8 *shadow-offset* 10)))))

(-> approval-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun approval-paint (workbench stage &key x y width height)
  "Draw the request and its numbered choices on a paper approval card."
  (let* ((approval (workbench-approval workbench))
         (options  (getf approval :options))
         (box-h    (- height *shadow-offset* 10))
         (bar      (paint-window-frame x y :width width :height box-h
                                       :title (format nil "Approve: ~A" (or (getf approval :title) "request"))))
         (entry    (make-paragraph (list (make-span (or (getf approval :entry) "") nil))
                                   :style (text-style ':code) :wrap ':anywhere))
         (rows-y   (- (+ y box-h) 8 (* *approval-row-height* (length options))))
         (label-w  (loop for option in options
                         maximize (text-width (approval--label option) (text-style ':label-bold))))
         (detail-x (+ x 14 30 label-w 22))
         ;; Only draw the descriptions if there is room for them. The
         ;; descriptions are also the tooltips of the choices.
         (detail-p (> (- (+ x width) detail-x 14) 220)))
    (paragraph-layout entry (- width 44))
    (stage-region stage ':approval-card :x x :y y :width width :height box-h :cursor ':default)
    (with-clip ((+ x 14) (+ y bar 12) (- width 28) (max 0 (- rows-y y bar 20)))
      (paragraph-paint entry (+ x 22) (+ y bar 12)))
    (approval--paint-choices workbench stage options
                             :x        x
                             :y        rows-y
                             :width    width
                             :label-w  label-w
                             :detail-x (and detail-p detail-x)))
  nil)

(-> approval--paint-choices
    (workbench stage list
               &key (:x integer) (:y integer) (:width integer) (:label-w integer) (:detail-x (or null integer)))
    null)
(defun approval--paint-choices (workbench stage options &key x y width label-w detail-x)
  "Draw the choices OPTIONS of the approval card, one in each row from Y down. Each row contains the number, the label in a column of the width LABEL-W, and the description at DETAIL-X. If DETAIL-X is NIL, the descriptions are left out."
  (let ((bold (text-style ':label-bold))
        (meta (text-style ':meta)))
    (loop for option in options
          for index from 0
          for top from y by *approval-row-height*
          do (let* ((choice (getf option :value))
                    (region (list ':approval choice))
                    (deny-p (equal choice "deny"))
                    (hot-p  (stage-hot-p stage region)))
               (flet ((content ()
                        (paint-text-in (format nil "~D" (1+ index)) bold
                                       :x      (+ x 14)
                                       :y      top
                                       :width  22
                                       :height *approval-row-height*
                                       :align  ':center)
                        (paint-text-in (approval--label option) bold
                                       :x      (+ x 14 30)
                                       :y      top
                                       :width  label-w
                                       :height *approval-row-height*
                                       :color  (and deny-p *red*))
                        (when detail-x
                          (paint-text-in (approval--sentence (or (getf option :description) "")) meta
                                         :x      detail-x
                                         :y      top
                                         :width  (max 0 (- (+ x width) detail-x 14))
                                         :height *approval-row-height*))))
                 (if hot-p
                     (with-inversion ((+ x 3) top (- width 6) *approval-row-height*)
                       (content))
                     (content)))
               (stage-region stage region
                             :x       (+ x 3)
                             :y       top
                             :width   (- width 6)
                             :height  *approval-row-height*
                             :action  (lambda () (workbench-answer workbench choice))
                             :tooltip (getf option :description)))))
  nil)


;;;; -- Pending Messages --

(defparameter *pending-shown* 5
  "The number of pending messages listed above the composer. Only the newest messages are listed.")

(-> pending-height (workbench) integer)
(defun pending-height (workbench)
  "Return the height of the lines of pending messages."
  (* 24 (min *pending-shown* (length (workbench-waiting workbench)))))

(-> pending-paint (workbench &key (:x integer) (:y integer) (:width integer)) null)
(defun pending-paint (workbench &key x y width)
  "Draw the pending messages, one in each line. Each line starts with an inverted label, which is steering or follow-up, and the message follows."
  (let ((meta (text-style ':meta))
        (bold (text-style ':meta-bold)))
    (loop for (text steer-p) in (last (workbench-waiting workbench) *pending-shown*)
          for row-y from y by 24
          do (let* ((label   (if steer-p "steering" "follow-up"))
                    (label-w (+ 12 (text-width label bold))))
               (with-inversion ((+ x 2) (+ row-y 3) label-w 18)
                 (paint-text-in label bold :x (+ x 2) :y (+ row-y 3) :width label-w :height 18 :align ':center))
               (paint-text-in (entries--one-line text) meta
                              :x      (+ x 2 label-w 10)
                              :y      (+ row-y 3)
                              :width  (max 0 (- width label-w 14))
                              :height 18))))
  nil)


;;;; -- When There Is No Conversation --

(-> chrome--paint-centered (string text-style &key (:x integer) (:y integer) (:width integer)) integer)
(defun chrome--paint-centered (text style &key (x 0) (y 0) (width 0))
  "Draw TEXT in STYLE, centered in WIDTH, with the top of the line at Y. Return the Y below the line."
  (paint-text-in (text-fit text style width) style
                 :x      x
                 :y      y
                 :width  width
                 :height (text-style-line-height style)
                 :align  ':center)
  (+ y (text-style-line-height style)))

(defvar *mascot* nil
  "The loaded picture of the mascot of Autolith. The value is :MISSING if the picture could not be read, and NIL before the first try.")

(defparameter *mascot-size* '(250 . 225)
  "The size of the mascot, as (WIDTH . HEIGHT).")

(-> chrome--paint-mascot (integer integer integer) integer)
(defun chrome--paint-mascot (x y width)
  "Draw the mascot centered in WIDTH with the top at Y, and return the Y below it. If the picture is missing, draw nothing and return Y."
  (unless *mascot*
    (setf *mascot*
          (or (lightfast:load-image
               (uiop:native-namestring (asdf:system-relative-pathname :aui "assets/mascot.png")))
              ':missing)))
  (if (eq *mascot* ':missing)
      y
      (progn
        (lightfast:draw-image *mascot* (+ x (floor (- width (first *mascot-size*)) 2)) y
                              :width (first *mascot-size*) :height (rest *mascot-size*))
        (+ y (rest *mascot-size*) 10))))

(-> empty-paint (workbench stage &key (:x integer) (:y integer) (:width integer) (:height integer)) null)
(defun empty-paint (workbench stage &key x y width height)
  "Draw the empty view in place of a conversation. It contains the progress of the opening, the reason of a failure, or an invitation to write."
  (let* ((column  (min 560 (- width 48)))
         (left    (+ x (floor (- width column) 2)))
         (roomy-p (>= height 560))
         (top     (+ y (max 24 (floor (- height (if roomy-p 500 260)) 2)))))
    ;; Draw the mascot above the text if there is enough room.
    (when (and roomy-p (member (workbench-attachment workbench) '(:opening :none :attached)))
      (setf top (chrome--paint-mascot left top column)))
    (ecase (workbench-attachment workbench)
      (:opening
       (destructuring-bind (&optional (step 0) (description "connecting to the world")) (workbench-progress workbench)
         (let ((below (chrome--paint-centered "Autolith" (text-style ':display) :x left :y top :width column)))
           (setf below (chrome--paint-centered description (text-style ':label)
                                               :x     left
                                               :y     (+ below 18)
                                               :width column))
           (let ((bar-w 240))
             (paint-frame (+ left (floor (- column bar-w) 2)) (+ below 14) :width bar-w :height 12)
             (paint-fill (+ left (floor (- column bar-w) 2)) (+ below 14)
                         :width  (floor (* bar-w (min 6 step)) 6)
                         :height 12)))))
      ((:failed :closed)
       (let ((below  (chrome--paint-centered (if (eq (workbench-attachment workbench) ':failed)
                                                 "The channel did not open"
                                                 "The channel closed") (text-style ':title)
                                             :x     left
                                             :y     top
                                             :width column))
             (reason (make-paragraph (list (make-span (or (workbench-failure workbench) "") nil))
                                     :style (text-style ':small) :align ':center)))
         (paragraph-layout reason column)
         (paragraph-paint reason left (+ below 12))
         (paint-button stage ':retry "Try again"
                       :x         (+ left (floor (- column 160) 2))
                       :y         (+ below 28 (paragraph-height reason))
                       :width     160
                       :height    36
                       :primary-p t
                       :action    (lambda ()
                                     (workbench-retry workbench)))
         (when (workbench-failure workbench)
           (paint-button stage ':copy-error "Copy error"
                         :x      (+ left (floor (- column 160) 2))
                         :y      (+ below 72 (paragraph-height reason))
                         :width 160 :height 36
                         :action (lambda () (lightfast:copy-text (workbench-failure workbench)))))))
      (:none
       (let ((below (chrome--paint-centered "Autolith" (text-style ':display) :x left :y top :width column)))
         (setf below (chrome--paint-centered "Pick a channel, or start a new one." (text-style ':body :face ':serif-italic)
                                             :x     left
                                             :y     (+ below 12)
                                             :width column))
         (paint-button stage ':empty-new (format nil "New channel in ~A" (short-directory (workbench-directory workbench)))
                       :x         (+ left (floor (- column 340) 2))
                       :y         (+ below 22)
                       :width     340
                       :height    38
                       :primary-p t
                       :mark      ':plus
                       :action    (lambda () (workbench-new-channel workbench)))
         (paint-button stage ':empty-choose "Choose another folder…"
                       :x      (+ left (floor (- column 340) 2))
                       :y      (+ below 70)
                       :width  340
                       :height 38
                       :action (lambda () (workbench-new-channel-elsewhere workbench)))))
      (:attached
       (let ((below (chrome--paint-centered "What shall we work on?" (text-style ':display)
                                            :x     left
                                            :y     top
                                            :width column)))
         (setf below (chrome--paint-centered "Write to the agent. Start a line with ( or / to run a command."
                                             (text-style ':body)
                                             :x     left
                                             :y     (+ below 14)
                                             :width column))
         ;; Draw the path upright, because slashes in italics look like bars.
         (chrome--paint-centered (format nil "~@[~A in ~]~A"
                                         (workbench-model workbench)
                                         (short-directory (workbench-directory workbench)))
                                 (text-style ':label)
                                 :x     left
                                 :y     (+ below 10)
                                 :width column)))))
  nil)
