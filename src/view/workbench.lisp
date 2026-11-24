(in-package #:aui)

;;;; -- Workbench --

(defclass workbench ()
  ((launch
    :initarg       :launch
    :reader        workbench-launch
    :type          launch
    :documentation "The options given on the command line.")
   (world
    :initarg       :world
    :reader        workbench-world
    :type          world
    :documentation "The world of channels shown in the window.")
   (queue
    :initarg       :queue
    :reader        workbench-queue
    :type          ui-queue
    :documentation "The queue to which the client threads post events.")
   (window
    :initform      nil
    :accessor      workbench-window
    :documentation "The window.")
   (stage
    :initform      nil
    :accessor      workbench-stage
    :documentation "The drawing surface of the window.")
   (generation
    :initform      0
    :accessor      workbench-generation
    :type          integer
    :documentation "The number of channel attachments so far. Events of an earlier attachment are dropped.")
   (session
    :initform      nil
    :accessor      workbench-session
    :documentation "The attachment to the channel shown, or NIL.")
   (open-request
    :initform      nil
    :accessor      workbench--open-request
    :type          list
    :documentation "The channel selection to retry if opening fails.")
   (transcript
    :initform      (make-transcript)
    :accessor      workbench-transcript
    :type          transcript
    :documentation "The history of the channel shown.")
   (projector
    :initform      nil
    :accessor      workbench-projector
    :documentation "The projector used to convert the records and events of the channel shown into the transcript.")
   (document
    :initform      nil
    :accessor      workbench-document
    :documentation "The scrolling view of the transcript.")
   (attachment
    :initform      ':none
    :accessor      workbench-attachment
    :type          (member :none :opening :attached :failed :closed)
    :documentation "The state of the attachment to the channel shown.")
   (progress
    :initform      nil
    :accessor      workbench-progress
    :type          list
    :documentation "The progress of the opening, as a list (STEP DESCRIPTION), or NIL.")
   (failure
    :initform      nil
    :accessor      workbench-failure
    :type          (or null string)
    :documentation "The reason why the last opening failed or the channel closed, or NIL.")
   (status
    :initform      nil
    :accessor      workbench-status
    :type          list
    :documentation "The last status reported by the channel, as a property list with :STATE, :MODEL, :CWD and other keys.")
   (state
    :initform      nil
    :accessor      workbench-state
    :documentation "The detailed state of the channel in portable form, such as the usage, the title and the plan.")
   (working-p
    :initform      nil
    :accessor      workbench-working-p
    :type          boolean
    :documentation "True while the channel does work that the person can stop.")
   (working-since
    :initform      nil
    :accessor      workbench-working-since
    :type          (or null integer)
    :documentation "The universal time at which the current work began.")
   (approval
    :initform      nil
    :accessor      workbench-approval
    :type          list
    :documentation "The approval that waits for an answer, as a property list of :ID, :KIND, :TITLE, :ENTRY and :OPTIONS, or NIL.")
   (pending
    :initform      nil
    :accessor      workbench-pending
    :type          list
    :documentation "The messages sent and not yet taken up by the agent, the oldest first. Each message is a list (IDENTIFIER TEXT STEER-P).")
   (operations
    :initform      nil
    :accessor      workbench-operations
    :type          list
    :documentation "The commands and tools of the channel as portable rows, once fetched.")
   (settings
    :initform      nil
    :accessor      workbench-settings
    :type          list
    :documentation "The settings of the channel as portable rows, once fetched.")
   (models
    :initform      nil
    :accessor      workbench-models
    :type          list
    :documentation "The available models as portable rows, once fetched.")
   (refreshes
    :initform      nil
    :accessor      workbench--refreshes
    :type          list
    :documentation "The snapshots that are being fetched, each as (METHOD . AGAIN-P). AGAIN-P is true if the snapshot was asked for again in the meantime.")
   (jobs
    :initform      nil
    :accessor      workbench-jobs
    :type          list
    :documentation "The background jobs of the channel as portable rows, as last fetched.")
   (jobs-fetched-at
    :initform      0
    :accessor      workbench-jobs-fetched-at
    :type          integer
    :documentation "The universal time at which the jobs were last asked for.")
   (notice
    :initform      nil
    :reader        workbench-notice
    :type          (or null string)
    :documentation "The text of the line above the composer about something that did not work, or NIL.")
   (sidebar-shown-p
    :initform      t
    :accessor      workbench-sidebar-shown-p
    :type          boolean
    :documentation "True if the person wants the channel list beside the conversation in a wide window.")
   (drawer-open-p
    :initform      nil
    :accessor      workbench-drawer-open-p
    :type          boolean
    :documentation "True if the channel list is open as a drawer that fills a narrow window.")
   (sidebar-scroll
    :initform      0
    :accessor      workbench-sidebar-scroll
    :type          integer
    :documentation "The scroll position of the channel list.")
   (filter
    :initform      ""
    :accessor      workbench-filter
    :type          string
    :documentation "The text typed by the person to narrow the channel list.")
   (editor
    :initform      nil
    :accessor      workbench-editor
    :documentation "The native text editor of the composer.")
   (attachments
    :initform      nil
    :accessor      workbench-attachments
    :type          list
    :documentation "The namestrings of the images attached to the next message.")
   (overlay
    :initform      nil
    :accessor      workbench-overlay
    :documentation "The view drawn over the window, such as the list of commands, or NIL.")
   (completion-index
    :initform      0
    :accessor      workbench-completion-index
    :type          integer
    :documentation "The index of the highlighted completion.")
   (completion-dismissed
    :initform      nil
    :accessor      workbench-completion-dismissed
    :type          (or null cons)
    :documentation "The draft and caret for which the person closed the list of completions, or NIL.")
   (history
    :initform      nil
    :accessor      workbench-history
    :type          list
    :documentation "The texts sent by the person from this window, the newest first. They can be recalled with the arrow keys.")
   (history-index
    :initform      nil
    :accessor      workbench-history-index
    :type          (or null integer)
    :documentation "The index in the history of the text shown in the composer during a recall, or NIL.")
   (demo-entries
    :initform      nil
    :accessor      workbench-demo-entries
    :type          list
    :documentation "The channel tree of the scripted demonstration, used in place of the tree of a world.")
   (waiting
    :initform      nil
    :accessor      workbench--waiting
    :type          list
    :documentation "The messages that the channel has not taken up yet, as read from the snapshot of the channel. Each message is a list (TEXT STEER-P).")
   (waiting-pathname
    :initform      nil
    :accessor      workbench--waiting-pathname
    :documentation "The file of the snapshot of waiting messages of the channel, or NIL if the channel has no such file yet.")
   (waiting-read-at
    :initform      0
    :accessor      workbench--waiting-read-at
    :type          integer
    :documentation "The internal time at which the snapshot of waiting messages was last read.")
   (participants
    :initform      nil
    :accessor      workbench--participants
    :type          list
    :documentation "The participants of the world as last read. Each participant is a property list of :NAME, :KIND, :IDENTIFIER and :RESIDENCE.")
   (participants-asked-at
    :initform      nil
    :accessor      workbench--participants-asked-at
    :type          (or null integer)
    :documentation "The internal time at which the participants were last asked for, or NIL before the first time.")
   (digest
    :initform      nil
    :accessor      workbench--digest
    :documentation "The digest of the conversation shown, with the plan, the files and the messages for the console, or NIL before the first reading.")
   (goal-record
    :initform      nil
    :accessor      workbench--goal-record
    :type          list
    :documentation "The newest goal record seen for the channel shown, or NIL.")
   (goal-layout
    :initform      nil
    :accessor      workbench--goal-layout
    :type          list
    :documentation "The last layout of the goal objective for the console, as (OBJECTIVE WIDTH BLOCK), or NIL.")
   (console-scroll
    :initform      0
    :accessor      workbench-console-scroll
    :type          integer
    :documentation "The scroll position of the console parts.")
   (console-share
    :initform      nil
    :accessor      workbench-console-share
    :type          (or null real)
    :documentation "The share of the console in the width beside the channel list, as dragged by the person, or NIL to use the saved preference."))
  (:documentation "The state of one AUI window."))

(-> notice-text (string) string)
(defun notice-text (text)
  "Return the first line of TEXT in the voice of the interface, with a lowercase first letter and without a closing period.

The first letter is only lowercased if the second letter is lowercase, so that
names in capitals are kept."
  (let* ((line  (string-right-trim '(#\Space #\Newline #\.)
                                   (subseq text 0 (or (position #\Newline text) (length text)))))
         (small (and (> (length line) 1)
                     (upper-case-p (char line 0))
                     (not (upper-case-p (char line 1))))))
    (if small
        (concatenate 'string (string (char-downcase (char line 0))) (subseq line 1))
        line)))

(-> (setf workbench-notice) ((or null string) workbench) (or null string))
(defun (setf workbench-notice) (text workbench)
  "Set the line above the composer to TEXT, or remove the line if TEXT is NIL."
  (setf (slot-value workbench 'notice) (and text (notice-text text))))

(-> workbench-redraw (workbench) null)
(defun workbench-redraw (workbench)
  "Ask for the window of WORKBENCH to be drawn again."
  (when (workbench-stage workbench)
    (stage-redraw (workbench-stage workbench)))
  nil)

(-> workbench-demo-p (workbench) boolean)
(defun workbench-demo-p (workbench)
  "Return T if WORKBENCH shows the scripted demonstration."
  (launch-demo-p (workbench-launch workbench)))

(-> workbench-attached-p (workbench) boolean)
(defun workbench-attached-p (workbench)
  "Return T if WORKBENCH is attached to a channel."
  (and (eq (workbench-attachment workbench) ':attached) t))

(-> workbench-channel (workbench) (or null string))
(defun workbench-channel (workbench)
  "Return the identifier of the channel shown in WORKBENCH, or NIL."
  (let ((session (workbench-session workbench)))
    (or (and session (session-identifier session))
        (getf (workbench-status workbench) :session-id))))


;;;; -- Channel State --

(-> workbench-title (workbench) string)
(defun workbench-title (workbench)
  "Return the title of the channel shown, or a placeholder if the channel has no title yet."
  (or (portable-string (getf (workbench-status workbench) :conversation-title))
      (portable-string (portable-get (workbench-state workbench) "title"))
      ;; The channel tree has the title of a first turn before the turn ends.
      (and (not (workbench-demo-p workbench))
           (world-channel-title (workbench-world workbench) (workbench-channel workbench)))
      (case (workbench-attachment workbench)
        (:none
         "Autolith")
        (:opening
         "Opening")
        (otherwise
         "New channel"))))

(-> workbench-model (workbench) (or null string))
(defun workbench-model (workbench)
  "Return the model of the channel shown, or NIL if the model is not known."
  (or (portable-string (portable-get (workbench-state workbench) "model"))
      (portable-string (getf (workbench-status workbench) :model))))

(-> workbench-effort (workbench) (or null string))
(defun workbench-effort (workbench)
  "Return the reasoning effort of the channel shown, or NIL if the effort is not known."
  (or (portable-string (portable-get (workbench-state workbench) "effort"))
      (portable-string (getf (workbench-status workbench) :reasoning-effort))))

(-> workbench-permission-mode (workbench) keyword)
(defun workbench-permission-mode (workbench)
  "Return the permission mode of the channel shown."
  (let ((mode (getf (workbench-status workbench) :permission-mode)))
    (if (keywordp mode) mode ':ask)))

(-> workbench-directory (workbench) string)
(defun workbench-directory (workbench)
  "Return the folder of the channel shown, or the launch directory if no channel is attached.

The folder in the state of the channel is used first, because the state is
fetched after each piece of work. The status sent by the channel can be older
than a change of folder."
  (or (portable-string (portable-get (workbench-state workbench) "workspace"))
      (portable-string (getf (workbench-status workbench) :cwd))
      (world-working-directory (workbench-world workbench))))

(-> workbench-context (workbench) (values (or null integer) (or null integer) boolean))
(defun workbench-context (workbench)
  "Return three values: the tokens of context used, the context limit, and T if Autolith estimated the use instead of measuring it. The first two values are NIL if they are not known."
  (let ((usage (portable-get (workbench-state workbench) "usage")))
    (values (let ((used (portable-get usage "contextUsed")))
              (and (integerp used) used))
            (let ((limit (portable-get usage "contextLimit")))
              (and (integerp limit) limit))
            (eq (portable-get usage "contextMeasured") ':false))))

(-> workbench-goal (workbench) list)
(defun workbench-goal (workbench)
  "Return the goal of the channel shown as a property list of :OBJECTIVE, :STATUS, :KIND and :CREATED-AT, or NIL if there is no goal.

The goal is taken from the state of the channel, if the state contains a goal.
Older versions of Autolith do not put the goal into the state. For them, the
goal comes from the newest goal record seen in the history and in live events."
  (let ((state (workbench-state workbench)))
    (if (portable-member-p state "goal")
        (portable-goal (portable-get state "goal"))
        (let ((record (workbench--goal-record workbench)))
          (and record (record-goal record))))))

(-> short-directory (string) string)
(defun short-directory (directory)
  "Return DIRECTORY with the home directory written as a tilde and without a trailing slash."
  (let* ((home    (namestring (user-homedir-pathname)))
         (trimmed (string-right-trim "/" directory)))
    (if (and (> (length home) 1)
             (>= (length directory) (length home))
             (string= home directory :end2 (length home)))
        (format nil "~~/~A" (string-right-trim "/" (subseq directory (length home))))
        (if (zerop (length trimmed)) "/" trimmed))))


;;;; -- Opening and Leaving Channels --

(-> workbench--reset-channel (workbench) null)
(defun workbench--reset-channel (workbench)
  "Clear the state of the channel shown and start an empty transcript."
  (images-forget)
  (let ((transcript (make-transcript)))
    (setf (workbench-transcript workbench)    transcript
          (workbench-projector workbench)     (make-projector transcript)
          (workbench-document workbench)      (make-document
                                               transcript
                                               :load-earlier (lambda () (workbench-load-earlier workbench)))
          (workbench-status workbench)        nil
          (workbench-state workbench)         nil
          (workbench-working-p workbench)     nil
          (workbench-working-since workbench) nil
          (workbench-approval workbench)      nil
          (workbench-pending workbench)       nil
          (workbench--refreshes workbench)        nil
          (workbench--waiting workbench)          nil
          (workbench--waiting-pathname workbench) nil
          (workbench--waiting-read-at workbench)  0
          (workbench--digest workbench)           nil
          (workbench--goal-record workbench)      nil
          (workbench-console-scroll workbench)    0
          (workbench-jobs workbench)          nil
          (workbench-progress workbench)      nil
          (workbench-failure workbench)       nil
          (workbench-notice workbench)        nil))
  nil)

(-> workbench-open (workbench &key (:channel (or null string)) (:directory (or null string)) (:new-p boolean))
    null)
(defun workbench-open (workbench &rest selection &key channel directory new-p)
  "Open CHANNEL, or select the latest channel in DIRECTORY. With NEW-P create a channel if CHANNEL does not exist."
  (let ((old (workbench-session workbench)))
    (when old
      (session-close old)))
  (incf (workbench-generation workbench))
  (workbench--reset-channel workbench)
  (setf (workbench-attachment workbench) ':opening
        (workbench--open-request workbench) selection
        (workbench-overlay workbench)    nil)
  (unless (workbench-demo-p workbench)
    (setf (workbench-session workbench)
          (session-open (workbench-world workbench) (workbench-queue workbench)
                        :generation (workbench-generation workbench)
                        :channel    channel
                        :directory  (or directory
                                        (and (null channel)
                                             (world-working-directory (workbench-world workbench))))
                         :new-p     new-p)))
  (workbench-redraw workbench)
  nil)

(-> workbench-retry (workbench) null)
(defun workbench-retry (workbench)
  "Reconnect a closed channel in its folder, or repeat a failed open request."
  (if (eq (workbench-attachment workbench) ':closed)
      (workbench-open workbench :channel (workbench-channel workbench)
                      :directory (workbench-directory workbench) :new-p t)
      (apply #'workbench-open workbench (workbench--open-request workbench))))

(-> workbench-run-aside (workbench t function) null)
(defun workbench-run-aside (workbench request function)
  "Call FUNCTION on a new thread, and post the value or the error report as the result of REQUEST.

The result is stamped with the generation at the time when it is posted. It is
therefore applied even if another channel is shown by then."
  (bordeaux-threads:make-thread
   (lambda ()
     (multiple-value-bind (outcome value report)
         (handler-case (values ':ok (funcall function) nil)
           (error (condition)
             (values ':condition nil (condition-report condition))))
       (ui-queue-push (workbench-queue workbench)
                      (make-instance 'result-event
                                     :generation (workbench-generation workbench)
                                     :request    request
                                     :outcome    outcome
                                     :value      value
                                     :report     report))))
   :name "AUI aside")
  nil)

(defparameter *participants-seconds* 3
  "The number of seconds for which the participants last read are used before they are read again.")

(-> workbench-participants (workbench) list)
(defun workbench-participants (workbench)
  "Return the participants of the world as last read. If the last reading is older than *PARTICIPANTS-SECONDS*, read them again on another thread."
  (let ((now   (get-internal-real-time))
        (asked (workbench--participants-asked-at workbench)))
    (when (and (not (workbench-demo-p workbench))
               (or (null asked)
                   (>= (- now asked) (* *participants-seconds* internal-time-units-per-second))))
      (setf (workbench--participants-asked-at workbench) now)
      (let ((world (workbench-world workbench)))
        (workbench-run-aside workbench ':participants (lambda () (world-participants world))))))
  (workbench--participants workbench))

(-> workbench-leave (workbench) null)
(defun workbench-leave (workbench)
  "Leave the channel shown without attaching to another channel."
  (let ((old (workbench-session workbench)))
    (when old
      (session-close old)))
  (incf (workbench-generation workbench))
  (workbench--reset-channel workbench)
  (setf (workbench-session workbench)    nil
        (workbench-attachment workbench) ':none
        (workbench-overlay workbench)    nil)
  (workbench-redraw workbench)
  nil)

(-> workbench-delete-channel (workbench string) null)
(defun workbench-delete-channel (workbench channel)
  "Delete CHANNEL, the channels nested in it and their histories. If CHANNEL is the channel shown, leave it first."
  (when (equal channel (workbench-channel workbench))
    (workbench-leave workbench))
  (let ((world (workbench-world workbench)))
    (workbench-run-aside
     workbench (list ':delete-channel channel)
     (lambda ()
       ;; A channel that was just left is released a moment later, so retry a few times.
       (loop for attempt from 1
             do (handler-case (return (world-delete-channel world channel))
                  (client-error (condition)
                    (if (< attempt 6)
                        (sleep 1/2)
                        (error condition))))))))
  nil)

(-> workbench-new-channel (workbench &key (:directory (or null string))) null)
(defun workbench-new-channel (workbench &key directory)
  "Open a new channel in DIRECTORY. The default DIRECTORY is the folder of the channel shown."
  (workbench-open workbench :directory (or directory (workbench-directory workbench)) :new-p t))

(-> workbench-new-channel-elsewhere (workbench) null)
(defun workbench-new-channel-elsewhere (workbench)
  "Let the person choose a folder and open a new channel there."
  (let ((directory (lightfast:choose-directory :title       "A folder for the new channel"
                                               :preset-path (workbench-directory workbench))))
    (when (and directory (plusp (length directory)))
      (workbench-new-channel workbench
                             :directory (namestring (uiop:ensure-directory-pathname directory)))))
  nil)

(-> workbench-change-directory (workbench) null)
(defun workbench-change-directory (workbench)
  "Let the person choose a new folder for the channel shown, and ask the channel to work there."
  (when (workbench-attached-p workbench)
    (let ((directory (lightfast:choose-directory :title       "A folder for the agent to work in"
                                                 :preset-path (workbench-directory workbench))))
      (when (and directory (plusp (length directory)))
        (workbench-submit workbench
                          (format nil "(cwd ~S)" (namestring (uiop:ensure-directory-pathname directory)))))))
  nil)

(-> workbench-goal-request (workbench keyword) null)
(defun workbench-goal-request (workbench action)
  "Apply ACTION to the goal of the channel shown. ACTION is :PAUSE, :RESUME or :CLEAR, or :UPDATE to ask the person for a new objective."
  (when (workbench-attached-p workbench)
    (if (eq action ':update)
        (workbench-ask workbench "Change the goal"
                       (lambda (objective)
                         (let ((objective (string-trim '(#\Space #\Newline #\Tab) objective)))
                           (when (plusp (length objective))
                             (workbench-submit workbench (format nil "(goal :update ~S)" objective)))))
                       :initial (or (getf (workbench-goal workbench) :objective) ""))
        (workbench-submit workbench (format nil "(goal ~(~S~))" action))))
  nil)

(-> workbench-stop-server (workbench) null)
(defun workbench-stop-server (workbench)
  "Stop the world server on another thread. This stops the work in all channels and closes the channel shown."
  (unless (workbench-demo-p workbench)
    (let ((world (workbench-world workbench)))
      (workbench-run-aside workbench ':stop-server
                           (lambda ()
                             (world-stop-server world)))))
  nil)

(-> workbench-load-earlier (workbench) null)
(defun workbench-load-earlier (workbench)
  "Fetch the page of history before the earliest entry shown."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (setf (document-more-p (workbench-document workbench)) nil)
      (session-load-earlier session)))
  nil)

(-> workbench-close (workbench) null)
(defun workbench-close (workbench)
  "Leave the channel shown and stop watching the world."
  (let ((session (workbench-session workbench)))
    (when session
      (session-close session)
      (setf (workbench-session workbench) nil)))
  (unless (workbench-demo-p workbench)
    (world-stop-watching (workbench-world workbench)))
  nil)


;;;; -- Asking the Channel --

(-> workbench-refresh (workbench &rest string) null)
(defun workbench-refresh (workbench &rest methods)
  "Fetch the snapshots of the channel named by METHODS again, such as _autolith/state.

If a snapshot is asked for while it is being fetched, it is fetched only once
more after the answer arrives, because only the last answer matters."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (dolist (method methods)
        (let ((under-way (assoc method (workbench--refreshes workbench) :test #'string=)))
          (if under-way
              (setf (rest under-way) t)
              (progn
                (push (cons method nil) (workbench--refreshes workbench))
                (session-operation session method)))))))
  nil)

(-> workbench--refreshed (workbench string) null)
(defun workbench--refreshed (workbench method)
  "Note that the snapshot METHOD arrived. Fetch it again if it was asked for in the meantime."
  (let ((under-way (assoc method (workbench--refreshes workbench) :test #'string=)))
    (setf (workbench--refreshes workbench) (remove under-way (workbench--refreshes workbench)))
    (when (rest under-way)
      (workbench-refresh workbench method)))
  nil)

(-> workbench--command-p (string) boolean)
(defun workbench--command-p (text)
  "Return T if TEXT is a Lisp form instead of a message to the agent."
  (let ((trimmed (string-left-trim '(#\Space #\Newline #\Tab) text)))
    (and (plusp (length trimmed))
         (member (char trimmed 0) '(#\( #\?))
         t)))

(defparameter *interface-commands*
  (list (cons "settings" (lambda (workbench)
                           (workbench-show-overlay workbench ':settings)))
        (cons "model" 'composer--choose-model)
        (cons "effort" 'composer--choose-effort)
        (cons "permissions" 'composer--choose-mode)
        (cons "agenda" 'panel-show-agenda)
        (cons "memories" 'panel-show-memories)
        (cons "open" 'window-focus-filter)
        (cons "new" 'workbench-new-channel)
        (cons "delete" (lambda (workbench)
                         (when (workbench-channel workbench)
                           (sidebar-confirm-delete workbench (workbench-channel workbench))))))
  "A list of the commands that open a view of the window if they are given no argument. Each entry is (NAME . FUNCTION). FUNCTION is called with the workbench and opens the view. In the terminal, these commands open a picker or a page.")

(defparameter *terminal-only-commands*
  '("onboard" "close" "exit" "detach" "rollback" "auth" "complain" "wtf")
  "The commands that need a terminal and have no counterpart in the window yet. These commands are left out of the lists.")

(-> command-text-name (string) (values (or null string) boolean))
(defun command-text-name (text)
  "Return the name of the command in TEXT, and T if TEXT gives the command arguments.

TEXT is a Lisp form such as (model). The name is NIL if TEXT is not a form."
  (let* ((trimmed (string-trim '(#\Space #\Newline #\Tab) text))
         (length  (length trimmed)))
    (if (and (> length 1) (char= (char trimmed 0) #\())
        (let* ((end  (or (position-if (lambda (character)
                                        (member character '(#\Space #\Newline #\Tab #\))))
                                      trimmed :start 1)
                         length))
               (rest (string-trim '(#\Space #\Newline #\Tab #\)) (subseq trimmed end))))
          (values (string-downcase (subseq trimmed 1 end))
                  (plusp (length rest))))
        (values nil nil))))

(-> workbench-run-interface-command (workbench string) boolean)
(defun workbench-run-interface-command (workbench name)
  "Open the view of the window for the command NAME. Return T if there is such a view."
  (let ((command (assoc name *interface-commands* :test #'string=)))
    (when command
      (funcall (rest command) workbench)
      t)))

(defparameter *history-recall-limit* 200
  "The number of sent texts kept for recall with the arrow keys.")

(-> workbench-submit (workbench string &key (:steer-p boolean) (:images list)) boolean)
(defun workbench-submit (workbench text &key steer-p images)
  "Send TEXT with IMAGES to the channel. Return T if the text was sent or handled by the window.

While the agent works, a message steers the running turn if STEER-P is true.
Otherwise, the message waits as a follow-up. The message is shown above the
composer until the agent takes it up."
  (let ((session (workbench-session workbench))
        (trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) text)))
    (cond
      ((and (zerop (length trimmed)) (null images))
       nil)
      ;; Open the view of the window for commands that open a picker in the terminal.
      ((multiple-value-bind (name arguments-p) (command-text-name trimmed)
         (and name
              (not arguments-p)
              (workbench-run-interface-command workbench name)))
       t)
      ((not (and session (workbench-attached-p workbench)))
       (setf (workbench-notice workbench) "Not attached to a channel yet.")
       (workbench-redraw workbench)
       nil)
      (t
       (let ((identifier (format nil "aui-~D-~D" (get-universal-time) (random 1000000))))
         (unless (workbench--command-p trimmed)
           (setf (workbench-pending workbench)
                 (append (workbench-pending workbench) (list (list identifier trimmed steer-p)))))
         ;; Show the output of a command under the command.
         (setf (projector-invocation (workbench-projector workbench))
               (and (workbench--command-p trimmed) trimmed))
         (setf (workbench-notice workbench)        nil
               (workbench-history-index workbench) nil)
         (unless (equal trimmed (first (workbench-history workbench)))
           (let ((history (cons trimmed (workbench-history workbench))))
             (setf (workbench-history workbench)
                   (subseq history 0 (min (length history) *history-recall-limit*)))))
         (session-submit session trimmed
                         :images     images
                         :steer-p    steer-p
                         :identifier identifier
                         :request    (list ':submit identifier))
         (document-scroll-to-end (workbench-document workbench))
         (workbench-redraw workbench)
         t)))))

(-> workbench-stop (workbench) null)
(defun workbench-stop (workbench)
  "Stop the running turn of the channel."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-cancel session)))
  nil)

(-> workbench-answer (workbench string) null)
(defun workbench-answer (workbench value)
  "Answer the waiting approval with the choice VALUE."
  (let ((session  (workbench-session workbench))
        (approval (workbench-approval workbench)))
    (when (and session approval)
      (session-answer session (getf approval :id) value)
      (setf (workbench-approval workbench) nil)
      (workbench-redraw workbench)))
  nil)

(-> workbench-set-mode (workbench keyword) null)
(defun workbench-set-mode (workbench mode)
  "Select the permission MODE of the channel."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-set-mode session mode)))
  nil)

(-> workbench-invoke (workbench string string) null)
(defun workbench-invoke (workbench name input)
  "Run the command NAME of the channel with the argument text INPUT."
  (let ((session (workbench-session workbench)))
    (if (and session (workbench-attached-p workbench))
        (session-operation session "_autolith/invoke"
                           :params  (portable-object "operation" name
                                                     "arguments" (portable-object "input" input))
                           :request (list ':invoke name input))
        (progn
          (setf (workbench-notice workbench) "Not attached to a channel yet.")
          (workbench-redraw workbench))))
  nil)

(-> workbench-set-setting (workbench string t) null)
(defun workbench-set-setting (workbench name value)
  "Set the setting NAME of the channel to VALUE."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-operation session "_autolith/set-setting"
                         :params  (portable-object "name" name "value" value)
                         :request (list ':set-setting name value))))
  nil)


;;;; -- Jobs --

(defparameter *jobs-poll-seconds* 3
  "The interval in seconds between fetches of the jobs while any job is live.")

(-> workbench-live-jobs (workbench) list)
(defun workbench-live-jobs (workbench)
  "Return the live jobs, which are pending, running, blocked or yielded, as portable rows."
  (remove-if-not (lambda (job)
                   (member (portable-get job "state") '("pending" "running" "blocked" "yielded")
                           :test #'equal))
                 (workbench-jobs workbench)))

(-> workbench-refresh-jobs (workbench) null)
(defun workbench-refresh-jobs (workbench)
  "Fetch the jobs of the channel again."
  (setf (workbench-jobs-fetched-at workbench) (get-universal-time))
  (workbench-refresh workbench "_autolith/jobs")
  nil)

(-> workbench-poll-jobs (workbench) null)
(defun workbench-poll-jobs (workbench)
  "Fetch the jobs again if some jobs are live and the last fetch is older than *JOBS-POLL-SECONDS*. Jobs are live if the last fetch contains live jobs or if the status of the channel counts live jobs."
  (when (and (workbench-attached-p workbench)
             (or (workbench-live-jobs workbench)
                 ;; Before Autolith 0.67.39, the count of live jobs was named :TASK-LIVE-COUNT.
                 (let ((count (or (getf (workbench-status workbench) :job-live-count)
                                  (getf (workbench-status workbench) :task-live-count))))
                   (and (integerp count) (plusp count))))
             (>= (- (get-universal-time) (workbench-jobs-fetched-at workbench)) *jobs-poll-seconds*))
    (workbench-refresh-jobs workbench))
  nil)

(-> workbench-cancel-job (workbench string) null)
(defun workbench-cancel-job (workbench identifier)
  "Cancel the job IDENTIFIER and the jobs it started."
  (let ((session (workbench-session workbench)))
    (when (and session (workbench-attached-p workbench))
      (session-operation session "_autolith/job-cancel"
                         :params  (portable-object "jobId" identifier)
                         :request (list ':cancel-job identifier))))
  nil)


;;;; -- Events --

(-> workbench--note-status (workbench list) null)
(defun workbench--note-status (workbench status)
  "Store STATUS as the current status of the channel, and update whether the channel works. Set the title of the window."
  ;; A change of folder makes the fetched state stale, so fetch the state again.
  (let ((known (getf (workbench-status workbench) :cwd)))
    (when (and known (not (equal known (getf status :cwd))))
      (workbench-refresh workbench "_autolith/state")))
  (setf (workbench-status workbench) status)
  (let ((working-p (and (member (getf status :state) '(:active :working :cancelling)) t)))
    (unless (eq working-p (workbench-working-p workbench))
      (setf (workbench-working-p workbench)     working-p
            (workbench-working-since workbench) (and working-p (get-universal-time)))))
  (let ((window (workbench-window workbench)))
    (when window
      (setf (lightfast:label window) (format nil "~A - Autolith" (workbench-title workbench)))))
  nil)

(defparameter *waiting-poll-seconds* 3/2
  "The interval in seconds between readings of the snapshot of waiting messages.")

(-> workbench-waiting (workbench) list)
(defun workbench-waiting (workbench)
  "Return the messages that the channel has not taken up yet, the oldest first. Each message is a list (TEXT STEER-P).

The list contains the messages in the snapshot of the channel from all
senders. The messages sent from this window that are missing from the
snapshot follow."
  (let ((known (workbench--waiting workbench)))
    (append known
            (loop for (nil text steer-p) in (workbench-pending workbench)
                  unless (find text known :key #'first :test #'equal)
                    collect (list text steer-p)))))

(-> workbench-poll-waiting (workbench &key (:now-p boolean)) null)
(defun workbench-poll-waiting (workbench &key now-p)
  "Read the snapshot of waiting messages of the channel again on another thread. Only read it if the last reading is old or if NOW-P is true."
  (let ((channel (workbench-channel workbench))
        (now     (get-internal-real-time)))
    (when (and channel
               (workbench-attached-p workbench)
               (not (workbench-demo-p workbench))
               (or now-p
                   (>= (- now (workbench--waiting-read-at workbench))
                       (* *waiting-poll-seconds* internal-time-units-per-second))))
      (setf (workbench--waiting-read-at workbench) now)
      (let ((configuration (world-configuration (workbench-world workbench)))
            (known         (workbench--waiting-pathname workbench)))
        (workbench-run-aside
         workbench (list ':waiting channel)
         (lambda ()
           ;; A channel only has a snapshot after it has a history. Look for the
           ;; file until it exists.
           (let ((pathname (or known (history-pending-pathname configuration channel))))
             (list pathname (and pathname (history-pending-inputs pathname channel)))))))))
  nil)

(-> workbench--take-waiting (workbench string list) boolean)
(defun workbench--take-waiting (workbench channel reading)
  "Store READING, which is a list of the snapshot file of CHANNEL and the messages waiting in it. Return T if the waiting messages changed.

A reading for a channel other than the channel shown is dropped."
  (when (equal channel (workbench-channel workbench))
    (destructuring-bind (pathname waiting) reading
      (setf (workbench--waiting-pathname workbench) pathname)
      (unless (equal waiting (workbench--waiting workbench))
        (setf (workbench--waiting workbench) waiting)
        t))))

(-> workbench--drop-pending (workbench t) null)
(defun workbench--drop-pending (workbench submission)
  "Remove the pending message identified by SUBMISSION, which is a property list with an :ID."
  (let ((identifier (and (listp submission) (getf submission :id))))
    (when identifier
      (setf (workbench-pending workbench)
            (remove identifier (workbench-pending workbench) :key #'first :test #'equal))))
  nil)

(-> workbench--channel-event (workbench list) null)
(defun workbench--channel-event (workbench payload)
  "Apply PAYLOAD, which is one event of the structured client protocol, to WORKBENCH."
  (let ((fields (rest payload)))
    (projector-apply (workbench-projector workbench) payload)
    (case (first payload)
      (:state
       (workbench--note-status workbench (rest (getf fields :state))))
      (:approval
       (setf (workbench-approval workbench)
             (list :id      (getf fields :id)
                   :kind    (getf fields :kind)
                   :title   (getf fields :title)
                   :entry   (getf fields :entry)
                   :options (getf fields :options))))
      (:approval-settled
       (when (equal (getf fields :id) (getf (workbench-approval workbench) :id))
         (setf (workbench-approval workbench) nil)))
      (:work-started
       (workbench--drop-pending workbench (getf fields :submission))
       (unless (workbench-working-p workbench)
         (setf (workbench-working-p workbench)     t
               (workbench-working-since workbench) (get-universal-time))))
      (:work-finished
       (workbench--drop-pending workbench (getf fields :submission))
       (when (getf fields :idle-p)
         (setf (workbench-working-p workbench)     nil
               (workbench-working-since workbench) nil
               (workbench-approval workbench)      nil))
       (workbench-refresh workbench "_autolith/state")
       (workbench-refresh-jobs workbench))
      (:record
       (let ((record (getf fields :record)))
         (when (eq (first record) ':message)
           (workbench--drop-pending workbench (getf (rest record) :submission))
           ;; A job that reports back has finished, so fetch the jobs again.
           (when (getf (rest record) :job-completion)
             (workbench-refresh-jobs workbench)))
         (when (eq (first record) ':goal)
           (setf (workbench--goal-record workbench)
                 (records-newest-goal (list record) (workbench--goal-record workbench)))
           (workbench-refresh workbench "_autolith/state"))))
      (:status
       (when (member (getf fields :status) '(:provider-request-completed :compaction-completed))
         (workbench-refresh workbench "_autolith/state"))
       (when (eq (getf fields :status) ':tool-call-completed)
         (workbench-refresh-jobs workbench)))))
  nil)

(-> workbench--attachment-event (workbench attachment-event) null)
(defun workbench--attachment-event (workbench event)
  "Update the state of the attachment to the channel shown from EVENT."
  (let ((detail (attachment-event-detail event)))
    (ecase (attachment-event-state event)
      (:opening
       (setf (workbench-attachment workbench) ':opening
             (workbench-progress workbench)   detail))
      (:attached
       (let ((projector (workbench-projector workbench)))
         (setf (workbench-attachment workbench)   ':attached
               (workbench-progress workbench)     nil
               (projector-channel projector)      (ui-event-channel event)
               (projector-boundary projector)     (or (getf detail :record-sequence) 0)
               (projector-image-root projector)   (getf detail :image-root)
               (workbench--goal-record workbench) (records-newest-goal
                                                   (list (getf detail :goal-record))
                                                   (workbench--goal-record workbench))
               ;; Show a request that is in progress at the attachment from the records.
               (projector-catch-up-p projector)   (and (getf (getf detail :status) :active-turn-p) t)))
       (workbench--note-status workbench (getf detail :status))
       (setf (world-current-channel (workbench-world workbench)) (ui-event-channel event))
       (workbench-refresh workbench "_autolith/state" "_autolith/operations" "_autolith/settings"
                          "_autolith/models")
       ;; Send the prompt from the command line once, to the first channel that attaches.
       (let ((prompt (shiftf (launch-prompt (workbench-launch workbench)) nil)))
         (when prompt
           (workbench-submit workbench prompt))))
      (:failed
       (setf (workbench-attachment workbench) ':failed
             (workbench-progress workbench)   nil
             (workbench-failure workbench)    (or detail "The channel could not be opened.")))
      (:closed
       (setf (workbench-attachment workbench) ':closed
             (workbench-working-p workbench)  nil
             (workbench-approval workbench)   nil
             (workbench-failure workbench)    (or detail "The channel closed.")))))
  nil)

(-> workbench--result-event (workbench result-event) boolean)
(defun workbench--result-event (workbench event)
  "Apply the result EVENT of a request made by WORKBENCH. Return T if the window needs to be drawn again."
  (let ((request   (result-event-request event))
        (value     (result-event-value event))
        (ok-p      (eq (result-event-outcome event) ':ok))
        (changed-p t))
    (when (stringp request)
      (workbench--refreshed workbench request))
    (cond
      ((equal request "_autolith/state")
       (when ok-p
         (setf (workbench-state workbench) value)))
      ((equal request "_autolith/operations")
       (when ok-p
         (setf (workbench-operations workbench) (portable-items (portable-get value "items")))))
      ((equal request "_autolith/settings")
       (when ok-p
         (setf (workbench-settings workbench) (portable-items (portable-get value "items")))))
      ((equal request "_autolith/models")
       (when ok-p
         (setf (workbench-models workbench) (portable-items (portable-get value "items")))))
      ((equal request "_autolith/jobs")
       (when ok-p
         (setf (workbench-jobs workbench) (portable-items (portable-get value "items")))))
      ((and (consp request) (eq (first request) ':cancel-job))
       (workbench-refresh-jobs workbench)
       (unless ok-p
         (setf (workbench-notice workbench)
               (format nil "Could not cancel the job: ~A"
                       (or (result-event-report event) "the channel refused.")))))
      ((and (consp request) (eq (first request) ':invoke))
       (if ok-p
           (workbench-refresh workbench "_autolith/state" "_autolith/settings")
           (setf (workbench-notice workbench)
                 (format nil "(~A ~A) failed: ~A" (second request) (third request)
                         (or (result-event-report event) "the channel refused it")))))
      ((and (consp request) (eq (first request) ':set-setting))
       (destructuring-bind (name &optional choice again-p) (rest request)
         (cond
           (ok-p
            (workbench-refresh workbench "_autolith/state" "_autolith/settings"))
           ;; The structured setter refuses strings for choices with keyword
           ;; values. The settings command reads the value as it is spelled, so
           ;; retry with that command.
           ((and (stringp choice) (not again-p) (workbench-session workbench)
                 (not (session-native-command "_autolith/set-setting")))
            (session-operation (workbench-session workbench) "_autolith/invoke"
                               :params  (portable-object
                                         "operation" "settings"
                                         "arguments" (portable-object "input" (format nil "~A ~A" name choice)))
                               :request (list ':set-setting name choice t)))
           (t
            (setf (workbench-notice workbench)
                  (format nil "Could not change ~A: ~A" name
                          (or (result-event-report event) "the channel refused.")))))))
      ((and (consp request) (eq (first request) ':delete-channel))
       (unless ok-p
         (setf (workbench-notice workbench)
               (or (result-event-report event) "The channel could not be deleted."))))
      ;; Repeated readings usually return the same value. Only redraw if it changed.
      ((eq request ':participants)
       (setf changed-p (and ok-p (not (equal value (workbench--participants workbench)))))
       (when changed-p
         (setf (workbench--participants workbench) value)))
      ((and (consp request) (eq (first request) ':waiting))
       (setf changed-p (and ok-p (workbench--take-waiting workbench (second request) value))))
      ((and (consp request) (eq (first request) ':read))
       (panel-take-read workbench request
                        :value  value
                        :report (and (not ok-p) (or (result-event-report event) "The content could not be read."))))
      ((eq request ':stop-server)
       (unless ok-p
         (setf (workbench-notice workbench)
               (or (result-event-report event) "The world server did not stop."))))
      ((and (consp request) (eq (first request) ':open-link))
       (unless ok-p
         (setf (workbench-notice workbench)
               (format nil "Could not open ~A with any program on this machine." (second request)))))
      ((and (consp request) (eq (first request) ':sign-in))
       (workbench-show-panel
        workbench (format nil "Sign in to ~A" (second request))
        (panel--empty (if ok-p
                          (princ-to-string value)
                          (format nil "The sign-in did not go through: ~A"
                                  (or (result-event-report event) "no reason was given.")))))
       (workbench-refresh workbench "_autolith/models" "_autolith/state"))
      ((and (consp request) (eq (first request) ':panel))
       (panel-take-output workbench (second request)
                          (or (and ok-p (panel-output-text workbench (third request) value))
                              (result-event-report event)
                              "The channel had nothing to report.")))
      ((and (consp request) (eq (first request) ':submit))
       (unless ok-p
         (setf (workbench-pending workbench)
               (remove (second request) (workbench-pending workbench) :key #'first :test #'equal)
               (workbench-notice workbench)
               (format nil "Not sent: ~A" (or (result-event-report event) "the channel refused.")))))
      ((not ok-p)
       (when (result-event-report event)
         (setf (workbench-notice workbench) (result-event-report event)))))
    changed-p))

(-> workbench-apply (workbench ui-event) null)
(defun workbench-apply (workbench event)
  "Apply EVENT, which was posted by a client thread, to WORKBENCH. Events of a channel that was left are dropped."
  (when (and (= (ui-event-generation event) (workbench-generation workbench))
             (etypecase event
               (channel-event
                (workbench--channel-event workbench (channel-event-payload event))
                t)
               (history-event
                (let ((projector (workbench-projector workbench)))
                  (transcript-prepend (workbench-transcript workbench)
                                      (records->entries projector (history-event-records event)))
                  (setf (workbench--goal-record workbench)
                        (records-newest-goal (history-event-records event) (workbench--goal-record workbench)))
                  (setf (document-more-p (workbench-document workbench)) (history-event-more-p event)))
                t)
               (attachment-event
                (workbench--attachment-event workbench event)
                t)
               (result-event
                (workbench--result-event workbench event))))
    (workbench-redraw workbench))
  nil)

(-> workbench-drain (workbench) null)
(defun workbench-drain (workbench)
  "Apply all events waiting in the queue of WORKBENCH, and update the channel tree of the world."
  ;; Guard each event by itself, so that a failed event does not stop the rest.
  (dolist (event (ui-queue-drain (workbench-queue workbench)))
    (with-stage-guard ("Applying what the channel said")
      (workbench-apply workbench event)))
  (when (and (not (workbench-demo-p workbench))
             (world-refresh (workbench-world workbench)))
    (workbench-redraw workbench))
  nil)
