;;;; -- Interface exercise --

;;; Drive the demonstration offscreen and check the result of each input.
;;;
;;; Usage: script/exercise

(load (merge-pathnames "exercise-support.lisp" *load-truename*))

(in-package #:aui)

(-> exercise-run () integer)
(defun exercise-run ()
  "Exercise the demonstration. Returns the number of failed expectations."
  (lightfast:load-library)
  (lightfast:enable-thread-wakeups)
  (theme-install)
  (let* ((launch                   (launch-parse '("--demo" "--size" "1200x800" "--shot" "unused.png")))
         (workbench                (make-instance 'workbench
                                                  :launch launch
                                                  :world  (launch-world launch)
                                                  :queue  (make-ui-queue)))
         (*stage-failure-function* (lambda (report)
                                     (incf *exercise-failures*)
                                     (format t "~&  FAIL  ~A~%" report))))
    (workbench--reset-channel workbench)
    (window-build workbench)
    (demo-install workbench)
    (window-show workbench)
    (let ((stage      (workbench-stage workbench))
          (document   (workbench-document workbench))
          (transcript (workbench-transcript workbench)))
      (exercise--draw workbench)
      (exercise-expect "the window has a region for every control"
                       (every (lambda (identifier) (exercise--region workbench identifier))
                              '(:new-channel :delete-channel :model :effort :mode :attach :stop :channel-menu
                                :open-settings :open-files :open-commands :header-directory :document)))
      (exercise-expect "the channel list is docked in a wide window"
                       (exercise--region workbench '(:channel "B2Qweni")))
      ;; Scrolling.
      (setf (document-following-p document) nil
            (document-scroll document)      0)
      (exercise--draw workbench)
      (stage--pointer stage ':wheel "700 400 0 0 3 0")
      (exercise-expect "turning the wheel scrolls the conversation" (plusp (document-scroll document)))
      (setf (document-scroll document) 0)
      (exercise--draw workbench)
      ;; Expanding a tool call.
      (let ((entry (find-if (lambda (entry)
                              (and (typep entry 'tool-entry) (eq (tool-entry-status entry) ':failed)))
                            (transcript-entries transcript))))
        (exercise-expect "clicking a tool call expands the call"
                         (and (exercise--click workbench (list ':entry (entry-identifier entry)))
                              (entry-expanded-p entry)))
        (exercise--draw workbench)
        (exercise-expect "an expanded tool call has blocks that can be copied"
                         (find ':copy (stage--regions stage)
                               :key (lambda (region)
                                      (let ((identifier (region-identifier region)))
                                        (and (consp identifier) (first identifier)))))))
      ;; Selecting.
      (stage--pointer stage ':push "560 120 1 0 0 0")
      (stage--pointer stage ':drag "900 600 1 0 0 0")
      (exercise--draw workbench)
      (exercise-expect "dragging selects text across several entries"
                       (let ((text (document-selected-text document)))
                         (and text (find #\Newline text))))
      (document-clear-selection document)
      ;; The command list.
      (window--stage-key workbench (make-key-event :code  (char-code #\k)
                                                   :state lightfast:+modifier-command+))
      (exercise--draw workbench)
      (exercise-expect "pressing the command key opens the command list"
                       (and (workbench-overlay workbench)
                            (exercise--region workbench '(:operation "status"))))
      ;; In a window too low for all commands, the command list can be scrolled with the wheel.
      (stage-resize stage 1200 420)
      (exercise--draw workbench)
      (stage--pointer stage ':wheel "600 250 0 0 2 0")
      (exercise--draw workbench)
      (exercise-expect "turning the wheel scrolls the command list"
                       (plusp (overlay-scroll (workbench-overlay workbench))))
      (stage-resize stage 1200 800)
      (exercise--draw workbench)
      (overlay-key workbench (make-key-event :code #xff1b))
      (exercise-expect "pressing Escape closes the command list" (null (workbench-overlay workbench)))
      ;; Settings.
      (workbench-show-overlay workbench ':settings)
      (exercise--draw workbench)
      (exercise-expect "the settings are listed with their values"
                       (exercise--region workbench '(:setting "reasoning-effort")))
      ;; A setting with choices is changed in a picker over the settings.
      (exercise--click workbench '(:setting "reasoning-effort"))
      (exercise--draw workbench)
      (exercise-expect "clicking a setting with choices opens a picker, and the current choice is marked"
                       (and (eq ':picker (overlay-kind (workbench-overlay workbench)))
                            (find-if #'choice-current-p (overlay-choices (workbench-overlay workbench)))
                            (exercise--region workbench '(:choice 0))))
      (exercise-expect "the picker is drawn over the settings, and the settings are drawn beneath it"
                       (and (exercise--region workbench '(:overlay-window :settings))
                            (exercise--region workbench '(:overlay-window :picker))))
      (overlay-key workbench (make-key-event :code #xff1b))
      (exercise-expect "closing the picker shows the settings again"
                       (eq ':settings (overlay-kind (workbench-overlay workbench))))
      ;; A setting with a number value is changed in a prompt.
      (exercise--draw workbench)
      (exercise--click workbench '(:setting "channel-idle-minutes"))
      (exercise--draw workbench)
      (exercise-expect "clicking a setting with a free value opens a prompt that contains the current value"
                       (and (eq ':prompt (overlay-kind (workbench-overlay workbench)))
                            (equal "30" (overlay-query (workbench-overlay workbench)))
                            (exercise--region workbench ':prompt-accept)))
      (workbench-show-overlay workbench nil)
      ;; The channel menu and the panels opened from it.
      (exercise--draw workbench)
      (exercise--click workbench ':channel-menu)
      (exercise--draw workbench)
      (exercise-expect "clicking the menu button of the channel opens the channel menu"
                       (and (eq ':menu (overlay-kind (workbench-overlay workbench)))
                            (exercise--region workbench '(:choice 0))))
      (let ((folders (position "Protected folders" (overlay-choices (workbench-overlay workbench))
                               :key #'choice-label :test #'equal)))
        (exercise-expect "the channel menu opens the protected folders panel"
                         (and folders (exercise--click workbench (list ':choice folders))
                              (equal "Protected folders" (overlay-title (workbench-overlay workbench)))))
        (alexandria:when-let ((directory (uiop:getenvp "AUI_EXERCISE_SHOTS")))
          (exercise--draw workbench)
          (save-window-picture (workbench-window workbench)
                               (namestring (merge-pathnames "protected-folders.png" (uiop:ensure-directory-pathname directory)))))
        (workbench-show-overlay workbench nil)
        (exercise--draw workbench)
        (exercise--click workbench ':channel-menu)
        (exercise--draw workbench))
      (let ((agenda (position "Agenda" (overlay-choices (workbench-overlay workbench))
                              :key #'choice-label :test #'equal)))
        (exercise--click workbench (list ':choice agenda)))
      (exercise--draw workbench)
      (exercise-expect "the agenda is opened as a board that has a tab for each state"
                       (and (eq ':board (overlay-kind (workbench-overlay workbench)))
                            (equal '("All" "Doing" "To do" "Blocked" "Notes" "Done")
                                   (mapcar #'first (overlay-tabs (workbench-overlay workbench))))
                            (exercise--region workbench '(:tab 1))))
      (exercise--click workbench '(:tab 1))
      (exercise--draw workbench)
      (exercise-expect "clicking a tab selects the items in the state of the tab"
                       (and (= 1 (overlay-selection (workbench-overlay workbench)))
                            (= 1 (second (nth 1 (overlay-tabs (workbench-overlay workbench)))))))
      (workbench-show-overlay workbench nil)
      (panel-show-memories workbench)
      (exercise--draw workbench)
      (exercise-expect "the memories are opened as a board"
                       (equal '("All" "Stale" "Unverified" "Current")
                              (mapcar #'first (overlay-tabs (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      ;; The chips under the composer.
      (exercise--draw workbench)
      (exercise--click workbench ':mode)
      (exercise--draw workbench)
      (exercise-expect "clicking the permission chip opens a menu that contains a description of each mode"
                       (and (eq ':menu (overlay-kind (workbench-overlay workbench)))
                            (every #'choice-detail (overlay-choices (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      (exercise--draw workbench)
      (exercise--click workbench ':model)
      (exercise--draw workbench)
      (exercise-expect "clicking the model chip opens a picker"
                       (eq ':picker (overlay-kind (workbench-overlay workbench))))
      (workbench-show-overlay workbench nil)
      ;; If the list of models did not arrive, the choices of the model setting are offered.
      (let ((models (workbench-models workbench)))
        (setf (workbench-models workbench) nil)
        (exercise--draw workbench)
        (exercise--click workbench ':model)
        (exercise-expect "if there is no list of models, the model picker contains the choices of the model setting"
                         (and (workbench-overlay workbench)
                              (equal '("gpt-6.1-sol" "gpt-6.1-terra")
                                     (mapcar #'choice-label (overlay-choices (workbench-overlay workbench))))))
        (workbench-show-overlay workbench nil)
        (setf (workbench-models workbench) models))
      ;; The buttons at the foot of the channel list.
      (exercise--draw workbench)
      (exercise--click workbench ':delete-channel)
      (exercise-expect "clicking the delete button at the foot of the channel list opens a confirmation"
                       (and (workbench-overlay workbench)
                            (eq ':picker (overlay-kind (workbench-overlay workbench)))
                            (search "Delete" (overlay-title (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      ;; A message from another agent has a link to the channel of that agent.
      (let ((message (find-if (lambda (entry)
                                (and (typep entry 'user-entry) (user-entry-sender entry)))
                              (transcript-entries transcript))))
        (document-scroll-to-end document)
        (exercise--draw workbench)
        (exercise-expect "a message from another agent has a link to the channel of the sender"
                         (and message
                              (exercise--region workbench (list ':entry-channel (entry-identifier message))))))
      ;; The files linked by an answer.
      (let* ((directory (uiop:ensure-directory-pathname
                         (format nil "~Aaui-exercise-~D/" (uiop:temporary-directory) (random 1000000))))
             (file      (merge-pathnames "notes on fonts.md" directory))
             (plan      (merge-pathnames "plan.md" directory)))
        (ensure-directories-exist file)
        (dolist (written (list file plan))
          (with-open-file (stream written :direction ':output :if-exists ':supersede)
            (write-line "placeholder" stream)))
        (unwind-protect
             (progn
               (setf (getf (workbench-status workbench) :cwd) (namestring directory))
               (transcript-append transcript
                                  (make-instance 'assistant-entry
                                                 :text (format nil "See [the notes](notes%20on%20fonts.md#L3), `~A:12`, the notes [again](notes%20on%20fonts.md), `make` and [the site](https://www.fltk.org)."
                                                               (uiop:native-namestring plan))))
               (exercise-expect "the files linked or named in an answer are found in the folder of the channel, once each, and the file named last is first"
                                (equal (list (truename file) (truename plan)) (files-linked workbench)))
               ;; A file changed by a tool is marked in the console.
               (transcript-append transcript
                                  (make-instance 'tool-entry
                                                 :name "resource.edit" :status ':completed
                                                 :arguments '(("uri" . "workspace:plan.md"))))
               (stage-resize stage 1500 900)
               (exercise--draw workbench)
               (exercise-expect "the console contains the files, and the file changed by a tool is marked and listed first"
                                (and (equal (list (cons (truename plan) t) (cons (truename file) nil))
                                            (digest-files (workbench-digest workbench)))
                                     (exercise--region workbench
                                                       (list ':console-file
                                                             (uiop:native-namestring (truename plan))))))
               (stage-resize stage 1200 800)
               (exercise--draw workbench)
               (exercise-expect "a link to a web page is treated as a web link"
                                (and (link-web-p "https://www.fltk.org")
                                     (null (link-file "https://www.fltk.org" (namestring directory)))))
               (exercise--draw workbench)
               (exercise--click workbench ':open-files)
               (exercise-expect "clicking the files button opens a list of the files linked by the agent"
                                (and (workbench-overlay workbench)
                                     (equal '("plan.md" "notes on fonts.md")
                                            (mapcar #'choice-label
                                                    (overlay-choices (workbench-overlay workbench))))))
               (workbench-show-overlay workbench nil))
          (setf (getf (workbench-status workbench) :cwd) "/home/you/lightfast/")
          (uiop:delete-directory-tree directory :validate (lambda (checked)
                                                            (search "aui-exercise-" (namestring checked))))))
      ;; The keys of the line editor.
      (setf (composer-text workbench) "one two three")
      (composer-caret-to-end workbench)
      (window--editor-key workbench (make-key-event :code #xff08 :state lightfast:+modifier-control+))
      (exercise-expect "pressing Ctrl-Backspace deletes a word" (string= "one two " (composer-text workbench)))
      (window--editor-key workbench (make-key-event :code (char-code #\u) :state lightfast:+modifier-control+))
      (exercise-expect "pressing Ctrl-U deletes the text up to the start of the line" (string= "" (composer-text workbench)))
      ;; The channel list can be hidden with the pointer.
      (exercise--draw workbench)
      (exercise--click workbench ':hide-sidebar)
      (exercise--draw workbench)
      (exercise-expect "the channel list can be hidden with one button and shown again with another button"
                       (and (not (workbench-sidebar-shown-p workbench))
                            (exercise--region workbench ':toggle-sidebar)))
      (exercise--click workbench ':toggle-sidebar)
      (exercise--draw workbench)
      ;; An approval.
      (setf (workbench-approval workbench) (demo-approval))
      (exercise--draw workbench)
      (exercise-expect "an approval has a button for each choice"
                       (and (exercise--region workbench '(:approval "once"))
                            (exercise--region workbench '(:approval "deny"))))
      (exercise-expect "the choices of an approval are in order and inside the window"
                       (let ((first (exercise--region workbench '(:approval "auto")))
                             (last  (exercise--region workbench '(:approval "deny"))))
                         (and first last
                              (or (< (region-x first) (region-x last))
                                  (< (region-y first) (region-y last)))
                              (every (lambda (region)
                                       (let ((identifier (region-identifier region)))
                                         (or (not (and (consp identifier) (eq (first identifier) ':approval)))
                                             (and (>= (region-x region) 0)
                                                  (<= (+ (region-x region) (region-width region))
                                                      (stage-width stage))))))
                                     (stage--regions stage)))))
      ;; The same card in a narrow window.
      (stage-resize stage 620 800)
      (exercise--draw workbench)
      (exercise-expect "in a narrow window, the choices are inside the window"
                       (let ((first (exercise--region workbench '(:approval "auto")))
                             (last  (exercise--region workbench '(:approval "deny"))))
                         (and first last
                              (< (region-y first) (region-y last))
                              (<= (+ (region-x last) (region-width last)) (stage-width stage)))))
      (stage-resize stage 1200 800)
      (setf (workbench-approval workbench) nil)
      ;; Completion.
      (setf (composer-text workbench) "(st")
      (exercise--draw workbench)
      (exercise-expect "commands are offered after an opening parenthesis"
                       (equal '("(status") (mapcar #'completion-label (completion-current workbench))))
      (completion-key workbench (make-key-event :code #xff09))
      (exercise-expect "pressing Tab accepts the offered command" (string= "(status)" (composer-text workbench)))
      (setf (composer-text workbench) "/st")
      (exercise--draw workbench)
      (exercise-expect "nothing is offered after a slash" (null (completion-current workbench)))
      ;; Sending. The demonstration has no channel, so a notice is shown.
      (setf (composer-text workbench) "hello")
      (window--editor-key workbench (make-key-event :code #xff0d))
      (exercise-expect "pressing Enter in the composer sends the text"
                       (and (workbench-notice workbench)
                            (search "not attached" (workbench-notice workbench))))
      (setf (workbench-notice workbench) nil
            (composer-text workbench)    "")
      ;; In a wide window, the console is beside the conversation.
      (stage-resize stage 1500 900)
      (exercise--draw workbench)
      (exercise-expect "in a wide window, the console is beside the conversation"
                       (let ((document (exercise--region workbench ':document))
                             (menu     (exercise--region workbench ':channel-menu))
                             (job      (exercise--region workbench '(:job "job-1"))))
                         (and document menu job
                              (>= (region-x menu) (+ (region-x document) (region-width document)))
                              (>= (region-x job) (+ (region-x document) (region-width document))))))
      (exercise-expect "the composer is under the conversation, and the status line is under the composer"
                       (let ((document (exercise--region workbench ':document))
                             (stop     (exercise--region workbench ':stop))
                             (meter    (exercise--region workbench ':context-meter)))
                         (and document stop meter
                              (< (region-x stop) (+ (region-x document) (region-width document)))
                              (>= (region-y stop) (+ (region-y document) (region-height document)))
                              (< (region-x meter) (+ (region-x document) (region-width document)))
                              (> (region-y meter) (region-y stop)))))
      ;; The edge between the conversation and the console can be dragged.
      (let* ((edge   (exercise--region workbench ':console-edge))
             (before (and edge (region-x edge))))
        (when edge
          (let ((from (format nil "~D 400 1 0 0 0" (+ (region-x edge) 4)))
                (to   (format nil "~D 400 1 0 0 0" (- (region-x edge) 120))))
            (stage--pointer stage ':push from)
            (stage--pointer stage ':drag to)
            (stage--pointer stage ':release to)))
        (exercise--draw workbench)
        (exercise-expect "dragging the edge of the console makes the console wider"
                         (let ((after (exercise--region workbench ':console-edge)))
                           (and before after (< (region-x after) (- before 80))))))
      ;; Under the activity list, the console contains what was read from the conversation.
      (let ((first (find-if (lambda (entry)
                              (typep entry 'user-entry))
                            (transcript-entries transcript))))
        (exercise-expect "the console has a section for the plan and a section for the messages"
                         (and (exercise--region workbench '(:console-section :plan))
                              (exercise--region workbench '(:console-section :messages))
                              (exercise--region workbench (list ':console-message (entry-identifier first)))))
        (document-scroll-to-end document)
        (exercise--draw workbench)
        (exercise--click workbench (list ':console-message (entry-identifier first)))
        (exercise--draw workbench)
        (exercise-expect "clicking a message in the console scrolls the conversation to the message"
                         (and (not (document-following-p document))
                              (< (document-scroll document) 40)))
        (exercise--click workbench '(:console-section :messages))
        (exercise--draw workbench)
        (exercise-expect "clicking the name of a section closes the section"
                         (and (console--closed-p ':messages)
                              (exercise--region workbench '(:console-section :messages))
                              (null (exercise--region workbench (list ':console-message (entry-identifier first))))))
        (exercise--click workbench '(:console-section :messages))
        (exercise--draw workbench)
        (document-scroll-to-end document)
        (exercise--draw workbench))
      ;; The goal of the channel and the buttons used to change it.
      (exercise-expect "the console contains the goal and buttons to pause, change and clear it"
                       (and (exercise--region workbench '(:console-section :goal))
                            (exercise--region workbench '(:console-goal :pause))
                            (exercise--region workbench '(:console-goal :update))
                            (exercise--region workbench '(:console-goal :clear))
                            (null (exercise--region workbench '(:console-goal :resume)))))
      (exercise--click workbench '(:console-goal :clear))
      (exercise-expect "clicking Clear on the goal opens a confirmation"
                       (and (workbench-overlay workbench)
                            (eq ':picker (overlay-kind (workbench-overlay workbench)))
                            (search "Clear" (overlay-title (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      (exercise--draw workbench)
      (exercise--click workbench '(:console-goal :update))
      (exercise-expect "changing the goal opens a prompt that contains the current goal"
                       (and (workbench-overlay workbench)
                            (eq ':prompt (overlay-kind (workbench-overlay workbench)))
                            (search "Lightfast builds" (overlay-query (workbench-overlay workbench)))))
      (workbench-show-overlay workbench nil)
      (let ((state (workbench-state workbench)))
        (setf (workbench-state workbench)
              (portable-object "goal" (portable-object "objective" "Port it." "status" "paused"
                                                       "kind" "mission")))
        (exercise--draw workbench)
        (exercise-expect "a paused mission has a button to resume it, and no buttons to pause or change it"
                         (and (exercise--region workbench '(:console-goal :resume))
                              (null (exercise--region workbench '(:console-goal :pause)))
                              (null (exercise--region workbench '(:console-goal :update)))))
        ;; If the state of the channel has no goal, the goal is taken from the newest goal record.
        (setf (workbench-state workbench)        (portable-object "title" "x")
              (workbench--goal-record workbench) (list :goal :seq 5 :objective "From a record." :status :active))
        (exercise--draw workbench)
        (exercise-expect "if the state has no goal, the goal is taken from the newest goal record"
                         (and (equal "From a record." (getf (workbench-goal workbench) :objective))
                              (exercise--region workbench '(:console-section :goal))))
        (setf (workbench--goal-record workbench) (list :goal :seq 6 :objective nil :status :cleared))
        (exercise--draw workbench)
        (exercise-expect "a cleared goal is not shown in the console"
                         (null (exercise--region workbench '(:console-section :goal))))
        (setf (workbench-state workbench)        state
              (workbench--goal-record workbench) nil)
        (exercise--draw workbench))
      ;; The child channels of the shown channel.
      (exercise-expect "the console contains the channels beneath the shown channel, and each channel is under the parent"
                       (let ((child      (exercise--region workbench '(:console-child "C4puSYz")))
                             (grandchild (exercise--region workbench '(:console-child "D7mWx2a"))))
                         (and (exercise--region workbench '(:console-section :children))
                              (equal '(("C4puSYz" . 0) ("D7mWx2a" . 1) ("E3nQr8b" . 0))
                                     (mapcar (lambda (row)
                                               (cons (getf row :id) (getf row :depth)))
                                             (workbench-children workbench)))
                              child grandchild
                              (< (region-y child) (region-y grandchild))
                              (null (exercise--region workbench '(:console-child "A55TkQx"))))))
      ;; More child channels than fit in the console.
      (let ((tree (workbench-demo-entries workbench)))
        (setf (workbench-demo-entries workbench)
              (append tree
                      (loop for index from 1 to 10
                            collect (list :kind ':channel :id (format nil "extra-~D" index)
                                          :name (format nil "extra-~D" index) :parent "B2Qweni" :depth 1
                                          :indent 2 :state ':absent :unread 0 :mentions 0 :pending 0))))
        (exercise--draw workbench)
        (exercise-expect "the channels that do not fit are counted in the last row"
                         (and (exercise--region workbench ':console-more-children)
                              (exercise--region workbench '(:console-child "extra-5"))
                              (null (exercise--region workbench '(:console-child "extra-6")))))
        (exercise--click workbench ':console-more-children)
        (exercise-expect "clicking that row opens a picker of all channels beneath the shown channel"
                         (and (workbench-overlay workbench)
                              (eq ':picker (overlay-kind (workbench-overlay workbench)))
                              (= 13 (length (overlay-choices (workbench-overlay workbench))))))
        (workbench-show-overlay workbench nil)
        (setf (workbench-demo-entries workbench) tree)
        (exercise--draw workbench))
      (exercise-expect "the running jobs are listed by kind, and each job has a cancel button"
                       (and (exercise--region workbench '(:cancel-job "job-1"))
                            (exercise--region workbench '(:job "job-2"))
                            (equal '("Agents" "Commands") (mapcar #'first (activity-sections workbench)))))
      (stage-resize stage 1200 800)
      (exercise--draw workbench)
      ;; A narrow window.
      (stage-resize stage 700 900)
      (exercise--draw workbench)
      (exercise-expect "a narrow window has the conversation and a button to show the channel list"
                       (and (exercise--region workbench ':toggle-sidebar)
                            (exercise--region workbench ':document)
                            (null (exercise--region workbench '(:channel "B2Qweni")))))
      (exercise--click workbench ':toggle-sidebar)
      (exercise--draw workbench)
      (exercise-expect "in a narrow window, the channel list is shown alone and is as wide as the window"
                       (let ((row (exercise--region workbench '(:channel "C4puSYz"))))
                         (and row
                              (= (region-width row) 700)
                              (null (exercise--region workbench ':document))
                              (null (exercise--region workbench ':send))
                              (null (exercise--region workbench ':stop)))))
      (window--stage-key workbench (make-key-event :code #xff1b))
      (exercise--draw workbench)
      (exercise-expect "pressing Escape closes the channel list and shows the conversation"
                       (and (exercise--region workbench ':document)
                            (null (exercise--region workbench '(:channel "C4puSYz")))))
      ;; The folder of an idle channel is shown under the composer, and it can be clicked.
      (setf (workbench-working-p workbench) nil)
      (exercise--draw workbench)
      (exercise-expect "the folder under the composer can be clicked"
                       (exercise--region workbench ':status-directory))
      (setf (workbench-working-p workbench) t)
      (exercise-expect "no failure was reported" (null (workbench-notice workbench)))))
  *exercise-failures*)

(exercise-run)
(exercise--finish)
