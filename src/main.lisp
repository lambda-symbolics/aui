(in-package #:aui)

;;;; -- Main --

;;; On macOS, run the event loop on the first thread of the process.

(defparameter *tick-seconds* 0.12
  "The interval in seconds between two animation and polling ticks.")

(-> main--settle (workbench real) null)
(defun main--settle (workbench seconds)
  "Apply the events from the world server for SECONDS."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop while (< (get-internal-real-time) deadline)
          do (workbench-drain workbench)
             (lightfast:wait 0.05)
             (sleep 0.02)))
  nil)

(-> main--start (workbench) null)
(defun main--start (workbench)
  "Open the channel or demo specified by the launch options of WORKBENCH."
  (let ((launch (workbench-launch workbench)))
    (cond
      ((launch-demo-p launch)
       (demo-install workbench)
       (when (eq (launch-view launch) ':approval)
         (setf (workbench-approval workbench) (demo-approval)))
       (when (eq (launch-view launch) ':welcome)
         (transcript-clear (workbench-transcript workbench))
         (setf (workbench-working-p workbench) nil
               (workbench-jobs workbench)      nil)))
      (t
       (world-start-watching (workbench-world workbench))
       (if (launch-pick-p launch)
           (workbench-open-picker workbench)
           (workbench-open workbench :channel (launch-channel launch)))))
    (when (launch-draft launch)
      (setf (composer-text workbench) (launch-draft launch)))
    (case (launch-view launch)
      ((:commands :settings)
       (workbench-show-overlay workbench (launch-view launch)))
      (:agenda
       (panel-show-agenda workbench))
      (:memories
       (panel-show-memories workbench))
      (:plan
       (panel-show-plan workbench))
      (:model
       (composer--choose-model workbench))
      (:permissions
       (composer--choose-mode workbench))
      (:menu
       (chrome--channel-menu workbench
                             :x (- (stage-width (workbench-stage workbench)) 50)
                             :y 47))
      (:choice
       (workbench-show-overlay workbench ':settings)
       (let ((row (find "reasoning-effort" (workbench-settings workbench)
                        :key  (lambda (row)
                                (portable-get row "name"))
                        :test #'equal)))
         (when row
           (catalog--change-setting workbench row))))
      (:prompt
       (workbench-ask workbench "Rename the agent" (constantly nil)
                      :note    "The name of the agent in @mentions."
                      :initial "lightfast"))
      (:channels
       (setf (workbench-sidebar-shown-p workbench) t
             (workbench-drawer-open-p workbench)   t))
      (:top
       (setf (document-following-p (workbench-document workbench)) nil
             (document-scroll (workbench-document workbench))      0))))
  nil)

(-> run-interface (launch) null)
(defun run-interface (launch)
  "Show the window requested by LAUNCH and run the event loop until the window is closed."
  (lightfast:load-library)
  (lightfast:enable-thread-wakeups)
  (theme-install)
  (unless (or (launch-demo-p launch) (launch-shot launch))
    (setf *preferences-pathname* (preferences-default-pathname)))
  (let* ((queue     (make-ui-queue :wakeup #'lightfast:awake))
         (workbench (make-instance 'workbench :launch launch :world (launch-world launch) :queue queue))
         (started   (get-internal-real-time)))
    (workbench--reset-channel workbench)
    (window-build workbench)
    (main--start workbench)
    (window-show workbench)
    (lightfast:add-timeout *tick-seconds*
                           (lambda ()
                             (with-stage-guard ("Polling the channel")
                               (let ((stage (workbench-stage workbench)))
                                 (workbench-poll-jobs workbench)
                                 (workbench-poll-waiting workbench)
                                 (when (stage-animating-p stage)
                                   (incf (stage-phase stage))
                                   (stage-redraw stage)))))
                           :repeat t)
    (when (and (launch-shot launch) (launch-size launch))
      (unless (launch-demo-p launch)
        (main--settle workbench (launch-shot-after launch)))
      (window-arrange workbench)
      (save-window-picture (workbench-window workbench) (launch-shot launch))
      (workbench-close workbench)
      (return-from run-interface nil))
    (when (launch-shot launch)
      (lightfast:add-timeout (launch-shot-after launch)
                             (lambda ()
                               (window-arrange workbench)
                               (stage-flush (workbench-stage workbench))
                               (lightfast:wait 0.3)
                               (save-window-picture (workbench-window workbench) (launch-shot launch))
                               (workbench-close workbench)
                               (lightfast:quit))))
    (setf *stage-failure-function*
          (lambda (report)
            (setf (workbench-notice workbench) report)
            (workbench-redraw workbench)))
    (lightfast:run-with-idle
     (lambda ()
       (with-stage-guard ("Reading channel events")
         (workbench-drain workbench))
       (with-stage-guard ("Laying out the window")
         (when (stage-dirty-p (workbench-stage workbench))
           (window-arrange workbench))
         (stage-flush (workbench-stage workbench)))))
    (workbench-close workbench)
    (format *error-output* "~&AUI ran for ~D seconds.~%"
            (round (- (get-internal-real-time) started) internal-time-units-per-second)))
  nil)

(defparameter *nursery-megabytes* 8
  "The allocation threshold in megabytes between garbage collections. Most window allocations are short-lived.")

(-> main--keep-small () null)
(defun main--keep-small ()
  "Set the SBCL garbage collection threshold to *NURSERY-MEGABYTES*."
  (setf (sb-ext:bytes-consed-between-gcs) (* *nursery-megabytes* 1024 1024))
  nil)

(-> main (&optional list) null)
(defun main (&optional (arguments (uiop:command-line-arguments)))
  "Run aui with the command line ARGUMENTS. Exit the process after the window is closed."
  (main--keep-small)
  (let ((status 0))
    (handler-case
        (let ((launch (launch-parse arguments)))
          (when launch
            (run-interface launch)))
      ((or autolith::autolith-error client-error theme-error) (condition)
        (format *error-output* "~&aui: ~A~%" condition)
        (setf status 64)))
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code status :abort t))
  nil)


;;;; -- Saved Image --

(-> image-toplevel () null)
(defun image-toplevel ()
  "Run aui in a saved image with the command line arguments of the process."
  (main (rest sb-ext:*posix-argv*)))

(-> save-image (string) null)
(defun save-image (pathname)
  "Save AUI as an executable at PATHNAME. Pass all runtime arguments to AUI."
  (ensure-directories-exist pathname)
  (sb-ext:save-lisp-and-die pathname
                            :toplevel             #'image-toplevel
                            :executable           t
                            :save-runtime-options t)
  nil)
