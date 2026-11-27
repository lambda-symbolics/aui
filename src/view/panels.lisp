(in-package #:aui)

;;;; -- Panels --

(-> panel--line (string &key (:style text-style) (:margin-bottom integer)) text-block)
(defun panel--line (text &key (style (text-style ':meta)) (margin-bottom 10))
  "Return a block of TEXT in STYLE. The default STYLE is the small style of the interface."
  (make-text-block (list (make-span text nil)) :style style :margin-bottom margin-bottom))

(-> panel--empty (string) list)
(defun panel--empty (text)
  "Return the blocks of an empty panel, which contain only TEXT."
  (list (panel--line text :style (text-style ':small :face ':serif-italic))))


;;;; -- Cards --

(-> panel--card (list) box-block)
(defun panel--card (blocks)
  "Return a card, which is a framed box that contains BLOCKS."
  (make-instance 'box-block
                 :children      blocks
                 :padding-x     16
                 :padding-y     12
                 :margin-bottom 14))

(-> panel--card-head (list) text-block)
(defun panel--card-head (parts)
  "Return the head line of a card, made of PARTS. Each part is a string or a list (TEXT STYLE). The parts are separated by middle dots."
  (make-text-block (loop for (part . more) on (remove nil parts)
                         collect (if (consp part)
                                     (make-span (first part) (second part))
                                     (make-span part nil))
                         when more
                           collect (make-span "  ·  " nil))
                   :style         (text-style ':meta)
                   :margin-bottom 6))

(-> panel--tabs (list list function) list)
(defun panel--tabs (names things card)
  "Return the tabs of a board for THINGS. The first tab contains all THINGS.

NAMES is a list of (KEY . LABEL). Each entry adds a tab named LABEL, which
contains the things whose :STATUS is KEY. The function CARD returns the card
of a thing. Empty tabs contain a short note."
  (flet ((tab (label members)
           (list* label
                  (length members)
                  (or (mapcar card members)
                      (panel--empty "Nothing here.")))))
    (cons (tab "All" things)
          (loop for (key . label) in names
                collect (tab label (remove-if-not (lambda (thing)
                                                    (eq (getf thing :status) key))
                                                  things))))))


;;;; -- Agenda --

(-> panel--agenda-card (list) box-block)
(defun panel--agenda-card (item)
  "Return the card of the agenda ITEM. The head line contains the status, the identifier and the number of memories of the item. The text of the item follows as Markdown."
  (let ((status (getf item :status)))
    (panel--card
     (append
      (list (panel--card-head
             (list (list (ecase status
                           (:doing
                            "Doing")
                           (:todo
                            "To do")
                           (:blocked
                            "Blocked")
                           (:note
                            "Note")
                           (:done
                            "Done"))
                         (text-style ':meta-bold
                                     :color (ecase status
                                              (:doing
                                               *blue*)
                                              (:blocked
                                               *red*)
                                              (:done
                                               *green*)
                                              (:note
                                               *plum*)
                                              (:todo
                                               nil))))
                   (getf item :id)
                   (and (getf item :memories)
                        (format nil "~D memor~:@P" (length (getf item :memories)))))))
      (let ((blocks (prose-blocks (or (getf item :text) "") :style (text-style ':list) :breaks-p t)))
        (when blocks
          (setf (block-margin-top (first blocks))           0
                (block-margin-bottom (first (last blocks))) 0))
        blocks)))))

(-> panel--shown-p (workbench string) boolean)
(defun panel--shown-p (workbench title)
  "Return T if the overlay shown is the panel titled TITLE."
  (let ((overlay (workbench-overlay workbench)))
    (and overlay
         (eq (overlay-kind overlay) ':panel)
         (equal (overlay-title overlay) title))))

(-> panel--show-read (workbench string &key (:read function) (:show function)) null)
(defun panel--show-read (workbench title &key read show)
  "Open an empty panel titled TITLE. Run READ on another thread, and call SHOW with the value of READ."
  (workbench-show-panel workbench title nil)
  (workbench-run-aside workbench (list ':read title show) read)
  nil)

(-> panel-take-read (workbench list &key (:value t) (:report (or null string))) null)
(defun panel-take-read (workbench request &key value report)
  "Show VALUE in the panel opened by REQUEST, if that panel is still shown. REPORT is the reason why nothing was read, or NIL."
  (destructuring-bind (title show) (rest request)
    (when (panel--shown-p workbench title)
      (if report
          (workbench-show-panel workbench title (panel--empty report))
          (funcall show value))))
  nil)

(-> panel-show-agenda (workbench) null)
(defun panel-show-agenda (workbench)
  "Show the agenda of the workspace of the channel as a board. The items are shown as cards, on one tab for each status."
  (let* ((directory (workbench-directory workbench))
         (title     (format nil "Agenda of ~A" (short-directory directory))))
    (flet ((show (items)
             (workbench-show-board
              workbench title
              (panel--tabs '((:doing . "Doing") (:todo . "To do") (:blocked . "Blocked")
                             (:note . "Notes") (:done . "Done"))
                           (stable-sort (copy-list items) #'<
                                        :key (lambda (item)
                                               (or (position (getf item :status)
                                                             '(:doing :todo :blocked :note :done))
                                                   1)))
                           #'panel--agenda-card))))
      (if (workbench-demo-p workbench)
          (show (demo-agenda))
          (panel--show-read workbench title
                            :read (lambda () (workspace-agenda directory))
                            :show #'show))))
  nil)


;;;; -- Memories --

(-> panel--memory-card (list) box-block)
(defun panel--memory-card (memory)
  "Return the card of MEMORY. The head line contains the status, scope, date and identifier of the memory. The title, the summary of a stale or unverified memory, the content as Markdown and the provenance follow."
  (let ((status (getf memory :status)))
    (panel--card
     (append
      (list (panel--card-head
             (list (list (ecase status
                           (:current
                            "Current")
                           (:stale
                            "Stale")
                           (:unverified
                            "Unverified"))
                         (text-style ':meta-bold
                                     :color (ecase status
                                              (:current
                                               *green*)
                                              (:stale
                                               *red*)
                                              (:unverified
                                               *amber*))))
                   (string-downcase (princ-to-string (getf memory :scope)))
                   (getf memory :date)
                   (getf memory :id)))
            (make-text-block (list (make-span (or (getf memory :title) "") nil))
                             :style         (text-style ':list-bold)
                             :margin-bottom 4))
      (unless (eq status ':current)
        (list (panel--line (entries--one-line (or (getf memory :summary) ""))
                           :style         (text-style ':meta :color (if (eq status ':stale) *red* *amber*))
                           :margin-bottom 6)))
      (let ((blocks (prose-blocks (or (getf memory :content) "") :style (text-style ':small))))
        (when blocks
          (setf (block-margin-top (first blocks))           0
                (block-margin-bottom (first (last blocks))) 0))
        blocks)
      (when (getf memory :provenance)
        (list (panel--line (format nil "from ~A" (getf memory :provenance)) :margin-bottom 0)))))))

(-> panel-show-memories (workbench) null)
(defun panel-show-memories (workbench)
  "Show the memories of the workspace of the channel as a board. The memories are shown as cards, on one tab for each status."
  (let ((directory (workbench-directory workbench)))
    (flet ((show (memories)
             (workbench-show-board workbench
                                   "Memories"
                                   (panel--tabs '((:stale . "Stale") (:unverified . "Unverified")
                                                  (:current . "Current"))
                                                memories
                                                #'panel--memory-card))))
      (if (workbench-demo-p workbench)
          (show (demo-memories))
          (panel--show-read workbench "Memories"
                            :read (lambda () (workspace-memories directory))
                            :show #'show))))
  nil)


;;;; -- Output --

(-> panel-show-protected-folders (workbench) null)
(defun panel-show-protected-folders (workbench)
  "Explain how to configure Autolith's best-effort folder denylist."
  (workbench-show-panel
   workbench "Protected folders"
   (list (panel--line "Autolith 0.69.3 and later can refuse paths passed directly to native tools, including in Full access mode."
                     :style (text-style ':small))
         (panel--line "This is best-effort protection, not an OS sandbox. It does not confine arbitrary shell commands, Lisp forms, indexed search, or external MCP servers."
                     :style (text-style ':small))
         (panel--line "To change the list, edit your trusted init file and restart the world server. Use absolute folder paths or paths starting with ~/."
                     :style (text-style ':small))
         (make-code-block (world-user-init-file (workbench-world workbench)) :label "User init file" :margin-bottom 8)
         (let ((example "(pushnew \"~/Private/\" autolith::*tool-path-blacklist* :test #'equal)"))
           (make-code-block example :spans (code-spans example "lisp") :label "Example"))))
  nil)

(-> panel-show-output (workbench string string) null)
(defun panel-show-output (workbench title operation)
  "Show the report of the operation OPERATION of the channel in a panel titled TITLE."
  (let ((session (workbench-session workbench)))
    (cond
      ((and session (workbench-attached-p workbench))
       (workbench-show-panel workbench title nil)
       (session-operation session (cond ((equal operation "status")
                                         "_autolith/state")
                                        ((equal operation "tasks")
                                         "_autolith/jobs")
                                        (t
                                         "_autolith/invoke"))
                          :params  (portable-object "operation" operation)
                          :request (list ':panel title operation)))
      (t
       (workbench-show-panel workbench title (panel--empty "Not attached to a channel yet.")))))
  nil)

(-> panel-output-text (workbench string list) (or null string))
(defun panel-output-text (workbench operation value)
  "Format a channel snapshot, a list of jobs, or a command's report for its panel."
  (cond ((equal operation "status")
         (format nil "Channel: ~A~%Folder: ~A~%Model: ~A~%State: ~A"
                 (workbench-channel workbench) (workbench-directory workbench)
                 (or (workbench-model workbench) "unknown")
                 (if (workbench-working-p workbench) "working" "idle")))
        ((equal operation "tasks")
         (let ((jobs (portable-items (portable-get value "items"))))
           (if jobs
               (format nil "~{~A~^~%~}" (loop for job in jobs collect
                                          (format nil "~A  ~A  ~A" (portable-get job "id")
                                                  (portable-get job "state") (or (portable-get job "summary") ""))))
               "No jobs.")))
        (t
         (or (portable-string (portable-get value "text")) (portable-string (portable-get value "result"))))))

(-> panel-take-output (workbench string string) null)
(defun panel-take-output (workbench title text)
  "Put TEXT into the panel titled TITLE, if that panel is still shown."
  (let ((overlay (workbench-overlay workbench)))
    (when (panel--shown-p workbench title)
      (setf (overlay-content overlay)
            (make-instance 'box-block
                           :children  (list (make-text-block
                                             (list (make-span (string-right-trim '(#\Newline #\Space) text) nil))
                                             :style (text-style ':code)))
                           :frame-p   nil
                           :padding-x 0
                           :padding-y 0))
      (workbench-redraw workbench)))
  nil)


;;;; -- Plan --

(-> panel-show-plan (workbench) null)
(defun panel-show-plan (workbench)
  "Show the plan of the agent. Use the latest plan update in the conversation, or ask the channel for the plan if there is no update."
  (let ((update (find-if (lambda (entry)
                           (and (typep entry 'tool-entry) (string= (tool-entry-name entry) "plan.update")))
                         (transcript-entries (workbench-transcript workbench))
                         :from-end t)))
    (if update
        (workbench-show-panel workbench "Plan" (entries--plan-steps update))
        (panel-show-output workbench "Plan" "plan.list")))
  nil)
