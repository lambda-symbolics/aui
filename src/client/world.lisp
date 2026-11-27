(in-package #:aui)

;;;; -- World Client --

;;; The connection of AUI to the world of channels in Autolith. It uses the
;;; client code of the Autolith terminal: the world endpoint record to find
;;; the server, WORLD-CLIENT-OPEN to open a channel, and WORLD-SIDEBAR for the
;;; tree of channels. If no server runs, AUI starts one with the autolith
;;; command. All code that uses Autolith definitions is in this directory.
;;; The interface above it only uses the vocabulary of AUI.

(define-condition client-error (error)
  ((message
    :initarg       :message
    :reader        client-error-message
    :type          string
    :documentation "A description of the failure that can be shown to a person."))
  (:report (lambda (condition stream)
             (write-string (client-error-message condition) stream)))
  (:documentation "A failure in the communication with Autolith. The interface reports these failures instead of crashing."))

(-> condition-report (condition) string)
(defun condition-report (condition)
  "Return the report of CONDITION, cut to at most 2000 characters."
  (let ((report (handler-case (princ-to-string condition)
                  (error ()
                    (format nil "~A" (type-of condition))))))
    (subseq report 0 (min 2000 (length report)))))

(defclass world ()
  ((configuration
    :initarg       :configuration
    :reader        world-configuration
    :documentation "The Autolith configuration used to find the world server and the data of the world.")
   (permission-mode
    :initarg       :permission-mode
    :initform      ':ask
    :reader        world-permission-mode
    :type          keyword
    :documentation "The permission mode requested for channels opened by this client.")
   (immutable-p
    :initarg       :immutable-p
    :initform      nil
    :reader        world-immutable-p
    :type          boolean
    :documentation "True if an immutable server was requested for this launch.")
   (launch
    :initarg       :launch
    :initform      nil
    :accessor      world-launch
    :type          list
    :documentation "The launch options given on the command line. They only apply to the first channel that is opened.")
   (sidebar
    :initform      nil
    :accessor      world--sidebar
    :documentation "The Autolith sidebar model of the channels in the world, or NIL before it is started.")
   (entries
    :initform      nil
    :accessor      world--entries
    :type          list
    :documentation "The entries of the sidebar model and the rows made from them, as (ENTRIES . ROWS), or NIL.")
   (rows
    :initform      nil
    :accessor      world--rows
    :type          list
    :documentation "The last inputs gathered by the sidebar model and the channel rows made from them, as (GATHERED . ROWS), or NIL."))
  (:documentation "The state of this client for one Autolith world."))

(-> world-working-directory (world) string)
(defun world-working-directory (world)
  "Return the working directory of WORLD."
  (namestring (autolith::config :working-directory (world-configuration world))))

(-> world-autolith-version () string)
(defun world-autolith-version ()
  "Return the version of the Autolith used to build this client."
  autolith::*autolith-version*)

(-> world-user-init-file (world) string)
(defun world-user-init-file (world)
  "Return the trusted user initialization file for WORLD without reading it."
  (namestring (autolith::configuration-user-init-path (world-configuration world))))


;;;; -- Channel Tree --

(-> world-start-watching (world) null)
(defun world-start-watching (world)
  "Start to gather the channels of WORLD on a thread created by Autolith.
Do nothing if the gathering already runs."
  (unless (world--sidebar world)
    (let ((sidebar (autolith::world-sidebar-create (world-configuration world))))
      (autolith::world-sidebar-start sidebar)
      (setf (world--sidebar world) sidebar)))
  nil)

(-> world-stop-watching (world) null)
(defun world-stop-watching (world)
  "Stop gathering the channels of WORLD and wait for the gathering thread to finish."
  (let ((sidebar (world--sidebar world)))
    (when sidebar
      (autolith::world-sidebar-stop sidebar)
      (setf (world--sidebar world) nil)))
  nil)

(-> world-refresh (world) boolean)
(defun world-refresh (world)
  "Take the channels gathered since the last call. Return T if the channel tree changed.
This function does not block."
  (let ((sidebar (world--sidebar world)))
    (and sidebar
         (autolith::screen-panel-refresh sidebar)
         t)))

(defgeneric world--entry-row (entry)
  (:documentation "Return the entry ENTRY of the Autolith sidebar model as a property list.

The :KIND of the list is :INBOX, :WORKSPACE or :CHANNEL."))

(-> world--fold-row (t) list)
(defun world--fold-row (node)
  "Return a property list that describes the folding of the tree node NODE and the channels hidden beneath it."
  (let ((open-p (autolith::sidebar-fold-open-p node)))
    (list :children-p      (autolith::sidebar-fold-children-p node)
          :open-p          open-p
          :hidden          (and (autolith::sidebar-fold-children-p node)
                                (not open-p)
                                (autolith::sidebar-fold-nested node))
          :hidden-unread   (autolith::sidebar-fold-hidden-unread node)
          :hidden-mentions (autolith::sidebar-fold-hidden-mentions node))))

(defmethod world--entry-row ((entry autolith::sidebar-home))
  "Return the home entry ENTRY as an inbox row."
  (list :kind      ':inbox
        :id        (autolith::sidebar-entry-identifier entry)
        :current-p (autolith::sidebar-entry-current-p entry)
        :unread    (autolith::sidebar-entry-unread entry)))

(defmethod world--entry-row ((entry autolith::sidebar-workspace))
  "Return the workspace entry ENTRY as a workspace row."
  (list* :kind   ':workspace
         :key    (autolith::sidebar-workspace-key entry)
         :name   (autolith::sidebar-entry-name entry)
         :path   (autolith::sidebar-workspace-path entry)
         :indent 2
         (world--fold-row entry)))

(defmethod world--entry-row ((entry autolith::sidebar-channel))
  "Return the channel entry ENTRY as a channel row.
The row contains the name and the state of the agent and the unread counts."
  (list* :kind               ':channel
         :id                 (autolith::sidebar-entry-identifier entry)
         :parent             (autolith::sidebar-channel-parent entry)
         :name               (autolith::sidebar-entry-name entry)
         :title              (autolith::sidebar-channel-title entry)
         :path               (autolith::sidebar-channel-path entry)
         :depth              (autolith::sidebar-channel-depth entry)
         :indent             2
         :state              (autolith::sidebar-channel-state entry)
         :waiting-for-jobs-p (autolith::sidebar-channel-waiting-for-jobs-p entry)
         :attached-p         (autolith::sidebar-channel-attached-p entry)
         :current-p          (autolith::sidebar-entry-current-p entry)
         :unread             (autolith::sidebar-entry-unread entry)
         :mentions           (autolith::sidebar-channel-mentions entry)
         :pending            (autolith::sidebar-channel-pending entry)
         (world--fold-row entry)))

(-> world-entries (world) list)
(defun world-entries (world)
  "Return the channel tree of WORLD as a list of rows.

Rows are property lists. The :KIND of a row is :INBOX, :WORKSPACE or :CHANNEL.
The rows are made once for each tree built by the sidebar model."
  (let* ((sidebar (world--sidebar world))
         (entries (and sidebar (autolith::world-sidebar-entries sidebar)))
         (known   (world--entries world)))
    (if (and known (eq (first known) entries))
        (rest known)
        (let ((rows (mapcar #'world--entry-row entries)))
          (setf (world--entries world) (cons entries rows))
          rows))))

(-> world-channel-title (world (or null string)) (or null string))
(defun world-channel-title (world identifier)
  "Return the title of the channel IDENTIFIER in the channel tree of WORLD.
Return NIL if the channel has no title there."
  (and identifier
       (let ((entry (find identifier (world-entries world)
                          :key  (lambda (entry)
                                  (getf entry :id))
                          :test #'equal)))
         (and entry (portable-string (getf entry :title))))))

(-> world-channel-rows (world) list)
(defun world-channel-rows (world)
  "Return the rows of all channels of WORLD other than home, in the order of the tree.
Channels in folded nodes are included.

The rows have the form returned by `world-entries'. The :PARENT of a row is
the channel from which the channel was started. The rows are made once for
each gathering of the tree."
  (let* ((sidebar  (world--sidebar world))
         (gathered (and sidebar (autolith::world-sidebar-inputs sidebar)))
         (known    (world--rows world)))
    (if (and known (eq (first known) gathered))
        (rest known)
        (let ((rows (and gathered
                         (loop for entry in (autolith::world-sidebar-tree sidebar)
                               when (typep entry 'autolith::sidebar-channel)
                                 collect (world--entry-row entry)))))
          (setf (world--rows world) (cons gathered rows))
          rows))))

(-> channel-row-working-p (list) boolean)
(defun channel-row-working-p (row)
  "Return T if the channel described by ROW is doing work."
  (and (member (getf row :state) '(:active :working :cancelling)) t))

(-> channel-row--rank (list) integer)
(defun channel-row--rank (row)
  "Return the sort rank of the channel of ROW among the siblings.
Channels at work have rank 0. Channels that wait for jobs have rank 1.
Absent channels have rank 3. All other channels have rank 2."
  (cond
    ((channel-row-working-p row)
     0)
    ((getf row :waiting-for-jobs-p)
     1)
    ((eq (getf row :state) ':absent)
     3)
    (t
     2)))

(-> channel-descendants ((or null string) list) list)
(defun channel-descendants (identifier rows)
  "Return the rows of ROWS that are beneath the channel IDENTIFIER in the tree.
Each channel comes before the channels started from it. Each returned row
has a :DEPTH, which is 0 for channels started from IDENTIFIER itself.

Rows are property lists that contain an :ID and a :PARENT. The children of
one channel are sorted by `channel-row--rank'. Children of the same rank are
in the order of ROWS. Each channel is listed once, so the result is finite
even if rows are parents of each other."
  (let ((seen (list identifier)))
    (labels ((beneath (parent depth)
               (let ((children (stable-sort
                                (loop for row in rows
                                      for child = (getf row :id)
                                      when (and (stringp child)
                                                (equal (getf row :parent) parent)
                                                (not (member child seen :test #'equal)))
                                        collect (progn
                                                  (push child seen)
                                                  row))
                                #'<
                                :key #'channel-row--rank)))
                 (loop for row in children
                       append (cons (list* :depth depth row) (beneath (getf row :id) (1+ depth)))))))
      (and identifier (beneath identifier 0)))))

(-> (setf world-current-channel) ((or null string) world) (or null string))
(defun (setf world-current-channel) (identifier world)
  "Mark the channel IDENTIFIER as the current channel in the channel tree of WORLD.
Return IDENTIFIER."
  (let ((sidebar (world--sidebar world)))
    (when sidebar
      (setf (autolith::world-sidebar-current sidebar) identifier)
      (let ((inputs (autolith::world-sidebar-inputs sidebar)))
        (when inputs
          (autolith::world-sidebar--take sidebar inputs)))))
  identifier)

(-> world-toggle-node (world string) null)
(defun world-toggle-node (world key)
  "Open the workspace or channel node KEY in the tree of WORLD if the node is closed.
Otherwise, close the node."
  (let* ((sidebar (world--sidebar world))
         (node    (and sidebar
                       (find key (autolith::world-sidebar-entries sidebar)
                             :key  (lambda (entry)
                                     (second (autolith::sidebar-entry-key entry)))
                             :test #'equal))))
    (when node
      (autolith::world-sidebar--toggle sidebar node)))
  nil)


;;;; -- World Requests --

(-> world--entry (world) (or null cons))
(defun world--entry (world)
  "Return the endpoint of the world server of WORLD if the server answers.
Otherwise, return NIL."
  (autolith::world-client-entry (world-configuration world)))

(-> world--last-line (string) string)
(defun world--last-line (text)
  "Return the last nonblank line of TEXT without surrounding whitespace.
Return the empty string if there is no such line."
  (or (find-if (lambda (line)
                 (plusp (length line)))
               (mapcar (lambda (line)
                         (string-trim '(#\Space #\Tab #\Return) line))
                       (uiop:split-string text :separator '(#\Newline)))
               :from-end t)
      ""))

(-> world-served-p (world) boolean)
(defun world-served-p (world)
  "Return T if the server of WORLD answers or is starting."
  (let ((configuration (world-configuration world)))
    (and (or (autolith::world-client-entry configuration)
             (autolith::world-server-running-p configuration))
         t)))

(-> world--run-autolith (world &rest string) (values (or null integer) (or null string)))
(defun world--run-autolith (world &rest arguments)
  "Run the autolith command with ARGUMENTS for WORLD.
Return the exit code and the last line written by the command, or NIL if the
command wrote nothing.

The command is the launcher of the Autolith used to build this client. If the
configuration of WORLD contains a site configuration root, the root is passed
in the environment of the command."
  (let ((site (autolith::config :site-config-root (world-configuration world))))
    (multiple-value-bind (output complaint code)
        (uiop:run-program (append (when site
                                    (list "env" (format nil "AUTOLITH_SITE_CONFIG_ROOT=~A"
                                                        (uiop:native-namestring site))))
                                  (cons (installation-program) arguments))
                          :input               nil
                          :output              ':string
                          :error-output        ':string
                          :ignore-error-status t)
      (let ((said (world--last-line (if (plusp (length (world--last-line complaint)))
                                        complaint
                                        output))))
        (values code (and (plusp (length said)) said))))))

(-> world-start-server (world) null)
(defun world-start-server (world)
  "Start the server of WORLD with the autolith command if no server runs.
Return when the command has finished.

The image and the helpers needed by the server are prepared by the command.
Signal a CLIENT-ERROR if the command fails and no server runs. The error
contains the last line written by the command."
  (unless (world-served-p world)
    (multiple-value-bind (code said) (world--run-autolith world "start")
      (unless (or (eql code 0) (world-served-p world))
        (error 'client-error
                 :message (format nil "Could not start the world server with ~A (exit ~A)~:[: no diagnostic output~;: ~:*~A~]"
                                  (installation-program) code said)))))
  nil)

(-> world-stop-server (world) null)
(defun world-stop-server (world)
  "Stop the server of WORLD with the autolith command. This also stops the work
in all channels. Return when the command has finished.

The channels and their histories are kept. Signal a CLIENT-ERROR if the
command fails and the server still answers. The error contains the last line
written by the command."
  (when (world--entry world)
    (multiple-value-bind (code said) (world--run-autolith world "stop")
      (unless (or (eql code 0) (null (world--entry world)))
        (error 'client-error
               :message (format nil "The world server did not stop~:[.~;: ~:*~A~]" said)))))
  nil)

(-> world-request (world keyword &rest t) list)
(defun world-request (world operation &rest arguments)
  "Send OPERATION with ARGUMENTS to the server of WORLD. Return the fields of the answer.

Signal a CLIENT-ERROR if no server runs or if the request fails."
  (let ((entry (world--entry world)))
    (unless entry
      (error 'client-error :message "No world server is running."))
    (let ((query (if (fboundp 'autolith::world-terminal-query-record)
                     'autolith::world-terminal-query-record
                     'autolith::localgroup-query-record)))
      (handler-case (rest (funcall query entry operation arguments))
        (autolith::autolith-error (condition)
          (error 'client-error :message (condition-report condition)))))))

(-> world-tell (world string string) list)
(defun world-tell (world channel text)
  "Send TEXT to CHANNEL as a message from the person, in the same way as (tell)."
  (world-request world ':tell :channel channel :text text))

(-> world-close-channel (world string) list)
(defun world-close-channel (world channel)
  "Ask the server to close CHANNEL. The history of the channel is kept."
  (world-request world ':close :channel channel))

(-> world-participants (world) list)
(defun world-participants (world)
  "Return the participants of WORLD, read from the durable world.
Each participant is a property list of :NAME, :KIND, :IDENTIFIER and :RESIDENCE.
Return NIL if the world cannot be read."
  (handler-case
      (let ((durable (autolith::world-load (world-configuration world))))
        (loop for participant in (autolith::world-participants durable)
              collect (list :name       (autolith::participant-name participant)
                            :kind       (autolith::participant-kind participant)
                            :identifier (autolith::participant-identifier participant)
                            :residence  (autolith::participant-residence participant))))
    (error ()
      nil)))


;;;; -- Deleting a Channel --

(-> world-delete-channel (world string) integer)
(defun world-delete-channel (world channel)
  "Delete CHANNEL, all channels nested in it and their histories.
Return the number of deleted channels.

Channels that are shown somewhere or that still have work running cannot be
deleted. Signal a CLIENT-ERROR with the reason if the deletion fails."
  (handler-case (autolith::channel-remove (world-configuration world) (list channel) :tool-name "the window")
    (autolith::autolith-error (condition)
      (error 'client-error :message (autolith::autolith-error-message condition)))
    (error (condition)
      (error 'client-error
             :message (format nil "The channel could not be deleted: ~A" (condition-report condition))))))


;;;; -- Signing In --

;;; AUI signs in to providers with the code of the Autolith auth command, in
;;; this process. For a key, the key is saved to the key store of Autolith.
;;; For a subscription, the browser is opened, and AUI waits for the login.

(-> world--provider (world string) t)
(defun world--provider (world name)
  "Return the Autolith provider that is registered for sign-in as NAME."
  (autolith::provider-authentication-provider (world-configuration world) name))

(-> world-sign-in-kind (world string) (or null keyword))
(defun world-sign-in-kind (world name)
  "Return the kind of sign-in for the provider NAME.
The kind is :KEY for an API key or :BROWSER for a login in the browser.
Return NIL if the provider is unknown."
  (handler-case
      (let ((manager (autolith::provider-credential-manager (world--provider world name))))
        (if (typep manager 'autolith::api-key-credential-manager)
            ':key
            ':browser))
    (error ()
      nil)))

(-> world-sign-in (world string &key (:key (or null string))) string)
(defun world-sign-in (world name &key key)
  "Sign in to the provider NAME. Return the message from Autolith about the sign-in.

If KEY is given, the key is checked and saved. Otherwise, the browser is
opened, and this function waits until the login is done. Signal a
CLIENT-ERROR with the reason from Autolith if the sign-in fails."
  (handler-case
      (let ((provider (world--provider world name))
            (output   (make-string-output-stream)))
        (autolith::configuration-ensure-directories (world-configuration world))
        (with-input-from-string (input (format nil "~A~%" (or key "")))
          (let ((*standard-input*  input)
                (*standard-output* output))
            (autolith::provider-authenticate-with-method provider nil
                                                         :stream         output
                                                         :open-browser-p (null key)))))
    (autolith::autolith-error (condition)
      (error 'client-error :message (autolith::autolith-error-message condition)))
    (error (condition)
      (error 'client-error :message (condition-report condition)))))

(-> world-providers () list)
(defun world-providers ()
  "Return the names of the providers that this client can sign in to."
  (handler-case (mapcar #'autolith::provider-registration-name (autolith::provider-registrations))
    (error ()
      nil)))
