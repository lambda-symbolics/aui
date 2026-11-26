(in-package #:aui)

;;;; -- Jobs in the transcript --

(-> job-view--fields-p (t) boolean)
(defun job-view--fields-p (value)
  "Does VALUE consist of keyword and value pairs?"
  (loop for tail = value then (nthcdr 2 tail)
        while tail
        always (and (consp tail) (keywordp (first tail)) (consp (rest tail)))))

(-> job-view--heading (string) text-block)
(defun job-view--heading (title)
  "Return a heading for a group of jobs."
  (make-text-block (list (make-span title (text-style ':label-bold))) :margin-bottom 4))

(-> job-view--row (t) (or null text-block))
(defun job-view--row (job)
  "Return the recorded state, job name and tool for JOB, or NIL for an unknown record."
  (when (job-view--fields-p job)
    (let ((state (getf job :state))
          (id    (getf job :id))
          (type  (getf job :type))
          (tool  (getf job :tool)))
      (when (and (stringp id) (< 0 (length id) 256)
                 (member state '(:queued :running :completed :failed :aborted :cancelled))
                 (or (eq type ':task) (and (eq type ':tool) (stringp tool) (< 0 (length tool) 128))))
        (make-text-block
         (list (make-span (string-downcase state)
                          (text-style ':label-bold :color (case state
                                                           ((:running :completed)
                                                            *green*)
                                                           (:failed
                                                            *red*)
                                                           (otherwise
                                                            *amber*))))
               (make-span (format nil "   ~A" id) (text-style ':list-bold))
               (make-span (if (eq type ':task) "   child agent" (format nil "   ~A" tool)) nil))
         :margin-bottom 4)))))

(-> job-view--rows (t) list)
(defun job-view--rows (jobs)
  "Render a bounded list of JOBS. Decline the whole page if any row is unfamiliar."
  (loop for tail = jobs then (rest tail)
        for n from 1
        for row = (and (consp tail) (<= n 64) (job-view--row (first tail)))
        while tail
        unless row do (return nil)
        collect row))

(-> entries--job-tool-blocks (tool-entry) list)
(defun entries--job-tool-blocks (entry)
  "Return the job state recorded at the time of ENTRY, or NIL for the ordinary result view."
  (let* ((details (tool-entry-details entry))
         (tag     (and (consp details) (first details)))
         (fields  (and tag (rest details)))
         (name    (tool-entry-name entry)))
    (when (and (eq (tool-entry-status entry) ':completed) (job-view--fields-p fields))
      (case tag
        (:tool-execution
         (let ((row (job-view--row (getf fields :job))))
           (when (and row (eq t (getf fields :handed-off-p))
                      (member (getf fields :handoff-reason) '(:requested :grace-expired))
                      (member (getf (getf fields :job) :state) '(:queued :running)))
             (list row))))
        (:task-run
         (when (and (string= name "child.spawn") (eq t (getf fields :succeeded-p)))
           (let* ((jobs (getf fields :results))
                  (rows (job-view--rows jobs)))
             (when (and rows (every (lambda (job) (and (eq (getf job :type) ':task)
                                                       (null (getf job :result)))) jobs))
               rows))))
        ((:job-get :job-wait :job-cancel)
         (let* ((job    (getf fields :job))
                (row    (and (equal name (ecase tag (:job-get
                                                     "job.get")
                                                    (:job-wait
                                                     "job.wait")
                                                    (:job-cancel
                                                     "job.cancel")))
                             (job-view--row job)))
                (result (and row (getf job :result))))
           (when (and row (job-view--fields-p result))
             (let ((report (or (portable-string (getf result :error)) (portable-string (getf result :content)))))
               (when (or (null result) report)
                 (append (when (eq tag ':job-cancel)
                           (list (job-view--heading (if (getf fields :accepted-p) "Cancellation requested" "Cancellation not accepted"))))
                         (list row)
                         (when report (list (make-code-block (entries--preview report :lines 40))))))))))
        (:job-list
         (when (string= name "job.list")
           (let* ((jobs   (getf fields :jobs ':missing))
                  (rows   (job-view--rows jobs))
                  (offset (getf fields :offset))
                  (total  (getf fields :total))
                  (count  (getf fields :count)))
             (when (and (typep offset '(integer 0)) (typep total '(integer 0))
                        (eql count (length rows)) (or rows (null jobs)))
               (cond
                 ((and rows (<= (+ offset count) total))
                  (cons (job-view--heading (if (= count 1)
                                               (format nil "Job ~D of ~D" (1+ offset) total)
                                               (format nil "Jobs ~D to ~D of ~D" (1+ offset) (+ offset count) total)))
                        rows))
                 ((and (null rows) (zerop total))
                  (list (job-view--heading "No jobs"))))))))))))
