(in-package #:aui)

(fiveam:def-suite tool-jobs
  :description "Job results in expanded tool calls."
  :in          aui)

(fiveam:in-suite tool-jobs)

(-> job-view-test--lines (string list &key (:status keyword)) list)
(defun job-view-test--lines (name details &key (status ':completed))
  "Return the visible job paragraphs for NAME with DETAILS and STATUS."
  (let ((entry (make-instance 'tool-entry :name name :details details :status status)))
    (loop for block in (entries--job-tool-blocks entry)
          collect (paragraph-text (text-block-paragraph block)))))

(fiveam:test background-shell-and-child-state
  (let* ((job     '(:id "exec:4" :type :tool :state :running :tool "shell.run" :owner "private"))
         (handoff (list ':tool-execution :handed-off-p t :handoff-reason ':grace-expired :job job))
         (child   '(:id "review-1" :type :task :state :queued)))
    (fiveam:is (equal '("Sent to background" "running   exec:4   shell.run")
                      (job-view-test--lines "shell.run" handoff)))
    (fiveam:is (null (job-view-test--lines "shell.run" handoff :status ':failed)))
    (fiveam:is (equal '("Child agents" "queued   review-1   child agent")
                      (job-view-test--lines "child.spawn" (list ':task-run :succeeded-p t :results (list child)))))))

(fiveam:test job-pages-use-the-recorded-range
  (let* ((job  '(:id "review-1" :type :task :state :failed))
         (page (list ':job-list :jobs (list job) :count 1 :offset 2 :total 6 :next-offset 3)))
    (fiveam:is (equal '("Job 3 of 6" "failed   review-1   child agent")
                      (job-view-test--lines "job.list" page)))
    (fiveam:is (equal '("No jobs") (job-view-test--lines "job.list" '(:job-list :jobs nil :count 0 :offset 0 :total 0))))
    (fiveam:is (null (job-view-test--lines "job.list" '(:job-list :count 0 :offset 0 :total 0))))
    (fiveam:is (null (job-view-test--lines "job.list" '(:job-list :jobs ((:state)) :count 1 :offset 0 :total 1))))))
