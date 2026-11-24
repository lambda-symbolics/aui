(in-package #:aui)

;;;; -- Command Preview Tests --

(fiveam:def-suite preview
  :description "Tests for the preview of the command that the composer draft names."
  :in          aui)

(fiveam:in-suite preview)

(defparameter *test-operations*
  (list (portable-object "name"         "cwd"
                         "kind"         "command"
                         "description"  "show or change the active workspace"
                         "argumentHint" "[PATH]")
        (portable-object "name"        "model"
                         "kind"        "command"
                         "description" "pick a registered provider model and reasoning effort")
        (portable-object "name"        "auth"
                         "kind"        "command"
                         "description" "pick and authenticate a registered provider")
        (portable-object "name"        "shell.run"
                         "kind"        "tool"
                         "description" "run a command"))
  "The operation rows that the preview tests search, as the channel sends them.")

(fiveam:test a-command-form-previews-its-command
  (let ((preview (draft-command-preview "(cwd" *test-operations*)))
    (fiveam:is (typep preview 'command-preview))
    (fiveam:is (string= "cwd" (command-preview-name preview)))
    (fiveam:is (string= "[PATH]" (command-preview-hint preview)))
    (fiveam:is (string= "show or change the active workspace"
                        (command-preview-detail preview)))
    (fiveam:is (null (command-preview-note preview)))))

(fiveam:test a-closed-form-previews-its-command
  (fiveam:is (string= "cwd"
                      (command-preview-name
                       (draft-command-preview "(cwd)" *test-operations*)))))

(fiveam:test arguments-keep-the-preview
  (fiveam:is (string= "cwd"
                      (command-preview-name
                       (draft-command-preview "(cwd \"/tmp\")" *test-operations*)))))

(fiveam:test prose-has-no-preview
  (fiveam:is (null (draft-command-preview "hello there" *test-operations*)))
  (fiveam:is (null (draft-command-preview "run (cwd) please" *test-operations*)))
  (fiveam:is (null (draft-command-preview "" *test-operations*)))
  (fiveam:is (null (draft-command-preview "(" *test-operations*))))

(fiveam:test a-question-mark-draft-has-no-preview
  (fiveam:is (null (draft-command-preview "? release notes" *test-operations*))))

(fiveam:test a-tool-is-not-previewed
  (fiveam:is (null (draft-command-preview "(shell.run \"ls\")" *test-operations*))))

(fiveam:test an-unknown-name-has-no-preview
  (fiveam:is (null (draft-command-preview "(frobnicate)" *test-operations*)))
  (fiveam:is (null (draft-command-preview "(cwd" nil))))

(fiveam:test the-name-is-matched-without-regard-to-case
  (fiveam:is (string= "cwd"
                      (command-preview-name
                       (draft-command-preview "(CWD" *test-operations*)))))

(fiveam:test a-command-without-a-hint-previews-by-name
  (let ((preview (draft-command-preview "(model" *test-operations*)))
    (fiveam:is (string= "model" (command-preview-name preview)))
    (fiveam:is (string= "" (command-preview-hint preview)))
    (fiveam:is (string= "pick a registered provider model and reasoning effort"
                        (command-preview-detail preview)))))

(fiveam:test a-terminal-only-command-says-so
  (fiveam:is (string= "terminal only"
                      (command-preview-note
                         (draft-command-preview "(auth" *test-operations*)))))
