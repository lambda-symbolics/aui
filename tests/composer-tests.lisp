(in-package #:aui)

;;;; -- Composer Tests --

(fiveam:def-suite composer
  :description "Tests for the braces that the composer pairs, as the line editor of Autolith does."
  :in          aui)

(fiveam:in-suite composer)

(fiveam:test an-open-brace-has-its-close
  (fiveam:is (eql (char "()" 1) (composer--closing-brace (char "()" 0))))
  (fiveam:is (eql (char "[]" 1) (composer--closing-brace (char "[]" 0))))
  (fiveam:is (eql (char "{}" 1) (composer--closing-brace (char "{}" 0))))
  (fiveam:is (null (composer--closing-brace (char "()" 1))))
    (fiveam:is (null (composer--closing-brace #\a))))

(fiveam:test an-odd-run-of-backslashes-escapes-a-brace
  (fiveam:is (composer--escaped-p "\\" 1))
  (fiveam:is (not (composer--escaped-p "(" 0)))
  (fiveam:is (not (composer--escaped-p "a(" 1)))
    (fiveam:is (not (composer--escaped-p "\\\\" 2))))

(fiveam:test an-open-brace-pairs-anywhere-in-a-draft
  (fiveam:is (eq :pair (composer--brace-step "" 0 (char "()" 0))))
  (fiveam:is (eq :pair (composer--brace-step "(model" 6 (char "()" 0))))
  (fiveam:is (eq :pair (composer--brace-step "(model" 6 (char "[]" 0))))
  (fiveam:is (eq :pair (composer--brace-step "x" 1 (char "{}" 0)))))

(fiveam:test a-brace-under-the-caret-is-stepped-over
  (fiveam:is (eq :step (composer--brace-step "(cwd)" 4 (char "()" 1))))
    (fiveam:is (eq :pair (composer--brace-step "(cwd)" 0 (char "()" 0))))
    (fiveam:is (eq :step (composer--brace-step "[]" 1 (char "[]" 1))))
    (fiveam:is (eq :step (composer--brace-step "{}" 1 (char "{}" 1)))))

(fiveam:test a-brace-with-none-under-the-caret-is-inserted
  (fiveam:is (null (composer--brace-step "(cwd" 4 (char "()" 1))))
  (fiveam:is (null (composer--brace-step "" 0 (char "()" 1))))
  (fiveam:is (null (composer--brace-step "abc" 1 #\a))))

(fiveam:test an-escaped-or-selected-brace-is-inserted
  (fiveam:is (null (composer--brace-step "\\" 1 (char "()" 0))))
  (fiveam:is (null (composer--brace-step "" 0 (char "()" 0) :selected-p t)))
  (fiveam:is (null (composer--brace-step "(cwd)" 4 (char "()" 1) :selected-p t))))

(fiveam:test an-empty-pair-around-the-caret-is-deleted-together
  (fiveam:is (composer--brace-pair-around-p "()" 1))
  (fiveam:is (composer--brace-pair-around-p "[]" 1))
  (fiveam:is (composer--brace-pair-around-p "{}" 1))
  (fiveam:is (not (composer--brace-pair-around-p "()" 0)))
  (fiveam:is (not (composer--brace-pair-around-p "()" 2)))
  (fiveam:is (not (composer--brace-pair-around-p "(a)" 1)))
  (fiveam:is (not (composer--brace-pair-around-p "\\()" 2))))

(fiveam:test a-shifted-key-reports-the-character-it-typed
  (fiveam:is (eql (char "()" 0)
                  (key-event-character (make-key-event :code 57 :state 65536 :text "("))))
  (fiveam:is (eql #\b (key-event-character (make-key-event :code 98 :state 0 :text "b"))))
  (fiveam:is (null (key-event-character (make-key-event :code 65288 :state 0 :text ""))))
  (fiveam:is (null (key-event-character (make-key-event :code 0 :state 0 :text "ab")))))
