(in-package #:aui)

;;;; -- Completion Tests --

(fiveam:def-suite completion
  :description "Tests for the command completion of the composer."
  :in          aui)

(fiveam:in-suite completion)

(fiveam:test an-empty-pair-offers-every-command
  (fiveam:is (string= "" (completion--command-prefix "()" 1)))
  (fiveam:is (string= "" (completion--command-prefix "(" 1))))

(fiveam:test the-name-before-the-caret-is-the-prefix
  (fiveam:is (string= "m" (completion--command-prefix "(m)" 2)))
  (fiveam:is (string= "model" (completion--command-prefix "(model)" 6))))

(fiveam:test a-close-paren-before-the-caret-ends-the-command
  (fiveam:is (null (completion--command-prefix "(model)" 7))))

(fiveam:test a-draft-that-is-not-a-command-offers-nothing
  (fiveam:is (null (completion--command-prefix "" 0)))
  (fiveam:is (null (completion--command-prefix "hello" 5)))
  (fiveam:is (null (completion--command-prefix "foo (m)" 7))))

(fiveam:test the-pair-under-the-caret-is-part-of-what-is-typed
  (fiveam:is (string= "()" (completion--typed-text "()" 1)))
  (fiveam:is (string= "(model)" (completion--typed-text "(model)" 6)))
  (fiveam:is (string= "hello @na" (completion--typed-text "hello @na world" 9))))
