;;;; -- AUI Systems --

(asdf:defsystem #:aui/toolkit
  :description "Shared drawing and input controls for AUI and Unslop."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:lightfast
                #:serapeum)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:module "model"
      :serial t
      :components
      ((:file "preferences")))
     (:module "ui"
      :serial t
      :components
      ((:file "theme")
       (:file "paint")
       (:file "picture")
       (:file "stage")
       (:file "paragraph")
       (:file "blocks")
       (:file "controls")))))))

(asdf:defsystem #:aui
  :description "A native graphical client for Autolith."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:aui/toolkit
                #:autolith
                #:sito
                #:bordeaux-threads)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "model"
      :serial t
      :components
      ((:file "transcript")
       (:file "events")
       (:file "markdown")
       (:file "lisp-format")))
     (:module "client"
      :serial t
      :components
      ((:file "portable")
       (:file "installation")
       (:file "world")
       (:file "records")
       (:file "history")
       (:file "workspace")
       (:file "highlight")
       (:file "session")
       (:file "launch")))
     (:module "view"
      :serial t
      :components
      ((:file "prose")
       (:file "entries")
       (:file "document")
       (:file "workbench")
       (:file "files")
       (:file "digest")
       (:file "overlay")
       (:file "sidebar")
       (:file "composer")
       (:file "completion")
       (:file "chrome")
       (:file "activity")
       (:file "catalogs")
       (:file "sheets")
       (:file "panels")
       (:file "console")
       (:file "window")
       (:file "demo")))
     (:file "main"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:aui/tests))))

(asdf:defsystem #:aui/unslop
  :description "An editor for prose in Common Lisp source files."
  :author      "Lambda Symbolics"
  :license     "MIT"
  :depends-on  (#:aui/toolkit
                #:sb-posix)
  :serial      t
  :components
  ((:module "src"
    :serial t
    :components
    ((:module "unslop"
      :serial t
      :components
      ((:file "lexer")
       (:file "proof")
       (:file "checkout")
       (:file "ledger")
       (:file "desk")
       (:file "shelf")
       (:file "page")
       (:file "unslop")))))))

(asdf:defsystem #:aui/test-support
  :description "FiveAM suites and test runner for AUI and Unslop."
  :depends-on  (#:aui/toolkit
                #:fiveam)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")))))

(asdf:defsystem #:aui/unslop/tests
  :description "Tests for unslop"
  :depends-on  (#:aui/unslop
                #:aui/test-support)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:module "unslop"
      :serial t
      :components
      ((:file "lexer-tests")
       (:file "proof-tests")
       (:file "checkout-tests")))))))

(asdf:defsystem #:aui/tests
  :description "Tests for AUI"
  :depends-on  (#:aui
                #:aui/test-support
                #:aui/unslop/tests)
  :serial      t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "transcript-tests")
     (:file "markdown-tests")
     (:file "records-tests")
     (:file "history-tests")
     (:file "session-tests")
     (:file "document-tests")
     (:file "links-tests")
     (:file "composer-tests")
     (:file "completion-tests")
     (:file "launch-tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (unless (uiop:symbol-call '#:aui '#:run-tests)
                   (error "AUI tests failed."))))
