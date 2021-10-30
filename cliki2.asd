(asdf:defsystem :cliki2
  :description "The Common Lisp wiki"
  :author "Andrey Moskvitin <archimag@gmail.com>, Vladimir Sedach <vsedach@common-lisp.net>"
  :maintainer "Vladimir Sedach <vsedach@common-lisp.net>"
  :license "AGPL-3.0-or-later"
  :version "2.0"
  :depends-on (#:alexandria
               #:hunchentoot
               #:bordeaux-threads
               #:ironclad
               #:colorize
               #:sanitize
               #:diff
               #:cl-interpol
               #:uri-template
               #:flexi-streams
               #:cl-ppcre
               #:cl-smtp
               #:cl-fad
               #:anaphora
               #:stem
               #:osicat ;; used for rename
               )
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "readtable")
             (:file "acceptor")
             (:file "http-resource")
             (:file "html-rendering")
             (:file "wiki")
             (:file "authentication")
             (:file "accounts")
             (:file "article")
             (:file "markup")
             (:file "diff")
             (:file "indexes")
             (:file "recent-changes")
             (:file "history")
             (:file "backlinks")
             (:file "tools")
             (:file "dispatcher")
             (:file "start")))))
