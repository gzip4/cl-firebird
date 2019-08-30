
(in-package :cl-user)
(defpackage :cl-firebird-asd
  (:use :cl :asdf))
(in-package :cl-firebird-asd)

(defsystem cl-firebird
    :version "0.2"
    :author "Kirill Zverev"
    :license "MIT"
    :description "Firebird database remote protocol client"
    :depends-on (:flexi-streams
		 :usocket
		 :ironclad
		 :nibbles
		 :log4cl
		 :crypt)
    :components ((:module "src"
		  :serial t
		  :components ((:file "packages")
			       (:file "consts")
			       (:file "specials")
			       (:file "ironclad")
			       (:file "utils")
			       (:file "conditions")
			       (:file "messages")
			       (:file "srp")
			       (:file "classes")
			       (:file "wireprotocol")
			       (:file "blob")
			       (:file "xsqlvar")
			       (:file "transaction")
			       (:file "statement")
			       (:file "connection")))))
