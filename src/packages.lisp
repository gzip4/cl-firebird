
(in-package #:cl-user)

(defpackage :cl-firebird
  (:nicknames :fbsql)
  (:use :cl)
  (:export #:integrity-error
	   #:operational-error
	   #:connect
	   #:disconnect
	   #:disconnected-p
	   #:db-info
	   #:drop-database
	   #:with-connection
	   #:prepare
	   #:execute
	   #:execute-many
	   #:callproc
	   #:fetch
	   #:fetch1
	   #:fetch!
	   #:connect*
	   #:disconnect*
	   #:disconnected-p*
	   #:db-info*
	   #:drop-database*
	   #:with-transaction*

	   #:begin
	   #:savepoint
	   #:commit
	   #:commit-retaining
	   #:rollback-savepoint
	   #:rollback
	   #:rollback-retaining
	   #:transaction-info
	   #:make-transaction
	   #:execute-immediate
	   #:with-transaction
	   #:with-transaction/ac

	   #:statement-row-count
	   #:with-statement
	   #:blob-value))

(in-package #:cl-user)

