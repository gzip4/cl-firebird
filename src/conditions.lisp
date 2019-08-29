
(in-package #:cl-firebird)

(declaim (ftype (function (condition stream) t) report-condition))

(define-condition integrity-error (error)
  ((message :initarg :msg :reader error-message)
   (gds-codes :initarg :gds :reader error-gds-codes :initform nil)
   (sql-code :initarg :sql :reader error-sql-code :initform 0))
  (:report report-condition))

(define-condition operational-error (error)
  ((message :initarg :msg :reader error-message)
   (gds-codes :initarg :gds :reader error-gds-codes :initform nil)
   (sql-code :initarg :sql :reader error-sql-code :initform 0))
  (:report report-condition))

(defun report-condition (condition stream)
  (format stream (error-message condition))
  (format stream "~&GDS-CODES: ~a~%" (error-gds-codes condition))
  (format stream "~&SQL-CODE: ~a~%" (error-sql-code condition)))

