
(in-package #:cl-firebird)


(defparameter +stmt-type+
  (list
   +isc-info-sql-stmt-select+ :select
   +isc-info-sql-stmt-insert+ :insert
   +isc-info-sql-stmt-update+ :update
   +isc-info-sql-stmt-delete+ :delete
   +isc-info-sql-stmt-ddl+ :ddl
   +isc-info-sql-stmt-get-segment+ :get-segment
   +isc-info-sql-stmt-put-segment+ :put-segment
   +isc-info-sql-stmt-exec-procedure+ :exec-procedure
   +isc-info-sql-stmt-start-trans+ :start-trans
   +isc-info-sql-stmt-commit+ :commit
   +isc-info-sql-stmt-rollback+ :rollback
   +isc-info-sql-stmt-select-for-upd+ :select-for-upd
   +isc-info-sql-stmt-set-generator+ :set-generator
   +isc-info-sql-stmt-savepoint+ :savepoint))


(defun make-statement (transaction)
  (make-instance 'statement :trans transaction))


(defun statement-allocate (stmt)
  (let ((conn (connection (transaction stmt))))
    (with-slots (accept-type lazy-response-count) conn
      (wp-op-allocate-statement conn)
      (if (= accept-type +ptype-lazy-send+)
	  (progn (incf lazy-response-count)
		 (setf (slot-value stmt 'handle) -1))
	  (setf (slot-value stmt 'handle)
		(wp-op-response conn)))))
  (values stmt))


(defmethod initialize-instance :after ((stmt statement) &key)
  (statement-allocate stmt))


(defmethod print-object ((object statement) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (handle open stmt-type plan)
	object
      (format stream "[~a] ~a/~a~a" handle (getf +stmt-type+ stmt-type)
	      (if open "OPEN" "CLOSE")
	      (if plan ", plan" "")))))


(defun statement-prepare (stmt sql &key explain-plan)
  (let ((conn (connection (transaction stmt)))
	(handle (slot-value stmt 'handle))
	(trans-handle (transaction-handle (transaction stmt))))
    (setf (slot-value stmt 'plan) nil)
    (wp-op-prepare-statement conn handle trans-handle sql (if explain-plan +isc-info-sql-get-plan+))

    (with-slots (lazy-response-count) conn
      (unless (zerop lazy-response-count)
	(decf lazy-response-count)
	(setf (slot-value stmt 'handle) (wp-op-response conn))))

    (multiple-value-bind (h oid buf)
	(wp-op-response conn)
      (declare (ignore h oid))
      (let ((i 0) l)
	(when (= (elt buf i) +isc-info-sql-get-plan+)
	  (setf l (bytes-to-long-le (subseq buf (1+ i) (+ i 3))))
	  (setf (slot-value stmt 'plan) (bytes-to-str (subseq buf (+ i 3) (+ i 3 l))))
	  (incf i (+ 3 l)))
	(multiple-value-bind (stmt-type xsqlda)
	    (xsqlvar-parse-xsqlda (subseq buf i) conn (slot-value stmt 'handle))
	  (setf (slot-value stmt 'stmt-type) stmt-type)
	  (setf (slot-value stmt 'xsqlda) xsqlda)
	  (when (= stmt-type +isc-info-sql-stmt-select+)
	    (setf (slot-value stmt 'open) t))))))
  (values stmt))
				 

(defun %statement-convert-params (params)
  (loop :for param :in params
     :collect (typecase param
		(character (string param))
		(t param))))


(defun %statement-execute-proc (stmt params)
  (wp-op-execute2 (connection (transaction stmt))
		  (object-handle stmt)
		  (transaction-handle (transaction stmt))
		  (%statement-convert-params params)
		  (xsqlvar-calc-blr (statement-xsqlda stmt)))
  (setf (slot-value stmt 'result)
	(wp-op-sql-response (connection (transaction stmt))
			    (statement-xsqlda stmt))))


(defun %statement-execute-other (stmt params)
  (wp-op-execute (connection (transaction stmt))
		 (object-handle stmt)
		 (transaction-handle (transaction stmt))
		 (%statement-convert-params params)))


(defun statement-execute-list (stmt &optional params)
  (check-trans-handle (transaction stmt))
  (if (= (statement-type stmt) +isc-info-sql-stmt-exec-procedure+)
      (%statement-execute-proc stmt params)
      (%statement-execute-other stmt params))
  (handler-case
      (wp-op-response (connection (transaction stmt)))
    (warning (w)
      (setf (slot-value stmt 'open) nil)
      (warn w)))
  (setf (slot-value (transaction stmt) 'dirty) t)
  (values stmt))


(defun statement-execute (stmt &rest params)
  (statement-execute-list stmt params))


(defun statement-make-fetcher (stmt &optional plist)
  (let* ((xsqlda (statement-xsqlda stmt))
	 (handle (object-handle stmt))
	 (conn (connection (transaction stmt)))
	 (blr (xsqlvar-calc-blr xsqlda))
	 rows (more-data t))
    (lambda ()
      (if rows
	  (pop rows)
	  (when more-data
	    (wp-op-fetch conn handle blr)
	    (multiple-value-bind (r md)
		(if plist
		    (wp-op-fetch-response-plist conn handle xsqlda)
		    (wp-op-fetch-response conn handle xsqlda))
	      (setf rows r more-data md)
	      (unless more-data (statement-close stmt))
	      (pop rows)))))))


(defun statement-fetch-one (stmt &optional plist)
  (let* ((xsqlda (statement-xsqlda stmt))
	 (handle (object-handle stmt))
	 (conn (connection (transaction stmt)))
	 (blr (xsqlvar-calc-blr (statement-xsqlda stmt))))
    (wp-op-fetch conn handle blr 1)
    (multiple-value-bind (rows)
	(if plist
	    (wp-op-fetch-response-plist conn handle xsqlda)
	    (wp-op-fetch-response conn handle xsqlda))
      (values (first rows) stmt))))


(defun statement-fetch-single (stmt)
  (values (first (statement-fetch-one stmt)) stmt))


(defun statement-fetch-all (stmt &optional plist)
  (let* ((xsqlda (statement-xsqlda stmt))
	 (handle (object-handle stmt))
	 (conn (connection (transaction stmt)))
	 (blr (xsqlvar-calc-blr xsqlda))
	 result)
    (loop
       (wp-op-fetch conn handle blr)
       (multiple-value-bind (rows more-data)
	   (if plist
	       (wp-op-fetch-response-plist conn handle xsqlda)
	       (wp-op-fetch-response conn handle xsqlda))
	 (setf result (nconc result rows))
	 (unless more-data
	   (statement-close stmt)
	   (return))))
    (values result stmt)))


(defun statement-row-count (stmt)
  (log:debug stmt)
  (let ((conn (connection (transaction stmt))))
    (wp-op-info-sql conn (object-handle stmt) (make-bytes +isc-info-sql-records+))
    (multiple-value-bind (h oid buf)
	(wp-op-response conn)
      (declare (ignore h oid))
      (assert (equalp (subseq buf 0 3) #(#x17 #x1d 0)))
      (let ((count (if (= (statement-type stmt) +isc-info-sql-stmt-select+)
		       (progn (assert (equalp (subseq buf 17 20) #(#x0d #x04 0)))
			      (bytes-to-long-le (subseq buf 20 24)))
		       (+ (bytes-to-long-le (subseq buf 27 31))
			  (bytes-to-long-le (subseq buf 6 10))
			  (bytes-to-long-le (subseq buf 13 17))))))
	(log:trace count)
	(values count stmt)))))


(defun statement-close (stmt)
  (log:debug stmt)
  (let ((conn (connection (transaction stmt))))
    (when (and (statement-type stmt)
	       (= (statement-type stmt) +isc-info-sql-stmt-select+)
	       (statement-open-p stmt))
      (wp-op-free-statement conn (object-handle stmt) +dsql-close+)
      (with-slots (accept-type lazy-response-count) conn
	(if (= accept-type +ptype-lazy-send+)
	    (incf lazy-response-count)
	    (wp-op-response conn)))))
  (setf (slot-value stmt 'open) nil)
  (values stmt))


(defun statement-drop (stmt)
  (log:debug stmt)
  (let ((conn (connection (transaction stmt))))
    (when (and (object-handle stmt) (/= (object-handle stmt) -1))
      (wp-op-free-statement conn (object-handle stmt) +dsql-drop+)
      (with-slots (accept-type lazy-response-count) conn
	(if (= accept-type +ptype-lazy-send+)
	    (incf lazy-response-count)
	    (wp-op-response conn)))))
  (setf (slot-value stmt 'open) nil
	(slot-value stmt 'handle) -1)
  (values stmt))
