
(in-package #:cl-firebird)


(defun %parse-dsn (dsn)
  (let ((i (position #\: dsn)) h p f)
    (if i
	(let* ((hostport (subseq dsn 0 i))
	       (j (position #\/ hostport)))
	  (setf f (subseq dsn (1+ i)))
	  (if j
	      (setf h (subseq hostport 0 j)
		    p (parse-integer (subseq hostport (1+ j))))
	      (setf h hostport)))
	(setf f dsn))
    (values h p f)))


(defun %socket-connect (conn)
  (setf (slot-value conn 'socket)
	(usocket:socket-connect (slot-value conn 'hostname)
				(slot-value conn 'port)
				:timeout (slot-value conn 'timeout)
				:element-type '(unsigned-byte 8))))


(defun connect (&key dsn user password role host database
		  (charset +default-charset+) (port 3050)
		  (page-size 4096) is-services timeout
		  isolation-level use-unicode (auth-plugin-name :srp)
		  (wire-crypt t) create-new timezone)
  (declare (ignorable is-services))
  (assert (member page-size '(2048 4096 8192 16384)))
  (let ((conn (make-instance 'connection)))
    (if dsn
	(multiple-value-bind (h p f)
	    (%parse-dsn dsn)
	  (setf (slot-value conn 'hostname) (or h host)
		(slot-value conn 'filename) (or f database)
		port (or p port)))
	(setf (slot-value conn 'hostname) host
	      (slot-value conn 'filename) database))
    (unless (slot-value conn 'hostname)
      (setf (slot-value conn 'hostname) "localhost"))
    (setf (slot-value conn 'port) port
	  (slot-value conn 'user) user
	  (slot-value conn 'password) password
	  (slot-value conn 'role) role
	  (slot-value conn 'charset) charset
	  (slot-value conn 'timeout) (if timeout (floor timeout))
	  (slot-value conn 'auth-plugin-name) auth-plugin-name
	  (slot-value conn 'wire-crypt) wire-crypt
	  (slot-value conn 'page-size) page-size
	  (slot-value conn 'use-unicode) use-unicode ; XXX: need implement
	  (slot-value conn 'timezone) timezone
	  (slot-value conn 'last-event-id) 0
	  (slot-value conn 'isolation-level) (if (null isolation-level)
						 +isolation-level-read-commited+
						 (floor isolation-level)))
    (let ((sock (%socket-connect conn)))
      (setf (slot-value conn 'stream) (usocket:socket-stream sock))
      (wp-op-connect conn auth-plugin-name wire-crypt)
      (handler-case
	  (wp-op-accept conn)
	(operational-error (e)
	  (usocket:socket-close sock)
	  (setf (slot-value conn 'socket) nil
		(slot-value conn 'stream) nil)
	  (error e))))
    (cond
      (create-new (wp-op-create conn page-size))
      (is-services (error "is-services"))
      (t (wp-op-attach conn)))
    (setf (slot-value conn 'db-handle) (wp-op-response conn))
    (values conn)))


(defun %clean-connection-object (connection)
  (usocket:socket-close (slot-value connection 'socket))
  (setf (slot-value connection 'socket) nil
	(slot-value connection 'stream) nil
	(slot-value connection 'stream-cypher-recv) nil
	(slot-value connection 'stream-cypher-send) nil
	(slot-value connection 'db-handle) nil)
  (slot-makunbound connection 'auth-data)
  (slot-makunbound connection 'client-public-key)
  (slot-makunbound connection 'client-private-key))


(defun disconnect (connection)
  (when (slot-value connection 'socket)
    (when (slot-value connection 'db-handle)
      (if (slot-value connection 'is-services)
	  (error "disconnect->is-services not implemented")
	  (wp-op-detach connection))
      (handler-case
	  (wp-op-response connection)
	(error (e) (warn "disconnect: ~a" e)))
      (%clean-connection-object connection)))
  (values))


(defun disconnected-p (connection)
  (not (slot-value connection 'socket)))


(defmethod print-object ((object connection) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (user role charset filename hostname port auth-plugin-name)
	object
      (format stream "[~a/~a:~a] [~a~a]~a AUTH:~a" hostname port filename
	      user (if role (format nil "/~a" role) "")
	      (if charset (format nil " CHAR:~a" charset) "")
	      auth-plugin-name))))


(defparameter +REQ-INT+
  (list +isc-info-allocation+ +isc-info-no-reserve+ +isc-info-db-sql-dialect+
        +isc-info-ods-minor-version+ +isc-info-ods-version+
        +isc-info-page-size+ +isc-info-current-memory+ +isc-info-forced-writes+
        +isc-info-max-memory+ +isc-info-num-buffers+ +isc-info-sweep-interval+
        #+nil +isc-info-limbo+ +isc-info-attachment-id+ +isc-info-fetches+
        +isc-info-marks+ +isc-info-reads+ +isc-info-writes+
        +isc-info-set-page-buffers+ +isc-info-db-read-only+
        +isc-info-db-size-in-pages+ +isc-info-page-errors+
        +isc-info-record-errors+ +isc-info-bpage-errors+
        +isc-info-dpage-errors+ +isc-info-ipage-errors+
        +isc-info-ppage-errors+ +isc-info-tpage-errors+
        ;; may not be available in some versions of Firebird
        +isc-info-oldest-transaction+ +isc-info-oldest-active+
        +isc-info-oldest-snapshot+ +isc-info-next-transaction+
        +isc-info-active-tran-count+))

(defparameter +REQ-COUNT+
  (list +isc-info-backout-count+ +isc-info-delete-count+
        +isc-info-expunge-count+ +isc-info-insert-count+ +isc-info-purge-count+
        +isc-info-read-idx-count+ +isc-info-read-seq-count+
        +isc-info-update-count+))


(defun db-info-creation-date (v)
  (let* ((nday (+ (bytes-to-long-le
		   (subseq v 0 4))
		  2400001 -1721119))
	 (ntime (bytes-to-long-le (subseq v 4)))
	 (century (floor (1- (* 4 nday)) 146097))
	 dd mm yy h m s ms)
    (setf nday (- (* 4 nday) 1 (* 146097 century)))
    (setf dd (floor nday 4))
    (setf nday (floor (+ 3 (* 4 dd)) 1461))
    (setf dd (- (+ (* 4 dd) 3) (* 1461 nday)))
    (setf dd (floor (+ dd 4) 4))
    (setf mm (floor (- (* 5 dd) 3) 153))
    (setf dd (- (* 5 dd) 3 (* 153 mm)))
    (setf dd (floor (+ 5 dd) 5))
    (setf yy (+ (* 100 century) nday))
    (if (< mm 10)
	(incf mm 3)
	(progn (decf mm 9) (incf yy)))
    (setf h (floor ntime (* 3600 +ISC-TIME-SECONDS-PRECISION+)))
    (setf ntime (mod ntime (* 3600 +ISC-TIME-SECONDS-PRECISION+)))
    (setf m (floor ntime (* 60 +ISC-TIME-SECONDS-PRECISION+)))
    (setf ntime (mod ntime (* 60 +ISC-TIME-SECONDS-PRECISION+)))
    (setf s (floor ntime +ISC-TIME-SECONDS-PRECISION+))
    (setf ms (* (mod ntime +ISC-TIME-SECONDS-PRECISION+) 100))
    (list (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~d" yy mm dd h m s ms)
	  yy mm dd h m s ms)))

(defun db-info-convert-type (ir v)
  (cond
    ((= ir +isc-info-base-level+) (elt v 1))
    ((= ir +isc-info-db-id+)
     (let* ((conn-code (elt v 0))
	    (len1 (elt v 1))
	    (filename (bytes-to-str (subseq v 2 (+ 2 len1))))
	    (len2 (elt v (+ 2 len1)))
	    (sitename (bytes-to-str (subseq v (+ 3 len1) (+ 3 len1 len2)))))
       (list conn-code filename sitename)))
    ((= ir +isc-info-implementation+)
     (list (elt v 1) (elt v 2)))
    ((member ir '(#.+isc-info-version+ #.+isc-info-firebird-version+))
     (bytes-to-str (subseq v 2 (+ 2 (elt v 1)))))
    ((= ir +isc-info-user-names+)
     (loop for un in v collect (bytes-to-str (subseq un 1))))
    ((member ir +REQ-INT+)
     (bytes-to-long-le v))
    ((member ir +REQ-COUNT+)
     (loop for i from 0 to (1- (length v)) by 6
	collect (list (bytes-to-long-le (subseq v i (+ 2 i)))
		      (bytes-to-long-le (subseq v (+ 2 i) (+ 6 i))))))
    ((= ir +isc-info-creation-date+)
     (db-info-creation-date v))
    (t v)))


(defun %parse-db-info (buf ireq)
  (let ((buflen (length buf))
	(i 0) (ir 0) req r l)
    (loop
       (unless (< i buflen) (return))
       (setf req (elt buf i))
       (when (= req +isc-info-end+) (return))
       (assert (or (= req (elt ireq ir))
		   (= req +isc-info-error+)))
       (if (= req +isc-info-user-names+)
	   (let (user-names)
	     (loop
		(unless (= req +isc-info-user-names+) (return))
		(setf l (bytes-to-long-le (subseq buf (1+ i) (+ i 3))))
		(push (subseq buf (+ i 3) (+ i 3 l)) user-names)
		(incf i (+ 3 l))
		(setf req (elt buf i)))
	     (push (list (elt ireq ir) (elt ireq ir) (nreverse user-names)) r))
	   (progn
	     (setf l (bytes-to-long-le (subseq buf (1+ i) (+ i 3))))
	     (push (list req (elt ireq ir) (subseq buf (+ i 3) (+ i 3 l))) r)
	     (incf i (+ 3 l))))
       (incf ir)) ; loop
    (values r)))


(defun db-info (conn info-requests)
  (setf info-requests (check-info-requests info-requests))
  (wp-op-info-database conn info-requests)
  (multiple-value-bind (h oid buf)
      (wp-op-response conn)
    (declare (ignore h oid))
    (let ((r nil))
      (loop for (x y v) in (%parse-db-info buf info-requests)
	 ;; make p-list
	 do (push (if (/= x +isc-info-error+) (db-info-convert-type y v)) r)
	 do (push y r))
      (values r))))


(defun drop-database (connection)
  (wp-op-drop-database connection)
  (wp-op-response connection)
  (%clean-connection-object connection)
  (values))


(defun start-transaction (conn &key auto-commit)
  (let ((tpb (%tpb (isolation-level conn))))
    (when auto-commit
      (vector-push-extend +isc-tpb-autocommit+ tpb))
    (wp-op-transaction conn (coerce tpb '(simple-array (unsigned-byte 8) (*))))
    (let ((h (wp-op-response conn))
	  (trans (make-instance 'transaction
				:conn conn
				:auto-commit auto-commit)))
      (setf (slot-value trans 'handle) (if (>= h 0) h)
	    (slot-value trans 'dirty) nil)
      (values trans))))


(defmacro with-connection ((var &rest args &key dsn user password role host database &allow-other-keys)
			   &body body)
  (declare (ignorable dsn user password role host database))
  (let* ((conn! (gensym "CONNECTION"))
	 (var! (when var (list (list var conn!))))
	 (var-decl! (when var (list 'declare (list 'ignorable var)))))
    `(let* ((,conn! (funcall #'connect ,@args))
	    (*connection* ,conn!)
	    ,@var!)
       ,var-decl!
       (unwind-protect
	    (progn ,@body)
	 (disconnect ,conn!)))))


(defun immediate (sql &optional trans)
  (execute-immediate sql (or trans *transaction*)))


(defmacro with-transaction ((&optional conn var) &body body)
  (let* ((conn! (gensym "CONNECTION"))
	 (tr! (gensym "TRANSACTION"))
	 (var! (when var (list (list var tr!))))
	 (var-decl! (when var (list 'declare (list 'ignorable var)))))
    `(let* ((,conn! (or ,conn *connection*))
	    (,tr! (start-transaction ,conn! :auto-commit nil))
	    (*transaction* ,tr!)
	    ,@var!)
       ,var-decl!
       (unwind-protect
            (multiple-value-prog1
		(progn ,@body)
	      (transaction-commit ,tr!))
	 (transaction-rollback ,tr!)))))


(defmacro with-transaction/ac ((&optional conn var) &body body)
  (let* ((conn! (gensym "CONNECTION"))
	 (tr! (gensym "TRANSACTION"))
	 (var! (when var (list (list var tr!))))
	 (var-decl! (when var (list 'declare (list 'ignorable var)))))
    `(let* ((,conn! (or ,conn *connection*))
	    (,tr! (start-transaction ,conn! :auto-commit t))
	    (*transaction* ,tr!)
	    ,@var!)
       ,var-decl!
       (unwind-protect
            (multiple-value-prog1
		(progn ,@body)
	      (transaction-commit ,tr!))
	 (transaction-rollback ,tr!)))))


(defun prepare (sql &key explain-plan)
  (statement-prepare (make-statement *transaction*)
		     sql :explain-plan explain-plan))


(defmacro with-statement ((var sql &key explain-plan) &body body)
  (let ((sql! (gensym "SQL")))
    `(let (,var)
       (unwind-protect
	    (let ((,sql! ,sql))
	      (setf ,var (etypecase ,sql!
			   (statement ,sql!)
			   (string (prepare ,sql! :explain-plan ,explain-plan))))
	      (let ((*statement* ,var))
		,@body))
	 (when ,var (statement-drop ,var))))))


(defun execute (query &rest params)
  (let ((stmt (etypecase query
		(statement query)
		(string (statement-prepare (make-statement *transaction*) query)))))
    (statement-execute-list stmt params)))
    

(defun execute-many (query params)
  (let ((stmt (etypecase query
		(statement query)
		(string (statement-prepare (make-statement *transaction*) query)))))
    (loop :for p :in params :do (statement-execute-list stmt p))
    (values stmt)))


(defun callproc (name &rest params)
  (let* ((p? (make-list (length params) :initial-element #\?))
	 (sql (format nil "EXECUTE PROCEDURE ~a ~{~a~^,~}" name p?))
	 (stmt (statement-prepare (make-statement *transaction*) sql)))
    (unwind-protect
	 (progn
	   (statement-execute-list stmt params)
	   (statement-result stmt))
      (statement-drop stmt))))


(defun fetch (&optional stmt (result :fetch-all))
  (setf stmt (or stmt *statement*))
  (let ((need-close t))
    (unwind-protect
	 (case result
	   (:fetch-all (statement-fetch-all stmt nil))
	   (:fetch-all-plist (statement-fetch-all stmt t))
	   (:fetch-one (statement-fetch-one stmt))
	   (:fetch-one-plist (statement-fetch-one stmt t))
	   (:one (statement-fetch-one stmt))
	   (:one-plist (statement-fetch-one stmt t))
	   (:single (statement-fetch-single stmt))
	   (:fetcher
	    (setf need-close nil)
	    (statement-make-fetcher stmt))
	   (:fetcher-plist
	    (setf need-close nil)
	    (statement-make-fetcher stmt t))
	   (otherwise (error "Unknown result type: ~a" result)))
      (when need-close
	(statement-close stmt)))))
  

(defun fetch1 (&optional stmt)
  (fetch stmt :one))
  

(defun fetch! (&optional stmt)
  (fetch stmt :single))
  

(defun commit (&optional trans)
  (transaction-commit (or trans *transaction*)))


(defun rollback (&optional trans)
  (transaction-rollback (or trans *transaction*)))


(defun savepoint (name &optional trans)
  (transaction-savepoint (or trans *transaction*) name))


(defun rollback-to-savepoint (name &optional trans)
  (transaction-rollback-to-savepoint (or trans *transaction*) name))


(defun row-count (&optional stmt)
  (statement-row-count (or stmt *statement*)))



;; == TOPLEVEL ==

(defun disconnect* ()
  (when *connection*
    (when *transaction*
      (ignore-errors (rollback *transaction*)))
    (ignore-errors (disconnect *connection*))
    (setf *connection* nil *transaction* nil))
  (values))


(defun connect* (&rest args &key &allow-other-keys)
  (when *connection*
    (restart-case
	(error "Already connected: ~a" *connection*)
      (restart-cancel ()
	:report "Cancel connection."
	(return-from connect* (values)))
      (restart-force ()
	:report "Force connect to database (disconnect previous)."
	(disconnect*))))
  (setf *connection* (apply #'connect args))
  (setf *transaction* (start-transaction *connection* :auto-commit t))
  (values))


(defun disconnected-p* ()
  (if *connection*
      (not (slot-value *connection* 'socket))
      t))


(defun db-info* (info-requests)
  (db-info *connection* info-requests))


(defun drop-database* ()
  (drop-database *connection*))

