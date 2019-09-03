
(in-package #:cl-firebird)

(declaim (inline %tpb))


(defparameter +transaction-parameter-block+
  (vector
   (vector			; ISOLATION-LEVEL-READ-COMMITED-LEGACY
    +isc-tpb-version3+
    +isc-tpb-write+
    +isc-tpb-wait+
    +isc-tpb-read-committed+
    +isc-tpb-no-rec-version+)
   (vector			       ; ISOLATION-LEVEL-READ-COMMITED
    +isc-tpb-version3+
    +isc-tpb-write+
    +isc-tpb-wait+
    +isc-tpb-read-committed+
    +isc-tpb-rec-version+)
   (vector			     ; ISOLATION-LEVEL-REPEATABLE-READ
    +isc-tpb-version3+
    +isc-tpb-write+
    +isc-tpb-wait+
    +isc-tpb-concurrency+)
   (vector				; ISOLATION-LEVEL-SERIALIZABLE
    +isc-tpb-version3+
    +isc-tpb-write+
    +isc-tpb-wait+
    +isc-tpb-consistency+)
   (vector			    ; ISOLATION-LEVEL-READ-COMMITED-RO
    +isc-tpb-version3+
    +isc-tpb-read+
    +isc-tpb-wait+
    +isc-tpb-read-committed+
    +isc-tpb-rec-version+)))


(defun %tpb (lvl)
  (handler-case
      (let ((x (svref +transaction-parameter-block+ lvl)))
	(make-array (length x)
		    :adjustable t
		    :fill-pointer t
		    :initial-contents x))
    (simple-error ()
      (error "Wrong isolation level: ~a" lvl))))


(defun transaction-savepoint (trans name)
  (when (object-handle trans)
    (wp-op-exec-immediate (connection trans)
			  (object-handle trans)
			  (format nil "SAVEPOINT ~a" name))
    (wp-op-response (connection trans)))
  (values))


(defun transaction-commit (trans)
  (when (object-handle trans)
    (wp-op-commit (connection trans) (object-handle trans))
    (wp-op-response (connection trans))
    (setf (slot-value trans 'handle) nil)
    (setf (slot-value trans 'dirty) nil))
  (values trans))


(defun transaction-commit-retaining (trans)
  (when (object-handle trans)
    (wp-op-commit-retaining (connection trans) (object-handle trans))
    (wp-op-response (connection trans))
    (setf (slot-value trans 'dirty) nil))
  (values trans))


(defun transaction-rollback-to-savepoint (trans savepoint)
  (when (object-handle trans)
    (wp-op-exec-immediate (connection trans)
			  (object-handle trans)
			  (format nil "ROLLBACK TO ~a" savepoint))
    (wp-op-response (connection trans))
    (values trans)))


(defun transaction-rollback (trans)
  (when (object-handle trans)
    (wp-op-rollback (connection trans) (object-handle trans))
    (wp-op-response (connection trans))
    (setf (slot-value trans 'handle) nil)
    (setf (slot-value trans 'dirty) nil))
  (values trans))


(defun transaction-rollback-retaining (trans)
  (when (object-handle trans)
    (wp-op-rollback-retaining (connection trans) (object-handle trans))
    (wp-op-response (connection trans))
    (setf (slot-value trans 'dirty) nil))
  (values trans))


(defun check-info-requests (info-requests)
  (cond
    ((null info-requests)
     (setf info-requests (list +isc-info-end+)))
    ((integerp info-requests)
     (setf info-requests (list info-requests +isc-info-end+)))
    ((zerop (length info-requests))
     (setf info-requests (list +isc-info-end+))))
  (let* ((len (length info-requests))
	 (add-end (/= (elt info-requests (1- len)) +isc-info-end+)))
    (values
     (with-byte-stream (bs)
       (append-bytes bs info-requests)
       (when add-end (append-bytes bs +isc-info-end+))))))


(defun %parse-trans-info (buf ireq)
  (let ((buflen (length buf))
	(i 0) (ir 0) req r l)
    (loop
       (unless (< i buflen) (return))
       (setf req (elt buf i))
       (when (= req +isc-info-end+) (return))
       (assert (or (= req (elt ireq ir))
		   (= req +isc-info-error+)))
       (setf l (bytes-to-long-le (subseq buf (1+ i) (+ i 3))))
       (push (list req (elt ireq ir) (subseq buf (+ i 3) (+ i 3 l))) r)
       (incf i (+ 3 l))
       (incf ir)) ; loop
    (values r)))


(defvar +isc-info-tra-all+
  (list
   +isc-info-tra-id+
   +isc-info-tra-oldest-interesting+
   +isc-info-tra-oldest-snapshot+
   +isc-info-tra-oldest-active+
   +isc-info-tra-isolation+
   +isc-info-tra-access+
   +isc-info-tra-lock-timeout+))


(defun transaction-info (trans info-requests)
  (unless trans (setf trans *transaction*))
  (setf info-requests (check-info-requests info-requests))
  (let ((conn (connection trans)))
    (wp-op-info-transaction conn (object-handle trans) info-requests)
    (multiple-value-bind (h oid buf)
	(wp-op-response conn)
      (declare (ignore h oid))
      (let ((r (%parse-trans-info buf info-requests))
	    (res nil))
	(loop for (x y z) in r
	   do (setf (getf res y)
		    (cond
		      ((= x +isc-info-tra-isolation+)
		       (cons (elt z 0) (elt z 1)))
		      ((= x +isc-info-error+) nil)
		      (t (bytes-to-long-le z)))))
	(values res)))))


(defmethod print-object ((object transaction) stream)
  (print-unreadable-object (object stream :type t :identity t)
     ))


(defun execute-immediate (query trans)
  (wp-op-exec-immediate (connection trans)
                        (object-handle trans)
                        query)
  (wp-op-response (connection trans))
  (setf (slot-value trans 'dirty) t)
  (values))

