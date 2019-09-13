
(in-package #:cl-firebird)


(defun %blob-bytes (blob-id conn trans-handle)
  (let ((val (byte-stream)))
    (wp-op-open-blob conn blob-id trans-handle)
    (loop :with h = (wp-op-response conn)
       :with n = 1
       :while (/= n 2)
       :do (wp-op-get-segment conn h)
       :do (multiple-value-bind (nn oid buf)
	       (wp-op-response conn)
	     (declare (ignore oid))
	     (setf n nn)
	     (loop
		(when (zerop (length buf)) (return))
		(let ((ln (bytes-to-long-le (subseq buf 0 2))))
		  (append-bytes val (subseq buf 2 (+ 2 ln)))
		  (setf buf (subseq buf (+ 2 ln))))))
       :finally (wp-op-close-blob conn h))
    (with-slots (accept-type lazy-response-count) conn
      (if (= accept-type +ptype-lazy-send+)
	  (incf lazy-response-count)
	  (wp-op-response conn)))
    (values (byte-stream-output val))))


(defun blob-create (bytes &optional trans)
  (let* ((trans (or trans *transaction*))
	 (conn (connection trans))
	 (trans-handle (object-handle trans))
	 (id (%create-blob conn trans-handle bytes :main))
	 (blob (make-instance 'blob :data bytes :blob-id id)))
    (values blob)))


(defun blob-create-temp (bytes &optional trans)
  (let* ((trans (or trans *transaction*))
	 (conn (connection trans))
	 (trans-handle (object-handle trans))
	 (id (%create-blob conn trans-handle bytes))
	 (blob (make-instance 'blob :data bytes :blob-id id)))
    (values blob)))


(defun blob-value (blob &optional trans)
  (let* ((trans (or trans *transaction*))
	 (conn (connection trans))
	 (trans-handle (object-handle trans)))
    (with-slots (data blob-id trans subtype) blob
      (if data
	  data
	  (progn
	    (block out
	      (handler-bind
		  ((operational-error
		    (lambda (e) (when (member 335544329 (error-gds-codes e))
				  (log:warn "Invalid BLOB ID: ~a" blob-id)
				  (return-from out)))))
		(setf data (%blob-bytes blob-id conn trans-handle))))
	    (if (= subtype 1)
		(setf data (if (connection-use-unicode conn)
			       (flex:octets-to-string data :external-format :utf8)
			       (flex:octets-to-string data)))
		data))))))


(defmethod print-object ((blob blob) stream)
  (with-slots (subtype blob-id) blob
    (print-unreadable-object (blob stream :type nil :identity nil)
      (format stream "BLOB ~a/~a"
	      (case subtype (0 "BINARY") (1 "TEXT") (t subtype))
	      (if blob-id (bytes-to-hex blob-id))))))




