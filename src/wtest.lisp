(in-package #:cl-firebird)

;;;;;;;;;;;;;;;;;;


(defclass wire-protocol-base ()
  ((socket :initarg :socket :reader wire-protocol-socket)
   (stream :initarg :stream :reader wire-protocol-stream)
   (stream-cypher-recv :initform nil)
   (stream-cypher-send :initform nil)
   (lazy-count :initform 0 :accessor wire-protocol-lazy-count)
   (accept-type :initform #.+ptype-batch-send+ :initarg :accept-type :reader wire-protocol-type)
   (buffer :initform (make-array 65536 :element-type 'nibbles:octet))
   (buffer4 :initform (make-array 4 :element-type 'nibbles:octet))
   (bytes-in :initform 0 :reader wire-protocol-bytes-in)
   (bytes-out :initform 0 :reader wire-protocol-bytes-out)
   (start-time :initform (get-universal-time) :reader wire-protocol-start-time)
   (auth-data :initform nil :reader wire-protocol-auth-data)
   (version :initform nil :initarg :version :reader wire-protocol-version)
   ))
(defclass wire-protocol-10 (wire-protocol-base) () (:default-initargs :version 10))
(defclass wire-protocol-11 (wire-protocol-10)   () (:default-initargs :version 11))
(defclass wire-protocol-12 (wire-protocol-11)   () (:default-initargs :version 12))
(defclass wire-protocol-13 (wire-protocol-12)   () (:default-initargs :version 13))
(defclass attachment ()
  ((protocol :initarg :protocol :reader attachment-protocol)
   (handle :initform nil :reader object-handle)
   (trans :initform nil :reader attachment-transaction)
   (auto-commit :initform nil :accessor attachment-auto-commit)
   (isolation-level :initform :read-committed :accessor attachment-isolation-level)))
(defclass cursor ()
  ((protocol :initarg :protocol :reader cursor-protocol)
   (handle :initform nil :initarg :handle :reader object-handle)
   (xsqlda :initform nil :initarg :xsqlda :reader cursor-xsqlda)))


(defgeneric fb-params-to-blr/vls (wp params))
(defgeneric fb-params-to-blr/null (wp vals is-null))
(defgeneric fb-op-fetch-row (wp xsqlda))


(defun wire-protocol-instance (socket stream accept-version accept-type)
  (make-instance 
   (case accept-version
     (10 'wire-protocol-10)
     (11 'wire-protocol-11)
     (12 'wire-protocol-12)
     (13 'wire-protocol-13)
     (t (error "Unknown protocol version: ~a" accept-version)))
   :socket socket
   :stream stream
   :accept-type accept-type))


(defun wire-protocol-lazy-p (wp)
  (= (wire-protocol-type wp) +ptype-lazy-send+))


(defun fb-socket-connect (host port)
  #+sbcl
  (let* ((he (etypecase host
	       (string (sb-bsd-sockets:get-host-by-name host))
	       (vector (sb-bsd-sockets:get-host-by-address host))))
	 (sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:host-ent-address he) port)
    (values (sb-bsd-sockets:socket-make-stream sock :element-type '(unsigned-byte 8)
					       :input t :output t)
	    sock))
  #-sbcl
  (let ((sock (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (values (usocket:socket-stream s) sock)))


#+nil
(defun fb-create-dpb-v1 (&optional auth-data)
  (with-byte-stream (bs)
    (flet ((strout (p s)
	     (let ((bytes (str-to-bytes s)))
	       (write-byte p bs)
	       (write-byte (length bytes) bs)
	       (write-sequence bytes bs))))
      (write-byte +isc-dpb-version1+ bs)
      (strout +isc-dpb-lc-ctype+ (string :utf8))
      (strout +isc-dpb-user-name+ (string :xxx))
      (strout +isc-dpb-password+ "536")
      ;;(strout +isc-dpb-password-enc+ (%crypt-password "536"))
      (when auth-data (strout +isc-dpb-specific-auth-data+ (bytes-to-hex auth-data)))
      )))


(defun fb-init-dpb-v1 (params)
  (with-byte-stream (bs)
    (flet ((strout (p s)
	     (let ((bytes (str-to-bytes s)))
	       (write-byte p bs)
	       (write-byte (length bytes) bs)
	       (write-sequence bytes bs)))
	   (u32out (p x)
	     (write-byte p bs) (write-byte 4 bs)
	     (nibbles:write-ub32/le x bs)))
      (write-byte +isc-dpb-version1+ bs)
      (u32out +isc-dpb-process-id+ (%getpid))
      (strout +isc-dpb-process-name+ (%getprocname))
      (let ((charset (getf params :charset :none)))
	(strout +isc-dpb-lc-ctype+ (string charset)))
      (let ((user (getf params :user)))
	(strout +isc-dpb-user-name+ user))
      (let ((password (getf params :password)))
	(when password (strout +isc-dpb-password+ password)))
      (let ((password (getf params :password-enc)))
	(when password (strout +isc-dpb-password-enc+ (%crypt-password password))))
      (let ((role (getf params :role)))
	(when role (strout +isc-dpb-sql-role-name+ (string role))))
      (let ((auth-data (getf params :auth-data)))
	(when auth-data (strout +isc-dpb-specific-auth-data+ (bytes-to-hex auth-data)))
	))))


(defun fb-send-channel (wp b &optional (flush t))
  (let ((ln (length b)))
    (if (> ln 0)
	(with-slots (buffer stream stream-cypher-send bytes-out socket) wp
	  (when stream-cypher-send
	    (setf b (copy-seq b))
	    (crypto:encrypt-in-place stream-cypher-send b))
	  (write-sequence b stream)
	  (incf bytes-out ln)
	  (if flush
	      (force-output stream)
	      (incf (wire-protocol-lazy-count wp)))
	  (values ln))
	(values 0))))


(defun fb-recv-channel (wp nbytes &optional word-alignment)
  (declare (type fixnum nbytes) (type boolean word-alignment))
  (if (zerop nbytes)
      #()
      (with-slots (buffer stream stream-cypher-recv bytes-in socket) wp
	(let* ((n nbytes) (pad (mod n 4)))
	  (when (and word-alignment (not (zerop pad))) (incf n (- 4 pad)))
	  (let ((b (if (> n (length buffer))
		       (make-array n :element-type 'nibbles:octet)
		       (subseq! buffer 0 n)))) ; displace buffer
	    (when (/= n (read-sequence b stream))
	      (setf socket nil)
	      (error "Unexpected end of stream: ~a" stream))
	    ;; must copy, because next call destructs buffer
	    (setf b (make-array n :initial-contents b :element-type 'nibbles:octet))
	    (when stream-cypher-recv
	      (crypto:decrypt-in-place stream-cypher-recv b))
	    (incf bytes-in n)
	    ;; XXX: what's the best way to cut padding off?
	    (values (if (= n nbytes) b (subseq b 0 nbytes))))))))


(defun fb-recv-int32 (wp)
  (with-slots ((b buffer4) stream stream-cypher-recv bytes-in socket) wp
    (let ((num (if stream-cypher-recv
		   (progn
		     (when (/= 4 (read-sequence b stream))
		       (setf socket nil)
		       (error "Unexpected end of stream: ~a" stream))
		     (crypto:decrypt-in-place stream-cypher-recv b)
		     (let ((x (crypto:octets-to-integer b)))
		       (if (< x (ash 1 31)) x (- x (ash 1 32)))))
		   (nibbles:read-sb32/be stream))))
      (incf bytes-in 4)
      (values num))))


(defun fb-parse-status-vector (wp)
  (let ((sql-code 0) (gds-code 0) (gds-codes nil)
	(message "") (num-arg 0)
	(n (fb-recv-int32 wp)))
    (loop
       (when (= n +isc-arg-end+) (return))
       (case n
	 (#.+isc-arg-gds+
	  (setf gds-code (fb-recv-int32 wp))
	  (unless (zerop gds-code)
	    (pushnew gds-code gds-codes)
	    (string+= message (gethash gds-code *messages* "@1"))
	    (setf num-arg 0)))
	 (#.+isc-arg-number+
	  (let ((num (fb-recv-int32 wp)))
	    (when (= gds-code 335544436)
	      (setf sql-code num))
	    (incf num-arg)
	    (let ((part (format nil "@~a" num-arg))
		  (strnum (format nil "~a" num)))
	      (setf message (replace-all message part strnum)))))
	 (#.+isc-arg-string+
	  (let ((s (bytes-to-str (fb-recv-channel wp (fb-recv-int32 wp) t))))
	    (incf num-arg)
	    (setf message (replace-all message (format nil "@~a" num-arg) s))))
	 (#.+isc-arg-interpreted+
	  (let ((s (bytes-to-str (fb-recv-channel wp (fb-recv-int32 wp) t))))
	    (string+= message s)))
	 (#.+isc-arg-sql-state+
	  (fb-recv-channel wp (fb-recv-int32 wp) t))) ; case
       (setf n (fb-recv-int32 wp)))		      ; end loop
    (values gds-codes sql-code message)))


(defun fb-parse-op-response (wp)
  (let* ((h (fb-recv-int32 wp))
	 (oid (fb-recv-channel wp 8))
	 (buf (fb-recv-channel wp (fb-recv-int32 wp) t)))
    (multiple-value-bind (gds-codes sql-code message)
	(fb-parse-status-vector wp)
      (cond
	((intersection gds-codes '(335544838 335544879 335544880 335544466 335544665 335544347 335544558))
	 (error 'integrity-error :msg message :gds gds-codes :sql sql-code))
	((intersection gds-codes '(335544321))
	 ;; arithmetic exception, numeric overflow, or string truncation
	 (error 'operational-error :msg message :gds gds-codes :sql sql-code))
	((and (or (not (zerop sql-code)) (not (string= "" message)))
	      (not (intersection gds-codes '(335544434))))
	 (error 'operational-error :msg message :gds gds-codes :sql sql-code))))
    (values h oid buf)))


(defun fb-attach (&key crypt (plugin :srp256))
  (declare (ignorable crypt))
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :protocol :tcp :type :stream))
	 (plugin-list "Srp256,Srp,Legacy_Auth")
	 stream cnct-params specific-data packet op-code client-public-key client-private-key
	 plugin-name auth-data-1 accept-version accept-type wp crypt-plugin crypt-key)
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) 3050)
    (setf stream
	  (sb-bsd-sockets:socket-make-stream
	   socket :element-type '(unsigned-byte 8) :output t :input t))
    (ecase plugin
      ((:srp :srp256)
       (setf plugin-name (string plugin))
       (multiple-value-bind (public-key private-key) (client-seed)
	 (setf client-public-key public-key client-private-key private-key)
	 (setf specific-data (long-to-hex public-key))))
      ((:legacy :legacy-auth)
       (setf plugin :legacy)
       (setf plugin-name "Legacy_Auth")
       (setf specific-data (%crypt-password "536"))))
    (print (cons :specific-data-len (length (str-to-bytes specific-data))))
    (setf cnct-params
 	  (make-bytes (%pack-cnct-param +cnct-login+         (str-to-bytes "XXX"))
		      (%pack-cnct-param +cnct-plugin-name+   (str-to-bytes plugin-name))
		      (%pack-cnct-param +cnct-plugin-list+   (str-to-bytes plugin-list))
		      (%pack-cnct-param +cnct-specific-data+ (str-to-bytes specific-data))
		      (if (eq plugin :legacy)
			  (%pack-cnct-param +cnct-client-crypt+  #(0 0 0 0))
			  (%pack-cnct-param +cnct-client-crypt+  (if crypt #(1 0 0 0) #(0 0 0 0))))
		      (%pack-cnct-param +cnct-user+          (str-to-bytes "KZverev"))
		      (%pack-cnct-param +cnct-host+          (str-to-bytes "LM-IT01G"))
		      (%pack-cnct-param +cnct-user-verification+ #())))
    (setf packet
	  (with-byte-stream (s)
	    (xdr-int32 +op-connect+)
	    (xdr-int32 +op-attach+)
	    (xdr-int32 +connect-version3+)
	    (xdr-int32 +arch-generic+)
	    ;;(xdr-string "")
	    (xdr-string "\\xxx\\db\\ZXA3789.FDB")
	    (xdr-int32 4)
	    (xdr-octets cnct-params)
	    (write-sequence #.(%encode-proto +protocol-version10+ 1 5 5 1) s)
	    (write-sequence #.(%encode-proto +protocol-version11+ 1 5 5 2) s)
	    (write-sequence #.(%encode-proto +protocol-version12+ 1 5 5 3) s)
	    (write-sequence #.(%encode-proto +protocol-version13+ 1 5 5 4) s)))

    (unwind-protect
	 (progn
	   (print :connect)
	   (write-sequence packet stream) ; XXX count bytes-out
	   (finish-output stream)
	   (print :connect-response)
	   (setf op-code (nibbles:read-sb32/be stream)) ; XXX count bytes-in
	   (print (cons :op-code op-code))

	   (when (= op-code +op-reject+) (error "Connection rejected."))
	     
	   (setf accept-version (ldb (byte 8 0) (nibbles:read-sb32/be stream)))
	   (nibbles:read-sb32/be stream) ; accept-architecture
	   (setf accept-type (nibbles:read-sb32/be stream))
	   (print (list :accept-version accept-version
			:accept-type accept-type))
	   
	   (setf wp (wire-protocol-instance socket stream accept-version accept-type))
	   
	   (case op-code
	     (#.+op-accept+
	      (print :legacy))
	     ;; if crypt => +op-cond-accept+ else +op-accept-data+
	     ((#.+op-cond-accept+ #.+op-accept-data+)
	      (let* ((ldata (fb-recv-int32 wp))
		     (data (fb-recv-channel wp ldata t))
		     (lapn (fb-recv-int32 wp))
		     (apn (fb-recv-channel wp lapn t))
		     (accept-plugin (%kw (bytes-to-str apn)))
		     (is-authenticated (fb-recv-int32 wp))
		     (lkeys (fb-recv-int32 wp))
		     (keys (fb-recv-channel wp lkeys t)))
		;;(print data)
		(print (cons :plugin accept-plugin))
		(print (cons :is-authenticated is-authenticated))
		(print (cons :keys keys))

		(when (= is-authenticated 0)
		  (when (zerop (length data))
		    (error "Server requires an `~a' plugin." accept-plugin))
		  
		  (let* ((ln (bytes-to-long-le (subseq data 0 2)))
			 (server-salt (subseq data 2 (+ ln 2)))
			 (b1 (subseq data (+ ln 4)))
			 (b2 (bytes-to-str b1))
			 (b3 (hex-to-long b2))
			 (server-public-key b3))
		    (multiple-value-bind (auth-data session-key)
			(client-proof (str-to-bytes "XXX")
				      (str-to-bytes "536")
				      server-salt
				      client-public-key
				      server-public-key
				      client-private-key
				      (if (eq accept-plugin :srp256) :sha256 :sha1))
		      ;;(print (cons :auth-data auth-data))
		      ;;(print (cons :session-key session-key))
		      (setf auth-data-1 auth-data)

		      ;; CONT-AUTH
		      (when crypt
			(setf packet
			      (with-byte-stream (s)
				(xdr-int32 +op-cont-auth+)
				(xdr-string (bytes-to-hex auth-data))
				(xdr-octets apn)
				(xdr-octets (str-to-bytes plugin-list))
				(xdr-octets keys)))
			(print :cont-auth)
			;;(print packet)
			(fb-send-channel wp packet)
			(print :cont-auth-response)
			(setf op-code (fb-recv-int32 wp))
			(print (cons :op-code op-code))
			(assert (= op-code +op-response+))
			(multiple-value-bind (h oid buf)
			    (fb-parse-op-response wp)
			  (print h) (print oid) (print buf)
			  (unless (zerop (length buf))
			    ;; buf (0 len <plugin> 1 len <key>)
			    (let ((len1 (aref buf 1)))
			      (setf crypt-key (bytes-to-str (subseq buf 2 (+ 2 len1))))
			      (setf crypt-plugin (bytes-to-str (subseq buf (+ 4 len1)))))))
			(setf packet
			      (with-byte-stream (s)
				(xdr-int32 +op-crypt+)
				(xdr-string crypt-plugin)
				(xdr-string crypt-key)))
			(print :crypt)
			(fb-send-channel wp packet)
			(print :crypt-response)
			(setf (slot-value wp 'stream-cypher-recv)
			      (crypto:make-cipher :arcfour :key session-key :mode :stream)
			      (slot-value wp 'stream-cypher-send)
			      (crypto:make-cipher :arcfour :key session-key :mode :stream))
			;; crypted
			(setf op-code (fb-recv-int32 wp))
			(print (cons :op-code op-code))
			(assert (= op-code +op-response+))
			(multiple-value-bind (h oid buf)
			    (fb-parse-op-response wp)
			  (print h) (print oid) (print buf))
			)		; when crypt
		      ))))))		; case


	   ;; ATTACH
	   (setf packet (with-byte-stream (s)
			  (xdr-int32 +op-attach+)
			  (xdr-int32 0)	; Database Object ID
			  (xdr-string "\\xxx\\db\\ZXA3789.FDB")
			  (xdr-octets (fb-create-dpb-v1 auth-data-1))))
	   (print :attach)
	   ;;(print packet)
	   (fb-send-channel wp packet)
	   (print :attach-response)
	   (setf op-code (fb-recv-int32 wp))
	   (print (cons :op-code op-code))

	   (when (= op-code +op-cont-auth+)
	     (error "Unauthorized."))
	   
	   (assert (= op-code +op-response+))
	   (multiple-value-bind (h oid buf)
	       (fb-parse-op-response wp)
	     (print h)
	     (print oid)
	     (print buf))

	   (describe wp)
	   
	   (setf packet
		 (with-byte-stream (s)
		   (xdr-int32 +op-transaction+)
		   (xdr-int32 0)       ; db-handle
		   (xdr-octets (vector ; ISOLATION-LEVEL-REPEATABLE-READ
				+isc-tpb-version3+
				+isc-tpb-write+
				+isc-tpb-wait+
				+isc-tpb-concurrency+))))
	   (print :transaction)
	   (fb-send-channel wp packet)
	   (print :transaction-response)
	   (setf op-code (fb-recv-int32 wp))
	   (print (cons :op-code op-code))
	   (assert (= op-code +op-response+))
	   (multiple-value-bind (h oid buf)
	       (fb-parse-op-response wp)
	     (print h)
	     (print oid)
	     (print buf))

	   ;;(sleep 10)
	   
	   nil)
      (when stream (close stream))
      (sb-bsd-sockets:socket-close socket))))








;;; =========================================================================
(defun fb-attach/legacy ()
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :protocol :tcp :type :stream))
	 stream cnct-params packet op-code accept-version accept-type wp)
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) 3050)
    (setf stream
	  (sb-bsd-sockets:socket-make-stream
	   socket :element-type '(unsigned-byte 8) :output t :input t))
    (setf cnct-params
 	  (make-bytes
	   (%pack-cnct-param +cnct-login+         (str-to-bytes "XXX"))
	   (%pack-cnct-param +cnct-user+          (str-to-bytes "KZverev"))
	   (%pack-cnct-param +cnct-host+          (str-to-bytes "LM-IT01G"))))
    (setf packet (with-byte-stream (s)
		   (xdr-int32 +op-connect+)
		   (xdr-int32 +op-attach+)
		   (xdr-int32 +connect-version2+)
		   (xdr-int32 +arch-generic+)
		   ;;(xdr-string "\\xxx\\db\\ZXA3789.FDB")
		   (xdr-string "")
		   (xdr-int32 1)
		   (xdr-octets cnct-params)
		   (write-sequence (%encode-proto +protocol-version10+ 1 5 5 2) s)))

    (unwind-protect
	 (progn
	   (print :out)
	   (print packet)
	   (write-sequence packet stream)
	   (finish-output stream)
	   (print :in)
	   (setf op-code (nibbles:read-sb32/be stream))
	   ;; +op-cond-accept+ +op-accept-data+ +op-accept+ +op-reject+ +op-response+
	   (print (cons :op-code op-code))

	   (setf accept-version (ldb (byte 8 0) (nibbles:read-sb32/be stream)))
	   (nibbles:read-sb32/be stream) ; accept-architecture
	   (setf accept-type (nibbles:read-sb32/be stream))

	   (print (list :accept-version accept-version
			:accept-type accept-type))

	   (assert (= op-code +op-accept+))

	   (setf wp (wire-protocol-instance socket stream accept-version accept-type))

	   (describe wp)
	   
	   (setf packet (with-byte-stream (s)
			  (xdr-int32 +op-attach+)
			  (xdr-int32 0) ; Database Object ID
			  (xdr-string "\\xxx\\db\\ZXA3789.FDB")
			  (xdr-octets (fb-create-dpb-v1))))
	   (print :out)
	   (print packet)
	   (write-sequence packet stream)
	   (finish-output stream)
	   (print :in)
	   (setf op-code (nibbles:read-sb32/be stream))
	   (print (cons :op-code op-code))
	   (assert (/= op-code +op-cont-auth+))
	   (assert (= op-code +op-response+))
	   (multiple-value-bind (h oid buf)
	       (fb-parse-op-response stream)
	     (print h)
	     (print oid)
	     (print buf))

	   nil)
      (when stream (close stream))
      (sb-bsd-sockets:socket-close socket))))






(defun fb-op-connect (stream socket database user password crypt plugin)
  (declare (ignorable crypt plugin))
  (let* ((plugin-list "Srp256,Srp,Legacy_Auth")
	 cnct-params specific-data packet op-code client-public-key client-private-key
	 plugin-name accept-version accept-type wp crypt-plugin crypt-key)
    (ecase plugin
      ((:srp :srp256)
       (setf plugin-name (string-capitalize (string plugin)))
       (multiple-value-bind (public-key private-key) (client-seed)
	 (setf client-public-key public-key client-private-key private-key)
	 (setf specific-data (long-to-hex public-key))))
      ((:legacy :legacy-auth)
       (setf plugin :legacy)
       (setf plugin-name "Legacy_Auth")
       (setf specific-data (%crypt-password password))))
    (setf cnct-params
 	  (make-bytes (%pack-cnct-param +cnct-login+         (str-to-bytes user))
		      (%pack-cnct-param +cnct-plugin-name+   (str-to-bytes plugin-name))
		      (%pack-cnct-param +cnct-plugin-list+   (str-to-bytes plugin-list))
		      (%pack-cnct-param +cnct-specific-data+ (str-to-bytes specific-data))
		      (unless (eq plugin :legacy)
			(%pack-cnct-param +cnct-client-crypt+
					  (if crypt #(1 0 0 0) #(0 0 0 0))))
		      (%pack-cnct-param +cnct-user+          (str-to-bytes (%get-username)))
		      (%pack-cnct-param +cnct-host+          (str-to-bytes (%get-hostname)))
		      (%pack-cnct-param +cnct-user-verification+ #())))
    (setf packet
	  (with-byte-stream (s)
	    (xdr-int32 +op-connect+)
	    (xdr-int32 +op-attach+)
	    (xdr-int32 +connect-version3+)
	    (xdr-int32 +arch-generic+)
	    (xdr-string (or database ""))
	    (xdr-int32 4)		; protocol's count
	    (xdr-octets cnct-params)
	    (write-sequence #.(%encode-proto +protocol-version10+ 1 5 5 1) s)
	    (write-sequence #.(%encode-proto +protocol-version11+ 1 5 5 2) s)
	    (write-sequence #.(%encode-proto +protocol-version12+ 1 5 5 3) s)
	    (write-sequence #.(%encode-proto +protocol-version13+ 1 5 5 4) s)))

    (write-sequence packet stream)	; XXX: count bytes-out
    (finish-output stream)
    (setf op-code (nibbles:read-sb32/be stream)) ; XXX: count bytes-in
    (when (= op-code +op-reject+)
      (error 'operational-error :msg "Connection is rejected"))
    (when (= op-code +op-response+)
      (fb-parse-op-response (make-instance 'wire-protocol-10 :stream stream)) ; error
      (error 'operational-error :msg "Connection response")) ; not reached
    (setf accept-version (ldb (byte 8 0) (nibbles:read-sb32/be stream)))
    (nibbles:read-sb32/be stream)	; accept-architecture
    (setf accept-type (nibbles:read-sb32/be stream))
    (setf wp (wire-protocol-instance socket stream accept-version accept-type))
	   
    (case op-code
      (#.+op-accept+)
      ;; if crypt => +op-cond-accept+ else +op-accept-data+
      ((#.+op-cond-accept+ #.+op-accept-data+)
       (let* ((ldata (fb-recv-int32 wp))
	      (data (fb-recv-channel wp ldata t))
	      (lapn (fb-recv-int32 wp))
	      (apn (fb-recv-channel wp lapn t))
	      (accept-plugin (%kw (bytes-to-str apn)))
	      (is-authenticated (fb-recv-int32 wp))
	      (lkeys (fb-recv-int32 wp))
	      (keys (fb-recv-channel wp lkeys t)))
	 (when (= is-authenticated 0)
	   (when (zerop (length data))
	     (error "Server requires an `~a' plugin." accept-plugin))
	   (let* ((ln (bytes-to-long-le (subseq data 0 2)))
		  (server-salt (subseq data 2 (+ ln 2)))
		  (b1 (subseq data (+ ln 4)))
		  (b2 (bytes-to-str b1))
		  (b3 (hex-to-long b2))
		  (server-public-key b3))
	     (multiple-value-bind (auth-data session-key)
		 (client-proof (str-to-bytes user)
			       (str-to-bytes password)
			       server-salt
			       client-public-key
			       server-public-key
			       client-private-key
			       (if (eq accept-plugin :srp256) :sha256 :sha1))
	       (setf (slot-value wp 'auth-data) auth-data)

	       (when crypt
		 (setf packet
		       (with-byte-stream (s)
			 (xdr-int32 +op-cont-auth+)
			 (xdr-string (bytes-to-hex auth-data))
			 (xdr-octets apn)
			 (xdr-octets (str-to-bytes plugin-list))
			 (xdr-octets keys)))
		 (fb-send-channel wp packet)
		 (loop (setf op-code (fb-recv-int32 wp))
		    (when (/= op-code +op-dummy+) (return)))
		 (assert (= op-code +op-response+))
		 (multiple-value-bind (h oid buf)
		     (fb-parse-op-response wp)
		   (declare (ignore h oid))
		   (unless (zerop (length buf))
		     ;; buf (0 len <plugin> 1 len <key>)
		     (let ((len1 (aref buf 1)))
		       (setf crypt-key (bytes-to-str (subseq buf 2 (+ 2 len1))))
		       (setf crypt-plugin (bytes-to-str (subseq buf (+ 4 len1)))))))
		 (setf packet
		       (with-byte-stream (s)
			 (xdr-int32 +op-crypt+)
			 (xdr-string crypt-plugin)
			 (xdr-string crypt-key)))
		 (fb-send-channel wp packet)
		 (setf (slot-value wp 'stream-cypher-recv)
		       (crypto:make-cipher :arcfour :key session-key :mode :stream)
		       (slot-value wp 'stream-cypher-send)
		       (crypto:make-cipher :arcfour :key session-key :mode :stream))
		 ;; crypted
		 (loop (setf op-code (fb-recv-int32 wp))
		    (when (/= op-code +op-dummy+) (return)))
		 (assert (= op-code +op-response+))
		 (fb-parse-op-response wp)) ; when crypt
	       ))))))			    ; case
    (values wp)))


(defun fb-op-connect/legacy (stream socket)
  (let* (cnct-params packet op-code accept-version accept-type)
    (setf cnct-params
 	  (make-bytes
	   (%pack-cnct-param +cnct-user+  (str-to-bytes (%get-username)))
	   (%pack-cnct-param +cnct-host+  (str-to-bytes (%get-hostname)))))
    (setf packet (with-byte-stream (s)
		   (xdr-int32 +op-connect+)
		   (xdr-int32 +op-attach+)
		   (xdr-int32 +connect-version2+)
		   (xdr-int32 +arch-generic+)
		   (xdr-string "")	; database
		   (xdr-int32 1)
		   (xdr-octets cnct-params)
		   (write-sequence #.(%encode-proto +protocol-version10+ 1 5 5 1) s)))

    (write-sequence packet stream)
    (finish-output stream)
    (setf op-code (nibbles:read-sb32/be stream))
    (when (= op-code +op-reject+) (error "Connection rejected."))
    (setf accept-version (ldb (byte 8 0) (nibbles:read-sb32/be stream)))
    (nibbles:read-sb32/be stream)	; accept-architecture
    (setf accept-type (nibbles:read-sb32/be stream))
    (assert (= op-code +op-accept+))
    (values (wire-protocol-instance socket stream accept-version accept-type))))


(defun attach (database host user password &rest args &key (port 3050) &allow-other-keys)
  "Attach to a `database' at `host' with `user' and `password'.
Keys supported:
`role' (default none), `charset' (default none), `version' (default 3.0),
`plugin' (default :srp256), `crypt' (default T) "
  (check-type database string)
  (check-type password string)
  (check-type port (integer 1 65535))
  (setf user (%convert-username (string user)))
  (multiple-value-bind (stream socket) (fb-socket-connect host port)
    (handler-bind ((error (lambda (e) (declare (ignore e))
				  #+sbcl (sb-bsd-sockets:socket-close socket)
				  #-sbcl (usocket:socket-close s))))
      (let* ((version (getf args :version 3.0))
	     (wp (if (< version 3.0)
		     (fb-op-connect/legacy stream socket)
		     (fb-op-connect stream socket database user password
				    (getf args :crypt t)
				    (getf args :plugin :srp256))))
	     (a (make-instance 'attachment :protocol wp))
	     packet op-code)
	(setf (getf args :user) user)
	(setf (getf args :auth-data) (wire-protocol-auth-data wp))
	(when (< (wire-protocol-version wp) 13)
	  (if (= (wire-protocol-version wp) 10)
	      (setf (getf args :password) password)
	      (setf (getf args :password-enc) password)))
	
	(setf packet (with-byte-stream (s)
		       (xdr-int32 +op-attach+)
		       (xdr-int32 0)	; Database Object ID
		       (xdr-string database)
		       (xdr-octets (fb-init-dpb-v1 args))))
	(fb-send-channel wp packet)
	(loop (setf op-code (fb-recv-int32 wp))
	   (when (/= op-code +op-dummy+) (return)))
	(when (= op-code +op-cont-auth+)
	  (error 'operational-error :msg "Unauthorized."))
	(assert (= op-code +op-response+))
	(setf (slot-value a 'handle) (fb-parse-op-response wp))
	(setf (slot-value wp 'auth-data) nil)

	(values a)))))


(defun detach (attachment)
  (let* ((socket (wire-protocol-socket (attachment-protocol attachment)))
	 (result (when socket
		   #+sbcl (sb-bsd-sockets:socket-close socket)
		   #-sbcl (usocket:socket-close socket))))
    (setf (slot-value (attachment-protocol attachment) 'socket) nil)
    (values result)))


(defun fb-op-dummy (wp)
  (let (op-code)
    (loop (setf op-code (fb-recv-int32 wp))
       (when (/= op-code +op-dummy+) (return op-code)))))


(defun fb-op-lazy (wp op-code)
  (loop
     :while (and (= op-code +op-response+)
		 (> (wire-protocol-lazy-count wp) 0))
     :do (decf (wire-protocol-lazy-count wp))
     :do (handler-case
	     (fb-parse-op-response wp)
	   (operational-error (e)
	     (warn "~aGDS-CODES: ~a~%SQL-CODE: ~a"
		   (error-message e)
		   (error-gds-codes e)
		   (error-sql-code e))))
     :do (setf op-code (fb-recv-int32 wp)))
  (values op-code))


(defun fb-op-response (wp)
  (log:debug wp)
  (finish-output (slot-value wp 'stream))
  (let (op-code)
    (setf op-code (fb-op-dummy wp))
    (setf op-code (fb-op-lazy wp op-code))
    (when (= op-code +op-cont-auth+)
      (error 'operational-error :msg "Unauthorized"))
    (when (/= op-code +op-response+)
      (error "InternalError: op-response:op_code = ~a" op-code))
    (multiple-value-bind (h oid buf)
	(fb-parse-op-response wp)
      (values h oid buf))))



(defparameter +isolation-level+
  (list :read-committed-legacy
	(vector
	 +isc-tpb-version3+
	 +isc-tpb-write+
	 +isc-tpb-wait+
	 +isc-tpb-read-committed+
	 +isc-tpb-no-rec-version+)
	:read-committed
	(vector
	 +isc-tpb-version3+
	 +isc-tpb-write+
	 +isc-tpb-wait+
	 +isc-tpb-read-committed+
	 +isc-tpb-rec-version+)
	:repeatable-read
	(vector
	 +isc-tpb-version3+
	 +isc-tpb-write+
	 +isc-tpb-wait+
	 +isc-tpb-concurrency+)
	:snapshot			; same as :repeatable-read
	(vector
	 +isc-tpb-version3+
	 +isc-tpb-write+
	 +isc-tpb-wait+
	 +isc-tpb-concurrency+)
	:serializable
	(vector
	 +isc-tpb-version3+
	 +isc-tpb-write+
	 +isc-tpb-wait+
	 +isc-tpb-consistency+)
	:read-committed-ro
	(vector
	 +isc-tpb-version3+
	 +isc-tpb-read+
	 +isc-tpb-wait+
	 +isc-tpb-read-committed+
	 +isc-tpb-rec-version+)))


(defparameter +tpb-map+
  (list
   :consistency      +isc-tpb-consistency+
   :concurrency      +isc-tpb-concurrency+
   :shared           +isc-tpb-shared+
   :protected        +isc-tpb-protected+
   :exclusive        +isc-tpb-exclusive+
   :wait             +isc-tpb-wait+
   :nowait           +isc-tpb-nowait+
   :read             +isc-tpb-read+
   :write            +isc-tpb-write+
   :lock-read        +isc-tpb-lock-read+
   :lock-write       +isc-tpb-lock-write+
   :verb-time        +isc-tpb-verb-time+
   :commit-time      +isc-tpb-commit-time+
   :ignore-limbo     +isc-tpb-ignore-limbo+
   :read-committed   +isc-tpb-read-committed+
   :autocommit       +isc-tpb-autocommit+
   :rec-version      +isc-tpb-rec-version+
   :no-rec-version   +isc-tpb-no-rec-version+
   :restart-requests +isc-tpb-restart-requests+
   :no-auto-undo     +isc-tpb-no-auto-undo+
   :lock-timeout     +isc-tpb-lock-timeout+))


;; example:
;; (set-transaction *a* (gen-tpb :concurrency :write :nowait :lock-write "T1" :protected))
(defun gen-tpb (&rest params)
  (with-byte-stream (s)
    (write-byte +isc-tpb-version3+ s)
    (loop :for p :in params
       :do (etypecase p
	     (string
	      (let ((b (str-to-bytes p)))
		(write-byte (length b) s)
		(write-sequence b s)))
	     (symbol
	      (write-byte (or (getf +tpb-map+ p)
			      (error "Unknown TPB parameter: ~a" p)) s))))))


;; +op-commit+
;; +op-prepare+
;; +op-rollback+
;; +op-unwind+
;; +op-release+
;; +op-close-blob+
;; +op-cancel-blob+
;; +op-detach+
;; +op-dr+op-database+
;; +op-service-detach+
;; +op-commit-retaining+
;; +op-rollback-retaining+
;; +op-allocate-statement+
(defun fb-release-object (wp handle op)
  (declare (type (unsigned-byte 32) handle)
	   (type (unsigned-byte 8) op))
  (let ((packet
	 (with-byte-stream (s)
	   (xdr-int32 op)
	   (xdr-int32 handle))))
    (if (wire-protocol-lazy-p wp)
	(prog1 -1 (fb-send-channel wp packet nil))
	(progn
	  (fb-send-channel wp packet)
	  (fb-op-response wp)))))


(defun set-transaction (attachment tpb &key auto-commit)
  (when (typep tpb 'symbol)
    (let ((v (getf +isolation-level+ tpb)))
      (unless v (error "Unknows isolation level: ~a" tpb))
      (setf tpb v)))
  (when (and auto-commit (not (find +isc-tpb-autocommit+ tpb)))
    (setf tpb (concatenate 'vector tpb #(#.+isc-tpb-autocommit+))))
  (when (attachment-transaction attachment)
    (block b1
      (handler-bind
	  ((operational-error
	    (lambda (e)
	      ;; invalid transaction handle
	      (when (member 335544332 (error-gds-codes e))
		(return-from b1)))))
	(fb-release-object (attachment-protocol attachment)
			   (attachment-transaction attachment)
			   +op-rollback+)))
    (setf (slot-value attachment 'trans) nil))
  (let ((packet
	 (with-byte-stream (s)
	   (xdr-int32 +op-transaction+)
	   (xdr-int32 (object-handle attachment))
	   (xdr-octets (coerce tpb 'vector)))))
    (fb-send-channel (attachment-protocol attachment) packet))
  (let ((handle (fb-op-response (attachment-protocol attachment))))
    (setf (slot-value attachment 'trans) handle)
    (values handle)))


(defun check-transaction (attachment)
  (unless (attachment-transaction attachment)
    (set-transaction attachment
		     (or (attachment-isolation-level attachment) :read-commited)
		     :auto-commit (attachment-auto-commit attachment))))


(defun commit* (attachment)
  (when (attachment-transaction attachment)
    (let ((h (fb-release-object (attachment-protocol attachment)
				(attachment-transaction attachment)
				+op-commit+)))
      (setf (slot-value attachment 'trans) nil)
      (or (unless (zerop (wire-protocol-lazy-count (attachment-protocol attachment)))
	    (decf (wire-protocol-lazy-count (attachment-protocol attachment)))
	    (prog1 (fb-op-response (attachment-protocol attachment))))
	  h))))


(defun rollback* (attachment)
  (when (attachment-transaction attachment)
    (let ((h (fb-release-object (attachment-protocol attachment)
				(attachment-transaction attachment)
				+op-rollback+)))
      (setf (slot-value attachment 'trans) nil)
      (or (unless (zerop (wire-protocol-lazy-count (attachment-protocol attachment)))
	    (decf (wire-protocol-lazy-count (attachment-protocol attachment)))
	    (prog1 (fb-op-response (attachment-protocol attachment))))
	  h))))


(defmethod (setf attachment-isolation-level) :before (value (attachment attachment))
  (unless (eql (attachment-isolation-level attachment) value)
    (rollback* attachment)))
	   

(defun fb-op-open-blob (attachment blob-id)
  (let ((packet (with-byte-stream (s)
		  (xdr-int32 +op-open-blob+)
		  (xdr-int32 (attachment-transaction attachment))
		  (write-sequence blob-id *xdr*))))
    (fb-send-channel (attachment-protocol attachment) packet))
  (multiple-value-bind (blob-handle)
      (fb-op-response (attachment-protocol attachment))
    (values blob-handle)))


(defun fb-op-create-blob2 (attachment &optional bpb)
  (let ((packet (with-byte-stream (s)
		  (xdr-int32 +op-create-blob2+)
		  (xdr-octets (or bpb #()))
		  (xdr-int32 (attachment-transaction attachment))
		  (xdr-int32 0)
		  (xdr-int32 0))))
    (fb-send-channel (attachment-protocol attachment) packet))
  (multiple-value-bind (blob-handle blob-id)
      (fb-op-response (attachment-protocol attachment))
    (values blob-handle blob-id)))


(defun fb-op-get-segment (wp blob-handle)
  (let ((packet (with-byte-stream (s)
		  (xdr-int32 +op-get-segment+)
		  (xdr-int32 blob-handle)
		  (xdr-int32 65535)
		  (xdr-octets #()))))
    (fb-send-channel wp packet))
  (multiple-value-bind (n oid buf)
      (fb-op-response wp)
    (declare (ignore oid))
    (values n buf)))


(defun fb-op-put-segment (wp blob-handle data)
  (let* ((ln (length data))
	 (packet (with-byte-stream (s)
		   (xdr-int32 +op-put-segment+)
		   (xdr-int32 blob-handle)
		   (xdr-int32 ln)
		   (xdr-octets data))))
    (fb-send-channel wp packet))
  (fb-op-response wp))


(defun fb-op-batch-segments (wp blob-handle data)
  (let* ((ln (length data))
	 (ln2 (+ ln 2))
	 (pad (pad-4-bytes ln2))
	 (packet (with-byte-stream (s)
		  (xdr-int32 +op-batch-segments+)
		  (xdr-int32 blob-handle)
		  (xdr-int32 ln2)
		  (xdr-int32 ln2)
		  (write-sequence (long-to-bytes ln 2) s)
		  (write-sequence data s)
		  (write-sequence pad s))))
    (fb-send-channel wp packet))
  (fb-op-response wp))


(defun fb-create-blob (attachment data &optional storage)
  (let ((bpb (make-bytes
	      +isc-bpb-version1+
	      +isc-bpb-type+ +isc-bpb-type-segmented+
	      +isc-bpb-storage+ (if storage
				    +isc-bpb-storage-main+
				    +isc-bpb-storage-temp+)))
	(wp (attachment-protocol attachment)))
    (flet ((put-data (blob-handle blob-id)
	     (loop :with i = 0 :with blen = (length data)
		:for j = (+ i *blob-segment-size*)
		:while (< i blen)
		:do (fb-op-put-segment wp blob-handle (subseq! data i (min j blen)))
		:do (incf i *blob-segment-size*))
	     (values blob-id)))
      (check-transaction attachment)
      (multiple-value-bind (blob-handle blob-id)
	  (fb-op-create-blob2 attachment bpb)
	(unwind-protect
	     (put-data blob-handle blob-id)
	  (fb-release-object wp blob-handle +op-close-blob+))))))
  

(defun fb-blob-contents (attachment blob-id)
  (check-transaction attachment)
  (let ((val (byte-stream))
	(wp (attachment-protocol attachment))
	(h (fb-op-open-blob attachment blob-id)))
    (unwind-protect
	 (loop (multiple-value-bind (n buf) (fb-op-get-segment wp h)
		 (loop (when (zerop (length buf)) (return))
		    (let ((ln (bytes-to-long-le (subseq buf 0 2))))
		      (append-bytes val (subseq! buf 2 (+ 2 ln)))
		      (setf buf (subseq! buf (+ 2 ln)))))
		 (when (= n 2) (return))))
      (fb-release-object wp h +op-close-blob+))
    (values (byte-stream-output val))))


(defun blob-contents (attachment blob)
  (fb-blob-contents attachment (blob-id blob)))


;; +op-info-blob+
;; +op-info-database+
;; +op-info-request+
;; +op-info-transaction+
;; +op-service-info+
;; +op-info-sql+
(defun fb-info-request (attachment operation object items &key incarnation)
  (let (packet)
    (setf packet
	  (with-byte-stream (s)
	    (xdr-int32 operation)
	    (xdr-int32 object)
	    (xdr-int32 (or incarnation 0))
	    (xdr-octets items)
	    (xdr-int32 +wp-buffer-length+)))
    (fb-send-channel (attachment-protocol attachment) packet))
  (multiple-value-bind (h oid buf)
      (fb-op-response (attachment-protocol attachment))
    (declare (ignore h oid))
    (values buf)))


(defun blob-info (attachment blob)
  (when (typep blob 'blob) (setf blob (blob-id blob)))
  (check-transaction attachment)
  (let* ((h (fb-op-open-blob attachment blob))
	 (items (make-bytes +isc-info-blob-num-segments+
			    +isc-info-blob-max-segment+
			    +isc-info-blob-total-length+
			    +isc-info-blob-type+))
	 (buf (fb-info-request attachment +op-info-blob+ h items)))
    (fb-release-object (attachment-protocol attachment) h
		       +op-close-blob+)
    ;; XXX: parse buf
    (values buf)))
    
  
(defun fb-row-count (attachment handle &optional select-p)
  (let ((buf (fb-info-request attachment +op-info-sql+ handle
			      (make-bytes +isc-info-sql-records+))))
    (assert (equalp (subseq buf 0 3) #(#x17 #x1d 0)))
    (let ((count (if select-p
		     (progn (assert (equalp (subseq buf 17 20) #(#x0d #x04 0)))
			    (bytes-to-long-le (subseq buf 20 24)))
		     (+ (bytes-to-long-le (subseq buf 27 31))
			(bytes-to-long-le (subseq buf 6 10))
			(bytes-to-long-le (subseq buf 13 17))))))
      (values count))))


(defun fb-parse-select-items/2 (buf xsqlda)
  (let ((i 0) (index 0) (item (elt buf 0)))
    (flet ((get-item (&optional x)
	     (let* ((l (bytes-to-long-le (subseq! buf (+ i 1) (+ i 3))))
		    (item (case x
			    (:str (bytes-to-str (subseq! buf (+ i 3) (+ i 3 l))))
			    (:int (bytes-to-int-le (subseq! buf (+ i 3) (+ i 3 l))))
			    (otherwise (bytes-to-long-le (subseq! buf (+ i 3) (+ i 3 l)))))))
	       (incf i (+ 3 l)) item)))
      (macrolet ((with-slots! ((&rest slots) obj &body body)
		     (let ((sl (loop :for s :in slots
				  :collect (list s (list 'slot-value obj (list 'quote s))))))
		       `(symbol-macrolet ,sl ,@body))))
	(loop
	   (unless (/= item +isc-info-end+) (return -1))
	   (with-slots! (sqltype sqlsubtype sqlscale sqllen
				 null-ok fieldname relname
				 ownname aliasname)
	       (elt xsqlda (1- index))
	     (case item
	       (#.+isc-info-sql-sqlda-seq+
		(setf index (get-item))
		(setf (elt xsqlda (1- index))
		      (make-instance 'xsqlvar :use-unicode nil)))
	       (#.+isc-info-sql-type+
		(setf sqltype (logand (get-item) (lognot 1)))) ; clear bit 0
	       (#.+isc-info-sql-sub-type+ (setf sqlsubtype (get-item)))
	       (#.+isc-info-sql-scale+    (setf sqlscale   (get-item :int)))
	       (#.+isc-info-sql-length+   (setf sqllen     (get-item)))
	       (#.+isc-info-sql-null-ind+ (setf null-ok    (> (get-item) 0)))
	       (#.+isc-info-sql-field+    (setf fieldname  (get-item :str)))
	       (#.+isc-info-sql-relation+ (setf relname    (get-item :str)))
	       (#.+isc-info-sql-owner+    (setf ownname    (get-item :str)))
	       (#.+isc-info-sql-alias+    (setf aliasname  (get-item :str)))
	       (#.+isc-info-truncated+    (return index)) ; return next index
	       (#.+isc-info-sql-describe-end+ (incf i))
	       (otherwise
		(log:warn "Invalid item: ~a ~a" (elt buf i) i)
		(incf i))))
	   (setf item (elt buf i))))))) ; loop


(defun fb-parse-xsqlda (buf attachment handle)
  (let (xsqlda stmt-type next-index l col-len (i 0) (buflen (length buf)))
    (loop
       (unless (< i buflen) (return))
       (cond
	 ((equalp (subseq! buf i (+ 3 i)) #(#.+isc-info-sql-stmt-type+ 4 0))
	  (setf stmt-type (bytes-to-long-le (subseq! buf (+ i 3) (+ i 7))))
	  (incf i 7))
	 ((equalp (subseq! buf i (+ 2 i)) #(#.+isc-info-sql-select+ #.+isc-info-sql-describe-vars+))
	  (incf i 2)
	  (setf l (bytes-to-long-le (subseq! buf i (+ i 2))))
	  (incf i 2)
	  (setf col-len (bytes-to-long-le (subseq! buf i (+ i l))))
	  (setf xsqlda (make-list col-len))
	  (setf next-index (fb-parse-select-items/2 (subseq! buf (+ i l)) xsqlda))
	  (loop
	     (unless (> next-index 0) (return))
	     (break "fb-parse-xsqlda") ; never been here, not tested
	     (let* ((vars (make-bytes +isc-info-sql-sqlda-start+ 2
				      (long-to-bytes-le next-index 2)
				      +info-sql-select-describe-vars+))
		    (buf (fb-info-request attachment +op-info-sql+ handle vars)))
	       (assert (equalp (subseq! buf 0 2) #(4 7)))
	       (setf l (bytes-to-long-le (subseq! buf 2 4)))
	       (assert (= (bytes-to-long-le (subseq! buf 4 (+ 4 l))) col-len))
	       (setf next-index (fb-parse-select-items/2 (subseq! buf (+ 4 l)) xsqlda)))))
	 (t (return))))
    (values stmt-type xsqlda)))


(defun fb-op-free-statement (wp handle &optional mode)
  (unless mode (setf mode +dsql-drop+))
  (let (packet)
    (setf packet (with-byte-stream (s)
		   (xdr-int32 +op-free-statement+)
		   (xdr-int32 handle)
		   (xdr-int32 mode)))
    (if (wire-protocol-lazy-p wp)
	(fb-send-channel wp packet nil)
	(progn
	  (fb-send-channel wp packet)
	  (fb-op-response wp)))))


(defun prepare* (attachment sql &key explain-plan)
  (check-transaction attachment)
  (let (handle plan packet desc-items)
    (setf handle			; `allocate' with `release'
	  (fb-release-object (attachment-protocol attachment)
			     (object-handle attachment)
			     +op-allocate-statement+))
    (setf desc-items
	  (make-bytes (if explain-plan +isc-info-sql-get-plan+)
		      +isc-info-sql-stmt-type+
		      +info-sql-select-describe-vars+))
    (setf packet (with-byte-stream (s)
		   (xdr-int32 +op-prepare-statement+)
		   (xdr-int32 (attachment-transaction attachment))
		   (xdr-int32 handle)
		   (xdr-int32 3)	; dialect = 3
		   (xdr-string sql)
		   (xdr-octets desc-items)
		   (xdr-int32 +wp-buffer-length+)))
    (fb-send-channel (attachment-protocol attachment) packet)
    (unless (zerop (wire-protocol-lazy-count (attachment-protocol attachment)))
      (decf (wire-protocol-lazy-count (attachment-protocol attachment)))
      (setf handle (fb-op-response (attachment-protocol attachment))))
    (multiple-value-bind (h oid buf)
	(fb-op-response (attachment-protocol attachment))
      (declare (ignore h oid))
      (let ((i 0) l)
	(when (= (elt buf i) +isc-info-sql-get-plan+)
	  (setf l (bytes-to-long-le (subseq! buf (1+ i) (+ i 3))))
	  (setf plan (bytes-to-str (subseq! buf (+ i 3) (+ i 3 l))))
	  (incf i (+ 3 l)))
	(multiple-value-bind (stmt-type xsqlda)
	    (fb-parse-xsqlda (subseq! buf i) attachment handle)
	  (values handle stmt-type xsqlda plan))))))


(defun fb-params-null-indicator (params)
  (let ((null-indicator 0))
    (loop :for i :from 0
       :for p :in params
       :do (setf (ldb (byte 1 i) null-indicator) (if (eq p :null) 1 0)))
    (values null-indicator)))


(defmethod fb-params-to-blr/vls ((wp wire-protocol-10) params)
  (declare (ignore params))
  (make-bytes))


(defmethod fb-params-to-blr/vls ((wp wire-protocol-13) params)
  (let* ((plen (length params))
	 (null-indicator (fb-params-null-indicator params))
	 (n (floor plen 8))
	 null-indicator-bytes)
    (when (/= 0 (mod plen 8)) (incf n))
    (when (/= 0 (mod n 4)) (incf n (- 4 (mod n 4))))
    (loop :for i :from 0 to (1- n)
       :do (push (logand null-indicator #xff) null-indicator-bytes)
       :do (setf null-indicator (ash null-indicator -8)))
    (log:trace null-indicator-bytes)
    (make-bytes (nreverse null-indicator-bytes))))


(defmethod fb-params-to-blr/null ((wp wire-protocol-10) vals is-null)
  (append-bytes vals (if is-null #(255 255 255 255) #(0 0 0 0))))


(defmethod fb-params-to-blr/null ((wp wire-protocol-13) vals is-null)
  :do-nothing)


(defun fb-params-to-blr/2 (attachment params)
  "Convert parameter array to BLR and values format."
  (let ((ln (* 2 (length params)))
	(blr (byte-stream))
	(vls (byte-stream))
	(wp (attachment-protocol attachment)))
    (append-bytes blr 5 2 4 0 (ldb (byte 8 0) ln) (ldb (byte 8 8) ln))
    (append-bytes vls (fb-params-to-blr/vls wp params))
    (loop :for p :in params
       :with v = (vector)
       :do (cond
	     ;; XXX: add date/time types converters
	     ;; XXX: add float types converters
	     ((eq p :null)		; NULL
	      (setf v (vector)) (append-bytes blr 14 0 0))
	     ((typep p '(signed-byte 32)) ; LONG
	      (setf v (long-to-bytes p 4)) (append-bytes blr 8 0))
	     ((typep p '(signed-byte 64)) ; INT64
	      (setf v (long-to-bytes p 8)) (append-bytes blr 16 0))
	     ((stringp p)		; TEXT
	      (let* ((s (str-to-bytes p))
		     (slen (length s)))
		(if (> slen +max-char-length+)
		    (progn (setf v (fb-create-blob attachment s))
			   (append-bytes blr 9 0))
		    (progn (setf v (make-bytes s (pad-4-bytes slen)))
			   (append-bytes blr 14
					 (ldb (byte 8 0) slen)
					 (ldb (byte 8 8) slen))))))
	     ((vectorp p)		; BLOB
	      (setf v (fb-create-blob attachment p))
	      (append-bytes blr 9 0))
	     ((typep p 'blob)		; BLOB
	      (if (blob-id p)
		  (setf v (blob-id p))
		  (progn
		    (setf v (fb-create-blob attachment (blob-data p)))
		    (setf (slot-value p 'blob-id) v)))
	      (append-bytes blr 9 0))
	     ((eq p nil) (setf v #(0 0 0 0)) (append-bytes blr 23))
	     ((eq p t)   (setf v #(1 0 0 0)) (append-bytes blr 23))
	     (t
	      (let* ((s (str-to-bytes (str p)))
		     (nbytes (length s)))
		(setf v (make-bytes s (pad-4-bytes nbytes)))
		(append-bytes blr 14 (ldb (byte 8 0) nbytes)
			      (ldb (byte 8 8) nbytes))))) ; cond
       :do (append-bytes blr 7 0) 
       :do (append-bytes vls v)
       :do (fb-params-to-blr/null wp vls (eq p :null))
       :finally (append-bytes blr 255 76)) ; loop

    (let ((blr (byte-stream-output blr))
	  (vls (byte-stream-output vls)))
      (values blr vls))))


(defmethod fb-op-fetch-row ((wp wire-protocol-10) xsqlda)
  (let ((r (make-list (length xsqlda) :initial-element :null)))
    (loop :for x in xsqlda
       :for i :from 0
       :do (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (fb-recv-int32 wp) io-len))
		  (raw-value (fb-recv-channel wp ln t)))
	     (when (equalp #(0 0 0 0) (fb-recv-channel wp 4)) ; not NULL
	       (setf (elt r i) (xsqlvar-value x raw-value)))))
    (values r)))


(defun fb-make-null-indicator (wp xsqlda)
  (let ((n (floor (length xsqlda) 8))
	(null-indicator 0))
    (when (/= 0 (mod (length xsqlda) 8)) (incf n))
    (loop :for c :across (nreverse (fb-recv-channel wp n t))
       :do (setf null-indicator (ash null-indicator 8))
       :do (setf null-indicator (logior null-indicator c)))
    (values null-indicator)))


(defmethod fb-op-fetch-row ((wp wire-protocol-13) xsqlda)
  (let ((r (make-list (length xsqlda) :initial-element :null))
	(null-indicator (fb-make-null-indicator wp xsqlda)))
    (loop :for x in xsqlda
       :for i :from 0
       :do (when (zerop (logand null-indicator (ash 1 i)))
	     (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (fb-recv-int32 wp) io-len))
		  (raw-value (fb-recv-channel wp ln t)))
	       (setf (elt r i) (xsqlvar-value x raw-value)))))
    (values r)))


(defun fb-op-sql-response (wp xsqlda)
  (finish-output (slot-value wp 'stream))
  (let (op-code)
    (setf op-code (fb-op-dummy wp))
    (setf op-code (fb-op-lazy wp op-code))
    (unless (= op-code +op-sql-response+)
      (when (= op-code +op-response+)
	(fb-parse-op-response wp))
      (error "op_sql_response: op-code = ~a" op-code))
    (unless (zerop (fb-recv-int32 wp))
      (fb-op-fetch-row wp xsqlda))))


(defun fb-op-fetch (wp stmt-handle blr &optional count)
  (unless count (setf count *default-fetch-size*))
  (let ((packet (with-byte-stream (s)
		  (xdr-int32 +op-fetch+)
		  (xdr-int32 stmt-handle)
		  (xdr-octets blr)
		  (xdr-int32 0)
		  (xdr-int32 count))))
    (fb-send-channel wp packet))
  (values))


(defun fb-op-fetch-response (wp xsqlda)
  (finish-output (slot-value wp 'stream))
  (let (op-code)
    (setf op-code (fb-op-dummy wp))
    (setf op-code (fb-op-lazy wp op-code))
    (unless (= op-code +op-fetch-response+)
      (when (= op-code +op-response+)
	(fb-parse-op-response wp))
      (error "op_fetch_response: op-code = ~a" op-code))
    (let ((status (fb-recv-int32 wp))
	  (count (fb-recv-int32 wp))
	  rows)
      (loop
	 (when (zerop count) (return))
	 (push (fb-op-fetch-row wp xsqlda) rows)
	 (setf op-code (fb-recv-int32 wp)
	       status (fb-recv-int32 wp)
	       count (fb-recv-int32 wp)))
      (values (nreverse rows) (/= status 100)))))


(defun fb-fetch (wp handle xsqlda &optional count)
  (unless count (setf count *default-fetch-size*))
  (let ((packet (with-byte-stream (s)
		  (xdr-int32 +op-fetch+)
		  (xdr-int32 handle)
		  (xdr-octets (xsqlvar-calc-blr xsqlda))
		  (xdr-int32 0)
		  (xdr-int32 count))))
    (fb-send-channel wp packet))
  (finish-output (slot-value wp 'stream))
  (let (op-code)
    (setf op-code (fb-op-dummy wp))
    (setf op-code (fb-op-lazy wp op-code))
    (unless (= op-code +op-fetch-response+)
      (when (= op-code +op-response+)
	(fb-parse-op-response wp))
      (error "op_fetch_response: op-code = ~a" op-code))
    (let ((status (fb-recv-int32 wp))
	  (count (fb-recv-int32 wp))
	  rows)
      (loop
	 (when (zerop count) (return))
	 (push (fb-op-fetch-row wp xsqlda) rows)
	 (setf op-code (fb-recv-int32 wp)
	       status (fb-recv-int32 wp)
	       count (fb-recv-int32 wp)))
      (values (nreverse rows) (/= status 100)))))


(defun fb-execute (attachment handle xsqlda type params)
  (let (packet result h (op +op-execute+))
    (when (eq type :exec-procedure) (setf op +op-execute2+))
    (setf packet
	  (with-byte-stream (s)
	    (xdr-int32 op)
	    (xdr-int32 handle)
	    (xdr-int32 (if (eq type :start-trans)
			   0
			   (attachment-transaction attachment)))
	    (if (zerop (length params))
		(progn
		  (xdr-octets #())
		  (xdr-int32 0)
		  (xdr-int32 0))
		(multiple-value-bind (blr vals)
		    (fb-params-to-blr/2 attachment params)
		  (xdr-octets blr)
		  (xdr-int32 0)
		  (xdr-int32 1)
		  (write-sequence vals s)))
	    (when (eq type :exec-procedure)
	      (xdr-octets (xsqlvar-calc-blr xsqlda))
	      (xdr-int32 0))))
    (fb-send-channel (attachment-protocol attachment) packet)
    (when (eq type :exec-procedure)
      (setf result (fb-op-sql-response (attachment-protocol attachment) xsqlda)))
    (setf h (fb-op-response (attachment-protocol attachment))) ; XXX: what is it?
    (values result h)))

      
(defun query* (attachment sql &rest params)
  (multiple-value-bind (handle type xsqlda)
      (block b1
	(handler-bind
	    ((operational-error
	      (lambda (e)
		;; invalid transaction handle
		(when (member 335544332 (error-gds-codes e))
		  (setf (slot-value attachment 'trans) nil)
		  (return-from b1 (prepare* attachment sql :explain-plan nil))))))
	  (prepare* attachment sql :explain-plan nil)))
    (setf type (getf +stmt-type+ type :unknown))
    (setf params (%statement-convert-params params))
    (unwind-protect
	 (multiple-value-bind (result h)
	     (fb-execute attachment handle xsqlda type params)
	   (case type
	     ((:select :select-for-upd)
	      (setf result (make-instance 'cursor
					  :protocol (attachment-protocol attachment)
					  :handle handle
					  :xsqlda xsqlda)))
	     ((:commit :rollback)
	      (setf result t)
	      (setf (slot-value attachment 'trans) nil))
	     (:start-trans
	      (fbsql::fb-release-object (attachment-protocol attachment)
					(attachment-transaction attachment)
					+op-rollback+)
	      (setf (slot-value attachment 'trans) h)
	      (setf result h))
	     ((:insert :update :delete)
	      (setf result (fb-row-count attachment handle nil))))
	   (values result type))
      (unless (find type (list :select :select-for-upd))
	(ignore-errors
	  (fb-op-free-statement (attachment-protocol attachment)
				handle
				+dsql-drop+))))))


(defun execute/many (attachment sql params-list)
  (when params-list
    (multiple-value-bind (handle type xsqlda)
	(block b1
	  (handler-bind
	      ((operational-error
		(lambda (e)
		  ;; invalid transaction handle
		  (when (member 335544332 (error-gds-codes e))
		    (setf (slot-value attachment 'trans) nil)
		    (return-from b1 (prepare* attachment sql :explain-plan nil))))))
	    (prepare* attachment sql :explain-plan nil)))
      (setf type (getf +stmt-type+ type :unknown))
      (ecase type (:insert) (:update) (:delete) (:exec-procedure))
      (unwind-protect
	   (let (result)
	     (loop :for params :in params-list
		:for p = (%statement-convert-params params)
		:for r = (fb-execute attachment handle xsqlda type p)
		:do (if (eql type :exec-procedure)
			(push r result)
			(push (fb-row-count attachment handle nil) result)))
	     (values result type))
	(ignore-errors
	  (fb-op-free-statement (attachment-protocol attachment)
				handle
				+dsql-drop+))))))


(defun cursor-close (cursor)
  (if (object-handle cursor)
      (progn
	(fb-op-free-statement (cursor-protocol cursor) (object-handle cursor) +dsql-close+)
	(fb-op-free-statement (cursor-protocol cursor) (object-handle cursor) +dsql-drop+)
	(setf (slot-value cursor 'handle) nil)
	t)
      nil))
  

(defun cursor-fetch-row (cursor &key plist)
  (if (object-handle cursor)
      (multiple-value-bind (rows more-data)
	  (fb-fetch (cursor-protocol cursor)
		    (object-handle cursor)
		    (cursor-xsqlda cursor)
		    1)
	(unless more-data (cursor-close cursor))
	(let ((r (car rows)))
	  (when (and r plist)
	    (setf r (loop :for v :in r :for x :in (cursor-xsqlda cursor)
		       :collect (list (%kw (xsqlvar-aliasname x)) v)))
	    (setf r (flatten r)))
	  (values r more-data)))
      (error 'operational-error :msg "Cursor is closed.")))


(defun cursor-fetch-many (cursor &optional count)
  (if (object-handle cursor)
      (multiple-value-bind (rows more-data)
	  (fb-fetch (cursor-protocol cursor)
		    (object-handle cursor)
		    (cursor-xsqlda cursor)
		    (or count 1))
	(unless more-data (cursor-close cursor))
	(values rows more-data))
      (error 'operational-error :msg "Cursor is closed.")))


(defmacro do-query ((row attachment sql &rest params) &body body)
  (let ((a (gensym "ATT")) (s (gensym "SQL")) (c (gensym "CUR")))
    `(let* ((,a ,attachment)
	    (,s ,sql)
	    (,c (query* ,a ,s ,@params)))
       (typecase ,c
	 (cursor
	  (unwind-protect
	       (loop :for ,row = (cursor-fetch-row ,c)
		  :while ,row :do (progn ,@body))
	    (cursor-close ,c)))
	 (t ,c)))))
       

(defun execute* (attachment sql)
  "Execute SQL immediately (non-selective)."
  (let ((type
	 (let ((usql (string-upcase sql)))
	   (cond
	     ((search "COMMIT" usql :test #'char=) :trans-end)
	     ((and (search "ROLLBACK" usql :test #'char=)
		   (not (search " TO " usql :test #'char=))) :trans-end) ; savepoint
	     ((and
	       (search "SET" usql :test #'char=)
	       (search "TRANSACTION" usql :test #'char=))
	      :trans-begin)
	     (t :other)))))
    (unless (eq type :trans-begin)
      (check-transaction attachment))
    (let (packet)
      (setf packet
	    (with-byte-stream (s)
	      (xdr-int32 +op-exec-immediate+)
	      (xdr-int32 (if (eq type :trans-begin)
			     0
			     (attachment-transaction attachment)))
	      (xdr-int32 (object-handle attachment))
	      (xdr-int32 3)		; dialect = 3
	      (xdr-string sql)
	      (xdr-octets #())
	      (xdr-int32 +wp-buffer-length+)))
      (fb-send-channel (attachment-protocol attachment) packet))
    (let ((trans-handle (fb-op-response (attachment-protocol attachment))))
      (case type
	(:trans-begin
	 (when (attachment-transaction attachment)
	   (fbsql::fb-release-object (attachment-protocol attachment)
				     (attachment-transaction attachment)
				     +op-rollback+))
	 (setf (slot-value attachment 'trans) trans-handle))
	(:trans-end (setf (slot-value attachment 'trans) nil))
	(:other))
      (values trans-handle))))


(defun callproc* (attachment name &rest params)
  (let* ((p? (make-list (length params) :initial-element #\?))
	 (sql (format nil "EXECUTE PROCEDURE ~a ~{~a~^,~}" name p?))
	 (res (apply #'query* attachment sql params)))
    (values res)))


(defun fb-parse-trans-info (buf ireq)
  (let ((buflen (length buf))
	(i 0) (ir 0) req r l)
    (loop
       (unless (< i buflen) (return))
       (setf req (elt buf i))
       (when (= req +isc-info-end+) (return))
       (assert (or (= req (elt ireq ir))
		   (= req +isc-info-error+)))
       (setf l (bytes-to-long-le (subseq! buf (1+ i) (+ i 3))))
       (push (list req (elt ireq ir) (subseq! buf (+ i 3) (+ i 3 l))) r)
       (incf i (+ 3 l))
       (incf ir)) ; loop
    (values r)))


(defun fb-transaction-info (attachment info-requests)
  (setf info-requests (check-info-requests info-requests))
  (let* ((buf (fb-info-request attachment +op-info-transaction+
			       (attachment-transaction attachment)
			       info-requests))
	 (r (fb-parse-trans-info buf info-requests))
	 (res nil)
	 (tr-type (list 1 :snapshot-table-stability 2 :snapshot 3 :read-committed))
	 (tr-subtype (list 0 :no-record-version 1 :record-version)))
    (loop for (x y z) in r
       do (setf (getf res y)
		(cond
		  ((= x +isc-info-tra-isolation+)
		   (if (> (length z) 1)
		       (cons (getf tr-type (elt z 0)) (getf tr-subtype (elt z 1)))
		       (getf tr-type (elt z 0))))
		  ((= x +isc-info-error+) nil)
		  (t (bytes-to-int-le z)))))
    (values res)))


(defun trans-info (attachment)
  (check-transaction attachment)
  (let ((info (fb-transaction-info attachment +my-isc-info-tra-all+)))
    (loop :for i :from 0 :to (1- (length info)) :by 2
       :do (setf (elt info i) (getf +my-isc-info-tra-map+ (elt info i))))
    (values info)))


(defun fb-db-info (attachment info-requests)
  (setf info-requests (check-info-requests info-requests))
  (let ((r nil)
	(buf (fb-info-request attachment +op-info-database+
			      (object-handle attachment)
			      info-requests)))
    (loop :for (x y v) :in (%parse-db-info buf info-requests)
       ;; make p-list
       :do (push (if (/= x +isc-info-error+) (%db-info-convert-type y v)) r)
       :do (push y r))
    (values r)))


(defun db-info* (attachment)
  (let ((info (fb-db-info attachment +my-isc-info-all+)))
    (loop :for i :from 0 :to (1- (length info)) :by 2
       :do (setf (elt info i) (getf +my-isc-info-map+ (elt info i))))
    (values info)))


(defun list-all-tables* (attachment)
  "Returns a list of all user-defined relations."
  (let ((c (query* attachment "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS
WHERE RDB$SYSTEM_FLAG = 0 ORDER BY 1")) result)
    (loop
       (multiple-value-bind (r more)
	   (cursor-fetch-many c 200)
	 (setf result (nconc result r))
	 (unless more (return))))
    (values result)))


(defun list-all-procedures* (attachment)
  "Returns a list of all PSQL procedures."
  (let ((c (query* attachment "SELECT RDB$PROCEDURE_NAME
FROM RDB$PROCEDURES ORDER BY 1")) result)
    (loop
       (multiple-value-bind (r more)
	   (cursor-fetch-many c 200)
	 (setf result (nconc result r))
	 (unless more (return))))
    (values result)))



