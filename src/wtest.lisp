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
   (isolation-level :initform :read-commited :accessor attachment-isolation-level)
   ))
(defclass cursor ()
  ((protocol :initarg :protocol :reader cursor-protocol)
   (handle :initform nil :initarg :handle :reader object-handle)
   (xsqlda :initform nil :initarg :xsqlda :reader cursor-xsqlda)))


(defgeneric fb-params-to-blr/vls (wp params))
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


(defun socket-connect (host port)
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
       (cond
	 ((= +isc-arg-gds+ n)
	  (setf gds-code (fb-recv-int32 wp))
	  (unless (zerop gds-code)
	    (pushnew gds-code gds-codes)
	    (string+= message (gethash gds-code *messages* "@1"))
	    (setf num-arg 0)))
	 ((= +isc-arg-number+ n)
	  (let ((num (fb-recv-int32 wp)))
	    (when (= gds-code 335544436)
	      (setf sql-code num))
	    (incf num-arg)
	    (let ((part (format nil "@~a" num-arg))
		  (strnum (format nil "~a" num)))
	      (setf message (replace-all message part strnum)))))
	 ((= +isc-arg-string+ n)
	  (let* ((nbytes (fb-recv-int32 wp))
		 (s (bytes-to-str (fb-recv-channel wp nbytes t))))
	    (incf num-arg)
	    (let ((part (format nil "@~a" num-arg)))
	      (setf message (replace-all message part s)))))
	 ((= +isc-arg-interpreted+ n)
	  (let* ((nbytes (fb-recv-int32 wp))
		 (s (bytes-to-str (fb-recv-channel wp nbytes t))))
	    (string+= message s)))
	 ((= +isc-arg-sql-state+ n)
	  (let ((nbytes (fb-recv-int32 wp)))
	    (fb-recv-channel wp nbytes t)))) ; cond
       (setf n (fb-recv-int32 wp)))	; end loop
    (values gds-codes sql-code message)))


(defun fb-parse-op-response (wp)
  (let* ((b (fb-recv-channel wp 16))
	 (h (bytes-to-long (subseq b 0 4)))
	 (oid (subseq b 4 12))
	 (buf-len (bytes-to-long (subseq b 12)))
	 (buf (fb-recv-channel wp buf-len t)))
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
		      #+nil(%pack-cnct-param +cnct-user-verification+ #())))
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
    ;; XXX: (= op-code +op-response+) => (fb-parse-op-response wp)
    ;;      but no wp instance yet
    (when (= op-code +op-response+)
      (error 'operational-error :msg "Connection needs response"))
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
  (check-type database string)
  (check-type password string)
  (check-type port (integer 1 65535))
  (setf user (%convert-username (string user)))
  (multiple-value-bind (stream socket) (socket-connect host port)
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
      (error "InternalError: wp-op-response:op_code = ~a" op-code))
    (multiple-value-bind (h oid buf)
	(fb-parse-op-response wp)
      ;;(log:debug h oid buf)
      (values h oid buf))))



(defparameter +isolation-level+
  (list :read-commited-legacy (vector
			       +isc-tpb-version3+
			       +isc-tpb-write+
			       +isc-tpb-wait+
			       +isc-tpb-read-committed+
			       +isc-tpb-no-rec-version+)
	:read-commited (vector
			+isc-tpb-version3+
			+isc-tpb-write+
			+isc-tpb-wait+
			+isc-tpb-read-committed+
			+isc-tpb-rec-version+)
	:repeatable-read (vector
			  +isc-tpb-version3+
			  +isc-tpb-write+
			  +isc-tpb-wait+
			  +isc-tpb-concurrency+)
	:serializable (vector
		       +isc-tpb-version3+
		       +isc-tpb-write+
		       +isc-tpb-wait+
		       +isc-tpb-consistency+)
	:read-commited-ro (vector
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


(defun gen-tpb (&rest params)
  ;; XXX: need more work
  (with-byte-stream (s)
    (write-byte +isc-tpb-version3+ s)
    (mapcar (lambda (p)
	      (let ((pp (getf +tpb-map+ p)))
		(if pp
		    (write-byte pp s)
		    (error "Unknown TPB parameter: ~a" p))))
	    params)))


;; case op_commit:
;; case op_prepare:
;; case op_rollback:
;; case op_unwind:
;; case op_release:
;; case op_close_blob:
;; case op_cancel_blob:
;; case op_detach:
;; case op_drop_database:
;; case op_service_detach:
;; case op_commit_retaining:
;; case op_rollback_retaining:
;; case op_allocate_statement:
(defun fb-release-object (attachment handle op)
  (declare (type (unsigned-byte 32) handle)
	   (type (unsigned-byte 8) op))
  (let ((packet
	 (with-byte-stream (s)
	   (xdr-int32 op)
	   (xdr-int32 handle))))
    (fb-send-channel (attachment-protocol attachment) packet))
  (prog1 (fb-op-response (attachment-protocol attachment))))


(defun set-transaction (attachment tpb &key auto-commit)
  (when (typep tpb 'symbol)
    (let ((v (getf +isolation-level+ tpb)))
      (unless v (error "Unknows isolation level: ~a" tpb))
      (setf tpb v)))
  (when (and auto-commit (not (find +isc-tpb-autocommit+ tpb)))
    (setf tpb (concatenate 'vector tpb #(#.+isc-tpb-autocommit+))))
  (when (attachment-transaction attachment)
    (fb-release-object attachment (attachment-transaction attachment) +op-rollback+)
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
    (fb-release-object attachment (attachment-transaction attachment) +op-commit+)
    (setf (slot-value attachment 'trans) nil)))


(defun rollback* (attachment)
  (when (attachment-transaction attachment)
    (fb-release-object attachment (attachment-transaction attachment) +op-rollback+)
    (setf (slot-value attachment 'trans) nil)))






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
		    (packet (with-byte-stream (s)
			      (xdr-int32 +op-info-sql+)
			      (xdr-int32 handle)
			      (xdr-int32 0)
			      (xdr-octets vars)
			      (xdr-int32 +wp-buffer-length+))))
	       (fb-send-channel (attachment-protocol attachment) packet))
	     (multiple-value-bind (h oid buf)
		 (fb-op-response (attachment-protocol attachment))
	       (declare (ignore h oid))
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
	(fb-op-response wp))))


(defun prepare* (attachment sql &key explain-plan)
  (check-transaction attachment)
  (let (handle plan packet desc-items)
    (setf packet
	  (with-byte-stream (s)
	    (xdr-int32 +op-allocate-statement+)
	    (xdr-int32 (object-handle attachment))))
    (setf handle
	  (if (wire-protocol-lazy-p (attachment-protocol attachment))
	      (prog1 -1 (fb-send-channel (attachment-protocol attachment) packet nil))
	      (fb-op-response (attachment-protocol attachment))))
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


(defun fb-params-to-blr/2 (wp params)
  "Convert parameter array to BLR and values format."
  (let ((ln (* 2 (length params)))
	(blr (byte-stream))
	(vls (byte-stream)))
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
		    (error "nicht blob")
		    #+nil
		    (progn (setf v (%create-blob wp trans-handle s))
			   (append-bytes blr 9 0))
		    (progn (setf v (make-bytes s (pad-4-bytes slen)))
			   (append-bytes blr 14
					 (ldb (byte 8 0) slen)
					 (ldb (byte 8 8) slen))))))
	     ((vectorp p)		; BLOB
	      (error "nicht blob")
	      #+nil(setf v (%create-blob wp trans-handle p))
	      #+nil(append-bytes blr 9 0))
	     ((typep p 'blob)		; BLOB
	      (if (blob-id p)
		  (setf v (blob-id p))
		  (progn
		    (error "nicht blob")
		    #+nil(setf v (%create-blob wp trans-handle (blob-data p)))
		    #+nil(setf (slot-value p 'blob-id) v)))
	      (append-bytes blr 9 0))
	     ((eq p nil) (setf v #(0 0 0 0)) (append-bytes blr 23))
	     ((eq p t)   (setf v #(1 0 0 0)) (append-bytes blr 23))
	     (t
	      (log:trace "DEFAULT: string =>" p (type-of p))
	      (let* ((s (str-to-bytes (str p)))
		     (nbytes (length s)))
		(setf v (make-bytes s (pad-4-bytes nbytes)))
		(append-bytes blr 14 (ldb (byte 8 0) nbytes)
			      (ldb (byte 8 8) nbytes))))) ; cond
       :do (append-bytes blr 7 0) 
       :do (append-bytes vls v)
       :do (when (< (wire-protocol-version wp) 13)
	     (append-bytes vls (if (eq p :null) #(255 255 255 255) #(0 0 0 0))))
       :finally (append-bytes blr 255 76)) ; loop

    (let ((blr (byte-stream-output blr))
	  (vls (byte-stream-output vls)))
      (log:trace "%params-to-blr: ~a ~a" blr vls)
      (values blr vls))))


(defmethod fb-op-fetch-row ((wp wire-protocol-10) xsqlda)
  (let ((r (make-list (length xsqlda) :initial-element :null)))
    (loop :for x in xsqlda
       :for i :from 0
       :do (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (fb-recv-int32 wp) io-len))
		  (raw-value (fb-recv-channel wp ln t)))
	     ;;(log:trace (xsqlvar-aliasname x) raw-value)
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
	       ;;(log:trace (xsqlvar-aliasname x) raw-value)
	       (setf (elt r i) (xsqlvar-value x raw-value)))))
    (values r)))


(defun fb-op-sql-response (wp xsqlda)
  (finish-output (slot-value wp 'stream))
  (let (op-code)
    (setf op-code (fb-op-dummy wp))
    (setf op-code (fb-op-lazy wp op-code))
    (unless (= op-code +op-sql-response+)
      (when (= op-code +op-response+)
	(%parse-op-response wp))
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


(defun query* (attachment sql &rest params)
  (multiple-value-bind (handle type xsqlda)
      (prepare* attachment sql :explain-plan nil)
    (setf type (getf +stmt-type+ type :unknown))
    (setf params (%statement-convert-params params))
    (let (packet result (op +op-execute+) h)
      (when (eq type :exec-procedure)
	(setf op +op-execute2+))
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
		      (fb-params-to-blr/2 (attachment-protocol attachment) params)
		    (log:debug "blr vals: ~a ~a" blr vals)
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
      (case type
	((:select :select-for-upd)
	 (setf result (make-instance 'cursor
				     :protocol (attachment-protocol attachment)
				     :handle handle
				     :xsqlda xsqlda)))
	((:commit :rollback)
	 (setf (slot-value attachment 'trans) nil))
	(:start-trans
	 (fbsql::fb-release-object
	  attachment
	  (attachment-transaction attachment)
	  +op-rollback+)
	 (setf (slot-value attachment 'trans) h)))
      (unless (find type (list :select :select-for-upd))
	(fb-op-free-statement (attachment-protocol attachment) handle +dsql-drop+))
      (values result type))))


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
      (progn
	(fb-op-fetch (cursor-protocol cursor)
		     (object-handle cursor)
		     (xsqlvar-calc-blr (cursor-xsqlda cursor))
		     1)			; count
	(multiple-value-bind (rows more-data)
	    (fb-op-fetch-response (cursor-protocol cursor) (cursor-xsqlda cursor))
	  (unless more-data (cursor-close cursor))
	  (let ((r (car rows)))
	    (when (and r plist)
	      (setf r (loop :for v :in r :for x :in (cursor-xsqlda cursor)
			 :collect (list (%kw (xsqlvar-aliasname x)) v)))
	      (setf r (flatten r)))
	    (values r more-data))))
      (error 'operational-error :msg "Cursor is closed.")))


(defmacro do-query ((row attachment sql &rest params) &body body)
  (let ((a (gensym "ATT")) (s (gensym "SQL")) (c (gensym "CUR")))
    `(let* ((,a ,attachment)
	    (,s ,sql)
	    (,c (query* ,a ,s ,@params)))
       (typecase ,c
	 (cursor (loop :for ,row = (cursor-fetch-row ,c)
		    :while ,row :do (progn ,@body)))
	 (t ,c)))))
       





