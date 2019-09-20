(in-package #:cl-firebird)

;;;;;;;;;;;;;;;;;;


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


#+nil
(defun fb-recv-channel (stream nbytes &optional alignment)
  (if (zerop nbytes)
      #()
      (let* ((n nbytes) (pad (mod n 4)))
	(when (and alignment (not (zerop pad))) (incf n (- 4 pad)))
	(let ((b (make-array n :element-type 'nibbles:octet)))
	  (when (/= n (read-sequence b stream))
	    (error "Unexpected end of stream: ~a" stream))
	  (values (if (= n nbytes) b (subseq b 0 nbytes)))))))

(defun fb-send-channel (wp b &optional (flush t))
  (when (> (length b) 0)
    ;;(log:trace "RAW: ~a #x~a" (length b) (bytes-to-hex b))
    (let ((s (slot-value wp 'stream)))
      (when (slot-value wp 'stream-cypher-send)
	(setf b (copy-seq b))
	(crypto:encrypt-in-place (slot-value wp 'stream-cypher-send) b)
	#+nil(log:trace "ENCRYPTED: ~a #x~a" (length b) (bytes-to-hex b)))
      (write-sequence b s)
      (incf (slot-value wp 'bytes-out) (length b))
      (if flush
	  (force-output s)
	  (incf (wire-protocol-lazy-count wp)))))
  (values))


(defun fb-recv-channel (wp nbytes &optional word-alignment)
  (declare (type fixnum nbytes) (type boolean word-alignment))
  (if (zerop nbytes)
      #()
      (with-slots (buffer stream stream-cypher-recv) wp
	(let* ((n nbytes) (pad (mod n 4)))
	  (when (and word-alignment (not (zerop pad))) (incf n (- 4 pad)))
	  (let ((b (if (> n (length buffer))
		       (make-array n :element-type 'nibbles:octet)
		       (subseq! buffer 0 n)))) ; displace buffer
	    ;; XXX: usocket:wait-for-input !!!
	    (when (/= n (read-sequence b stream))
	      (error "Unexpected end of stream: ~a" stream))
	    ;; must copy, because next call destructs buffer
	    (setf b (make-array n :initial-contents b :element-type 'nibbles:octet))
	    (log:trace "RAW: ~a ~a~%#x~a"
	  	       (length b) b (bytes-to-hex b))
	    (when stream-cypher-recv
	      (crypto:decrypt-in-place stream-cypher-recv b)
	      (log:trace "DECRYPTED: ~a ~a~%#x~a"
	    		 (length b) b (bytes-to-hex b)))
	    (incf (slot-value wp 'bytes-in) n)
	    ;; XXX: what's the best way to cut padding off?
	    (values (if (= n nbytes) b (subseq b 0 nbytes))))))))


(defun fb-recv-int32 (wp)
  (with-slots ((b buffer4) stream stream-cypher-recv bytes-in) wp
    (let ((num (if stream-cypher-recv
		   (progn
		     (when (/= 4 (read-sequence b stream))
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
      ;;(print :status-vector)
      ;;(print gds-codes) (print sql-code) (print message)
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


(defclass wire-protocol-base ()
  ((socket :initarg :socket :reader wire-protocol-socket)
   (stream :initarg :stream :reader wire-protocol-stream)
   (stream-cypher-recv :initform nil)
   (stream-cypher-send :initform nil)
   (lazy-count :initform 0 :accessor wire-protocol-lazy-count)
   (accept-type :initform #.+ptype-batch-send+ :initarg :accept-type :reader wire-protocol-accept-type)
   (buffer :initform (make-array 65536 :element-type 'nibbles:octet))
   (buffer4 :initform (make-array 4 :element-type 'nibbles:octet))
   (bytes-in :initform 0 :reader wire-protocol-bytes-in)
   (bytes-out :initform 0 :reader wire-protocol-bytes-out)
   (start-time :initform (get-universal-time) :reader wire-protocol-start-time)
   ))
(defclass wire-protocol-10 (wire-protocol-base) ())
(defclass wire-protocol-11 (wire-protocol-10) ())
(defclass wire-protocol-12 (wire-protocol-11) ())
(defclass wire-protocol-13 (wire-protocol-12) ())


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


