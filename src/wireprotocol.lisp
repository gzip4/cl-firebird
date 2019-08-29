
(in-package #:cl-firebird)


(defconstant +wp-buffer-length+ 65535)
(defvar *default-fetch-size* 200)
(defvar *blob-segment-size* 32000)


#|
    def recv_channel(self, nbytes, word_alignment=False):
        n = nbytes
        if word_alignment and (n % 4):
            n += 4 - nbytes % 4  # 4 bytes word alignment
        r = bs([])
        while n:
            if (self.timeout is not None and select.select([self.sock._sock], [], [], self.timeout)[0] == []):
                break
            b = self.sock.recv(n)
            if not b:
                break
            r += b
            n -= len(b)
        if len(r) < nbytes:
            raise OperationalError('Can not recv() packets')
        return r[:nbytes]
|#

(defun send-channel (wp b)
  (when (> (length b) 0)
    (log:trace "RAW: ~a #x~a" (length b) (bytes-to-hex b))
    (let ((s (slot-value wp 'stream)))
      (when (slot-value wp 'stream-cypher-send)
	(setf b (copy-seq b))
	(ironclad:encrypt-in-place (slot-value wp 'stream-cypher-send) b)
	(log:trace "ENCRYPTED: ~a #x~a" (length b) (bytes-to-hex b)))
      (write-sequence b s)
      (force-output s)))
  (values))
    

(defun recv-channel (wp nbytes &optional word-alignment)
  (declare (type fixnum nbytes) (type boolean word-alignment))
  (if (zerop nbytes)
      (vector)
      (let ((n nbytes))
	(when (and word-alignment (not (zerop (mod n 4))))
	  (incf n (- 4 (mod n 4))))
	(let ((b (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
	  #+nil
	  (let ((s (usocket:wait-for-input (slot-value wp 'socket) :timeout 1)))
	    (log:info s))
	  #+nil(log:info (usocket:socket-state (slot-value wp 'socket)))
	  ;; XXX: usocket:wait-for-input !!!
	  (when (/= n (read-sequence b (slot-value wp 'stream)))
	    (error "Unexpected end of stream: ~a" (slot-value wp 'stream)))
	  (log:trace "RAW: ~a ~a~%#x~a ~a"
	  	     (length b) b (bytes-to-hex b) (bytes-to-long b))
	  (when (slot-value wp 'stream-cypher-recv)
	    (ironclad:decrypt-in-place (slot-value wp 'stream-cypher-recv) b)
	    (log:trace "DECRYPTED: ~a ~a~%#x~a ~a"
	    	       (length b) b (bytes-to-hex b) (bytes-to-long b)))
	  (values (subseq b 0 nbytes))))))


(declaim (inline %recv-int32))

(defun %recv-int32 (wp)
  ;; XXX: (u)int32 ?
  (bytes-to-long (recv-channel wp 4)))


(defun %pack-cnct-param (k v)
  (flex:with-output-to-sequence (s)
    (if (/= k +cnct-specific-data+)
	(progn (write-byte k s)
	       (write-byte (length v) s)
	       (write-sequence v s))
	(loop :for c :in (split-sequence-chunks v 254)
	   :for i :from 0
	   :do (progn (write-byte k s)
		      (write-byte (1+ (length c)) s)
		      (write-byte i s)
		      (write-sequence c s))))))


(defun %get-hostname ()
  (or (machine-instance) "localhost"))


(defun %my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list* :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro   (sys:getenv name)
   #+CLISP     (ext:getenv name)
   #+ECL       (si:getenv name)
   #+SBCL      (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   #+CCL       (ccl:getenv name)
   default))


(defun %get-username ()
  ;; os-macos?
  #+os-unix(%my-getenv "USER" "user")
  #+os-windows(%my-getenv "USERNAME" "user"))


(defun wp-uid (wp auth-plugin wire-crypt)
  (log:trace wp)
  (let ((auth-plugin-list "Srp256,Srp,Legacy_Auth")
	(user (%get-hostname))
	(hostname (%get-username))
	(specific-data nil)
	(client-crypt nil))
    (cond
      ((member auth-plugin '(:Srp256 :Srp))
       (multiple-value-bind (A$ a)
	   (client-seed)
	 (setf (slot-value wp 'client-public-key) A$
	       (slot-value wp 'client-private-key) a
	       specific-data (long-to-hex A$))))
      ((member auth-plugin '(:Legacy-Auth :legacy))
       (setf auth-plugin "legacy_auth")
       (setf specific-data (subseq (crypt:crypt (slot-value wp 'password) "9z") 2)))
      (t (error 'operational-error
		:msg (format nil "Unknown auth plugin name '~a'" auth-plugin))))
    
    (setf (slot-value wp 'plugin-name) (string-capitalize auth-plugin))
    (setf (slot-value wp 'plugin-list) auth-plugin-list)
    (setf client-crypt (if wire-crypt #(1 0 0 0) #(0 0 0 0)))

    (setf (slot-value wp 'plugin-list*) (list :srp256 :srp :legacy-auth))

    (let ((cnct-params
	   (make-bytes (%pack-cnct-param +cnct-login+         (str-to-bytes (slot-value wp 'user)))
		       (%pack-cnct-param +cnct-plugin-name+   (str-to-bytes (slot-value wp 'plugin-name)))
		       (%pack-cnct-param +cnct-plugin-list+   (str-to-bytes (slot-value wp 'plugin-list)))
		       (%pack-cnct-param +cnct-specific-data+ (str-to-bytes specific-data))
		       (%pack-cnct-param +cnct-client-crypt+  client-crypt)
		       (%pack-cnct-param +cnct-user+          (str-to-bytes user))
		       (%pack-cnct-param +cnct-host+          (str-to-bytes hostname))
		       (%pack-cnct-param +cnct-user-verification+ #()))))
      (log:trace cnct-params)
      (values cnct-params))))


#+nil
(defun %encode-proto/2 (version arch-type min max weigth)
  (assert (<= (integer-length version) 16))
  (let ((proto #xffffffffffffffffffffffffffffffffffffffff))
    (when (= 1 (ldb (byte 1 15) version))
      (setf version (logior #xffff0000 version)))
    (setf (ldb (byte 32 128) proto) version
	  (ldb (byte 32 96)  proto) arch-type
	  (ldb (byte 32 64)  proto) min
	  (ldb (byte 32 32)  proto) max
	  (ldb (byte 32 0)   proto) weigth)
    (values (long-to-bytes proto 20) proto)))

(defun %encode-proto (version arch-type min max weigth)
  (when (= 1 (ldb (byte 1 15) version))
    (setf version (logior #xffff0000 version)))
  (with-byte-stream (p)
    (nibbles:write-ub32/be version p)
    (nibbles:write-ub32/be arch-type p)
    (nibbles:write-ub32/be min p)
    (nibbles:write-ub32/be max p)
    (nibbles:write-ub32/be weigth p)))


(defparameter +protocols+
  (list
   (%encode-proto +protocol-version10+ 1 2 5 2)
   (%encode-proto +protocol-version11+ 1 2 5 4)
   (%encode-proto +protocol-version12+ 1 2 5 6)
   (%encode-proto +protocol-version13+ 1 2 5 8)))
  

(defun wp-op-connect (wp auth-plugin wire-crypt)
  (log:debug wp)
  (let ((packet
	 (make-bytes
	  (with-xdr
	    (xdr-int32 +op-connect+)
	    (xdr-int32 +op-attach+)
	    (xdr-int32 3)		; CONNECT_VERSION
	    (xdr-int32 1)		; arch_generic
	    (xdr-string (or (slot-value wp 'filename) ""))
	    (xdr-int32 (length +protocols+))
	    (xdr-octets (wp-uid wp auth-plugin wire-crypt)))
	  (apply #'make-bytes +protocols+))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun %parse-status-vector (wp)
  ;;(log:debug wp)
  (let ((sql-code 0)
	(gds-code 0)
	(gds-codes nil)
	(message "")
	(num-arg 0)
	(n (%recv-int32 wp)))
    (loop
       (when (= n +isc-arg-end+) (return))
       (cond
	 ((= +isc-arg-gds+ n)
	  (setf gds-code (%recv-int32 wp))
	  (unless (zerop gds-code)
	    (pushnew gds-code gds-codes)
	    (string+= message (gethash gds-code *messages* "@1"))
	    (setf num-arg 0)))

	 ((= +isc-arg-number+ n)
	  (let ((num (%recv-int32 wp)))
	    (when (= gds-code 335544436)
	      (setf sql-code (unsigned-to-signed-int num)))
	    (incf num-arg)
	    (let ((part (format nil "@~a" num-arg))
		  (strnum (format nil "~a" (unsigned-to-signed-int num))))
	      (setf message (replace-all message part strnum)))))

	 ((= +isc-arg-string+ n)
	  (let* ((nbytes (%recv-int32 wp))
		 (s (bytes-to-str (recv-channel wp nbytes t))))
	    (incf num-arg)
	    (let ((part (format nil "@~a" num-arg)))
	      (setf message (replace-all message part s)))))

	 ((= +isc-arg-interpreted+ n)
	  (let* ((nbytes (%recv-int32 wp))
		 (s (bytes-to-str (recv-channel wp nbytes t))))
	    (string+= message s)))

	 ((= +isc-arg-sql-state+ n)
	  (let ((nbytes (%recv-int32 wp)))
	    (recv-channel wp nbytes t))))

       (setf n (%recv-int32 wp))) ; end loop
    
    (values gds-codes sql-code message)))


(defun %parse-op-response (wp)
  ;;(log:debug wp)
  (let* ((b (recv-channel wp 16))
	 (h (bytes-to-long (subseq b 0 4)))
	 (oid (subseq b 4 12))
	 (buf-len (bytes-to-long (subseq b 12)))
	 (buf (recv-channel wp buf-len t)))
    (multiple-value-bind (gds-codes sql-code message)
	(%parse-status-vector wp)
      (cond
	((intersection gds-codes '(335544838 335544879 335544880 335544466 335544665 335544347 335544558))
	 (error 'integrity-error :msg message :gds gds-codes :sql sql-code))
	((intersection gds-codes '(335544321))
	 #+nil(error 'operational-error :msg message :gds gds-codes :sql sql-code)
	 ;; XXX: must catch warning if one dont want to break protocol
	 (warn message))
	((and (or (not (zerop sql-code)) (not (string= "" message)))
	      (not (intersection gds-codes '(335544434))))
	 (error 'operational-error :msg message :gds gds-codes :sql sql-code))))
    (values h oid buf)))


(defun %skip-op-dummy (wp)
  (loop :for b = (%recv-int32 wp)
     :while (= b +op-dummy+)
     :do (progn)
     :finally (return b)))


(defun %skip-lazy-response (wp op-code)
  (loop
     :while (and (= op-code +op-response+)
		 (> (slot-value wp 'lazy-response-count) 0))
     :do (progn (decf (slot-value wp 'lazy-response-count))
		(%parse-op-response wp)
		(%recv-int32 wp))))


(defun wp-op-response (wp)
  (log:debug wp)
  (let ((op-code (%skip-op-dummy wp)))
    (%skip-lazy-response wp op-code)
    (log:trace op-code)
    (when (= op-code +op-cont-auth+)
      (error 'operational-error :msg "Unauthorized"))
    (when (/= op-code +op-response+)
      (error "InternalError: wp-op-response:op_code = ~a" op-code)))
  (%parse-op-response wp))


(defun %wp-op-accept/wire-crypt (wp auth-data session-key)
  (log:trace wp)
  (let ((packet (with-xdr
		  (xdr-int32 +op-cont-auth+)
		  (xdr-string (bytes-to-hex auth-data))
		  (xdr-string (slot-value wp 'accept-plugin-name))
		  (xdr-octets (str-to-bytes (slot-value wp 'plugin-list)))
		  (xdr-octets #()))))
    (log:trace packet)
    (send-channel wp packet)
    (wp-op-response wp))

  (let ((packet (with-xdr
		  (xdr-int32 +op-crypt+)
		  (xdr-string "Arc4")
		  (xdr-string "Symmetric"))))
    (log:trace packet)
    (send-channel wp packet))

  ;; All traffic must be encrypted since that
  (setf (slot-value wp 'stream-cypher-recv)
	(crypto:make-cipher :arcfour :key session-key :mode :stream)
	(slot-value wp 'stream-cypher-send)
	(crypto:make-cipher :arcfour :key session-key :mode :stream))
  
  (values (wp-op-response wp)))


(defun %convert-username (wp)
  (let* ((user (slot-value wp 'user))
	 (ul (length user)))
    (if (and (> ul 2)
	     (char= #\" (elt user 0))
	     (char= #\" (elt user (1- ul))))
	(replace-all (subseq user 1 (1- ul)) "\"\"" "\"")
	(string-upcase user))))
  

(defun %wp-op-accept/auth (wp)
  (log:trace wp)
  (let* ((data-len (%recv-int32 wp))
	 (data (recv-channel wp data-len t))
	 (apn (recv-channel wp (%recv-int32 wp) t))
	 (accept-plugin-name (bytes-to-str apn))
	 (is-authenticated (%recv-int32 wp))
	 (hash-algo :sha1))
    (setf (slot-value wp 'accept-plugin-name) accept-plugin-name)
    (recv-channel wp (%recv-int32 wp)) ; keys
    (log:trace data-len data apn accept-plugin-name is-authenticated)

    (when (zerop is-authenticated)
      (cond
	((or (string= "Srp256" accept-plugin-name)
	     (string= "Srp" accept-plugin-name))
	 (when (string= "Srp256" accept-plugin-name)
	   (setf hash-algo :sha256))
	 (when (zerop (length data))
	   (error 'operational-error :msg "Unauthorized"))
	 (let* ((ln (bytes-to-long-le (subseq data 0 2)))
		(server-salt (subseq data 2 (+ ln 2)))
		(b1 (subseq data (+ ln 4)))
		(b2 (bytes-to-str b1))
		(b3 (hex-to-long b2))
		(server-public-key b3)
		(user (%convert-username wp)))
	   (log:trace user server-salt server-public-key)
	   (multiple-value-bind (auth-data session-key)
	       (client-proof (str-to-bytes user)
			     (str-to-bytes (slot-value wp 'password))
			     server-salt
			     (slot-value wp 'client-public-key)
			     server-public-key
			     (slot-value wp 'client-private-key)
			     hash-algo)
	     (log:trace "auth-data  : ~a" (bytes-to-hex auth-data))
	     (log:trace "session-key: ~a" (bytes-to-hex session-key))
	     (if (slot-value wp 'wire-crypt)
		 (%wp-op-accept/wire-crypt wp auth-data session-key)
		 (setf (slot-value wp 'auth-data) auth-data)))))
	((string= "Legacy_Auth" accept-plugin-name)
	 ;; XXX: never be here, maybe unneeded
	 (setf (slot-value wp 'auth-data)
	       (subseq (crypt:crypt (slot-value wp 'password) "9z") 2)))
	(t
	 (error 'operational-error
		:msg (format nil "Unknown auth plugin: ~a" accept-plugin-name))))))
  (values))


(defun wp-op-accept (wp)
  (log:debug wp)
  (let ((op-code (%skip-op-dummy wp)))
    (declare (type (unsigned-byte 8) op-code))
    (when (= op-code +op-reject+)
      (error 'operational-error :msg "Connection is rejected"))
    (when (= op-code +op-response+)
      (return-from wp-op-accept (%parse-op-response wp)))
    
    (let ((b (recv-channel wp 12)))
      (with-slots (accept-version
		   accept-architecture
		   accept-type
		   lazy-response-count)
	  wp
	(setf accept-version      (bytes-to-long (subseq b 2 4))
	      accept-architecture (bytes-to-long (subseq b 4 8))
	      accept-type         (bytes-to-long (subseq b 8))
	      lazy-response-count 0)))

    (log:trace op-code)
    (setf (slot-value wp 'auth-data) nil)
    (if (or (= op-code +op-cond-accept+)
	    (= op-code +op-accept-data+))
	(%wp-op-accept/auth wp)
	(assert (= op-code +op-accept+)))))


(defun %getpid ()
  (or
   ;; XXX: add for other lisps
   #+CCL (ccl::getpid)
   1))


(defun %getprocname ()
  (or
   ;; XXX: add for other lisps
   #+CCL (first ccl:*command-line-argument-list*)
   "lisp"))

(defun create-dpb-create (wp page-size)
  (with-bytes
    (<< +isc-dpb-version1+)
    (let ((cs (str-to-bytes (slot-value wp 'charset)))
	  (un (str-to-bytes (slot-value wp 'user))))
      (<< +isc-dpb-set-db-charset+) (<< (length cs)) (<< cs)
      (<< +isc-dpb-lc-ctype+)  (<< (length cs)) (<< cs)
      (<< +isc-dpb-user-name+) (<< (length un)) (<< un))
    (when (< (protocol-accept-version wp) +protocol-version13+)
      (if (= (protocol-accept-version wp) +protocol-version10+)
	  (let ((pwd (str-to-bytes (slot-value wp 'password))))
	    (<< +isc-dpb-password+) (<< (length pwd)) (<< pwd))
	  (let ((pwd (str-to-bytes (subseq (crypt:crypt (slot-value wp 'password) "9z") 2))))
	    (<< +isc-dpb-password-enc+) (<< (length pwd)) (<< pwd))))
    (when (slot-value wp 'role)
      (let ((rn (str-to-bytes (slot-value wp 'role))))
	(<< +isc-dpb-sql-role-name+) (<< (length rn)) (<< rn)))
    (<< +isc-dpb-process-id+) (<< 4) (<< (long-to-bytes (%getpid) 4))
    (let ((pn (str-to-bytes (%getprocname))))
      (<< +isc-dpb-process-name+) (<< (length pn)) (<< pn))
    (when (slot-value wp 'auth-data)
      (let ((ad (str-to-bytes (bytes-to-hex (slot-value wp 'auth-data)))))
	(<< +isc-dpb-specific-auth-data+) (<< (length ad)) (<< ad)))
    (when (slot-value wp 'timezone)
      (let ((tz (str-to-bytes (slot-value wp 'timezone))))
	(<< +isc-dpb-session-time-zone+) (<< (length tz)) (<< tz)))
    (<< +isc-dpb-sql-dialect+) (<< 4) (<< (long-to-bytes-le 3 4))
    (<< +isc-dpb-force-write+) (<< 4) (<< (long-to-bytes-le 1 4))
    (<< +isc-dpb-overwrite+)   (<< 4) (<< (long-to-bytes-le 1 4))
    (<< +isc-dpb-page-size+)   (<< 4) (<< (long-to-bytes-le page-size 4))
    ))


(defun wp-op-create (wp &optional (page-size 4096))
  (log:debug wp page-size)
  (let* ((dpb (create-dpb-create wp page-size))
	 (packet (with-xdr
		   (xdr-int32 +op-create+)
		   (xdr-int32 0)	; Database Object ID
		   (xdr-string (slot-value wp 'filename))
		   (xdr-octets dpb))))
    (log:trace "DPB ~a" (bytes-to-hex (with-xdr (xdr-octets dpb))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-drop-database (wp)
  (log:debug wp)
  (let ((db-handle (slot-value wp 'db-handle)))
    (unless db-handle
      (error 'operational-error :msg "op_drop_database() Invalid db handle"))
    (let ((packet (with-xdr
		    (xdr-int32 +op-drop-database+)
		    (xdr-int32 db-handle))))
      (log:trace packet)
      (send-channel wp packet)))
  (values))


(defun create-dpb-attach (wp)
  (with-bytes
    (<< +isc-dpb-version1+)
    (let ((cs (str-to-bytes (slot-value wp 'charset)))
	  (un (str-to-bytes (slot-value wp 'user))))
      (<< +isc-dpb-lc-ctype+)  (<< (length cs)) (<< cs)
      (<< +isc-dpb-user-name+) (<< (length un)) (<< un))
    (when (< (protocol-accept-version wp) +protocol-version13+)
      (if (= (protocol-accept-version wp) +protocol-version10+)
	  (let ((pwd (str-to-bytes (slot-value wp 'password))))
	    (<< +isc-dpb-password+) (<< (length pwd)) (<< pwd))
	  (let ((pwd (str-to-bytes (subseq (crypt:crypt (slot-value wp 'password) "9z") 2))))
	    (<< +isc-dpb-password-enc+) (<< (length pwd)) (<< pwd))))
    (when (slot-value wp 'role)
      (let ((rn (str-to-bytes (slot-value wp 'role))))
	(<< +isc-dpb-sql-role-name+) (<< (length rn)) (<< rn)))
    (<< +isc-dpb-process-id+) (<< 4) (<< (long-to-bytes (%getpid) 4))
    (let ((pn (str-to-bytes (%getprocname))))
      (<< +isc-dpb-process-name+) (<< (length pn)) (<< pn))
    (when (slot-value wp 'auth-data)
      (let ((ad (str-to-bytes (bytes-to-hex (slot-value wp 'auth-data)))))
	(<< +isc-dpb-specific-auth-data+) (<< (length ad)) (<< ad)))
    (when (slot-value wp 'timezone)
      (let ((tz (str-to-bytes (slot-value wp 'timezone))))
	(<< +isc-dpb-session-time-zone+) (<< (length tz)) (<< tz)))
    ))


(defun wp-op-attach (wp)
  (log:debug wp)
  (let* ((dpb (create-dpb-attach wp))
	 (packet (with-xdr
		   (xdr-int32 +op-attach+)
		   (xdr-int32 0) ; Database Object ID
		   (xdr-string (slot-value wp 'filename))
		   (xdr-octets dpb))))
    (log:trace "DPB ~a" (bytes-to-hex (with-xdr (xdr-octets dpb))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-detach (wp)
  (log:debug wp)
  (let ((db-handle (slot-value wp 'db-handle)))
    (unless db-handle
      (error 'operational-error :msg "op_detach() Invalid db handle"))
    (let ((packet (with-xdr
		    (xdr-int32 +op-detach+)
		    (xdr-int32 db-handle))))
      (log:trace packet)
      (send-channel wp packet)))
  (values))
	  

(defun wp-op-transaction (wp tpb)
  (log:debug wp tpb)
  (let ((db-handle (slot-value wp 'db-handle)))
    (unless db-handle
      (error 'operational-error :msg "op_transaction() Invalid db handle"))
    (let ((packet (with-xdr
		    (xdr-int32 +op-transaction+)
		    (xdr-int32 db-handle)
		    (xdr-octets tpb))))
      (log:trace packet)
      (send-channel wp packet)))
  (values))


(defun wp-op-commit (wp trans-handle)
  (log:debug wp trans-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-commit+)
		  (xdr-int32 trans-handle))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-commit-retaining (wp trans-handle)
  (log:debug wp trans-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-commit-retaining+)
		  (xdr-int32 trans-handle))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-rollback (wp trans-handle)
  (log:debug wp trans-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-rollback+)
		  (xdr-int32 trans-handle))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-rollback-retaining (wp trans-handle)
  (log:debug wp trans-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-rollback-retaining+)
		  (xdr-int32 trans-handle))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-exec-immediate (wp trans-handle query)
  (log:debug wp trans-handle query)
  (let ((db-handle (slot-value wp 'db-handle)))
    (unless db-handle
      (error 'operational-error :msg "op_exec_immediate() Invalid db handle"))
    (let ((packet (with-xdr
		    (xdr-int32 +op-exec-immediate+)
		    (xdr-int32 trans-handle)
		    (xdr-int32 db-handle)
		    (xdr-int32 3)	; dialect = 3
		    (xdr-string query)
		    (xdr-octets #())
		    (xdr-int32 +wp-buffer-length+))))
      (log:trace packet)
      (send-channel wp packet)))
  (values))


(defun wp-op-info-transaction (wp trans-handle info-requests)
  (log:debug wp trans-handle info-requests)
  (let ((packet (with-xdr
		  (xdr-int32 +op-info-transaction+)
		  (xdr-int32 trans-handle)
		  (xdr-int32 0)
		  (xdr-octets info-requests)
		  (xdr-int32 +wp-buffer-length+))))
    (log:trace packet)
    (send-channel wp packet))
  (values))

(defun wp-op-info-database (wp info-requests)
  (log:debug wp info-requests)
  (let ((db-handle (slot-value wp 'db-handle)))
    (unless db-handle
      (error 'operational-error :msg "op_info_database() Invalid db handle"))
    (let ((packet (with-xdr
		    (xdr-int32 +op-info-database+)
		    (xdr-int32 db-handle)
		    (xdr-int32 0)
		    (xdr-octets info-requests)
		    (xdr-int32 +wp-buffer-length+))))
      (log:trace packet)
      (send-channel wp packet)))
  (values))


(defun wp-op-allocate-statement (wp)
  (log:debug wp)
  (with-slots (db-handle) wp
    (unless db-handle
      (error 'operational-error :msg "op_allocate_statement() Invalid db handle"))
    (let ((packet (with-xdr
		    (xdr-int32 +op-allocate-statement+)
		    (xdr-int32 db-handle))))
      (log:trace packet)
      (send-channel wp packet)))
  (values))


(defun wp-op-free-statement (wp stmt-handle mode)
  (log:debug wp stmt-handle mode)
  (let ((packet (with-xdr
		  (xdr-int32 +op-free-statement+)
		  (xdr-int32 stmt-handle)
		  (xdr-int32 mode))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-prepare-statement (wp stmt-handle trans-handle query &optional option-items)
  (unless option-items (setf option-items #()))
  (log:debug wp stmt-handle trans-handle query option-items)
  (let* ((desc-items (make-bytes option-items
				 +isc-info-sql-stmt-type+
				 +info-sql-select-describe-vars+))
	 (packet (with-xdr
		   (xdr-int32 +op-prepare-statement+)
		   (xdr-int32 trans-handle)
		   (xdr-int32 stmt-handle)
		   (xdr-int32 3)	; dialect = 3
		   (xdr-string query)
		   (xdr-octets desc-items)
		   (xdr-int32 +wp-buffer-length+))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun %create-blob (wp trans-handle b)
  (wp-op-create-blob2 wp trans-handle)
  (multiple-value-bind (blob-handle blob-id)
      (wp-op-response wp)
    (loop :with i = 0 :with blen = (length b)
       :for j = (+ i *blob-segment-size*)
       :while (< i blen)
       :do (wp-op-put-segment wp blob-handle (subseq b i (min j blen)))
       :do (wp-op-response wp)
       :do (incf i *blob-segment-size*))
    (wp-op-close-blob wp blob-handle)
    (wp-op-response wp)
    (values blob-id)))
  

(defun %params-null-indicator (params)
  (let ((null-indicator 0))
    (loop :for i :from 0
       :for p :in params
       :do (setf (ldb (byte 1 i) null-indicator) (if (eq p :null) 1 0)))
    (values null-indicator)))


(defun %params-to-blr/vls (wp params)
  (if (< (protocol-accept-version wp) +protocol-version13+)
      (make-bytes nil)
      (let* ((plen (length params))
	     (null-indicator (%params-null-indicator params))
	     (n (floor plen 8))
	     null-indicator-bytes)
	(when (/= 0 (mod plen 8)) (incf n))
	(when (/= 0 (mod n 4)) (incf n (- 4 (mod n 4))))
	(loop :for i :from 0 to (1- n)
	   :do (push (logand null-indicator #xff) null-indicator-bytes)
	   :do (setf null-indicator (ash null-indicator -8)))
	(log:trace null-indicator-bytes)
	(make-bytes (nreverse null-indicator-bytes)))))


#|
            elif t == decimal.Decimal or t == float:
                if t == float:
                    p = decimal.Decimal(str(p))
                (sign, digits, exponent) = p.as_tuple()
                v = 0
                ln = len(digits)
                for i in range(ln):
                    v += digits[i] * (10 ** (ln - i - 1))
                if sign:
                    v *= -1
                v = bint_to_bytes(v, 8)
                if exponent < 0:
                    exponent += 256
                blr += bs([16, exponent])
|#

#+nil
(defun %split-float (f)
  (let* ((sign (if (< f 0) -1 1))
	 ;;(sign (floor (float-sign f)))
	 (parts (str:split #\. (format nil "~f" f)))
	 (digits (first parts))
	 (exponent (second parts)))
    (when (= sign -1)
      (setf digits (subseq digits 1)))
    (setf digits (concatenate 'string digits exponent))
    (values sign
	    (map 'vector (lambda (x) (- (char-code x) (char-code #\0))) digits)
	    (- (length exponent)))))
    
    
(defun %params-to-blr (wp trans-handle params)
  "Convert parameter array to BLR and values format."
  (let ((ln (* 2 (length params)))
	(blr (byte-stream))
	(vls (byte-stream)))
    (append-bytes blr 5 2 4 0 (ldb (byte 8 0) ln) (ldb (byte 8 8) ln))
    (append-bytes vls (%params-to-blr/vls wp params))
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
		    (progn (setf v (%create-blob wp trans-handle s))
			   (append-bytes blr 9 0))
		    (progn (setf v (make-bytes s (pad4-bytes slen)))
			   (append-bytes blr 14
					 (ldb (byte 8 0) slen)
					 (ldb (byte 8 8) slen))))))
	     ((vectorp p)		; BLOB
	      (setf v (%create-blob wp trans-handle p))
	      (append-bytes blr 9 0))
	     ((eq p nil) (setf v #(0 0 0 0)) (append-bytes blr 23))
	     ((eq p t)   (setf v #(1 0 0 0)) (append-bytes blr 23))
	     (t
	      (log:trace "DEFAULT: string =>" p (type-of p))
	      (let* ((s (str-to-bytes (str p)))
		     (nbytes (length s)))
		(setf v (make-bytes s (pad4-bytes nbytes)))
		(append-bytes blr 14 (ldb (byte 8 0) nbytes)
			      (ldb (byte 8 8) nbytes))))) ; cond
       :do (append-bytes blr 7 0) 
       :do (append-bytes vls v)
       :do (when (< (protocol-accept-version wp) +protocol-version13+)
	     (append-bytes vls (if (eq p :null) #(255 255 255 255) #(0 0 0 0))))
       :finally (append-bytes blr 255 76)) ; loop

    (let ((blr (byte-stream-output blr))
	  (vls (byte-stream-output vls)))
      (log:trace "%params-to-blr: ~a ~a" blr vls)
      (values blr vls))))


(defun wp-op-execute (wp stmt-handle trans-handle params)
  (log:debug wp stmt-handle trans-handle params)
  (let ((packet (with-xdr
		  (xdr-int32 +op-execute+)
		  (xdr-int32 stmt-handle)
		  (xdr-int32 trans-handle)
		  (if (zerop (length params))
		      (progn
			(xdr-octets #())
			(xdr-int32 0)
			(xdr-int32 0))
		      (multiple-value-bind (blr vals)
			  (%params-to-blr wp trans-handle params)
			(log:debug "blr vals: ~a ~a" blr vals)
			(xdr-octets blr)
			(xdr-int32 0)
			(xdr-int32 1)
			(write-sequence vals *xdr*))))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-execute2 (wp stmt-handle trans-handle params output-blr)
  (log:debug wp stmt-handle trans-handle params output-blr)
  (let ((packet (with-xdr
		  (xdr-int32 +op-execute2+)
		  (xdr-int32 stmt-handle)
		  (xdr-int32 trans-handle)
		  (if (zerop (length params))
		      (progn
			(xdr-octets #())
			(xdr-int32 0)
			(xdr-int32 0))
		      (multiple-value-bind (blr vals)
			  (%params-to-blr wp trans-handle params)
			(log:debug "blr vals: ~a ~a" blr vals)
			(xdr-octets blr)
			(xdr-int32 0)
			(xdr-int32 1)
			(write-sequence vals *xdr*)))
		  (xdr-octets output-blr)
		  (xdr-int32 0))))
    (log:debug packet)
    (send-channel wp packet))
  (values))


(defun wp-op-info-sql (wp stmt-handle vars)
  (log:debug wp stmt-handle vars)
  (let ((packet (with-xdr
		  (xdr-int32 +op-info-sql+)
		  (xdr-int32 stmt-handle)
		  (xdr-int32 0)
		  (xdr-octets vars)
		  (xdr-int32 +wp-buffer-length+))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-fetch (wp stmt-handle blr &optional count)
  (unless count (setf count *default-fetch-size*))
  (log:debug wp stmt-handle blr)
  (let ((packet (with-xdr
		  (xdr-int32 +op-fetch+)
		  (xdr-int32 stmt-handle)
		  (xdr-octets blr)
		  (xdr-int32 0)
		  (xdr-int32 count))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun %kw (x)
  (intern (string-upcase x) :keyword))


(defun %make-null-indicator (wp xsqlda)
  (let ((n (floor (length xsqlda) 8))
	(null-indicator 0))
    (when (/= 0 (mod (length xsqlda) 8)) (incf n))
    (loop :for c :across (nreverse (recv-channel wp n t))
       :do (setf null-indicator (ash null-indicator 8))
       :do (setf null-indicator (logior null-indicator c)))
    (values null-indicator)))


(declaim (ftype (function (t t) t) xsqlvar-value)
	 (ftype (function (t) t) xsqlvar-io-length))

(defun wp-op-fetch-response/v12 (wp xsqlda)
  (let ((r (make-list (length xsqlda) :initial-element :null)))
    (loop :for x in xsqlda
       :for i :from 0
       :do (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (%recv-int32 wp) io-len))
		  (raw-value (recv-channel wp ln t)))
	     (log:trace (xsqlvar-aliasname x) raw-value)
	     (when (equalp #(0 0 0 0) (recv-channel wp 4)) ; not NULL
	       (setf (elt r i) (xsqlvar-value x raw-value)))))
    (values r)))


(defun wp-op-fetch-response/v12-plist (wp xsqlda)
  (let ((r (make-list (* 2 (length xsqlda)) :initial-element :null))) ; row p-list
    (loop :for x in xsqlda
       :for i :from 0
       :do (setf (elt r (* 2 i)) (%kw (xsqlvar-aliasname x)))
       :do (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (%recv-int32 wp) io-len))
		  (raw-value (recv-channel wp ln t)))
	     (log:trace (xsqlvar-aliasname x) raw-value)
	     (when (equalp #(0 0 0 0) (recv-channel wp 4)) ; not NULL
	       (setf (elt r (1+ (* 2 i)))
		     (xsqlvar-value x raw-value)))))
    (values r)))


(defun wp-op-fetch-response/v13 (wp xsqlda)
  (let ((r (make-list (length xsqlda) :initial-element :null))
	(null-indicator (%make-null-indicator wp xsqlda)))
    (loop :for x in xsqlda
       :for i :from 0
       :do (when (zerop (logand null-indicator (ash 1 i)))
	     (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (%recv-int32 wp) io-len))
		  (raw-value (recv-channel wp ln t)))
	       (log:trace (xsqlvar-aliasname x) raw-value)
	       (setf (elt r i) (xsqlvar-value x raw-value)))))
    (values r)))


(defun wp-op-fetch-response/v13-plist (wp xsqlda)
  (let ((r (make-list (* 2 (length xsqlda)) :initial-element :null)) ; row p-list
	(null-indicator (%make-null-indicator wp xsqlda)))
    (loop :for x in xsqlda
       :for i :from 0
       :do (setf (elt r (* 2 i)) (%kw (xsqlvar-aliasname x)))
       :do (when (zerop (logand null-indicator (ash 1 i)))
	     (let* ((io-len (xsqlvar-io-length x))
		  (ln (if (< io-len 0) (%recv-int32 wp) io-len))
		  (raw-value (recv-channel wp ln t)))
	       (log:trace (xsqlvar-aliasname x) raw-value)
	       (setf (elt r (1+ (* 2 i))) (xsqlvar-value x raw-value)))))
    (values r)))


(defun wp-op-fetch-response (wp stmt-handle xsqlda)
  (log:debug wp stmt-handle xsqlda)
  (let ((op-code (%skip-op-dummy wp)))
    (%skip-lazy-response wp op-code)
    (unless (= op-code +op-fetch-response+)
      (when (= op-code +op-response+)
	(%parse-op-response wp))
      (error "op_fetch_response: op-code = ~a" op-code))

    (let* ((b (recv-channel wp 8))
	   (status (bytes-to-long (subseq b 0 4)))
	   (count (bytes-to-long (subseq b 4 8)))
	   rows)
      (log:trace b status count)
      (loop
	 (when (zerop count) (return))
	 (push (if (< (protocol-accept-version wp) +protocol-version13+)
		   (wp-op-fetch-response/v12 wp xsqlda)
		   (wp-op-fetch-response/v13 wp xsqlda))
	       rows)
	 (setf b (recv-channel wp 12)
	       op-code (bytes-to-long (subseq b 0 4))
	       status (bytes-to-long (subseq b 4 8))
	       count (bytes-to-long (subseq b 8)))
	 (log:trace b op-code status count))
      (values (nreverse rows) (/= status 100)))))
		   

(defun wp-op-fetch-response-plist (wp stmt-handle xsqlda)
  (log:debug wp stmt-handle xsqlda)
  (let ((op-code (%skip-op-dummy wp)))
    (%skip-lazy-response wp op-code)
    (unless (= op-code +op-fetch-response+)
      (when (= op-code +op-response+)
	(%parse-op-response wp))
      (error "op_fetch_response: op-code = ~a" op-code))

    (let* ((b (recv-channel wp 8))
	   (status (bytes-to-long (subseq b 0 4)))
	   (count (bytes-to-long (subseq b 4 8)))
	   rows)
      (log:trace b status count)
      (loop
	 (when (zerop count) (return))
	 (push (if (< (protocol-accept-version wp) +protocol-version13+)
		   (wp-op-fetch-response/v12-plist wp xsqlda)
		   (wp-op-fetch-response/v13-plist wp xsqlda))
	       rows)
	 (setf b (recv-channel wp 12)
	       op-code (bytes-to-long (subseq b 0 4))
	       status (bytes-to-long (subseq b 4 8))
	       count (bytes-to-long (subseq b 8)))
	 (log:trace b op-code status count))
      (values (nreverse rows) (/= status 100)))))
		   
      

(defun wp-op-sql-response (wp xsqlda)
  (log:debug wp xsqlda)
  (let ((op-code (%skip-op-dummy wp)))
    (%skip-lazy-response wp op-code)
    (unless (= op-code +op-sql-response+)
      (when (= op-code +op-response+)
	(%parse-op-response wp))
      (error "op_sql_response: op-code = ~a" op-code))

    (unless (zerop (%recv-int32 wp))
      (if (< (protocol-accept-version wp) +protocol-version13+)
	  (wp-op-fetch-response/v12 wp xsqlda)
	  (wp-op-fetch-response/v13 wp xsqlda)))))
      

(defun wp-op-open-blob (wp blob-id trans-handle)
  (log:debug wp blob-id trans-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-open-blob+)
		  (xdr-int32 trans-handle)
		  (write-sequence blob-id *xdr*))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-create-blob2 (wp trans-handle)
  (log:debug wp trans-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-create-blob2+)
		  (xdr-int32 0)
		  (xdr-int32 trans-handle)
		  (xdr-int32 0)
		  (xdr-int32 0))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-get-segment (wp blob-handle)
  (log:debug wp blob-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-get-segment+)
		  (xdr-int32 blob-handle)
		  (xdr-int32 65535)
		  (xdr-int32 0))))
    (log:trace packet)
    (send-channel wp packet))
  (values))


(defun wp-op-put-segment (wp blob-handle data)
  (log:debug wp blob-handle)
  (let* ((ln (length data))
	 (pad (pad4-bytes ln))
	 (packet (with-xdr
		  (xdr-int32 +op-put-segment+)
		  (xdr-int32 blob-handle)
		  (xdr-int32 ln)
		  (xdr-int32 ln))))
    (log:trace packet)
    (send-channel wp packet)
    (send-channel wp (make-bytes data pad)))
  (values))


(defun wp-op-batch-segments (wp blob-handle data)
  (log:debug wp blob-handle)
  (let* ((ln (length data))
	 (ln2 (+ ln 2))
	 (pad (pad4-bytes ln2))
	 (packet (with-xdr
		  (xdr-int32 +op-batch-segments+)
		  (xdr-int32 blob-handle)
		  (xdr-int32 ln2)
		  (xdr-int32 ln2))))
    (log:trace packet)
    (send-channel wp packet)
    (send-channel wp (make-bytes (long-to-bytes ln 2) data pad)))
  (values))


(defun wp-op-close-blob (wp blob-handle)
  (log:debug wp blob-handle)
  (let ((packet (with-xdr
		  (xdr-int32 +op-close-blob+)
		  (xdr-int32 blob-handle))))
    (log:trace packet)
    (send-channel wp packet))
  (values))

