(in-package #:cl-firebird)


(defparameter +xsqlvar-type-length+
  (list
   +sql-type-varying+ -1
   +sql-type-short+ 4
   +sql-type-long+ 4
   +sql-type-float+ 4
   +sql-type-time+ 4
   +sql-type-date+ 4
   +sql-type-double+ 8
   +sql-type-timestamp+ 8
   +sql-type-blob+ 8
   +sql-type-array+ 8
   +sql-type-quad+ 8
   +sql-type-int64+ 8
   +sql-type-timestamp-tz+ 10
   +sql-type-time-tz+ 6
   +sql-type-dec64+  8
   +sql-type-dec128+  16
   +sql-type-dec-fixed+ 16
   +sql-type-boolean+ 1))
(defparameter +xsqlvar-type-display-length+
  (list
   +sql-type-varying+ -1
   +sql-type-short+ 6
   +sql-type-long+ 11
   +sql-type-float+ 17
   +sql-type-time+ 11
   +sql-type-date+ 10
   +sql-type-double+ 17
   +sql-type-timestamp+ 22
   +sql-type-blob+ 0
   +sql-type-array+ -1
   +sql-type-quad+ 20
   +sql-type-int64+ 20
   +sql-type-timestamp-tz+ 28
   +sql-type-time-tz+ 17
   +sql-type-dec64+ 16
   +sql-type-dec128+ 34
   +sql-type-dec-fixed+ 34
   +sql-type-boolean+ 5))


(defun %xsqlvar-type-length (k)
  (getf +xsqlvar-type-length+ k))


(defun %xsqlvar-type-display-length (k)
  (getf +xsqlvar-type-display-length+ k))


(defun xsqlvar-io-length (v)
  (with-slots (sqltype sqllen) v
    (if (= sqltype +sql-type-text+)
	sqllen
	(%xsqlvar-type-length sqltype))))


(defun xsqlvar-display-length (v)
  (with-slots (sqltype sqllen) v
    (if (= sqltype +sql-type-text+)
	sqllen
	(%xsqlvar-type-display-length sqltype))))


(defun xsqlvar-precision (v)
  (xsqlvar-display-length v))


(defparameter +xsqlvar-type-repr+
  (list
   +sql-type-text+ :text
   +sql-type-varying+ :varying
   +sql-type-short+ :short
   +sql-type-long+ :long
   +sql-type-float+ :float
   +sql-type-double+ :double
   +sql-type-d-float+ :d-float
   +sql-type-timestamp+ :timestamp
   +sql-type-blob+ :blob
   +sql-type-array+ :array
   +sql-type-quad+ :quad
   +sql-type-time+ :time
   +sql-type-date+ :date
   +sql-type-int64+ :int64
   +sql-type-timestamp-tz+ :timestamp-tz
   +sql-type-time-tz+ :time-tz
   +sql-type-dec-fixed+ :dec-fixed
   +sql-type-dec64+ :dec64
   +sql-type-dec128+ :dec128
   +sql-type-boolean+ :boolean
   +sql-type-null+ :null))


(defmethod print-object ((object xsqlvar) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (with-slots (sqltype sqlscale sqlsubtype sqllen
			 null-ok fieldname relname ownname aliasname)
	object
      (format stream "XSQLVAR [~a/~a, ~{~a~^, ~}]"
	      (getf +xsqlvar-type-repr+ sqltype) sqlsubtype
	      (list sqlscale sqllen (if null-ok "NULLABLE" "NOT NULL")
		    fieldname aliasname relname ownname)))))


(defun %xsqlvar-parse-date (raw-value)
  (let* ((nday (+ 678882 (bytes-to-long raw-value)))
	 (century (floor (1- (* 4 nday)) 146097))
	 year month day)
    (setf nday (- (1- (* 4 nday)) (* 146097 century)))
    (setf day (floor nday 4))
    (setf nday (floor (+ (* 4 day) 3) 1461))
    (setf day (- (+ (* 4 day) 3) (* 1461 nday)))
    (setf day (floor (+ day 4) 4))
    (setf month (floor (- (* 5 day) 3) 153))
    (setf day (- (- (* 5 day) 3) (* 153 month)))
    (setf day (floor (+ day 5) 5))
    (setf year (+ (* 100 century) nday))
    (if (< month 10)
	(incf month 3)
	(progn (decf month 9) (incf year)))
    (values year month day)))


(defun %xsqlvar-parse-time (raw-value)
  (let* ((n (bytes-to-long raw-value))
	 (s (floor n +isc-time-seconds-precision+))
	 (m (floor s 60))
	 (h (floor m 60))
	 (ms (* 100 (mod n +isc-time-seconds-precision+))))
    (setf m (mod m 60)
	  s (mod s 60))
    (values h m s ms)))


(defun %xsqlvar-parse-time-zone (raw-value)
  (declare (ignore raw-value))
  (values nil))


(defun xsqlvar-value (v raw-value)
  (with-slots (sqltype sqlsubtype sqlscale) v
    (cond
      ((= sqltype +sql-type-text+)
       (if (member sqlsubtype '(0 1))	; character set (NONE|OCTETS)
	   raw-value
	   ;; XXX: how to know what charset to use to decode string?
	   (string-right-trim '(#\Space) (bytes-to-str raw-value))))
      ((= sqltype +sql-type-varying+)
       (if (member sqlsubtype '(0 1))	; character set (NONE|OCTETS)
	   raw-value
	   ;; XXX: how to know what charset to use to decode string?
	   (bytes-to-str raw-value)))
      ((member sqltype '(#.+sql-type-short+ #.+sql-type-long+ #.+sql-type-int64+))
       (let ((x (bytes-to-int raw-value)))
	 (if (zerop sqlscale)
	     x
	     (/ x (expt 10 (abs sqlscale))))))
      ((= sqltype +sql-type-date+)
       (multiple-value-bind (y m d)
	   (%xsqlvar-parse-date raw-value)
	 (values (list :date y m d))))
      ((= sqltype +sql-type-time+)
       (multiple-value-bind (h m s ms)
	   (%xsqlvar-parse-time raw-value)
	 (values (list :time h m s ms))))
      ((= sqltype +sql-type-timestamp+)
       (multiple-value-bind (yy mm dd)
	   (%xsqlvar-parse-date (subseq raw-value 0 4))
	 (multiple-value-bind (h m s ms)
	     (%xsqlvar-parse-time (subseq raw-value 4))
	   (values (list :timestamp yy mm dd h m s ms)))))
      ((= sqltype +sql-type-float+)
       (flex:with-input-from-sequence (s raw-value)
	 (nibbles:read-ieee-single/be s)))
      ((= sqltype +sql-type-double+)
       (flex:with-input-from-sequence (s raw-value)
	 (nibbles:read-ieee-double/be s)))
      ((= sqltype +sql-type-boolean+)
       (not (zerop (elt raw-value 0))))
      ((= sqltype +sql-type-timestamp-tz+)
       (multiple-value-bind (yy mm dd)
	   (%xsqlvar-parse-date (subseq raw-value 0 4))
	 (multiple-value-bind (h m s ms)
	     (%xsqlvar-parse-time (subseq raw-value 4 8))
	   (let ((tz (%xsqlvar-parse-time-zone (subseq raw-value 8))))
	     (values (list :timestamp-tz yy mm dd h m s ms :tzinfo tz))))))
      ((= sqltype +sql-type-time-tz+)
       (multiple-value-bind (h m s ms)
	   (%xsqlvar-parse-time (subseq raw-value 0 4))
	 (let ((tz (%xsqlvar-parse-time-zone (subseq raw-value 4))))
	   (values (list :time-tz h m s ms :tzinfo tz)))))
      ((= sqltype +sql-type-blob+)
       (make-blob raw-value sqlsubtype))
      ((= sqltype +sql-type-dec-fixed+)
       (error "dec-fixed"))
      ((= sqltype +sql-type-dec64+)
       (error "dec64"))
      ((= sqltype +sql-type-dec128+)
       (error "dec128"))
      (t raw-value))))



(defparameter +sqltype-to-blr+
  (list
   +sql-type-double+       '(27)
   +sql-type-float+        '(10)
   +sql-type-d-float+      '(11)
   +sql-type-date+         '(12)
   +sql-type-time+         '(13)
   +sql-type-timestamp+    '(35)
   +sql-type-blob+         '(9 0)
   +sql-type-array+        '(9 0)
   +sql-type-boolean+      '(23)
   +sql-type-dec64+        '(24)
   +sql-type-dec128+       '(25)
   +sql-type-time-tz+      '(28)
   +sql-type-timestamp-tz+ '(29)))

#|
blr_text = 14
blr_text2 = 15
blr_short = 7
blr_long = 8
blr_quad = 9
blr_float = 10
blr_double = 27
blr_d_float = 11
blr_timestamp = 35
blr_varying = 37
blr_varying2 = 38
blr_blob = 261
blr_cstring = 40
blr_cstring2 = 41
blr_blob_id = 45
blr_sql_date = 12
blr_sql_time = 13
blr_int64 = 16
blr_blob2 = 17
blr_domain_name = 18
blr_domain_name2 = 19
blr_not_nullable = 20
blr_column_name = 21
blr_column_name2 = 22
blr_bool = 23 # Firebird 3.0

blr_assignment = 1
blr_begin = 2
blr_dcl_variable = 3
blr_message = 4
blr_version4 = 4
blr_version5 = 5
blr_eoc = 76
blr_end = 255

	    case SQL_BLOB:
		    if (protocol >= PROTOCOL_VERSION12)
		    {
			    appendUChar(blr_blob2);
			    appendUShort(subType);
			    appendUShort(charSet);
		    }
		    else
		    {
			    // Servers prior to FB 2.5 don't expect blr_blob2 in remote messages,
			    // so BLOB IDs are described as blr_quad instead
			    appendUChar(blr_quad);
			    appendUChar(0);
		    }

|#

(defun xsqlvar-calc-blr (xsqlda)
  "Calculate BLR from XSQLVAR array."
  (with-byte-stream (blr)
    (append-bytes blr 5 2 4 0) ; blr_version5, blr_begin, blr_message, message_number
    (nibbles:write-ub16/le (* 2 (length xsqlda)) blr)
    (loop :for v :in xsqlda
       :do (with-slots (sqltype sqllen sqlscale) v
	     (case sqltype
	       (#.+sql-type-varying+
		(append-bytes blr 37)
		(nibbles:write-ub16/le sqllen blr))
	       (#.+sql-type-text+
		(append-bytes blr 14)
		(nibbles:write-ub16/le sqllen blr))
	       (#.+sql-type-long+
		(append-bytes blr 8 sqlscale))
	       (#.+sql-type-short+
		(append-bytes blr 7 sqlscale))
	       (#.+sql-type-int64+
		(append-bytes blr 16 sqlscale))
	       (#.+sql-type-quad+
		(append-bytes blr 9 sqlscale))
	       (#.+sql-type-dec-fixed+
		(append-bytes blr 26 sqlscale))
	       (otherwise
		(append-bytes blr (getf +sqltype-to-blr+ sqltype)))))
       :do (append-bytes blr 7 0))	; [blr_short, 0]
    (append-bytes blr 255 76)))		; [blr_end, blr_eoc]


(defun %parse-select-items (buf xsqlda connection)
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
		(let ((use-unicode (connection-use-unicode connection)))
		  (setf (elt xsqlda (1- index))
			(make-instance 'xsqlvar :use-unicode use-unicode))))
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


(defun xsqlvar-parse-xsqlda (buf connection stmt-handle)
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
	  (setf next-index (%parse-select-items (subseq! buf (+ i l)) xsqlda connection))
	  (loop
	     (unless (> next-index 0) (return))
	     (break "xsqlvar-parse-xsqlda")
	     (wp-op-info-sql connection stmt-handle
			     (make-bytes +isc-info-sql-sqlda-start+ 2
					 (long-to-bytes-le next-index 2)
					 +info-sql-select-describe-vars+))
	     (multiple-value-bind (h oid buf)
		 (wp-op-response connection)
	       (declare (ignore h oid))
	       (assert (equalp (subseq! buf 0 2) #(4 7)))
	       (setf l (bytes-to-long-le (subseq! buf 2 4)))
	       (assert (= (bytes-to-long-le (subseq! buf 4 (+ 4 l))) col-len))
	       (setf next-index (%parse-select-items (subseq! buf (+ 4 l)) xsqlda connection)))))
	 (t (return))))
    (values stmt-type xsqlda)))

