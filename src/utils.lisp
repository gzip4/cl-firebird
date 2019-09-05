
(in-package #:cl-firebird)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'repr) #'write-to-string))

(declaim (inline byte-stream byte-stream-output))

(defun byte-stream ()
  (flexi-streams:make-in-memory-output-stream))

(defun byte-stream-output (s)
  (flexi-streams:get-output-stream-sequence s))

(defmacro with-byte-stream ((var) &body body)
  `(let (,var)
     (unwind-protect
	  (let ((*xdr* (byte-stream)))	; (xdr-*) stream
	    (setf ,var *xdr*)
	    ,@body
	    (byte-stream-output ,var))
       (when ,var (close ,var)))))


(declaim (inline pad-4-bytes xdr-int32 xdr-uint32 xdr-octets xdr-string))

(defun pad-4-bytes (x)
  (case (- 4 (mod x 4))
    (3 #(0 0 0))
    (2 #(0 0))
    (1 #(0))
    (0 #())))

(defun xdr-int32 (x)
  (nibbles:write-sb32/be x *xdr*))

(defun xdr-uint32 (x)
  (nibbles:write-ub32/be x *xdr*))

(defun xdr-octets (octets)
  (let ((len (length octets)))
    (nibbles:write-sb32/be len *xdr*)
    (write-sequence octets *xdr*)
    (write-sequence (pad-4-bytes len) *xdr*)))

(defun xdr-string (string &key (external-format :utf-8))
  (let ((octets (flex:string-to-octets string :external-format external-format)))
    (xdr-octets octets)))


(defun append-bytes (stream &rest args)
  (loop :for x :in args
     :do (etypecase x
	   (null nil)
	   ((unsigned-byte 8)  (write-byte x stream))
	   ((signed-byte 8)    (write-byte (+ 256 x) stream))
	   ((unsigned-byte 16) (nibbles:write-ub16/be x stream))
	   ((signed-byte 16)   (nibbles:write-sb16/be x stream))
	   ((unsigned-byte 32) (nibbles:write-ub32/be x stream))
	   ((signed-byte 32)   (nibbles:write-sb32/be x stream))
	   ((unsigned-byte 64) (nibbles:write-ub64/be x stream))
	   ((signed-byte 64)   (nibbles:write-sb64/be x stream))
	   (sequence           (write-sequence x stream)))))


(declaim (inline make-bytes))

(defun make-bytes (&rest args)
  (flexi-streams:with-output-to-sequence
   (bs)
   (apply #'append-bytes bs args)))


;; https://stackoverflow.com/questions/4366668/str-replace-in-lisp
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))


(defun split-sequence-chunks (sequence size)
  "Returns a list of sub-sequences of the sequence with size given."
  (loop with sl = (length sequence)
     with start = 0
     while (< start sl)
     collect (subseq sequence start (min (+ start size) sl))
     do (incf start size)))


(declaim (inline bytes-to-long bytes-to-long-le bytes-to-str bytes-to-int bytes-to-int-le bytes-to-hex
		 long-to-bytes long-to-bytes-le long-to-hex
		 str str-to-bytes hex-to-bytes hex-to-long
		 subseq!))

(defun subseq! (sequence start &optional end)
  "Vectors are displaced to sequence, other just subseq."
  (declare (type sequence sequence)
	   (type fixnum start)
	   (type (or null fixnum) end))
  (typecase sequence
    (vector (make-array (- (or end (length sequence)) start)
			:element-type (array-element-type sequence)
			:displaced-to sequence
			:displaced-index-offset start))
    (t (subseq sequence start end))))

(defun str (x)
  (typecase x
    (string x)
    (t (write-to-string x))))

(defun bytes-to-long (s)
  (octets-to-integer s :big-endian t))

(defun bytes-to-long-le (s)
  (octets-to-integer s :big-endian nil))

(defun long-to-bytes (n &optional nbytes)
  (crypto:integer-to-octets n :big-endian t
			    :n-bits (if nbytes (* 8 nbytes))))

(defun long-to-bytes-le (n &optional nbytes)
  (crypto:integer-to-octets n :big-endian nil
			    :n-bits (if nbytes (* 8 nbytes))))

(defun long-to-hex (n)
  (crypto:byte-array-to-hex-string
   (crypto:integer-to-octets n)))

(defun bytes-to-hex (s)
  (crypto:byte-array-to-hex-string s))

(defun str-to-bytes (s)
  (flex:string-to-octets s :external-format :utf8))

(defun bytes-to-str (s)
  ;; XXX: what to do here with external format?
  (handler-case
      (flex:octets-to-string s :external-format :utf8)
    (flexi-streams:external-format-encoding-error ()
      (flex:octets-to-string s :external-format :latin1))))

(defun hex-to-bytes (h)
  (crypto:hex-string-to-byte-array h))

(defun hex-to-long (h)
  (octets-to-integer
   (crypto:hex-string-to-byte-array
    (if (zerop (mod (length h) 2))
	h
	(format nil "0~a" h)))))

(defun bytes-to-int (s)
  (let ((x (octets-to-integer s :big-endian t)))
    (case (length s)
      (4  (if (< x #.(ash 1 31))  x (- x #.(ash 1 32))))
      (8  (if (< x #.(ash 1 63))  x (- x #.(ash 1 64))))
      (2  (if (< x #.(ash 1 15))  x (- x #.(ash 1 16))))
      (1  (if (< x #.(ash 1 7))   x (- x #.(ash 1 8))))
      (16 (if (< x #.(ash 1 127)) x (- x #.(ash 1 128))))
      (otherwise x))))

(defun bytes-to-int-le (s)
  (let ((x (octets-to-integer s :big-endian nil)))
    (case (length s)
      (4  (if (< x #.(ash 1 31))  x (- x #.(ash 1 32))))
      (8  (if (< x #.(ash 1 63))  x (- x #.(ash 1 64))))
      (2  (if (< x #.(ash 1 15))  x (- x #.(ash 1 16))))
      (1  (if (< x #.(ash 1 7))   x (- x #.(ash 1 8))))
      (16 (if (< x #.(ash 1 127)) x (- x #.(ash 1 128))))
      (otherwise x))))



(defmacro string+= (v &rest args)
  `(setf ,v (concatenate 'string ,v ,@args)))


(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) `(,structure))
        (t (mapcan #'flatten structure))))


