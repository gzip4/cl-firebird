
(in-package #:cl-firebird)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'repr) #'write-to-string))

(defun pad4-bytes (x)
  (let ((m (mod x 4)))
    (if (zerop m)
	#()
	(subseq #(0 0 0) 0 (- 4 m)))))

(defun %write-array-padding (stream array-length)
  (let ((m (mod array-length 4)))
    (unless (zerop m)
      (write-sequence (subseq #(0 0 0) 0 (- 4 m)) stream))))

(defmacro with-xdr (&body body)
  (let* ((stream (gensym "STREAM")))
    `(flexi-streams:with-output-to-sequence
      (,stream)
      (let ((*xdr* ,stream))
	,@body))))

(declaim (inline xdr-int32 xdr-uint32))

(defun xdr-int32 (x)
  (nibbles:write-sb32/be x *xdr*))

(defun xdr-uint32 (x)
  (nibbles:write-ub32/be x *xdr*))

(defun xdr-octets (octets)
  (let ((len (length octets)))
    (nibbles:write-sb32/be len *xdr*)
    (write-sequence octets *xdr*)
    (%write-array-padding *xdr* len)))

(defun xdr-string (string &key (external-format :utf-8))
  (let ((octets (flex:string-to-octets string :external-format external-format)))
    (xdr-octets octets)))


(defun append-bytes (stream &rest args)
  (loop :for x :in args
     :do (etypecase x
	   (null nil)
	   ((unsigned-byte 8)  (write-byte x stream))
	   ((signed-byte 8)    (write-byte ( + 256 x) stream))
	   ((unsigned-byte 16) (nibbles:write-ub16/be x stream))
	   ((signed-byte 16)   (nibbles:write-sb16/be x stream))
	   ((unsigned-byte 32) (nibbles:write-ub32/be x stream))
	   ((signed-byte 32)   (nibbles:write-sb32/be x stream))
	   ((unsigned-byte 64) (nibbles:write-ub64/be x stream))
	   ((signed-byte 64)   (nibbles:write-sb64/be x stream))
	   ((or vector array list) (write-sequence x stream)))))


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


(declaim (inline seq-to-bytes bytes-to-long bytes-to-long-le long-to-bytes long-to-bytes-le
		 long-to-hex bytes-to-hex str-to-bytes bytes-to-str
		 hex-to-bytes hex-to-long signed-int32 str
		 byte-stream byte-stream-output))

(defun byte-stream ()
  (flexi-streams:make-in-memory-output-stream))

(defun byte-stream-output (s)
  (flexi-streams:get-output-stream-sequence s))

(defmacro with-byte-stream ((var) &body body)
  (let ((bs (gensym "BYTE-STREAM")))
    `(let ((,bs (byte-stream)))
       (unwind-protect
	    (let ((,var ,bs))
	      (declare (ignorable ,var))
	      ,@body
	      (byte-stream-output ,bs))
	 (when ,bs (close ,bs))))))


(defun str (x)
  (typecase x
    (string x)
    (t (write-to-string x))))


(defun seq-to-bytes (seq)
  (coerce seq '(vector (unsigned-byte 8))))
;;  (coerce seq '(simple-array (unsigned-byte 8) *)))

(defun bytes-to-long (s)
  (ironclad:octets-to-integer (seq-to-bytes s) :big-endian t))

(defun bytes-to-long-le (s)
  (ironclad:octets-to-integer (seq-to-bytes s) :big-endian nil))

(defun long-to-bytes (n &optional nbytes)
  (ironclad:integer-to-octets n :big-endian t
			      :n-bits (if nbytes (* 8 nbytes))))

(defun long-to-bytes-le (n &optional nbytes)
  (ironclad:integer-to-octets n :big-endian nil
			      :n-bits (if nbytes (* 8 nbytes))))

(defun bytes-to-long/naive (s)
  (loop with r = 0
     for c across s
     do (progn (setf r (+ (ash r 8) c)))
     finally (return r)))

(defun long-to-bytes/naive (n)
  (flex:with-output-to-sequence (s)
    (loop with r = (list)
       while (> n 0)
       do (push (logand n #xff) r)
       do (setf n (ash n -8))
       finally (write-sequence r s))))

(defun long-to-hex (n)
  (ironclad:byte-array-to-hex-string
   (ironclad:integer-to-octets n)))

(defun bytes-to-hex (s)
  (ironclad:byte-array-to-hex-string s))

(defun str-to-bytes (s)
  (flex:string-to-octets s :external-format :utf8))

(defun bytes-to-str (s)
  ;; XXX: what to do here with external format?
  (handler-case
      (flex:octets-to-string s :external-format :utf8)
    (flexi-streams:external-format-encoding-error ()
      (flex:octets-to-string s :external-format :latin1))))

(defun hex-to-bytes (h)
  (ironclad:hex-string-to-byte-array h))

(defun hex-to-long (h)
  (ironclad:octets-to-integer
   (ironclad:hex-string-to-byte-array
    (if (zerop (mod (length h) 2))
	h
	(with-output-to-string (s)
          (write-char #\0 s)
          (write-string h s))))))


(defun unsigned-to-signed-int (x &optional (scale 0))
  (let* ((signed (etypecase x
		   ((unsigned-byte   8) (if (< x #.(ash 1 7))   x (- x #.(ash 1 8))))
		   ((unsigned-byte  16) (if (< x #.(ash 1 15))  x (- x #.(ash 1 16))))
		   ((unsigned-byte  32) (if (< x #.(ash 1 31))  x (- x #.(ash 1 32))))
		   ((unsigned-byte  64) (if (< x #.(ash 1 63))  x (- x #.(ash 1 64))))
		   ((unsigned-byte 128) (if (< x #.(ash 1 127)) x (- x #.(ash 1 128)))))))
    (if (zerop scale)
	signed
	(/ signed (expt 10 (abs scale))))))


(defun signed-int32 (x)
  (if (<= x 0)
      x
      (if (<= x #x7fffffff)
	  x
	  (- x #x100000000))))


(defmacro string+= (v &rest args)
  `(setf ,v (concatenate 'string ,v ,@args)))


;; deprecated
(defmacro with-bytes (&body body)
  (let ((out (gensym "BYTE-STREAM")))
  `(flex:with-output-to-sequence
    (,out)
    (flet ((<< (&rest args)
	     (loop :for x :in args
		:do (typecase x
		      (null)
		      (integer (write-byte (if (< x 0) (+ 256 x) x) ,out))
		      (t (write-sequence x ,out))))))
      ,@body))))




;; ????
#+nil
(defun bs (&rest byte-array)
  (make-array (length byte-array)
	      :element-type '(unsigned-byte 8)
	      :adjustable t
	      :fill-pointer t
	      :initial-contents byte-array))


