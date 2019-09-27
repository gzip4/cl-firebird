
;;; https://github.com/sharplispers/ironclad
;;; https://www.cliki.net/Ironclad

(in-package #:cl-firebird)

(defun octets-to-integer (octet-vec &key (start 0) end (big-endian t) n-bits)
  (when (typep octet-vec '(simple-array (unsigned-byte 8)))
    (return-from octets-to-integer
      (crypto:octets-to-integer octet-vec :start start :end end
				:big-endian big-endian :n-bits n-bits)))
  (let ((end (or end (length octet-vec))))
    (multiple-value-bind (n-bits n-bytes)
        (let ((size (- end start)))
          (if n-bits
              (values n-bits (min (ceiling n-bits 8) size))
              (values (* 8 size) size)))
      (let ((sum (if big-endian
                     (loop with sum = 0
                           for i from (- end n-bytes) below end
                           do (setf sum (+ (ash sum 8) (aref octet-vec i)))
                           finally (return sum))
                     (loop for i from start below (+ start n-bytes)
                           for j from 0 by 8
                           sum (ash (aref octet-vec i) j)))))
        (ldb (byte n-bits 0) sum)))))


(defun integer-to-octets (bignum &key n-bits (big-endian t))
  (let* ((n-bits (or n-bits (integer-length bignum)))
         (bignum (ldb (byte n-bits 0) bignum))
         (n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec))
    (if big-endian
        (loop for i from (1- n-bytes) downto 0
              for index from 0
              do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
              finally (return octet-vec))
        (loop for i from 0 below n-bytes
              for byte from 0 by 8
              do (setf (aref octet-vec i) (ldb (byte 8 byte) bignum))
              finally (return octet-vec)))))


(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
       for i from start below end
       for j from 0 below (* length 2) by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref hexdigits (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref hexdigits (ldb (byte 4 0) byte))))
       finally (return string))))


(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (or (position char "0123456789abcdef" :test #'char-equal)
                 (error 'crypto:ironclad-error
                        :format-control "~A is not a hex digit"
                        :format-arguments (list char)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
         finally (return key)))))


(defun ascii-string-to-byte-array (string &key (start 0) end)
  "Convert STRING to a (VECTOR (UNSIGNED-BYTE 8)).  It is an error if
STRING contains any character whose CHAR-CODE is greater than 255."
  (let* ((length (length string))
         (vec (make-array length :element-type '(unsigned-byte 8)))
         (end (or end length)))
    (loop for i from start below end do
          (let ((byte (char-code (char string i))))
            (unless (< byte 256)
              (error 'crypto:ironclad-error
                     :format-control "~A is not an ASCII character"
                     :format-arguments (list (char string i))))
            (setf (aref vec i) byte))
          finally (return vec))))

