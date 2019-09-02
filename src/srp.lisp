
#|
# This SRP implementation is in reference to
'''
Following document was copied from <http://srp.stanford.edu/design.html>.
-----
SRP Protocol Design

SRP is the newest addition to a new class of strong authentication protocols
that resist all the well-known passive and active attacks over the network. SRP
borrows some elements from other key-exchange and identification protcols and
adds some subtlee modifications and refinements. The result is a protocol that
preserves the strength and efficiency of the EKE family protocols while fixing
some of their shortcomings.

The following is a description of SRP-6 and 6a, the latest versions of SRP:

  N    A large safe prime (N = 2q+1, where q is prime)
       All arithmetic is done modulo N.
  g    A generator modulo N
  k    Multiplier parameter (k = H(N, g) in SRP-6a, k = 3 for legacy SRP-6)
  s    User's salt
  I    Username
  p    Cleartext Password
  H()  One-way hash function
  ^    (Modular) Exponentiation
  u    Random scrambling parameter
  a,b  Secret ephemeral values
  A,B  Public ephemeral values
  x    Private key (derived from p and s)
  v    Password verifier

The host stores passwords using the following formula:

  x = H(s, p)               (s is chosen randomly)
  v = g^x                   (computes password verifier)

The host then keeps {I, s, v} in its password database. The authentication
protocol itself goes as follows:

User -> Host:  I, A = g^a                  (identifies self, a = random number)
Host -> User:  s, B = kv + g^b             (sends salt, b = random number)

        Both:  u = H(A, B)

        User:  x = H(s, p)                 (user enters password)
        User:  S = (B - kg^x) ^ (a + ux)   (computes session key)
        User:  K = H(S)

        Host:  S = (Av^u) ^ b              (computes session key)
        Host:  K = H(S)

Now the two parties have a shared, strong session key K. To complete
authentication, they need to prove to each other that their keys match.
One possible way:

User -> Host:  M = H(H(N) xor H(g), H(I), s, A, B, K)
Host -> User:  H(A, M, K)

The two parties also employ the following safeguards:

  1. The user will abort if he receives B == 0 (mod N) or u == 0.
  2. The host will abort if it detects that A == 0 (mod N).
  3. The user must show his proof of K first. If the server detects that the user's proof is incorrect, it must abort without showing its own proof of K.

See http://srp.stanford.edu/ for more information.
|#

(in-package #:cl-firebird)


(defparameter *debug* nil)
(defparameter *DEBUG-PRIVATE-KEY* #x60975527035CF2AD1989806F0407210BC81EDC04E2762A56AFD529DDDA2D4393)

(defconstant +SRP-KEY-SIZE+ 128)
(defconstant +SRP-SALT-SIZE+ 32)



(defun pow% (x y p)
  "res = (x ^ y) % p
https://www.geeksforgeeks.org/modular-exponentiation-power-in-modular-arithmetic/"
  (declare (type integer x y p))
  (loop with res = 1
     with x = (mod x p)
     for i from 0
     while (> y 0)
     do (progn
	  (unless (zerop (logand y 1))
	    (setf res (mod (* res x) p)))
	  (setf y (ash y -1))
	  (setf x (mod (* x x) p)))
     finally (return res)))


(defun get-prime ()
  (let ((N #xE67D2E994B2F900C3F41F08F5BB2627ED0D49EE1FE767A52EFCD565CD6E768812C3E1E9CE8F0A8BEA6CB13CD29DDEBF7A96D4A93B55D488DF099A15C89DCB0640738EB2CBDD9A8F7BAB561AB1B0DC1C6CDABF303264A08D1BCA932D1F1EE428B619D970F342ABA9A65793B8B2F041AE5364350C16F735F56ECBCA87BD57B29E7)
	(g 2)
	;; #k = bytes2long(sha1(pad(N, SRP_KEY_SIZE), pad(g, SRP_KEY_SIZE)))
	(k 1277432915985975349439481660349303019122249719989))
    (values N g k)))


(defun pad (n)
  (flex:with-output-to-sequence (s)
    (let ((r))
      (dotimes (x +SRP-KEY-SIZE+)
	(push (logand n #xff) r)
	(setf n (ash n -8))
	(when (zerop n) (return)))
      (write-sequence r s))))


(defun hash-digest (algo &rest args)
  (crypto:digest-sequence
   algo
   (coerce
    (flex:with-output-to-sequence (s)
      (loop :for x :in args
	 :do (etypecase x
	       (sequence (write-sequence x s))
	       (integer (write-sequence (long-to-bytes x) s)))))
    '(simple-array (unsigned-byte 8)))))


(defun get-scramble (x y)
  (bytes-to-long (hash-digest :sha1 (pad x) (pad y))))


(defun get-user-hash (salt user password)
  (let ((hash1 (hash-digest :sha1 user (str-to-bytes ":") password)))
    (bytes-to-long (hash-digest :sha1 salt hash1))))


(defun client-seed ()
  (multiple-value-bind (N g)
      (get-prime)
    (let* ((private-key (if *debug*
			    *debug-private-key*
			    (random (ash 1 +SRP-KEY-SIZE+))))
	   (public-key (pow% g private-key N)))
      (log:trace "a=~a~%A=~a"
		 (long-to-hex private-key)
		 (long-to-hex public-key))
      (values public-key private-key))))  ; A a


(defun server-seed (v)
  (multiple-value-bind (N g k)
      (get-prime)
    (let* ((b (if *debug*
		  *debug-private-key*
		  (random (ash 1 +SRP-KEY-SIZE+))))
	   (gb (pow% g b N))
	   (kv (mod (* k v) N))
	   (private-key b)
	   (public-key (mod (+ kv gb) N)))
      (log:trace "b=~a~%B=~a"
		 (long-to-hex private-key)
		 (long-to-hex public-key))
      (values public-key private-key))))  ; B b


(defun client-session (user password salt A$ B$ a)
  (multiple-value-bind (N g k)
      (get-prime)
    (let* ((u    (get-scramble A$ B$))
	   (x    (get-user-hash salt user password))
	   (gx   (pow% g x N))
	   (kgx  (mod (* k gx) N))
	   (diff (mod (- B$ kgx) N))
	   (ux   (mod (* u x) N))
	   (aux  (mod (+ a ux) N))
	   (session-secret (pow% diff aux N))
	   (K$   (hash-digest :sha1 session-secret)))
      (log:trace "B=~a" (long-to-hex B$))
      (log:trace "u=~a" (long-to-hex u))
      (log:trace "x=~a" (long-to-hex x))
      (log:trace "gx=~a" (long-to-hex gx))
      (log:trace "kgx=~a" (long-to-hex kgx))
      (log:trace "diff=~a" (long-to-hex diff))
      (log:trace "ux=~a" (long-to-hex ux))
      (log:trace "aux=~a" (long-to-hex aux))
      (log:trace "session-secret=~a" (long-to-hex session-secret))
      (log:trace "session_key:K=~a" (bytes-to-hex K$))
      (values K$))))


(defun get-verifier (user password salt)
  (multiple-value-bind (N g)
      (get-prime)
    (let ((x (get-user-hash salt user password)))
      (values (pow% g x N)))))


(defun server-session (user password salt A$ B$ b)
  (multiple-value-bind (N)
      (get-prime)
    (let* ((u    (get-scramble A$ B$))
	   (v    (get-verifier  user password salt))
	   (vu   (pow% v u N))
	   (A$vu (mod (* A$ vu) N))
	   (session-secret (pow% A$vu b N))
	   (K$   (hash-digest :sha1 session-secret)))
      (log:trace "server session_secret=~a" (long-to-hex session-secret))
      (log:trace "server session hash K=~a" (bytes-to-hex K$))
      (values K$))))


(defun get-salt ()
  (let ((salt (if *debug*
		  (long-to-bytes #x02E268803000000079A478A700000002D1A6979000000026E1601C000000054F)
		  (flex:with-output-to-sequence (s)
		    (loop for x from 1 to +SRP-SALT-SIZE+
		       do (write-byte (random 256) s))))))
    (log:trace "salt=~a" (bytes-to-hex salt))
    (values salt)))


(defun client-proof (user password salt A$ B$ a hash-algo)
  "    M = H(H(N) xor H(g), H(I), s, A, B, K)"
  (multiple-value-bind (N g)
      (get-prime)
    (let ((K$ (client-session user password salt A$ B$ a))
	  (n1 (bytes-to-long (hash-digest :sha1 N)))
	  (n2 (bytes-to-long (hash-digest :sha1 g))))
      (log:trace "n1-1=~a" (long-to-hex n1))
      (log:trace "n2-1=~a" (long-to-hex n2))
      (let* ((n1 (pow% n1 n2 N))
	     (n2 (bytes-to-long (hash-digest :sha1 user)))
	     (M$ (hash-digest hash-algo n1 n2 salt A$ B$ K$)))
	(log:trace "n1-2=~a" (long-to-hex n1))
	(log:trace "n2-2=~a" (long-to-hex n2))
	(log:trace "client_proof:M=~a" (bytes-to-hex M$))
	(values M$ K$)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %srp-test ()
  (let ((user (str-to-bytes "SYSDBA"))
	(password (str-to-bytes "masterkey")))
    (multiple-value-bind (A$ a)
	(client-seed)
      (let* ((salt (get-salt))
	     (v (get-verifier user password salt)))
	(multiple-value-bind (B$ b)
	    (server-seed v)
	  (let ((server-key (server-session user password salt A$ B$ b)))
	    (multiple-value-bind (M$ client-key)
		(client-proof user password salt A$ B$ a :sha1)
	      (declare (ignorable M$))
	      (log:debug "1: ~a ~a ~a" (equalp client-key server-key) client-key server-key))
	    (multiple-value-bind (M$ client-key)
		(client-proof user password salt A$ B$ a :sha256)
	      (declare (ignorable M$))
	      (log:debug "2: ~a ~a ~a" (equalp client-key server-key) client-key server-key)))))))
  (values))




#|

python pow() algorithm

def power(x, y, p) : 
    res = 1     # Initialize result 
  
    # Update x if it is more 
    # than or equal to p 
    x = x % p  
  
    while (y > 0) : 
          
        # If y is odd, multiply 
        # x with result 
        if ((y & 1) == 1) : 
            res = (res * x) % p 
  
        # y must be even now 
        y = y >> 1      # y = y/2 
        x = (x * x) % p 
          
    return res 

|#
