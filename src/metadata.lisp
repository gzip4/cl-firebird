
(in-package #:cl-firebird)


(defun list-all-tables (&optional conn)
  "Returns a list of all user-defined relations."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS
WHERE RDB$SYSTEM_FLAG = 0 ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-tables/system (&optional conn)
  "Returns a list of all system (virtual) relations."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$RELATION_NAME FROM RDB$RELATIONS
WHERE RDB$SYSTEM_FLAG = 1 ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-table-fields (table &optional conn)
  "Returns a list of XSQLVARs of a table."
  (with-transaction (conn)
    (with-statement (s (format nil "SELECT * FROM ~a" table))
      (copy-seq (statement-xsqlda s)))))


(defun list-character-sets (&optional conn)
  "Returns a list of all system character sets."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$CHARACTER_SET_NAME FROM RDB$CHARACTER_SETS
WHERE RDB$SYSTEM_FLAG = 1 ORDER BY 1")
      (mapcar (lambda (x) (intern x :keyword))
	      (flatten (fetch (execute s)))))))

(defun list-character-sets-info (&optional conn)
  "Returns a list of all system character sets info (id, bytes-per-character)."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$CHARACTER_SET_NAME,RDB$CHARACTER_SET_ID,RDB$BYTES_PER_CHARACTER
FROM RDB$CHARACTER_SETS WHERE RDB$SYSTEM_FLAG = 1 ORDER BY 1")
      (loop :for (cs id bpc) :in (fetch (execute s))
	 :collect (intern cs :keyword)
	 :collect (cons id bpc)))))

(defun select-from-database (&rest args)
  "Select one row from a database in current transaction."
  (let ((sql (format nil "SELECT ~{~a~^, ~} FROM RDB$DATABASE"
		     (loop :for x :in args
			:collect (typecase x
				   (null :null)
				   (list (format nil "~a as ~a" (second x) (first x)))
				   (t x))))))
    (with-statement (s sql)
      (values (fetch1 (execute s)) sql))))
