
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
  "Returns a plist of all system character sets info (id, bytes-per-character)."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$CHARACTER_SET_NAME,RDB$CHARACTER_SET_ID,RDB$BYTES_PER_CHARACTER
FROM RDB$CHARACTER_SETS WHERE RDB$SYSTEM_FLAG = 1 ORDER BY 1")
      (loop :for (cs id bpc) :in (fetch (execute s))
	 :collect (intern cs :keyword)
	 :collect (cons id bpc)))))


(defun list-all-collations (&optional conn)
  "Returns a list of all collations."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$COLLATION_NAME FROM RDB$COLLATIONS ORDER BY 1")
      (mapcar (lambda (x) (intern x :keyword))
	      (flatten (fetch (execute s)))))))


(defun list-all-procedures (&optional conn)
  "Returns a list of all PSQL procedures."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$PROCEDURE_NAME FROM RDB$PROCEDURES ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-functions (&optional conn)
  "Returns a list of all PSQL functions."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$FUNCTION_NAME FROM RDB$FUNCTIONS ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-indices (&optional conn)
  "Returns a list of all indices."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$INDEX_NAME FROM RDB$INDICES ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-triggers (&optional conn)
  "Returns a list of all user-defined triggers."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$TRIGGER_NAME FROM RDB$TRIGGERS ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-generators (&optional conn)
  "Returns a list of all user-defined generators."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-check-constraints (&optional conn)
  "Returns a list of all user-defined check constraints."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$CONSTRAINT_NAME FROM RDB$CHECK_CONSTRAINTS ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-exceptions (&optional conn)
  "Returns a list of all PSQL exceptions."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$EXCEPTION_NAME FROM RDB$EXCEPTIONS ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-roles (&optional conn)
  "Returns a list of all roles."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$ROLE_NAME FROM RDB$ROLES ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-psql-packages (&optional conn)
  "Returns a list of all PSQL packages."
  (with-transaction (conn)
    (with-statement (s "SELECT RDB$PACKAGE_NAME FROM RDB$PACKAGES ORDER BY 1")
      (flatten (fetch (execute s))))))


(defun list-all-domains (&optional conn)
  "Returns a list of all user-defined domains as (name type length)."
  (with-transaction (conn)
    (with-statement (s "SELECT
T.RDB$FIELD_NAME                     NAME,
CASE T.RDB$FIELD_TYPE
    WHEN 7 THEN 'SMALLINT'
    WHEN 8 THEN 'INTEGER'
    WHEN 10 THEN 'FLOAT'
    WHEN 12 THEN 'DATE'
    WHEN 13 THEN 'TIME'
    WHEN 14 THEN 'CHAR'
    WHEN 16 THEN 'BIGINT'
    WHEN 27 THEN 'DOUBLE PRECISION'
    WHEN 35 THEN 'TIMESTAMP'
    WHEN 37 THEN 'VARCHAR'
    WHEN 261 THEN 'BLOB'
END                                  TYPE_NAME,
T.RDB$CHARACTER_LENGTH               CHR_LENGTH
FROM RDB$FIELDS T
WHERE COALESCE(RDB$SYSTEM_FLAG, 0) = 0 
    AND NOT (RDB$FIELD_NAME STARTING WITH 'RDB$')")
      (let ((r (fetch (execute s))))
	(values r)))))


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



