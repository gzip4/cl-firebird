
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

