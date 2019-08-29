;;;;
;;; The contents of this file are subject to the Interbase Public
;;; License Version 1.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy
;;; of the License at http://www.Inprise.com/IPL.html
;;;
;;; Software distributed under the License is distributed on an
;;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;;; or implied. See the License for the specific language governing
;;; rights and limitations under the License.

(in-package #:cl-firebird)


(defparameter *messages* (make-hash-table :size 1284))

(eval-when (:load-toplevel :execute)
  (setf (gethash 335544321 *messages*) "arithmetic exception, numeric overflow, or string truncation
")
  (setf (gethash 335544322 *messages*) "invalid database key
")
  (setf (gethash 335544323 *messages*) "file @1 is not a valid database
")
  (setf (gethash 335544324 *messages*) "invalid database handle (no active connection)
")
  (setf (gethash 335544325 *messages*) "bad parameters on attach or create database
")
  (setf (gethash 335544326 *messages*) "unrecognized database parameter block
")
  (setf (gethash 335544327 *messages*) "invalid request handle
")
  (setf (gethash 335544328 *messages*) "invalid BLOB handle
")
  (setf (gethash 335544329 *messages*) "invalid BLOB ID
")
  (setf (gethash 335544330 *messages*) "invalid parameter in transaction parameter block
")
  (setf (gethash 335544331 *messages*) "invalid format for transaction parameter block
")
  (setf (gethash 335544332 *messages*) "invalid transaction handle (expecting explicit transaction start)
")
  (setf (gethash 335544333 *messages*) "internal Firebird consistency check (@1)
")
  (setf (gethash 335544334 *messages*) "conversion error from string \"@1\"
")
  (setf (gethash 335544335 *messages*) "database file appears corrupt (@1)
")
  (setf (gethash 335544336 *messages*) "deadlock
")
  (setf (gethash 335544337 *messages*) "attempt to start more than @1 transactions
")
  (setf (gethash 335544338 *messages*) "no match for first value expression
")
  (setf (gethash 335544339 *messages*) "information type inappropriate for object specified
")
  (setf (gethash 335544340 *messages*) "no information of this type available for object specified
")
  (setf (gethash 335544341 *messages*) "unknown information item
")
  (setf (gethash 335544342 *messages*) "action cancelled by trigger (@1) to preserve data integrity
")
  (setf (gethash 335544343 *messages*) "invalid request BLR at offset @1
")
  (setf (gethash 335544344 *messages*) "I/O error during \"@1\" operation for file \"@2\"
")
  (setf (gethash 335544345 *messages*) "lock conflict on no wait transaction
")
  (setf (gethash 335544346 *messages*) "corrupt system table
")
  (setf (gethash 335544347 *messages*) "validation error for column @1, value \"@2\"
")
  (setf (gethash 335544348 *messages*) "no current record for fetch operation
")
  (setf (gethash 335544349 *messages*) "attempt to store duplicate value (visible to active transactions) in unique index \"@1\"
")
  (setf (gethash 335544350 *messages*) "program attempted to exit without finishing database
")
  (setf (gethash 335544351 *messages*) "unsuccessful metadata update
")
  (setf (gethash 335544352 *messages*) "no permission for @1 access to @2 @3
")
  (setf (gethash 335544353 *messages*) "transaction is not in limbo
")
  (setf (gethash 335544354 *messages*) "invalid database key
")
  (setf (gethash 335544355 *messages*) "BLOB was not closed
")
  (setf (gethash 335544356 *messages*) "metadata is obsolete
")
  (setf (gethash 335544357 *messages*) "cannot disconnect database with open transactions (@1 active)
")
  (setf (gethash 335544358 *messages*) "message length error (encountered @1, expected @2)
")
  (setf (gethash 335544359 *messages*) "attempted update of read-only column
")
  (setf (gethash 335544360 *messages*) "attempted update of read-only table
")
  (setf (gethash 335544361 *messages*) "attempted update during read-only transaction
")
  (setf (gethash 335544362 *messages*) "cannot update read-only view @1
")
  (setf (gethash 335544363 *messages*) "no transaction for request
")
  (setf (gethash 335544364 *messages*) "request synchronization error
")
  (setf (gethash 335544365 *messages*) "request referenced an unavailable database
")
  (setf (gethash 335544366 *messages*) "segment buffer length shorter than expected
")
  (setf (gethash 335544367 *messages*) "attempted retrieval of more segments than exist
")
  (setf (gethash 335544368 *messages*) "attempted invalid operation on a BLOB
")
  (setf (gethash 335544369 *messages*) "attempted read of a new, open BLOB
")
  (setf (gethash 335544370 *messages*) "attempted action on BLOB outside transaction
")
  (setf (gethash 335544371 *messages*) "attempted write to read-only BLOB
")
  (setf (gethash 335544372 *messages*) "attempted reference to BLOB in unavailable database
")
  (setf (gethash 335544373 *messages*) "operating system directive @1 failed
")
  (setf (gethash 335544374 *messages*) "attempt to fetch past the last record in a record stream
")
  (setf (gethash 335544375 *messages*) "unavailable database
")
  (setf (gethash 335544376 *messages*) "table @1 was omitted from the transaction reserving list
")
  (setf (gethash 335544377 *messages*) "request includes a DSRI extension not supported in this implementation
")
  (setf (gethash 335544378 *messages*) "feature is not supported
")
  (setf (gethash 335544379 *messages*) "unsupported on-disk structure for file @1; found @2.@3, support @4.@5
")
  (setf (gethash 335544380 *messages*) "wrong number of arguments on call
")
  (setf (gethash 335544381 *messages*) "Implementation limit exceeded
")
  (setf (gethash 335544382 *messages*) "@1
")
  (setf (gethash 335544383 *messages*) "unrecoverable conflict with limbo transaction @1
")
  (setf (gethash 335544384 *messages*) "internal error
")
  (setf (gethash 335544385 *messages*) "internal error
")
  (setf (gethash 335544386 *messages*) "too many requests
")
  (setf (gethash 335544387 *messages*) "internal error
")
  (setf (gethash 335544388 *messages*) "block size exceeds implementation restriction
")
  (setf (gethash 335544389 *messages*) "buffer exhausted
")
  (setf (gethash 335544390 *messages*) "BLR syntax error *messages*) expected @1 at offset @2, encountered @3
")
  (setf (gethash 335544391 *messages*) "buffer in use
")
  (setf (gethash 335544392 *messages*) "internal error
")
  (setf (gethash 335544393 *messages*) "request in use
")
  (setf (gethash 335544394 *messages*) "incompatible version of on-disk structure
")
  (setf (gethash 335544395 *messages*) "table @1 is not defined
")
  (setf (gethash 335544396 *messages*) "column @1 is not defined in table @2
")
  (setf (gethash 335544397 *messages*) "internal error
")
  (setf (gethash 335544398 *messages*) "internal error
")
  (setf (gethash 335544399 *messages*) "internal error
")
  (setf (gethash 335544400 *messages*) "internal error
")
  (setf (gethash 335544401 *messages*) "internal error
")
  (setf (gethash 335544402 *messages*) "internal error
")
  (setf (gethash 335544403 *messages*) "page @1 is of wrong type (expected @2, found @3)
")
  (setf (gethash 335544404 *messages*) "database corrupted
")
  (setf (gethash 335544405 *messages*) "checksum error on database page @1
")
  (setf (gethash 335544406 *messages*) "index is broken
")
  (setf (gethash 335544407 *messages*) "database handle not zero
")
  (setf (gethash 335544408 *messages*) "transaction handle not zero
")
  (setf (gethash 335544409 *messages*) "transaction--request mismatch (synchronization error)
")
  (setf (gethash 335544410 *messages*) "bad handle count
")
  (setf (gethash 335544411 *messages*) "wrong version of transaction parameter block
")
  (setf (gethash 335544412 *messages*) "unsupported BLR version (expected @1, encountered @2)
")
  (setf (gethash 335544413 *messages*) "wrong version of database parameter block
")
  (setf (gethash 335544414 *messages*) "BLOB and array data types are not supported for @1 operation
")
  (setf (gethash 335544415 *messages*) "database corrupted
")
  (setf (gethash 335544416 *messages*) "internal error
")
  (setf (gethash 335544417 *messages*) "internal error
")
  (setf (gethash 335544418 *messages*) "transaction in limbo
")
  (setf (gethash 335544419 *messages*) "transaction not in limbo
")
  (setf (gethash 335544420 *messages*) "transaction outstanding
")
  (setf (gethash 335544421 *messages*) "connection rejected by remote interface
")
  (setf (gethash 335544422 *messages*) "internal error
")
  (setf (gethash 335544423 *messages*) "internal error
")
  (setf (gethash 335544424 *messages*) "no lock manager available
")
  (setf (gethash 335544425 *messages*) "context already in use (BLR error)
")
  (setf (gethash 335544426 *messages*) "context not defined (BLR error)
")
  (setf (gethash 335544427 *messages*) "data operation not supported
")
  (setf (gethash 335544428 *messages*) "undefined message number
")
  (setf (gethash 335544429 *messages*) "undefined parameter number
")
  (setf (gethash 335544430 *messages*) "unable to allocate memory from operating system
")
  (setf (gethash 335544431 *messages*) "blocking signal has been received
")
  (setf (gethash 335544432 *messages*) "lock manager error
")
  (setf (gethash 335544433 *messages*) "communication error with journal \"@1\"
")
  (setf (gethash 335544434 *messages*) "key size exceeds implementation restriction for index \"@1\"
")
  (setf (gethash 335544435 *messages*) "null segment of UNIQUE KEY
")
  (setf (gethash 335544436 *messages*) "SQL error code = @1
")
  (setf (gethash 335544437 *messages*) "wrong DYN version
")
  (setf (gethash 335544438 *messages*) "function @1 is not defined
")
  (setf (gethash 335544439 *messages*) "function @1 could not be matched
")
  (setf (gethash 335544440 *messages*) "
")
  (setf (gethash 335544441 *messages*) "database detach completed with errors
")
  (setf (gethash 335544442 *messages*) "database system cannot read argument @1
")
  (setf (gethash 335544443 *messages*) "database system cannot write argument @1
")
  (setf (gethash 335544444 *messages*) "operation not supported
")
  (setf (gethash 335544445 *messages*) "@1 extension error
")
  (setf (gethash 335544446 *messages*) "not updatable
")
  (setf (gethash 335544447 *messages*) "no rollback performed
")
  (setf (gethash 335544448 *messages*) "
")
  (setf (gethash 335544449 *messages*) "
")
  (setf (gethash 335544450 *messages*) "@1
")
  (setf (gethash 335544451 *messages*) "update conflicts with concurrent update
")
  (setf (gethash 335544452 *messages*) "product @1 is not licensed
")
  (setf (gethash 335544453 *messages*) "object @1 is in use
")
  (setf (gethash 335544454 *messages*) "filter not found to convert type @1 to type @2
")
  (setf (gethash 335544455 *messages*) "cannot attach active shadow file
")
  (setf (gethash 335544456 *messages*) "invalid slice description language at offset @1
")
  (setf (gethash 335544457 *messages*) "subscript out of bounds
")
  (setf (gethash 335544458 *messages*) "column not array or invalid dimensions (expected @1, encountered @2)
")
  (setf (gethash 335544459 *messages*) "record from transaction @1 is stuck in limbo
")
  (setf (gethash 335544460 *messages*) "a file in manual shadow @1 is unavailable
")
  (setf (gethash 335544461 *messages*) "secondary server attachments cannot validate databases
")
  (setf (gethash 335544462 *messages*) "secondary server attachments cannot start journaling
")
  (setf (gethash 335544463 *messages*) "generator @1 is not defined
")
  (setf (gethash 335544464 *messages*) "secondary server attachments cannot start logging
")
  (setf (gethash 335544465 *messages*) "invalid BLOB type for operation
")
  (setf (gethash 335544466 *messages*) "violation of FOREIGN KEY constraint \"@1\" on table \"@2\"
")
  (setf (gethash 335544467 *messages*) "minor version too high found @1 expected @2
")
  (setf (gethash 335544468 *messages*) "transaction @1 is @2
")
  (setf (gethash 335544469 *messages*) "transaction marked invalid and cannot be committed
")
  (setf (gethash 335544470 *messages*) "cache buffer for page @1 invalid
")
  (setf (gethash 335544471 *messages*) "there is no index in table @1 with id @2
")
  (setf (gethash 335544472 *messages*) "Your user name and password are not defined. Ask your database administrator to set up a Firebird login.
")
  (setf (gethash 335544473 *messages*) "invalid bookmark handle
")
  (setf (gethash 335544474 *messages*) "invalid lock level @1
")
  (setf (gethash 335544475 *messages*) "lock on table @1 conflicts with existing lock
")
  (setf (gethash 335544476 *messages*) "requested record lock conflicts with existing lock
")
  (setf (gethash 335544477 *messages*) "maximum indexes per table (@1) exceeded
")
  (setf (gethash 335544478 *messages*) "enable journal for database before starting online dump
")
  (setf (gethash 335544479 *messages*) "online dump failure. Retry dump
")
  (setf (gethash 335544480 *messages*) "an online dump is already in progress
")
  (setf (gethash 335544481 *messages*) "no more disk/tape space.  Cannot continue online dump
")
  (setf (gethash 335544482 *messages*) "journaling allowed only if database has Write-ahead Log
")
  (setf (gethash 335544483 *messages*) "maximum number of online dump files that can be specified is 16
")
  (setf (gethash 335544484 *messages*) "error in opening Write-ahead Log file during recovery
")
  (setf (gethash 335544485 *messages*) "invalid statement handle
")
  (setf (gethash 335544486 *messages*) "Write-ahead log subsystem failure
")
  (setf (gethash 335544487 *messages*) "WAL Writer error
")
  (setf (gethash 335544488 *messages*) "Log file header of @1 too small
")
  (setf (gethash 335544489 *messages*) "Invalid version of log file @1
")
  (setf (gethash 335544490 *messages*) "Log file @1 not latest in the chain but open flag still set
")
  (setf (gethash 335544491 *messages*) "Log file @1 not closed properly; database recovery may be required
")
  (setf (gethash 335544492 *messages*) "Database name in the log file @1 is different
")
  (setf (gethash 335544493 *messages*) "Unexpected end of log file @1 at offset @2
")
  (setf (gethash 335544494 *messages*) "Incomplete log record at offset @1 in log file @2
")
  (setf (gethash 335544495 *messages*) "Log record header too small at offset @1 in log file @2
")
  (setf (gethash 335544496 *messages*) "Log block too small at offset @1 in log file @2
")
  (setf (gethash 335544497 *messages*) "Illegal attempt to attach to an uninitialized WAL segment for @1
")
  (setf (gethash 335544498 *messages*) "Invalid WAL parameter block option @1
")
  (setf (gethash 335544499 *messages*) "Cannot roll over to the next log file @1
")
  (setf (gethash 335544500 *messages*) "database does not use Write-ahead Log
")
  (setf (gethash 335544501 *messages*) "cannot drop log file when journaling is enabled
")
  (setf (gethash 335544502 *messages*) "reference to invalid stream number
")
  (setf (gethash 335544503 *messages*) "WAL subsystem encountered error
")
  (setf (gethash 335544504 *messages*) "WAL subsystem corrupted
")
  (setf (gethash 335544505 *messages*) "must specify archive file when enabling long term journal for databases with round-robin log files
")
  (setf (gethash 335544506 *messages*) "database @1 shutdown in progress
")
  (setf (gethash 335544507 *messages*) "refresh range number @1 already in use
")
  (setf (gethash 335544508 *messages*) "refresh range number @1 not found
")
  (setf (gethash 335544509 *messages*) "CHARACTER SET @1 is not defined
")
  (setf (gethash 335544510 *messages*) "lock time-out on wait transaction
")
  (setf (gethash 335544511 *messages*) "procedure @1 is not defined
")
  (setf (gethash 335544512 *messages*) "Input parameter mismatch for procedure @1
")
  (setf (gethash 335544513 *messages*) "Database @1 *messages*) WAL subsystem bug for pid @2@3
")
  (setf (gethash 335544514 *messages*) "Could not expand the WAL segment for database @1
")
  (setf (gethash 335544515 *messages*) "status code @1 unknown
")
  (setf (gethash 335544516 *messages*) "exception @1 not defined
")
  (setf (gethash 335544517 *messages*) "exception @1
")
  (setf (gethash 335544518 *messages*) "restart shared cache manager
")
  (setf (gethash 335544519 *messages*) "invalid lock handle
")
  (setf (gethash 335544520 *messages*) "long-term journaling already enabled
")
  (setf (gethash 335544521 *messages*) "Unable to roll over please see Firebird log.
")
  (setf (gethash 335544522 *messages*) "WAL I/O error.  Please see Firebird log.
")
  (setf (gethash 335544523 *messages*) "WAL writer - Journal server communication error.  Please see Firebird log.
")
  (setf (gethash 335544524 *messages*) "WAL buffers cannot be increased.  Please see Firebird log.
")
  (setf (gethash 335544525 *messages*) "WAL setup error.  Please see Firebird log.
")
  (setf (gethash 335544526 *messages*) "obsolete
")
  (setf (gethash 335544527 *messages*) "Cannot start WAL writer for the database @1
")
  (setf (gethash 335544528 *messages*) "database @1 shutdown
")
  (setf (gethash 335544529 *messages*) "cannot modify an existing user privilege
")
  (setf (gethash 335544530 *messages*) "Cannot delete PRIMARY KEY being used in FOREIGN KEY definition.
")
  (setf (gethash 335544531 *messages*) "Column used in a PRIMARY constraint must be NOT NULL.
")
  (setf (gethash 335544532 *messages*) "Name of Referential Constraint not defined in constraints table.
")
  (setf (gethash 335544533 *messages*) "Non-existent PRIMARY or UNIQUE KEY specified for FOREIGN KEY.
")
  (setf (gethash 335544534 *messages*) "Cannot update constraints (RDB$REF_CONSTRAINTS).
")
  (setf (gethash 335544535 *messages*) "Cannot update constraints (RDB$CHECK_CONSTRAINTS).
")
  (setf (gethash 335544536 *messages*) "Cannot delete CHECK constraint entry (RDB$CHECK_CONSTRAINTS)
")
  (setf (gethash 335544537 *messages*) "Cannot delete index segment used by an Integrity Constraint
")
  (setf (gethash 335544538 *messages*) "Cannot update index segment used by an Integrity Constraint
")
  (setf (gethash 335544539 *messages*) "Cannot delete index used by an Integrity Constraint
")
  (setf (gethash 335544540 *messages*) "Cannot modify index used by an Integrity Constraint
")
  (setf (gethash 335544541 *messages*) "Cannot delete trigger used by a CHECK Constraint
")
  (setf (gethash 335544542 *messages*) "Cannot update trigger used by a CHECK Constraint
")
  (setf (gethash 335544543 *messages*) "Cannot delete column being used in an Integrity Constraint.
")
  (setf (gethash 335544544 *messages*) "Cannot rename column being used in an Integrity Constraint.
")
  (setf (gethash 335544545 *messages*) "Cannot update constraints (RDB$RELATION_CONSTRAINTS).
")
  (setf (gethash 335544546 *messages*) "Cannot define constraints on views
")
  (setf (gethash 335544547 *messages*) "internal Firebird consistency check (invalid RDB$CONSTRAINT_TYPE)
")
  (setf (gethash 335544548 *messages*) "Attempt to define a second PRIMARY KEY for the same table
")
  (setf (gethash 335544549 *messages*) "cannot modify or erase a system trigger
")
  (setf (gethash 335544550 *messages*) "only the owner of a table may reassign ownership
")
  (setf (gethash 335544551 *messages*) "could not find object for GRANT
")
  (setf (gethash 335544552 *messages*) "could not find column for GRANT
")
  (setf (gethash 335544553 *messages*) "user does not have GRANT privileges for operation
")
  (setf (gethash 335544554 *messages*) "object has non-SQL security class defined
")
  (setf (gethash 335544555 *messages*) "column has non-SQL security class defined
")
  (setf (gethash 335544556 *messages*) "Write-ahead Log without shared cache configuration not allowed
")
  (setf (gethash 335544557 *messages*) "database shutdown unsuccessful
")
  (setf (gethash 335544558 *messages*) "Operation violates CHECK constraint @1 on view or table @2
")
  (setf (gethash 335544559 *messages*) "invalid service handle
")
  (setf (gethash 335544560 *messages*) "database @1 shutdown in @2 seconds
")
  (setf (gethash 335544561 *messages*) "wrong version of service parameter block
")
  (setf (gethash 335544562 *messages*) "unrecognized service parameter block
")
  (setf (gethash 335544563 *messages*) "service @1 is not defined
")
  (setf (gethash 335544564 *messages*) "long-term journaling not enabled
")
  (setf (gethash 335544565 *messages*) "Cannot transliterate character between character sets
")
  (setf (gethash 335544566 *messages*) "WAL defined; Cache Manager must be started first
")
  (setf (gethash 335544567 *messages*) "Overflow log specification required for round-robin log
")
  (setf (gethash 335544568 *messages*) "Implementation of text subtype @1 not located.
")
  (setf (gethash 335544569 *messages*) "Dynamic SQL Error
")
  (setf (gethash 335544570 *messages*) "Invalid command
")
  (setf (gethash 335544571 *messages*) "Data type for constant unknown
")
  (setf (gethash 335544572 *messages*) "Invalid cursor reference
")
  (setf (gethash 335544573 *messages*) "Data type unknown
")
  (setf (gethash 335544574 *messages*) "Invalid cursor declaration
")
  (setf (gethash 335544575 *messages*) "Cursor @1 is not updatable
")
  (setf (gethash 335544576 *messages*) "Attempt to reopen an open cursor
")
  (setf (gethash 335544577 *messages*) "Attempt to reclose a closed cursor
")
  (setf (gethash 335544578 *messages*) "Column unknown
")
  (setf (gethash 335544579 *messages*) "Internal error
")
  (setf (gethash 335544580 *messages*) "Table unknown
")
  (setf (gethash 335544581 *messages*) "Procedure unknown
")
  (setf (gethash 335544582 *messages*) "Request unknown
")
  (setf (gethash 335544583 *messages*) "SQLDA error
")
  (setf (gethash 335544584 *messages*) "Count of read-write columns does not equal count of values
")
  (setf (gethash 335544585 *messages*) "Invalid statement handle
")
  (setf (gethash 335544586 *messages*) "Function unknown
")
  (setf (gethash 335544587 *messages*) "Column is not a BLOB
")
  (setf (gethash 335544588 *messages*) "COLLATION @1 for CHARACTER SET @2 is not defined
")
  (setf (gethash 335544589 *messages*) "COLLATION @1 is not valid for specified CHARACTER SET
")
  (setf (gethash 335544590 *messages*) "Option specified more than once
")
  (setf (gethash 335544591 *messages*) "Unknown transaction option
")
  (setf (gethash 335544592 *messages*) "Invalid array reference
")
  (setf (gethash 335544593 *messages*) "Array declared with too many dimensions
")
  (setf (gethash 335544594 *messages*) "Illegal array dimension range
")
  (setf (gethash 335544595 *messages*) "Trigger unknown
")
  (setf (gethash 335544596 *messages*) "Subselect illegal in this context
")
  (setf (gethash 335544597 *messages*) "Cannot prepare a CREATE DATABASE/SCHEMA statement
")
  (setf (gethash 335544598 *messages*) "must specify column name for view select expression
")
  (setf (gethash 335544599 *messages*) "number of columns does not match select list
")
  (setf (gethash 335544600 *messages*) "Only simple column names permitted for VIEW WITH CHECK OPTION
")
  (setf (gethash 335544601 *messages*) "No WHERE clause for VIEW WITH CHECK OPTION
")
  (setf (gethash 335544602 *messages*) "Only one table allowed for VIEW WITH CHECK OPTION
")
  (setf (gethash 335544603 *messages*) "DISTINCT, GROUP or HAVING not permitted for VIEW WITH CHECK OPTION
")
  (setf (gethash 335544604 *messages*) "FOREIGN KEY column count does not match PRIMARY KEY
")
  (setf (gethash 335544605 *messages*) "No subqueries permitted for VIEW WITH CHECK OPTION
")
  (setf (gethash 335544606 *messages*) "expression evaluation not supported
")
  (setf (gethash 335544607 *messages*) "gen.c *messages*) node not supported
")
  (setf (gethash 335544608 *messages*) "Unexpected end of command
")
  (setf (gethash 335544609 *messages*) "INDEX @1
")
  (setf (gethash 335544610 *messages*) "EXCEPTION @1
")
  (setf (gethash 335544611 *messages*) "COLUMN @1
")
  (setf (gethash 335544612 *messages*) "Token unknown
")
  (setf (gethash 335544613 *messages*) "union not supported
")
  (setf (gethash 335544614 *messages*) "Unsupported DSQL construct
")
  (setf (gethash 335544615 *messages*) "column used with aggregate
")
  (setf (gethash 335544616 *messages*) "invalid column reference
")
  (setf (gethash 335544617 *messages*) "invalid ORDER BY clause
")
  (setf (gethash 335544618 *messages*) "Return mode by value not allowed for this data type
")
  (setf (gethash 335544619 *messages*) "External functions cannot have more than 10 parameters
")
  (setf (gethash 335544620 *messages*) "alias @1 conflicts with an alias in the same statement
")
  (setf (gethash 335544621 *messages*) "alias @1 conflicts with a procedure in the same statement
")
  (setf (gethash 335544622 *messages*) "alias @1 conflicts with a table in the same statement
")
  (setf (gethash 335544623 *messages*) "Illegal use of keyword VALUE
")
  (setf (gethash 335544624 *messages*) "segment count of 0 defined for index @1
")
  (setf (gethash 335544625 *messages*) "A node name is not permitted in a secondary, shadow, cache or log file name
")
  (setf (gethash 335544626 *messages*) "TABLE @1
")
  (setf (gethash 335544627 *messages*) "PROCEDURE @1
")
  (setf (gethash 335544628 *messages*) "cannot create index @1
")
  (setf (gethash 335544629 *messages*) "Write-ahead Log with shadowing configuration not allowed
")
  (setf (gethash 335544630 *messages*) "there are @1 dependencies
")
  (setf (gethash 335544631 *messages*) "too many keys defined for index @1
")
  (setf (gethash 335544632 *messages*) "Preceding file did not specify length, so @1 must include starting page number
")
  (setf (gethash 335544633 *messages*) "Shadow number must be a positive integer
")
  (setf (gethash 335544634 *messages*) "Token unknown - line @1, column @2
")
  (setf (gethash 335544635 *messages*) "there is no alias or table named @1 at this scope level
")
  (setf (gethash 335544636 *messages*) "there is no index @1 for table @2
")
  (setf (gethash 335544637 *messages*) "table @1 is not referenced in plan
")
  (setf (gethash 335544638 *messages*) "table @1 is referenced more than once in plan; use aliases to distinguish
")
  (setf (gethash 335544639 *messages*) "table @1 is referenced in the plan but not the from list
")
  (setf (gethash 335544640 *messages*) "Invalid use of CHARACTER SET or COLLATE
")
  (setf (gethash 335544641 *messages*) "Specified domain or source column @1 does not exist
")
  (setf (gethash 335544642 *messages*) "index @1 cannot be used in the specified plan
")
  (setf (gethash 335544643 *messages*) "the table @1 is referenced twice; use aliases to differentiate
")
  (setf (gethash 335544644 *messages*) "attempt to fetch before the first record in a record stream
")
  (setf (gethash 335544645 *messages*) "the current position is on a crack
")
  (setf (gethash 335544646 *messages*) "database or file exists
")
  (setf (gethash 335544647 *messages*) "invalid comparison operator for find operation
")
  (setf (gethash 335544648 *messages*) "Connection lost to pipe server
")
  (setf (gethash 335544649 *messages*) "bad checksum
")
  (setf (gethash 335544650 *messages*) "wrong page type
")
  (setf (gethash 335544651 *messages*) "Cannot insert because the file is readonly or is on a read only medium.
")
  (setf (gethash 335544652 *messages*) "multiple rows in singleton select
")
  (setf (gethash 335544653 *messages*) "cannot attach to password database
")
  (setf (gethash 335544654 *messages*) "cannot start transaction for password database
")
  (setf (gethash 335544655 *messages*) "invalid direction for find operation
")
  (setf (gethash 335544656 *messages*) "variable @1 conflicts with parameter in same procedure
")
  (setf (gethash 335544657 *messages*) "Array/BLOB/DATE data types not allowed in arithmetic
")
  (setf (gethash 335544658 *messages*) "@1 is not a valid base table of the specified view
")
  (setf (gethash 335544659 *messages*) "table @1 is referenced twice in view; use an alias to distinguish
")
  (setf (gethash 335544660 *messages*) "view @1 has more than one base table; use aliases to distinguish
")
  (setf (gethash 335544661 *messages*) "cannot add index, index root page is full.
")
  (setf (gethash 335544662 *messages*) "BLOB SUB_TYPE @1 is not defined
")
  (setf (gethash 335544663 *messages*) "Too many concurrent executions of the same request
")
  (setf (gethash 335544664 *messages*) "duplicate specification of @1 - not supported
")
  (setf (gethash 335544665 *messages*) "violation of PRIMARY or UNIQUE KEY constraint \"@1\" on table \"@2\"
")
  (setf (gethash 335544666 *messages*) "server version too old to support all CREATE DATABASE options
")
  (setf (gethash 335544667 *messages*) "drop database completed with errors
")
  (setf (gethash 335544668 *messages*) "procedure @1 does not return any values
")
  (setf (gethash 335544669 *messages*) "count of column list and variable list do not match
")
  (setf (gethash 335544670 *messages*) "attempt to index BLOB column in index @1
")
  (setf (gethash 335544671 *messages*) "attempt to index array column in index @1
")
  (setf (gethash 335544672 *messages*) "too few key columns found for index @1 (incorrect column name?)
")
  (setf (gethash 335544673 *messages*) "cannot delete
")
  (setf (gethash 335544674 *messages*) "last column in a table cannot be deleted
")
  (setf (gethash 335544675 *messages*) "sort error
")
  (setf (gethash 335544676 *messages*) "sort error *messages*) not enough memory
")
  (setf (gethash 335544677 *messages*) "too many versions
")
  (setf (gethash 335544678 *messages*) "invalid key position
")
  (setf (gethash 335544679 *messages*) "segments not allowed in expression index @1
")
  (setf (gethash 335544680 *messages*) "sort error *messages*) corruption in data structure
")
  (setf (gethash 335544681 *messages*) "new record size of @1 bytes is too big
")
  (setf (gethash 335544682 *messages*) "Inappropriate self-reference of column
")
  (setf (gethash 335544683 *messages*) "request depth exceeded. (Recursive definition?)
")
  (setf (gethash 335544684 *messages*) "cannot access column @1 in view @2
")
  (setf (gethash 335544685 *messages*) "dbkey not available for multi-table views
")
  (setf (gethash 335544686 *messages*) "journal file wrong format
")
  (setf (gethash 335544687 *messages*) "intermediate journal file full
")
  (setf (gethash 335544688 *messages*) "The prepare statement identifies a prepare statement with an open cursor
")
  (setf (gethash 335544689 *messages*) "Firebird error
")
  (setf (gethash 335544690 *messages*) "Cache redefined
")
  (setf (gethash 335544691 *messages*) "Insufficient memory to allocate page buffer cache
")
  (setf (gethash 335544692 *messages*) "Log redefined
")
  (setf (gethash 335544693 *messages*) "Log size too small
")
  (setf (gethash 335544694 *messages*) "Log partition size too small
")
  (setf (gethash 335544695 *messages*) "Partitions not supported in series of log file specification
")
  (setf (gethash 335544696 *messages*) "Total length of a partitioned log must be specified
")
  (setf (gethash 335544697 *messages*) "Precision must be from 1 to 18
")
  (setf (gethash 335544698 *messages*) "Scale must be between zero and precision
")
  (setf (gethash 335544699 *messages*) "Short integer expected
")
  (setf (gethash 335544700 *messages*) "Long integer expected
")
  (setf (gethash 335544701 *messages*) "Unsigned short integer expected
")
  (setf (gethash 335544702 *messages*) "Invalid ESCAPE sequence
")
  (setf (gethash 335544703 *messages*) "service @1 does not have an associated executable
")
  (setf (gethash 335544704 *messages*) "Failed to locate host machine.
")
  (setf (gethash 335544705 *messages*) "Undefined service @1/@2.
")
  (setf (gethash 335544706 *messages*) "The specified name was not found in the hosts file or Domain Name Services.
")
  (setf (gethash 335544707 *messages*) "user does not have GRANT privileges on base table/view for operation
")
  (setf (gethash 335544708 *messages*) "Ambiguous column reference.
")
  (setf (gethash 335544709 *messages*) "Invalid aggregate reference
")
  (setf (gethash 335544710 *messages*) "navigational stream @1 references a view with more than one base table
")
  (setf (gethash 335544711 *messages*) "Attempt to execute an unprepared dynamic SQL statement.
")
  (setf (gethash 335544712 *messages*) "Positive value expected
")
  (setf (gethash 335544713 *messages*) "Incorrect values within SQLDA structure
")
  (setf (gethash 335544714 *messages*) "invalid blob id
")
  (setf (gethash 335544715 *messages*) "Operation not supported for EXTERNAL FILE table @1
")
  (setf (gethash 335544716 *messages*) "Service is currently busy *messages*) @1
")
  (setf (gethash 335544717 *messages*) "stack size insufficent to execute current request
")
  (setf (gethash 335544718 *messages*) "Invalid key for find operation
")
  (setf (gethash 335544719 *messages*) "Error initializing the network software.
")
  (setf (gethash 335544720 *messages*) "Unable to load required library @1.
")
  (setf (gethash 335544721 *messages*) "Unable to complete network request to host \"@1\".
")
  (setf (gethash 335544722 *messages*) "Failed to establish a connection.
")
  (setf (gethash 335544723 *messages*) "Error while listening for an incoming connection.
")
  (setf (gethash 335544724 *messages*) "Failed to establish a secondary connection for event processing.
")
  (setf (gethash 335544725 *messages*) "Error while listening for an incoming event connection request.
")
  (setf (gethash 335544726 *messages*) "Error reading data from the connection.
")
  (setf (gethash 335544727 *messages*) "Error writing data to the connection.
")
  (setf (gethash 335544728 *messages*) "Cannot deactivate index used by an integrity constraint
")
  (setf (gethash 335544729 *messages*) "Cannot deactivate index used by a PRIMARY/UNIQUE constraint
")
  (setf (gethash 335544730 *messages*) "Client/Server Express not supported in this release
")
  (setf (gethash 335544731 *messages*) "
")
  (setf (gethash 335544732 *messages*) "Access to databases on file servers is not supported.
")
  (setf (gethash 335544733 *messages*) "Error while trying to create file
")
  (setf (gethash 335544734 *messages*) "Error while trying to open file
")
  (setf (gethash 335544735 *messages*) "Error while trying to close file
")
  (setf (gethash 335544736 *messages*) "Error while trying to read from file
")
  (setf (gethash 335544737 *messages*) "Error while trying to write to file
")
  (setf (gethash 335544738 *messages*) "Error while trying to delete file
")
  (setf (gethash 335544739 *messages*) "Error while trying to access file
")
  (setf (gethash 335544740 *messages*) "A fatal exception occurred during the execution of a user defined function.
")
  (setf (gethash 335544741 *messages*) "connection lost to database
")
  (setf (gethash 335544742 *messages*) "User cannot write to RDB$USER_PRIVILEGES
")
  (setf (gethash 335544743 *messages*) "token size exceeds limit
")
  (setf (gethash 335544744 *messages*) "Maximum user count exceeded.  Contact your database administrator.
")
  (setf (gethash 335544745 *messages*) "Your login @1 is same as one of the SQL role name. Ask your database administrator to set up a valid Firebird login.
")
  (setf (gethash 335544746 *messages*) "\"REFERENCES table\" without \"(column)\" requires PRIMARY KEY on referenced table
")
  (setf (gethash 335544747 *messages*) "The username entered is too long.  Maximum length is 31 bytes.
")
  (setf (gethash 335544748 *messages*) "The password specified is too long.  Maximum length is 8 bytes.
")
  (setf (gethash 335544749 *messages*) "A username is required for this operation.
")
  (setf (gethash 335544750 *messages*) "A password is required for this operation
")
  (setf (gethash 335544751 *messages*) "The network protocol specified is invalid
")
  (setf (gethash 335544752 *messages*) "A duplicate user name was found in the security database
")
  (setf (gethash 335544753 *messages*) "The user name specified was not found in the security database
")
  (setf (gethash 335544754 *messages*) "An error occurred while attempting to add the user.
")
  (setf (gethash 335544755 *messages*) "An error occurred while attempting to modify the user record.
")
  (setf (gethash 335544756 *messages*) "An error occurred while attempting to delete the user record.
")
  (setf (gethash 335544757 *messages*) "An error occurred while updating the security database.
")
  (setf (gethash 335544758 *messages*) "sort record size of @1 bytes is too big
")
  (setf (gethash 335544759 *messages*) "can not define a not null column with NULL as default value
")
  (setf (gethash 335544760 *messages*) "invalid clause --- '@1'
")
  (setf (gethash 335544761 *messages*) "too many open handles to database
")
  (setf (gethash 335544762 *messages*) "size of optimizer block exceeded
")
  (setf (gethash 335544763 *messages*) "a string constant is delimited by double quotes
")
  (setf (gethash 335544764 *messages*) "DATE must be changed to TIMESTAMP
")
  (setf (gethash 335544765 *messages*) "attempted update on read-only database
")
  (setf (gethash 335544766 *messages*) "SQL dialect @1 is not supported in this database
")
  (setf (gethash 335544767 *messages*) "A fatal exception occurred during the execution of a blob filter.
")
  (setf (gethash 335544768 *messages*) "Access violation.  The code attempted to access a virtual address without privilege to do so.
")
  (setf (gethash 335544769 *messages*) "Datatype misalignment.  The attempted to read or write a value that was not stored on a memory boundary.
")
  (setf (gethash 335544770 *messages*) "Array bounds exceeded.  The code attempted to access an array element that is out of bounds.
")
  (setf (gethash 335544771 *messages*) "Float denormal operand.  One of the floating-point operands is too small to represent a standard float value.
")
  (setf (gethash 335544772 *messages*) "Floating-point divide by zero.  The code attempted to divide a floating-point value by zero.
")
  (setf (gethash 335544773 *messages*) "Floating-point inexact result.  The result of a floating-point operation cannot be represented as a deciaml fraction.
")
  (setf (gethash 335544774 *messages*) "Floating-point invalid operand.  An indeterminant error occurred during a floating-point operation.
")
  (setf (gethash 335544775 *messages*) "Floating-point overflow.  The exponent of a floating-point operation is greater than the magnitude allowed.
")
  (setf (gethash 335544776 *messages*) "Floating-point stack check.  The stack overflowed or underflowed as the result of a floating-point operation.
")
  (setf (gethash 335544777 *messages*) "Floating-point underflow.  The exponent of a floating-point operation is less than the magnitude allowed.
")
  (setf (gethash 335544778 *messages*) "Integer divide by zero.  The code attempted to divide an integer value by an integer divisor of zero.
")
  (setf (gethash 335544779 *messages*) "Integer overflow.  The result of an integer operation caused the most significant bit of the result to carry.
")
  (setf (gethash 335544780 *messages*) "An exception occurred that does not have a description.  Exception number @1.
")
  (setf (gethash 335544781 *messages*) "Stack overflow.  The resource requirements of the runtime stack have exceeded the memory available to it.
")
  (setf (gethash 335544782 *messages*) "Segmentation Fault. The code attempted to access memory without privileges.
")
  (setf (gethash 335544783 *messages*) "Illegal Instruction. The Code attempted to perform an illegal operation.
")
  (setf (gethash 335544784 *messages*) "Bus Error. The Code caused a system bus error.
")
  (setf (gethash 335544785 *messages*) "Floating Point Error. The Code caused an Arithmetic Exception or a floating point exception.
")
  (setf (gethash 335544786 *messages*) "Cannot delete rows from external files.
")
  (setf (gethash 335544787 *messages*) "Cannot update rows in external files.
")
  (setf (gethash 335544788 *messages*) "Unable to perform operation
")
  (setf (gethash 335544789 *messages*) "Specified EXTRACT part does not exist in input datatype
")
  (setf (gethash 335544790 *messages*) "Service @1 requires SYSDBA permissions.  Reattach to the Service Manager using the SYSDBA account.
")
  (setf (gethash 335544791 *messages*) "The file @1 is currently in use by another process.  Try again later.
")
  (setf (gethash 335544792 *messages*) "Cannot attach to services manager
")
  (setf (gethash 335544793 *messages*) "Metadata update statement is not allowed by the current database SQL dialect @1
")
  (setf (gethash 335544794 *messages*) "operation was cancelled
")
  (setf (gethash 335544795 *messages*) "unexpected item in service parameter block, expected @1
")
  (setf (gethash 335544796 *messages*) "Client SQL dialect @1 does not support reference to @2 datatype
")
  (setf (gethash 335544797 *messages*) "user name and password are required while attaching to the services manager
")
  (setf (gethash 335544798 *messages*) "You created an indirect dependency on uncommitted metadata. You must roll back the current transaction.
")
  (setf (gethash 335544799 *messages*) "The service name was not specified.
")
  (setf (gethash 335544800 *messages*) "Too many Contexts of Relation/Procedure/Views. Maximum allowed is 256
")
  (setf (gethash 335544801 *messages*) "data type not supported for arithmetic
")
  (setf (gethash 335544802 *messages*) "Database dialect being changed from 3 to 1
")
  (setf (gethash 335544803 *messages*) "Database dialect not changed.
")
  (setf (gethash 335544804 *messages*) "Unable to create database @1
")
  (setf (gethash 335544805 *messages*) "Database dialect @1 is not a valid dialect.
")
  (setf (gethash 335544806 *messages*) "Valid database dialects are @1.
")
  (setf (gethash 335544807 *messages*) "SQL warning code = @1
")
  (setf (gethash 335544808 *messages*) "DATE data type is now called TIMESTAMP
")
  (setf (gethash 335544809 *messages*) "Function @1 is in @2, which is not in a permitted directory for external functions.
")
  (setf (gethash 335544810 *messages*) "value exceeds the range for valid dates
")
  (setf (gethash 335544811 *messages*) "passed client dialect @1 is not a valid dialect.
")
  (setf (gethash 335544812 *messages*) "Valid client dialects are @1.
")
  (setf (gethash 335544813 *messages*) "Unsupported field type specified in BETWEEN predicate.
")
  (setf (gethash 335544814 *messages*) "Services functionality will be supported in a later version  of the product
")
  (setf (gethash 335544815 *messages*) "GENERATOR @1
")
  (setf (gethash 335544816 *messages*) "UDF @1
")
  (setf (gethash 335544817 *messages*) "Invalid parameter to FETCH or FIRST. Only integers >= 0 are allowed.
")
  (setf (gethash 335544818 *messages*) "Invalid parameter to OFFSET or SKIP. Only integers >= 0 are allowed.
")
  (setf (gethash 335544819 *messages*) "File exceeded maximum size of 2GB.  Add another database file or use a 64 bit I/O version of Firebird.
")
  (setf (gethash 335544820 *messages*) "Unable to find savepoint with name @1 in transaction context
")
  (setf (gethash 335544821 *messages*) "Invalid column position used in the @1 clause
")
  (setf (gethash 335544822 *messages*) "Cannot use an aggregate or window function in a WHERE clause, use HAVING (for aggregate only) instead
")
  (setf (gethash 335544823 *messages*) "Cannot use an aggregate or window function in a GROUP BY clause
")
  (setf (gethash 335544824 *messages*) "Invalid expression in the @1 (not contained in either an aggregate function or the GROUP BY clause)
")
  (setf (gethash 335544825 *messages*) "Invalid expression in the @1 (neither an aggregate function nor a part of the GROUP BY clause)
")
  (setf (gethash 335544826 *messages*) "Nested aggregate and window functions are not allowed
")
  (setf (gethash 335544827 *messages*) "Invalid argument in EXECUTE STATEMENT - cannot convert to string
")
  (setf (gethash 335544828 *messages*) "Wrong request type in EXECUTE STATEMENT '@1'
")
  (setf (gethash 335544829 *messages*) "Variable type (position @1) in EXECUTE STATEMENT '@2' INTO does not match returned column type
")
  (setf (gethash 335544830 *messages*) "Too many recursion levels of EXECUTE STATEMENT
")
  (setf (gethash 335544831 *messages*) "Use of @1 at location @2 is not allowed by server configuration
")
  (setf (gethash 335544832 *messages*) "Cannot change difference file name while database is in backup mode
")
  (setf (gethash 335544833 *messages*) "Physical backup is not allowed while Write-Ahead Log is in use
")
  (setf (gethash 335544834 *messages*) "Cursor is not open
")
  (setf (gethash 335544835 *messages*) "Target shutdown mode is invalid for database \"@1\"
")
  (setf (gethash 335544836 *messages*) "Concatenation overflow. Resulting string cannot exceed 32765 bytes in length.
")
  (setf (gethash 335544837 *messages*) "Invalid offset parameter @1 to SUBSTRING. Only positive integers are allowed.
")
  (setf (gethash 335544838 *messages*) "Foreign key reference target does not exist
")
  (setf (gethash 335544839 *messages*) "Foreign key references are present for the record
")
  (setf (gethash 335544840 *messages*) "cannot update
")
  (setf (gethash 335544841 *messages*) "Cursor is already open
")
  (setf (gethash 335544842 *messages*) "@1
")
  (setf (gethash 335544843 *messages*) "Context variable @1 is not found in namespace @2
")
  (setf (gethash 335544844 *messages*) "Invalid namespace name @1 passed to @2
")
  (setf (gethash 335544845 *messages*) "Too many context variables
")
  (setf (gethash 335544846 *messages*) "Invalid argument passed to @1
")
  (setf (gethash 335544847 *messages*) "BLR syntax error. Identifier @1... is too long
")
  (setf (gethash 335544848 *messages*) "exception @1
")
  (setf (gethash 335544849 *messages*) "Malformed string
")
  (setf (gethash 335544850 *messages*) "Output parameter mismatch for procedure @1
")
  (setf (gethash 335544851 *messages*) "Unexpected end of command - line @1, column @2
")
  (setf (gethash 335544852 *messages*) "partner index segment no @1 has incompatible data type
")
  (setf (gethash 335544853 *messages*) "Invalid length parameter @1 to SUBSTRING. Negative integers are not allowed.
")
  (setf (gethash 335544854 *messages*) "CHARACTER SET @1 is not installed
")
  (setf (gethash 335544855 *messages*) "COLLATION @1 for CHARACTER SET @2 is not installed
")
  (setf (gethash 335544856 *messages*) "connection shutdown
")
  (setf (gethash 335544857 *messages*) "Maximum BLOB size exceeded
")
  (setf (gethash 335544858 *messages*) "Can't have relation with only computed fields or constraints
")
  (setf (gethash 335544859 *messages*) "Time precision exceeds allowed range (0-@1)
")
  (setf (gethash 335544860 *messages*) "Unsupported conversion to target type BLOB (subtype @1)
")
  (setf (gethash 335544861 *messages*) "Unsupported conversion to target type ARRAY
")
  (setf (gethash 335544862 *messages*) "Stream does not support record locking
")
  (setf (gethash 335544863 *messages*) "Cannot create foreign key constraint @1. Partner index does not exist or is inactive.
")
  (setf (gethash 335544864 *messages*) "Transactions count exceeded. Perform backup and restore to make database operable again
")
  (setf (gethash 335544865 *messages*) "Column has been unexpectedly deleted
")
  (setf (gethash 335544866 *messages*) "@1 cannot depend on @2
")
  (setf (gethash 335544867 *messages*) "Blob sub_types bigger than 1 (text) are for internal use only
")
  (setf (gethash 335544868 *messages*) "Procedure @1 is not selectable (it does not contain a SUSPEND statement)
")
  (setf (gethash 335544869 *messages*) "Datatype @1 is not supported for sorting operation
")
  (setf (gethash 335544870 *messages*) "COLLATION @1
")
  (setf (gethash 335544871 *messages*) "DOMAIN @1
")
  (setf (gethash 335544872 *messages*) "domain @1 is not defined
")
  (setf (gethash 335544873 *messages*) "Array data type can use up to @1 dimensions
")
  (setf (gethash 335544874 *messages*) "A multi database transaction cannot span more than @1 databases
")
  (setf (gethash 335544875 *messages*) "Bad debug info format
")
  (setf (gethash 335544876 *messages*) "Error while parsing procedure @1's BLR
")
  (setf (gethash 335544877 *messages*) "index key too big
")
  (setf (gethash 335544878 *messages*) "concurrent transaction number is @1
")
  (setf (gethash 335544879 *messages*) "validation error for variable @1, value \"@2\"
")
  (setf (gethash 335544880 *messages*) "validation error for @1, value \"@2\"
")
  (setf (gethash 335544881 *messages*) "Difference file name should be set explicitly for database on raw device
")
  (setf (gethash 335544882 *messages*) "Login name too long (@1 characters, maximum allowed @2)
")
  (setf (gethash 335544883 *messages*) "column @1 is not defined in procedure @2
")
  (setf (gethash 335544884 *messages*) "Invalid SIMILAR TO pattern
")
  (setf (gethash 335544885 *messages*) "Invalid TEB format
")
  (setf (gethash 335544886 *messages*) "Found more than one transaction isolation in TPB
")
  (setf (gethash 335544887 *messages*) "Table reservation lock type @1 requires table name before in TPB
")
  (setf (gethash 335544888 *messages*) "Found more than one @1 specification in TPB
")
  (setf (gethash 335544889 *messages*) "Option @1 requires READ COMMITTED isolation in TPB
")
  (setf (gethash 335544890 *messages*) "Option @1 is not valid if @2 was used previously in TPB
")
  (setf (gethash 335544891 *messages*) "Table name length missing after table reservation @1 in TPB
")
  (setf (gethash 335544892 *messages*) "Table name length @1 is too long after table reservation @2 in TPB
")
  (setf (gethash 335544893 *messages*) "Table name length @1 without table name after table reservation @2 in TPB
")
  (setf (gethash 335544894 *messages*) "Table name length @1 goes beyond the remaining TPB size after table reservation @2
")
  (setf (gethash 335544895 *messages*) "Table name length is zero after table reservation @1 in TPB
")
  (setf (gethash 335544896 *messages*) "Table or view @1 not defined in system tables after table reservation @2 in TPB
")
  (setf (gethash 335544897 *messages*) "Base table or view @1 for view @2 not defined in system tables after table reservation @3 in TPB
")
  (setf (gethash 335544898 *messages*) "Option length missing after option @1 in TPB
")
  (setf (gethash 335544899 *messages*) "Option length @1 without value after option @2 in TPB
")
  (setf (gethash 335544900 *messages*) "Option length @1 goes beyond the remaining TPB size after option @2
")
  (setf (gethash 335544901 *messages*) "Option length is zero after table reservation @1 in TPB
")
  (setf (gethash 335544902 *messages*) "Option length @1 exceeds the range for option @2 in TPB
")
  (setf (gethash 335544903 *messages*) "Option value @1 is invalid for the option @2 in TPB
")
  (setf (gethash 335544904 *messages*) "Preserving previous table reservation @1 for table @2, stronger than new @3 in TPB
")
  (setf (gethash 335544905 *messages*) "Table reservation @1 for table @2 already specified and is stronger than new @3 in TPB
")
  (setf (gethash 335544906 *messages*) "Table reservation reached maximum recursion of @1 when expanding views in TPB
")
  (setf (gethash 335544907 *messages*) "Table reservation in TPB cannot be applied to @1 because it's a virtual table
")
  (setf (gethash 335544908 *messages*) "Table reservation in TPB cannot be applied to @1 because it's a system table
")
  (setf (gethash 335544909 *messages*) "Table reservation @1 or @2 in TPB cannot be applied to @3 because it's a temporary table
")
  (setf (gethash 335544910 *messages*) "Cannot set the transaction in read only mode after a table reservation isc_tpb_lock_write in TPB
")
  (setf (gethash 335544911 *messages*) "Cannot take a table reservation isc_tpb_lock_write in TPB because the transaction is in read only mode
")
  (setf (gethash 335544912 *messages*) "value exceeds the range for a valid time
")
  (setf (gethash 335544913 *messages*) "value exceeds the range for valid timestamps
")
  (setf (gethash 335544914 *messages*) "string right truncation
")
  (setf (gethash 335544915 *messages*) "blob truncation when converting to a string *messages*) length limit exceeded
")
  (setf (gethash 335544916 *messages*) "numeric value is out of range
")
  (setf (gethash 335544917 *messages*) "Firebird shutdown is still in progress after the specified timeout
")
  (setf (gethash 335544918 *messages*) "Attachment handle is busy
")
  (setf (gethash 335544919 *messages*) "Bad written UDF detected *messages*) pointer returned in FREE_IT function was not allocated by ib_util_malloc
")
  (setf (gethash 335544920 *messages*) "External Data Source provider '@1' not found
")
  (setf (gethash 335544921 *messages*) "Execute statement error at @1 :@2Data source  *messages*) @3
")
  (setf (gethash 335544922 *messages*) "Execute statement preprocess SQL error
")
  (setf (gethash 335544923 *messages*) "Statement expected
")
  (setf (gethash 335544924 *messages*) "Parameter name expected
")
  (setf (gethash 335544925 *messages*) "Unclosed comment found near '@1'
")
  (setf (gethash 335544926 *messages*) "Execute statement error at @1 :@2Statement  *messages*) @3Data source  *messages*) @4
")
  (setf (gethash 335544927 *messages*) "Input parameters mismatch
")
  (setf (gethash 335544928 *messages*) "Output parameters mismatch
")
  (setf (gethash 335544929 *messages*) "Input parameter '@1' have no value set
")
  (setf (gethash 335544930 *messages*) "BLR stream length @1 exceeds implementation limit @2
")
  (setf (gethash 335544931 *messages*) "Monitoring table space exhausted
")
  (setf (gethash 335544932 *messages*) "module name or entrypoint could not be found
")
  (setf (gethash 335544933 *messages*) "nothing to cancel
")
  (setf (gethash 335544934 *messages*) "ib_util library has not been loaded to deallocate memory returned by FREE_IT function
")
  (setf (gethash 335544935 *messages*) "Cannot have circular dependencies with computed fields
")
  (setf (gethash 335544936 *messages*) "Security database error
")
  (setf (gethash 335544937 *messages*) "Invalid data type in DATE/TIME/TIMESTAMP addition or subtraction in add_datettime()
")
  (setf (gethash 335544938 *messages*) "Only a TIME value can be added to a DATE value
")
  (setf (gethash 335544939 *messages*) "Only a DATE value can be added to a TIME value
")
  (setf (gethash 335544940 *messages*) "TIMESTAMP values can be subtracted only from another TIMESTAMP value
")
  (setf (gethash 335544941 *messages*) "Only one operand can be of type TIMESTAMP
")
  (setf (gethash 335544942 *messages*) "Only HOUR, MINUTE, SECOND and MILLISECOND can be extracted from TIME values
")
  (setf (gethash 335544943 *messages*) "HOUR, MINUTE, SECOND and MILLISECOND cannot be extracted from DATE values
")
  (setf (gethash 335544944 *messages*) "Invalid argument for EXTRACT() not being of DATE/TIME/TIMESTAMP type
")
  (setf (gethash 335544945 *messages*) "Arguments for @1 must be integral types or NUMERIC/DECIMAL without scale
")
  (setf (gethash 335544946 *messages*) "First argument for @1 must be integral type or floating point type
")
  (setf (gethash 335544947 *messages*) "Human readable UUID argument for @1 must be of string type
")
  (setf (gethash 335544948 *messages*) "Human readable UUID argument for @2 must be of exact length @1
")
  (setf (gethash 335544949 *messages*) "Human readable UUID argument for @3 must have \"-\" at position @2 instead of \"@1\"
")
  (setf (gethash 335544950 *messages*) "Human readable UUID argument for @3 must have hex digit at position @2 instead of \"@1\"
")
  (setf (gethash 335544951 *messages*) "Only HOUR, MINUTE, SECOND and MILLISECOND can be added to TIME values in @1
")
  (setf (gethash 335544952 *messages*) "Invalid data type in addition of part to DATE/TIME/TIMESTAMP in @1
")
  (setf (gethash 335544953 *messages*) "Invalid part @1 to be added to a DATE/TIME/TIMESTAMP value in @2
")
  (setf (gethash 335544954 *messages*) "Expected DATE/TIME/TIMESTAMP type in evlDateAdd() result
")
  (setf (gethash 335544955 *messages*) "Expected DATE/TIME/TIMESTAMP type as first and second argument to @1
")
  (setf (gethash 335544956 *messages*) "The result of TIME-<value> in @1 cannot be expressed in YEAR, MONTH, DAY or WEEK
")
  (setf (gethash 335544957 *messages*) "The result of TIME-TIMESTAMP or TIMESTAMP-TIME in @1 cannot be expressed in HOUR, MINUTE, SECOND or MILLISECOND
")
  (setf (gethash 335544958 *messages*) "The result of DATE-TIME or TIME-DATE in @1 cannot be expressed in HOUR, MINUTE, SECOND and MILLISECOND
")
  (setf (gethash 335544959 *messages*) "Invalid part @1 to express the difference between two DATE/TIME/TIMESTAMP values in @2
")
  (setf (gethash 335544960 *messages*) "Argument for @1 must be positive
")
  (setf (gethash 335544961 *messages*) "Base for @1 must be positive
")
  (setf (gethash 335544962 *messages*) "Argument #@1 for @2 must be zero or positive
")
  (setf (gethash 335544963 *messages*) "Argument #@1 for @2 must be positive
")
  (setf (gethash 335544964 *messages*) "Base for @1 cannot be zero if exponent is negative
")
  (setf (gethash 335544965 *messages*) "Base for @1 cannot be negative if exponent is not an integral value
")
  (setf (gethash 335544966 *messages*) "The numeric scale must be between -128 and 127 in @1
")
  (setf (gethash 335544967 *messages*) "Argument for @1 must be zero or positive
")
  (setf (gethash 335544968 *messages*) "Binary UUID argument for @1 must be of string type
")
  (setf (gethash 335544969 *messages*) "Binary UUID argument for @2 must use @1 bytes
")
  (setf (gethash 335544970 *messages*) "Missing required item @1 in service parameter block
")
  (setf (gethash 335544971 *messages*) "@1 server is shutdown
")
  (setf (gethash 335544972 *messages*) "Invalid connection string
")
  (setf (gethash 335544973 *messages*) "Unrecognized events block
")
  (setf (gethash 335544974 *messages*) "Could not start first worker thread - shutdown server
")
  (setf (gethash 335544975 *messages*) "Timeout occurred while waiting for a secondary connection for event processing
")
  (setf (gethash 335544976 *messages*) "Argument for @1 must be different than zero
")
  (setf (gethash 335544977 *messages*) "Argument for @1 must be in the range [-1, 1]
")
  (setf (gethash 335544978 *messages*) "Argument for @1 must be greater or equal than one
")
  (setf (gethash 335544979 *messages*) "Argument for @1 must be in the range ]-1, 1[
")
  (setf (gethash 335544980 *messages*) "Incorrect parameters provided to internal function @1
")
  (setf (gethash 335544981 *messages*) "Floating point overflow in built-in function @1
")
  (setf (gethash 335544982 *messages*) "Floating point overflow in result from UDF @1
")
  (setf (gethash 335544983 *messages*) "Invalid floating point value returned by UDF @1
")
  (setf (gethash 335544984 *messages*) "Database is probably already opened by another engine instance in another Windows session
")
  (setf (gethash 335544985 *messages*) "No free space found in temporary directories
")
  (setf (gethash 335544986 *messages*) "Explicit transaction control is not allowed
")
  (setf (gethash 335544987 *messages*) "Use of TRUSTED switches in spb_command_line is prohibited
")
  (setf (gethash 335544988 *messages*) "PACKAGE @1
")
  (setf (gethash 335544989 *messages*) "Cannot make field @1 of table @2 NOT NULL because there are NULLs present
")
  (setf (gethash 335544990 *messages*) "Feature @1 is not supported anymore
")
  (setf (gethash 335544991 *messages*) "VIEW @1
")
  (setf (gethash 335544992 *messages*) "Can not access lock files directory @1
")
  (setf (gethash 335544993 *messages*) "Fetch option @1 is invalid for a non-scrollable cursor
")
  (setf (gethash 335544994 *messages*) "Error while parsing function @1's BLR
")
  (setf (gethash 335544995 *messages*) "Cannot execute function @1 of the unimplemented package @2
")
  (setf (gethash 335544996 *messages*) "Cannot execute procedure @1 of the unimplemented package @2
")
  (setf (gethash 335544997 *messages*) "External function @1 not returned by the external engine plugin @2
")
  (setf (gethash 335544998 *messages*) "External procedure @1 not returned by the external engine plugin @2
")
  (setf (gethash 335544999 *messages*) "External trigger @1 not returned by the external engine plugin @2
")
  (setf (gethash 335545000 *messages*) "Incompatible plugin version @1 for external engine @2
")
  (setf (gethash 335545001 *messages*) "External engine @1 not found
")
  (setf (gethash 335545002 *messages*) "Attachment is in use
")
  (setf (gethash 335545003 *messages*) "Transaction is in use
")
  (setf (gethash 335545004 *messages*) "Error loading plugin @1
")
  (setf (gethash 335545005 *messages*) "Loadable module @1 not found
")
  (setf (gethash 335545006 *messages*) "Standard plugin entrypoint does not exist in module @1
")
  (setf (gethash 335545007 *messages*) "Module @1 exists but can not be loaded
")
  (setf (gethash 335545008 *messages*) "Module @1 does not contain plugin @2 type @3
")
  (setf (gethash 335545009 *messages*) "Invalid usage of context namespace DDL_TRIGGER
")
  (setf (gethash 335545010 *messages*) "Value is NULL but isNull parameter was not informed
")
  (setf (gethash 335545011 *messages*) "Type @1 is incompatible with BLOB
")
  (setf (gethash 335545012 *messages*) "Invalid date
")
  (setf (gethash 335545013 *messages*) "Invalid time
")
  (setf (gethash 335545014 *messages*) "Invalid timestamp
")
  (setf (gethash 335545015 *messages*) "Invalid index @1 in function @2
")
  (setf (gethash 335545016 *messages*) "@1
")
  (setf (gethash 335545017 *messages*) "Asynchronous call is already running for this attachment
")
  (setf (gethash 335545018 *messages*) "Function @1 is private to package @2
")
  (setf (gethash 335545019 *messages*) "Procedure @1 is private to package @2
")
  (setf (gethash 335545020 *messages*) "Request can't access new records in relation @1 and should be recompiled
")
  (setf (gethash 335545021 *messages*) "invalid events id (handle)
")
  (setf (gethash 335545022 *messages*) "Cannot copy statement @1
")
  (setf (gethash 335545023 *messages*) "Invalid usage of boolean expression
")
  (setf (gethash 335545024 *messages*) "Arguments for @1 cannot both be zero
")
  (setf (gethash 335545025 *messages*) "missing service ID in spb
")
  (setf (gethash 335545026 *messages*) "External BLR message mismatch *messages*) invalid null descriptor at field @1
")
  (setf (gethash 335545027 *messages*) "External BLR message mismatch *messages*) length = @1, expected @2
")
  (setf (gethash 335545028 *messages*) "Subscript @1 out of bounds [@2, @3]
")
  (setf (gethash 335545029 *messages*) "Install incomplete, please read the Compatibility chapter in the release notes for this version
")
  (setf (gethash 335545030 *messages*) "@1 operation is not allowed for system table @2
")
  (setf (gethash 335545031 *messages*) "Libtommath error code @1 in function @2
")
  (setf (gethash 335545032 *messages*) "unsupported BLR version (expected between @1 and @2, encountered @3)
")
  (setf (gethash 335545033 *messages*) "expected length @1, actual @2
")
  (setf (gethash 335545034 *messages*) "Wrong info requested in isc_svc_query() for anonymous service
")
  (setf (gethash 335545035 *messages*) "No isc_info_svc_stdin in user request, but service thread requested stdin data
")
  (setf (gethash 335545036 *messages*) "Start request for anonymous service is impossible
")
  (setf (gethash 335545037 *messages*) "All services except for getting server log require switches
")
  (setf (gethash 335545038 *messages*) "Size of stdin data is more than was requested from client
")
  (setf (gethash 335545039 *messages*) "Crypt plugin @1 failed to load
")
  (setf (gethash 335545040 *messages*) "Length of crypt plugin name should not exceed @1 bytes
")
  (setf (gethash 335545041 *messages*) "Crypt failed - already crypting database
")
  (setf (gethash 335545042 *messages*) "Crypt failed - database is already in requested state
")
  (setf (gethash 335545043 *messages*) "Missing crypt plugin, but page appears encrypted
")
  (setf (gethash 335545044 *messages*) "No providers loaded
")
  (setf (gethash 335545045 *messages*) "NULL data with non-zero SPB length
")
  (setf (gethash 335545046 *messages*) "Maximum (@1) number of arguments exceeded for function @2
")
  (setf (gethash 335545047 *messages*) "External BLR message mismatch *messages*) names count = @1, blr count = @2
")
  (setf (gethash 335545048 *messages*) "External BLR message mismatch *messages*) name @1 not found
")
  (setf (gethash 335545049 *messages*) "Invalid resultset interface
")
  (setf (gethash 335545050 *messages*) "Message length passed from user application does not match set of columns
")
  (setf (gethash 335545051 *messages*) "Resultset is missing output format information
")
  (setf (gethash 335545052 *messages*) "Message metadata not ready - item @1 is not finished
")
  (setf (gethash 335545053 *messages*) "Missing configuration file *messages*) @1
")
  (setf (gethash 335545054 *messages*) "@1 *messages*) illegal line <@2>
")
  (setf (gethash 335545055 *messages*) "Invalid include operator in @1 for <@2>
")
  (setf (gethash 335545056 *messages*) "Include depth too big
")
  (setf (gethash 335545057 *messages*) "File to include not found
")
  (setf (gethash 335545058 *messages*) "Only the owner can change the ownership
")
  (setf (gethash 335545059 *messages*) "undefined variable number
")
  (setf (gethash 335545060 *messages*) "Missing security context for @1
")
  (setf (gethash 335545061 *messages*) "Missing segment @1 in multisegment connect block parameter
")
  (setf (gethash 335545062 *messages*) "Different logins in connect and attach packets - client library error
")
  (setf (gethash 335545063 *messages*) "Exceeded exchange limit during authentication handshake
")
  (setf (gethash 335545064 *messages*) "Incompatible wire encryption levels requested on client and server
")
  (setf (gethash 335545065 *messages*) "Client attempted to attach unencrypted but wire encryption is required
")
  (setf (gethash 335545066 *messages*) "Client attempted to start wire encryption using unknown key @1
")
  (setf (gethash 335545067 *messages*) "Client attempted to start wire encryption using unsupported plugin @1
")
  (setf (gethash 335545068 *messages*) "Error getting security database name from configuration file
")
  (setf (gethash 335545069 *messages*) "Client authentication plugin is missing required data from server
")
  (setf (gethash 335545070 *messages*) "Client authentication plugin expected @2 bytes of @3 from server, got @1
")
  (setf (gethash 335545071 *messages*) "Attempt to get information about an unprepared dynamic SQL statement.
")
  (setf (gethash 335545072 *messages*) "Problematic key value is @1
")
  (setf (gethash 335545073 *messages*) "Cannot select virtual table @1 for update WITH LOCK
")
  (setf (gethash 335545074 *messages*) "Cannot select system table @1 for update WITH LOCK
")
  (setf (gethash 335545075 *messages*) "Cannot select temporary table @1 for update WITH LOCK
")
  (setf (gethash 335545076 *messages*) "System @1 @2 cannot be modified
")
  (setf (gethash 335545077 *messages*) "Server misconfigured - contact administrator please
")
  (setf (gethash 335545078 *messages*) "Deprecated backward compatibility ALTER ROLE ... SET/DROP AUTO ADMIN mapping may be used only for RDB$ADMIN role
")
  (setf (gethash 335545079 *messages*) "Mapping @1 already exists
")
  (setf (gethash 335545080 *messages*) "Mapping @1 does not exist
")
  (setf (gethash 335545081 *messages*) "@1 failed when loading mapping cache
")
  (setf (gethash 335545082 *messages*) "Invalid name <*> in authentication block
")
  (setf (gethash 335545083 *messages*) "Multiple maps found for @1
")
  (setf (gethash 335545084 *messages*) "Undefined mapping result - more than one different results found
")
  (setf (gethash 335545085 *messages*) "Incompatible mode of attachment to damaged database
")
  (setf (gethash 335545086 *messages*) "Attempt to set in database number of buffers which is out of acceptable range [@1:@2]
")
  (setf (gethash 335545087 *messages*) "Attempt to temporarily set number of buffers less than @1
")
  (setf (gethash 335545088 *messages*) "Global mapping is not available when database @1 is not present
")
  (setf (gethash 335545089 *messages*) "Global mapping is not available when table RDB$MAP is not present in database @1
")
  (setf (gethash 335545090 *messages*) "Your attachment has no trusted role
")
  (setf (gethash 335545091 *messages*) "Role @1 is invalid or unavailable
")
  (setf (gethash 335545092 *messages*) "Cursor @1 is not positioned in a valid record
")
  (setf (gethash 335545093 *messages*) "Duplicated user attribute @1
")
  (setf (gethash 335545094 *messages*) "There is no privilege for this operation
")
  (setf (gethash 335545095 *messages*) "Using GRANT OPTION on @1 not allowed
")
  (setf (gethash 335545096 *messages*) "read conflicts with concurrent update
")
  (setf (gethash 335545097 *messages*) "@1 failed when working with CREATE DATABASE grants
")
  (setf (gethash 335545098 *messages*) "CREATE DATABASE grants check is not possible when database @1 is not present
")
  (setf (gethash 335545099 *messages*) "CREATE DATABASE grants check is not possible when table RDB$DB_CREATORS is not present in database @1
")
  (setf (gethash 335545100 *messages*) "Interface @3 version too old *messages*) expected @1, found @2
")
  (setf (gethash 335545101 *messages*) "Input parameter mismatch for function @1
")
  (setf (gethash 335545102 *messages*) "Error during savepoint backout - transaction invalidated
")
  (setf (gethash 335545103 *messages*) "Domain used in the PRIMARY KEY constraint of table @1 must be NOT NULL
")
  (setf (gethash 335545104 *messages*) "CHARACTER SET @1 cannot be used as a attachment character set
")
  (setf (gethash 335545105 *messages*) "Some database(s) were shutdown when trying to read mapping data
")
  (setf (gethash 335545106 *messages*) "Error occurred during login, please check server firebird.log for details
")
  (setf (gethash 335545107 *messages*) "Database already opened with engine instance, incompatible with current
")
  (setf (gethash 335545108 *messages*) "Invalid crypt key @1
")
  (setf (gethash 335545109 *messages*) "Page requires encyption but crypt plugin is missing
")
  (setf (gethash 335545110 *messages*) "Maximum index depth (@1 levels) is reached
")
  (setf (gethash 335545111 *messages*) "System privilege @1 does not exist
")
  (setf (gethash 335545112 *messages*) "System privilege @1 is missing
")
  (setf (gethash 335545113 *messages*) "Invalid or missing checksum of encrypted database
")
  (setf (gethash 335545114 *messages*) "You must have SYSDBA rights at this server
")
  (setf (gethash 335545115 *messages*) "Cannot open cursor for non-SELECT statement
")
  (setf (gethash 335545116 *messages*) "If <window frame bound 1> specifies @1, then <window frame bound 2> shall not specify @2
")
  (setf (gethash 335545117 *messages*) "RANGE based window with <expr> {PRECEDING | FOLLOWING} cannot have ORDER BY with more than one value
")
  (setf (gethash 335545118 *messages*) "RANGE based window must have an ORDER BY key of numerical, date, time or timestamp types
")
  (setf (gethash 335545119 *messages*) "Window RANGE/ROWS PRECEDING/FOLLOWING value must be of a numerical type
")
  (setf (gethash 335545120 *messages*) "Invalid PRECEDING or FOLLOWING offset in window function *messages*) cannot be negative
")
  (setf (gethash 335545121 *messages*) "Window @1 not found
")
  (setf (gethash 335545122 *messages*) "Cannot use PARTITION BY clause while overriding the window @1
")
  (setf (gethash 335545123 *messages*) "Cannot use ORDER BY clause while overriding the window @1 which already has an ORDER BY clause
")
  (setf (gethash 335545124 *messages*) "Cannot override the window @1 because it has a frame clause. Tip *messages*) it can be used without parenthesis in OVER
")
  (setf (gethash 335545125 *messages*) "Duplicate window definition for @1
")
  (setf (gethash 335545126 *messages*) "SQL statement is too long. Maximum size is @1 bytes.
")
  (setf (gethash 335740929 *messages*) "data base file name (@1) already given
")
  (setf (gethash 335740930 *messages*) "invalid switch @1
")
  (setf (gethash 335740932 *messages*) "incompatible switch combination
")
  (setf (gethash 335740933 *messages*) "replay log pathname required
")
  (setf (gethash 335740934 *messages*) "number of page buffers for cache required
")
  (setf (gethash 335740935 *messages*) "numeric value required
")
  (setf (gethash 335740936 *messages*) "positive numeric value required
")
  (setf (gethash 335740937 *messages*) "number of transactions per sweep required
")
  (setf (gethash 335740940 *messages*) "\"full\" or \"reserve\" required
")
  (setf (gethash 335740941 *messages*) "user name required
")
  (setf (gethash 335740942 *messages*) "password required
")
  (setf (gethash 335740943 *messages*) "subsystem name
")
  (setf (gethash 335740944 *messages*) "\"wal\" required
")
  (setf (gethash 335740945 *messages*) "number of seconds required
")
  (setf (gethash 335740946 *messages*) "numeric value between 0 and 32767 inclusive required
")
  (setf (gethash 335740947 *messages*) "must specify type of shutdown
")
  (setf (gethash 335740948 *messages*) "please retry, specifying an option
")
  (setf (gethash 335740951 *messages*) "please retry, giving a database name
")
  (setf (gethash 335740991 *messages*) "internal block exceeds maximum size
")
  (setf (gethash 335740992 *messages*) "corrupt pool
")
  (setf (gethash 335740993 *messages*) "virtual memory exhausted
")
  (setf (gethash 335740994 *messages*) "bad pool id
")
  (setf (gethash 335740995 *messages*) "Transaction state @1 not in valid range.
")
  (setf (gethash 335741012 *messages*) "unexpected end of input
")
  (setf (gethash 335741018 *messages*) "failed to reconnect to a transaction in database @1
")
  (setf (gethash 335741036 *messages*) "Transaction description item unknown
")
  (setf (gethash 335741038 *messages*) "\"read_only\" or \"read_write\" required
")
  (setf (gethash 335741042 *messages*) "positive or zero numeric value required
")
  (setf (gethash 336003074 *messages*) "Cannot SELECT RDB$DB_KEY from a stored procedure.
")
  (setf (gethash 336003075 *messages*) "Precision 10 to 18 changed from DOUBLE PRECISION in SQL dialect 1 to 64-bit scaled integer in SQL dialect 3
")
  (setf (gethash 336003076 *messages*) "Use of @1 expression that returns different results in dialect 1 and dialect 3
")
  (setf (gethash 336003077 *messages*) "Database SQL dialect @1 does not support reference to @2 datatype
")
  (setf (gethash 336003079 *messages*) "DB dialect @1 and client dialect @2 conflict with respect to numeric precision @3.
")
  (setf (gethash 336003080 *messages*) "WARNING *messages*) Numeric literal @1 is interpreted as a floating-point
")
  (setf (gethash 336003081 *messages*) "value in SQL dialect 1, but as an exact numeric value in SQL dialect 3.
")
  (setf (gethash 336003082 *messages*) "WARNING *messages*) NUMERIC and DECIMAL fields with precision 10 or greater are stored
")
  (setf (gethash 336003083 *messages*) "as approximate floating-point values in SQL dialect 1, but as 64-bit
")
  (setf (gethash 336003084 *messages*) "integers in SQL dialect 3.
")
  (setf (gethash 336003085 *messages*) "Ambiguous field name between @1 and @2
")
  (setf (gethash 336003086 *messages*) "External function should have return position between 1 and @1
")
  (setf (gethash 336003087 *messages*) "Label @1 @2 in the current scope
")
  (setf (gethash 336003088 *messages*) "Datatypes @1are not comparable in expression @2
")
  (setf (gethash 336003089 *messages*) "Empty cursor name is not allowed
")
  (setf (gethash 336003090 *messages*) "Statement already has a cursor @1 assigned
")
  (setf (gethash 336003091 *messages*) "Cursor @1 is not found in the current context
")
  (setf (gethash 336003092 *messages*) "Cursor @1 already exists in the current context
")
  (setf (gethash 336003093 *messages*) "Relation @1 is ambiguous in cursor @2
")
  (setf (gethash 336003094 *messages*) "Relation @1 is not found in cursor @2
")
  (setf (gethash 336003095 *messages*) "Cursor is not open
")
  (setf (gethash 336003096 *messages*) "Data type @1 is not supported for EXTERNAL TABLES. Relation '@2', field '@3'
")
  (setf (gethash 336003097 *messages*) "Feature not supported on ODS version older than @1.@2
")
  (setf (gethash 336003098 *messages*) "Primary key required on table @1
")
  (setf (gethash 336003099 *messages*) "UPDATE OR INSERT field list does not match primary key of table @1
")
  (setf (gethash 336003100 *messages*) "UPDATE OR INSERT field list does not match MATCHING clause
")
  (setf (gethash 336003101 *messages*) "UPDATE OR INSERT without MATCHING could not be used with views based on more than one table
")
  (setf (gethash 336003102 *messages*) "Incompatible trigger type
")
  (setf (gethash 336003103 *messages*) "Database trigger type can't be changed
")
  (setf (gethash 336003104 *messages*) "To be used with RDB$RECORD_VERSION, @1 must be a table or a view of single table
")
  (setf (gethash 336003105 *messages*) "SQLDA version expected between @1 and @2, found @3
")
  (setf (gethash 336003106 *messages*) "at SQLVAR index @1
")
  (setf (gethash 336003107 *messages*) "empty pointer to NULL indicator variable
")
  (setf (gethash 336003108 *messages*) "empty pointer to data
")
  (setf (gethash 336003109 *messages*) "No SQLDA for input values provided
")
  (setf (gethash 336003110 *messages*) "No SQLDA for output values provided
")
  (setf (gethash 336003111 *messages*) "Wrong number of parameters (expected @1, got @2)
")
  (setf (gethash 336003112 *messages*) "Invalid DROP SQL SECURITY clause
")
  (setf (gethash 336068645 *messages*) "BLOB Filter @1 not found
")
  (setf (gethash 336068649 *messages*) "Function @1 not found
")
  (setf (gethash 336068656 *messages*) "Index not found
")
  (setf (gethash 336068662 *messages*) "View @1 not found
")
  (setf (gethash 336068697 *messages*) "Domain not found
")
  (setf (gethash 336068717 *messages*) "Triggers created automatically cannot be modified
")
  (setf (gethash 336068740 *messages*) "Table @1 already exists
")
  (setf (gethash 336068748 *messages*) "Procedure @1 not found
")
  (setf (gethash 336068752 *messages*) "Exception not found
")
  (setf (gethash 336068754 *messages*) "Parameter @1 in procedure @2 not found
")
  (setf (gethash 336068755 *messages*) "Trigger @1 not found
")
  (setf (gethash 336068759 *messages*) "Character set @1 not found
")
  (setf (gethash 336068760 *messages*) "Collation @1 not found
")
  (setf (gethash 336068763 *messages*) "Role @1 not found
")
  (setf (gethash 336068767 *messages*) "Name longer than database column size
")
  (setf (gethash 336068784 *messages*) "column @1 does not exist in table/view @2
")
  (setf (gethash 336068796 *messages*) "SQL role @1 does not exist
")
  (setf (gethash 336068797 *messages*) "user @1 has no grant admin option on SQL role @2
")
  (setf (gethash 336068798 *messages*) "user @1 is not a member of SQL role @2
")
  (setf (gethash 336068799 *messages*) "@1 is not the owner of SQL role @2
")
  (setf (gethash 336068800 *messages*) "@1 is a SQL role and not a user
")
  (setf (gethash 336068801 *messages*) "user name @1 could not be used for SQL role
")
  (setf (gethash 336068802 *messages*) "SQL role @1 already exists
")
  (setf (gethash 336068803 *messages*) "keyword @1 can not be used as a SQL role name
")
  (setf (gethash 336068804 *messages*) "SQL roles are not supported in on older versions of the database.  A backup and restore of the database is required.
")
  (setf (gethash 336068812 *messages*) "Cannot rename domain @1 to @2.  A domain with that name already exists.
")
  (setf (gethash 336068813 *messages*) "Cannot rename column @1 to @2.  A column with that name already exists in table @3.
")
  (setf (gethash 336068814 *messages*) "Column @1 from table @2 is referenced in @3
")
  (setf (gethash 336068815 *messages*) "Cannot change datatype for column @1.  Changing datatype is not supported for BLOB or ARRAY columns.
")
  (setf (gethash 336068816 *messages*) "New size specified for column @1 must be at least @2 characters.
")
  (setf (gethash 336068817 *messages*) "Cannot change datatype for @1.  Conversion from base type @2 to @3 is not supported.
")
  (setf (gethash 336068818 *messages*) "Cannot change datatype for column @1 from a character type to a non-character type.
")
  (setf (gethash 336068820 *messages*) "Zero length identifiers are not allowed
")
  (setf (gethash 336068822 *messages*) "Sequence @1 not found
")
  (setf (gethash 336068829 *messages*) "Maximum number of collations per character set exceeded
")
  (setf (gethash 336068830 *messages*) "Invalid collation attributes
")
  (setf (gethash 336068840 *messages*) "@1 cannot reference @2
")
  (setf (gethash 336068843 *messages*) "Collation @1 is used in table @2 (field name @3) and cannot be dropped
")
  (setf (gethash 336068844 *messages*) "Collation @1 is used in domain @2 and cannot be dropped
")
  (setf (gethash 336068845 *messages*) "Cannot delete system collation
")
  (setf (gethash 336068846 *messages*) "Cannot delete default collation of CHARACTER SET @1
")
  (setf (gethash 336068849 *messages*) "Table @1 not found
")
  (setf (gethash 336068851 *messages*) "Collation @1 is used in procedure @2 (parameter name @3) and cannot be dropped
")
  (setf (gethash 336068852 *messages*) "New scale specified for column @1 must be at most @2.
")
  (setf (gethash 336068853 *messages*) "New precision specified for column @1 must be at least @2.
")
  (setf (gethash 336068855 *messages*) "Warning *messages*) @1 on @2 is not granted to @3.
")
  (setf (gethash 336068856 *messages*) "Feature '@1' is not supported in ODS @2.@3
")
  (setf (gethash 336068857 *messages*) "Cannot add or remove COMPUTED from column @1
")
  (setf (gethash 336068858 *messages*) "Password should not be empty string
")
  (setf (gethash 336068859 *messages*) "Index @1 already exists
")
  (setf (gethash 336068864 *messages*) "Package @1 not found
")
  (setf (gethash 336068865 *messages*) "Schema @1 not found
")
  (setf (gethash 336068866 *messages*) "Cannot ALTER or DROP system procedure @1
")
  (setf (gethash 336068867 *messages*) "Cannot ALTER or DROP system trigger @1
")
  (setf (gethash 336068868 *messages*) "Cannot ALTER or DROP system function @1
")
  (setf (gethash 336068869 *messages*) "Invalid DDL statement for procedure @1
")
  (setf (gethash 336068870 *messages*) "Invalid DDL statement for trigger @1
")
  (setf (gethash 336068871 *messages*) "Function @1 has not been defined on the package body @2
")
  (setf (gethash 336068872 *messages*) "Procedure @1 has not been defined on the package body @2
")
  (setf (gethash 336068873 *messages*) "Function @1 has a signature mismatch on package body @2
")
  (setf (gethash 336068874 *messages*) "Procedure @1 has a signature mismatch on package body @2
")
  (setf (gethash 336068875 *messages*) "Default values for parameters are allowed only in declaration of packaged procedure @1.@2
")
  (setf (gethash 336068877 *messages*) "Package body @1 already exists
")
  (setf (gethash 336068878 *messages*) "Invalid DDL statement for function @1
")
  (setf (gethash 336068879 *messages*) "Cannot alter new style function @1 with ALTER EXTERNAL FUNCTION. Use ALTER FUNCTION instead.
")
  (setf (gethash 336068886 *messages*) "Parameter @1 in function @2 not found
")
  (setf (gethash 336068887 *messages*) "Parameter @1 of routine @2 not found
")
  (setf (gethash 336068888 *messages*) "Parameter @1 of routine @2 is ambiguous (found in both procedures and functions). Use a specifier keyword.
")
  (setf (gethash 336068889 *messages*) "Collation @1 is used in function @2 (parameter name @3) and cannot be dropped
")
  (setf (gethash 336068890 *messages*) "Domain @1 is used in function @2 (parameter name @3) and cannot be dropped
")
  (setf (gethash 336068891 *messages*) "ALTER USER requires at least one clause to be specified
")
  (setf (gethash 336068894 *messages*) "Duplicate @1 @2
")
  (setf (gethash 336068895 *messages*) "System @1 @2 cannot be modified
")
  (setf (gethash 336068896 *messages*) "INCREMENT BY 0 is an illegal option for sequence @1
")
  (setf (gethash 336068897 *messages*) "Can't use @1 in FOREIGN KEY constraint
")
  (setf (gethash 336068898 *messages*) "Default values for parameters are allowed only in declaration of packaged function @1.@2
")
  (setf (gethash 336330753 *messages*) "found unknown switch
")
  (setf (gethash 336330754 *messages*) "page size parameter missing
")
  (setf (gethash 336330755 *messages*) "Page size specified (@1) greater than limit (32768 bytes)
")
  (setf (gethash 336330756 *messages*) "redirect location for output is not specified
")
  (setf (gethash 336330757 *messages*) "conflicting switches for backup/restore
")
  (setf (gethash 336330758 *messages*) "device type @1 not known
")
  (setf (gethash 336330759 *messages*) "protection is not there yet
")
  (setf (gethash 336330760 *messages*) "page size is allowed only on restore or create
")
  (setf (gethash 336330761 *messages*) "multiple sources or destinations specified
")
  (setf (gethash 336330762 *messages*) "requires both input and output filenames
")
  (setf (gethash 336330763 *messages*) "input and output have the same name.  Disallowed.
")
  (setf (gethash 336330764 *messages*) "expected page size, encountered \"@1\"
")
  (setf (gethash 336330765 *messages*) "REPLACE specified, but the first file @1 is a database
")
  (setf (gethash 336330766 *messages*) "database @1 already exists.  To replace it, use the -REP switch
")
  (setf (gethash 336330767 *messages*) "device type not specified
")
  (setf (gethash 336330772 *messages*) "gds_$blob_info failed
")
  (setf (gethash 336330773 *messages*) "do not understand BLOB INFO item @1
")
  (setf (gethash 336330774 *messages*) "gds_$get_segment failed
")
  (setf (gethash 336330775 *messages*) "gds_$close_blob failed
")
  (setf (gethash 336330776 *messages*) "gds_$open_blob failed
")
  (setf (gethash 336330777 *messages*) "Failed in put_blr_gen_id
")
  (setf (gethash 336330778 *messages*) "data type @1 not understood
")
  (setf (gethash 336330779 *messages*) "gds_$compile_request failed
")
  (setf (gethash 336330780 *messages*) "gds_$start_request failed
")
  (setf (gethash 336330781 *messages*) "gds_$receive failed
")
  (setf (gethash 336330782 *messages*) "gds_$release_request failed
")
  (setf (gethash 336330783 *messages*) "gds_$database_info failed
")
  (setf (gethash 336330784 *messages*) "Expected database description record
")
  (setf (gethash 336330785 *messages*) "failed to create database @1
")
  (setf (gethash 336330786 *messages*) "RESTORE *messages*) decompression length error
")
  (setf (gethash 336330787 *messages*) "cannot find table @1
")
  (setf (gethash 336330788 *messages*) "Cannot find column for BLOB
")
  (setf (gethash 336330789 *messages*) "gds_$create_blob failed
")
  (setf (gethash 336330790 *messages*) "gds_$put_segment failed
")
  (setf (gethash 336330791 *messages*) "expected record length
")
  (setf (gethash 336330792 *messages*) "wrong length record, expected @1 encountered @2
")
  (setf (gethash 336330793 *messages*) "expected data attribute
")
  (setf (gethash 336330794 *messages*) "Failed in store_blr_gen_id
")
  (setf (gethash 336330795 *messages*) "do not recognize record type @1
")
  (setf (gethash 336330796 *messages*) "Expected backup version 1..10.  Found @1
")
  (setf (gethash 336330797 *messages*) "expected backup description record
")
  (setf (gethash 336330798 *messages*) "string truncated
")
  (setf (gethash 336330799 *messages*) "warning -- record could not be restored
")
  (setf (gethash 336330800 *messages*) "gds_$send failed
")
  (setf (gethash 336330801 *messages*) "no table name for data
")
  (setf (gethash 336330802 *messages*) "unexpected end of file on backup file
")
  (setf (gethash 336330803 *messages*) "database format @1 is too old to restore to
")
  (setf (gethash 336330804 *messages*) "array dimension for column @1 is invalid
")
  (setf (gethash 336330807 *messages*) "Expected XDR record length
")
  (setf (gethash 336330817 *messages*) "cannot open backup file @1
")
  (setf (gethash 336330818 *messages*) "cannot open status and error output file @1
")
  (setf (gethash 336330934 *messages*) "blocking factor parameter missing
")
  (setf (gethash 336330935 *messages*) "expected blocking factor, encountered \"@1\"
")
  (setf (gethash 336330936 *messages*) "a blocking factor may not be used in conjunction with device CT
")
  (setf (gethash 336330940 *messages*) "user name parameter missing
")
  (setf (gethash 336330941 *messages*) "password parameter missing
")
  (setf (gethash 336330952 *messages*) " missing parameter for the number of bytes to be skipped
")
  (setf (gethash 336330953 *messages*) "expected number of bytes to be skipped, encountered \"@1\"
")
  (setf (gethash 336330965 *messages*) "character set
")
  (setf (gethash 336330967 *messages*) "collation
")
  (setf (gethash 336330972 *messages*) "Unexpected I/O error while reading from backup file
")
  (setf (gethash 336330973 *messages*) "Unexpected I/O error while writing to backup file
")
  (setf (gethash 336330985 *messages*) "could not drop database @1 (no privilege or database might be in use)
")
  (setf (gethash 336330990 *messages*) "System memory exhausted
")
  (setf (gethash 336331002 *messages*) "SQL role
")
  (setf (gethash 336331005 *messages*) "SQL role parameter missing
")
  (setf (gethash 336331010 *messages*) "page buffers parameter missing
")
  (setf (gethash 336331011 *messages*) "expected page buffers, encountered \"@1\"
")
  (setf (gethash 336331012 *messages*) "page buffers is allowed only on restore or create
")
  (setf (gethash 336331014 *messages*) "size specification either missing or incorrect for file @1
")
  (setf (gethash 336331015 *messages*) "file @1 out of sequence
")
  (setf (gethash 336331016 *messages*) "can't join -- one of the files missing
")
  (setf (gethash 336331017 *messages*) " standard input is not supported when using join operation
")
  (setf (gethash 336331018 *messages*) "standard output is not supported when using split operation or in verbose mode
")
  (setf (gethash 336331019 *messages*) "backup file @1 might be corrupt
")
  (setf (gethash 336331020 *messages*) "database file specification missing
")
  (setf (gethash 336331021 *messages*) "can't write a header record to file @1
")
  (setf (gethash 336331022 *messages*) "free disk space exhausted
")
  (setf (gethash 336331023 *messages*) "file size given (@1) is less than minimum allowed (@2)
")
  (setf (gethash 336331025 *messages*) "service name parameter missing
")
  (setf (gethash 336331026 *messages*) "Cannot restore over current database, must be SYSDBA or owner of the existing database.
")
  (setf (gethash 336331031 *messages*) "\"read_only\" or \"read_write\" required
")
  (setf (gethash 336331033 *messages*) "just data ignore all constraints etc.
")
  (setf (gethash 336331034 *messages*) "restoring data only ignoring foreign key, unique, not null & other constraints
")
  (setf (gethash 336331078 *messages*) "verbose interval value parameter missing
")
  (setf (gethash 336331079 *messages*) "verbose interval value cannot be smaller than @1
")
  (setf (gethash 336331081 *messages*) "verify (verbose) and verbint options are mutually exclusive
")
  (setf (gethash 336331082 *messages*) "option -@1 is allowed only on restore or create
")
  (setf (gethash 336331083 *messages*) "option -@1 is allowed only on backup
")
  (setf (gethash 336331084 *messages*) "options -@1 and -@2 are mutually exclusive
")
  (setf (gethash 336331085 *messages*) "parameter for option -@1 was already specified with value \"@2\"
")
  (setf (gethash 336331086 *messages*) "option -@1 was already specified
")
  (setf (gethash 336331091 *messages*) "dependency depth greater than @1 for view @2
")
  (setf (gethash 336331092 *messages*) "value greater than @1 when calculating length of rdb$db_key for view @2
")
  (setf (gethash 336331093 *messages*) "Invalid metadata detected. Use -FIX_FSS_METADATA option.
")
  (setf (gethash 336331094 *messages*) "Invalid data detected. Use -FIX_FSS_DATA option.
")
  (setf (gethash 336331096 *messages*) "Expected backup version @2..@3.  Found @1
")
  (setf (gethash 336331100 *messages*) "database format @1 is too old to backup
")
  (setf (gethash 336397205 *messages*) "ODS versions before ODS@1 are not supported
")
  (setf (gethash 336397206 *messages*) "Table @1 does not exist
")
  (setf (gethash 336397207 *messages*) "View @1 does not exist
")
  (setf (gethash 336397208 *messages*) "At line @1, column @2
")
  (setf (gethash 336397209 *messages*) "At unknown line and column
")
  (setf (gethash 336397210 *messages*) "Column @1 cannot be repeated in @2 statement
")
  (setf (gethash 336397211 *messages*) "Too many values (more than @1) in member list to match against
")
  (setf (gethash 336397212 *messages*) "Array and BLOB data types not allowed in computed field
")
  (setf (gethash 336397213 *messages*) "Implicit domain name @1 not allowed in user created domain
")
  (setf (gethash 336397214 *messages*) "scalar operator used on field @1 which is not an array
")
  (setf (gethash 336397215 *messages*) "cannot sort on more than 255 items
")
  (setf (gethash 336397216 *messages*) "cannot group on more than 255 items
")
  (setf (gethash 336397217 *messages*) "Cannot include the same field (@1.@2) twice in the ORDER BY clause with conflicting sorting options
")
  (setf (gethash 336397218 *messages*) "column list from derived table @1 has more columns than the number of items in its SELECT statement
")
  (setf (gethash 336397219 *messages*) "column list from derived table @1 has less columns than the number of items in its SELECT statement
")
  (setf (gethash 336397220 *messages*) "no column name specified for column number @1 in derived table @2
")
  (setf (gethash 336397221 *messages*) "column @1 was specified multiple times for derived table @2
")
  (setf (gethash 336397222 *messages*) "Internal dsql error *messages*) alias type expected by pass1_expand_select_node
")
  (setf (gethash 336397223 *messages*) "Internal dsql error *messages*) alias type expected by pass1_field
")
  (setf (gethash 336397224 *messages*) "Internal dsql error *messages*) column position out of range in pass1_union_auto_cast
")
  (setf (gethash 336397225 *messages*) "Recursive CTE member (@1) can refer itself only in FROM clause
")
  (setf (gethash 336397226 *messages*) "CTE '@1' has cyclic dependencies
")
  (setf (gethash 336397227 *messages*) "Recursive member of CTE can't be member of an outer join
")
  (setf (gethash 336397228 *messages*) "Recursive member of CTE can't reference itself more than once
")
  (setf (gethash 336397229 *messages*) "Recursive CTE (@1) must be an UNION
")
  (setf (gethash 336397230 *messages*) "CTE '@1' defined non-recursive member after recursive
")
  (setf (gethash 336397231 *messages*) "Recursive member of CTE '@1' has @2 clause
")
  (setf (gethash 336397232 *messages*) "Recursive members of CTE (@1) must be linked with another members via UNION ALL
")
  (setf (gethash 336397233 *messages*) "Non-recursive member is missing in CTE '@1'
")
  (setf (gethash 336397234 *messages*) "WITH clause can't be nested
")
  (setf (gethash 336397235 *messages*) "column @1 appears more than once in USING clause
")
  (setf (gethash 336397236 *messages*) "feature is not supported in dialect @1
")
  (setf (gethash 336397237 *messages*) "CTE \"@1\" is not used in query
")
  (setf (gethash 336397238 *messages*) "column @1 appears more than once in ALTER VIEW
")
  (setf (gethash 336397239 *messages*) "@1 is not supported inside IN AUTONOMOUS TRANSACTION block
")
  (setf (gethash 336397240 *messages*) "Unknown node type @1 in dsql/GEN_expr
")
  (setf (gethash 336397241 *messages*) "Argument for @1 in dialect 1 must be string or numeric
")
  (setf (gethash 336397242 *messages*) "Argument for @1 in dialect 3 must be numeric
")
  (setf (gethash 336397243 *messages*) "Strings cannot be added to or subtracted from DATE or TIME types
")
  (setf (gethash 336397244 *messages*) "Invalid data type for subtraction involving DATE, TIME or TIMESTAMP types
")
  (setf (gethash 336397245 *messages*) "Adding two DATE values or two TIME values is not allowed
")
  (setf (gethash 336397246 *messages*) "DATE value cannot be subtracted from the provided data type
")
  (setf (gethash 336397247 *messages*) "Strings cannot be added or subtracted in dialect 3
")
  (setf (gethash 336397248 *messages*) "Invalid data type for addition or subtraction in dialect 3
")
  (setf (gethash 336397249 *messages*) "Invalid data type for multiplication in dialect 1
")
  (setf (gethash 336397250 *messages*) "Strings cannot be multiplied in dialect 3
")
  (setf (gethash 336397251 *messages*) "Invalid data type for multiplication in dialect 3
")
  (setf (gethash 336397252 *messages*) "Division in dialect 1 must be between numeric data types
")
  (setf (gethash 336397253 *messages*) "Strings cannot be divided in dialect 3
")
  (setf (gethash 336397254 *messages*) "Invalid data type for division in dialect 3
")
  (setf (gethash 336397255 *messages*) "Strings cannot be negated (applied the minus operator) in dialect 3
")
  (setf (gethash 336397256 *messages*) "Invalid data type for negation (minus operator)
")
  (setf (gethash 336397257 *messages*) "Cannot have more than 255 items in DISTINCT list
")
  (setf (gethash 336397258 *messages*) "ALTER CHARACTER SET @1 failed
")
  (setf (gethash 336397259 *messages*) "COMMENT ON @1 failed
")
  (setf (gethash 336397260 *messages*) "CREATE FUNCTION @1 failed
")
  (setf (gethash 336397261 *messages*) "ALTER FUNCTION @1 failed
")
  (setf (gethash 336397262 *messages*) "CREATE OR ALTER FUNCTION @1 failed
")
  (setf (gethash 336397263 *messages*) "DROP FUNCTION @1 failed
")
  (setf (gethash 336397264 *messages*) "RECREATE FUNCTION @1 failed
")
  (setf (gethash 336397265 *messages*) "CREATE PROCEDURE @1 failed
")
  (setf (gethash 336397266 *messages*) "ALTER PROCEDURE @1 failed
")
  (setf (gethash 336397267 *messages*) "CREATE OR ALTER PROCEDURE @1 failed
")
  (setf (gethash 336397268 *messages*) "DROP PROCEDURE @1 failed
")
  (setf (gethash 336397269 *messages*) "RECREATE PROCEDURE @1 failed
")
  (setf (gethash 336397270 *messages*) "CREATE TRIGGER @1 failed
")
  (setf (gethash 336397271 *messages*) "ALTER TRIGGER @1 failed
")
  (setf (gethash 336397272 *messages*) "CREATE OR ALTER TRIGGER @1 failed
")
  (setf (gethash 336397273 *messages*) "DROP TRIGGER @1 failed
")
  (setf (gethash 336397274 *messages*) "RECREATE TRIGGER @1 failed
")
  (setf (gethash 336397275 *messages*) "CREATE COLLATION @1 failed
")
  (setf (gethash 336397276 *messages*) "DROP COLLATION @1 failed
")
  (setf (gethash 336397277 *messages*) "CREATE DOMAIN @1 failed
")
  (setf (gethash 336397278 *messages*) "ALTER DOMAIN @1 failed
")
  (setf (gethash 336397279 *messages*) "DROP DOMAIN @1 failed
")
  (setf (gethash 336397280 *messages*) "CREATE EXCEPTION @1 failed
")
  (setf (gethash 336397281 *messages*) "ALTER EXCEPTION @1 failed
")
  (setf (gethash 336397282 *messages*) "CREATE OR ALTER EXCEPTION @1 failed
")
  (setf (gethash 336397283 *messages*) "RECREATE EXCEPTION @1 failed
")
  (setf (gethash 336397284 *messages*) "DROP EXCEPTION @1 failed
")
  (setf (gethash 336397285 *messages*) "CREATE SEQUENCE @1 failed
")
  (setf (gethash 336397286 *messages*) "CREATE TABLE @1 failed
")
  (setf (gethash 336397287 *messages*) "ALTER TABLE @1 failed
")
  (setf (gethash 336397288 *messages*) "DROP TABLE @1 failed
")
  (setf (gethash 336397289 *messages*) "RECREATE TABLE @1 failed
")
  (setf (gethash 336397290 *messages*) "CREATE PACKAGE @1 failed
")
  (setf (gethash 336397291 *messages*) "ALTER PACKAGE @1 failed
")
  (setf (gethash 336397292 *messages*) "CREATE OR ALTER PACKAGE @1 failed
")
  (setf (gethash 336397293 *messages*) "DROP PACKAGE @1 failed
")
  (setf (gethash 336397294 *messages*) "RECREATE PACKAGE @1 failed
")
  (setf (gethash 336397295 *messages*) "CREATE PACKAGE BODY @1 failed
")
  (setf (gethash 336397296 *messages*) "DROP PACKAGE BODY @1 failed
")
  (setf (gethash 336397297 *messages*) "RECREATE PACKAGE BODY @1 failed
")
  (setf (gethash 336397298 *messages*) "CREATE VIEW @1 failed
")
  (setf (gethash 336397299 *messages*) "ALTER VIEW @1 failed
")
  (setf (gethash 336397300 *messages*) "CREATE OR ALTER VIEW @1 failed
")
  (setf (gethash 336397301 *messages*) "RECREATE VIEW @1 failed
")
  (setf (gethash 336397302 *messages*) "DROP VIEW @1 failed
")
  (setf (gethash 336397303 *messages*) "DROP SEQUENCE @1 failed
")
  (setf (gethash 336397304 *messages*) "RECREATE SEQUENCE @1 failed
")
  (setf (gethash 336397305 *messages*) "DROP INDEX @1 failed
")
  (setf (gethash 336397306 *messages*) "DROP FILTER @1 failed
")
  (setf (gethash 336397307 *messages*) "DROP SHADOW @1 failed
")
  (setf (gethash 336397308 *messages*) "DROP ROLE @1 failed
")
  (setf (gethash 336397309 *messages*) "DROP USER @1 failed
")
  (setf (gethash 336397310 *messages*) "CREATE ROLE @1 failed
")
  (setf (gethash 336397311 *messages*) "ALTER ROLE @1 failed
")
  (setf (gethash 336397312 *messages*) "ALTER INDEX @1 failed
")
  (setf (gethash 336397313 *messages*) "ALTER DATABASE failed
")
  (setf (gethash 336397314 *messages*) "CREATE SHADOW @1 failed
")
  (setf (gethash 336397315 *messages*) "DECLARE FILTER @1 failed
")
  (setf (gethash 336397316 *messages*) "CREATE INDEX @1 failed
")
  (setf (gethash 336397317 *messages*) "CREATE USER @1 failed
")
  (setf (gethash 336397318 *messages*) "ALTER USER @1 failed
")
  (setf (gethash 336397319 *messages*) "GRANT failed
")
  (setf (gethash 336397320 *messages*) "REVOKE failed
")
  (setf (gethash 336397321 *messages*) "Recursive member of CTE cannot use aggregate or window function
")
  (setf (gethash 336397322 *messages*) "@2 MAPPING @1 failed
")
  (setf (gethash 336397323 *messages*) "ALTER SEQUENCE @1 failed
")
  (setf (gethash 336397324 *messages*) "CREATE GENERATOR @1 failed
")
  (setf (gethash 336397325 *messages*) "SET GENERATOR @1 failed
")
  (setf (gethash 336397326 *messages*) "WITH LOCK can be used only with a single physical table
")
  (setf (gethash 336397327 *messages*) "FIRST/SKIP cannot be used with OFFSET/FETCH or ROWS
")
  (setf (gethash 336397328 *messages*) "WITH LOCK cannot be used with aggregates
")
  (setf (gethash 336397329 *messages*) "WITH LOCK cannot be used with @1
")
  (setf (gethash 336397330 *messages*) "Number of arguments (@1) exceeds the maximum (@2) number of EXCEPTION USING arguments
")
  (setf (gethash 336397331 *messages*) "String literal with @1 bytes exceeds the maximum length of @2 bytes
")
  (setf (gethash 336397332 *messages*) "String literal with @1 characters exceeds the maximum length of @2 characters for the @3 character set
")
  (setf (gethash 336397333 *messages*) "Too many BEGIN...END nesting. Maximum level is @1
")
  (setf (gethash 336723983 *messages*) "unable to open database
")
  (setf (gethash 336723984 *messages*) "error in switch specifications
")
  (setf (gethash 336723985 *messages*) "no operation specified
")
  (setf (gethash 336723986 *messages*) "no user name specified
")
  (setf (gethash 336723987 *messages*) "add record error
")
  (setf (gethash 336723988 *messages*) "modify record error
")
  (setf (gethash 336723989 *messages*) "find/modify record error
")
  (setf (gethash 336723990 *messages*) "record not found for user *messages*) @1
")
  (setf (gethash 336723991 *messages*) "delete record error
")
  (setf (gethash 336723992 *messages*) "find/delete record error
")
  (setf (gethash 336723996 *messages*) "find/display record error
")
  (setf (gethash 336723997 *messages*) "invalid parameter, no switch defined
")
  (setf (gethash 336723998 *messages*) "operation already specified
")
  (setf (gethash 336723999 *messages*) "password already specified
")
  (setf (gethash 336724000 *messages*) "uid already specified
")
  (setf (gethash 336724001 *messages*) "gid already specified
")
  (setf (gethash 336724002 *messages*) "project already specified
")
  (setf (gethash 336724003 *messages*) "organization already specified
")
  (setf (gethash 336724004 *messages*) "first name already specified
")
  (setf (gethash 336724005 *messages*) "middle name already specified
")
  (setf (gethash 336724006 *messages*) "last name already specified
")
  (setf (gethash 336724008 *messages*) "invalid switch specified
")
  (setf (gethash 336724009 *messages*) "ambiguous switch specified
")
  (setf (gethash 336724010 *messages*) "no operation specified for parameters
")
  (setf (gethash 336724011 *messages*) "no parameters allowed for this operation
")
  (setf (gethash 336724012 *messages*) "incompatible switches specified
")
  (setf (gethash 336724044 *messages*) "Invalid user name (maximum 31 bytes allowed)
")
  (setf (gethash 336724045 *messages*) "Warning - maximum 8 significant bytes of password used
")
  (setf (gethash 336724046 *messages*) "database already specified
")
  (setf (gethash 336724047 *messages*) "database administrator name already specified
")
  (setf (gethash 336724048 *messages*) "database administrator password already specified
")
  (setf (gethash 336724049 *messages*) "SQL role name already specified
")
  (setf (gethash 336920577 *messages*) "found unknown switch
")
  (setf (gethash 336920578 *messages*) "please retry, giving a database name
")
  (setf (gethash 336920579 *messages*) "Wrong ODS version, expected @1, encountered @2
")
  (setf (gethash 336920580 *messages*) "Unexpected end of database file.
")
  (setf (gethash 336920605 *messages*) "Can't open database file @1
")
  (setf (gethash 336920606 *messages*) "Can't read a database page
")
  (setf (gethash 336920607 *messages*) "System memory exhausted
")
  (setf (gethash 336986113 *messages*) "Wrong value for access mode
")
  (setf (gethash 336986114 *messages*) "Wrong value for write mode
")
  (setf (gethash 336986115 *messages*) "Wrong value for reserve space
")
  (setf (gethash 336986116 *messages*) "Unknown tag (@1) in info_svr_db_info block after isc_svc_query()
")
  (setf (gethash 336986117 *messages*) "Unknown tag (@1) in isc_svc_query() results
")
  (setf (gethash 336986118 *messages*) "Unknown switch \"@1\"
")
  (setf (gethash 336986159 *messages*) "Wrong value for shutdown mode
")
  (setf (gethash 336986160 *messages*) "could not open file @1
")
  (setf (gethash 336986161 *messages*) "could not read file @1
")
  (setf (gethash 336986162 *messages*) "empty file @1
")
  (setf (gethash 336986164 *messages*) "Invalid or missing parameter for switch @1
")
  (setf (gethash 337051649 *messages*) "Switches trusted_user and trusted_role are not supported from command line
")
  (setf (gethash 337117213 *messages*) "Missing parameter for switch @1
")
  (setf (gethash 337117214 *messages*) "Only one of -LOCK, -UNLOCK, -FIXUP, -BACKUP or -RESTORE should be specified
")
  (setf (gethash 337117215 *messages*) "Unrecognized parameter @1
")
  (setf (gethash 337117216 *messages*) "Unknown switch @1
")
  (setf (gethash 337117217 *messages*) "Fetch password can't be used in service mode
")
  (setf (gethash 337117218 *messages*) "Error working with password file \"@1\"
")
  (setf (gethash 337117219 *messages*) "Switch -SIZE can be used only with -LOCK
")
  (setf (gethash 337117220 *messages*) "None of -LOCK, -UNLOCK, -FIXUP, -BACKUP or -RESTORE specified
")
  (setf (gethash 337117223 *messages*) "IO error reading file *messages*) @1
")
  (setf (gethash 337117224 *messages*) "IO error writing file *messages*) @1
")
  (setf (gethash 337117225 *messages*) "IO error seeking file *messages*) @1
")
  (setf (gethash 337117226 *messages*) "Error opening database file *messages*) @1
")
  (setf (gethash 337117227 *messages*) "Error in posix_fadvise(@1) for database @2
")
  (setf (gethash 337117228 *messages*) "Error creating database file *messages*) @1
")
  (setf (gethash 337117229 *messages*) "Error opening backup file *messages*) @1
")
  (setf (gethash 337117230 *messages*) "Error creating backup file *messages*) @1
")
  (setf (gethash 337117231 *messages*) "Unexpected end of database file @1
")
  (setf (gethash 337117232 *messages*) "Database @1 is not in state (@2) to be safely fixed up
")
  (setf (gethash 337117233 *messages*) "Database error
")
  (setf (gethash 337117234 *messages*) "Username or password is too long
")
  (setf (gethash 337117235 *messages*) "Cannot find record for database \"@1\" backup level @2 in the backup history
")
  (setf (gethash 337117236 *messages*) "Internal error. History query returned null SCN or GUID
")
  (setf (gethash 337117237 *messages*) "Unexpected end of file when reading header of database file \"@1\" (stage @2)
")
  (setf (gethash 337117238 *messages*) "Internal error. Database file is not locked. Flags are @1
")
  (setf (gethash 337117239 *messages*) "Internal error. Cannot get backup guid clumplet
")
  (setf (gethash 337117240 *messages*) "Internal error. Database page @1 had been changed during backup (page SCN=@2, backup SCN=@3)
")
  (setf (gethash 337117241 *messages*) "Database file size is not a multiple of page size
")
  (setf (gethash 337117242 *messages*) "Level 0 backup is not restored
")
  (setf (gethash 337117243 *messages*) "Unexpected end of file when reading header of backup file *messages*) @1
")
  (setf (gethash 337117244 *messages*) "Invalid incremental backup file *messages*) @1
")
  (setf (gethash 337117245 *messages*) "Unsupported version @1 of incremental backup file *messages*) @2
")
  (setf (gethash 337117246 *messages*) "Invalid level @1 of incremental backup file *messages*) @2, expected @3
")
  (setf (gethash 337117247 *messages*) "Wrong order of backup files or invalid incremental backup file detected, file *messages*) @1
")
  (setf (gethash 337117248 *messages*) "Unexpected end of backup file *messages*) @1
")
  (setf (gethash 337117249 *messages*) "Error creating database file *messages*) @1 via copying from *messages*) @2
")
  (setf (gethash 337117250 *messages*) "Unexpected end of file when reading header of restored database file (stage @1)
")
  (setf (gethash 337117251 *messages*) "Cannot get backup guid clumplet from L0 backup
")
  (setf (gethash 337117255 *messages*) "Wrong parameter @1 for switch -D, need ON or OFF
")
  (setf (gethash 337117257 *messages*) "Terminated due to user request
")
  (setf (gethash 337117259 *messages*) "Too complex decompress command (> @1 arguments)
")
  (setf (gethash 337117261 *messages*) "Cannot find record for database \"@1\" backup GUID @2 in the backup history
")
  (setf (gethash 337182750 *messages*) "conflicting actions \"@1\" and \"@2\" found
")
  (setf (gethash 337182751 *messages*) "action switch not found
")
  (setf (gethash 337182752 *messages*) "switch \"@1\" must be set only once
")
  (setf (gethash 337182753 *messages*) "value for switch \"@1\" is missing
")
  (setf (gethash 337182754 *messages*) "invalid value (\"@1\") for switch \"@2\"
")
  (setf (gethash 337182755 *messages*) "unknown switch \"@1\" encountered
")
  (setf (gethash 337182756 *messages*) "switch \"@1\" can be used by service only
")
  (setf (gethash 337182757 *messages*) "switch \"@1\" can be used by interactive user only
")
  (setf (gethash 337182758 *messages*) "mandatory parameter \"@1\" for switch \"@2\" is missing
")
  (setf (gethash 337182759 *messages*) "parameter \"@1\" is incompatible with action \"@2\"
")
  (setf (gethash 337182760 *messages*) "mandatory switch \"@1\" is missing
"))


