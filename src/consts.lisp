
(in-package #:cl-firebird)

;; firebird\src\remote\protocol.h 
;; Protocol 10 includes support for warnings and removes the requirement for
;; encoding and decoding status codes
;; Since protocol 11 we must be separated from Borland Interbase.
;; Therefore always set highmost bit in protocol version to 1.
;; For unsigned protocol version this does not break version's compare.
;; Protocol 11 has support for user authentication related
;; operations (op_update_account_info, op_authenticate_user and
;; op_trusted_auth). When specific operation is not supported,
;; we say "sorry".
;; Protocol 12 has support for asynchronous call op_cancel.
;; Currently implemented asynchronously only for TCP/IP.
;; Protocol 13 has support for authentication plugins (op_cont_auth).
;; It also transfers SQL messages in the packed (null aware) format.
;; Protocol 14:
;;	- fixes a bug in database crypt key callback
;; Protocol 15:
;;	- supports crypt key callback at connect phase
;; Protocol 16:
;;	- supports statement timeouts

(defconstant +fb-protocol-flag+ #x8000)
(defconstant +protocol-version10+ 10)
(defconstant +protocol-version11+ (logior +fb-protocol-flag+ 11))
(defconstant +protocol-version12+ (logior +fb-protocol-flag+ 12))
(defconstant +protocol-version13+ (logior +fb-protocol-flag+ 13))
(defconstant +protocol-version14+ (logior +fb-protocol-flag+ 14))
(defconstant +protocol-version15+ (logior +fb-protocol-flag+ 15))
(defconstant +protocol-version16+ (logior +fb-protocol-flag+ 16))

(defconstant +connect-version2+  2)
(defconstant +connect-version3+  3)

(defconstant +arch-generic+      1)   ; Generic -- always use canonical forms
(defconstant +arch-sun+          3)
(defconstant +arch-sun4+         8)
(defconstant +arch-sunx86+       9)
(defconstant +arch-hpux+        10)
(defconstant +arch-rt+          14)
(defconstant +arch-intel-32+    29)   ; generic Intel chip w/32-bit compilation
(defconstant +arch-linux+       36)
(defconstant +arch-freebsd+     37)
(defconstant +arch-netbsd+      38)
(defconstant +arch-darwin-ppc+  39)
(defconstant +arch-winnt-64+    40)
(defconstant +arch-darwin-x64+  41)
(defconstant +arch-darwin-ppc64+ 42)
(defconstant +arch-arm+         43)
(defconstant +arch-max+         44)   ; Keep this at the end

;; Protocol Types (accept_type)
;;(defconstant +ptype-page+        1)   ; Page server protocol (?)
;;(defconstant +ptype-rpc+         2)   ; Simple remote procedure call
(defconstant +ptype-batch-send+  3)   ; Batch sends, no asynchrony
(defconstant +ptype-out-of-band+ 4)   ; Batch sends w/ out of band notification
(defconstant +ptype-lazy-send+   5)   ; Deferred packets delivery

;; upper byte is used for protocol flags
(defconstant +pflag-compress+ #x100)  ; Turn on compression if possible

(defconstant +max-objct-handles+ 65000)
(defconstant +invalid-object+ 0xffff)

(defparameter +default-charset+ "UTF8")
(defparameter +legacy-password-salt+ "9z")

(defconstant +isc-time-seconds-precision+ 10000)
(defconstant +max-char-length+ 32767)
(defconstant +blob-segment-size+ 32000)

(defconstant +description-name+ 	0)
(defconstant +description-type-code+	1)
(defconstant +description-display-size+ 2)
(defconstant +description-internal-size+ 3)
(defconstant +description-precision+	4)
(defconstant +description-scale+	5)
(defconstant +description-null-ok+	6)

(defconstant +isc-info-end+ 1)
(defconstant +isc-info-truncated+ 2)
(defconstant +isc-info-error+ 3)
(defconstant +isc-info-data-not-ready+ 4)
(defconstant +isc-info-length+ 126)
(defconstant +isc-info-flag-end+ 127)

(defconstant +isc-info-db-id+ 4)
(defconstant +isc-info-reads+ 5)
(defconstant +isc-info-writes+ 6)
(defconstant +isc-info-fetches+ 7)
(defconstant +isc-info-marks+ 8)
(defconstant +isc-info-implementation+ 11)
(defconstant +isc-info-version+ 12)
(defconstant +isc-info-base-level+ 13)
(defconstant +isc-info-page-size+ 14)
(defconstant +isc-info-num-buffers+ 15)
(defconstant +isc-info-limbo+ 16)
(defconstant +isc-info-current-memory+ 17)
(defconstant +isc-info-max-memory+ 18)
(defconstant +isc-info-window-turns+ 19)
(defconstant +isc-info-license+ 20)
(defconstant +isc-info-allocation+ 21)
(defconstant +isc-info-attachment-id+ 22)
(defconstant +isc-info-read-seq-count+ 23)
(defconstant +isc-info-read-idx-count+ 24)
(defconstant +isc-info-insert-count+ 25)
(defconstant +isc-info-update-count+ 26)
(defconstant +isc-info-delete-count+ 27)
(defconstant +isc-info-backout-count+ 28)
(defconstant +isc-info-purge-count+ 29)
(defconstant +isc-info-expunge-count+ 30)
(defconstant +isc-info-sweep-interval+ 31)
(defconstant +isc-info-ods-version+ 32)
(defconstant +isc-info-ods-minor-version+ 33)
(defconstant +isc-info-no-reserve+ 34)
(defconstant +isc-info-logfile+ 35)
(defconstant +isc-info-cur-logfile-name+ 36)
(defconstant +isc-info-cur-log-part-offset+ 37)
(defconstant +isc-info-num-wal-buffers+ 38)
(defconstant +isc-info-wal-buffer-size+ 39)
(defconstant +isc-info-wal-ckpt-length+ 40)
(defconstant +isc-info-wal-cur-ckpt-interval+ 41)
(defconstant +isc-info-wal-prv-ckpt-fname+ 42)
(defconstant +isc-info-wal-prv-ckpt-poffset+ 43)
(defconstant +isc-info-wal-recv-ckpt-fname+ 44)
(defconstant +isc-info-wal-recv-ckpt-poffset+ 45)
(defconstant +isc-info-wal-grpc-wait-usecs+ 47)
(defconstant +isc-info-wal-num-io+ 48)
(defconstant +isc-info-wal-avg-io-size+ 49)
(defconstant +isc-info-wal-num-commits+ 50)
(defconstant +isc-info-wal-avg-grpc-size+ 51)
(defconstant +isc-info-forced-writes+ 52)
(defconstant +isc-info-user-names+ 53)
(defconstant +isc-info-page-errors+ 54)
(defconstant +isc-info-record-errors+ 55)
(defconstant +isc-info-bpage-errors+ 56)
(defconstant +isc-info-dpage-errors+ 57)
(defconstant +isc-info-ipage-errors+ 58)
(defconstant +isc-info-ppage-errors+ 59)
(defconstant +isc-info-tpage-errors+ 60)
(defconstant +isc-info-set-page-buffers+ 61)
(defconstant +isc-info-db-sql-dialect+ 62)
(defconstant +isc-info-db-read-only+ 63)
(defconstant +isc-info-db-size-in-pages+ 64)
(defconstant +isc-info-att-charset+ 101)
(defconstant +isc-info-db-class+ 102)
(defconstant +isc-info-firebird-version+ 103)
(defconstant +isc-info-oldest-transaction+ 104)
(defconstant +isc-info-oldest-active+ 105)
(defconstant +isc-info-oldest-snapshot+ 106)
(defconstant +isc-info-next-transaction+ 107)
(defconstant +isc-info-db-provider+ 108)
(defconstant +isc-info-active-transactions+ 109)
(defconstant +isc-info-active-tran-count+ 110)
(defconstant +isc-info-creation-date+ 111)
(defconstant +isc-info-db-file-size+ 112)

;;  +isc-info-sql-records+ items
(defconstant +isc-info-req-select-count+ 13)
(defconstant +isc-info-req-insert-count+ 14)
(defconstant +isc-info-req-update-count+ 15)
(defconstant +isc-info-req-delete-count+ 16)

(defconstant +isc-info-svc-svr-db-info+ 50)
(defconstant +isc-info-svc-get-license+ 51)
(defconstant +isc-info-svc-get-license-mask+ 52)
(defconstant +isc-info-svc-get-config+ 53)
(defconstant +isc-info-svc-version+ 54)
(defconstant +isc-info-svc-server-version+ 55)
(defconstant +isc-info-svc-implementation+ 56)
(defconstant +isc-info-svc-capabilities+ 57)
(defconstant +isc-info-svc-user-dbpath+ 58)
(defconstant +isc-info-svc-get-env+ 59)
(defconstant +isc-info-svc-get-env-lock+ 60)
(defconstant +isc-info-svc-get-env-msg+ 61)
(defconstant +isc-info-svc-line+ 62)
(defconstant +isc-info-svc-to-eof+ 63)
(defconstant +isc-info-svc-timeout+ 64)
(defconstant +isc-info-svc-get-licensed-users+ 65)
(defconstant +isc-info-svc-limbo-trans+ 66)
(defconstant +isc-info-svc-running+ 67)
(defconstant +isc-info-svc-get-users+ 68)

(defconstant +sql-type-text+ 452)
(defconstant +sql-type-varying+ 448)
(defconstant +sql-type-short+ 500)
(defconstant +sql-type-long+ 496)
(defconstant +sql-type-float+ 482)
(defconstant +sql-type-double+ 480)
(defconstant +sql-type-d-float+ 530)
(defconstant +sql-type-timestamp+ 510)
(defconstant +sql-type-blob+ 520)
(defconstant +sql-type-array+ 540)
(defconstant +sql-type-quad+ 550)
(defconstant +sql-type-time+ 560)
(defconstant +sql-type-date+ 570)
(defconstant +sql-type-int64+ 580)
(defconstant +sql-type-timestamp-tz+ 32754)
(defconstant +sql-type-time-tz+ 32756)
(defconstant +sql-type-dec-fixed+ 32758)
(defconstant +sql-type-dec64+ 32760)
(defconstant +sql-type-dec128+ 32762)
(defconstant +sql-type-boolean+ 32764)
(defconstant +sql-type-null+ 32766)


(defconstant +isolation-level-read-commited-legacy+ 0)
(defconstant +isolation-level-read-commited+ 1)
(defconstant +isolation-level-repeatable-read+ 2)
(defconstant +isolation-level-snapshot+ +isolation-level-repeatable-read+)
(defconstant +isolation-level-serializable+ 3)
(defconstant +isolation-level-read-commited-ro+ 4)




;; Database Parameter Block parameter
(defconstant +isc-dpb-version1+ 1)
(defconstant +isc-dpb-version2+ 2)	; >= FB30
(defconstant +isc-dpb-cdd-pathname+ 1)
(defconstant +isc-dpb-allocation+ 2)
(defconstant +isc-dpb-journal+ 3)
(defconstant +isc-dpb-page-size+ 4)
(defconstant +isc-dpb-num-buffers+ 5)
(defconstant +isc-dpb-buffer-length+ 6)
(defconstant +isc-dpb-debug+ 7)
(defconstant +isc-dpb-garbage-collect+ 8)
(defconstant +isc-dpb-verify+ 9)
(defconstant +isc-dpb-sweep+ 10)
(defconstant +isc-dpb-enable-journal+ 11)
(defconstant +isc-dpb-disable-journal+ 12)
(defconstant +isc-dpb-dbkey-scope+ 13)
(defconstant +isc-dpb-number-of-users+ 14)
(defconstant +isc-dpb-trace+ 15)
(defconstant +isc-dpb-no-garbage-collect+ 16)
(defconstant +isc-dpb-damaged+ 17)
(defconstant +isc-dpb-license+ 18)
(defconstant +isc-dpb-sys-user-name+ 19)
(defconstant +isc-dpb-encrypt-key+ 20)
(defconstant +isc-dpb-activate-shadow+ 21)
(defconstant +isc-dpb-sweep-interval+ 22)
(defconstant +isc-dpb-delete-shadow+ 23)
(defconstant +isc-dpb-force-write+ 24)
(defconstant +isc-dpb-begin-log+ 25)
(defconstant +isc-dpb-quit-log+ 26)
(defconstant +isc-dpb-no-reserve+ 27)
(defconstant +isc-dpb-user-name+ 28)
(defconstant +isc-dpb-password+ 29)
(defconstant +isc-dpb-password-enc+ 30)
(defconstant +isc-dpb-sys-user-name-enc+ 31)
(defconstant +isc-dpb-interp+ 32)
(defconstant +isc-dpb-online-dump+ 33)
(defconstant +isc-dpb-old-file-size+ 34)
(defconstant +isc-dpb-old-num-files+ 35)
(defconstant +isc-dpb-old-file+ 36)
(defconstant +isc-dpb-old-start-page+ 37)
(defconstant +isc-dpb-old-start-seqno+ 38)
(defconstant +isc-dpb-old-start-file+ 39)
(defconstant +isc-dpb-drop-walfile+ 40)
(defconstant +isc-dpb-old-dump-id+ 41)
(defconstant +isc-dpb-wal-backup-dir+ 42)
(defconstant +isc-dpb-wal-chkptlen+ 43)
(defconstant +isc-dpb-wal-numbufs+ 44)
(defconstant +isc-dpb-wal-bufsize+ 45)
(defconstant +isc-dpb-wal-grp-cmt-wait+ 46)
(defconstant +isc-dpb-lc-messages+ 47)
(defconstant +isc-dpb-lc-ctype+ 48)
(defconstant +isc-dpb-cache-manager+ 49)
(defconstant +isc-dpb-shutdown+ 50)
(defconstant +isc-dpb-online+ 51)
(defconstant +isc-dpb-shutdown-delay+ 52)
(defconstant +isc-dpb-reserved+ 53)
(defconstant +isc-dpb-overwrite+ 54)
(defconstant +isc-dpb-sec-attach+ 55)
(defconstant +isc-dpb-disable-wal+ 56)
(defconstant +isc-dpb-connect-timeout+ 57)
(defconstant +isc-dpb-dummy-packet-interval+ 58)
(defconstant +isc-dpb-gbak-attach+ 59)
(defconstant +isc-dpb-sql-role-name+ 60)
(defconstant +isc-dpb-set-page-buffers+ 61)
(defconstant +isc-dpb-working-directory+ 62)
(defconstant +isc-dpb-sql-dialect+ 63)
(defconstant +isc-dpb-set-db-readonly+ 64)
(defconstant +isc-dpb-set-db-sql-dialect+ 65)
(defconstant +isc-dpb-gfix-attach+ 66)
(defconstant +isc-dpb-gstat-attach+ 67)
(defconstant +isc-dpb-set-db-charset+ 68)
(defconstant +isc-dpb-gsec-attach+ 69)
(defconstant +isc-dpb-address-path+ 70)
(defconstant +isc-dpb-process-id+ 71)
(defconstant +isc-dpb-no-db-triggers+ 72)
(defconstant +isc-dpb-trusted-auth+ 73)
(defconstant +isc-dpb-process-name+ 74)
(defconstant +isc-dpb-trusted-role+ 75)
(defconstant +isc-dpb-org-filename+ 76)
(defconstant +isc-dpb-utf8-filename+ 77)
(defconstant +isc-dpb-ext-call-depth+ 78)
(defconstant +isc-dpb-auth-block+ 79)
(defconstant +isc-dpb-client-version+ 80)
(defconstant +isc-dpb-remote-protocol+ 81)
(defconstant +isc-dpb-host-name+ 82)
(defconstant +isc-dpb-os-user+ 83)
(defconstant +isc-dpb-specific-auth-data+ 84)
(defconstant +isc-dpb-auth-plugin-list+ 85)
(defconstant +isc-dpb-auth-plugin-name+ 86)
(defconstant +isc-dpb-config+ 87)
(defconstant +isc-dpb-nolinger+ 88)
(defconstant +isc-dpb-reset-icu+ 89)
(defconstant +isc-dpb-map-attach+ 90)
(defconstant +isc-dpb-session-time-zone+ 91)


;; Transaction Parameter Block parameter
(defconstant +isc-tpb-version1+ 1)
(defconstant +isc-tpb-version3+ 3)
(defconstant +isc-tpb-consistency+ 1)
(defconstant +isc-tpb-concurrency+ 2)
(defconstant +isc-tpb-shared+ 3)
(defconstant +isc-tpb-protected+ 4)
(defconstant +isc-tpb-exclusive+ 5)
(defconstant +isc-tpb-wait+ 6)
(defconstant +isc-tpb-nowait+ 7)
(defconstant +isc-tpb-read+ 8)
(defconstant +isc-tpb-write+ 9)
(defconstant +isc-tpb-lock-read+ 10)
(defconstant +isc-tpb-lock-write+ 11)
(defconstant +isc-tpb-verb-time+ 12)
(defconstant +isc-tpb-commit-time+ 13)
(defconstant +isc-tpb-ignore-limbo+ 14)
(defconstant +isc-tpb-read-committed+ 15)
(defconstant +isc-tpb-autocommit+ 16)
(defconstant +isc-tpb-rec-version+ 17)
(defconstant +isc-tpb-no-rec-version+ 18)
(defconstant +isc-tpb-restart-requests+ 19)
(defconstant +isc-tpb-no-auto-undo+ 20)
(defconstant +isc-tpb-lock-timeout+ 21)


;; Operation (packet) types
(defconstant +op-void+ 0)
(defconstant +op-connect+ 1)
(defconstant +op-exit+ 2)
(defconstant +op-accept+ 3)
(defconstant +op-reject+ 4)
(defconstant +op-protocol+ 5)
(defconstant +op-disconnect+ 6)
(defconstant +op-response+ 9)
;; Full context server operations
(defconstant +op-attach+ 19)
(defconstant +op-create+ 20)
(defconstant +op-detach+ 21)
(defconstant +op-transaction+ 29)
(defconstant +op-commit+ 30)
(defconstant +op-rollback+ 31)
(defconstant +op-open-blob+ 35)
(defconstant +op-get-segment+ 36)
(defconstant +op-put-segment+ 37)
(defconstant +op-close-blob+ 39)
(defconstant +op-info-database+ 40)
(defconstant +op-info-transaction+ 42)
(defconstant +op-batch-segments+ 44)
(defconstant +op-que-events+ 48)
(defconstant +op-cancel-events+ 49)
(defconstant +op-commit-retaining+ 50)
(defconstant +op-event+ 52)
(defconstant +op-connect-request+ 53)
(defconstant +op-aux-connect+ 53)
(defconstant +op-create-blob2+ 57)
;; DSQL operations
(defconstant +op-allocate-statement+ 62)
(defconstant +op-execute+ 63)
(defconstant +op-exec-immediate+ 64)
(defconstant +op-fetch+ 65)
(defconstant +op-fetch-response+ 66)
(defconstant +op-free-statement+ 67)
(defconstant +op-prepare-statement+ 68)
(defconstant +op-set-cursor+ 69)
(defconstant +op-info-sql+ 70)
(defconstant +op-dummy+ 71)
(defconstant +op-execute2+ 76)
(defconstant +op-sql-response+ 78)
(defconstant +op-drop-database+ 81)
(defconstant +op-service-attach+ 82)
(defconstant +op-service-detach+ 83)
(defconstant +op-service-info+ 84)
(defconstant +op-service-start+ 85)
(defconstant +op-rollback-retaining+ 86)
;; FB3
(defconstant +op-update-account-info+ 87)
(defconstant +op-authenticate-user+ 88)
(defconstant +op-partial+ 89)
(defconstant +op-trusted-auth+ 90)
(defconstant +op-cancel+ 91)
(defconstant +op-cont-auth+ 92)
(defconstant +op-ping+ 93)
(defconstant +op-accept-data+ 94)
(defconstant +op-abort-aux-connection+ 95)
(defconstant +op-crypt+ 96)
(defconstant +op-crypt-key-callback+ 97)
(defconstant +op-cond-accept+ 98)

(defconstant +op-batch-create+ 99)
(defconstant +op-batch-msg+ 100)
(defconstant +op-batch-exec+ 101)
(defconstant +op-batch-rls+ 102)
(defconstant +op-batch-cs+ 103)
(defconstant +op-batch-regblob+ 104)
(defconstant +op-batch-blob-stream+ 105)
(defconstant +op-batch-set-bpb+ 106)
(defconstant +op-repl-data+ 107)
(defconstant +op-repl-req+ 108)



(defconstant +cnct-user+ 1)
(defconstant +cnct-passwd+ 2)
(defconstant +cnct-host+ 4)
(defconstant +cnct-group+ 5)
(defconstant +cnct-user-verification+ 6)
(defconstant +cnct-specific-data+ 7)
(defconstant +cnct-plugin-name+ 8)
(defconstant +cnct-login+ 9)
(defconstant +cnct-plugin-list+ 10)
(defconstant +cnct-client-crypt+ 11)



;; Transaction informatino items
(defconstant +isc-info-tra-id+ 4)
(defconstant +isc-info-tra-oldest-interesting+ 5)
(defconstant +isc-info-tra-oldest-snapshot+ 6)
(defconstant +isc-info-tra-oldest-active+ 7)
(defconstant +isc-info-tra-isolation+ 8)
(defconstant +isc-info-tra-access+ 9)
(defconstant +isc-info-tra-lock-timeout+ 10)

;; SQL information items)
(defconstant +isc-info-sql-select+ 4)
(defconstant +isc-info-sql-bind+ 5)
(defconstant +isc-info-sql-num-variables+ 6)
(defconstant +isc-info-sql-describe-vars+ 7)
(defconstant +isc-info-sql-describe-end+ 8)
(defconstant +isc-info-sql-sqlda-seq+ 9)
(defconstant +isc-info-sql-message-seq+ 10)
(defconstant +isc-info-sql-type+ 11)
(defconstant +isc-info-sql-sub-type+ 12)
(defconstant +isc-info-sql-scale+ 13)
(defconstant +isc-info-sql-length+ 14)
(defconstant +isc-info-sql-null-ind+ 15)
(defconstant +isc-info-sql-field+ 16)
(defconstant +isc-info-sql-relation+ 17)
(defconstant +isc-info-sql-owner+ 18)
(defconstant +isc-info-sql-alias+ 19)
(defconstant +isc-info-sql-sqlda-start+ 20)
(defconstant +isc-info-sql-stmt-type+ 21)
(defconstant +isc-info-sql-get-plan+ 22)
(defconstant +isc-info-sql-records+ 23)
(defconstant +isc-info-sql-batch-fetch+ 24)

(defconstant +isc-info-sql-stmt-select+ 1)
(defconstant +isc-info-sql-stmt-insert+ 2)
(defconstant +isc-info-sql-stmt-update+ 3)
(defconstant +isc-info-sql-stmt-delete+ 4)
(defconstant +isc-info-sql-stmt-ddl+ 5)
(defconstant +isc-info-sql-stmt-get-segment+ 6)
(defconstant +isc-info-sql-stmt-put-segment+ 7)
(defconstant +isc-info-sql-stmt-exec-procedure+ 8)
(defconstant +isc-info-sql-stmt-start-trans+ 9)
(defconstant +isc-info-sql-stmt-commit+ 10)
(defconstant +isc-info-sql-stmt-rollback+ 11)
(defconstant +isc-info-sql-stmt-select-for-upd+ 12)
(defconstant +isc-info-sql-stmt-set-generator+ 13)
(defconstant +isc-info-sql-stmt-savepoint+ 14)

;; Parse Status Vector
(defconstant +isc-arg-end+       0)
(defconstant +isc-arg-gds+       1)
(defconstant +isc-arg-string+    2)
(defconstant +isc-arg-cstring+   3)
(defconstant +isc-arg-number+    4)
(defconstant +isc-arg-interpreted+ 5)
(defconstant +isc-arg-vms+       6)
(defconstant +isc-arg-unix+      7)
(defconstant +isc-arg-domain+    8)
(defconstant +isc-arg-dos+       9)
(defconstant +isc-arg-mpexl+     10)
(defconstant +isc-arg-mpexl-ipc+ 11)
(defconstant +isc-arg-next-mach+ 15)
(defconstant +isc-arg-netware+   16)
(defconstant +isc-arg-win32+     17)
(defconstant +isc-arg-warning+   18)
(defconstant +isc-arg-sql-state+ 19)

(defconstant +DSQL-close+     1)
(defconstant +DSQL-drop+      2)
(defconstant +DSQL-unprepare+ 4)      ; >= 2.5

(defparameter +info-sql-select-describe-vars+
  (vector
   +isc-info-sql-select+
   +isc-info-sql-describe-vars+
   +isc-info-sql-sqlda-seq+
   +isc-info-sql-type+
   +isc-info-sql-sub-type+
   +isc-info-sql-scale+
   +isc-info-sql-length+
   +isc-info-sql-null-ind+
   +isc-info-sql-field+
   +isc-info-sql-relation+
   +isc-info-sql-owner+
   +isc-info-sql-alias+
   +isc-info-sql-describe-end+))

