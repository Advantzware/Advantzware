/* pGetAuditQueryFilters.i */

&IF DEFINED(FilterFrame) EQ 0 &THEN
&SCOPED-DEFINE FilterFrame {&FRAME-NAME}
&ENDIF

PROCEDURE pGetFilterValues:
    DEFINE INPUT PARAMETER ipcFilter AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FilterFrame}:
        CASE ipcFilter:
            WHEN "INIT" THEN DO:
                EMPTY TEMP-TABLE ttAuditDBTable.
                EMPTY TEMP-TABLE ttAudit.
                EMPTY TEMP-TABLE ttuser.
                FOR EACH ASI._file NO-LOCK
                    :
                    IF CAN-FIND(FIRST AuditHdr
                                WHERE AuditHdr.AuditDB    EQ "ASI"
                                  AND AuditHdr.AuditTable EQ ASI._file._file-name) THEN DO: 
                        CREATE ttAuditDBTable.
                        ASSIGN
                            ttAuditDBTable.AuditDB    = "ASI"
                            ttAuditDBTable.AuditTable = ASI._file._file-name
                            .
                        FOR EACH ASI._field OF ASI._file NO-LOCK
                            :
                            CREATE ttAudit.
                            ASSIGN
                                ttAudit.AuditDB    = "ASI"
                                ttAudit.AuditTable = ASI._file._file-name
                                ttAudit.AuditField = ASI._field._field-name
                                .
                        END. /* each _field */
                    END. /* if can-find */
                END. /* each _file */
                FOR EACH prgrms NO-LOCK:
                    IF CAN-FIND(FIRST AuditHdr
                                WHERE AuditHdr.AuditDB    EQ "ASI"
                                  AND AuditHdr.AuditTable EQ prgrms.prgmname) THEN DO:
                        CREATE ttAuditDBTable.
                        ASSIGN
                            ttAuditDBTable.AuditDB    = "ASI"
                            ttAuditDBTable.AuditTable = prgrms.prgmname
                            .
                        CREATE ttAudit.
                        ASSIGN
                            ttAudit.AuditDB    = "ASI"
                            ttAudit.AuditTable = prgrms.prgmname
                            .
                    END. /* if can-find */
                END. /* each prgrms */
                FOR EACH users NO-LOCK:
                    IF CAN-FIND(FIRST AuditHdr
                                WHERE AuditHdr.AuditUser EQ users.user_id) THEN DO:  
                        CREATE ttUser.
                        ttUser.AuditUser = users.user_id.
                    END. /* if can-find */
                END. /* each users */
            END. /* when init */
            WHEN "ALL" THEN DO:
                /* From and To Date Range */
                ASSIGN
                    dtStartDateTime = DATETIME(STRING(TODAY - 90,"99/99/9999") + " 00:00:00")
                    svStartDate = DATE(dtStartDateTime)
                    svStartDate:SCREEN-VALUE = STRING(svStartDate)
                    dtEndDateTime = DATETIME(STRING(TODAY,"99/99/9999") + " 23:59:59")
                    svEndDate = DATE(dtEndDateTime)
                    svEndDate:SCREEN-VALUE = STRING(svEndDate)
                    .
                /* user */
                svUser:LIST-ITEMS = "All".
                FOR EACH ttUser:
                    svUser:ADD-LAST(ttUser.AuditUser).
                END. /* each audithdr */
                ASSIGN
                    svUser:INNER-LINES  = svUser:NUM-ITEMS
                    svUser:SCREEN-VALUE = svUser:ENTRY(1)
                    .
                /* db */
                svDB:LIST-ITEMS = "All".
                FOR EACH ttAudit
                    BREAK BY ttAudit.AuditDB
                    :
                    IF FIRST-OF(ttAudit.AuditDB) THEN
                    svDB:ADD-LAST(ttAudit.AuditDB).
                END. /* each audithdr */
                ASSIGN
                    svDB:INNER-LINES  = svDB:NUM-ITEMS
                    svDB:SCREEN-VALUE = svDB:ENTRY(1)
                    .
            END. /* when all */
            WHEN "TABLE" THEN DO:
                /* table */
                svTable:LIST-ITEMS = "All".
                FOR EACH ttAudit
                    WHERE ttAudit.AuditDB EQ svDB
                       OR svDB EQ "All"
                    BREAK BY ttAudit.AuditTable
                    :
                    IF FIRST-OF(ttAudit.AuditTable) THEN
                    svTable:ADD-LAST(ttAudit.AuditTable).
                END. /* each audithdr */
                ASSIGN
                    svTable:INNER-LINES  = svTable:NUM-ITEMS
                    svTable:SCREEN-VALUE = svTable:ENTRY(1)
                    .
            END. /* when table */
            WHEN "FIELD" THEN DO:
                /* field */
                svField:LIST-ITEMS = "All".
                FOR EACH ttAudit
                    WHERE (ttAudit.AuditDB EQ svDB
                       OR svDB EQ "All")
                      AND (ttAudit.AuditTable EQ svTable
                       OR svTable EQ "All")
                    BREAK BY ttAudit.AuditField
                    :
                    IF FIRST-OF(ttAudit.AuditField) THEN
                    svField:ADD-LAST(ttAudit.AuditField).
                END. /* each audithdr */
                ASSIGN
                    svField:INNER-LINES  = svField:NUM-ITEMS
                    svField:SCREEN-VALUE = svField:ENTRY(1)
                    .
            END. /* when field */
        END CASE.
    END. /* do with frame */

END PROCEDURE.
