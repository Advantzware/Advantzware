/* pGetAuditQueryFilters.i */

&IF DEFINED(FilterFrame) EQ 0 &THEN
&SCOPED-DEFINE FilterFrame {&FRAME-NAME}
&ENDIF

PROCEDURE pGetFilterValues:
    DEFINE INPUT PARAMETER ipcFilter AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableLabel AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FilterFrame}:
        CASE ipcFilter:
            WHEN "INIT" THEN DO:
                EMPTY TEMP-TABLE ttAuditDBTable.
                EMPTY TEMP-TABLE ttAudit.
                FOR EACH ASI._file NO-LOCK
                    WHERE ASI._file._Tbl-type EQ "T"
                    :
                    IF CAN-FIND(FIRST AuditHdr
                                WHERE AuditHdr.AuditDB    EQ "ASI"
                                  AND AuditHdr.AuditTable EQ ASI._file._file-name) THEN DO: 
                        CREATE ttAuditDBTable.
                        ASSIGN
                            cTableLabel               = IF ASI._file._file-label NE ? THEN ASI._file._file-label ELSE ""
                            ttAuditDBTable.AuditDB    = "ASI"
                            ttAuditDBTable.AuditTable = ASI._file._file-name
                            ttAuditDBTable.TableName  = cTableLabel + " (" + ASI._file._file-name + ")"
                            .
                        FOR EACH ASI._field OF ASI._file NO-LOCK
                            :
                            CREATE ttAudit.
                            ASSIGN
                                ttAudit.AuditDB    = "ASI"
                                ttAudit.AuditTable = ASI._file._file-name
                                ttAudit.DBTable    = YES
                                ttAudit.TableName  = cTableLabel + " (" + ASI._file._file-name + ")"
                                ttAudit.AuditField = ASI._field._field-name
                                cFieldLabel        = IF ASI._field._label NE ? THEN ASI._field._label ELSE ""
                                ttAudit.FieldName  = cFieldLabel + " (" + ASI._field._field-name + ")"
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
                            ttAuditDBTable.TableName  = "[" + prgrms.mnemonic + "] "
                                                      + prgrms.prgTitle
                                                      + " (" + prgrms.prgmname + ")"
                                                      .
                        CREATE ttAudit.
                        ASSIGN
                            ttAudit.AuditDB    = "ASI"
                            ttAudit.AuditTable = prgrms.prgmname
                            ttAudit.DBTable    = NO
                            ttAudit.TableName  = "[" + prgrms.mnemonic + "] "
                                               + prgrms.prgTitle
                                               + " (" + prgrms.prgmname + ")"
                                               .
                    END. /* if can-find */
                END. /* each prgrms */
                /* user */
                svUser:LIST-ITEMS = "All".
                FOR EACH users NO-LOCK:
                    IF CAN-FIND(FIRST AuditHdr
                                WHERE AuditHdr.AuditUser EQ users.user_id) THEN DO:  
                    svUser:ADD-LAST(users.user_id).
                    END. /* if can-find */
                END. /* each users */
                ASSIGN
                    svUser:INNER-LINES  = svUser:NUM-ITEMS
                    svUser:SCREEN-VALUE = "All" /*svUser:ENTRY(1)*/
                    svUser
                    .
            END. /* when init */
            WHEN "ALL" THEN DO:
                /* From and To Date Range */
                ASSIGN
                    dtStartDateTime          = DATETIME(STRING(TODAY - 90,"99/99/9999") + " 00:00:00")
                    svStartDate              = DATE(dtStartDateTime)
                    svStartDate:SCREEN-VALUE = STRING(svStartDate)
                    dtEndDateTime            = DATETIME(STRING(12/31/2049,"99/99/9999") + " 23:59:59")
                    svEndDate                = DATE(dtEndDateTime)
                    svEndDate:SCREEN-VALUE   = STRING(svEndDate)
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
                    svDB:SCREEN-VALUE = "All" /*svDB:ENTRY(1)*/
                    svDB
                    .
            END. /* when all */
            WHEN "TABLE" THEN DO:
                /* table */
                svTable:LIST-ITEM-PAIRS = "All,All".
                FOR EACH ttAudit
                    WHERE (ttAudit.AuditDB EQ svDB OR svDB EQ "All")
                      AND ((ttAudit.DBTable EQ YES AND  svType NE "LOG" AND svType NE "TRACK")
                       OR  (ttAudit.DBTable EQ NO  AND (svType EQ "LOG" OR  svType EQ "TRACK")
                       OR   svType EQ "All"))
                    BREAK BY ttAudit.TableName
                    :
                    IF FIRST-OF(ttAudit.TableName) THEN
                    svTable:ADD-LAST(ttAudit.TableName,ttAudit.AuditTable).
                END. /* each audithdr */
                ASSIGN
                    svTable:INNER-LINES  = svTable:NUM-ITEMS
                    svTable:SCREEN-VALUE = "All" /*svTable:ENTRY(1)*/
                    svTable
                    .
            END. /* when table */
            WHEN "FIELD" THEN DO:
                /* field */
                svField:LIST-ITEM-PAIRS = "All,All".
                FOR EACH ttAudit
                    WHERE (ttAudit.AuditDB EQ svDB
                       OR svDB EQ "All")
                      AND (ttAudit.AuditTable EQ svTable
                       OR svTable EQ "All")
                    BREAK BY ttAudit.FieldName
                    :
                    IF FIRST-OF(ttAudit.FieldName) THEN
                    svField:ADD-LAST(ttAudit.FieldName,ttAudit.AuditField).
                END. /* each audithdr */
                ASSIGN
                    svField:INNER-LINES  = svField:NUM-ITEMS
                    svField:SCREEN-VALUE = "All" /*svField:ENTRY(1)*/
                    svField
                    .
            END. /* when field */
        END CASE.
    END. /* do with frame */
END PROCEDURE.
