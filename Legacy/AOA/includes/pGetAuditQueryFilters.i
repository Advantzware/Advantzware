/* pGetAuditQueryFilters.i */

&IF DEFINED(FilterFrame) EQ 0 &THEN
&SCOPED-DEFINE FilterFrame {&FRAME-NAME}
&ENDIF

PROCEDURE pGetFilterValues:
    DEFINE INPUT PARAMETER ipcFilter AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cValues AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FilterFrame}:
        CASE ipcFilter:
            WHEN "ALL" THEN DO:
                /* From and To Date Range */
                FIND FIRST AuditHdr NO-LOCK
                     USE-INDEX AuditDateTime
                     NO-ERROR.
                ASSIGN
                    dtStartDateTime = IF AVAILABLE AuditHdr THEN AuditHdr.AuditDateTime
                                      ELSE DATETIME(STRING(TODAY,"99/99/9999") + " 00:00:00")
                    svStartDate = DATE(dtStartDateTime)
                    svStartDate:SCREEN-VALUE = STRING(svStartDate)
                    .
                FIND LAST AuditHdr NO-LOCK
                     USE-INDEX AuditDateTime
                     NO-ERROR.
                ASSIGN
                    dtEndDateTime = IF AVAILABLE AuditHdr THEN AuditHdr.AuditDateTime
                                      ELSE DATETIME(STRING(TODAY,"99/99/9999") + " 23:59:59")
                    svEndDate = DATE(dtEndDateTime)
                    svEndDate:SCREEN-VALUE = STRING(svEndDate)
                    .
                /* user */
                cValues = "ALL".
                FOR EACH AuditHdr NO-LOCK
                    BREAK BY AuditHdr.AuditUser
                    :
                    IF FIRST-OF(AuditHdr.AuditUser) THEN
                    cValues = cValues + "," + AuditHdr.AuditUser.
                END. /* each audithdr */
                ASSIGN
                    svUser:LIST-ITEMS   = cValues
                    svUser:INNER-LINES  = svUser:NUM-ITEMS
                    svUser:SCREEN-VALUE = svUser:ENTRY(1)
                    .
                /* db */
                cValues = "ALL".
                FOR EACH AuditHdr NO-LOCK
                    BREAK BY AuditHdr.AuditDB
                    :
                    IF FIRST-OF(AuditHdr.AuditDB) THEN
                    cValues = cValues + "," + AuditHdr.AuditDB.
                END. /* each audithdr */
                ASSIGN
                    svDB:LIST-ITEMS   = cValues
                    svDB:INNER-LINES  = svDB:NUM-ITEMS
                    svDB:SCREEN-VALUE = svDB:ENTRY(1)
                    .
            END. /* when all */
            WHEN "TABLE" THEN DO:
                cValues = "ALL".
                /* table */
                FOR EACH AuditHdr NO-LOCK
                    WHERE AuditHdr.AuditDB EQ svDB
                       OR svDB EQ "ALL"
                    BREAK BY AuditHdr.AuditTable
                    :
                    IF FIRST-OF(AuditHdr.AuditTable) THEN
                    cValues = cValues + "," + AuditHdr.AuditTable.
                END. /* each audithdr */
                ASSIGN
                    svTable:LIST-ITEMS   = cValues
                    svTable:INNER-LINES  = svTable:NUM-ITEMS
                    svTable:SCREEN-VALUE = svTable:ENTRY(1)
                    .
            END. /* when table */
            WHEN "FIELD" THEN DO:
                /* field */
                cValues = "ALL".
                FOR EACH AuditHdr NO-LOCK
                    WHERE (AuditHdr.AuditDB EQ svDB
                       OR svDB EQ "ALL")
                      AND (AuditHdr.AuditTable EQ svTable
                       OR svTable EQ "ALL"),
                    EACH AuditDtl OF AuditHdr NO-LOCK
                    BREAK BY AuditDtl.AuditField
                    :
                    IF FIRST-OF(AuditDtl.AuditField) THEN
                    cValues = cValues + "," + AuditDtl.AuditField.
                END. /* each audithdr */
                ASSIGN
                    svField:LIST-ITEMS   = cValues
                    svField:INNER-LINES  = svField:NUM-ITEMS
                    svField:SCREEN-VALUE = svField:ENTRY(1)
                    .
            END. /* when field */
        END CASE.
    END. /* do with frame */

END PROCEDURE.
