/* spDynInitializeProc.p - rstark - 2.27.2019 */

/* add dynamic initialize procedures in alphabetical order */
/* always use a RETURN value of datatype character         */

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD AuditField AS CHARACTER 
        INDEX AuditField IS PRIMARY UNIQUE AuditField
        .
/* **********************  Internal Functions  ************************ */

/* **********************  Internal Procedures  *********************** */

PROCEDURE dynInitCompany:
    RUN spGetCompany (OUTPUT cCompany).
    RETURN cCompany.
END PROCEDURE.

PROCEDURE dynInitAuditDB:
    DEFINE VARIABLE cDBs AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx  AS INTEGER   NO-UNDO.
    
    cDBs = "All".
    DO idx = 1 TO NUM-DBS:
        cDBs = cDBs + "," + LDBNAME(idx).
    END. /* do idx */
    RETURN cDBs.
END PROCEDURE.

PROCEDURE dynInitAuditField:
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables AS CHARACTER NO-UNDO.
    
    RUN pAuditTable (YES, OUTPUT cTables).
    cFields = "All".
    FOR EACH ttField:
        cFields = cFields + "," + ttField.AuditField.
    END. /* each ttfield */
    RETURN cFields.
END PROCEDURE.

PROCEDURE dynInitAuditTable:
    DEFINE VARIABLE cTables AS CHARACTER NO-UNDO.
    
    RUN pAuditTable (NO, OUTPUT cTables).
    RETURN cTables.
END PROCEDURE.

PROCEDURE dynInitAuditType:
    RETURN "All,CREATE,DELETE,UPDATE,LOG,ASK,TRACK,RESTORE".
END PROCEDURE.

PROCEDURE dynInitAuditUser:
    DEFINE VARIABLE cUsers AS CHARACTER NO-UNDO.
    
    cUsers = "All".
    FOR EACH users NO-LOCK
        BY users.user_id
        :
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditUser EQ users.user_id) THEN  
        cUsers = cUsers + "," + users.user_id.
    END. /* each users */
    RETURN cUsers.
END PROCEDURE.

PROCEDURE dynInitCompanyList:
    DEFINE VARIABLE cCompanyList AS CHARACTER NO-UNDO.
    
    FOR EACH company NO-LOCK:
        cCompanyList = cCompanyList + company.company + ",".
    END. /* each company */
    cCompanyList = TRIM(cCompanyList).
    
    RETURN cCompanyList.
END PROCEDURE.

PROCEDURE pAuditTable:
    DEFINE INPUT  PARAMETER iplFields AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTables AS CHARACTER NO-UNDO.
    
    opcTables = "All".
    FOR EACH ASI._file NO-LOCK
        :
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditDB    EQ "ASI"
                      AND AuditHdr.AuditTable EQ ASI._file._file-name) THEN DO: 
            opcTables = opcTables + "," + ASI._file._file-name.
            IF iplFields THEN
            FOR EACH ASI._field OF ASI._file NO-LOCK
                :
                IF CAN-FIND(FIRST ttField
                            WHERE ttField.AuditField EQ ASI._field._field-name) THEN
                NEXT.
                CREATE ttField.
                ttField.AuditField = ASI._field._field-name.
            END. /* each _field */
        END. /* if can-find */
    END. /* each _file */
    IF iplFields EQ NO THEN
    FOR EACH prgrms NO-LOCK:
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditDB    EQ "ASI"
                      AND AuditHdr.AuditTable EQ prgrms.prgmname) THEN DO:
            opcTables = opcTables + "," + prgrms.prgmname.
        END. /* if can-find */
    END. /* each prgrms */
END PROCEDURE.

PROCEDURE spSetCompany:
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    cCompany = ipcCompany.
END PROCEDURE.
