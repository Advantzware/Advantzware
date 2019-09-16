/* spDynInitializeProc.p - rstark - 2.27.2019 */

/* add dynamic initialize procedures in alphabetical order */
/* always use a RETURN value of datatype character         */

DEFINE VARIABLE cSessionValue AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD FieldName  AS CHARACTER
    FIELD AuditField AS CHARACTER 
        INDEX FieldName IS PRIMARY UNIQUE FieldName
        INDEX AuditField AuditField
        .
/* **********************  Internal Functions  ************************ */

/* **********************  Internal Procedures  *********************** */

PROCEDURE dynInitAuditDB:
    DEFINE VARIABLE cDBs AS CHARACTER NO-UNDO.
    
    cDBs = "All,ASI".
    RETURN cDBs.
END PROCEDURE.

PROCEDURE dynInitAuditField:
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables AS CHARACTER NO-UNDO.
    
    RUN pAuditTable (YES, OUTPUT cTables).
    cFields = "All,All".
    FOR EACH ttField
        BY ttField.AuditField
        :
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
    RETURN "All,CREATE,DELETE,UPDATE,LOG,TASK,TRACK,RESTORE".
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

PROCEDURE dynInitCompany:
    RUN spGetSessionParam ("Company", OUTPUT cSessionValue).
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitCompanyList:
    DEFINE VARIABLE cCompanyList AS CHARACTER NO-UNDO.
    
    FOR EACH company NO-LOCK:
        cCompanyList = cCompanyList + company.company + ",".
    END. /* each company */
    cCompanyList = TRIM(cCompanyList).
    
    RETURN cCompanyList.
END PROCEDURE.

PROCEDURE dynInitLocation:
    RUN spGetSessionParam ("Location", OUTPUT cSessionValue).
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitSecure:
    RUN spSetSessionParam ("Secure", "NO").
    RETURN "NO".
END PROCEDURE.

PROCEDURE pAuditTable:
    DEFINE INPUT  PARAMETER iplFields AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTables AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableLabel AS CHARACTER NO-UNDO.

    opcTables = "All,All".
    FOR EACH ASI._file NO-LOCK
        WHERE ASI._file._Tbl-type EQ "T"
        :
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditDB    EQ "ASI"
                      AND AuditHdr.AuditTable EQ ASI._file._file-name) THEN DO: 
            ASSIGN
                cTableLabel = IF ASI._file._file-label NE ? THEN ASI._file._file-label ELSE ""
                opcTables   = opcTables + ","
                            + cTableLabel + " ("
                            + ASI._file._file-name  + "),"
                            + ASI._file._file-name
                            .
            IF iplFields THEN
            FOR EACH ASI._field OF ASI._file NO-LOCK
                BY ASI._field._Label
                BY ASI._field._field-name
                :
                IF CAN-FIND(FIRST ttField
                            WHERE ttField.FieldName EQ ASI._field._field-name) THEN
                NEXT.
                CREATE ttField.
                ASSIGN
                    cFieldLabel        = IF ASI._field._Label NE ? THEN ASI._field._Label ELSE ""
                    ttField.FieldName  = ASI._field._field-name
                    ttField.AuditField = cFieldLabel + " ("
                                       + ASI._field._field-name + "),"
                                       + ASI._field._field-name
                                       .
            END. /* each _field */
        END. /* if can-find */
    END. /* each _file */
    IF iplFields EQ NO THEN
    FOR EACH prgrms NO-LOCK
        BY prgrms.mnemonic
        BY prgrms.prgTitle
        :
        IF CAN-FIND(FIRST AuditHdr
                    WHERE AuditHdr.AuditDB    EQ "ASI"
                      AND AuditHdr.AuditTable EQ prgrms.prgmname) THEN DO:
            opcTables = opcTables + "," + "["
                      + prgrms.mnemonic + "] "
                      + prgrms.prgTitle
                      + " (" + prgrms.prgmname + "),"
                      + prgrms.prgmname
                      .
        END. /* if can-find */
    END. /* each prgrms */
END PROCEDURE.
