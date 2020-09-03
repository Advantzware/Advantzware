/* spDynInitializeProc.p - rstark - 2.27.2019 */

/* add dynamic initialize procedures in alphabetical order */
/* always use a RETURN value of datatype character         */

DEFINE VARIABLE cSessionValue AS CHARACTER NO-UNDO.

/* **********************  Internal Functions  ************************ */

FUNCTION sfGetUserControlFieldValue RETURNS CHARACTER PRIVATE
    (ipcField AS CHARACTER):
    DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO.

    FIND FIRST userControl NO-LOCK.
    CASE ipcField:
        WHEN "AdminEmailAddr" THEN
        cFieldValue = userControl.adminEmailAddr.
        WHEN "MaxAllowedUsers" THEN
        cFieldValue = STRING(userControl.maxAllowedUsers).
        WHEN "MaxSessionPerUser" THEN
        cFieldValue = STRING(userControl.maxSessionsPerUser).
        WHEN "NumLicensedUsers" THEN
        cFieldValue = STRING(userControl.numLicensedUsers).
        WHEN "NumUsersOverLimit" THEN
        cFieldValue = STRING(userControl.numUsersOverLimit).
    END CASE.
    RETURN cFieldValue.
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE dynInitAdminEmailAddr:
    RETURN sfGetUserControlFieldValue ("AdminEmailAddr").
END PROCEDURE.

PROCEDURE dynInitAuditDB:
    DEFINE VARIABLE cDBs AS CHARACTER NO-UNDO.
    
    cDBs = "All,ASI".
    RETURN cDBs.
END PROCEDURE.

PROCEDURE dynInitAuditType:
    RETURN "{AOA/includes/auditTypes.i}".
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

PROCEDURE dynInitDBTableList:
    DEFINE VARIABLE cTableLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName  AS CHARACTER NO-UNDO.

    FOR EACH ASI._file NO-LOCK
        WHERE ASI._file._Tbl-Type EQ "T"
        :
        ASSIGN
            cTableLabel = IF ASI._file._file-label NE ? THEN ASI._file._file-label ELSE ""
            cTableName  = cTableName
                        + cTableLabel + " (" + ASI._file._file-name + ")" + ","
                        + ASI._file._file-name + ","
            .
    END. /* each _file */
    cTableName = TRIM(cTableName,",").
    RETURN cTableName.
END PROCEDURE.

PROCEDURE dynInitEstTypeCorr:
    cSessionValue = "5 - Single,5,6 - Set,6,7 - Combo/Tandem,7,8 - Combo/Tandem,8,9 - Other,9".
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitEstTypeFolding:
    cSessionValue = "1 - Single,1,2 - Set,2,3 - Combo/Tandem,3,4 - Combo/Tandem,4".
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitEstTypeID:
    cSessionValue = ",,MISC,MISC".
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitItemType:
    RUN spGetSessionParam ("ItemType", OUTPUT cSessionValue).
    IF cSessionValue EQ "" THEN
    cSessionValue = "All,FG,RM,WP".
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitLocation:
    RUN spGetSessionParam ("Location", OUTPUT cSessionValue).
    RETURN cSessionValue.
END PROCEDURE.

PROCEDURE dynInitMaxAllowedUsers:
    RETURN sfGetUserControlFieldValue ("MaxAllowedUsers").
END PROCEDURE.

PROCEDURE dynInitMaxSessionPerUser:
    RETURN sfGetUserControlFieldValue ("MaxSessionPerUser").
END PROCEDURE.

PROCEDURE dynInitNO:
    RETURN "NO".
END PROCEDURE.

PROCEDURE dynInitNumLicensedUsers:
    RETURN sfGetUserControlFieldValue ("NumLicensedUsers").
END PROCEDURE.

PROCEDURE dynInitNumUsersOverLimit:
    RETURN sfGetUserControlFieldValue ("NumUsersOverLimit").
END PROCEDURE.

PROCEDURE dynInitSBID:
    DEFINE VARIABLE cSearchDir    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO FORMAT "X(60)".
    DEFINE VARIABLE cAttrList     AS CHARACTER NO-UNDO FORMAT "X(4)".
    DEFINE VARIABLE cListItems    AS CHARACTER NO-UNDO.

    cSearchDir = ".\schedule\data\ASI".
    INPUT FROM OS-DIR(cSearchDir) NO-ECHO.
    REPEAT:
        SET cFileName ^ cAttrList.
        IF cAttrList NE "d" THEN NEXT.
        IF cFileName EQ "." OR cFileName EQ ".." THEN NEXT.
        cListItems = cListItems + "ASI/" + cFileName + ",".
    END. /* repeat */
    INPUT CLOSE.
    RETURN TRIM(cListItems,",").
END PROCEDURE.

PROCEDURE dynInitSecure:
    RUN spSetSessionParam ("Secure", "NO").
    RETURN "NO".
END PROCEDURE.

PROCEDURE dynInitUser:
    RETURN USERID("ASI").
END PROCEDURE.

PROCEDURE dynInitYES:
    RETURN "YES".
END PROCEDURE.

PROCEDURE dynNK1APSecureAllUsers:
    DEFINE VARIABLE cAPSecure AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisable  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAPSecure AS LOGICAL   NO-UNDO.

    RUN pNK1APSecure (OUTPUT cAPSecure, OUTPUT lAPSecure).
    IF cAPSecure EQ "YES" THEN
    cDisable =  ":DISABLE".
    RETURN STRING(cAPSecure EQ "NO") + cDisable.
END PROCEDURE.

PROCEDURE dynNK1APSecureUserID:
    DEFINE VARIABLE cAPSecure AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAPSecure AS LOGICAL   NO-UNDO.

    RUN pNK1APSecure (OUTPUT cAPSecure, OUTPUT lAPSecure).
    IF lAPSecure AND cAPSecure EQ "YES" THEN
    RETURN USERID("ASI") + ":DISABLE".
    ELSE
    RETURN "".
END PROCEDURE.

PROCEDURE pNK1APSecure PRIVATE:
    DEFINE OUTPUT PARAMETER opcAPSecure AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAPSecure AS LOGICAL   NO-UNDO.

    RUN spGetSessionParam ("Company", OUTPUT cSessionValue).
    RUN sys/ref/nk1look.p (
        cSessionValue,"APSecure","L",NO,NO,"","",
        OUTPUT opcAPSecure, OUTPUT oplAPSecure
        ).
END PROCEDURE.
