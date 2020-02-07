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
