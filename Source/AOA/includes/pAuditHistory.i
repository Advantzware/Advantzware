/* pAuditHistory.i - auto generated 11.02.2017 @  5:25:10 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDB AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBeforeValueFilter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAfterValueFilter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPurge AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "AuditHist.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cType = DYNAMIC-FUNCTION("fGetParamValue","svType")
        dtStartDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDate"))
        cStartDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDateOption")
        dtStartDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDateOption,dtStartDate)
        dtEndDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDate"))
        cEndDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDateOption")
        dtEndDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDateOption,dtEndDate)
        cUser = DYNAMIC-FUNCTION("fGetParamValue","svUser")
        cDB = DYNAMIC-FUNCTION("fGetParamValue","svDB")
        cTable = DYNAMIC-FUNCTION("fGetParamValue","svTable")
        cField = DYNAMIC-FUNCTION("fGetParamValue","svField")
        cBeforeValueFilter = DYNAMIC-FUNCTION("fGetParamValue","svBeforeValueFilter")
        cAfterValueFilter = DYNAMIC-FUNCTION("fGetParamValue","svAfterValueFilter")
        lPurge = DYNAMIC-FUNCTION("fGetParamValue","svPurge") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttAuditHistory:HANDLE, cAvailableColumns, cSelectedColumns).
