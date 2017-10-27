/* pCostOutReport.i - auto generated 10.26.2017 @  4:09:25 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtStartDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOpened AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "costOut.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        lAllJobNo = DYNAMIC-FUNCTION("fGetParamValue","svAllJobNo") EQ "yes"
        cStartJobNo = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo")
        iStartJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo2")
        cEndJobNo = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo")
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo2")
        dtStartDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDate"))
        cStartDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDateOption")
        dtStartDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDateOption,dtStartDate)
        dtEndDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDate"))
        cEndDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDateOption")
        dtEndDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDateOption,dtEndDate)
        cOpened = DYNAMIC-FUNCTION("fGetParamValue","svOpened")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttCostOutReport:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllJobNo THEN
    ASSIGN
        cStartJobNo = CHR(32)
        cEndJobNo   = CHR(254)
        .

