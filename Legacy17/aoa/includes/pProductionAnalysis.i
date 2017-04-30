/* pProductionAnalysis.i - auto generated 11.03.2016 @ 12:48:10 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllDept AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndDept AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartOpDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartOpDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOpDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndOpDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllShift AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPrintByScheduledMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRoundDecimals AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-prodlys.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        lAllDept = DYNAMIC-FUNCTION("fGetParamValue","svAllDept") EQ "yes"
        cStartDept = DYNAMIC-FUNCTION("fGetParamValue","svStartDept")
        cEndDept = DYNAMIC-FUNCTION("fGetParamValue","svEndDept")
        lAllMachine = DYNAMIC-FUNCTION("fGetParamValue","svAllMachine") EQ "yes"
        cStartMachine = DYNAMIC-FUNCTION("fGetParamValue","svStartMachine")
        cEndMachine = DYNAMIC-FUNCTION("fGetParamValue","svEndMachine")
        dtStartOpDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOpDate"))
        cStartOpDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOpDateOption")
        dtStartOpDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartOpDateOption,dtStartOpDate)
        dtEndOpDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOpDate"))
        cEndOpDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndOpDateOption")
        dtEndOpDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndOpDateOption,dtEndOpDate)
        lAllShift = DYNAMIC-FUNCTION("fGetParamValue","svAllShift") EQ "yes"
        iStartShift = DYNAMIC-FUNCTION("fGetParamValue","svStartShift")
        iEndShift = DYNAMIC-FUNCTION("fGetParamValue","svEndShift")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lPrintByScheduledMachine = DYNAMIC-FUNCTION("fGetParamValue","svPrintByScheduledMachine") EQ "yes"
        lRoundDecimals = DYNAMIC-FUNCTION("fGetParamValue","svRoundDecimals") EQ "yes"
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttProductionAnalysis:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllDept THEN
    ASSIGN
        cStartDept = CHR(32)
        cEndDept   = CHR(254)
        .

    IF lAllMachine THEN
    ASSIGN
        cStartMachine = CHR(32)
        cEndMachine   = CHR(254)
        .

    IF lAllShift THEN
    ASSIGN
        iStartShift = 0
        iEndShift   = 99999999
        .

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "DE2", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
