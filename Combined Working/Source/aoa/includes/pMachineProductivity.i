/* pMachineProductivity.i - auto generated 10.27.2017 @  9:49:26 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
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
    DEFINE VARIABLE lUseTime AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartAMPM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndAMPM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPrintByScheduledMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRoundDecimals AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "machprod.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
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
        lUseTime = DYNAMIC-FUNCTION("fGetParamValue","svUseTime") EQ "yes"
        cStartTime = DYNAMIC-FUNCTION("fGetParamValue","svStartTime")
        cStartAMPM = DYNAMIC-FUNCTION("fGetParamValue","svStartAMPM")
        cEndTime = DYNAMIC-FUNCTION("fGetParamValue","svEndTime")
        cEndAMPM = DYNAMIC-FUNCTION("fGetParamValue","svEndAMPM")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lPrintByScheduledMachine = DYNAMIC-FUNCTION("fGetParamValue","svPrintByScheduledMachine") EQ "yes"
        lRoundDecimals = DYNAMIC-FUNCTION("fGetParamValue","svRoundDecimals") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttMachineProductivity:HANDLE, cAvailableColumns, cSelectedColumns).

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
    RUN pBuildCustList (ipcCompany, "DE3", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
