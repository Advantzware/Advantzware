/* pMachineProductivity.i - auto generated 07.07.2016 @ 11:39:09 am from aoa/aoaParam.w */

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

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
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPrintByScheduledMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lRoundDecimals AS LOGICAL NO-UNDO.
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
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lPrintByScheduledMachine = DYNAMIC-FUNCTION("fGetParamValue","svPrintByScheduledMachine") EQ "yes"
        lRoundDecimals = DYNAMIC-FUNCTION("fGetParamValue","svRoundDecimals") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttMachineProductivity:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllMachine THEN
    ASSIGN
        cStartMachine = CHR(32)
        cEndMachine   = CHR(255)
        .

    IF lAllShift THEN
    ASSIGN
        iStartShift = 0
        iEndShift   = 99999999
        .

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(255)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "DE3").
