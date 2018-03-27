/* pMachineTransactions.i - auto generated 10.27.2017 @  8:35:45 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE dtStartMachTranDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartMachTranDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndMachTranDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndMachTranDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllMachine AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllShift AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE lUseTime AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartAMPM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndAMPM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubRpt_EmployeeTransactions AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-mchtrn.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        dtStartMachTranDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartMachTranDate"))
        cStartMachTranDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartMachTranDateOption")
        dtStartMachTranDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartMachTranDateOption,dtStartMachTranDate)
        dtEndMachTranDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndMachTranDate"))
        cEndMachTranDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndMachTranDateOption")
        dtEndMachTranDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndMachTranDateOption,dtEndMachTranDate)
        lAllMachine = DYNAMIC-FUNCTION("fGetParamValue","svAllMachine") EQ "yes"
        cStartMachine = DYNAMIC-FUNCTION("fGetParamValue","svStartMachine")
        cEndMachine = DYNAMIC-FUNCTION("fGetParamValue","svEndMachine")
        lAllShift = DYNAMIC-FUNCTION("fGetParamValue","svAllShift") EQ "yes"
        iStartShift = DYNAMIC-FUNCTION("fGetParamValue","svStartShift")
        iEndShift = DYNAMIC-FUNCTION("fGetParamValue","svEndShift")
        lUseTime = DYNAMIC-FUNCTION("fGetParamValue","svUseTime") EQ "yes"
        cStartTime = DYNAMIC-FUNCTION("fGetParamValue","svStartTime")
        cStartAMPM = DYNAMIC-FUNCTION("fGetParamValue","svStartAMPM")
        cEndTime = DYNAMIC-FUNCTION("fGetParamValue","svEndTime")
        cEndAMPM = DYNAMIC-FUNCTION("fGetParamValue","svEndAMPM")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lSubRpt_EmployeeTransactions = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_EmployeeTransactions") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttMachineTransactions:HANDLE, cAvailableColumns, cSelectedColumns).

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

