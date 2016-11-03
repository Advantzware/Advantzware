/* pMachineTransactions.i - auto generated 11.03.2016 @ 12:47:42 am from aoa/aoaParam.w */

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
        cEndMachine   = CHR(255)
        .

    IF lAllShift THEN
    ASSIGN
        iStartShift = 0
        iEndShift   = 99999999
        .

