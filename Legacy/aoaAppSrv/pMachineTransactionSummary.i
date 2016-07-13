/* pMachineTransactionSummary.i - auto generated 06.07.2016 @ 10:34:32 pm from aoa/aoaParam.w */

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

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
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "mtransum.", ipcUserID, ipiBatch).

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
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttMachineTransactionSummary:HANDLE,
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

