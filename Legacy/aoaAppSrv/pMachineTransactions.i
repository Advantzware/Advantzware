/* pMachineTransactions.i - auto generated 05.13.2016 @  3:17:34 pm from aoa/aoaParam.w */

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
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubRpt_EmployeeTransactions AS LOGICAL NO-UNDO.
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
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lSubRpt_EmployeeTransactions = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_EmployeeTransactions") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttMachineTransactions:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllMachine THEN
    ASSIGN
        cStartMachine = CHR(32)
        cEndMachine   = CHR(255)
        .

