/* pMachineOrdersbyDueDate.i - auto generated 01.31.2019 @ 11:42:49 pm from AOA/aoaParam.w */

    {AOA/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-mchord.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAllOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svAllOrderNo") EQ "yes"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderNo")
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDueDate"))
        cStartDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDueDateOption")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDueDateOption,dtStartDueDate)
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDueDate"))
        cEndDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDueDateOption")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDueDateOption,dtEndDueDate)
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttMachineOrdersbyDueDate:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllOrderNo THEN
    ASSIGN
        iStartOrderNo = 0
        iEndOrderNo   = 99999999
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

