/* pCRMContacts.i - auto generated 11.03.2016 @ 12:46:55 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAutoAdd AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAutoUpdate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "crmCont.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAutoAdd = DYNAMIC-FUNCTION("fGetParamValue","svAutoAdd") EQ "yes"
        lAutoUpdate = DYNAMIC-FUNCTION("fGetParamValue","svAutoUpdate") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttCRMContacts:HANDLE, cAvailableColumns, cSelectedColumns).

