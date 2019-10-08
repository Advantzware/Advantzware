/* pInvoiceHighlights.i - auto generated 11.03.2016 @ 12:48:41 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE iYear AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "dbinvmh.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        iYear = DYNAMIC-FUNCTION("fGetParamValue","svYear")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttInvoiceHighlights:HANDLE, cAvailableColumns, cSelectedColumns).
