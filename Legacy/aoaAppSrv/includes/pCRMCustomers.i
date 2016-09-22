/* pCRMCustomers.i - auto generated 09.21.2016 @  9:38:47 pm from aoa/aoaParam.w */

    {aoaAppSrv/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAutoAdd AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAutoUpdate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "crmCust.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAutoAdd = DYNAMIC-FUNCTION("fGetParamValue","svAutoAdd") EQ "yes"
        lAutoUpdate = DYNAMIC-FUNCTION("fGetParamValue","svAutoUpdate") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttCRMCustomers:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

