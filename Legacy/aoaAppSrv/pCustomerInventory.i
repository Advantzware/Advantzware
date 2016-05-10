/* pCustomerInventory.i - auto generated 05.09.2016 @  3:38:57 pm from aoa/aoaParam.w */

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustomers AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInventoryClasses AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIncludeInactiveCustomers AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeZeroQty AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-cusinv.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustomers = DYNAMIC-FUNCTION("fGetParamValue","svAllCustomers") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        cInventoryClasses = DYNAMIC-FUNCTION("fGetParamValue","svInventoryClasses")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lIncludeInactiveCustomers = DYNAMIC-FUNCTION("fGetParamValue","svIncludeInactiveCustomers") EQ "yes"
        lIncludeZeroQty = DYNAMIC-FUNCTION("fGetParamValue","svIncludeZeroQty") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttCustomerInventory:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllCustomers THEN
    ASSIGN
        cStartCustomers = CHR(32)
        cEndCustomers   = CHR(255)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "IL12").
