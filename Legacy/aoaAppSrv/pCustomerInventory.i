/* pCustomerInventory.i - auto generated 05.24.2016 @  5:53:19 pm from aoa/aoaParam.w */

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
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
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
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

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(255)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "IL12").
