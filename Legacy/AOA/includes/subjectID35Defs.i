/* subjectID35Defs.i - auto generated 08.13.2019 @ 12:42:10 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cStartProCatDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS Character NO-UNDO.
DEFINE VARIABLE lAllProCats AS Logical NO-UNDO.
DEFINE VARIABLE cStartProCat AS Character NO-UNDO.
DEFINE VARIABLE cEndProCat AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS Date NO-UNDO.
DEFINE VARIABLE cStartItemDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndItemDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllItemNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartItemNo AS Character NO-UNDO.
DEFINE VARIABLE cEndItemNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCompany AS Character NO-UNDO.
DEFINE VARIABLE cEndCompany AS Character NO-UNDO.
DEFINE VARIABLE cStartDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllCompany AS Logical NO-UNDO.
DEFINE VARIABLE lPurge AS Logical NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cStartItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startItemDescription")
        cEndItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endItemDescription")
        lAllItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllItemNo") EQ "YES"
        cStartItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","startItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","endItemNo")
        cStartCompany = DYNAMIC-FUNCTION("fGetDynParamValue","startCompany")
        cEndCompany = DYNAMIC-FUNCTION("fGetDynParamValue","endCompany")
        cStartDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startDescription")
        cEndDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endDescription")
        lAllCompany = DYNAMIC-FUNCTION("fGetDynParamValue","allCompany") EQ "YES"
        lPurge = DYNAMIC-FUNCTION("fGetDynParamValue","Purge") EQ "YES"
        .
END PROCEDURE.
