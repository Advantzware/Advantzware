/* subjectID104Defs.i - auto generated 04.28.2020 @  9:19:42 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllCustTypes AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustType AS Character NO-UNDO.
DEFINE VARIABLE cStartCustDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndCustType AS Character NO-UNDO.
DEFINE VARIABLE cEndCustDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllGroups AS Logical NO-UNDO.
DEFINE VARIABLE cStartSalesGroup AS Character NO-UNDO.
DEFINE VARIABLE cStartSalesGroupDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndSalesGroup AS Character NO-UNDO.
DEFINE VARIABLE cEndSalesGroupDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS Logical NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS Character NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS Character NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS Character NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS Character NO-UNDO.
DEFINE VARIABLE lAllProCats AS Logical NO-UNDO.
DEFINE VARIABLE cStartProCat AS Character NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cEndProCat AS Character NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS Character NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS Date NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustTypes = DYNAMIC-FUNCTION("fGetDynParamValue","allCustTypes") EQ "YES"
        cStartCustType = DYNAMIC-FUNCTION("fGetDynParamValue","startCustType")
        cStartCustDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startCustDescription")
        cEndCustType = DYNAMIC-FUNCTION("fGetDynParamValue","endCustType")
        cEndCustDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endCustDescription")
        lAllGroups = DYNAMIC-FUNCTION("fGetDynParamValue","allGroups") EQ "YES"
        cStartSalesGroup = DYNAMIC-FUNCTION("fGetDynParamValue","StartSalesGroup")
        cStartSalesGroupDescription = DYNAMIC-FUNCTION("fGetDynParamValue","StartSalesGroupDescription")
        cEndSalesGroup = DYNAMIC-FUNCTION("fGetDynParamValue","EndSalesGroup")
        cEndSalesGroupDescription = DYNAMIC-FUNCTION("fGetDynParamValue","EndSalesGroupDescription")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        .
END PROCEDURE.
