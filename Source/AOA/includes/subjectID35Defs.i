/* subjectID35Defs.i - auto generated 07.06.2021 @  7:14:07 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllCompany AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartItemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndItemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPurge AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lAllCompany = DYNAMIC-FUNCTION("fGetDynParamValue","allCompany") EQ "YES"
        cStartCompany = DYNAMIC-FUNCTION("fGetDynParamValue","startCompany")
        cStartDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startDescription")
        cEndCompany = DYNAMIC-FUNCTION("fGetDynParamValue","endCompany")
        cEndDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endDescription")
        lAllItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllItemNo") EQ "YES"
        cStartItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","startItemNo")
        cStartItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startItemDescription")
        cEndItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","endItemNo")
        cEndItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endItemDescription")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndInvoiceDate)
        lPurge = DYNAMIC-FUNCTION("fGetDynParamValue","Purge") EQ "YES"
        .
END PROCEDURE.
