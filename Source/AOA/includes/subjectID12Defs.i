/* subjectID12Defs.i - auto generated 06.17.2019 @ 10:32:53 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeTermsDiscount AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludePrepCharges AS LOGICAL NO-UNDO.
DEFINE VARIABLE iDayOld AS INTEGER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartReceiptDate)
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndReceiptDate)
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        lIncludeTermsDiscount = DYNAMIC-FUNCTION("fGetDynParamValue","includeTermsDiscount") EQ "YES"
        lIncludePrepCharges = DYNAMIC-FUNCTION("fGetDynParamValue","includePrepCharges") EQ "YES"
        iDayOld = DYNAMIC-FUNCTION("fGetDynParamValue","dayOld")
        .
END PROCEDURE.
