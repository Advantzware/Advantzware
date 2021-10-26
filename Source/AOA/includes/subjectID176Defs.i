/* subjectID176Defs.i - auto generated 07.16.2021 @  3:43:25 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllInvNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartInvNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndInvNo AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllInvoiceID AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartInvoiceID AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndInvoiceID AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
DEFINE VARIABLE dtStartBOLDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndBOLDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReprint AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintSetComponent AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintInstructions AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOnlyOpenInvoices AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","allInvNo") EQ "YES"
        iStartInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","startInvNo")
        iEndInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","endInvNo")
        lAllInvoiceID = DYNAMIC-FUNCTION("fGetDynParamValue","AllInvoiceID") EQ "YES"
        iStartInvoiceID = DYNAMIC-FUNCTION("fGetDynParamValue","StartInvoiceID")
        iEndInvoiceID = DYNAMIC-FUNCTION("fGetDynParamValue","EndInvoiceID")
        lAllBOL = DYNAMIC-FUNCTION("fGetDynParamValue","allBOL") EQ "YES"
        iStartBOL = DYNAMIC-FUNCTION("fGetDynParamValue","startBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetDynParamValue","endBOL")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startBOLDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartBOLDate)
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endBOLDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndBOLDate)
        lReprint = DYNAMIC-FUNCTION("fGetDynParamValue","Reprint") EQ "YES"
        lPrintSetComponent = DYNAMIC-FUNCTION("fGetDynParamValue","PrintSetComponent") EQ "YES"
        lPrintInstructions = DYNAMIC-FUNCTION("fGetDynParamValue","PrintInstructions") EQ "YES"
        lOnlyOpenInvoices = DYNAMIC-FUNCTION("fGetDynParamValue","OnlyOpenInvoices") EQ "YES"
        .
END PROCEDURE.
