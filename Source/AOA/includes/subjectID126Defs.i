/* subjectID126Defs.i - auto generated 07.06.2021 @  7:17:47 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
DEFINE VARIABLE dtStartBOLDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndBOLDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterID AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllOrderNo") EQ "YES"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","startOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","endOrderNo")
        lAllBOL = DYNAMIC-FUNCTION("fGetDynParamValue","allBOL") EQ "YES"
        iStartBOL = DYNAMIC-FUNCTION("fGetDynParamValue","startBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetDynParamValue","endBOL")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startBOLDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartBOLDate)
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endBOLDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndBOLDate)
        cPrinterID = DYNAMIC-FUNCTION("fGetDynParamValue","PrinterID")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        .
END PROCEDURE.
