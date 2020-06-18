/* subjectID126Defs.i - auto generated 06.15.2020 @  6:59:49 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS Logical NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE dtStartBOLDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndBOLDate AS Date NO-UNDO.
DEFINE VARIABLE lAllBOL AS Logical NO-UNDO.
DEFINE VARIABLE iStartBOL AS Integer NO-UNDO.
DEFINE VARIABLE iEndBOL AS Integer NO-UNDO.
DEFINE VARIABLE cPrinterID AS Character NO-UNDO.
DEFINE VARIABLE lCustList AS Logical NO-UNDO.

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
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startBOLDate"))
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endBOLDate"))
        lAllBOL = DYNAMIC-FUNCTION("fGetDynParamValue","allBOL") EQ "YES"
        iStartBOL = DYNAMIC-FUNCTION("fGetDynParamValue","startBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetDynParamValue","endBOL")
        cPrinterID = DYNAMIC-FUNCTION("fGetDynParamValue","PrinterID")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        .
END PROCEDURE.
