/* subjectID67Defs.i - auto generated 07.28.2020 @  3:14:21 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllBOL AS Logical NO-UNDO.
DEFINE VARIABLE iStartBOL AS Integer NO-UNDO.
DEFINE VARIABLE iEndBOL AS Integer NO-UNDO.
DEFINE VARIABLE lPost AS Logical NO-UNDO.
DEFINE VARIABLE lUnpost AS Logical NO-UNDO.
DEFINE VARIABLE cSummary-Details AS Character NO-UNDO.
DEFINE VARIABLE dtStartBOLDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndBOLDate AS Date NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS Logical NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllCustPoNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustPoNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustPoNo AS Character NO-UNDO.
DEFINE VARIABLE lAllFGItems AS Logical NO-UNDO.
DEFINE VARIABLE cStartFGItem AS Character NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItem AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS Character NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllBOL = DYNAMIC-FUNCTION("fGetDynParamValue","allBOL") EQ "YES"
        iStartBOL = DYNAMIC-FUNCTION("fGetDynParamValue","startBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetDynParamValue","endBOL")
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        lUnpost = DYNAMIC-FUNCTION("fGetDynParamValue","unpost") EQ "YES"
        cSummary-Details = DYNAMIC-FUNCTION("fGetDynParamValue","Summary-Details")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startBOLDate"))
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endBOLDate"))
        lAllOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllOrderNo") EQ "YES"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","startOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","endOrderNo")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustPoNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustPoNo") EQ "YES"
        cStartCustPoNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustPoNo")
        cEndCustPoNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustPoNo")
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        .
END PROCEDURE.
