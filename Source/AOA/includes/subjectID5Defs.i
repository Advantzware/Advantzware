/* subjectID5Defs.i - auto generated 06.10.2019 @ 12:31:14 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cStartRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllRMItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocs AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartMatDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMatDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllMats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartMat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cItemCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lZeroBalance AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        cStartRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItemDescriptn")
        cEndRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItemDescriptn")
        lAllRMItems = DYNAMIC-FUNCTION("fGetDynParamValue","allRMItems") EQ "YES"
        cStartRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItem")
        cEndRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItem")
        cStartLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startLocDescription")
        cEndLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endLocDescription")
        lAllLocs = DYNAMIC-FUNCTION("fGetDynParamValue","allLocs") EQ "YES"
        cStartLoc = DYNAMIC-FUNCTION("fGetDynParamValue","startLoc")
        cEndLoc = DYNAMIC-FUNCTION("fGetDynParamValue","endLoc")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cStartMatDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMatDescription")
        cEndMatDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMatDescription")
        lAllMats = DYNAMIC-FUNCTION("fGetDynParamValue","allMats") EQ "YES"
        cStartMat = DYNAMIC-FUNCTION("fGetDynParamValue","startMat")
        cEndMat = DYNAMIC-FUNCTION("fGetDynParamValue","endMat")
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartReceiptDate)
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndReceiptDate)
        cItemCode = DYNAMIC-FUNCTION("fGetDynParamValue","itemCode")
        lZeroBalance = DYNAMIC-FUNCTION("fGetDynParamValue","zeroBalance") EQ "YES"
        .
END PROCEDURE.
