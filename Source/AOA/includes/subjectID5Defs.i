/* subjectID5Defs.i - auto generated 07.06.2021 @  7:12:17 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllRMItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndRMItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocs AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllMats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartMat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartMatDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMatDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lZeroBalance AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        lAllRMItems = DYNAMIC-FUNCTION("fGetDynParamValue","allRMItems") EQ "YES"
        cStartRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItem")
        cStartRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startRMItemDescriptn")
        cEndRMItem = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItem")
        cEndRMItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endRMItemDescriptn")
        lAllLocs = DYNAMIC-FUNCTION("fGetDynParamValue","allLocs") EQ "YES"
        cStartLoc = DYNAMIC-FUNCTION("fGetDynParamValue","startLoc")
        cStartLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startLocDescription")
        cEndLoc = DYNAMIC-FUNCTION("fGetDynParamValue","endLoc")
        cEndLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endLocDescription")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lAllMats = DYNAMIC-FUNCTION("fGetDynParamValue","allMats") EQ "YES"
        cStartMat = DYNAMIC-FUNCTION("fGetDynParamValue","startMat")
        cStartMatDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMatDescription")
        cEndMat = DYNAMIC-FUNCTION("fGetDynParamValue","endMat")
        cEndMatDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMatDescription")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartReceiptDate)
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndReceiptDate)
        cItemCode = DYNAMIC-FUNCTION("fGetDynParamValue","itemCode")
        lZeroBalance = DYNAMIC-FUNCTION("fGetDynParamValue","zeroBalance") EQ "YES"
        .
END PROCEDURE.
