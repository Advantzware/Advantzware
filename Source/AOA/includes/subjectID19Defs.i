/* subjectID19Defs.i - auto generated 11.19.2020 @ 12:47:42 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE cLocation AS Character NO-UNDO.
DEFINE VARIABLE dtPostDate AS Date NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS Logical NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE dtStartBOLDate AS Date NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndBOLDate AS Date NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocs AS Logical NO-UNDO.
DEFINE VARIABLE cStartLoc AS Character NO-UNDO.
DEFINE VARIABLE cStartLocDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndLoc AS Character NO-UNDO.
DEFINE VARIABLE cEndLocDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllBOL AS Logical NO-UNDO.
DEFINE VARIABLE iStartBOL AS Integer NO-UNDO.
DEFINE VARIABLE iEndBOL AS Integer NO-UNDO.
DEFINE VARIABLE lAllLocBin AS Logical NO-UNDO.
DEFINE VARIABLE cStartLocBin AS Character NO-UNDO.
DEFINE VARIABLE cEndLocBin AS Character NO-UNDO.
DEFINE VARIABLE lPost AS Logical NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","postDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lCustList = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,lCustList)
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startBOLDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endBOLDate"))
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndBOLDate)
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        lAllLocs = DYNAMIC-FUNCTION("fGetDynParamValue","allLocs") EQ "YES"
        lAllLocs = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,lAllLocs)
        cStartLoc = DYNAMIC-FUNCTION("fGetDynParamValue","startLoc")
        cStartLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startLocDescription")
        cEndLoc = DYNAMIC-FUNCTION("fGetDynParamValue","endLoc")
        cEndLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endLocDescription")
        lAllBOL = DYNAMIC-FUNCTION("fGetDynParamValue","allBOL") EQ "YES"
        iStartBOL = DYNAMIC-FUNCTION("fGetDynParamValue","startBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetDynParamValue","endBOL")
        lAllLocBin = DYNAMIC-FUNCTION("fGetDynParamValue","allLocBin") EQ "YES"
        cStartLocBin = DYNAMIC-FUNCTION("fGetDynParamValue","startLocBin")
        cEndLocBin = DYNAMIC-FUNCTION("fGetDynParamValue","endLocBin")
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        .
END PROCEDURE.
