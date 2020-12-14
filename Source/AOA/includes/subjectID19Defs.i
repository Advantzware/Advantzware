/* subjectID19Defs.i - auto generated 11.30.2020 @  1:51:50 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtPostDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartBOLDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndBOLDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocs AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllLocBin AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLocBin AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocBin AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","postDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtPostDate)
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startBOLDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartBOLDate)
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endBOLDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndBOLDate)
        lAllLocs = DYNAMIC-FUNCTION("fGetDynParamValue","allLocs") EQ "YES"
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
