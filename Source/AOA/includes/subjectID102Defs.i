/* subjectID102Defs.i - auto generated 07.06.2021 @  7:16:30 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lUseImportFile AS LOGICAL NO-UNDO.
DEFINE VARIABLE cImportType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImportFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOrderHeaderDueDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOrderLineDueDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lActualReleaseDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lScheduledReleaseDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllReleases AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartRelease AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndRelease AS INTEGER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lUseImportFile = DYNAMIC-FUNCTION("fGetDynParamValue","UseImportFile") EQ "YES"
        cImportType = DYNAMIC-FUNCTION("fGetDynParamValue","ImportType")
        cImportFile = DYNAMIC-FUNCTION("fGetDynParamValue","ImportFile")
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        lOrderHeaderDueDate = DYNAMIC-FUNCTION("fGetDynParamValue","OrderHeaderDueDate") EQ "YES"
        lOrderLineDueDate = DYNAMIC-FUNCTION("fGetDynParamValue","OrderLineDueDate") EQ "YES"
        lActualReleaseDate = DYNAMIC-FUNCTION("fGetDynParamValue","ActualReleaseDate") EQ "YES"
        lScheduledReleaseDate = DYNAMIC-FUNCTION("fGetDynParamValue","ScheduledReleaseDate") EQ "YES"
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllOrderNo") EQ "YES"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","startOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","endOrderNo")
        lAllReleases = DYNAMIC-FUNCTION("fGetDynParamValue","allReleases") EQ "YES"
        iStartRelease = DYNAMIC-FUNCTION("fGetDynParamValue","startRelease")
        iEndRelease = DYNAMIC-FUNCTION("fGetDynParamValue","endRelease")
        .
END PROCEDURE.
