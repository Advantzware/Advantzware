/* subjectID102Defs.i - auto generated 04.23.2020 @  9:52:27 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lUseImportFile AS Logical NO-UNDO.
DEFINE VARIABLE cImportType AS Character NO-UNDO.
DEFINE VARIABLE cImportFile AS Character NO-UNDO.
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS Date NO-UNDO.
DEFINE VARIABLE lOrderHeaderDueDate AS Logical NO-UNDO.
DEFINE VARIABLE lOrderLineDueDate AS Logical NO-UNDO.
DEFINE VARIABLE lActualReleaseDate AS Logical NO-UNDO.
DEFINE VARIABLE lScheduledReleaseDate AS Logical NO-UNDO.
DEFINE VARIABLE lPost AS Logical NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS Logical NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE lAllReleases AS Logical NO-UNDO.
DEFINE VARIABLE iStartRelease AS Integer NO-UNDO.
DEFINE VARIABLE iEndRelease AS Integer NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lUseImportFile = DYNAMIC-FUNCTION("fGetDynParamValue","UseImportFile") EQ "YES"
        cImportType = DYNAMIC-FUNCTION("fGetDynParamValue","ImportType")
        cImportFile = DYNAMIC-FUNCTION("fGetDynParamValue","ImportFile")
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
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
