/* subjectID213Defs.i - auto generated 09.29.2022 @ 12:08:44 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllMachines AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE dtStartDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShifts AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDoNotShowNoCrewMessage AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        lAllMachines = DYNAMIC-FUNCTION("fGetDynParamValue","allMachines") EQ "YES"
        cStartMachine = DYNAMIC-FUNCTION("fGetDynParamValue","startMachine")
        cStartMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMachDescription")
        cEndMachine = DYNAMIC-FUNCTION("fGetDynParamValue","endMachine")
        cEndMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMachDescription")
        lAllJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","allJobNo") EQ "YES"
        cStartJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","startJobNo")
        iStartJobNo2 = DYNAMIC-FUNCTION("fGetDynParamValue","startJobNo2")
        cEndJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","endJobNo")
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetDynParamValue","endJobNo2")
        dtStartDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","StartDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartDate)
        dtEndDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","EndDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndDate)
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        lDoNotShowNoCrewMessage = DYNAMIC-FUNCTION("fGetDynParamValue","DoNotShowNoCrewMessage") EQ "YES"
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        .
END PROCEDURE.
