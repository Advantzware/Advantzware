/* subjectID216Defs.i - auto generated 10.27.2022 @  7:00:26 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSbID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDepartments AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReload AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
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
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startDueDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtStartDueDate)
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endDueDate"))
        cDatePickList-4 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-4")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-4,dtEndDueDate)
        cSbID = DYNAMIC-FUNCTION("fGetDynParamValue","sbID")
        cDepartments = DYNAMIC-FUNCTION("fGetDynParamValue","Departments")
        lReload = DYNAMIC-FUNCTION("fGetDynParamValue","Reload") EQ "YES"
        .
END PROCEDURE.
