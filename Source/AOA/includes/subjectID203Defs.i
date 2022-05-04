/* subjectID203Defs.i - auto generated 05.04.2022 @  6:06:28 am */

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
DEFINE VARIABLE cJobStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartMachineRunDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndMachineRunDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.

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
        cJobStatus = DYNAMIC-FUNCTION("fGetDynParamValue","jobStatus")
        dtStartMachineRunDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startMachineRunDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartMachineRunDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartMachineRunDate)
        dtEndMachineRunDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endMachineRunDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndMachineRunDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndMachineRunDate)
        .
END PROCEDURE.
