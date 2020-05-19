/* subjectID8Defs.i - auto generated 04.11.2019 @ 12:35:23 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cStartMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllMachines AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShifts AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseTimes AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startTransDate"))
        dtStartTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartTransDate)
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        dtEndTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndTransDate)
        cStartMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMachDescription")
        cEndMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMachDescription")
        lAllMachines = DYNAMIC-FUNCTION("fGetDynParamValue","allMachines") EQ "YES"
        cStartMachine = DYNAMIC-FUNCTION("fGetDynParamValue","startMachine")
        cEndMachine = DYNAMIC-FUNCTION("fGetDynParamValue","endMachine")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cStartTime = DYNAMIC-FUNCTION("fGetDynParamValue","startTime")
        cEndTime = DYNAMIC-FUNCTION("fGetDynParamValue","endTime")
        lUseTimes = DYNAMIC-FUNCTION("fGetDynParamValue","useTimes") EQ "YES"
        .
END PROCEDURE.
