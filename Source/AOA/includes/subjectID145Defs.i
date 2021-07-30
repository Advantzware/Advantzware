/* subjectID145Defs.i - auto generated 07.06.2021 @  7:19:03 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllDeptNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartDeptNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCStartDeptName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDeptNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCEndDeptName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllMachines AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShifts AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCodeNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCodeNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCodeName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCodeNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCodeName AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        lAllDeptNo = DYNAMIC-FUNCTION("fGetDynParamValue","allDeptNo") EQ "YES"
        cStartDeptNo = DYNAMIC-FUNCTION("fGetDynParamValue","StartDeptNo")
        cCStartDeptName = DYNAMIC-FUNCTION("fGetDynParamValue","cStartDeptName")
        cEndDeptNo = DYNAMIC-FUNCTION("fGetDynParamValue","EndDeptNo")
        cCEndDeptName = DYNAMIC-FUNCTION("fGetDynParamValue","cEndDeptName")
        lAllMachines = DYNAMIC-FUNCTION("fGetDynParamValue","allMachines") EQ "YES"
        cStartMachine = DYNAMIC-FUNCTION("fGetDynParamValue","startMachine")
        cStartMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMachDescription")
        cEndMachine = DYNAMIC-FUNCTION("fGetDynParamValue","endMachine")
        cEndMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMachDescription")
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        dtStartTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startTransDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartTransDate)
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndTransDate)
        cStartTime = DYNAMIC-FUNCTION("fGetDynParamValue","StartTime")
        cEndTime = DYNAMIC-FUNCTION("fGetDynParamValue","EndTime")
        lAllCodeNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCodeNo") EQ "YES"
        cStartCodeNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCodeNo")
        cStartCodeName = DYNAMIC-FUNCTION("fGetDynParamValue","startCodeName")
        cEndCodeNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCodeNo")
        cEndCodeName = DYNAMIC-FUNCTION("fGetDynParamValue","endCodeName")
        .
END PROCEDURE.
