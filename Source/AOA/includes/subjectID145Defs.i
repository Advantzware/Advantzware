/* subjectID145Defs.i - auto generated 09.20.2020 @ 11:48:09 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE cLocation AS Character NO-UNDO.
DEFINE VARIABLE lAllCodeNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartDeptNo AS Character NO-UNDO.
DEFINE VARIABLE cCStartDeptName AS Character NO-UNDO.
DEFINE VARIABLE cEndDeptNo AS Character NO-UNDO.
DEFINE VARIABLE cCEndDeptName AS Character NO-UNDO.
DEFINE VARIABLE lAllMachines AS Logical NO-UNDO.
DEFINE VARIABLE cStartMachine AS Character NO-UNDO.
DEFINE VARIABLE cStartMachDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndMachine AS Character NO-UNDO.
DEFINE VARIABLE cEndMachDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllShifts AS Logical NO-UNDO.
DEFINE VARIABLE cStartShift AS Character NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndShift AS Character NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS Character NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS Date NO-UNDO.
DEFINE VARIABLE cStartTime AS Character NO-UNDO.
DEFINE VARIABLE cEndTime AS Character NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        lAllCodeNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCodeNo") EQ "YES"
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
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        cStartTime = DYNAMIC-FUNCTION("fGetDynParamValue","StartTime")
        cEndTime = DYNAMIC-FUNCTION("fGetDynParamValue","EndTime")
        .
END PROCEDURE.
