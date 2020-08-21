/* subjectID139Defs.i - auto generated 08.21.2020 @  5:05:52 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllMachines AS Logical NO-UNDO.
DEFINE VARIABLE cStartMachine AS Character NO-UNDO.
DEFINE VARIABLE cStartMachDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndMachine AS Character NO-UNDO.
DEFINE VARIABLE cEndMachDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllCodeNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartDeptNo AS Character NO-UNDO.
DEFINE VARIABLE cStartDeptName AS Character NO-UNDO.
DEFINE VARIABLE cEndDeptNo AS Character NO-UNDO.
DEFINE VARIABLE cEndDeptName AS Character NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS Date NO-UNDO.
DEFINE VARIABLE cTotalBy AS Character NO-UNDO.
DEFINE VARIABLE lFolding AS Logical NO-UNDO.
DEFINE VARIABLE lCorrugated AS Logical NO-UNDO.
DEFINE VARIABLE lJobDetail AS Logical NO-UNDO.
DEFINE VARIABLE cSortBy AS Character NO-UNDO.
DEFINE VARIABLE lAllShifts AS Logical NO-UNDO.
DEFINE VARIABLE cStartShift AS Character NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndShift AS Character NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS Character NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllMachines = DYNAMIC-FUNCTION("fGetDynParamValue","allMachines") EQ "YES"
        cStartMachine = DYNAMIC-FUNCTION("fGetDynParamValue","startMachine")
        cStartMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMachDescription")
        cEndMachine = DYNAMIC-FUNCTION("fGetDynParamValue","endMachine")
        cEndMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMachDescription")
        lAllCodeNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCodeNo") EQ "YES"
        cStartDeptNo = DYNAMIC-FUNCTION("fGetDynParamValue","StartDeptNo")
        cStartDeptName = DYNAMIC-FUNCTION("fGetDynParamValue","StartDeptName")
        cEndDeptNo = DYNAMIC-FUNCTION("fGetDynParamValue","EndDeptNo")
        cEndDeptName = DYNAMIC-FUNCTION("fGetDynParamValue","EndDeptName")
        dtStartTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startTransDate"))
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        cTotalBy = DYNAMIC-FUNCTION("fGetDynParamValue","TotalBy")
        lFolding = DYNAMIC-FUNCTION("fGetDynParamValue","Folding") EQ "YES"
        lCorrugated = DYNAMIC-FUNCTION("fGetDynParamValue","Corrugated") EQ "YES"
        lJobDetail = DYNAMIC-FUNCTION("fGetDynParamValue","JobDetail") EQ "YES"
        cSortBy = DYNAMIC-FUNCTION("fGetDynParamValue","SortBy")
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        .
END PROCEDURE.
