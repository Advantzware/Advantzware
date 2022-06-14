/* subjectID200Defs.i - auto generated 02.14.2022 @  8:17:36 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllDeptNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartDeptNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCStartDeptName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDeptNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCEndDeptName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShifts AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseTimes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllMachines AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndMachDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintByScheduledMachine AS LOGICAL NO-UNDO.
DEFINE VARIABLE lRoundDecimals AS LOGICAL NO-UNDO.

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
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        lUseTimes = DYNAMIC-FUNCTION("fGetDynParamValue","useTimes") EQ "YES"
        cStartTime = DYNAMIC-FUNCTION("fGetDynParamValue","startTime")
        cEndTime = DYNAMIC-FUNCTION("fGetDynParamValue","endTime")
        dtStartDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","StartDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartDate)
        dtEndDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","EndDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndDate)
        lAllMachines = DYNAMIC-FUNCTION("fGetDynParamValue","allMachines") EQ "YES"
        cStartMachine = DYNAMIC-FUNCTION("fGetDynParamValue","startMachine")
        cStartMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startMachDescription")
        cEndMachine = DYNAMIC-FUNCTION("fGetDynParamValue","endMachine")
        cEndMachDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endMachDescription")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lPrintByScheduledMachine = DYNAMIC-FUNCTION("fGetDynParamValue","PrintByScheduledMachine") EQ "YES"
        lRoundDecimals = DYNAMIC-FUNCTION("fGetDynParamValue","RoundDecimals") EQ "YES"
        .
END PROCEDURE.
