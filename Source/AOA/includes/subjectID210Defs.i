/* subjectID210Defs.i - auto generated 06.30.2022 @  3:01:20 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllDMIID AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartDMIID AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndDMIID AS INTEGER NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShifts AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseTimes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPostedType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDMIStateType AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllDMIID = DYNAMIC-FUNCTION("fGetDynParamValue","AllDMIID") EQ "YES"
        iStartDMIID = DYNAMIC-FUNCTION("fGetDynParamValue","StartDMIID")
        iEndDMIID = DYNAMIC-FUNCTION("fGetDynParamValue","EndDMIID")
        dtStartTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startTransDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartTransDate)
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndTransDate)
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        lUseTimes = DYNAMIC-FUNCTION("fGetDynParamValue","useTimes") EQ "YES"
        cStartTime = DYNAMIC-FUNCTION("fGetDynParamValue","startTime")
        cEndTime = DYNAMIC-FUNCTION("fGetDynParamValue","endTime")
        cPostedType = DYNAMIC-FUNCTION("fGetDynParamValue","PostedType")
        cDMIStateType = DYNAMIC-FUNCTION("fGetDynParamValue","DMIStateType")
        .
END PROCEDURE.
