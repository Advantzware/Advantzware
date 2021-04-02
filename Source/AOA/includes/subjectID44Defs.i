/* subjectID44Defs.i - auto generated 02.22.2021 @ 12:29:39 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSbID AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cShift = DYNAMIC-FUNCTION("fGetDynParamValue","shift")
        dtStartTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startTransDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartTransDate)
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndTransDate)
        cSbID = DYNAMIC-FUNCTION("fGetDynParamValue","sbID")
        .
END PROCEDURE.
