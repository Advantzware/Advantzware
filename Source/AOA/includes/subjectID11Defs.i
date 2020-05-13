/* subjectID11Defs.i - auto generated 06.17.2019 @  7:00:54 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cAfterValueFilter AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBeforeValueFilter AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPurge AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTypes AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUsers AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbs AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTables AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.

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
        cAfterValueFilter = DYNAMIC-FUNCTION("fGetDynParamValue","afterValueFilter")
        cBeforeValueFilter = DYNAMIC-FUNCTION("fGetDynParamValue","beforeValueFilter")
        lPurge = DYNAMIC-FUNCTION("fGetDynParamValue","purge") EQ "YES"
        cTypes = DYNAMIC-FUNCTION("fGetDynParamValue","types")
        cUsers = DYNAMIC-FUNCTION("fGetDynParamValue","users")
        cDbs = DYNAMIC-FUNCTION("fGetDynParamValue","dbs")
        cTables = DYNAMIC-FUNCTION("fGetDynParamValue","tables")
        cFields = DYNAMIC-FUNCTION("fGetDynParamValue","fields")
        .
END PROCEDURE.
