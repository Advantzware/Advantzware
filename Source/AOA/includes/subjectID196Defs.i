/* subjectID196Defs.i - auto generated 11.11.2021 @  1:12:17 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cImportType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lGenerateTemplate AS LOGICAL NO-UNDO.
DEFINE VARIABLE cGenerateTemplate AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeHelpRow AS LOGICAL NO-UNDO.
DEFINE VARIABLE cImportFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFirstRowHeader AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFieldValidation AS LOGICAL NO-UNDO.
DEFINE VARIABLE cOverwriteSkipDup AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWriteIgnore AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogFolder AS CHARACTER NO-UNDO.
DEFINE VARIABLE lGenerateLogOnly AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOnlyLogErrors AS LOGICAL NO-UNDO.
DEFINE VARIABLE lProcessImport AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cImportType = DYNAMIC-FUNCTION("fGetDynParamValue","ImportType")
        lGenerateTemplate = DYNAMIC-FUNCTION("fGetDynParamValue","GenerateTemplate") EQ "YES"
        cGenerateTemplate = DYNAMIC-FUNCTION("fGetDynParamValue","GenerateTemplate")
        lIncludeHelpRow = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeHelpRow") EQ "YES"
        cImportFile = DYNAMIC-FUNCTION("fGetDynParamValue","ImportFile")
        lFirstRowHeader = DYNAMIC-FUNCTION("fGetDynParamValue","FirstRowHeader") EQ "YES"
        lFieldValidation = DYNAMIC-FUNCTION("fGetDynParamValue","FieldValidation") EQ "YES"
        cOverwriteSkipDup = DYNAMIC-FUNCTION("fGetDynParamValue","OverwriteSkipDup")
        cWriteIgnore = DYNAMIC-FUNCTION("fGetDynParamValue","WriteIgnore")
        cLogFolder = DYNAMIC-FUNCTION("fGetDynParamValue","LogFolder")
        lGenerateLogOnly = DYNAMIC-FUNCTION("fGetDynParamValue","GenerateLogOnly") EQ "YES"
        lOnlyLogErrors = DYNAMIC-FUNCTION("fGetDynParamValue","OnlyLogErrors") EQ "YES"
        lProcessImport = DYNAMIC-FUNCTION("fGetDynParamValue","ProcessImport") EQ "YES"
        .
END PROCEDURE.
