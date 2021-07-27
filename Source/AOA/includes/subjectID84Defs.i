/* subjectID84Defs.i - auto generated 07.06.2021 @  7:15:49 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFGItemStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllStyleNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartStyleNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartStyleName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndStyleNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndStyleName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustPart AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllEstimateNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSpecNotes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSpecCode AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        cFGItemStatus = DYNAMIC-FUNCTION("fGetDynParamValue","FGItemStatus")
        lAllStyleNo = DYNAMIC-FUNCTION("fGetDynParamValue","allStyleNo") EQ "YES"
        cStartStyleNo = DYNAMIC-FUNCTION("fGetDynParamValue","startStyleNo")
        cStartStyleName = DYNAMIC-FUNCTION("fGetDynParamValue","startStyleName")
        cEndStyleNo = DYNAMIC-FUNCTION("fGetDynParamValue","endStyleNo")
        cEndStyleName = DYNAMIC-FUNCTION("fGetDynParamValue","endStyleName")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lAllCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","allCustPart") EQ "YES"
        cStartCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","startCustPart")
        cEndCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","endCustPart")
        lAllEstimateNo = DYNAMIC-FUNCTION("fGetDynParamValue","allEstimateNo") EQ "YES"
        cStartEstimateNo = DYNAMIC-FUNCTION("fGetDynParamValue","startEstimateNo")
        cEndEstimateNo = DYNAMIC-FUNCTION("fGetDynParamValue","endEstimateNo")
        lAllSpecNotes = DYNAMIC-FUNCTION("fGetDynParamValue","allSpecNotes") EQ "YES"
        cSpecCode = DYNAMIC-FUNCTION("fGetDynParamValue","specCode")
        .
END PROCEDURE.
