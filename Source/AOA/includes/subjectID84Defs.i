/* subjectID84Defs.i - auto generated 07.07.2020 @  6:42:32 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllFGItems AS Logical NO-UNDO.
DEFINE VARIABLE cStartFGItem AS Character NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItem AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cFGItemStatus AS Character NO-UNDO.
DEFINE VARIABLE lAllStyleNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartStyleNo AS Character NO-UNDO.
DEFINE VARIABLE cStartStyleName AS Character NO-UNDO.
DEFINE VARIABLE cEndStyleNo AS Character NO-UNDO.
DEFINE VARIABLE cEndStyleName AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllProCats AS Logical NO-UNDO.
DEFINE VARIABLE cStartProCat AS Character NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cEndProCat AS Character NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS Character NO-UNDO.
DEFINE VARIABLE lAllCustPart AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustPart AS Character NO-UNDO.
DEFINE VARIABLE cEndCustPart AS Character NO-UNDO.
DEFINE VARIABLE lAllEstimateNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartEstimateNo AS Character NO-UNDO.
DEFINE VARIABLE cEndEstimateNo AS Character NO-UNDO.
DEFINE VARIABLE lAllSpecNotes AS Logical NO-UNDO.
DEFINE VARIABLE cSpecCode AS Character NO-UNDO.

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
