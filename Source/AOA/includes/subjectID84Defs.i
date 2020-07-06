/* subjectID84Defs.i - */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllStyleNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartStyle AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndStyle AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCats AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCats AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustPart AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllEstimateNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSpecNote AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSpecCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemStatus AS CHARACTER NO-UNDO.


PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")        
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"

        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")        
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")

        lAllStyleNo = DYNAMIC-FUNCTION("fGetDynParamValue","allStyleNo") EQ "YES"
        cStartStyle = DYNAMIC-FUNCTION("fGetDynParamValue","startStyleNo")        
        cEndStyle = DYNAMIC-FUNCTION("fGetDynParamValue","endStyleNo")

        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCats = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")        
        cEndProCats = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")

	    lAllCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","allCustPart") EQ "YES"
        cStartCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","startCustPart")        
        cEndCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","endCustPart")

	    lAllEstimateNo = DYNAMIC-FUNCTION("fGetDynParamValue","allEstimateNo") EQ "YES"
        cStartEstimateNo = DYNAMIC-FUNCTION("fGetDynParamValue","startEstimateNo")        
        cEndEstimateNo = DYNAMIC-FUNCTION("fGetDynParamValue","endEstimateNo")

	    lAllSpecNote = DYNAMIC-FUNCTION("fGetDynParamValue","allSpecNotes") EQ "YES"
        cSpecCode = DYNAMIC-FUNCTION("fGetDynParamValue","specCode") 
	    cItemStatus = DYNAMIC-FUNCTION("fGetDynParamValue","fgItemStatus")        
        
       
        .
END PROCEDURE.
