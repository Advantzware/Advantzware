
/*------------------------------------------------------------------------
    File        : calcMatType.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon May 18 14:39:04 EDT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcMatType AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcMatType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcMatType AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
cCalcMatType = ipcMatType.
IF cCalcMatType BEGINS "!" AND LOOKUP(cCalcMatType, "!R,!F,!P,!M,!") GT 0 THEN
    cCalcMatType = SUBSTRING(CAPS(cCalcMatType), 2, 1). 

  
IF cCalcMatType EQ "F" THEN cCalcMatType = "D".
IF cCalcMatType EQ "M" THEN cCAlcMatType = "A".
opcMatType = cCalcMatType.