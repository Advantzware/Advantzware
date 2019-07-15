
/*------------------------------------------------------------------------
    File        : Terr.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Kuldeep
    Created     :  March 18, 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTerr NO-UNDO 
    FIELD vCompany  LIKE terr.company FORMAT "X(3)"
    FIELD vTerr LIKE terr.terr FORMAT "X(3)"
    FIELD vDscr LIKE terr.dscr FORMAT "X(20)"
    
    .
        
DEFINE DATASET dsTerr FOR ttTerr.
DEFINE INPUT PARAMETER prmComp    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vTerr      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vDscr      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTerr.
DEFINE BUFFER buff-terr FOR terr.


IF prmComp = ?   THEN ASSIGN prmComp = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmUser = ?   THEN ASSIGN prmUser = "".
IF vTerr = ?     THEN ASSIGN vTerr = "".
IF vDscr  = ?    THEN ASSIGN vDscr  = "".

IF prmAction = ? THEN ASSIGN prmAction = "Select".
IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.
    
   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.


/*************************************/
FOR EACH ttTerr:
    DELETE ttTerr.
END.

IF prmAction = "Delete" THEN DO:
    FIND FIRST buff-terr WHERE buff-terr.terr = vTerr EXCLUSIVE-LOCK .
    IF AVAIL buff-terr  THEN DO:
        DELETE buff-terr.        
    END.  /*IF AVAIL buff-terr */
    
END. /*IF prmAction = "delete"*/
/*************************************************/
IF prmAction = "Add" THEN DO:
    FIND FIRST buff-terr WHERE buff-terr.company = prmComp NO-LOCK.
    IF NOT AVAILABLE buff-terr  THEN DO:
        CREATE ttTerr.
        ASSIGN 
            ttTerr.vTerr    = buff-terr.terr 
            ttTerr.vDscr    = buff-terr.dscr
            .
    END. /* buff-terr  */
END. /*IF prmAction = "add" THEN DO:*/

/*************************************************************/
IF prmAction = "Update" THEN DO:
    FIND FIRST buff-terr WHERE buff-terr.terr = vTerr AND buff-terr.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE buff-terr  THEN DO:
        ASSIGN 
            ttTerr.vDscr = buff-terr.dscr
            .
    END. /* buff-terr  */
  END. /*IF prmAction = "update" THEN DO:*/
/*****************************************************************/
IF prmAction = "Select" THEN DO:
    FOR EACH terr WHERE terr.company = prmComp 
        NO-LOCK:
        CREATE ttTerr.
           ASSIGN 
               ttTerr.vTerr    = terr.terr 
               ttTerr.vDscr    = terr.dscr
               .

    END. /*FOR EACH terr  */
END. /*IF prmAction = "Select" THEN DO:*/
/*needs to be dynamic query*/
IF prmAction = "Search" THEN DO:
    FOR EACH terr WHERE terr.company = prmComp
                   AND (terr.terr = vTerr OR vTerr = "") AND (terr.dscr = vDscr OR vDscr = "") NO-LOCK:
        CREATE ttTerr.
        ASSIGN 
            ttTerr.vTerr = terr.terr 
            ttTerr.vDscr = terr.dscr
            
            .

      END. /*FOR EACH terr  */
END. /*IF prmAction = "Select" THEN DO:*/
