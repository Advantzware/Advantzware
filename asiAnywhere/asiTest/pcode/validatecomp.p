
                                 
/*------------------------------------------------------------------------
    File        : validatecomp.p
    Purpose     : validation

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Parveen
    Created     : fri may 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmstate         AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmzip           AS CHARACTER NO-UNDO. 
                                    
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ?   THEN ASSIGN prmAction = "".

IF prmstate      = ? THEN ASSIGN   prmstate         = "".
IF prmzip        = ? THEN ASSIGN   prmzip           = "".
                                                 
IF prmAction = ""  THEN ASSIGN prmAction = "Select".

DEF VAR prmLoc AS CHAR NO-UNDO.

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

IF prmAction = "Validate" THEN DO:
   
    
    FIND FIRST statecod WHERE statecod.statecod = prmstate NO-LOCK NO-ERROR.
    IF NOT AVAILABLE statecod THEN DO:
        ASSIGN cError  = "Invalid State".
        RETURN. 
    END.
    FIND FIRST zipcode WHERE zipcode.zipcode = prmzip NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid zip".
        RETURN. 
    END.
    
     
END.

/*************************************/
/*****************************************PROCEDURE assign-RfqShipping******************************/


