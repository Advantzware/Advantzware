




/*------------------------------------------------------------------------
    File         : ProcatLook.p
    Purpose     :  ProcatLook

    Syntax      :

    Description : Return a Dataset of category

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttProcatLook NO-UNDO 
    FIELD vProcat           AS CHARACTER
    FIELD vDscr          AS CHARACTER
    FIELD vitemname        AS CHARACTER
   
    .
                                           
    
DEFINE DATASET dsProcatLook FOR ttProcatLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsProcatLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
        
    FOR EACH procat WHERE procat.company = prmComp NO-LOCK:
        create ttProcatLook.
        assign                                     
            ttProcatLook.vProcat      = procat.procat
            ttProcatLook.vDscr     = procat.dscr.
            
        
   END.  /*FOR EACH procat*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
       
     if prmField = "procat"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH procat WHERE procat.company = prmComp AND procat.procat = prmText  NO-LOCK :
                  IF procat.procat <> "" THEN DO:
                 create ttProcatLook.
                 assign                                     
                    ttProcatLook.vProcat      = procat.procat
                     ttProcatLook.vDscr     = procat.dscr.
                 END.
             END.
          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH procat WHERE procat.company = prmComp AND procat.procat BEGINS prmText NO-LOCK :
                       IF procat.procat <> "" THEN DO:
                      create ttProcatLook.
                      assign   
                          ttProcatLook.vProcat      = procat.procat
                          ttProcatLook.vDscr     = procat.dscr.
                       END.
                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */




