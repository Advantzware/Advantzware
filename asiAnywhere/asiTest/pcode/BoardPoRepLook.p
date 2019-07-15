




/*------------------------------------------------------------------------
    File        : BoardPoRepLook.p
    Purpose     : Board Po

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBoardPoRepLook NO-UNDO 
    FIELD vpono       AS INTEGER
    FIELD vpodate     AS DATE
    FIELD vvendor     AS CHARACTER
    FIELD vprint       AS LOGICAL
    FIELD vstat       AS CHARACTER
   
    .
                                           
    
DEFINE DATASET dsBoardPoRepLook FOR ttBoardPoRepLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBoardPoRepLook.
       
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
    FOR EACH po-ord WHERE po-ord.company = prmComp NO-LOCK:
       
       create ttBoardPoRepLook.
                 assign                                     
                    ttBoardPoRepLook.vpono    = po-ord.po-no
                    ttBoardPoRepLook.vpodate  = po-ord.po-date
                    ttBoardPoRepLook.vvendor  = po-ord.vend-no
                    ttBoardPoRepLook.vprint    = po-ord.printed
                    ttBoardPoRepLook.vstat    = po-ord.stat   
                   
                   .
                  END.  /*FOR EACH po-ord*/
        
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
       if prmField = "Boardpo"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH po-ord WHERE po-ord.company = prmComp AND po-ord.po-no = INT(prmtext) NO-LOCK:
            
                  create ttBoardPoRepLook.
                 assign                                     
                    ttBoardPoRepLook.vpono    = po-ord.po-no
                    ttBoardPoRepLook.vpodate  = po-ord.po-date
                    ttBoardPoRepLook.vvendor  = po-ord.vend-no
                    ttBoardPoRepLook.vprint    = po-ord.printed
                    ttBoardPoRepLook.vstat    = po-ord.stat  
  .
            

          END. /*FOR EACH state*/
         END.
          IF prmCondition = "BEGIN" then do:
               FOR EACH po-ord WHERE po-ord.company = prmComp  AND po-ord.po-no = INT(prmtext)  NO-LOCK:
            
                   create ttBoardPoRepLook.
                 assign                                     
                    ttBoardPoRepLook.vpono    = po-ord.po-no
                    ttBoardPoRepLook.vpodate  = po-ord.po-date
                    ttBoardPoRepLook.vvendor  = po-ord.vend-no
                    ttBoardPoRepLook.vprint    = po-ord.printed
                    ttBoardPoRepLook.vstat    = po-ord.stat   .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
       
END.  /* IF prmAction = search then do: */



