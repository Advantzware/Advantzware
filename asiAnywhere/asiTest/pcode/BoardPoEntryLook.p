




/*------------------------------------------------------------------------
    File        : BoardPoEntryLook.p
    Purpose     : Board Po

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBoardPoEntryLook NO-UNDO 
    FIELD vpono       AS INTEGER
    FIELD vpodate     AS DATE
    FIELD vvendor     AS CHARACTER
    FIELD vitem       AS CHARACTER
    FIELD vname       AS CHARACTER
    FIELD vjob        AS CHARACTER
    FIELD vrun        AS INTEGER
    .
                                           
    
DEFINE DATASET dsBoardPoEntryLook FOR ttBoardPoEntryLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBoardPoEntryLook.
       
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
        FIND FIRST po-ordl WHERE
             po-ordl.company EQ po-ord.company AND
             po-ordl.po-no   EQ po-ord.po-no AND
             po-ordl.deleted = no and po-ordl.job-no <> ""  
            NO-LOCK NO-ERROR.
       /* AND ((po-ordl.stat = "C" and not ip-open) or ~
      (po-ordl.stat <> "C" and ip-open) )*/
       
                 create ttBoardPoEntryLook.
                 assign                                     
                    ttBoardPoEntryLook.vpono    = po-ord.po-no
                    ttBoardPoEntryLook.vpodate  = po-ord.po-date
                    ttBoardPoEntryLook.vvendor  = po-ord.vend-no
                    ttBoardPoEntryLook.vitem    = po-ordl.i-no
                    ttBoardPoEntryLook.vname    = po-ordl.i-name  
                    ttBoardPoEntryLook.vjob     = po-ordl.job-no
                    ttBoardPoEntryLook.vrun     = po-ordl.job-no2 

                   .
                  END.  /*FOR EACH po-ord*/
        
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
       if prmField = "Boardpo"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH po-ord WHERE po-ord.company = prmComp NO-LOCK:
             FOR EACH po-ordl WHERE  po-ord.company = po-ordl.company AND po-ord.po-no = int(prmText)  NO-LOCK :
                 create ttBoardPoEntryLook.
                 assign                                     
                            ttBoardPoEntryLook.vpono    = po-ord.po-no
                            ttBoardPoEntryLook.vpodate  = po-ord.po-date
                            ttBoardPoEntryLook.vvendor  = po-ord.vend-no
                            ttBoardPoEntryLook.vitem    = po-ordl.i-no
                            ttBoardPoEntryLook.vname    = po-ordl.i-name  
                            ttBoardPoEntryLook.vjob     = po-ordl.job-no
                            ttBoardPoEntryLook.vrun     = po-ordl.job-no2 
  .
             END.

          END. /*FOR EACH state*/
         END.
          IF prmCondition = "BEGIN" then do:
               FOR EACH po-ord WHERE po-ord.company = prmComp NO-LOCK:
             FOR EACH po-ordl WHERE  po-ord.company = po-ordl.company AND po-ordl.po-no = int(prmText)  NO-LOCK :
                  create ttBoardPoEntryLook.
                      assign   
                          ttBoardPoEntryLook.vpono      = po-ord.po-no
                            ttBoardPoEntryLook.vpodate  = po-ord.po-date
                            ttBoardPoEntryLook.vvendor  = po-ord.vend-no
                            ttBoardPoEntryLook.vitem    = po-ordl.i-no
                            ttBoardPoEntryLook.vname    = po-ordl.i-name  
                            ttBoardPoEntryLook.vjob     = po-ordl.job-no
                            ttBoardPoEntryLook.vrun     = po-ordl.job-no2   .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
       END.
END.  /* IF prmAction = search then do: */


