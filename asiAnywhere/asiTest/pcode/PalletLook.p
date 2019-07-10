



/*------------------------------------------------------------------------
    File        : PalletLook.p
    Purpose     : Pallet

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttmatLook NO-UNDO 
    FIELD vmat AS CHARACTER
    FIELD vdscr AS CHARACTER
   .
                                           
    
DEFINE DATASET dsrfqpallet1Look FOR ttmatLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsrfqpallet1Look.
       
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
        
    FOR EACH matprep WHERE matprep.company = prmComp NO-LOCK:
                 create ttmatLook.
                 assign                                     
                    ttmatLook.vmat = matprep.mat
                    ttmatLook.vdscr= matprep.dscr
                    

                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "matcode"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH matprep WHERE  matprep.company = prmComp AND matprep.mat = prmText  NO-LOCK :
                 create ttmatLook.
                 assign                                     
                    ttmatLook.vmat  = matprep.mat
                    ttmatLook.vdscr = matprep.dscr  .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH matprep WHERE  matprep.company = prmComp AND matprep.mat BEGINS prmText NO-LOCK :
                     create ttmatLook.
                 assign                                     
                    ttmatLook.vmat  = matprep.mat
                    ttmatLook.vdscr = matprep.dscr  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */



