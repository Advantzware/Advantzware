



/*------------------------------------------------------------------------
    File        : Dielook.p
    Purpose     : dielookup

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttDieLook NO-UNDO 
    FIELD vspc-no          AS CHARACTER
    FIELD vi-name           AS CHARACTER
    FIELD vMat              AS CHAR
   
    .

DEFINE DATASET dsDieLook FOR ttDieLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPlateType  like item.mat-type NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDieLook.
       
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
MESSAGE "testdie" prmAction prmPlateType .
if prmAction <> "search" then do:
    MESSAGE "act" prmPlateType.    
    FOR EACH prep WHERE  prep.company = prmComp   and 
             (INDEX(prmPlateType, prep.mat-type) > 0  or prmPlateType = "") NO-LOCK:
                 create ttDieLook.
                 assign                                     
                    ttDieLook.vspc-no        = prep.CODE
                    ttDieLook.vi-name        = prep.dscr 
                    ttDieLook.vMat           = prep.mat-type
                       .
                    
          
                  
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "code"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH prep WHERE  prmComp = prep.company AND  prep.CODE = prmText  NO-LOCK :
                 create ttDieLook.
                 assign                                     
                    ttDieLook.vspc-no        = prep.CODE
                    ttDieLook.vi-name        = prep.dscr 
                    ttDieLook.vMat           = prep.mat-type
                    
 .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH prep WHERE prmComp = prep.company AND prep.CODE BEGINS prmText NO-LOCK :
                      create ttDieLook.
                 assign                                     
                    ttDieLook.vspc-no        = prep.CODE
                    ttDieLook.vi-name        = prep.dscr 
                    ttDieLook.vMat           = prep.mat-type
  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           

  if prmField = "plate"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH prep WHERE  prmComp = prep.company AND  prep.CODE = prmText AND prep.mat-type = prmPlateType   NO-LOCK :
                 create ttDieLook.
                 assign                                     
                    ttDieLook.vspc-no        = prep.CODE
                    ttDieLook.vi-name        = prep.dscr 
                    ttDieLook.vMat           = prep.mat-type
                    
 .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH prep WHERE prmComp = prep.company AND prep.CODE BEGINS prmText AND prep.mat-type = prmPlateType NO-LOCK :
                      create ttDieLook.
                 assign                                     
                    ttDieLook.vspc-no        = prep.CODE
                    ttDieLook.vi-name        = prep.dscr 
                    ttDieLook.vMat           = prep.mat-type
  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

END.  /* IF prmAction = search then do: */



