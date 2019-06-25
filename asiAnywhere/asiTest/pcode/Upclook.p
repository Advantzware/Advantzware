



/*------------------------------------------------------------------------
    File        : dielook.p
    Purpose     : dielookup

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttupcLook NO-UNDO 
    FIELD vupc-no          AS CHARACTER
    FIELD vi-name           AS CHARACTER
   
    .
                                           
   


DEFINE DATASET dsupcLook FOR ttupcLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsupcLook.
       
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
        
    FOR EACH itemfg WHERE prmComp = itemfg.company NO-LOCK:
        IF itemfg.upc-no <> "" THEN DO:
       
                 create ttupcLook.
                 assign                                     
                    ttupcLook.vupc-no        = itemfg.upc-no
                    ttupcLook.vi-name        = itemfg.i-name   .
                    
        END.
                  
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "upc-no"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE prmComp = itemfg.company AND  itemfg.upc-no = prmText  NO-LOCK :
                 create ttupcLook.
                 assign                                     
                    ttupcLook.vupc-no          = itemfg.upc-no
                    ttupcLook.vi-name          = itemfg.i-name
                    
 .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH itemfg WHERE prmComp = itemfg.company AND  itemfg.upc-no BEGINS prmText NO-LOCK :
                      create ttupcLook.
                 assign                                     
                     ttupcLook.vupc-no        = itemfg.upc-no
                    ttupcLook.vi-name         = itemfg.i-name
                    
  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */



