/*------------------------------------------------------------------------
    File        : fgitemLook.p
    Purpose     : FGItem
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFGitemLookup NO-UNDO 
    FIELD vno           AS CHARACTER
    FIELD vname         AS CHARACTER
    FIELD vcust          AS CHARACTER
    FIELD vcustpart      AS CHARACTER
    .
    
DEFINE DATASET dsFGitemLookup FOR ttFGitemLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFGitemLookup.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE custcount AS CHARACTER NO-UNDO .
IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

if prmAction <> "search" then do:
        
    FOR EACH itemfg WHERE itemfg.company = prmComp
        AND LOOKUP(itemfg.cust-no, custcount ) NE 0 AND itemfg.cust-no NE ""  NO-LOCK:
              create ttFGitemLookup.
                      assign
                           ttFGitemLookup.vno       = itemfg.i-no
                           ttFGitemLookup.vname     = itemfg.i-name
                           ttFGitemLookup.vcust      = itemfg.cust-no
                           ttFGitemLookup.vcustpart  = itemfg.part-no
                          
                          .
    END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
IF prmAction = "search" then do:
     if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp  
                 AND LOOKUP(itemfg.cust-no, custcount ) NE 0 
                 AND itemfg.cust-no NE ""
                 AND itemfg.i-no = prmText NO-LOCK:
                 create ttFGitemLookup.
                      assign
                           ttFGitemLookup.vno       = itemfg.i-no
                           ttFGitemLookup.vname     = itemfg.i-name
                           ttFGitemLookup.vcust      = itemfg.cust-no
                           ttFGitemLookup.vcustpart  = itemfg.part-no .
             END.
         END. /*FOR EACH state*/

         IF prmCondition = "BEGIN" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no BEGINS prmText 
                 AND LOOKUP(itemfg.cust-no, custcount ) NE 0 AND itemfg.cust-no NE "" NO-LOCK :
                 create ttFGitemLookup.
                      assign
                           ttFGitemLookup.vno       = itemfg.i-no
                           ttFGitemLookup.vname     = itemfg.i-name
                           ttFGitemLookup.vcust      = itemfg.cust-no
                           ttFGitemLookup.vcustpart  = itemfg.part-no .

             END .  /*FOR EACH itemfg*/
         END .    /*if prmCondition = BEGIN*/    
     END .  /* if prmField = state  */


     if prmField = "custpart"  then do:
          if prmCondition = "EQUAL" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.part-no = prmText 
                  AND LOOKUP(itemfg.cust-no, custcount ) NE 0 AND itemfg.cust-no NE "" NO-LOCK :
                  create ttFGitemLookup.
                      assign
                           ttFGitemLookup.vno       = itemfg.i-no
                           ttFGitemLookup.vname     = itemfg.i-name
                           ttFGitemLookup.vcust      = itemfg.cust-no
                           ttFGitemLookup.vcustpart  = itemfg.part-no
                      .
              END. /*FOR EACH itemfg */
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp AND  itemfg.part-no BEGINS prmText
                  AND LOOKUP(itemfg.cust-no, custcount ) NE 0 AND itemfg.cust-no NE "" NO-LOCK :
                  create ttFGitemLookup.
                      assign
                           ttFGitemLookup.vno       = itemfg.i-no
                           ttFGitemLookup.vname     = itemfg.i-name
                           ttFGitemLookup.vcust      = itemfg.cust-no
                           ttFGitemLookup.vcustpart  = itemfg.part-no
                      .
              END.  /*FOR EACH itemfg*/
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */



