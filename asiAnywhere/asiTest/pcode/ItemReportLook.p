



/*------------------------------------------------------------------------
    File        : ItemReportLook.p
    Purpose     : Item

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemReportLook NO-UNDO 
    FIELD vItemno AS CHARACTER
    FIELD vItemName AS CHARACTER
    FIELD vItemDesc AS CHARACTER
    .
                                           
    
DEFINE DATASET dsItemReportLook FOR ttItemReportLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemReportLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCust      = ? THEN ASSIGN prmCust      = "".

DEFINE VAR custcount AS CHAR NO-UNDO.

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
END.


if prmAction <> "search" then do:
        
    FOR EACH itemfg WHERE itemfg.company = prmComp NO-LOCK:
        IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
              create ttItemReportLook.
                      assign
                           ttItemReportLook.vItemno      = itemfg.i-no
                           ttItemReportLook.vItemName    = itemfg.i-name 
                           ttItemReportLook.vItemDesc    = itemfg.i-dscr .
    END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
IF prmAction = "search" then do:
     if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no = prmText NO-LOCK:
                 IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
                 create ttItemReportLook.
                      assign
                           ttItemReportLook.vItemno      = itemfg.i-no
                           ttItemReportLook.vItemName    = itemfg.i-name 
                           ttItemReportLook.vItemDesc    = itemfg.i-dscr .
             END.
         END. /*FOR EACH state*/

         IF prmCondition = "BEGIN" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no BEGINS prmText NO-LOCK :
                 IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
                 create ttItemReportLook.
                      assign
                           ttItemReportLook.vItemno      = itemfg.i-no
                           ttItemReportLook.vItemName    = itemfg.i-name 
                           ttItemReportLook.vItemDesc    = itemfg.i-dscr .

             END .  /*FOR EACH itemfg*/
         END .    /*if prmCondition = BEGIN*/    
     END .  /* if prmField = state  */


     if prmField = "ItemName"  then do:
          if prmCondition = "EQUAL" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-name = prmText NO-LOCK :
                  IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
                  create ttItemReportLook.
                      assign
                           ttItemReportLook.vItemno      = itemfg.i-no
                           ttItemReportLook.vItemName    = itemfg.i-name 
                           ttItemReportLook.vItemDesc    = itemfg.i-dscr .
              END. /*FOR EACH itemfg */
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp AND  itemfg.i-name BEGINS prmText NO-LOCK :
                  IF LOOKUP(itemfg.cust-no, custcount) = 0 THEN NEXT.
                  create ttItemReportLook.
                      assign
                           ttItemReportLook.vItemno      = itemfg.i-no
                           ttItemReportLook.vItemName    = itemfg.i-name 
                           ttItemReportLook.vItemDesc    = itemfg.i-dscr .
              END.  /*FOR EACH itemfg*/
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = ItemName  */
END.  /* IF prmAction = search then do: */

