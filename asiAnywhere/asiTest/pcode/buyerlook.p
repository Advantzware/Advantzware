/*------------------------------------------------------------------------
    File        : buyerlook.p
    Purpose     : PO#
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttbuyerLookup NO-UNDO 
    FIELD vbuyer           AS CHAR
    FIELD vBuyerN         AS CHAR
    .

DEFINE DATASET dsbuyerLookup FOR ttbuyerLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsbuyerLookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.


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
MESSAGE "prmAction  " prmAction.
if prmAction = "PoSelect" then do:


    FOR EACH buyer WHERE buyer.company = prmComp NO-LOCK: 
    
        IF AVAIL buyer THEN
              create ttbuyerLookup.
                      assign
                           ttbuyerLookup.vBuyer      = buyer.buyer
                           ttbuyerLookup.vBuyerN    = buyer.buyer-n
                            .

       

    END.  /*FOR EACH buyer*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
MESSAGE "prmField  prmCondition prmText   " prmField prmCondition prmText.
IF prmAction = "PoSearch" then do:
     if prmField = "Buyer"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH buyer WHERE buyer.company = prmComp AND buyer.buyer = prmText NO-LOCK:
                 create ttbuyerLookup.
                      assign
                           ttbuyerLookup.vBuyer      = buyer.buyer
                           ttbuyerLookup.vBuyerN    = buyer.buyer-n .

        
             END.   /*FOR EACH state*/
         END. 

        if prmCondition = "BEGIN" then do:
             FOR EACH buyer WHERE buyer.company = prmComp AND buyer.buyer BEGINS prmText NO-LOCK:
                 create ttbuyerLookup.
                      assign
                           ttbuyerLookup.vBuyer      = buyer.buyer
                           ttbuyerLookup.vBuyerN    = buyer.buyer-n .

        
             END.   /*FOR EACH state*/
         END. 

          
     END .  /* if prmField = state  */


     if prmField = "Buyer Name"  then do:
          if prmCondition = "EQUAL" then do:
              FOR EACH buyer WHERE buyer.company = prmComp AND buyer.buyer-n = prmText NO-LOCK:
                 create ttbuyerLookup.
                      assign
                           ttbuyerLookup.vBuyer      = buyer.buyer
                           ttbuyerLookup.vBuyerN    = buyer.buyer-n .

        
             END.   /*FOR EACH state*/
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
              FOR EACH buyer WHERE buyer.company = prmComp AND buyer.buyer-n BEGINS prmText NO-LOCK:
                 create ttbuyerLookup.
                      assign
                           ttbuyerLookup.vBuyer      = buyer.buyer
                           ttbuyerLookup.vBuyerN    = buyer.buyer-n .

        
             END.   /*FOR EACH state*/
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */

          
