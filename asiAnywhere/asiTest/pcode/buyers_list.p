
/*------------------------------------------------------------------------
    File        : buyers_list.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttNewBuyersList NO-UNDO
    FIELD vbuyer     AS CHAR
    FIELD vnum       AS CHAR
    FIELD reckey     AS CHAR
    FIELD extra      AS CHAR 
    .

DEFINE DATASET dsNewBuyersList FOR ttNewBuyersList.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmvbuyer   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmvnum     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHAR  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsNewBuyersList .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction        = ?  THEN ASSIGN prmAction    = "Select".
IF prmComp          = ?  THEN ASSIGN prmComp      = "".
IF prmUser          = ?  THEN ASSIGN prmUser      = "".
IF prmvbuyer         = ?  THEN ASSIGN prmvbuyer     = "".
IF prmvnum           = ?  THEN ASSIGN prmvnum       = "".
                                                  


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
 DEF BUFFER bf-buyer FOR buyer .
         
     
 IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.
   ASSIGN  cocode  = prmComp .



IF prmAction = "GridSelect" THEN DO:
    FOR EACH buyer WHERE buyer.company = prmComp NO-LOCK: 
        CREATE ttNewBuyersList.
        ASSIGN
            ttNewBuyersList.vbuyer = buyer.buyer
            ttNewBuyersList.vnum   = buyer.buyer-n
            ttNewBuyersList.reckey = buyer.rec_key  . 
    END.
                                        
                                        
     
END. /*IF prmAction = "Select" THEN DO:*/


IF prmAction = "GridSearch" THEN DO:
    FOR EACH buyer WHERE buyer.company = prmComp 
        AND (buyer.buyer BEGINS prmvbuyer OR prmvbuyer = "")
        AND (buyer.buyer-n BEGINS prmvnum OR prmvnum = "") NO-LOCK: 
        CREATE ttNewBuyersList.
        ASSIGN
            ttNewBuyersList.vbuyer = buyer.buyer
            ttNewBuyersList.vnum   = buyer.buyer-n
            ttNewBuyersList.reckey = buyer.rec_key  . 
    END.
          

 END.

 

 IF prmAction = "Update" THEN DO:
     FIND FIRST buyer WHERE buyer.company = prmComp
         AND buyer.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.


    IF AVAIL buyer THEN
        ASSIGN
            buyer.buyer         = prmvbuyer 
            buyer.buyer-n       = prmvnum   . 
    ASSIGN 
        prmAction = "View" .



 END.

 IF prmAction = "Add" THEN DO:
     FIND FIRST bf-buyer WHERE bf-buyer.company = prmComp 
                                AND bf-buyer.buyer = prmvbuyer NO-LOCK NO-ERROR.
     IF AVAIL bf-buyer THEN DO:
         cError = "Buyer Already exists with company " + prmComp + " Buyer " + prmvbuyer + "." . 
         RETURN.
     END.

 END. /* validate of add */ 
 
 IF prmAction = "Add" THEN DO:

     CREATE buyer.

     ASSIGN
            buyer.company       = prmComp
            buyer.buyer         = prmvbuyer 
            buyer.buyer-n       = prmvnum   . 

     ASSIGN
         prmReckey = buyer.rec_key
         prmAction = "View" .

 END.

 IF prmAction = "Delete" THEN DO:
    FIND FIRST buyer WHERE buyer.company = prmComp
         AND buyer.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

     IF AVAIL buyer THEN
         DELETE buyer.

    FIND FIRST buyer WHERE buyer.company = prmComp NO-LOCK NO-ERROR.
     IF AVAIL buyer THEN
         ASSIGN
         prmAction = "View"
         prmReckey = buyer.rec_key.

 END.

 IF prmAction = "View" THEN DO:
     FIND FIRST buyer WHERE buyer.company = prmComp
         AND buyer.rec_key = prmReckey NO-LOCK NO-ERROR.

     CREATE ttNewBuyersList.
        ASSIGN
              ttNewBuyersList.vbuyer = buyer.buyer
              ttNewBuyersList.vnum   = buyer.buyer-n
              ttNewBuyersList.reckey = buyer.rec_key  . 


 END.


