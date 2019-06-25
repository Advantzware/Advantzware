
/*------------------------------------------------------------------------
    File        : apctrl_view.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttAPControlView NO-UNDO
    FIELD payable      AS CHAR
    FIELD purch      AS CHAR
    FIELD cashact    AS CHAR
    FIELD disc       AS CHAR
    FIELD stax       AS CHAR
    FIELD freight    AS CHAR
    FIELD reckey     AS CHAR
    FIELD extra      AS CHAR 
    .

DEFINE DATASET dsAPControlView FOR ttAPControlView.
    

DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmpayable    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmpurch      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcashact    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmdisc       AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmstax       AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmfreight    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmReckey   AS CHAR  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAPControlView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     

IF prmAction         = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp           = ?  THEN ASSIGN prmComp        = "".
IF prmUser           = ?  THEN ASSIGN prmUser        = "".
IF prmpayable        = ?  THEN ASSIGN prmpayable     = "".
IF prmpurch          = ?  THEN ASSIGN prmpurch       = "".
IF prmcashact        = ?  THEN ASSIGN prmcashact     = "".
IF prmdisc           = ?  THEN ASSIGN prmdisc        = "".
IF prmstax           = ?  THEN ASSIGN prmstax        = "".
IF prmfreight        = ?  THEN ASSIGN prmfreight     = "".
                                                  


DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.

         
     
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


   IF prmAction = "Update" THEN DO:
     FIND FIRST ap-ctrl WHERE ap-ctrl.company = prmComp
          NO-LOCK NO-ERROR.

     if not can-find(first account where account.company = cocode and
                                         account.actnum BEGINS prmpayable)
      then do:
         cError = "Invalid Account Payable, Try help.. " .
         return .
      end.

    if not can-find(first account where account.company = cocode and
                                         account.actnum BEGINS prmpurch)
      then do:
         cError = "Invalid Purchases Account, Try help.. " .
         return .
      end.

       if not can-find(first account where account.company = cocode and
                                         account.actnum BEGINS prmcashact)
      then do:
         cError = "Invalid Case Account, Try help.. " .
         return .
      end.
       if not can-find(first account where account.company = cocode and
                                         account.actnum BEGINS prmdisc)
      then do:
         cError = "Invalid Discount Account, Try help.. " .
         return .
      end.
       if not can-find(first account where account.company = cocode and
                                         account.actnum BEGINS prmstax)
      then do:
         cError = "Invalid Sales Tax Account, Try help.. " .
         return .
      end.
       if not can-find(first account where account.company = cocode and
                                         account.actnum BEGINS prmfreight)
      then do:
         cError = "Invalid Freight Account, Try help.. " .
         return .
      end.
   
   END. /* validate update */
 

 IF prmAction = "Update" THEN DO:
     FIND FIRST ap-ctrl WHERE ap-ctrl.company = prmComp
          EXCLUSIVE-LOCK NO-ERROR.


    IF AVAIL ap-ctrl THEN
        ASSIGN
            ap-ctrl.payables      = prmpayable
            ap-ctrl.purchases     = prmpurch   
            ap-ctrl.cash-act      = prmcashact
            ap-ctrl.discount      = prmdisc   
            ap-ctrl.stax          = prmstax   
            ap-ctrl.freight        = prmfreight .

        ASSIGN
            prmAction = "View" . 
 END.
 

 IF prmAction = "View" THEN DO:
     FIND FIRST ap-ctrl WHERE ap-ctrl.company = prmComp
          NO-LOCK NO-ERROR.

     CREATE ttAPControlView.
        ASSIGN
             ttAPControlView.payable = ap-ctrl.payables
             ttAPControlView.purch   = ap-ctrl.purchases
             ttAPControlView.cashact = ap-ctrl.cash-act 
             ttAPControlView.disc    = ap-ctrl.discount
             ttAPControlView.stax    = ap-ctrl.stax
             ttAPControlView.freight = ap-ctrl.freight . 


 END.


