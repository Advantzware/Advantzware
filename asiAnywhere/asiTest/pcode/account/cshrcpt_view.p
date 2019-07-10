
/*------------------------------------------------------------------------
    File        : cshrcpt_view.p
    Purpose     : Accounts Receivable

     Main File   : 
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEnterEditCashReceiptView NO-UNDO
    FIELD custno       AS CHAR
    FIELD invno        AS INT
    FIELD invdt        AS CHAR
    FIELD baldu        AS DEC
    FIELD disc         AS DEC
    FIELD cashpy       AS DEC
    FIELD totl_app     AS DECIMAL
    FIELD actno        AS CHAR
    FIELD actdscr      AS CHAR
    FIELD balaftr      AS DEC
    
    FIELD ext          AS CHAR
    FIELD reckey       AS CHAR

  
    .

DEFINE DATASET dsEnterEditCashReceiptView FOR ttEnterEditCashReceiptView.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustno       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prminvno        AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prminvdt        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmbaldu        AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmdisc         AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmcashpy       AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmtotl_app     AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmactno        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmbalaftr      AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmOut          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEnterEditCashReceiptView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttEnterEditCashReceiptView:
        DELETE ttEnterEditCashReceiptView .
    END.

IF prmAction      = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp        = ?  THEN ASSIGN prmComp       = "".
IF prmUser        = ?  THEN ASSIGN prmUser       = "".
IF prmcustno      = ?  THEN ASSIGN prmcustno     = "".
IF prminvno       = ?  THEN ASSIGN prminvno      = 0.
IF prminvdt       = ?  THEN ASSIGN prminvdt      = "".
IF prmbaldu       = ?  THEN ASSIGN prmbaldu      = 0.
IF prmdisc        = ?  THEN ASSIGN prmdisc       = 0.
IF prmcashpy      = ?  THEN ASSIGN prmcashpy     = 0.
IF prmtotl_app    = ?  THEN ASSIGN prmtotl_app   = 0.
IF prmactno       = ?  THEN ASSIGN prmactno      = "".
IF prmactdscr     = ?  THEN ASSIGN prmactdscr    = "".
IF prmbalaftr     = ?  THEN ASSIGN prmbalaftr    = 0.
IF prmOut         = ?  THEN ASSIGN prmOut        = "".
IF prmReckey      = ?  THEN ASSIGN prmReckey     = "".





DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.


{sys/inc/VAR.i "new shared"}  


     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .
             


DEF BUFFER bf-cashl FOR ar-cashl.
DEF BUFFER b-ar-inv FOR ar-inv.
DEF BUFFER ar-cashl-inv-line FOR reftable.

DEF VAR lv-account-recid AS RECID NO-UNDO.
DEF VAR account_dscr AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR ll-new-record AS LOG NO-UNDO.
DEF VAR ll-is-a-return AS LOG NO-UNDO.
DEF VAR v-armemo-log AS LOG NO-UNDO.

DEF BUFFER bf-cash FOR ar-cash.
DEF BUFFER ar-c-memo FOR reftable.


DEF VAR lv-unapp-amt AS DEC NO-UNDO.
DEF VAR lv-pay-amt AS DEC FORM "->>>,>>9.99" NO-UNDO.
DEF VAR v-overpay AS LOG NO-UNDO.

DEF VAR lv-tot-pay AS DEC NO-UNDO.  /* line cash total pay amt */
DEF VAR lv-disc-calced AS LOG NO-UNDO.

DEF VAR ll-from-memo AS LOG NO-UNDO.

&SCOPED-DEFINE ARCASH ARCASH
&SCOPED-DEFINE where-ar-c-memo                                      ~
        WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"             ~
          AND ar-c-memo.company  EQ ar-cash.company                 ~
          AND ar-c-memo.loc      EQ ""

DEF VAR v-inv-bal AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.

FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.

FUNCTION calc-inv-bal RETURNS DECIMAL
    /*(ip-browser AS LOG)*/:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR v-amt-due  LIKE ar-cashl.amt-due  NO-UNDO.
  DEF VAR v-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
  
  
 /* IF ip-browser THEN*/
    ASSIGN
     v-amt-due  = ar-cashl.amt-due
     v-amt-paid = ar-cashl.amt-paid.
 /* ELSE 
    ASSIGN
     v-amt-due  =  DEC(prmbaldu)
     v-amt-paid =  DEC(prmtotl_app).  */

  /* Function return value. */
  RETURN IF ar-cashl.inv-no NE 0 
           THEN v-amt-due - (IF ar-cash.posted THEN 0 ELSE v-amt-paid)
           ELSE 0.

END FUNCTION.

FUNCTION calc-paid-amt RETURNS DECIMAL
  /*(ip-browser AS LOG)*/:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
  DEF VAR ld-amt-disc LIKE ar-cashl.amt-disc NO-UNDO.

 /* IF ip-browser THEN */
    ASSIGN
     ld-amt-paid = ar-cashl.amt-paid
     ld-amt-disc = ar-cashl.amt-disc.
 /* ELSE 
    ASSIGN
     ld-amt-paid = DEC(prmtotl_app)
     ld-amt-disc = DEC(prmdisc). */

  /* Function return value. */
  RETURN ld-amt-paid - (IF ar-cash.posted THEN  0 ELSE ld-amt-disc). 
         
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calc-tot-app B-table-Win 
FUNCTION calc-tot-app RETURNS DECIMAL
  /*(ip-browser AS LOG)*/:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ld-amt-paid LIKE ar-cashl.amt-paid NO-UNDO.
  DEF VAR ld-amt-disc LIKE ar-cashl.amt-disc NO-UNDO.


/*  IF ip-browser THEN
    ASSIGN
     ld-amt-paid = ar-cashl.amt-paid
     ld-amt-disc = ar-cashl.amt-disc. */
 /* ELSE */
    ASSIGN
     ld-amt-paid = DEC(prmtotl_app)
     ld-amt-disc = DEC(prmdisc).

  /* Function return value. */
  RETURN ld-amt-paid + (IF ar-cash.posted THEN ld-amt-disc ELSE 0).

END FUNCTION.

FUNCTION display-account RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RELEASE account.

  IF lv-account-recid NE ? THEN
  FIND account WHERE RECID(account) EQ lv-account-recid NO-LOCK NO-ERROR.

  ELSE
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ (IF AVAIL ar-cashl THEN ar-cashl.actnum
                                  ELSE prmactno)
        NO-LOCK NO-ERROR.         
 

  RETURN IF AVAIL account THEN account.dscr ELSE "".

END FUNCTION.




IF prmAction = "SelectGrid" THEN DO:
    FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FOR EACH ar-cashl OF ar-cash NO-LOCK:

             CREATE ttEnterEditCashReceiptView.
             ASSIGN 
                 ttEnterEditCashReceiptView.invno        = ar-cashl.inv-no 
                 ttEnterEditCashReceiptView.invdt        = STRING(ar-cashl.inv-date)  
                 ttEnterEditCashReceiptView.baldu        = ar-cashl.amt-due  
                 ttEnterEditCashReceiptView.disc         = ar-cashl.amt-disc   
                 ttEnterEditCashReceiptView.totl_app     = ar-cashl.amt-paid
                 ttEnterEditCashReceiptView.cashpy       = calc-paid-amt()
                 ttEnterEditCashReceiptView.actno        = ar-cashl.actnum    
                 ttEnterEditCashReceiptView.actdscr      = display-account()
                 ttEnterEditCashReceiptView.balaftr      = calc-inv-bal() 
                 ttEnterEditCashReceiptView.reckey       = ar-cashl.rec_key
                 
                .
             
            
      END. /*FOR EACH ar-cashl  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/********************************Add **********************************/

IF prmAction = "ValidateAdd" THEN DO:
    
      FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
   
      /*RELEASE bf-cashl.

       FIND FIRST bf-cashl WHERE bf-cashl.c-no = ar-cash.c-no
                             AND bf-cashl.inv-no = prminvno
                             AND NOT CAN-FIND(FIRST ar-c-memo WHERE
                                             /* {&where-ar-c-memo}
                                                AND*/ ar-c-memo.code EQ bf-cashl.rec_key)
                             NO-LOCK NO-ERROR.
       IF AVAIL bf-cashl THEN DO:
           cError = "Invoice already on Cash Receipt.Please Re-Enter Invoice Number.".
           RETURN.
       END.*/  

       IF ar-cashl.inv-no = 0 AND ar-cashl.amt-disc <> 0 THEN DO:
         cError = "Discount cannot be entered on account." .
         RETURN .
      END.

      IF  ar-cashl.inv-no = 0 AND ar-cashl.amt-paid LT 0 THEN DO:
         cError = "Total Applied cannot be negative.".
         RETURN .
      END.

      FIND FIRST account WHERE account.company = cocode AND
                                account.TYPE <> "T" AND
                                account.actnum = prmactno
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN.
       END.
       prmactdscr = display-account() .

       IF ar-cash.posted THEN do:
           cError = "This Cash Receipt has been posted. No addings are allowed!" .
           RETURN.
       END.
      
    

END.  /* end of validate add*/

/***********************check add user******************************************/


IF prmAction = "AddNew" THEN DO:

     FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
    
    DEF VAR li-next-line AS INT NO-UNDO.

  FOR EACH bf-cashl OF ar-cash NO-LOCK BY LINE DESCENDING:
      li-next-line = bf-cashl.LINE.
      LEAVE.
  END.

  CREATE ar-cashl.

  ASSIGN ar-cashl.company = ar-cash.company
         ar-cashl.c-no = ar-cash.c-no
         ar-cashl.LINE = li-next-line + 1
         ar-cashl.cust-no = ar-cash.cust-no
         ar-cashl.check-no = STRING(ar-cash.check-no,"9999999999")
         ar-cashl.inv-date = TODAY.

  find first ar-ctrl where ar-ctrl.company = ar-cash.company no-lock no-error.

  find first bank where bank.company = ar-cash.company and
                        bank.bank-code = ar-cash.bank-code no-lock no-error.
  if avail bank THEN do:
     find first account where account.company = ar-cash.company and
                              account.actnum  = bank.actnum no-lock no-error.
     assign ar-cashl.actnum = bank.actnum.
  end.
  ELSE do:
    if ar-cash.check-no ge 90000000 AND ar-cash.check-no le 99999999 
    THEN find first account where account.company = ar-cash.company and
                            account.actnum  = ar-ctrl.sales no-lock no-error.
    ELSE find first account where account.company = ar-cash.company and
                             account.actnum  = ar-ctrl.cash-act no-lock no-error.
    if avail account THEN assign ar-cashl.actnum = account.actnum.
  end.
  
  if available account then lv-account-recid = RECID(account). /* for displaying*/
  

  ASSIGN
      prmReckey = ar-cashl.rec_key 
      prmAction = "View"  .

  

END.

IF prmAction = "Add" THEN DO:
    
    FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR. 

     ASSIGN
         ar-cashl.inv-no      =  prminvno      
         ar-cashl.amt-disc    =  prmdisc       
         ar-cashl.amt-paid    =  prmtotl_app   
         ar-cashl.actnum      =  prmactno      
         ar-cashl.dscr        =  prmactdscr  . 


     ASSIGN
      prmReckey = ar-cashl.rec_key 
      prmAction = "View" .
                               
                               

END. 


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.


ll-from-memo = AVAIL ar-cashl AND
                 CAN-FIND(FIRST ar-c-memo
                          {&where-ar-c-memo}
                            AND ar-c-memo.code EQ ar-cashl.rec_key).
     
        RELEASE bf-cashl.
       
        IF NOT ll-from-memo THEN
           FIND FIRST bf-cashl WHERE bf-cashl.c-no = ar-cashl.c-no
                                 AND bf-cashl.inv-no = INT(prminvno)
                                 AND RECID(bf-cashl) <> RECID(ar-cashl)
                                 AND NOT CAN-FIND(FIRST ar-c-memo
                                                  {&where-ar-c-memo}
                                                    AND ar-c-memo.code EQ bf-cashl.rec_key)
                NO-LOCK NO-ERROR.
        IF AVAIL bf-cashl THEN DO:
           cError = "Invoice already on Cash Receipt. Please Re-Enter Invoice Number.".
           RETURN.
        END.
       
        FIND FIRST ar-inv WHERE ar-inv.company = g_company 
                            AND ar-inv.posted
                            AND ar-inv.cust-no = ar-cash.cust-no
                            AND ar-inv.inv-no = INT(prminvno)
                            AND (ar-inv.due <> 0 OR v-overpay)
                          NO-LOCK NO-ERROR.
        IF NOT AVAIL ar-inv THEN DO:
           ASSIGN prminvno = 0
                  prmbaldu = 0
                  prminvdt = STRING(ar-cash.check-date)
                  prmtotl_app = (ar-cashl.amt-paid - ar-cashl.amt-disc)
                  prmdisc = 0
                  prmcashpy = calc-paid-amt().
           RETURN.
        END.
       
        
     /*   IF ar-inv.due < INPUT ar-cashl.amt-paid THEN DO:
           DEF VAR choice AS LOG NO-UNDO.
           choice = v-overpay.
           IF choice THEN
              cError = "Allow overpayment on invoice?" .
           
           IF NOT choice THEN DO:
              MESSAGE "Amount being applied may not exceed the invoice amount." VIEW-AS ALERT-BOX ERROR.
              ASSIGN ar-cashl.amt-paid:SCREEN-VALUE = STRING(ar-inv.due)
                     lv-pay-amt:SCREEN-VALUE = STRING(ar-inv.due).
           END.
        END.
        END.*/
       
        FIND FIRST account WHERE account.company = cocode AND
                                account.TYPE <> "T" AND
                                account.actnum = prmactno
                                NO-LOCK NO-ERROR.
       IF NOT AVAIL account THEN DO:
          cError = "Invalid GL Account Number.".
          RETURN.
       END.
       prmactdscr = display-account() .                    
       
        IF  ar-cashl.inv-no = 0 AND  ar-cashl.amt-disc <> 0 THEN DO:
           cError = "Discount can not be entered on account.".
           RETURN.
        END.
       
        IF  ar-cashl.amt-disc = 0 AND  ar-cashl.amt-paid = 0 THEN DO:
           cError = "Line Item Total must be greater than 0." .
           RETURN .
        END.
       
        IF NOT ll-from-memo THEN DO:
           lv-tot-pay =  ar-cashl.amt-paid -  ar-cashl.amt-disc .
           FOR EACH bf-cashl OF ar-cash NO-LOCK
               WHERE RECID(bf-cashl) <> RECID(ar-cashl)
                 AND NOT CAN-FIND(FIRST ar-c-memo
                                  {&where-ar-c-memo}
                                    AND ar-c-memo.code EQ bf-cashl.rec_key):
             lv-tot-pay = lv-tot-pay + bf-cashl.amt-paid - bf-cashl.amt-disc.
           END. 
          
           IF lv-tot-pay > ar-cash.check-amt THEN DO:
              cError = "Amount being applied may not exceed the check amount...".
                 
              RETURN .
           END.
        END.
       
        IF  ar-cashl.inv-no = 0 AND  ar-cashl.amt-paid LT 0 THEN DO:
           cError = "Total Applied cannot be negative.".
           
           RETURN .
        END.
        IF ar-cash.posted THEN do:
            cError = "This Cash Receipt has been posted. No updates are allowed!".
            RETURN.
        END.


END.


IF prmAction = "Update" THEN DO:
     FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        ar-cashl.inv-no       =  prminvno      
         ar-cashl.amt-disc    =  prmdisc       
         ar-cashl.amt-paid    =  prmtotl_app   
         ar-cashl.actnum      =  prmactno      
        /* ar-cashl.dscr        =  prmdscr  */. 

         ASSIGN
             prmAction = "View".
END.  

/*********************************delete ******************************/

IF prmAction = "ValidateDelete"  THEN DO:
     FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cash WHERE  ar-cash.company = cocode AND ar-cash.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF ar-cash.posted THEN do:
     cError =  "This Cash Receipt has been posted. No deletion allowed!"  .
     RETURN .
    END.
    

END.


IF prmAction = "DataDelete"  THEN DO:

    FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL ar-cashl THEN
        DELETE ar-cashl .

    FIND LAST ar-cashl WHERE ar-cashl.company = prmComp AND ar-cashl.c-no eq ar-cash.c-no  NO-LOCK NO-ERROR.
    IF AVAIL ar-cashl THEN
        ASSIGN
        prmReckey = ar-cashl.rec_key
        prmAction = "View" .

END.  

IF prmAction = "AddDelete"  THEN DO:

    FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL ar-cashl THEN
        DELETE ar-cashl .
END.  

/*******************************View************************************/


IF prmAction = "View" THEN DO:
  
    /*FIND FIRST ap-inv WHERE  ap-inv.company = cocode AND ap-inv.inv-no = prmInv  NO-LOCK NO-ERROR.*/
         FOR EACH ar-cashl WHERE ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey NO-LOCK :
             
             CREATE ttEnterEditCashReceiptView.
             ASSIGN 
                 ttEnterEditCashReceiptView.invno        = ar-cashl.inv-no 
                 ttEnterEditCashReceiptView.invdt        = STRING(ar-cashl.inv-date)  
                 ttEnterEditCashReceiptView.baldu        = ar-cashl.amt-due  
                 ttEnterEditCashReceiptView.disc         = ar-cashl.amt-disc   
                 ttEnterEditCashReceiptView.totl_app     = ar-cashl.amt-paid
                 ttEnterEditCashReceiptView.cashpy       = calc-paid-amt()
                 ttEnterEditCashReceiptView.actno        = ar-cashl.actnum    
                 ttEnterEditCashReceiptView.actdscr      = display-account()
                 ttEnterEditCashReceiptView.balaftr      = calc-inv-bal() 
                 ttEnterEditCashReceiptView.reckey       = ar-cashl.rec_key .
             
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/*****************calc-amt *******************************************/

