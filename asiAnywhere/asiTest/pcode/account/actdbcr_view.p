
/*------------------------------------------------------------------------
    File        : actdbcr_view.p
    Purpose     : Accounts Receivable

     Main File   : 
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttActWriteDebitCreditView NO-UNDO
    FIELD custno       AS CHAR
    FIELD invno        AS INT
    FIELD invdt        AS CHAR
    FIELD baldu        AS DEC
    FIELD disc         AS DEC
    FIELD totl_app     AS DECIMAL
    FIELD actno        AS CHAR
    FIELD actdscr      AS CHAR
    FIELD memodscr     AS CHAR
    
    FIELD ext          AS CHAR
    FIELD reckey       AS CHAR

  
    .

DEFINE DATASET dsActWriteDebitCreditView FOR ttActWriteDebitCreditView.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustno       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prminvno        AS INT        NO-UNDO.
DEFINE INPUT PARAMETER prminvdt        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmbaldu        AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmdisc         AS DEC        NO-UNDO.
DEFINE INPUT PARAMETER prmtotl_app     AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER prmactno        AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmactdscr      AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmmemodscr     AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmOut          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsActWriteDebitCreditView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttActWriteDebitCreditView:
        DELETE ttActWriteDebitCreditView .
    END.

IF prmAction      = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp        = ?  THEN ASSIGN prmComp       = "".
IF prmUser        = ?  THEN ASSIGN prmUser       = "".
IF prmcustno      = ?  THEN ASSIGN prmcustno     = "".
IF prminvno       = ?  THEN ASSIGN prminvno      = 0.
IF prminvdt       = ?  THEN ASSIGN prminvdt      = "".
IF prmbaldu       = ?  THEN ASSIGN prmbaldu      = 0.
IF prmdisc        = ?  THEN ASSIGN prmdisc       = 0.
IF prmtotl_app    = ?  THEN ASSIGN prmtotl_app   = 0.
IF prmactno       = ?  THEN ASSIGN prmactno      = "".
IF prmactdscr     = ?  THEN ASSIGN prmactdscr    = "".
IF prmmemodscr    = ?  THEN ASSIGN prmmemodscr   = "".
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

find first sys-ctrl where
     sys-ctrl.company eq cocode and
     sys-ctrl.name    eq "ARMEMO"
     no-lock no-error.

if not avail sys-ctrl then
do transaction:
   create sys-ctrl.
   assign
       sys-ctrl.company = cocode
       sys-ctrl.name    = "ARMEMO"
       sys-ctrl.descrip = "Credit/Debit Memo Form".
   cError = "System control record NOT found.  Which Memo format should print?" .
     update sys-ctrl.char-fld.
end.

v-armemo-log = sys-ctrl.log-fld.

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
    FOR EACH ar-cashl WHERE ar-cashl.c-no eq ar-cash.c-no NO-LOCK:

             CREATE ttActWriteDebitCreditView.
             ASSIGN 
                 ttActWriteDebitCreditView.invno        = ar-cashl.inv-no 
                 ttActWriteDebitCreditView.invdt        = STRING(ar-cashl.inv-date)  
                 ttActWriteDebitCreditView.baldu        = ar-cashl.amt-due  
                 ttActWriteDebitCreditView.disc         = ar-cashl.amt-disc   
                 ttActWriteDebitCreditView.totl_app     = ar-cashl.amt-paid
                 ttActWriteDebitCreditView.actno        = ar-cashl.actnum    
                 ttActWriteDebitCreditView.actdscr      = display-account()
                 ttActWriteDebitCreditView.memodscr     = ar-cashl.dscr 
                 ttActWriteDebitCreditView.reckey       = ar-cashl.rec_key
                 
                .
             
            
      END. /*FOR EACH ar-cashl  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/********************************Add **********************************/

IF prmAction = "ValidateAdd" THEN DO:
    
      FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
      /*FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmcustno  NO-LOCK NO-ERROR.*/

     IF ar-cash.posted THEN do:
     cError = "This Cash Receipt has been posted. No addings are allowed!".
     RETURN.
     END.

     IF NOT CAN-FIND(FIRST account
                    WHERE account.company EQ g_company
                      AND account.actnum  EQ prmactno
                      AND account.TYPE    NE "T") THEN DO:
      cError = "Invalid GL Account Number".
      RETURN.
    END.   

    IF DEC(prmdisc) LT 0 THEN DO:
      cError = "Discount may not be negative...".
      RETURN.
    END.
  
    IF DEC(prmdisc) EQ 0 AND DEC(prmtotl_app) EQ 0 THEN DO:
      cError = "The CREDIT or DEBIT AMOUNT must be greater than 0...".
      RETURN.
    END.

    RELEASE ar-inv.

    IF INT(prminvno) NE 0 THEN DO:
      FIND FIRST ar-inv NO-LOCK
          WHERE ar-inv.company EQ cocode 
            AND ar-inv.posted  EQ YES
            AND ar-inv.cust-no EQ ar-cash.cust-no
            AND ar-inv.inv-no  EQ INT(prminvno)
          NO-ERROR.
      IF NOT AVAIL ar-inv THEN DO:
        cError = "Invalid Invoice Number, try help...".           
        RETURN.
      END.
    END.
  
    
    
    

END.  /* end of validate add*/

/***********************check add user******************************************/


IF prmAction = "AddNew" THEN DO:

     FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
    
    
     DEF VAR li-next-line AS INT NO-UNDO.
     DEF VAR lv-dscr LIKE ar-cashl.dscr NO-UNDO.

     /* Code placed here will execute PRIOR to standard behavior. */
     FOR EACH bf-cashl OF ar-cash NO-LOCK BY bf-cashl.LINE DESCENDING:
         ASSIGN
             li-next-line = bf-cashl.LINE
             lv-dscr      = bf-cashl.dscr.
         LEAVE.
     END.

  /* Dispatch standard ADM method.                             */
  CREATE ar-cashl .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ar-cashl.company = ar-cash.company
         ar-cashl.c-no = ar-cash.c-no
         ar-cashl.LINE = li-next-line + 1
         ar-cashl.cust-no = ar-cash.cust-no
         ar-cashl.check-no = STRING(ar-cash.check-no,"9999999999")
         ar-cashl.dscr = lv-dscr
         ar-cashl.memo = YES.

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
  ASSIGN
      prmReckey = ar-cashl.rec_key 
      prmAction = "View"  .

  

END.

IF prmAction = "Add" THEN DO:
    
    FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR. 

     ASSIGN
         ar-cashl.inv-no      =  prminvno      
         ar-cashl.inv-date    =  date(prminvdt)
         ar-cashl.amt-due     =  prmbaldu      
         ar-cashl.amt-disc    =  prmdisc       
         ar-cashl.amt-paid    =  prmtotl_app   
         ar-cashl.actnum      =  prmactno      
         ar-cashl.dscr        =  prmmemodscr  . 


     ASSIGN
      prmReckey = ar-cashl.rec_key 
      prmAction = "View" .
                               
                               

END. 


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
    RELEASE ar-inv.

    IF INT(prminvno) NE 0 THEN DO:
      FIND FIRST ar-inv NO-LOCK
          WHERE ar-inv.company EQ cocode 
            AND ar-inv.posted  EQ YES
            AND ar-inv.cust-no EQ ar-cash.cust-no
            AND ar-inv.inv-no  EQ INT(prminvno)
          NO-ERROR.
      IF NOT AVAIL ar-inv THEN DO:
        cError = "Invalid Invoice Number, try help...".           
        RETURN.
      END.
    END.

    IF ar-cash.posted THEN do:
     cError = "This Cash Receipt has been posted. No updating allowed!".
     RETURN.
     END.

     IF NOT CAN-FIND(FIRST account
                    WHERE account.company EQ g_company
                      AND account.actnum  EQ prmactno
                      AND account.TYPE    NE "T") THEN DO:
      cError = "Invalid GL Account Number".
      RETURN.
    END.   

    IF DEC(prmdisc) LT 0 THEN DO:
      cError = "Discount may not be negative...".
      RETURN.
    END.
  
    IF DEC(prmdisc) EQ 0 AND DEC(prmtotl_app) EQ 0 THEN DO:
      cError = "The CREDIT or DEBIT AMOUNT must be greater than 0...".
      RETURN.
    END.

    

          
END.


IF prmAction = "Update" THEN DO:
     FIND FIRST ar-cash WHERE ar-cash.rec_key EQ prmOut NO-LOCK NO-ERROR.
     FIND FIRST ar-cashl WHERE  ar-cashl.company = cocode AND ar-cashl.rec_key = prmReckey  EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN
        ar-cashl.inv-no            =  prminvno       
        ar-cashl.amt-paid          =  prmtotl_app     
        ar-cashl.actnum            =  prmactno
        ar-cashl.dscr              =  prmmemodscr.   

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
             
             CREATE ttActWriteDebitCreditView.
             ASSIGN 
                 ttActWriteDebitCreditView.invno        = ar-cashl.inv-no 
                 ttActWriteDebitCreditView.invdt        = STRING(ar-cashl.inv-date)  
                 ttActWriteDebitCreditView.baldu        = ar-cashl.amt-due  
                 ttActWriteDebitCreditView.disc         = ar-cashl.amt-disc   
                 ttActWriteDebitCreditView.totl_app     = ar-cashl.amt-paid
                 ttActWriteDebitCreditView.actno        = ar-cashl.actnum   
                 ttActWriteDebitCreditView.actdscr      = display-account()
                 ttActWriteDebitCreditView.memodscr     = ar-cashl.dscr 
                 ttActWriteDebitCreditView.reckey       = ar-cashl.rec_key .
             
            
      END. /*FOR EACH buff-cust  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/*****************calc-amt *******************************************/

