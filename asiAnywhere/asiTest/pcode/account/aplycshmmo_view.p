
/*------------------------------------------------------------------------
    File        : aplycshmmo_view.p
    Purpose     : Accounts Receivable

     Main File   : 
    Syntax      :

    Description : Return a Dataset of all Invpice

    Author(s)   : 
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttApplyReApplyCashMemoView NO-UNDO
    FIELD cust      AS CHAR
    FIELD inv       AS INT
    FIELD invdt     AS CHAR
    FIELD bal       AS DEC
    FIELD app       AS DEC
    FIELD disc      AS DEC
    
    
    FIELD ext          AS CHAR
    FIELD reckey       AS CHAR

  
    .

DEFINE DATASET dsApplyReApplyCashMemoView FOR ttApplyReApplyCashMemoView.
    

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcust         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prminv          AS INT   NO-UNDO.
DEFINE INPUT PARAMETER prminvdt        AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmbal          AS DEC  NO-UNDO.
DEFINE INPUT PARAMETER prmapp          AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmdisc         AS DEC   NO-UNDO.
DEFINE INPUT PARAMETER prmOut          AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER prmReckey       AS CHARACTER  NO-UNDO.

          
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsApplyReApplyCashMemoView .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.


     FOR EACH ttApplyReApplyCashMemoView:
        DELETE ttApplyReApplyCashMemoView .
    END.

IF prmAction     = ?  THEN ASSIGN prmAction     = "Select".
IF prmComp       = ?  THEN ASSIGN prmComp       = "".
IF prmUser       = ?  THEN ASSIGN prmUser       = "".
IF prmcust       = ?  THEN ASSIGN prmcust       = "".
IF prminv        = ?  THEN ASSIGN prminv        = 0.
IF prminvdt      = ?  THEN ASSIGN prminvdt      = "".
IF prmbal        = ?  THEN ASSIGN prmbal        = 0.
IF prmapp        = ?  THEN ASSIGN prmapp        = 0.
IF prmdisc       = ?  THEN ASSIGN prmdisc       = 0.
IF prmOut        = ?  THEN ASSIGN prmOut        = "".
   


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
             

DEF VAR lv-app-amt AS DEC NO-UNDO.
DEF VAR lv-unapp-amt AS DEC NO-UNDO.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR lv-tmp-amt AS DEC NO-UNDO.
DEF VAR lv-new-recid AS RECID NO-UNDO.
DEF VAR ll-reapply AS LOG NO-UNDO.
DEF VAR hold-inv-no LIKE ar-cashl.inv-no NO-UNDO.

DEF BUFFER bf-cashl FOR ar-cashl.

DEF TEMP-TABLE tt-cashl LIKE ar-cashl.
    DEF VAR lv-rowid AS ROWID NO-UNDO.
    DEF VAR li-next-line AS INT NO-UNDO.
    DEF VAR ld-onacct AS DEC NO-UNDO.    
    DEF VAR lv-inv-no LIKE ar-inv.inv-no NO-UNDO.

DEF VAR ld-amt-due AS DEC NO-UNDO.
  DEF VAR ld-amt-disc AS DEC NO-UNDO.
  DEF VAR ld-prev-paid AS DEC NO-UNDO.
  DEF VAR ld-prev-disc AS DEC NO-UNDO.


IF prmAction = "SelectGrid" THEN DO:
    
    FIND FIRST ar-cash WHERE ar-cash.rec_key = prmOut 
        AND ar-cash.company EQ cocode NO-LOCK NO-ERROR.
    FOR EACH ar-cashl OF ar-cash NO-LOCK:

             CREATE ttApplyReApplyCashMemoView.
             ASSIGN 
                 ttApplyReApplyCashMemoView.inv         = ar-cashl.inv-no 
                 ttApplyReApplyCashMemoView.invdt       = string(ar-cashl.inv-date)
                 ttApplyReApplyCashMemoView.bal         = ar-cashl.amt-due 
                 ttApplyReApplyCashMemoView.app         = ar-cashl.amt-paid
                 ttApplyReApplyCashMemoView.disc        = ar-cashl.amt-disc
                 ttApplyReApplyCashMemoView.reckey      = ar-cashl.rec_key .


             
      END. /*FOR EACH ar-mcashl  */
      
END. /*IF prmAction = "Select" THEN DO:*/


/**************Update *************************************************/

IF prmAction = "ValidateUpdate" THEN DO:
   /* FIND FIRST ar-cash WHERE ar-cash.rec_key = prmOut NO-LOCK NO-ERROR.
    FIND FIRST ar-cashl OF ar-cash NO-LOCK NO-ERROR.*/
    FIND FIRST ar-cash WHERE ar-cash.rec_key = prmOut NO-LOCK NO-ERROR.
    FIND FIRST ar-cashl WHERE ar-cashl.company = cocode AND
        ar-cashl.rec_key = prmReckey NO-LOCK NO-ERROR.

    lv-inv-no = INT(prminv).
    
    FIND bf-cashl WHERE ROWID(bf-cashl) EQ ROWID(ar-cashl) NO-LOCK NO-ERROR.

    FIND FIRST ar-inv NO-LOCK
        WHERE ar-inv.company EQ bf-cashl.company
          AND ar-inv.posted  EQ YES
          AND ar-inv.cust-no EQ bf-cashl.cust-no
          AND ar-inv.inv-no  EQ lv-inv-no
        USE-INDEX posted NO-ERROR.
    IF NOT AVAIL ar-inv OR lv-inv-no EQ 0 THEN DO:
      cError = "Invalid invoice#, try help..." .
      RETURN.
    END.

    FIND FIRST bf-cashl OF ar-cash WHERE bf-cashl.inv-no = 0 NO-LOCK NO-ERROR.
    ld-onacct = IF AVAIL bf-cashl THEN bf-cashl.amt-paid ELSE 0.

    lv-app-amt = 0.
    FOR EACH bf-cashl OF ar-cash
        WHERE bf-cashl.inv-no NE 0
          AND ROWID(bf-cashl) NE ROWID(ar-cashl)
        NO-LOCK:
      lv-app-amt = lv-app-amt + bf-cashl.amt-paid - bf-cashl.amt-disc.
    END.
    lv-unapp-amt = ar-cash.check-amt - lv-app-amt.

    IF (lv-unapp-amt GE 0 AND ar-cash.check-amt GT 0 AND
        DEC(prmapp) - DEC(prmdisc)
                                                GT lv-unapp-amt) OR
       (ar-cash.check-amt LT 0 AND
        DEC(prmapp) - DEC(prmdisc)
                                                LT lv-unapp-amt) THEN DO:
      cError = "Total applied amount cannot be greater/less than the check amount...".
      RETURN .
    END.
END.


IF prmAction = "Update" THEN DO:
  /*  FIND FIRST ar-cash WHERE ar-cash.rec_key = prmOut NO-LOCK NO-ERROR.
    FIND FIRST ar-cashl OF ar-cash NO-LOCK NO-ERROR. */
    FIND FIRST ar-cash WHERE ar-cash.rec_key = prmOut NO-LOCK NO-ERROR.
    FIND FIRST ar-cashl WHERE ar-cashl.company = cocode AND
        ar-cashl.rec_key = prmReckey EXCLUSIVE-LOCK NO-ERROR.

    
   FOR EACH tt-cashl:
    DELETE tt-cashl.
   END.
  CREATE tt-cashl.
  BUFFER-COPY ar-cashl TO tt-cashl.
  lv-rowid = ROWID(ar-cashl).

  ASSIGN
       ar-cashl.inv-no    = prminv 
       ar-cashl.inv-date  = date(prminvdt)
       ar-cashl.amt-due   = prmbal  
       ar-cashl.amt-paid  = prmapp  
       ar-cashl.amt-disc  = prmdisc .


    ASSIGN
   ld-amt-due   = DEC(prmbal)
   ld-amt-disc  = DEC(prmdisc)
   ld-prev-paid = IF ar-cashl.inv-no NE 0 THEN ar-cashl.amt-paid ELSE 0
   ld-prev-disc = IF ar-cashl.inv-no NE 0 THEN ar-cashl.amt-disc ELSE 0.

    IF tt-cashl.inv-no NE ar-cashl.inv-no AND
     NOT ar-cashl.on-account            THEN DO:

    {ar/ar-oreg.i tt-cashl -1}
      ar-cashl.on-account = YES.
     END.

    lv-tmp-amt = 0.

    FOR EACH bf-cashl OF ar-cash NO-LOCK:
        lv-tmp-amt = lv-tmp-amt + bf-cashl.amt-paid.
    END.
    IF ar-cash.check-amt NE lv-tmp-amt THEN DO:
        lv-tmp-amt = 0.
  FOR EACH bf-cashl OF ar-cash WHERE NOT ar-cashl.memo NO-LOCK:
    lv-tmp-amt = lv-tmp-amt + bf-cashl.amt-paid.
  END.

  IF ar-cash.check-amt NE lv-tmp-amt THEN DO:
    FIND FIRST bf-cashl OF ar-cash WHERE bf-cashl.inv-no EQ 0 EXCLUSIVE NO-ERROR.  
    IF NOT AVAIL bf-cashl THEN do:
      FOR EACH bf-cashl OF ar-cash NO-LOCK BY bf-cashl.LINE DESCENDING:
        li-next-line = bf-cashl.LINE.
        LEAVE.
      END.

      CREATE bf-cashl.
      BUFFER-COPY ar-cashl EXCEPT rec_key TO bf-cashl
      ASSIGN
       bf-cashl.LINE     = li-next-line + 1
       bf-cashl.inv-no   = 0
       bf-cashl.amt-paid = 0
       bf-cashl.amt-disc = 0
       bf-cashl.amt-due  = 0.

      find first ar-ctrl where ar-ctrl.company = ar-cash.company no-lock no-error.

      find first bank where bank.company = ar-cash.company and
                             bank.bank-code = ar-cash.bank-code no-lock no-error.
      if avail bank THEN do:
        find first account where account.company = ar-cash.company and
                                   account.actnum  = bank.actnum no-lock no-error.
        assign bf-cashl.actnum = bank.actnum.
      end.
      ELSE do:
        if ar-cash.check-no ge 90000000 AND ar-cash.check-no le 99999999 
        THEN find first account where account.company = ar-cash.company and
                                 account.actnum  = ar-ctrl.sales no-lock no-error.
        ELSE find first account where account.company = ar-cash.company and
                                  account.actnum  = ar-ctrl.cash-act no-lock no-error.
        if avail account THEN assign bf-cashl.actnum = account.actnum.
      end.
    END.
  
    bf-cashl.amt-paid = bf-cashl.amt-paid + (ar-cash.check-amt - lv-tmp-amt).
  END.
END.

         ASSIGN
             prmAction = "View".
END.  



/*******************************View************************************/


IF prmAction = "View" THEN DO:
  
   FIND FIRST ar-cash WHERE ar-cash.rec_key = prmOut NO-LOCK NO-ERROR.
    FIND FIRST ar-cashl WHERE ar-cashl.company = cocode AND
        ar-cashl.rec_key = prmReckey NO-LOCK NO-ERROR.
             CREATE ttApplyReApplyCashMemoView.
             ASSIGN 
                 ttApplyReApplyCashMemoView.inv         = ar-cashl.inv-no 
                 ttApplyReApplyCashMemoView.invdt       = string(ar-cashl.inv-date)
                 ttApplyReApplyCashMemoView.bal         = ar-cashl.amt-due 
                 ttApplyReApplyCashMemoView.app         = ar-cashl.amt-paid
                 ttApplyReApplyCashMemoView.disc        = ar-cashl.amt-disc
                 ttApplyReApplyCashMemoView.reckey      = ar-cashl.rec_key .
            
END. /*IF prmAction = "Select" THEN DO:*/


/*****************calc-amt *******************************************/

