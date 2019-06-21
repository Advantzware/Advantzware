

/*------------------------------------------------------------------------
    File        : arvoidcsh_rprt.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttArVoidCashRegRcptReport NO-UNDO
        FIELD arvoid AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsArVoidCashRegRcptReport FOR ttArVoidCashRegRcptReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsArVoidCashRegRcptReport.

     IF  prmUser       = ? THEN ASSIGN   prmUser       = "".   
     IF  prmAction     = ? THEN ASSIGN   prmAction     = "". 
     IF  prmtrnsdt     = ? THEN ASSIGN   prmtrnsdt     = "". 
     IF  prmperiod     = ? THEN ASSIGN   prmperiod     = 0.
     IF  prmOut        = ? THEN ASSIGN   prmOut        = "". 
     
     
DEFINE VARIABLE tran-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
/*DEF VAR tmp-dir AS cha NO-UNDO.*/
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

assign
 cocode = prmComp 
 v-today = TODAY 
 g_company = cocode
 g_user    = prmUser 
tran-date   =   date(prmtrnsdt) .

 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

    
def buffer xar-cashl for ar-cashl.    
    
def new shared var v-trnum as INT NO-UNDO.    
def var v-postable as log init NO NO-UNDO.
DEF VAR xar-acct AS CHAR NO-UNDO.
DEF VAR xdis-acct AS CHAR NO-UNDO.
def var v-post as logical NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
def TEMP-TABLE xcashl NO-UNDO field recnum as recid.

{sys/form/r-top3w.f}

form ar-cash.check-no at 1 format "zzzzzzz9"
     ar-cash.check-date at 17
     ar-cash.check-amt at 33 format "-ZZ,ZZZ,ZZ9.99"
     ar-cash.cust-no at 59 space(1) cust.name
     with frame report-lines width 132 no-box NO-LABELS STREAM-IO.



        find first period                   
           where period.company eq cocode
           and period.pst     le tran-date
           and period.pend    ge tran-date
           no-lock no-error.
        if avail period then do:
            IF NOT period.pstat THEN DO:
                cError = "Period Already Closed. " .
            END.
        prmperiod = (period.pnum).
        END.
        ELSE DO:
            cError = "No Defined Period Exists..".
            RETURN.
        end.
   


  IF prmAction = "arvoid" THEN DO:

   ASSIGN            
     
      tran-date    = date(prmtrnsdt) 
      tran-period  = prmperiod   
      .
       

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "VoidCshRcpt" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        find first period                   
           where period.company eq cocode
           and period.pst     le tran-date
           and period.pend    ge tran-date
           no-lock no-error.
        if avail period then do:
            IF NOT period.pstat THEN DO:
                cError = "Period Already Closed. " .
            END.
        prmperiod = (period.pnum).
        END.
        ELSE DO:
            cError = "No Defined Period Exists..".
            RETURN.
        end.

       REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN v-trnum       = gl-ctrl.trnum + 1
               gl-ctrl.trnum = v-trnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
     
       RUN run-report.


  IF v-postable THEN DO:
 
    IF prmOut = "Yes" THEN do:      
        RUN post-gl.
        RUN copy-report-to-audit-dir.
             cError = "Posting Complete" .
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE do:
      cError = "No Voided Cash Receipts available for posting...".
      RUN undo-trnum.
  END.


  CREATE ttArVoidCashRegRcptReport.
        ASSIGN ttArVoidCashRegRcptReport.arvoid = vTextFile .

  END.

  

/*****************************************************************************************/
PROCEDURE run-report :
form HEADER
         "Check #" at 1
         "Date" at 17
         "Amount" at 33
         "Customer" at 59 skip
         fill("_",132) format "x(130)" skip
         with STREAM-IO frame f-top width 132 no-box no-labels no-underline.

  ASSIGN
    tmpstore   = fill("_",125).             

 /* {sys/inc/print1.i}*/
  if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

{sys/inc/outprint.i VALUE(lines-per-page)}

    
  ASSIGN
   str-tit  = coname + " - " + loname                                    
   str-tit2 = "A/R VOIDED CASH RECEIPT REGISTER " + STRING(v-trnum)
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3
   v-postable = NO.

display "" with frame r-top.
display "" with frame f-top.


for each ar-cash FIELDS(company cleared posted check-no check-date check-amt
    cust-no) where
    ar-cash.company = cocode and
    ar-cash.cleared = ? and
    ar-cash.posted = YES
    NO-LOCK
    break by ar-cash.check-no:

    find first cust where
         cust.company = cocode and
         cust.cust-no = ar-cash.cust-no
         no-lock no-error.

    display ar-cash.check-no FORMAT "ZZZZZZZZZ9"
            ar-cash.check-date
            ar-cash.check-amt
            ar-cash.cust-no
            cust.name when avail cust
            with frame report-lines width 132 no-box NO-LABELS STREAM-IO.
    down with frame report-lines.
    
    v-postable = YES.
end. /* for each ar-cash record */


END PROCEDURE.
PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF v-trnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

END PROCEDURE.
 
PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.
  
  ASSIGN targetfile = lv-audit-dir + "\AR\AC8\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC8".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
  END.
END PROCEDURE.

PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
 DEF VAR t1 AS DEC NO-UNDO.
 DEF VAR lv-rowid AS ROWID NO-UNDO.

 postit:
 do transaction on error undo, leave:
   
   for each ar-cash EXCLUSIVE-LOCK where
       ar-cash.company = cocode and
       ar-cash.cleared = ? and
       ar-cash.reconciled = NO, 
       FIRST cust WHERE
             cust.company EQ cocode AND
             cust.cust-no EQ ar-cash.cust-no
        EXCLUSIVE-LOCK
        on error undo postit, leave postit:

       assign t1 = 0
              ar-cash.cleared = NO
              ar-cash.reconciled = ?.

       find first bank where
            bank.company = cocode and
            bank.bank-code = ar-cash.bank-code
            no-error.

       if available bank then
       DO:
          bank.bal = bank.bal - ar-cash.check-amt.
          RELEASE bank.
       END.

       EMPTY TEMP-TABLE xcashl.
       
       for each ar-cashl where ar-cashl.c-no = ar-cash.c-no
           EXCLUSIVE-LOCK:

           create xcashl.   

           xcashl.recnum = recid(ar-cashl).   

           IF ar-cashl.inv-no NE 0 AND ar-cashl.on-account EQ NO THEN DO:

              find first ar-inv where
                   ar-inv.company = cocode and
                   ar-inv.inv-no = ar-cashl.inv-no
                   EXCLUSIVE-LOCK no-error.

              if AVAIL ar-inv then
              do:
                 assign
                 ar-inv.paid = ar-inv.paid - ar-cashl.amt-paid - ar-cashl.amt-disc
                 ar-inv.disc-taken = ar-inv.disc-taken + ar-cashl.amt-disc
                 ar-inv.due = ar-inv.due + ar-cashl.amt-paid + ar-cashl.amt-disc.

                 RELEASE ar-inv.
                 
              end. /* if avail ar-inv  */

           END. /*inv-no ne 0 AND ar-cashl.on-account EQ NO*/

           IF ar-cashl.inv-no EQ 0 AND ar-cashl.on-account EQ YES THEN
              cust.on-account = cust.on-account - ar-cashl.amt-paid.

           CREATE gltrans.
           ASSIGN
             t1 = t1 + ar-cashl.amt-paid + ar-cashl.amt-disc
             gltrans.company   = cocode
             gltrans.actnum    = ar-cashl.actnum
             gltrans.jrnl      = "CASHRVD"
             gltrans.tr-dscr   = "VOID " + cust.cust-no + " " +
                                 STRING(ar-cash.check-no,"9999999999") +
                                 " Inv# " + STRING(ar-cashl.inv-no)
             gltrans.tr-date   = tran-date
             gltrans.tr-amt    = -1 * ar-cashl.amt-paid
             gltrans.period    = tran-period
             gltrans.trnum     = v-trnum
             /*ar-cashl.amt-paid = -1 * ar-cashl.amt-paid*/ .

           RELEASE gltrans.

           IF ar-cashl.amt-disc NE 0 THEN DO:
             CREATE gltrans.
             ASSIGN
              gltrans.company = cocode
              gltrans.actnum  = xdis-acct
              gltrans.jrnl    = "CRDISVD"
              gltrans.tr-dscr = "VOID " + cust.cust-no + " " +
                                STRING(ar-cash.check-no,"9999999999") +
                                " Inv# " + STRING(ar-cashl.inv-no)
              gltrans.tr-date = tran-date
              gltrans.tr-amt  = -1 * ar-cashl.amt-disc
              gltrans.period  = tran-period
              gltrans.trnum   = v-trnum.
            
             RELEASE gltrans.

             CREATE ar-ledger.
             ASSIGN
              ar-ledger.company  = cocode
              ar-ledger.cust-no  = ar-cash.cust-no
              ar-ledger.amt      = -1 * ar-cashl.amt-disc
              ar-ledger.ref-num  = "DISC VD" +
                                   STRING(ar-cash.check-no,"9999999999") +
                                   "-" + STRING(ar-cashl.line,"9999999999")
              ar-ledger.ref-date = ar-cash.check-date
              ar-ledger.tr-date  = tran-date
              ar-ledger.tr-num   = v-trnum.

             RELEASE ar-ledger.
           END.
       end. /* for each ar-cashl record */
       
       cust.acc-bal = cust.acc-bal + t1.

       IF cust.acc-bal GE cust.hibal THEN
          ASSIGN
             cust.hibal      = cust.acc-bal
             cust.hibal-date = tran-date.

       IF t1 NE 0 THEN DO:
          FIND gltrans WHERE ROWID(gltrans) EQ lv-rowid NO-ERROR.
          IF NOT AVAIL gltrans THEN DO:
            CREATE gltrans.
            ASSIGN
             gltrans.company = cocode
             gltrans.actnum  = xar-acct
             gltrans.jrnl    = "CASHRVD"
             gltrans.tr-dscr = "CASH RECEIPTS VOID"
             gltrans.tr-date = tran-date
             gltrans.period  = tran-period
             gltrans.trnum   = v-trnum
             lv-rowid        = ROWID(gltrans).
          END.
          gltrans.tr-amt = gltrans.tr-amt + t1.

          RELEASE gltrans.
       END.

       create ar-ledger.
       assign
        ar-ledger.company = cocode
        ar-ledger.cust-no = ar-cash.cust-no
        ar-ledger.amt = -1 * ar-cash.check-amt
        ar-ledger.ref-num = "VOIDED CHK# " + STRING(ar-cash.check-no,"9999999999")
        ar-ledger.ref-date = ar-cash.check-date
        ar-ledger.tr-date = tran-date
        ar-ledger.tr-num = v-trnum.

       RELEASE ar-ledger.

       for each xcashl,
           first ar-cashl where recid(ar-cashl) eq xcashl.recnum
           no-lock:
           
          find last xar-cashl where xar-cashl.c-no eq ar-cashl.c-no
              use-index c-no no-lock no-error.
          x = if avail xar-cashl then xar-cashl.line else 0.
          
          create xar-cashl.

          BUFFER-COPY ar-cashl EXCEPT LINE amt-disc amt-due amt-paid rec_key
                   TO xar-cashl 
             assign
              xar-cashl.line       = x + 1
              xar-cashl.amt-due    = ar-cashl.amt-due
              xar-cashl.amt-disc   = -(ar-cashl.amt-disc)
              
              xar-cashl.amt-paid   = -(ar-cashl.amt-paid).

          CREATE reftable.
          ASSIGN
            reftable.reftable = "ARCASHLVDDATE"
            reftable.rec_key = xar-cashl.rec_key
            reftable.CODE = STRING(tran-date,"99/99/9999").
          RELEASE reftable.

          RELEASE xar-cashl.
       end.  /* for each xcashl */
   end. /* for each ar-cash record */

 end. /* postit */

 FIND CURRENT cust NO-LOCK NO-ERROR.
 FIND CURRENT ar-cashl NO-LOCK NO-ERROR.

END PROCEDURE.
