

/*------------------------------------------------------------------------
    File        : cashrcpt_rept.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttEditPostCashReceiptReport NO-UNDO
        FIELD cash AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsEditPostCashReceiptReport FOR ttEditPostCashReceiptReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmsort          AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEditPostCashReceiptReport.

     IF   prmUser       = ? THEN ASSIGN    prmUser       = "".   
     IF   prmAction     = ? THEN ASSIGN    prmAction     = "". 
     IF   prmtrnsdt     = ? THEN ASSIGN    prmtrnsdt     = "". 
     IF   prmperiod     = ? THEN ASSIGN    prmperiod     = 0.
     IF   prmbegcust    = ? THEN ASSIGN    prmbegcust    = "". 
     IF   prmbegdt      = ? THEN ASSIGN    prmbegdt      = "".   
     IF   prmendcust    = ? THEN ASSIGN    prmendcust    = "". 
     IF   prmenddt      = ? THEN ASSIGN    prmenddt      = "". 
     IF   prmsort       = ? THEN ASSIGN    prmsort       = "". 
     IF   prmOut        = ? THEN ASSIGN    prmOut        = "".
     
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE end_cust   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_date   AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE tran-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer" NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
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
TRAN-date = TODAY.
    
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.

def var v-from-date  as date format "99/99/9999" init today.
def var v-to-date    as date format "99/99/9999" init today.
def var xtrnum as INT NO-UNDO.
def var xar-acct  as CHAR NO-UNDO.
def var xdis-acct as CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR ld-curr AS DEC NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-post NO-UNDO FIELD row-id AS ROWID
                               FIELD ex-rate LIKE currency.ex-rate INIT 1
                               FIELD curr-amt LIKE ar-cash.check-amt
                               FIELD actnum LIKE account.actnum.

  {sys/inc/postdate.i}
  find first sys-ctrl where
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "AUDITDIR"
        no-lock no-error.
   
   if not avail sys-ctrl THEN DO:
      create sys-ctrl.
      assign
         sys-ctrl.company = cocode
         sys-ctrl.name    = "AUDITDIR"
         sys-ctrl.descrip = "Audit Trails directory"
         sys-ctrl.char-fld = ".\AUDIT TRAILS".
   end.
  
   lv-audit-dir = sys-ctrl.char-fld.
  
   IF LOOKUP(SUBSTR(lv-audit-dir,LENGTH(lv-audit-dir),1),"/,\") > 0 THEN
      lv-audit-dir = SUBSTR(lv-audit-dir,1,LENGTH(lv-audit-dir) - 1).
  
   RELEASE sys-ctrl.


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
   


  IF prmAction = "cash" THEN DO:


   ASSIGN            
                     
      begin_cust   = prmbegcust
      begin_date   = date(prmbegdt)  
      end_cust     = prmendcust  
      end_date     = date(prmenddt)  
      tran-date    = date(prmtrnsdt) 
      tran-period  = prmperiod   
      rd_sort      = prmsort .
       

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "PostCashRcpt" +
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
        ASSIGN xtrnum        = gl-ctrl.trnum + 1
               gl-ctrl.trnum = xtrnum.
        RELEASE gl-ctrl. 
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
       END.
     
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
      cError = "No Cash Receipts available for posting...".
      RUN undo-trnum.
  END.


  CREATE ttEditPostCashReceiptReport.

    ASSIGN ttEditPostCashReceiptReport.cash = vTextFile .

  END.

/*****************************************************************************************/
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */

{sys/FORM/r-top3w.f}

def var dsc     as dec format "->>,>>>,>>9.99".
def var net-cr  as dec format "->>,>>>,>>9.99".
def var save_id as recid.
def var time_stamp as ch.
def var qfirst as l.
def var post as log format "Yes/No"
                            label "   Post to G/L & Customer files?   " init no.
def var g1 as dec format "->>,>>>,>>9.99".
def var t1 like g1.
def var g2 like g1.
def var t3 like g1.
def var v1 like g1.
def var v2 like g1.
def var t2 like g1.


def var v-amt-due-sub as dec format "->,>>>,>>9.99" no-undo.
def var v-amt-due-tot as dec format "->,>>>,>>9.99" no-undo.
def var v-amt-paid-sub as dec format "->,>>>,>>9.99" no-undo.
def var v-amt-paid-tot as dec format "->,>>>,>>9.99" no-undo.
def var v-disc-sub as dec format "->>>,>>9.99" no-undo.
def var v-disc-tot as dec format "->>>,>>9.99" no-undo.
def var v-on-act-amt as dec format "->>>>>>9.99" no-undo.
def var v-on-act-sub as dec format "->>>>>>9.99" no-undo.
def var v-on-act-tot as dec format "->>>>>>9.99" no-undo.
def var archk as dec format ">>>>>>>>>>".
def var sort-by-cust as log init yes format "Customer/Sequence" no-UNDO.
/*
DEF VAR tmp-dir AS cha NO-UNDO. 
DEF VAR str-tit AS cha FORM "x(50)" NO-UNDO.
DEF VAR str-tit2 AS cha FORM "x(50)" NO-UNDO.
DEF VAR str-tit3 AS cha FORM "x(50)" NO-UNDO. 
DEF VAR coname AS cha NO-UNDO. 
DEF VAR loname AS cha NO-UNDO.   */


               
assign sort-by-cust = rd_sort = "customer"
       v-from-date  = begin_date
       v-to-date    = END_date
       .

form header
"CUSTOMER      NAME                   CHECK       DATE     CASH       INVOICE         AMOUNT       AMOUNT     DISCOUNT     ON ACCOUNT"
"   #                                 NUMBER               RECEIVED   NUMBER          DUE          APPLIED                   PAYMENTS"
skip fill("_",132) format "x(132)"
    with no-labels no-box no-underline frame f-top page-top WIDTH 200 STREAM-IO.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}



/* str-tit3 = "Period " + STRING(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (STRING(period.pst) + " to " + STRING(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}. */
  assign
   str-tit  = coname + " - " + loname
   str-tit2 = "CASH RECEIPTS  -  EDIT REGISTER " + string(xtrnum)
   str-tit3 = "Period " + string(tran-period,"99") + " - " + string(tran-date) 
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
   str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3.

  assign v-amt-due-tot = 0
         v-amt-paid-tot = 0
         v-disc-tot = 0
         v-on-act-tot = 0.
  
  DISPLAY "" with frame r-top.
  DISPLAY "" with frame f-top.

  EMPTY TEMP-TABLE tt-post.

  if sort-by-cust then do:
    {ar/ar-creg.i cust-no 1}
  end.
  else do:
    {ar/ar-creg.i c-no 2}
  end.

  ASSIGN
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
              "Summary by Account".
   x = (132 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3.

  page.

  format header
    "ACCOUNT                                  DATE   CUSTOMER   MEMO #    "
    "LINE DESCRIPTION                QTY   UNIT PRICE           AMOUNT" skip
    fill("_",132) format "x(132)"
    with no-labels no-box no-underline frame f-top2 page-top WIDTH 200 STREAM-IO.

  display "" with frame f-top2.
      
  g2 = 0.

  FOR EACH tt-post,
      FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id NO-LOCK,
      EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK
      BREAK BY ar-cashl.actnum BY ar-cashl.c-no:

    IF FIRST-OF(ar-cashl.actnum) THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ cocode
            AND account.actnum  EQ ar-cashl.actnum
          NO-ERROR.
      IF AVAIL account THEN
        PUT ar-cashl.actnum + " - " + account.dscr FORMAT "x(39)".
      ELSE
        PUT ar-cashl.actnum.
    END.
    v-postable = YES.

    archk = ar-cash.check-no.

    PUT ar-cash.check-date AT 41        SPACE(1)
        ar-cash.cust-no                 SPACE(1)
        archk                           SPACE(1)
        ar-cashl.line   FORMAT ">>>>"   SPACE(1)
        ar-cashl.dscr   FORMAT "x(20)"  SPACE(1)
        ar-cashl.amt-paid - ar-cashl.amt-disc FORMAT "->,>>>,>>9.99" TO 132
        SKIP.

    ACCUM ar-cashl.amt-paid - ar-cashl.amt-disc (TOTAL BY ar-cashl.actnum).

    IF LAST-OF(ar-cashl.actnum) THEN DO:
      PUT "** TOTAL "                           TO 116
          (ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid - ar-cashl.amt-disc)
                       FORMAT "->,>>>,>>9.99"   TO 132
          SKIP(1).

      g2 = g2 +
           (ACCUM TOTAL BY ar-cashl.actnum ar-cashl.amt-paid - ar-cashl.amt-disc).
    END.
  END.

  FOR EACH tt-post WHERE tt-post.actnum NE "",
      FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id NO-LOCK
      BREAK BY tt-post.actnum BY ar-cash.c-no:

    IF FIRST-OF(tt-post.actnum) THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ cocode
            AND account.actnum  EQ tt-post.actnum
          NO-ERROR.
      IF AVAIL account THEN
        PUT tt-post.actnum + " - " + account.dscr FORMAT "x(39)".
      ELSE
        PUT tt-post.actnum.
    END.

    archk = ar-cash.check-no.

    PUT ar-cash.check-date AT 41        SPACE(1)
        ar-cash.cust-no                 SPACE(1)
        archk                           SPACE(1)
        0               FORMAT ">>>>"   SPACE(1)
        ""              FORMAT "x(20)"  SPACE(1)
        tt-post.curr-amt - ar-cash.check-amt FORMAT "->,>>>,>>9.99" TO 132
        SKIP.

    ACCUM tt-post.curr-amt - ar-cash.check-amt (TOTAL BY tt-post.actnum).

    IF LAST-OF(tt-post.actnum) THEN DO:
      PUT "** TOTAL "                           TO 116
          (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt)
                        FORMAT "->,>>>,>>9.99"  TO 132
          SKIP(1).

      g2 = g2 +
           (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).
    END.
  END.

  IF v-postable THEN
    DISPLAY "***** TOTAL FOR ALL ACCOUNTS "   TO 116
            g2 FORMAT "->,>>>,>>9.99"         TO 132
        WITH NO-LABELS NO-BOX STREAM-IO WIDTH 200.


END PROCEDURE.
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR t1 AS DEC NO-UNDO.
  DEF VAR xar-cashl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li-iter AS INT NO-UNDO.

  DEF BUFFER b-cashl FOR ar-cashl.
  DEF BUFFER ar-c-memo FOR reftable.

 DO WHILE CAN-FIND(FIRST tt-post) AND li-iter LE 100000:
  li-iter = li-iter + 1.

  RELEASE tt-post.

  FOR EACH tt-post,
      FIRST ar-cash WHERE ROWID(ar-cash) EQ tt-post.row-id EXCLUSIVE
      BREAK BY tt-post.actnum
      TRANSACTION:

    FIND FIRST bank
        WHERE bank.company   EQ cocode
          AND bank.bank-code EQ ar-cash.bank-code
        EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL bank THEN NEXT.

    FIND FIRST cust
        {sys/ref/cust.w}
          AND cust.cust-no EQ ar-cash.cust-no
        EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL cust THEN NEXT.

    ASSIGN
     xar-cashl = bank.actnum
     bank.bal  = bank.bal + tt-post.curr-amt
     t1        = 0.

    FOR EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no:
      ar-cashl.posted = YES.
          
      RELEASE b-cashl.

      IF ar-cashl.inv-no NE 0 AND ar-cashl.on-account EQ NO THEN DO:
        FIND FIRST ar-inv
            WHERE ar-inv.company EQ cocode
              AND ar-inv.inv-no  EQ ar-cashl.inv-no
            NO-ERROR.
        IF AVAIL ar-inv THEN DO:
          ASSIGN
           ar-inv.disc-taken = ar-inv.disc-taken + ar-cashl.amt-disc
           ar-inv.paid       = ar-inv.paid + ar-cashl.amt-paid
           ar-inv.due        = ar-inv.due - ar-cashl.amt-paid
           ar-inv.pay-date   = ar-cash.check-date.

          IF ar-inv.due LE 0 THEN DO:
            RUN sys/inc/daytopay.p (RECID(ar-cashl)).
            cust.num-inv = cust.num-inv + 1.
          END.

          FIND CURRENT ar-inv NO-LOCK NO-ERROR.
        END.

        FIND FIRST ar-c-memo NO-LOCK
            WHERE ar-c-memo.reftable EQ "ar-cashl.ar-cashl"
              AND ar-c-memo.company  EQ ar-cash.company
              AND ar-c-memo.loc      EQ ""
              AND ar-c-memo.code     EQ ar-cashl.rec_key
            NO-ERROR.

        IF AVAIL ar-c-memo THEN
        FIND FIRST b-cashl
            WHERE b-cashl.c-no EQ INT(ar-c-memo.val[1])
              AND b-cashl.line EQ INT(ar-c-memo.val[2])
            NO-ERROR.
      END.

      IF AVAIL b-cashl THEN DO:
        ASSIGN
         b-cashl.inv-no     = ar-cashl.inv-no
         b-cashl.inv-date   = ar-cashl.inv-date
         b-cashl.amt-due    = ar-cashl.amt-due
         b-cashl.on-account = NO.
        DELETE ar-cashl.
      END.

      ELSE DO:
        IF ar-cashl.inv-no EQ 0 AND ar-cashl.on-account EQ YES THEN
          cust.on-account = cust.on-account + ar-cashl.amt-paid.

        CREATE gltrans.
        ASSIGN
         t1 = t1 + ar-cashl.amt-paid

         gltrans.company   = cocode
         gltrans.actnum    = ar-cashl.actnum
         gltrans.jrnl      = "CASHR"
         gltrans.tr-dscr   = cust.cust-no + " " +
                             STRING(ar-cash.check-no,"9999999999") +
                             " Inv# " + STRING(ar-cashl.inv-no)
         gltrans.tr-date   = tran-date
         gltrans.tr-amt    = ar-cashl.amt-paid - ar-cashl.amt-disc
         gltrans.period    = tran-period
         gltrans.trnum     = xtrnum
         ar-cashl.amt-paid = ar-cashl.amt-paid - ar-cashl.amt-disc.

        RELEASE gltrans.

        IF ar-cashl.amt-disc NE 0 THEN DO:
          CREATE gltrans.
          ASSIGN
           gltrans.company = cocode
           gltrans.actnum  = xdis-acct
           gltrans.jrnl    = "CRDIS"
           gltrans.tr-dscr = cust.cust-no + " " +
                             STRING(ar-cash.check-no,"9999999999") +
                             " Inv# " + STRING(ar-cashl.inv-no)
           gltrans.tr-date = tran-date
           gltrans.tr-amt  = ar-cashl.amt-disc
           gltrans.period  = tran-period
           gltrans.trnum   = xtrnum.

          RELEASE gltrans.

          CREATE ar-ledger.
          ASSIGN
           ar-ledger.company  = cocode
           ar-ledger.cust-no  = ar-cash.cust-no
           ar-ledger.amt      = ar-cashl.amt-disc
           ar-ledger.ref-num  = "DISC " +
                                STRING(ar-cash.check-no,"9999999999") +
                                "-" + STRING(ar-cashl.line,"9999999999")
           ar-ledger.ref-date = ar-cash.check-date
           ar-ledger.tr-date  = tran-date
           ar-ledger.tr-num   = xtrnum.
          RELEASE ar-ledger.
        END.
      END.
    END.  /* each line */
    
    ASSIGN
     cust.acc-bal   = cust.acc-bal - t1
     cust.lpay      = t1
     cust.lpay-date = ar-cash.check-date.

    IF cust.acc-bal GE cust.hibal THEN
      ASSIGN
       cust.hibal      = cust.acc-bal
       cust.hibal-date = ar-cash.check-date.

    IF t1 NE 0 THEN DO:
      FIND gltrans WHERE ROWID(gltrans) EQ lv-rowid NO-ERROR.
      IF NOT AVAIL gltrans THEN DO:
        CREATE gltrans.
        ASSIGN
         gltrans.company = cocode
         gltrans.actnum  = xar-acct
         gltrans.jrnl    = "CASHR"
         gltrans.tr-dscr = "CASH RECEIPTS"
         gltrans.tr-date = tran-date
         gltrans.period  = tran-period
         gltrans.trnum   = xtrnum
         lv-rowid        = ROWID(gltrans).
      END.
      gltrans.tr-amt = gltrans.tr-amt - t1.
      RELEASE gltrans.
    END.

    CREATE ar-ledger.
    ASSIGN
     ar-ledger.company  = cocode
     ar-ledger.cust-no  = ar-cash.cust-no
     ar-ledger.amt      = ar-cash.check-amt
     ar-ledger.ref-num  = "CHK# " + STRING(ar-cash.check-no,"9999999999")
     ar-ledger.ref-date = ar-cash.check-date
     ar-ledger.tr-date  = tran-date
     ar-ledger.tr-num   = xtrnum
     ar-cash.posted     = YES.
    RELEASE ar-ledger.

    RELEASE cust.
    RELEASE bank.

    ACCUM tt-post.curr-amt - ar-cash.check-amt (TOTAL BY tt-post.actnum).

    IF LAST-OF(tt-post.actnum) AND tt-post.actnum NE "" THEN DO:
      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-post.actnum
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPTS CURRENCY GAIN/LOSS"
       gltrans.tr-date = tran-date
       gltrans.period  = tran-period
       gltrans.trnum   = xtrnum
       gltrans.tr-amt  = (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).

      RELEASE gltrans.

      CREATE gltrans.
      ASSIGN
       gltrans.company = cocode
       gltrans.actnum  = tt-post.actnum
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPTS CURRENCY GAIN/LOSS"
       gltrans.tr-date = tran-date
       gltrans.period  = tran-period
       gltrans.trnum   = xtrnum
       gltrans.tr-amt  = - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-cash.check-amt).
      RELEASE gltrans.
    END.

    DELETE tt-post.
  END.
 END.  /* DO WHILE */

 RELEASE ar-cash.

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
  
  ASSIGN targetfile = lv-audit-dir + "\AR\AC2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC2".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
  END.
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
        IF xtrnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

END PROCEDURE.
