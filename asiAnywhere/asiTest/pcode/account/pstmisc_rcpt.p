

/*------------------------------------------------------------------------
    File        : pstmisc_rcpt.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttEditPstArMiscReceiptReport NO-UNDO
        FIELD pstmisc AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsEditPstArMiscReceiptReport FOR ttEditPstArMiscReceiptReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmIn            AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEditPstArMiscReceiptReport.

     IF  prmUser      = ? THEN ASSIGN  prmUser      = "".   
     IF  prmAction    = ? THEN ASSIGN  prmAction    = "". 
     IF  prmtrnsdt    = ? THEN ASSIGN  prmtrnsdt    = "". 
     IF  prmperiod    = ? THEN ASSIGN  prmperiod    = 0.
     IF  prmbegdt     = ? THEN ASSIGN  prmbegdt     = "". 
     IF  prmenddt     = ? THEN ASSIGN  prmenddt     = "".   
     IF  prmOut       = ? THEN ASSIGN  prmOut       = "". 
     IF  prmIn        = ? THEN ASSIGN  prmIn        = "". 
     
     
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE end_date   AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE tran-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
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

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.
    
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.
DEFINE VARIABLE tb_excel       AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_runExcel    AS LOGICAL INITIAL no NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var qfirst as l NO-UNDO.
def var post as logical format "Yes/No"
                        label "   Post to G/L files?   " initial no NO-UNDO.
def var xtrnum as int NO-UNDO.
def var xcs-acct as char NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.


def TEMP-TABLE w-bank NO-UNDO
   field bank   like bank.bank-code  
   field actnum like account.actnum  
   field bal    like bank.bal  .

DEF TEMP-TABLE tt-post NO-UNDO FIELD row-id AS ROWID
                               FIELD ex-rate LIKE currency.ex-rate INIT 1
                               FIELD curr-amt LIKE ar-cash.check-amt
                               FIELD actnum LIKE account.actnum.

DO TRANSACTION:
  {sys/inc/postdate.i}
END.

DEF STREAM excel.



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
   


  IF prmAction = "pstmisc" THEN DO:


   ASSIGN            
                     
      begin_date   = date(prmbegdt)  
      end_date     = date(prmenddt)  
      tran-date    = date(prmtrnsdt) 
      tran-period  = prmperiod   
      .
       

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "PostMisc" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "PostMisc" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .


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
 
    IF prmIn = "Yes" THEN do:      
        RUN post-gl.
        RUN copy-report-to-audit-dir.
             cError = "Posting Complete" .
    END.
    ELSE RUN undo-trnum.  
  END.

  ELSE do:
      cError = "No Miscellaneous Cash Receipts available for posting...".
      RUN undo-trnum.
  END.


  CREATE ttEditPstArMiscReceiptReport.
    IF prmOut = "Yes" THEN
        ASSIGN ttEditPstArMiscReceiptReport.pstmisc = vExcalFile .
    ELSE
        ASSIGN ttEditPstArMiscReceiptReport.pstmisc = vTextFile .

  END.

/*****************************************************************************************/
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */
/*
DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO. */

def var g1 as dec format "->>>,>>>,>>9.99" NO-UNDO.
def var g2 as dec format "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-first AS LOG NO-UNDO.

/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   excelHeader = 'Rec#,NAME,DATE,AMOUNT,G/L DISTRIBUTION,'.
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
END. /* if tb_excel */


v-postable = NO.

form header
"Rec#      NAME                                  DATE        "
"AMOUNT                 G/L DISTRIBUTION" skip fill("_",131) format "x(131)"
with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

ASSIGN
time_stamp = string(time, "HH:MMam")
tmpstore   = fill("_",125).

{sys/form/r-top3w.f}


   assign
   str-tit  = coname + " - " + loname
   str-tit2 = "MISC. CASH RECEIPTS  -  EDIT REGISTER " + string(xtrnum)
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
   str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3
   z = 0.

   display "" with frame r-top.
   display "" with frame f-top.

   EMPTY TEMP-TABLE tt-post.

   FOR EACH ar-mcash NO-LOCK
       WHERE ar-mcash.company    EQ cocode
         AND ar-mcash.posted     EQ NO
         AND ar-mcash.check-date GE begin_date
         AND ar-mcash.check-date LE end_date
       BREAK BY ar-mcash.bank-code
       WITH FRAME a1:

      IF FIRST-OF(ar-mcash.bank-code) THEN DO:
         ASSIGN g1 = 0.
         FIND FIRST bank WHERE bank.company = cocode AND
                           bank.bank-code = ar-mcash.bank-code
                           NO-LOCK NO-ERROR.
         IF AVAIL bank THEN
         DO:
            PUT bank.bank-name bank.actnum SKIP.
            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' bank.bank-name + " " + bank.actnum '",' SKIP.
         END.
       
         ELSE DO:
            cError = "No Bank Record Available.".       
            IF tb_excel THEN
               OUTPUT STREAM excel CLOSE.
           /* op-error = YES.*/
            RETURN.
         END.
 
         z = z + 1.
      END.

      CREATE tt-post.
      ASSIGN
       tt-post.row-id   = ROWID(ar-mcash)
       tt-post.curr-amt = ar-mcash.check-amt.
    
      RELEASE currency.
      IF lv-comp-curr NE "" AND lv-comp-curr NE ar-mcash.curr-code[1] THEN
      FIND FIRST currency NO-LOCK
          WHERE currency.company     EQ ar-mcash.company
            AND currency.c-code      EQ ar-mcash.curr-code[1]
            AND currency.ar-ast-acct NE ""
            AND currency.ex-rate     GT 0
          NO-ERROR.

      IF AVAIL currency THEN
        ASSIGN
         tt-post.actnum   = currency.ar-ast-acct
         tt-post.ex-rate  = currency.ex-rate
         tt-post.curr-amt = tt-post.curr-amt * tt-post.ex-rate.

      ASSIGN g1 = g1 + tt-post.curr-amt.

      PUT ar-mcash.m-no
          ar-mcash.payer
          ar-mcash.check-date AT 50
          tt-post.curr-amt    AT 62
          ar-mcash.actnum     AT 85  SPACE(1)
          tt-post.curr-amt    TO 125 SKIP.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
             '"' STRING(ar-mcash.m-no) + " " + ar-mcash.payer '",'
             '"' "" '",'
             '"' STRING(ar-mcash.check-date,"99/99/99") '",'
             '"' STRING(tt-post.curr-amt,"->>,>>>,>>9.99") '",'
             '"' ar-mcash.actnum '",'
             '"' STRING(tt-post.curr-amt,"->>,>>>,>>9.99") '",'
             SKIP.

      IF LAST-OF(ar-mcash.bank-code) THEN DO:
         PUT "**  TOTAL  "  at 85  g1 to 125 SKIP.

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "TOTAL" '",'
                '"' STRING(g1,"->>>,>>>,>>9.99") '",'
                SKIP(2).

         ASSIGN
            g2 = g2 + g1
            g1 = 0.
      END.

      v-postable = YES.
   END. /* each invoice */

   if z > 1 then
   DO:
       display  "** GRAND TOTAL  "  at 85  g2 to 125
         with no-labels no-underline width 132 frame gt.

       IF tb_excel THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'
              '"' "" '",'
              '"' "" '",'
              '"' "" '",'
              '"' "GRAND TOTAL" '",'
              '"' STRING(g2,"->>>,>>>,>>9.99") '",'
              SKIP(2).
   END.

   hide frame f-top.

   ASSIGN
      str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
                 "Summary by Account"
      x = (132 - length(str-tit3)) / 2
      str-tit3 = fill(" ",x) + str-tit3 .
   page.
   form header
   "ACCCOUNT                                  DATE     Rec.# PAID BY"
   "                               AMOUNT"
   skip
   fill("_",132) format "x(130)"
   with no-labels no-box no-underline frame f-top2 page-top width 132.

   display "" with frame f-top2.

   IF tb_excel THEN DO:
      excelHeader = 'ACCOUNT,DATE,Rec.#,PAID BY,AMOUNT,'.
      PUT STREAM excel UNFORMATTED '"' REPLACE(excelHeader,',','","') '"' SKIP.
   END. /* if tb_excel */

   FOR EACH tt-post,
       FIRST ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-post.row-id
       BREAK BY ar-mcash.actnum
             BY ar-mcash.m-no
       WITH WIDTH 132 NO-LABELS:

      IF FIRST-OF(ar-mcash.actnum) THEN DO:
         FIND FIRST account WHERE account.company = cocode AND
                                  account.actnum  = ar-mcash.actnum
                                  NO-LOCK NO-ERROR.

         IF AVAIL account THEN
         DO:
            PUT SKIP ar-mcash.actnum + " - " + account.dscr FORMAT "x(39)".
            IF tb_excel THEN
            DO:
               v-first = YES.
               PUT STREAM excel UNFORMATTED
                   '"' ar-mcash.actnum + " - " + account.dscr '",'.
            END.
         END.
         ELSE
         DO:
            cError = "No Account Record Available for this Account #: " .
            IF tb_excel THEN
               OUTPUT STREAM excel CLOSE.
            
          /*  op-error = YES.*/
            RETURN.
         END.
      END.

      PUT ar-mcash.check-date AT 41     SPACE(1)
          ar-mcash.m-no                 SPACE(1)
          ar-mcash.payer                SPACE(1)
          ar-mcash.check-amt           .

      IF tb_excel THEN
      DO:
         IF v-first = NO THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
         ELSE
         DO:
            PUT STREAM excel UNFORMATTED
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
            v-first = NO.
         END.
      END.

      ACCUMULATE ar-mcash.check-amt (TOTAL BY ar-mcash.actnum).
      ACCUMULATE ar-mcash.check-amt (TOTAL).

      IF LAST-OF(ar-mcash.actnum) THEN
      DO:
         PUT SKIP "** TOTAL "  TO 100
             (ACCUM TOTAL BY ar-mcash.actnum ar-mcash.check-amt)
             FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "** TOTAL" '",'
                '"' STRING((ACCUM TOTAL BY ar-mcash.actnum ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
                SKIP(2).
      END.
   END.

   FOR EACH tt-post WHERE tt-post.actnum NE "",
       FIRST ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-post.row-id
       BREAK BY tt-post.actnum
             BY ar-mcash.m-no
       WITH WIDTH 132 NO-LABELS:

      IF FIRST-OF(tt-post.actnum) THEN DO:
         FIND FIRST account WHERE account.company = cocode AND
                                  account.actnum  = tt-post.actnum
                                  NO-LOCK NO-ERROR.
         IF AVAIL account THEN
         DO:
            PUT SKIP tt-post.actnum + " - " + account.dscr FORMAT "x(39)".
            IF tb_excel THEN
            DO:
               v-first = YES.
               PUT STREAM excel UNFORMATTED
                   '"' tt-post.actnum + " - " + account.dscr '",'.
            END.
         END.
         ELSE
         DO:
            cError = "No Account Record Available for this Account #: " .
            IF tb_excel THEN
               OUTPUT STREAM excel CLOSE.
           /* op-error = YES.*/
            RETURN.
         END.
      END.

      PUT  ar-mcash.check-date AT 41     SPACE(1)
           ar-mcash.m-no                 SPACE(1)
           ar-mcash.payer                SPACE(1)
           tt-post.curr-amt - ar-mcash.check-amt
                                         FORMAT "->>,>>>,>>9.99".

      IF tb_excel THEN
      DO:
         IF v-first = NO THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(tt-post.curr-amt - ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
         ELSE
         DO:
            PUT STREAM excel UNFORMATTED
                '"' ar-mcash.check-date '",'
                '"' ar-mcash.m-no '",'
                '"' ar-mcash.payer '",'
                '"' STRING(tt-post.curr-amt - ar-mcash.check-amt,"->>,>>>,>>9.99") '",'
                SKIP.
            v-first = NO.
         END.
      END.

      ACCUMULATE tt-post.curr-amt - ar-mcash.check-amt (TOTAL BY tt-post.actnum).
      ACCUMULATE tt-post.curr-amt - ar-mcash.check-amt (TOTAL).

      IF LAST-OF(tt-post.actnum) THEN
      DO:
         PUT SKIP "** TOTAL "  TO 100
             (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt)
             FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "" '",'
                '"' "** TOTAL" '",'
                '"' STRING((ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
                SKIP(2).
      END.
   END.

   PUT "***** TOTAL FOR ALL ACCOUNTS " TO 100
       (ACCUM TOTAL ar-mcash.check-amt) +
         (ACCUM TOTAL tt-post.curr-amt - ar-mcash.check-amt)
       FORMAT "->>>,>>>,>>9.99" TO 125 SKIP(1).

   IF tb_excel THEN
   DO:
      PUT STREAM excel UNFORMATTED
          '"' "" '",'
          '"' "" '",' 
          '"' "" '",'
          '"' "" '",'
          '"' "***** TOTAL FOR ALL ACCOUNTS" '",'
          '"' STRING((ACCUM TOTAL ar-mcash.check-amt) +
                     (ACCUM TOTAL tt-post.curr-amt - ar-mcash.check-amt),"->>>,>>>,>>9.99") '",'
          SKIP.
   
      OUTPUT STREAM excel CLOSE.

      /*IF tb_runExcel THEN
         OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(fi_file)). */
   END.

END PROCEDURE.
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR li-iter AS INT NO-UNDO.


FOR EACH w-bank:
  DELETE w-bank.
END.

DO TRANSACTION:
 DO WHILE CAN-FIND(FIRST tt-post) AND li-iter LE 100000:
  li-iter = li-iter + 1.

  RELEASE tt-post.

  FOR EACH tt-post,
      FIRST ar-mcash WHERE ROWID(ar-mcash) EQ tt-post.row-id
      BREAK BY tt-post.actnum:

    FIND FIRST bank
        WHERE bank.company   EQ cocode
          AND bank.bank-code EQ ar-mcash.bank-code
        EXCLUSIVE NO-WAIT NO-ERROR.
    IF NOT AVAIL bank THEN NEXT.

    bank.bal = bank.bal + ar-mcash.check-amt.

    FIND FIRST w-bank WHERE w-bank.bank EQ ar-mcash.bank-code NO-ERROR.
    IF NOT AVAIL w-bank THEN DO:
      CREATE w-bank.
      ASSIGN
       w-bank.bank   = bank.bank-code
       w-bank.actnum = bank.actnum.
     END.
     w-bank.bal = w-bank.bal + ar-mcash.check-amt.

     CREATE gltrans.
     ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = ar-mcash.actnum
      gltrans.jrnl    = "MCSHREC"
      gltrans.tr-dscr = STRING(ar-mcash.m-no)
      gltrans.tr-date = tran-date
      gltrans.period  = tran-period
      gltrans.trnum   = xtrnum
      gltrans.tr-amt  = - ar-mcash.check-amt.
     RELEASE gltrans.

     CREATE ar-ledger.
     ASSIGN
      ar-ledger.company  = cocode
      ar-ledger.amt      = ar-mcash.check-amt
      ar-ledger.ref-num  = STRING(ar-mcash.m-no) + " " + ar-mcash.payer
      ar-ledger.ref-date = ar-mcash.check-date
      ar-ledger.tr-date  = tran-date
      ar-ledger.tr-num   = xtrnum
      ar-mcash.posted    = YES.
     RELEASE ar-ledger.

     ACCUM tt-post.curr-amt - ar-mcash.check-amt (TOTAL BY tt-post.actnum).

     IF LAST-OF(tt-post.actnum) AND tt-post.actnum NE "" THEN DO:
       CREATE gltrans.
       ASSIGN
        gltrans.company = cocode
        gltrans.actnum  = tt-post.actnum
        gltrans.jrnl    = "MCSHREC"
        gltrans.tr-dscr = "MISC CASH RECEIPTS CURRENCY GAIN/LOSS " +
                          STRING(ar-mcash.m-no)
        gltrans.tr-date = tran-date
        gltrans.period  = tran-period
        gltrans.trnum   = xtrnum
        gltrans.tr-amt  = (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt).
       RELEASE gltrans.

       CREATE gltrans.
       ASSIGN
        gltrans.company = cocode
        gltrans.actnum  = tt-post.actnum
        gltrans.jrnl    = "MCSHREC"
        gltrans.tr-dscr = "MISC CASH RECEIPTS CURRENCY GAIN/LOSS " +
                          STRING(ar-mcash.m-no)
        gltrans.tr-date = tran-date
        gltrans.period  = tran-period
        gltrans.trnum   = xtrnum
        gltrans.tr-amt  = - (ACCUM TOTAL BY tt-post.actnum tt-post.curr-amt - ar-mcash.check-amt).
       RELEASE gltrans.
    END.

    DELETE tt-post.
  END.
 END.  /* DO WHILE */

 FOR EACH w-bank:
   CREATE gltrans.
   ASSIGN
    gltrans.company = cocode
    gltrans.actnum  = w-bank.actnum
    gltrans.jrnl    = "MCSHREC"
    gltrans.tr-dscr = "MISC CASH RECEIPTS"
    gltrans.tr-date = tran-date
    gltrans.period  = tran-period
    gltrans.trnum   = xtrnum
    gltrans.tr-amt  = w-bank.bal.
   RELEASE gltrans.
 END.
END. /* DO TRANS */

FIND CURRENT ar-mcash NO-LOCK NO-ERROR.
FIND CURRENT bank NO-LOCK NO-ERROR.

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
  
  ASSIGN targetfile = lv-audit-dir + "\AR\AC4\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AC4".

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
        IF xtrnum = gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

END PROCEDURE.
