

/*------------------------------------------------------------------------
    File        : crdbmemo.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttCreditDebitMemoRpt NO-UNDO
        FIELD crdb AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsCreditDebitMemoRpt FOR ttCreditDebitMemoRpt.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPeriod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCreditDebitMemoRpt.

     IF prmUser       = ? THEN ASSIGN     prmUser        = "".   
     IF prmAction     = ? THEN ASSIGN     prmAction      = "". 
     IF prmtrnsdt     = ? THEN ASSIGN     prmtrnsdt      = "". 
     IF prmPeriod     = ? THEN ASSIGN     prmPeriod      = 0.
     IF prmOut        = ? THEN ASSIGN     prmOut         = "". 


    

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
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

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.


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


   find first company where company.company = cocode no-lock no-error.  
   TRAN-date = TODAY.


  IF prmAction = "crdb" THEN DO:


   ASSIGN
       
       tran-period =   prmPeriod 
       tran-date   =   date(prmtrnsdt) .
                    




        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "CRDRMemo" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        

        
      find first period                   
            where period.company eq cocode
            and period.pst     le tran-date
            and period.pend    ge tran-date
            no-lock no-error.
        if avail period then prmPeriod = (period.pnum).
        
        ELSE DO:
            cError = "No Defined Period Exists" .
            RETURN.
        end.


        run run-report. 
        

   
  CREATE ttCreditDebitMemoRpt.
  
    ASSIGN ttCreditDebitMemoRpt.crdb = vTextFile .

  END.

/*****************************************************************************************/
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-dreg.p  7/94 RM */
/* A/R Credit/Debit Memo Edit Register Program - A/R Module                   */
/* -------------------------------------------------------------------------- */
 
 {sys/form/r-top3w.f}
 
   DEFINE VARIABLE qfirst AS l.
   DEFINE VARIABLE g1 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE archk AS DECIMAL FORMAT ">>>>>>99".
   DEFINE VARIABLE t1 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE g2 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE t3 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE v1 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE v2 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE t2 AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE v-on-act-amt AS DECIMAL format "->,>>>,>>9.99".
   DEFINE VARIABLE xtrnum AS INTEGER.
   DEFINE VARIABLE xar-acct AS CHARACTER.
   DEFINE VARIABLE xcs-acct AS CHARACTER.

   FORMAT HEADER
     "CUSTOMER NAME                                      MEMO #    DATE          "
     "AMOUNT" skip
     "INVOICE   G/L DISTRIBUTION" AT 70 SKIP FILL("_",132) FORMAT "x(132)"
     WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP STREAM-IO WIDTH 132.

 
    FIND FIRST ar-ctrl WHERE ar-ctrl.company = cocode NO-LOCK.
    xar-acct = ar-ctrl.receivables.
    xcs-acct = ar-ctrl.cash-act.
    RELEASE ar-ctrl.

   ASSIGN
    str-tit2 = "Credit / Debit Memo - Edit Register"
    str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date)
    {sys/inc/ctrtext.i str-tit2 112}
    {sys/inc/ctrtext.i str-tit3 132}.

 /*{sys/inc/print1.i}*/


  if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir.

 {sys/inc/outprint.i value(lines-per-page)}

 
 display "" with frame r-top.
 DISPLAY "" WITH FRAME f-top.

 
 FOR EACH ar-cash WHERE ar-cash.company = cocode and
                          NOT posted AND memo NO-LOCK BREAK BY ar-cash.cust-no BY
       ar-cash.check-no WITH FRAME a1:
    IF FIRST-OF(ar-cash.cust-no) THEN
    DO:
      FIND FIRST cust where cust.company = cocode AND cust.cust-no = ar-cash.cust-no
        NO-LOCK NO-ERROR.
      PUT cust.cust-no SPACE(1) cust.name.
    END.
    ELSE
    IF FIRST-OF(ar-cash.check-no) THEN
    PUT SKIP(1).
    PUT ar-cash.check-no  FORMAT ">>>>>>>9"  TO 57
      ar-cash.check-date AT 60 FORMAT "99/99/99"
      ar-cash.check-amt  format "->,>>>,>>9.99" AT 72.

    FOR EACH ar-cashl WHERE ar-cashl.company = cocode and
                         ar-cashl.c-no = ar-cash.c-no NO-LOCK BREAK
        BY ar-cashl.line
        WITH FRAME a2 NO-BOX NO-LABELS STREAM-IO WIDTH 132:
      v2 = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc.
      PUT ar-cashl.inv-no AT 70 ar-cashl.actnum AT 80 SPACE(1)
        ar-cashl.amt-paid - ar-cashl.amt-disc
                     format "->,>>>,>>9.99" TO 126 SKIP.
    END. /* each ar-cashl */
    IF LAST-OF(ar-cash.cust-no) THEN
    DO:
      DISPLAY  "*  CUSTOMER TOTALS" AT 88 v2 TO 126 " *" SKIP(1)
        WITH FRAME vtot NO-BOX NO-LABELS STREAM-IO WIDTH 132.
      g1 = g1 + v1.
      g2 = g2 + v2.
      v1 = 0.
      v2 = 0.
    END.
  END. /* each invoice */

  DISPLAY  "** GRAND TOTAL  "  AT 90  g2 TO 126 " **"
    WITH NO-LABELS NO-UNDERLINE STREAM-IO WIDTH 132 FRAME GT.
  
    HIDE FRAME f-top.

  FORMAT HEADER
    "ACCOUNT                                  DATE   CUSTOMER   MEMO #"
    "LINE DESCRIPTION                QTY   UNIT PRICE           AMOUNT" SKIP
    FILL("_",132) FORMAT "x(132)"
    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP STREAM-IO WIDTH 132.

  DISPLAY "" WITH FRAME f-top2.

  FOR EACH ar-cashl WHERE ar-cashl.company = cocode and
                         NOT ar-cashl.posted NO-LOCK
      BREAK BY ar-cashl.actnum BY ar-cashl.check-no
      WITH STREAM-IO WIDTH 132 NO-LABELS:
    FIND ar-cash WHERE ar-cash.company = cocode and
                 ar-cash.c-no = ar-cashl.c-no
      AND ar-cash.memo NO-LOCK NO-ERROR.
    IF AVAILABLE ar-cash THEN
    DO:
      FIND cust where cust.company = cocode AND cust.cust-no = ar-cash.cust-no
        NO-LOCK NO-ERROR.
  
      IF FIRST-OF(ar-cashl.actnum) THEN
      DO:
        FIND FIRST account WHERE account.company = cocode AND
          account.actnum  = ar-cashl.actnum NO-LOCK NO-ERROR.
        IF AVAILABLE account THEN
        PUT ar-cashl.actnum + " - " + account.dscr FORMAT "x(39)" .
        ELSE
        PUT ar-cashl.actnum.
      END.
      archk = ar-cash.check-no.
      PUT
        ar-cash.check-date AT 41 FORMAT "99/99/99"   SPACE(1)
        ar-cash.cust-no                              SPACE(1)
        archk                                        SPACE(1)
        ar-cashl.line FORMAT ">>>9"                  SPACE(1)
        ar-cashl.dscr FORMAT "x(20)"                 SPACE(1)
        ar-cashl.amt-paid - ar-cashl.amt-disc format "->,>>>,>>9.99" TO 132
        SKIP.
      ACCUMULATE ar-cashl.amt-paid - ar-cashl.amt-disc
        (TOTAL BY ar-cashl.actnum).
      ACCUMULATE ar-cashl.amt-paid - ar-cashl.amt-disc (TOTAL).
      IF LAST-OF(ar-cashl.actnum) THEN
      DO:
        PUT "** TOTAL "  TO 116
          ACCUMULATE TOTAL BY ar-cashl.actnum
          ar-cashl.amt-paid - ar-cashl.amt-disc format "->,>>>,>>9.99" TO 132
          SKIP(1).
      END.
    END.
  END.
  PUT "***** TOTAL FOR ALL ACCOUNTS " TO 116
    ACCUMULATE TOTAL ar-cashl.amt-paid - ar-cashl.amt-disc
                           format "->,>>>,>>9.99" TO 132.
   
   
  
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
