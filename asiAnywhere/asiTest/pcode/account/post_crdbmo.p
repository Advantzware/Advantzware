

/*------------------------------------------------------------------------
    File        : post_crdbmo.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttPostCRDRMemoReport NO-UNDO
        FIELD pstcd AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsPostCRDRMemoReport FOR ttPostCRDRMemoReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmexprt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPostCRDRMemoReport.

     IF   prmUser       = ? THEN ASSIGN    prmUser       = "".   
     IF   prmAction     = ? THEN ASSIGN    prmAction     = "". 
     IF   prmtrnsdt     = ? THEN ASSIGN    prmtrnsdt     = "". 
     IF   prmperiod     = ? THEN ASSIGN    prmperiod     = 0.
     IF   prmbegcust    = ? THEN ASSIGN    prmbegcust    = "". 
     IF   prmbegdt      = ? THEN ASSIGN    prmbegdt      = "".   
     IF   prmendcust    = ? THEN ASSIGN    prmendcust    = "". 
     IF   prmenddt      = ? THEN ASSIGN    prmenddt      = "". 
     IF   prmexprt      = ? THEN ASSIGN    prmexprt      = "". 
     IF   prmOut        = ? THEN ASSIGN    prmOut        = "".
     
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE end_cust   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_date   AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE tran-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE tb_export  AS LOGICAL INITIAL NO NO-UNDO.
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


DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEFINE VARIABLE save_id AS RECID NO-UNDO.
DEFINE VARIABLE time_stamp AS ch NO-UNDO.
DEFINE VARIABLE qfirst AS l NO-UNDO.
DEFINE VARIABLE post AS LOGICAL FORMAT "Yes/No"
  LABEL "        Post to G/L & Customer files?        " INITIAL NO NO-UNDO.
DEFINE VARIABLE g1 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE archk AS DECIMAL FORMAT ">>>>>>>>" NO-UNDO.
DEFINE VARIABLE t1 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE g2 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE t3 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v1 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v2 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE t2 AS DECIMAL format "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-on-act-amt AS DECIMAL NO-UNDO.
DEFINE VARIABLE xtrnum AS INTEGER NO-UNDO.
DEFINE VARIABLE xar-acct AS CHARACTER NO-UNDO.
DEFINE VARIABLE xcs-acct AS CHARACTER NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF BUFFER b-reftable2 FOR reftable.

DO TRANSACTION:
  {sys/inc/postdate.i}
  {sys/inc/inexport.i}
END.


DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF NEW SHARED VAR v-term-id AS cha NO-UNDO.

find first sys-ctrl
        where sys-ctrl.company eq cocode
          and sys-ctrl.name    eq "INVPRINT"
        no-lock no-error.
v-print-fmt = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".


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
   


  IF prmAction = "pstcd" THEN DO:


   ASSIGN            
                     
      begin_cust   = prmbegcust
      begin_date   = date(prmbegdt)  
      end_cust     = prmendcust  
      end_date     = date(prmenddt)  
      tran-date    = date(prmtrnsdt) 
      tran-period  = prmperiod   
      tb_export    = IF prmexprt = "Yes" THEN TRUE ELSE FALSE .
       

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "PostCRDBMemo" +
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

      IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
       THEN is-xprint-form = YES.
      ELSE is-xprint-form = NO.

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
      cError = "No Credit/Debit Memos available for posting...".
      RUN undo-trnum.
  END.


  CREATE ttPostCRDRMemoReport.

    ASSIGN ttPostCRDRMemoReport.pstcd = vTextFile .

  END.

/*****************************************************************************************/
PROCEDURE run-report :
/* ---------------------------------------------------- ar/ar-creg.p 10/94 gb */
/* AR Cash  - Edit Register & Post Transactions                   */
/* -------------------------------------------------------------------------- */
{sa/sa-sls01.i}
v-term-id = v-term.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}

ASSIGN
  v-postable = NO
  g2 = 0.

{sys/form/r-top3w.f}

FORMAT HEADER
  "CUSTOMER NAME                                      MEMO #    DATE          "
  "AMOUNT" skip
  "INVOICE   G/L DISTRIBUTION" AT 70 SKIP FILL("_",132) FORMAT "x(132)"
  WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

  ASSIGN
    time_stamp = STRING(TIME, "HH:MMam")
    tmpstore   = FILL("_",125)
    str-tit  = coname + " - " + loname
    str-tit2 = "CR/DB MEMOS -  POSTING REPORT  " + STRING(xtrnum)
    str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date)
    x = (112 - LENGTH(str-tit)) / 2
    str-tit  = FILL(" ",x) + str-tit
    x = (114 - LENGTH(str-tit2)) / 2
    str-tit2 = FILL(" ",x) + str-tit2
    x = (132 - LENGTH(str-tit3)) / 2
    str-tit3 = FILL(" ",x) + str-tit3 .

  DISPLAY "" WITH FRAME r-top.
  DISPLAY "" WITH FRAME f-top.
    
  for each ar-cash
      where ar-cash.company   eq cocode
        and ar-cash.posted    eq no
        and ar-cash.memo      eq YES
      /* gdm - 07130905 */
        AND ar-cash.cust-no    GE begin_cust
        AND ar-cash.cust-no    LE end_cust
        AND ar-cash.check-date GE begin_date
        AND ar-cash.check-date LE end_date
      /* gdm - 07130905 end */
        and can-find(first ar-cashl where ar-cashl.c-no eq ar-cash.c-no)
        and can-find(first reftable
                     where reftable.reftable eq "AR-CASH"
                       and reftable.code     eq string(ar-cash.c-no,"9999999999")
                     use-index code)
      use-index posted
      
      break by ar-cash.cust-no
            by ar-cash.check-no
      with frame a1:

    FIND FIRST reftable WHERE
         reftable.reftable = "ARCASHHOLD" AND
         reftable.rec_key = ar-cash.rec_key
         USE-INDEX rec_key
         NO-LOCK NO-ERROR.

    /*skip on hold cms*/
    IF AVAIL reftable AND reftable.CODE EQ "H" THEN
    DO:
      RELEASE reftable.
      NEXT.
    END.

    RELEASE reftable no-error.
    
    IF can-find(FIRST ar-cashl WHERE ar-cashl.company = cocode and
                               ar-cashl.c-no = ar-cash.c-no AND
                               (ar-cashl.amt-paid + ar-cashl.amt-disc) < 0 )
    THEN DO:
       CREATE report.
       ASSIGN
        report.term-id = v-term-id
        report.key-01  = STRING(ar-cash.check-no,"9999999999")
        report.rec-id  = RECID(ar-cash).
        FIND FIRST cust where cust.company = cocode AND cust.cust-no = ar-cash.cust-no NO-LOCK NO-ERROR.
        IF cust.factored THEN
            report.key-02 = "Factored".  /* for ar/rep/expcmemo.p task#  09200521*/
    END.
    v-postable = yes.
    IF FIRST-OF(ar-cash.cust-no) THEN DO:
      FIND FIRST cust where cust.company = cocode AND cust.cust-no = ar-cash.cust-no
        NO-LOCK NO-ERROR.
      PUT cust.cust-no SPACE(1) cust.name.
    END.
    ELSE
    IF FIRST-OF(ar-cash.check-no) THEN
    PUT SKIP(1).
    PUT ar-cash.check-no  FORMAT ">>>>>>>9"  TO 57
      ar-cash.check-date AT 60
      ar-cash.check-amt  format "->,>>>,>>9.99" AT 72 skip.

    FOR EACH ar-cashl WHERE ar-cashl.company = cocode and
                            ar-cashl.c-no = ar-cash.c-no 
        NO-LOCK
        USE-INDEX c-no
        BREAK BY ar-cashl.line
        WITH FRAME a2 NO-BOX NO-LABELS WIDTH 132:
      
      v2 = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc.
      PUT ar-cashl.inv-no AT 70 ar-cashl.actnum AT 80 SPACE(1)
          ar-cashl.amt-paid - ar-cashl.amt-disc format "->,>>>,>>9.99" TO 126 SKIP.
    END. /* each ar-cashl */
    IF LAST-OF(ar-cash.cust-no) THEN
    DO:
      DISPLAY  "*  CUSTOMER TOTALS" AT 88 v2 TO 126 " *" SKIP(1)
        WITH FRAME vtot NO-BOX NO-LABELS WIDTH 132 STREAM-IO.
      ASSIGN
         g1 = g1 + v1
         g2 = g2 + v2
         v1 = 0
         v2 = 0.
    END.
  END. /* each invoice */

  DISPLAY  "** GRAND TOTAL  "  AT 90  g2 TO 126 " **"
    WITH NO-LABELS NO-UNDERLINE WIDTH 132 FRAME GT.

  HIDE FRAME f-top.

  ASSIGN
     str-tit3 = "Period " + STRING(tran-period,"99") + " " + STRING(tran-date) + " - " +
                "Summary by Account"
     x = (132 - LENGTH(str-tit3)) / 2
     str-tit3 = FILL(" ",x) + str-tit3.
  PAGE.
  FORMAT HEADER
    "ACCOUNT                                  DATE   CUSTOMER   MEMO #"
    "LINE DESCRIPTION                QTY   UNIT PRICE           AMOUNT" SKIP
    FILL("_",132) FORMAT "x(132)"
    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132 STREAM-IO.

  DISPLAY "" WITH FRAME f-top2.

  for each ar-cashl
      where ar-cashl.company eq cocode
        and ar-cashl.posted  eq no
        and can-find(first reftable
                     where reftable.reftable eq "AR-CASH"
                       and reftable.code     eq
                                      string(ar-cashl.c-no,"9999999999")
                     use-index code)
      use-index inv-no
      NO-LOCK
      break by ar-cashl.actnum
            by ar-cashl.check-no
      
      with width 132 no-labels:
      
    FIND ar-cash WHERE ar-cash.company = cocode and
                       ar-cash.c-no = ar-cashl.c-no and         
                 /* gdm - 07130905 */
                      ar-cash.cust-no    GE begin_cust
                  AND ar-cash.cust-no    LE end_cust
                  AND ar-cash.check-date GE begin_date
                  AND ar-cash.check-date LE end_date
                /* gdm - 07130905 end */
                  AND ar-cash.memo USE-INDEX c-no NO-LOCK NO-ERROR.
    IF AVAILABLE ar-cash THEN
    DO:
       FIND FIRST reftable WHERE
            reftable.reftable = "ARCASHHOLD" AND
            reftable.rec_key = ar-cash.rec_key
            USE-INDEX rec_key
            NO-LOCK NO-ERROR.

       /*skip on hold cms*/
       IF AVAIL reftable AND reftable.CODE EQ "H" THEN
       DO:
          RELEASE reftable.
          NEXT.
       END.

       RELEASE reftable no-error.

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
         ar-cash.check-date AT 41     SPACE(1)
         ar-cash.cust-no              SPACE(1)
         archk                        SPACE(1)
         ar-cashl.line FORMAT ">>>9"  SPACE(1)
         ar-cashl.dscr FORMAT "x(20)" SPACE(1)
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
         
OUTPUT CLOSE.

v-ftp-done = NO.
IF tb_export AND inexport-log THEN DO:    
   DEF VAR v-exp-file AS cha NO-UNDO.
   v-exp-file = inexport-desc +  
                "MEMO_" + trim(v-print-fmt) + 
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat".
   OUTPUT TO VALUE(v-exp-file).
   IF inexport-cha EQ "CIT" THEN DO:
      RUN ar/rep/expfmemo.p .
      OUTPUT CLOSE.
      OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
      PUT UNFORMATTED 
       "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
       "ftpa1526" SKIP  /* userid*/
       "none" SKIP  /* password */
       "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
       "quit" .
      OUTPUT CLOSE.
      OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
      v-ftp-done = YES.
   END.
   ELSE IF inexport-cha EQ "ContSrvc" THEN DO:
        OUTPUT TO VALUE(v-exp-file).
        RUN ar/rep/expcmemo.p .
        OUTPUT CLOSE.
   END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.

END PROCEDURE.

PROCEDURE post-gl :
postit:
 DO TRANSACTION ON ERROR UNDO:
    g2 = 0.

    for each ar-cash
        where ar-cash.company   eq cocode
          and ar-cash.posted    eq no
          and ar-cash.memo      eq yes
          /* gdm - 07130905 */
          AND ar-cash.cust-no    GE begin_cust
          AND ar-cash.cust-no    LE end_cust
          AND ar-cash.check-date GE begin_date
          AND ar-cash.check-date LE end_date
        /* gdm - 07130905 end */
          and can-find(first ar-cashl where ar-cashl.c-no eq ar-cash.c-no)
        use-index posted,
      
        first reftable
        where reftable.reftable eq "AR-CASH"
          and reftable.code     eq string(ar-cash.c-no,"9999999999")
        use-index CODE on error undo postit, leave postit:
      
      FIND FIRST b-reftable2 WHERE
           b-reftable2.reftable = "ARCASHHOLD" AND
           b-reftable2.rec_key = ar-cash.rec_key
           USE-INDEX rec_key
           NO-LOCK NO-ERROR.

     /*skip on hold cms*/
      IF AVAIL b-reftable2 AND b-reftable2.CODE EQ "H" THEN
      DO:
         RELEASE b-reftable2.
         NEXT.
      END.

      delete reftable.

      {ar/ar-dreg.i}

    end.  /* for each */

    RELEASE ar-ledger.

    CREATE gltrans.
    ASSIGN
      gltrans.company = cocode
      gltrans.actnum  = xar-acct
      gltrans.jrnl    = "DBMEM"
      gltrans.tr-dscr = "CREDIT/DEBIT MEMO"
      gltrans.tr-date = tran-date
      gltrans.tr-amt  = + g2
      gltrans.period  = tran-period
      gltrans.trnum   = xtrnum.
    IF gltrans.tr-amt < 0 THEN
    gltrans.jrnl = "CRMEM".

    RELEASE gltrans.
  END. /* postit: transaction */
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
  
  ASSIGN targetfile = lv-audit-dir + "\AR\AW4\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AR"
         dirname3 = lv-audit-dir + "\AR\AW4".

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
