

/*------------------------------------------------------------------------
    File        : prnt_crdbmo.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttPrintCreditDebitMemo NO-UNDO
        FIELD prcd AS CHAR
        FIELD ext AS CHAR.

DEFINE DATASET dsPrintCreditDebitMemo FOR ttPrintCreditDebitMemo.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegcust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegmemo       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendcust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendmemo       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmprtmo         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmpstmo         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmexprt         AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrintCreditDebitMemo.

     IF  prmUser       = ? THEN ASSIGN   prmUser        = "".   
     IF  prmAction     = ? THEN ASSIGN   prmAction      = "". 
     IF  prmbegcust    = ? THEN ASSIGN   prmbegcust     = "". 
     IF  prmbegmemo    = ? THEN ASSIGN   prmbegmemo     = 0.
     IF  prmbegdt      = ? THEN ASSIGN   prmbegdt       = "". 
     IF  prmendcust    = ? THEN ASSIGN   prmendcust     = "".   
     IF  prmendmemo    = ? THEN ASSIGN   prmendmemo     = 0. 
     IF  prmenddt      = ? THEN ASSIGN   prmenddt       = "". 
     IF  prmprtmo      = ? THEN ASSIGN   prmprtmo       = "". 
     IF  prmpstmo      = ? THEN ASSIGN   prmpstmo       = "".
     IF  prmexprt      = ? THEN ASSIGN   prmexprt       = "".
     IF  prmOut        = ? THEN ASSIGN   prmOut         = "".


    

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_memo AS INTEGER FORMAT ">>>>>>>>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE end_cust   AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_memo   AS INTEGER FORMAT ">>>>>>>>>9" INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_date   AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_posted  AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_export AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF VAR  tmp-path AS CHAR NO-UNDO. 
DEF VAR v-VERSION AS CHAR NO-UNDO. 


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
.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

   find first company where company.company = cocode no-lock no-error.  



DEF VAR v-program AS CHAR NO-UNDO.
def NEW SHARED var v-lo-cust like cust.cust-no init "" NO-UNDO.
def NEW SHARED var v-hi-cust like cust.cust-no init "zzzzzzzz" NO-UNDO.
def NEW SHARED var v-lo-memo like ar-cash.check-no init 0 NO-UNDO.
def NEW SHARED var v-hi-memo like ar-cash.check-no init 99999999 NO-UNDO.
def NEW SHARED var v-reprint as   log NO-UNDO.

/* gdm - 04210922 */
DEF NEW SHARED VAR v-begdt LIKE ar-cash.check-date NO-UNDO.
DEF NEW SHARED VAR v-enddt LIKE ar-cash.check-date NO-UNDO.
DEF NEW SHARED VAR v-tbpst AS LOG                  NO-UNDO.

def var save_id as recid NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var qfirst as l NO-UNDO.
def var g1 as dec format "->,>>>,>>9.99" NO-UNDO.
def var archk as dec format ">>>>>>99" NO-UNDO.
def var t1 as dec format "->,>>>,>>9.99" NO-UNDO.
def var g2 as dec format "->,>>>,>>9.99" NO-UNDO.
def var t3 as dec format "->,>>>,>>9.99" NO-UNDO.
def var v1 as dec format "->,>>>,>>9.99" NO-UNDO.
def var v2 as dec format "->,>>>,>>9.99" NO-UNDO.
def var t2 as dec format "->,>>>,>>9.99" NO-UNDO.
def var v-on-act-amt as dec format "->,>>>,>>9.99" NO-UNDO.
def var big_ul as char format "x(80)" no-undo.
def var big_ul2 as char format "x(80)" no-undo.
def var letterhead as char format "x(50)" extent 5 no-undo.

def var v-printlines as int init 0 no-undo.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS CHAR FORMAT "x(250)" NO-UNDO.

DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF VAR v-print-fmt AS cha NO-UNDO.
DEF NEW SHARED VAR v-term-id AS cha NO-UNDO.

/* gdm - 03120909 */
DEF VAR v-chrfld  AS CHAR NO-UNDO.
  
 IF prmAction = "getvalue" THEN do:
     IF v-print-fmt = "frankstn" OR v-print-fmt = "Mirpkg" THEN 
       ASSIGN tb_export = NO .
    ELSE ASSIGN
        tb_export = YES. 

    CREATE ttPrintCreditDebitMemo .
    ASSIGN ttPrintCreditDebitMemo.ext = STRING(tb_export).
    
 END.
    
    IF prmpstmo = 'yes' THEN prmprtmo = "YES".
 

   


  IF prmAction = "prcd" THEN DO:
      FIND FIRST sys-ctrl NO-LOCK
      WHERE sys-ctrl.company eq cocode AND sys-ctrl.name eq "ARMEMO" NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN 
  DO TRANSACTION:
     CREATE sys-ctrl.
     ASSIGN 
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "ARMEMO"
         sys-ctrl.descrip  = "Credit/Debit Memo Form"
         sys-ctrl.char-fld = "".
  END. 

  ASSIGN v-chrfld = IF AVAIL sys-ctrl THEN TRIM(sys-ctrl.char-fld) ELSE "" 
         v-print-fmt  = sys-ctrl.char-fld.

  IF AVAIL sys-ctrl AND 
     (TRIM(sys-ctrl.char-fld) NE "" AND
      TRIM(sys-ctrl.char-fld) NE "HOP")
    THEN IS-xprint-form = TRUE.  
  
  do transaction:
    {sys/inc/inexport.i}
  end.

    ASSIGN
       begin_cust  =  prmbegcust
       begin_memo  =  prmbegmemo 
       begin_date  =  date(prmbegdt)  
       end_cust    =  prmendcust
       end_memo    =  prmendmemo
       end_date    =  date(prmenddt)  
       tb_reprint  =  IF prmprtmo = "True" THEN YES ELSE NO 
       tb_posted   =  IF prmpstmo = "True" THEN TRUE ELSE FALSE 
       tb_export   =  IF prmexprt = "True" THEN TRUE ELSE FALSE    . 


          FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
           sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.
       
       IF NOT AVAIL sys-ctrl THEN
           DO:
           CREATE sys-ctrl.
           ASSIGN
               sys-ctrl.company  = cocode
               sys-ctrl.name     = "X-VERSION"
               sys-ctrl.descrip  = "Server Name"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "Server 2003".
           END.
           IF AVAIL sys-ctrl  THEN
               v-VERSION = sys-ctrl.char-fld .
           RELEASE sys-ctrl.
           
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
               sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.
           
           IF NOT AVAIL sys-ctrl THEN
               DO:
               CREATE sys-ctrl.
               ASSIGN
                   sys-ctrl.company  = cocode
                   sys-ctrl.name     = "Xspool"
                   sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
                   sys-ctrl.log-fld = YES
                   sys-ctrl.char-fld = "c:\spool\".
               END.
               IF AVAIL sys-ctrl  THEN
                   tmp-path = sys-ctrl.char-fld .
                   RELEASE sys-ctrl.

                   ASSIGN  
                       init-dir    = v-webrootpath 
                       lv-pdf-file = init-dir + 'PrintCrDbMemo' 
                       lv-pdf-file = lv-pdf-file + string(prmbegcust) +  STRING(YEAR(v-today),"9999")
                                    + STRING(MONTH(v-today),"99")
                                    + STRING(DAY(v-today),"99")
                       vPdfFile    = 'PrintCrDbMemo' + string(prmbegcust) +  STRING(YEAR(v-today),"9999")
                                    + STRING(MONTH(v-today),"99")
                                    + STRING(DAY(v-today),"99") + '.pdf'.
       

       

       IF IS-xprint-form THEN DO:
           RUN Set-Memo-Form. 
           RUN run-report. 
       END.
       ELSE RUN run-report-old.  

        IF v-VERSION = "Server 2008" THEN do:
        OS-COPY VALUE(list-name) VALUE (tmp-path).
        PAUSE 1.
        END.
        ELSE
        RUN printFile(list-name).

        CREATE ttPrintCreditDebitMemo.
        ASSIGN ttPrintCreditDebitMemo.prcd = vPdfFile .

 END.


/*****************************************************************************************/
PROCEDURE Set-Memo-Form :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR icFormName AS CHAR NO-UNDO.

ASSIGN
    icFormName = v-chrfld.

CASE icFormName:

    WHEN "AllWest" THEN
     ASSIGN
        v-program = "ar/rep/AlWstMem.p"
        lines-per-page = 66
        is-xprint-form = YES.
       
    WHEN "PremierPkg" THEN
     ASSIGN
        v-program = "ar/rep/prpkgmem.p"
        lines-per-page = 66
        is-xprint-form = YES.  
    OTHERWISE
     ASSIGN
        v-program      = "ar/rep/crdbmemo.p"
        is-xprint-form = YES
        lines-per-page = 66.

END CASE.

   
END PROCEDURE.

PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/invoice.p  9/94 RM */
/* PRINT INVOICE - O/E MODULE                                                 */
/* -------------------------------------------------------------------------- */
list-name = init-dir + "tmp" + string(time).
{sys/inc/outprint.i value(lines-per-page)}

{sa/sa-sls01.i}

v-term-id = v-term.

{sys/form/r-top3.f}

    
PUT "<PDF=DIRECT><PORTRAIT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(340)". 


ASSIGN 
    v-lo-cust = begin_cust
    v-hi-cust = end_cust
    v-lo-memo = begin_memo
    v-hi-memo = end_memo
    v-reprint = tb_reprint
    v-begdt   = begin_date
    v-enddt   = end_date
    v-tbpst   = tb_posted.

if v-hi-cust eq "" then v-hi-cust = "zzzzzzzz".



IF IS-xprint-form THEN RUN VALUE(v-program).

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.


end procedure.

PROCEDURE run-report-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    list-name = init-dir + "tmp" + string(time).
    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

IF IS-xprint-form THEN
  DO:
PUT "<PDF=DIRECT><PORTRAIT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(340)". 
  END.

 DEF VAR lv-desc LIKE ar-cashl.dscr NO-UNDO.


{sa/sa-sls01.i}
v-term-id = v-term.

{sys/form/r-top3.f}


format 
    lv-desc at 4
    ar-cashl.inv-no at 55
    ar-cashl.amt-paid to 78
   with frame crdb-lines no-box no-labels width 80 down.


assign
 time_stamp = string(time, "hh:mmam")
 tmpstore   = fill("_",125)
 big_ul     = fill("=",80)
 big_ul2    = fill("=",80).

ASSIGN v-lo-cust = begin_cust
       v-hi-cust = end_cust
       v-lo-memo = begin_memo
       v-hi-memo = end_memo
       v-reprint = tb_reprint.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

if avail oe-ctrl and oe-ctrl.prcom then do:
   
  assign
   letterhead[1] = ""
   letterhead[2] =  company.name
   letterhead[3] =  company.addr[1]
   letterhead[4] =  company.addr[2]
   letterhead[5] =  company.city + ", " + company.state + "  " + company.zip.

  do i = 5 to 2 by -1:
    if letterhead[i - 1] le " " and letterhead[i] ge "" then
      assign
       letterhead[i - 1] = letterhead[i]
       letterhead[i]     = "".
  end.
  do i = 1 to 5:
    {sys/inc/ctrtext.i letterhead[i] 50}.
  end.
end.

if v-hi-cust eq "" then v-hi-cust = "zzzzzzzz".

if v-chrfld eq "HOP" then do:
  run ar/rep/memohop.p.
  return.
end.

  for each ar-cash
      where ar-cash.company    eq cocode
        and ar-cash.posted     eq tb_posted
        and ar-cash.memo       eq yes
        and ar-cash.cust-no    ge v-lo-cust
        and ar-cash.cust-no    le v-hi-cust
        and ar-cash.check-no   ge v-lo-memo
        and ar-cash.check-no   le v-hi-memo
        and ar-cash.check-date ge begin_date
        and ar-cash.check-date le end_date
        and can-find(first ar-cashl where ar-cashl.c-no eq ar-cash.c-no)
        and ((v-reprint and
              can-find(first reftable
                       where reftable.reftable eq "AR-CASH"
                         and reftable.code     eq
                                    string(ar-cash.c-no,"9999999999")
                           use-index code)) or
             (not v-reprint and
              not can-find(first reftable
                           where reftable.reftable eq "AR-CASH"
                             and reftable.code     eq
                                        string(ar-cash.c-no,"9999999999")
                           use-index code)))
      use-index posted        
      break by ar-cash.cust-no
            by ar-cash.check-no with frame a1:

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

    release reftable no-error.
    
    IF can-find(FIRST ar-cashl WHERE ar-cashl.company = cocode and
                               ar-cashl.c-no = ar-cash.c-no AND
                               (ar-cashl.amt-paid + ar-cashl.amt-disc) < 0 )
    THEN DO:
      CREATE report.
      ASSIGN
          report.term-id = v-term-id
          report.key-01  = STRING(ar-cash.check-no,"9999999999")
          report.rec-id  = RECID(ar-cash).
    END.

    if first-of(ar-cash.cust-no) then do:
      v-printlines = 1.
      find first cust
          where cust.company = cocode
            and cust.cust-no eq ar-cash.cust-no
          no-lock no-error.

      if first(ar-cash.cust-no) then
        format HEADER
               SKIP(1)
               letterhead[1] at 5 "  Date:" to 70
               ar-cash.check-date FORMAT "99/99/99" skip
               letterhead[2] at 5
               letterhead[3] at 5 "Memo #:" to 70
               ar-cash.check-no format ">>>>>>>9" skip
               letterhead[4] at 5
               letterhead[5] at 5
               skip (4)
               "Customer #:" at 11 ar-cash.cust-no
               cust.name              at 11
               cust.addr [1]          at 11
               cust.addr [2]          at 11
               cust.city              at 11 cust.state cust.zip
               skip(4)
               big_ul at 1
        "********************     CREDIT / DEBIT MEMO     ********************"
               at 6
               big_ul2 at 1 skip(1)
               "Description" at 4 "Invoice #" at 55 "Amount" to 78
        "---------------------------------------------" at 4 "---------" at 55
        "--------------" to 78
            with frame crdb-header no-box no-labels width 80 PAGE-TOP STREAM-IO.
     
    end.
    view frame crdb-header.
    if first-of(ar-cash.check-no) then PAGE.

    for each ar-cashl
       where ar-cashl.company eq cocode
         and ar-cashl.c-no    eq ar-cash.c-no
       break by ar-cashl.line
       with frame crdb-lines no-box no-labels width 80 down:

      ASSIGN
        v2 = v2 + ar-cashl.amt-paid - ar-cashl.amt-disc
        lv-desc = ar-cashl.dscr.

      IF tb_posted THEN DO:

        IF lv-desc BEGINS "Credit -" THEN
          lv-desc = SUBSTR(lv-desc,10).
        ELSE IF lv-desc BEGINS "Debit -" THEN
          lv-desc = SUBSTR(lv-desc,9).
      END.

      display lv-desc ar-cashl.inv-no
              (ar-cashl.amt-paid - ar-cashl.amt-disc) @ ar-cashl.amt-paid
          with frame crdb-lines STREAM-IO.
      down with fram crdb-lines.
      v-printlines = v-printlines + 1.
      if v-printlines gt 26 then do:
        put "** CONTINUED **" at 35.
        page.
        assign v-printlines = 0.
      end.      

    end. /* each ar-cashl */

    if last-of(ar-cash.check-no) then do:
      put skip(27 - v-printlines) "Memo Amount" to 78 skip v2 to 78 skip.
      assign
       g1 = g1 + v1
       g2 = g2 + v2
       v1 = 0
       v2 = 0.
    end.
    
    find first reftable
        where reftable.reftable eq "AR-CASH"
          and reftable.code     eq string(ar-cash.c-no,"9999999999")
        use-index code no-error.
    if not avail reftable then do:
      create reftable.
      assign
       reftable.reftable = "AR-CASH"
       reftable.code     = string(ar-cash.c-no,"9999999999").
    end.

    /* gdm 07010903 */
    ASSIGN ar-cash.ret-memo = YES.

  end. /* each ar-cash */

  g2 = 0.
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
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.


END PROCEDURE.
