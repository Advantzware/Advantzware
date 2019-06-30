MESSAGE "top " .

/*------------------------------------------------------------------------
    File        : glacthist.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

  MESSAGE "enter".
    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttGLAccountHistory NO-UNDO
        FIELD hist AS CHAR
        FIELD post AS CHAR.

DEFINE DATASET dsGLAccountHistory FOR ttGLAccountHistory.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmautodis       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdetailed      AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLAccountHistory.

     IF prmUser         = ? THEN ASSIGN   prmUser       = "".   
     IF prmAction       = ? THEN ASSIGN   prmAction     = "". 
     IF prmbegdate      = ? THEN ASSIGN   prmbegdate    = "". 
     IF prmenddate      = ? THEN ASSIGN   prmenddate    = "". 
     IF prmBegact       = ? THEN ASSIGN   prmBegact     = "".  
     IF prmEndact       = ? THEN ASSIGN   prmEndact     = "".  
     IF prmOut          = ? THEN ASSIGN   prmOut        = "". 
     IF prmautodis      = ? THEN ASSIGN   prmautodis    = "". 
     IF prmdetailed     = ? THEN ASSIGN   prmdetailed   = "". 

DEFINE VARIABLE begin_date     AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_date       AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE begin_acct     AS CHARACTER FORMAT "X(25)" NO-UNDO.
DEFINE VARIABLE end_acct       AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE tb_exc-auto    AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_detailed    AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.
DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Account#" NO-UNDO.
DEFINE VARIABLE tb_excel       AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel    AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR post-line-error AS LOGICAL NO-UNDO INITIAL NO.
DEF VAR v-jrnal-2 AS CHAR NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---" NO-UNDO.
def var time_stamp as ch NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.


def var save_id as recid NO-UNDO.
def var tbal as dec format "->>,>>>,>>9.99" NO-UNDO.
def var deb  as dec label "DEBIT"  format "->>,>>>,>>9.99" NO-UNDO.
def var cred as dec label "CREDIT" format "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lv-rev AS CHAR LABEL " " NO-UNDO.
def var tot-deb  as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot-cred like tot-deb NO-UNDO.
def var ad like tot-deb NO-UNDO.
def var ac like tot-deb NO-UNDO.
def var gc like tot-deb NO-UNDO.
def var gd like tot-deb NO-UNDO.
def var curjnl as char format "x(8)" NO-UNDO.
def var fjrnl as int label "  From Journal Number" NO-UNDO.
def var tjrnl as int label "  Thru Journal Number" NO-UNDO.
def var xtrnum as int NO-UNDO.
def var sort-by-acct as log NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.



DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF VAR ip-post AS LOG NO-UNDO.
ASSIGN ip-post = YES.
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
 v-today   = TODAY 
 g_company = cocode
 g_user    = prmUser .


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

DEF NEW SHARED VAR udate AS DATE NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.

DEF STREAM excel.


assign
   begin_date = date(1,1,year(today))
   end_date   = today.


MESSAGE "prmAxtion" prmAction .
  IF prmAction = "hist" THEN DO:
    
        ASSIGN
       begin_date      = date(prmbegdate) 
       end_date        = date(prmenddate) 
       begin_acct      = prmBegact   
       end_acct        = prmEndact 
       tb_exc-auto     = IF prmautodis = "Yes" THEN TRUE ELSE FALSE   
       tb_detailed     = IF prmdetailed = "Yes" THEN TRUE ELSE FALSE.

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "GlHistPost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "GlHistPost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .

        
    
        RUN run-report.
   
   
  CREATE ttGLAccountHistory.
  IF prmOut = "Yes" THEN
    ASSIGN ttGLAccountHistory.hist = vExcalFile .
  ELSE
    ASSIGN ttGLAccountHistory.hist = vTextFile .

  END.
/*****************************************************************************************/
PROCEDURE run-report :
/* -------------------------------------------------- gl/gl-hist.p 01/98 FWK  */
/* GL History Report by Date and Account                                      */
/* -------------------------------------------------------------------------- */
MESSAGE "excel " .
{sys/form/r-top3lw.f}

def var save_id as recid.
def var time_stamp as ch NO-UNDO.
def var tot-all  as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tot-tx   like tot-all NO-UNDO.
def var tot-act  like tot-all NO-UNDO.
def var open-amt like tot-all NO-UNDO.
def var pri-amt  like tot-all NO-UNDO.
def var net-inc  as DEC NO-UNDO.
def var tmp-amt like gltrans.tr-amt NO-UNDO.
def var tmp-dscr AS CHAR FORMAT "X(54)" NO-UNDO.
def buffer xgltrans for gltrans.
def buffer xglhist for glhist.
def buffer xperiod for period.
def var str-tit4 as char no-undo.
def var str-tit5 as char no-undo.
def var v-s-date as date format "99/99/9999" init 01/01/01 NO-UNDO.
def var v-e-date as date format "99/99/9999" init today NO-UNDO.
def var v-s-yr like period.yr NO-UNDO.
def var v-e-yr like period.yr NO-UNDO.
def var v-sumdet as LOG NO-UNDO.
def var v-answer as LOG NO-UNDO.
DEF VAR li-dscr AS INT NO-UNDO.
DEF VAR ld-per-start AS DATE NO-UNDO.

DEF VAR tacct LIKE gltrans.actnum  LABEL "    To Account Number" NO-UNDO.
DEF VAR facct LIKE gltrans.actnum  LABEL "  From Account Number" NO-UNDO.
DEF VAR op AS CHAR FORMAT "x" INITIAL "S" LABEL "  S)ummary or D)etail?" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR acct-hdr-printed AS LOG NO-UNDO.

form  account.actnum format "x(75)" open-amt to 131
      with frame r-cmon down stream-io width 132 no-labels no-box no-underline.


assign
 str-tit2 = "G L  Account History" 
 {sys/inc/ctrtext.i str-tit2 126}

 facct    = begin_acct
 tacct    = end_acct
 v-s-date = begin_date
 v-e-date = end_date
 v-sumdet = not tb_detailed
    
 str-tit3 = "Date Range: " + STRING(v-s-date,"99/99/9999") + "-" +
                             STRING(v-e-date,"99/99/9999")
 {sys/inc/ctrtext.i str-tit3 146}
 hdr-tit  = "Account Number             Journal  Reference                                             Date         Amount                              Balance"
 hdr-tit2 = "                   Run #"
 hdr-tit3 = fill("-",146)
 uperiod = period
 udate   = today.
       
/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

find first period where period.company = cocode and
                        period.pst <= v-s-date and
                        period.pend >= v-s-date no-lock no-error.
if avail period then
  assign uperiod = period.pnum
         v-s-yr = period.yr
         ld-per-start = period.pst.

find first period where period.company = cocode and
                        period.pst <= v-e-date and
                        period.pend >= v-e-date no-lock no-error.
if avail period then
   v-e-yr = period.yr.
         
{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Account Number,Account Description,Run #,Journal,"
              + "Reference, , ,Date,Amount,Balance, ".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


   DISPLAY "" WITH FRAME r-top.

   for each account where account.company = cocode and
                          account.actnum ge facct and
                          account.actnum le tacct no-lock:
      if line-counter > page-size - 2 then page.

      acct-hdr-printed = NO.

      run gl/gl-open1.p (recid(account), v-s-yr, v-s-date, uperiod,
                         output open-amt).

      FOR EACH glhist fields(tr-amt) NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.tr-date >= ld-per-start
            AND glhist.tr-date < v-s-date
            AND (glhist.jrnl   NE "AUTODIST" OR NOT tb_exc-auto):
        open-amt = open-amt + glhist.tr-amt.
      END.

      FOR EACH gltrans fields(tr-amt) NO-LOCK
          WHERE gltrans.company EQ account.company
            AND gltrans.actnum  EQ account.actnum
            AND gltrans.tr-date >= ld-per-start
            AND gltrans.tr-date < v-s-date
            AND (gltrans.jrnl   NE "AUTODIST" OR NOT tb_exc-auto):
        open-amt = open-amt + gltrans.tr-amt.
      END.

      display string(account.actnum) + "  " + account.dscr format "x(75)" @
              account.actnum
              open-amt with frame r-cmon.
      down.

      tot-all = tot-all + open-amt.

    if not v-sumdet then do:
      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum and
                                        glhist.tr-date >= v-s-date and
                                        glhist.tr-date <= v-e-date and
                                        (glhist.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
      break by glhist.tr-date by glhist.jrnl by glhist.tr-num:

         assign tmp-dscr = trim(glhist.tr-dscr).

         IF glhist.jrnl EQ "MCSHREC" THEN DO:
           RELEASE ar-mcash.
           li-dscr = INT(tmp-dscr) NO-ERROR.
           IF NOT ERROR-STATUS:ERROR THEN
           FIND FIRST ar-mcash WHERE ar-mcash.m-no EQ li-dscr NO-LOCK NO-ERROR.
           IF AVAIL ar-mcash THEN tmp-dscr = ar-mcash.payer + " Rec# " + tmp-dscr.
         END.

         put space(19)
             glhist.tr-num format "9999999" space(1)
             glhist.jrnl space(1)
             tmp-dscr FORMAT "X(54)"
             glhist.tr-date FORMAT "99/99/99"
             glhist.tr-amt skip.

         IF tb_excel THEN
         DO:
            IF NOT acct-hdr-printed THEN
            DO:
               acct-hdr-printed = YES.
               RUN excel-acct-proc(INPUT open-amt).
            END.

            RUN excel-det-proc(INPUT tmp-dscr,
                               INPUT glhist.tr-num,
                               INPUT glhist.jrnl,
                               INPUT glhist.tr-date,
                               INPUT glhist.tr-amt).
         END.

         ASSIGN
            tot-all  = tot-all  + glhist.tr-amt
            tot-tx   = tot-tx   + glhist.tr-amt
            tot-act  = tot-act  + glhist.tr-amt.
      end.
      FOR EACH gltrans NO-LOCK
          WHERE gltrans.company EQ account.company
            AND gltrans.actnum  EQ account.actnum and
                                        gltrans.tr-date >= v-s-date and
                                        gltrans.tr-date <= v-e-date and
                                        (gltrans.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
        break by gltrans.tr-date by gltrans.jrnl by gltrans.trnum:

        assign tmp-dscr = trim(gltrans.tr-dscr).

         IF gltrans.jrnl EQ "MCSHREC" THEN DO:
           RELEASE ar-mcash.
           li-dscr = INT(tmp-dscr) NO-ERROR.
           IF NOT ERROR-STATUS:ERROR THEN
           FIND FIRST ar-mcash WHERE ar-mcash.m-no EQ li-dscr NO-LOCK NO-ERROR.
           IF AVAIL ar-mcash THEN tmp-dscr = ar-mcash.payer + " Rec# " + tmp-dscr.
         END.

         put space(19)
             gltrans.trnum format "9999999" space(1)
             gltrans.jrnl space(1)
             tmp-dscr FORMAT "X(54)"
             gltrans.tr-date FORMAT "99/99/99"
             gltrans.tr-amt skip.

         IF tb_excel THEN
         DO:
            IF NOT acct-hdr-printed THEN
            DO:
               acct-hdr-printed = YES.
               RUN excel-acct-proc(INPUT open-amt).
            END.

            RUN excel-det-proc(INPUT tmp-dscr,
                               INPUT gltrans.trnum,
                               INPUT gltrans.jrnl,
                               INPUT gltrans.tr-date,
                               INPUT gltrans.tr-amt).
         END.

         ASSIGN
            tot-all  = tot-all  + gltrans.tr-amt
            tot-tx   = tot-tx   + gltrans.tr-amt
            tot-act  = tot-act  + gltrans.tr-amt.
      end.
    end.
    else
    do:

      FOR EACH glhist NO-LOCK
          WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum and
                                        glhist.tr-date >= v-s-date and
                                        glhist.tr-date <= v-e-date and
                                        (glhist.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
        break by glhist.tr-date by glhist.jrnl by glhist.tr-num:

        if line-counter > page-size - 2 then page.
        if last-of(glhist.tr-num) then
        do:
        assign tmp-amt = 0.
        for each xglhist FIELDS(tr-amt) WHERE
            xglhist.company = glhist.company and
            xglhist.actnum = glhist.actnum and
            xglhist.period = glhist.period and
            xglhist.tr-date = glhist.tr-date and
            xglhist.tr-num = glhist.tr-num and
            xglhist.jrnl = glhist.jrnl no-lock:

            assign tmp-amt = tmp-amt + xglhist.tr-amt.
        end.

        if glhist.jrnl = "CASHR" then
          assign tmp-dscr = "CASH RECEIPTS                           ".
        else if glhist.jrnl = "APCKR" then
          assign tmp-dscr = "ACCOUNTS PAYABLE CHECK REGISTER         ".
        else if glhist.jrnl = "GENERAL" then
          assign tmp-dscr = "GENERAL                                 ".
        else if glhist.jrnl = "OEINV" then
          assign tmp-dscr = "ORDER ENTRY INVOICE                     ".
        else if glhist.jrnl = "ARINV" then
          assign tmp-dscr = "ACCOUNTS RECEIVABLE INVOICE             ".
        else if glhist.jrnl = "MCSHREC" then
          assign tmp-dscr = "MISC CASH RECEIPTS                      ".
        else if glhist.jrnl = "CDISB" then
          assign tmp-dscr = "CASH DISBURSEMENT                       ".
        else if glhist.jrnl = "APMEM" then
          assign tmp-dscr = "ACCOUNTS PAYABLE MEMO                   ".
        else if glhist.jrnl = "CRMEM" then
          assign tmp-dscr = "CREDIT MEMO                             ".
        else if glhist.jrnl = "ACPAY" then
          assign tmp-dscr = "ACCOUNTS PAYABLE                        ".
        else if glhist.jrnl = "APVOIDCK" then
          assign tmp-dscr = "ACCOUNTS PAYABLE VOID CHECK             ".
        else if glhist.jrnl = "ADJUST" then
          assign tmp-dscr = "ADJUSTMENT                              ".
        else if glhist.jrnl = "AUTODIST" then
          assign tmp-dscr = "AUTOMATIC DISTRIBUTION                  ".
        else
          assign tmp-dscr = "                                        ".
         
         put space(19)
                glhist.tr-num format "9999999" space(1)
                glhist.jrnl space(1)
                tmp-dscr FORMAT "X(54)"
                glhist.tr-date FORMAT "99/99/99"
                tmp-amt skip.

         IF tb_excel THEN
         DO:
            IF NOT acct-hdr-printed THEN
               DO:
                  acct-hdr-printed = YES.
                  RUN excel-acct-proc(INPUT open-amt).
               END.
           
            RUN excel-det-proc(INPUT tmp-dscr,
                               INPUT glhist.tr-num,
                               INPUT glhist.jrnl,
                               INPUT glhist.tr-date,
                               INPUT tmp-amt).
         END.
        end.
        
        ASSIGN
           tot-all  = tot-all  + glhist.tr-amt
           tot-tx   = tot-tx   + glhist.tr-amt
           tot-act  = tot-act  + glhist.tr-amt.
      end. /* each glhist */

      FOR EACH gltrans NO-LOCK
          WHERE gltrans.company EQ account.company
            AND gltrans.actnum  EQ account.actnum and
                                        gltrans.tr-date >= v-s-date and
                                        gltrans.tr-date <= v-e-date and
                                        (gltrans.jrnl NE "AUTODIST" or
                                         NOT tb_exc-auto)
      break by gltrans.tr-date by gltrans.jrnl by gltrans.trnum:
        if line-counter > page-size - 2 then page.
        if last-of(gltrans.trnum) then
        do:
        assign tmp-amt = 0.
        for each xgltrans FIELDS(tr-amt)
            where xgltrans.company = cocode and
                                xgltrans.actnum = gltrans.actnum and
                                xgltrans.period = gltrans.period and
                                xgltrans.tr-date = gltrans.tr-date and
                                xgltrans.trnum = gltrans.trnum and
                                xgltrans.jrnl = gltrans.jrnl no-lock:

          assign tmp-amt = tmp-amt + xgltrans.tr-amt.
        end.

        if gltrans.jrnl = "CASHR" then
          assign tmp-dscr = "CASH RECEIPTS                           ".
        else if gltrans.jrnl = "APCKR" then
          assign tmp-dscr = "ACCOUNTS PAYABLE CHECK REGISTER         ".
        else if gltrans.jrnl = "GENERAL" then
          assign tmp-dscr = "GENERAL                                 ".
        else if gltrans.jrnl = "OEINV" then
          assign tmp-dscr = "ORDER ENTRY INVOICE                     ".
        else if gltrans.jrnl = "ARINV" then
          assign tmp-dscr = "ACCOUNTS RECEIVABLE INVOICE             ".
        else if gltrans.jrnl = "MCSHREC" then
          assign tmp-dscr = "MISC CASH RECEIPTS                      ".
        else if gltrans.jrnl = "CDISB" then
          assign tmp-dscr = "CASH DISBURSEMENT                       ".
        else if gltrans.jrnl = "APMEM" then
          assign tmp-dscr = "ACCOUNTS PAYABLE MEMO                   ".
        else if gltrans.jrnl = "CRMEM" then
          assign tmp-dscr = "CREDIT MEMO                             ".
        else if gltrans.jrnl = "ACPAY" then
          assign tmp-dscr = "ACCOUNTS PAYABLE                        ".
        else if gltrans.jrnl = "APVOIDCK" then
          assign tmp-dscr = "ACCOUNTS PAYABLE VOID CHECK             ".
        else if gltrans.jrnl = "ADJUST" then
          assign tmp-dscr = "ADJUSTMENT                              ".
        else if gltrans.jrnl = "AUTODIST" then
          assign tmp-dscr = "AUTOMATIC DISTRIBUTION                  ".
        else
          assign tmp-dscr = "                                        ".

        put space(19)
            gltrans.trnum format "9999999" space(1)
            gltrans.jrnl space(1)
            tmp-dscr FORMAT "X(54)"
            gltrans.tr-date FORMAT "99/99/99"
            tmp-amt skip.

        IF tb_excel THEN
        DO:
           IF NOT acct-hdr-printed THEN
              DO:
                 acct-hdr-printed = YES.
                 RUN excel-acct-proc(INPUT open-amt).
              END.

           RUN excel-det-proc(INPUT tmp-dscr,
                              INPUT gltrans.trnum,
                              INPUT gltrans.jrnl,
                              INPUT gltrans.tr-date,
                              INPUT tmp-amt).
        END.
        end.

         ASSIGN
            tot-all  = tot-all  + gltrans.tr-amt
            tot-tx   = tot-tx   + gltrans.tr-amt
            tot-act  = tot-act  + gltrans.tr-amt.
      end. /* each gltrans */
    end.

    IF tb_excel AND NOT acct-hdr-printed AND open-amt NE 0 THEN
       RUN excel-acct-proc(INPUT open-amt).

    if tot-act ne 0 then
    DO:
      put tot-act to 111
          tot-act + open-amt format "->>>,>>>,>>>,>>9.99" to 131 " *" skip(1).

      IF tb_excel THEN
         RUN excel-total-proc(INPUT "", INPUT tot-act, INPUT open-amt).
    END.
    else
    if open-amt ne 0 THEN
       put skip(1).

    down.
    tot-act = 0.
   end. /* each account */
   display "TOTAL" to 95

           tot-all to 131
           with frame r-cmon3 no-labels no-box stream-io width 146.

IF tb_excel THEN DO:

  RUN excel-total-proc(INPUT "TOTAL", INPUT tot-all, INPUT 0).

  OUTPUT STREAM excel CLOSE.
  
END.



/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

PROCEDURE excel-det-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-tmp-dscr AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-tr-num AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-jrnl AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-date AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER ip-amt AS DEC NO-UNDO.
   
   DEF VAR v-ref1 AS CHAR NO-UNDO.
   DEF VAR v-ref2 AS CHAR NO-UNDO.
   DEF VAR v-ref3 AS CHAR NO-UNDO.
   DEF VAR inv-index AS INT NO-UNDO.

   ASSIGN
     v-ref1 = ip-tmp-dscr
     v-ref2 = ""
     v-ref3 = ""
     inv-index = INDEX(v-ref1,"Inv#").

   IF inv-index > 0 THEN
   DO:
      ASSIGN
        v-ref1 = SUBSTRING(v-ref1,1,inv-index - 1)
        v-ref2 = SUBSTRING(ip-tmp-dscr,inv-index,13)
        v-ref3 = SUBSTRING(ip-tmp-dscr,inv-index + 13).
   END.

   PUT STREAM excel UNFORMATTED
       '"' ""                                      '",'
       '"' ""                                      '",'
       '"' STRING(ip-tr-num,"9999999")             '",'
       '"' ip-jrnl                                 '",'
       '"' v-ref1                                  '",'
       '"' v-ref2                                  '",'
       '"' v-ref3                                  '",'
       '"' ip-date                                 '",'
       '"' STRING(ip-amt,"(>>,>>>,>>9.99)")        '",'
       '"' ""                                      '",'
       '"' ""                                      '",'
       SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-total-proc C-Win 
PROCEDURE excel-total-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-text     AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ip-tot-act  AS DEC NO-UNDO.
  DEFINE INPUT PARAMETER ip-open-amt AS DEC NO-UNDO.
  
  DEF VAR i AS INT NO-UNDO.

  DO i = 1 TO 8:
     PUT STREAM excel UNFORMATTED
         '"' "" '",'.
  END.
  
  IF ip-text = "" THEN
     PUT STREAM excel UNFORMATTED
         '"' STRING(ip-tot-act,"->>>,>>>,>>>,>>9.99")               '",'
         '"' STRING(ip-tot-act + ip-open-amt,"->>>,>>>,>>>,>>9.99") '",'
         '"' "*"                                                    '",'
      SKIP(1).
  ELSE
     PUT STREAM excel UNFORMATTED
         '"' "TOTAL"                                  '",'
         '"' STRING(ip-tot-act,"->>>,>>>,>>>,>>9.99") '",'
      SKIP(1).
END PROCEDURE.
