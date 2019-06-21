

/*------------------------------------------------------------------------
    File        : glmondetl.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttCurrentMonthDetail NO-UNDO
        FIELD detail AS CHAR
        FIELD post AS CHAR.

DEFINE DATASET dsCurrentMonthDetail FOR ttCurrentMonthDetail.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtnsdate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmdetaild       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmactiv         AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCurrentMonthDetail.

     IF prmUser       = ? THEN ASSIGN     prmUser      = "".   
     IF prmAction     = ? THEN ASSIGN     prmAction    = "". 
     IF prmtnsdate    = ? THEN ASSIGN     prmtnsdate   = "". 
     IF prmperiod     = ? THEN ASSIGN     prmperiod    = 0. 
     IF prmBegact     = ? THEN ASSIGN     prmBegact    = "".  
     IF prmEndact     = ? THEN ASSIGN     prmEndact    = "".  
     IF prmOut        = ? THEN ASSIGN     prmOut       = "". 
     IF prmdetaild    = ? THEN ASSIGN     prmdetaild   = "". 
     IF prmactiv      = ? THEN ASSIGN     prmactiv     = "". 

DEFINE VARIABLE begin_acct     AS CHARACTER FORMAT "X(25)" NO-UNDO.
DEFINE VARIABLE end_acct       AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE tran-date      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period    AS INTEGER FORMAT ">>":U INITIAL 0   NO-UNDO.
DEFINE VARIABLE tb_activ       AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_detailed    AS LOGICAL INITIAL NO NO-UNDO.
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




/* gdm - 10010905 */
DEF STREAM str-exl.

tran-date = today.
     


  IF prmAction = "detail" THEN DO:
    
        ASSIGN
       begin_acct       = prmBegact 
       end_acct         = prmEndact 
       tran-date        = date(prmtnsdate) 
       tran-period      = prmperiod  
       tb_activ         = IF prmdetaild = "Yes" THEN TRUE ELSE FALSE   
       tb_detailed      = IF prmactiv = "Yes" THEN TRUE ELSE FALSE.

       tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
           

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "Journalpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "Journalpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .

        find first period
            where period.company eq COCODE
            and period.pst     le tran-date
            and period.pend    ge tran-date
            no-lock no-error.
        if avail period then do:  
            tran-period = period.pnum.
        end.
        else do:
            cError = "No Defined Period Exists".
            RETURN.
        end.

    
        RUN run-report.
   
   
  CREATE ttCurrentMonthDetail.
  IF prmOut = "Yes" THEN
    ASSIGN ttCurrentMonthDetail.detail = vExcalFile .
  ELSE
    ASSIGN ttCurrentMonthDetail.detail = vTextFile .

  END.
/*****************************************************************************************/
PROCEDURE run-report :
/* --------------------------------------------------- gl/gl-cmon.p 11/92 cd  */
/* g/l current period transaction report                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-topw.f}

def buffer xgltrans for gltrans.

def var tacct like gltrans.actnum  label "    To Account Number".
def var facct like gltrans.actnum  label "  From Account Number".
def var op as char format "!" init "S" label "  S)ummary or D)etail?".
def var inc as log init no label "  Print Accounts with NO Activity?".

def var suppress_zero as logical no-undo init true
    label "Suppress Zero Balances?".

def var tot-all  as   dec format "->>>,>>>,>>>,>>9.99".
def var tot-tx   like tot-all.
def var tot-act  like tot-all.
def var open-amt like tot-all.
def var net-inc  as   dec.
def var tmp-amt  like gltrans.tr-amt.
def var tmp-dscr like gltrans.tr-dscr.
def var v-crdit  like tmp-amt extent 3.
def var v-debit  like v-crdit.
def var str-tit4 as   char no-undo.
def var str-tit5 as   char no-undo.
def var vyear like period.yr no-undo.

/* gdm - 10010905 */
DEF VAR v-excel-hdr  AS CHAR.
DEF VAR v-runbal LIKE open-amt NO-UNDO.

form account.actnum format "x(75)" open-amt to 132
    with frame r-cmon down stream-io width 200 no-labels no-box no-underline.

find first period
    where period.company eq cocode
      and period.pst     le tran-date
      and period.pend    ge tran-date
    no-lock no-error.

assign
 str-tit2 = "Current Month General Ledger" + IF tb_detailed THEN " Detail " ELSE " Summary"
 {sys/inc/ctrtext.i str-tit2 112}
 
 str-tit3 = "Period " + string(tran-period,"99") + " - " +
            string(period.pst) + " to " + string(period.pend)
 {sys/inc/ctrtext.i str-tit3 132}
             
 str-tit4 = "Account Number             Journal  Reference                           Date             Debits        Credits               Balance"
 str-tit5 = "           Jrn#    Run #"
 
 facct = begin_acct
 tacct = end_acct
 op    = string(tb_detailed,"D/S")
 inc   = tb_activ.

find last period
    where period.company eq cocode
      and period.pst     le tran-date
      and period.pend    ge tran-date
      and period.pnum    eq tran-period
    no-lock.
vyear = period.yr.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .

{sys/inc/outprint.i value(lines-per-page)}


  
  display str-tit3      format "x(132)" skip(1)
          str-tit4      format "x(132)" skip
          str-tit5      format "x(132)" skip
          fill("-",132) format "x(132)" skip
          
      with frame r-top.

/* gdm - 10010905 */
IF tb_excel THEN DO:
  ASSIGN v-excel-hdr = "Account Number,Description,Jrn#,Run #,Journal,Reference,Date," +
                       "Debits,Credits,Balance".
  OUTPUT STREAM str-exl TO VALUE(TRIM(fi_file)).
  PUT STREAM str-exl UNFORMATTED v-excel-hdr SKIP.
END. 
/* gdm - 10010905 */

  for each account
      where account.company eq cocode
        and account.actnum  ge facct
        and account.actnum  le tacct
      no-lock
      break by account.actnum:

    run gl/gl-open1.p (recid(account), vyear, tran-date, tran-period,
                       output open-amt).

    find first gltrans no-lock
        where gltrans.company eq cocode
          and gltrans.actnum  eq account.actnum
          and gltrans.period  eq tran-period
        no-error.

    if avail gltrans or inc then do:
      if line-counter gt page-size - 2 then page.
      display string(account.actnum) + "  " + account.dscr format "x(75)" @
              account.actnum open-amt with frame r-cmon.
      down with frame r-cmon.
      if not avail gltrans then put skip(1).
    end.

    /* gdm - 10010905 */
    ASSIGN v-runbal = open-amt.

    for each gltrans no-lock
        where gltrans.company eq cocode
          and gltrans.actnum  eq account.actnum
          and gltrans.period  eq tran-period
        break by gltrans.trnum
              by gltrans.tr-date
              by gltrans.jrnl:

      if gltrans.tr-amt ge 0 then
        assign
         v-debit[1] = v-debit[1] + gltrans.tr-amt
         v-crdit[1] = v-crdit[1] + 0.
      else
        assign
         v-crdit[1] = v-crdit[1] + gltrans.tr-amt
         v-debit[1] = v-debit[1] + 0.

      /* gdm - 10010905 */
      ASSIGN v-runbal = v-runbal + v-crdit[1] + v-debit[1].

      if last-of(gltrans.trnum) or op eq "D" then do:
        if line-counter gt page-size - 2 then page.

        ASSIGN
        tmp-dscr = if op eq "D" then gltrans.tr-dscr                   else
                   if gltrans.jrnl eq "CASHR"                          then
                     "CASH RECEIPTS"                                   else
                   if gltrans.jrnl eq "APCKR"                          then
                     "ACCOUNTS PAYABLE CHECK REGISTER"                 else
                   if gltrans.jrnl eq "GENERAL"                        then
                     "GENERAL"                                         else
                   if gltrans.jrnl eq "ARINV"                          then
                     "ACCOUNTS RECEIVABLE INVOICE"                     else
                   if gltrans.jrnl eq "MCSHREC"                        then
                     "MISC CASH RECEIPTS"                              else
                   if gltrans.jrnl eq "CDISB"                          then
                     "CASH DISBURSEMENT"                               else
                     ""
                     
        tmp-dscr = if op eq "D" then gltrans.tr-dscr                   else
                   if tmp-dscr ne "" then tmp-dscr                     else
                   if gltrans.jrnl eq "APMEM"                          then
                     "ACCOUNTS PAYABLE MEMO"                           else
                   if gltrans.jrnl eq "CRMEM"                          then
                     "CREDIT MEMO"                                     else
                   if gltrans.jrnl eq "DBMEM"                          then
                     "DEBIT MEMO"                                      else
                   if gltrans.jrnl eq "ACPAY"                          then
                     "ACCOUNTS PAYABLE"                                else
                   if gltrans.jrnl eq "APVOIDCK"                       then
                     "ACCOUNTS PAYABLE VOID CHECK"                     else
                   if gltrans.jrnl eq "OEINV"                          then
                     "ORDER ENTRY INVOICE"                             else
                   if gltrans.jrnl eq "JCOST"                          then
                     "PRODUCTION JOB COSTING"                          else
                     ""
        
        tmp-dscr = if op eq "D" then gltrans.tr-dscr                   else
                   if tmp-dscr ne "" then tmp-dscr                     else
                   if gltrans.jrnl eq "ADJUST"                         then
                     "ADJUSTMENT"                                      else
                     "".

        /* gdm - 10010905 */
        IF tb_excel THEN DO:
          PUT STREAM str-exl UNFORMATTED 
              STRING(account.actnum)  ","  
              REPLACE(account.dscr, "," ," ")  ",,"               
              gltrans.trnum ",".

          IF op EQ "D" 
            THEN PUT STREAM str-exl UNFORMATTED  gltrans.jrnl ",".
            ELSE PUT STREAM str-exl UNFORMATTED  " " ",".

          PUT STREAM str-exl UNFORMATTED 
              REPLACE(TRIM(tmp-dscr),","," ") ","
              STRING(gltrans.tr-date,"99/99/9999") ",".

          IF v-debit[1] NE 0
            THEN PUT STREAM str-exl UNFORMATTED v-debit[1] ",".
            ELSE PUT STREAM str-exl UNFORMATTED " " ",".
          IF v-crdit[1] NE 0 
            THEN PUT STREAM str-exl UNFORMATTED v-crdit[1] ",".
            ELSE PUT STREAM str-exl UNFORMATTED " " ",".

          PUT STREAM str-exl UNFORMATTED v-runbal.


          PUT STREAM str-exl UNFORMATTED  SKIP.
          
        END.
        /* gdm - 10010905 */

        if op eq "D" and gltrans.jrnl eq "GENERAL" and
           length(tmp-dscr) ge 11 then
        do:   
          if op eq "D" and gltrans.jrnl eq "GENERAL" and
             substring(tmp-dscr,(length(tmp-dscr) - 10),4) eq "JRN#" then
          do:
            display space(11)
                    substring(tmp-dscr,(length(tmp-dscr) - 6),length(tmp-dscr))
                                    format "x(7)"
                    gltrans.trnum   format "9999999"
                    gltrans.jrnl    when op eq "D"
                    substring(tmp-dscr,1,length(tmp-dscr) - 11) format "x(35)"
                    gltrans.tr-date format "99/99/99"
                    v-debit[1]      when v-debit[1] ne 0
                    v-crdit[1]      when v-crdit[1] ne 0
                with no-box stream-io width 200 no-attr-space no-labels frame f1.
            down with frame f1.            
          end.
          else                          
          do:
            display space(19)
                    gltrans.trnum   format "9999999"
                    gltrans.jrnl    when op eq "D"
                    tmp-dscr        format "x(35)"
                    gltrans.tr-date format "99/99/99"
                    v-debit[1]      when v-debit[1] ne 0
                    v-crdit[1]      when v-crdit[1] ne 0
                with no-box stream-io width 200 no-attr-space no-labels frame f2.
            down with frame f2.
          end.
        end.
        else                          
        do:
          display space(19)
                  gltrans.trnum   format "9999999"
                  gltrans.jrnl    when op eq "D"
                  tmp-dscr        format "x(35)"
                  gltrans.tr-date format "99/99/99"
                  v-debit[1]      when v-debit[1] ne 0
                  v-crdit[1]      when v-crdit[1] ne 0
              with no-box stream-io width 200 no-attr-space no-labels frame f2.
          down with frame f2.
        end.
        assign v-debit[1] = 0
               v-crdit[1] = 0.

      end.

      if gltrans.tr-amt ge 0 then
        assign
          v-debit[2] = v-debit[2] + gltrans.tr-amt
          v-crdit[2] = v-crdit[2] + 0.
      else
        assign
          v-debit[2] = v-debit[2] + 0
          v-crdit[2] = v-crdit[2] + gltrans.tr-amt.
/*
      assign
       v-debit[2] = v-debit[2] + v-debit[1]
       v-crdit[2] = v-crdit[2] + v-crdit[1].
*/

      if last(gltrans.trnum) then do:
        put v-debit[2]                                   to 95
            v-crdit[2]                                   to 110
            v-debit[2] +
            v-crdit[2] +
            open-amt        format "->>>,>>>,>>>,>>9.99" to 132 " *" skip
            skip(1).
        down.

        /* gdm - 10010905 */
        IF tb_excel THEN DO:
          PUT STREAM str-exl UNFORMATTED 
              .
        END.
        /* gdm - 10010905 end */

                /*djk*/
        tot-all = tot-all +  v-debit[2] + v-crdit[2] + open-amt.

        assign
         v-debit[3] = v-debit[3] + v-debit[2]
         v-crdit[3] = v-crdit[3] + v-crdit[2]
         v-debit[2] = 0
         v-crdit[2] = 0.

      end.
    end.

    if last(account.actnum) then
      display "TOTAL"    to 71
              v-debit[3] to 95  format "(>>>,>>>,>>>,>>9.99)"
              tot-all    to 132 skip
              v-crdit[3] to 110 format "(>>>,>>>,>>>,>>9.99)"
          with frame r-cmon3 no-labels no-box stream-io width 200.
  end. /* each account */

/* gdm - 10010905 */
IF tb_excel THEN DO:
   OUTPUT STREAM str-exl CLOSE.

  
END.

end procedure.
