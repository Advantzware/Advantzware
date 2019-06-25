

/*------------------------------------------------------------------------
    File        : tribal.p
    Purpose     : Trial Balance
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttGLTrialBalance NO-UNDO
        FIELD tribal AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsGLTrialBalance FOR ttGLTrialBalance.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsdate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegsub        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmendact        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendsub        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmZerobal       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsortsub       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmactlevl       AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLTrialBalance.

     IF prmUser     = ? THEN ASSIGN    prmUser       = "".   
     IF prmAction   = ? THEN ASSIGN    prmAction     = "". 
     IF prmtrnsdate = ? THEN ASSIGN    prmtrnsdate   = "". 
     IF prmperiod   = ? THEN ASSIGN    prmperiod     = 0. 
     IF prmbegact   = ? THEN ASSIGN    prmbegact     = "".
     IF prmbegsub   = ? THEN ASSIGN    prmbegsub     = 0. 
     IF prmendact   = ? THEN ASSIGN    prmendact     = "". 
     IF prmendsub   = ? THEN ASSIGN    prmendsub     = 0.
     IF prmZerobal  = ? THEN ASSIGN    prmZerobal    = "".
     IF prmsortsub  = ? THEN ASSIGN    prmsortsub    = "". 
     IF prmactlevl  = ? THEN ASSIGN    prmactlevl    = 0.
     IF prmOut      = ? THEN ASSIGN    prmOut        = "".




DEFINE VARIABLE begin_acct-no   AS CHARACTER FORMAT "x(25)" INITIAL "0" NO-UNDO.
DEFINE VARIABLE begin_sub-acct  AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 NO-UNDO.
DEFINE VARIABLE end_acct-no     AS CHARACTER FORMAT "x(25)" INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE end_sub-acct    AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 NO-UNDO.
DEFINE VARIABLE tran-date       AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tran-period     AS INTEGER FORMAT ">9":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-sub-acct-lvl  AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE tb_sub-acct     AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_sup-zero     AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.

DEF VAR lv-audit-dir AS CHAR NO-UNDO.
DEF VAR lv-post AS LOG NO-UNDO.
DEFINE BUFFER bf-chk FOR ap-chk.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR lv-comp-curr AS cha NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
IF AVAIL company THEN lv-comp-curr = company.curr-code.

def new shared var v-post as log init NO NO-UNDO.
def new shared var v-trnum as INT NO-UNDO.


DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.

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
 vuser     = prmUser
 v-today   = TODAY 
 g_company = cocode
 g_user    = prmUser
 tran-date   = TODAY .
 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  ASSIGN  
      init-dir    = v-webrootpath .

DEF VAR v-invalid AS LOG NO-UNDO.
def var v-download as log init no no-undo.
def var v-prior as log init no no-undo.

def buffer tmp-per for period.

def stream s-temp.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.

DEF STREAM excel.


find first sys-ctrl where
  sys-ctrl.company = cocode and
  sys-ctrl.name    = "TRIALBAL"
  no-lock no-error.
IF NOT(AVAILABLE(sys-ctrl)) then
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "TRIALBAL"
    sys-ctrl.descrip = "Download Trial Balance to Excel?".
  cError =  "Download Trial Balance to Excel?".
end.
v-download = sys-ctrl.log-fld.


find first company where company.company eq cocode no-lock.
v-sub-acct-lvl = company.acc-level.

find first tmp-per where tmp-per.company eq cocode
                     and tmp-per.pstat
                     /*and tmp-per.yr      eq (period.yr - 1)*/
                   no-lock no-error.
if avail tmp-per or not company.yend-per then
  assign v-prior = true.

/*ASSIGN
prmtrnsdate = string(TODAY).*/


    

  
  IF prmAction = "tribal" THEN DO:
     
        ASSIGN
       begin_acct-no  =  prmbegact                         
       begin_sub-acct =  prmbegsub                         
       end_acct-no    =  prmendact                         
       end_sub-acct   =  prmendsub                               
       tran-date      =  date(prmtrnsdate)
       tran-period    =  prmperiod                                     
       v-sub-acct-lvl =  prmactlevl
       tb_sub-acct    =  IF prmsortsub = "Yes" THEN TRUE ELSE FALSE 
       tb_sup-zero    =  IF prmZerobal = "Yes" THEN TRUE ELSE FALSE
       tb_excel       =  IF prmOut  = "Yes" THEN TRUE ELSE FALSE  .

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "TriBal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "TriBal" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .

       find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then prmperiod = (period.pnum).

    ELSE DO:
      cError = "No Defined Period Exists".
      RETURN.
    end. 


       IF v-print-fmt EQ "Pacific" OR v-print-fmt EQ "Xprint" OR v-print-fmt = "southpak"
            THEN is-xprint-form = YES.     
        ELSE is-xprint-form = NO. 

             

             run run-report. 

  
        
   
  CREATE ttGLTrialBalance.
    IF prmOut = "Yes" THEN
    ASSIGN ttGLTrialBalance.tribal = vExcalFile .
  ELSE
    ASSIGN ttGLTrialBalance.tribal = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE run-report :
def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var subac as int format ">>>>>>>>9" NO-UNDO.
def var subac-lvl as int format "9" NO-UNDO.
def var fsubac as int format ">>>>>>>>9" init 0 NO-UNDO.
def var tsubac as int format ">>>>>>>>9" init 999999999 NO-UNDO.
def var aclevel as INT NO-UNDO.
def var cyr as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tcyr as DEC NO-UNDO.
def var dadj as char label "DB Adjust" format "x(10)" init "__________" NO-UNDO.
def var cadj as char label "CR Adjust" format "x(10)" init "__________" NO-UNDO.
def var bsht as char label "Bal Sheet" format "x(10)" init "__________" NO-UNDO.
def var incs as char label "Income Stat" format "x(11)" init "___________" NO-UNDO.
def var v-rep-tot as dec no-undo.
def var vyear like period.yr no-undo.
def var vdate like gltrans.tr-date no-undo.
def var v-fisc-yr like period.yr no-undo.

def var tacct like gltrans.actnum  label "      To Account Number" NO-UNDO.
def var facct like gltrans.actnum  label "    From Account Number" NO-UNDO.
def var ptd-value as dec format "->>>,>>>,>>9.99" init 0 no-undo.
def var tot-ptd as dec format "->>>,>>>,>>9.99" init 0 no-undo.
def var suppress-zero as logical no-undo init true
    label "Suppress Zero Balances?".
def var break-flag as log init no no-undo.
def var start-lvl as int init 0 no-undo.
def var temp_fid as char no-undo.

def var str_buffa as char no-undo.
def var v-first as log init yes no-undo.
def var v-hdr as char initial
"Account#,Description,PTD,YTD,DB Adjust,CR Adjust,Bal Sheet,Income Stat" no-undo.
def var v-comma as char format "x" initial "," no-undo.


 /*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

 {sys/inc/outprint.i VALUE(lines-per-page)}

 

IF tb_excel THEN DO:
   OUTPUT STREAM excel TO VALUE(fi_file).
   EXPORT STREAM excel DELIMITER ","
       "Account Number"
       "Description"
       "PTD"
       "YTD"
       "DB Adjust"
       "CR Adjust"
       "Bal Sheet"
       "Income Stat"
       SKIP.
END. 

/* create a unique filename ... */
temp_fid = 
IF OPSYS eq 'win32' THEN
"TryB" + substring(string(TODAY,"999999"),1,6) + ".csv"
  ELSE
"TryB" + substring(string(TODAY,"999999"),1,4) + ".csv".

time_stamp = string(TIME, "hh:mmam").

{sys/form/r-topw.f}

ASSIGN facct = begin_acct-no
       tacct = END_acct-no
       subac-lvl = v-sub-acct-lvl
       fsubac = begin_sub-acct
       tsubac = END_sub-acct
       break-flag = tb_sub-acct
       suppress-zero = tb_sup-zero.

find last period
    where period.company eq cocode
      and period.pst     le tran-date
      and period.pend    ge tran-date
      and period.pnum    eq tran-period
    no-lock.

ASSIGN
 vyear = period.yr
 vdate = period.pst.

blok:
DO:
   assign
     str-tit  = company.name
     str-tit2 = "TRIAL  BALANCE"
     str-tit3 = "Period " + string(tran-period,"99")
     v-rep-tot = 0
     {sys/inc/ctrtext.i str-tit  112}
     {sys/inc/ctrtext.i str-tit2 112}
     {sys/inc/ctrtext.i str-tit3 132}.

    display str-tit3 format "x(130)" skip(1) with frame r-top STREAM-IO.

    if break-flag and subac-lvl ne 1 then do:
      start-lvl = 0.
      do i = 1 to (subac-lvl - 1):
        start-lvl = start-lvl + company.acc-dig[i].
      end.
      start-lvl = start-lvl + subac-lvl - 1.
    end.
    if start-lvl le 1 then start-lvl = 1.
    
    if v-download /*and v-first*/  then
    do:         
          assign str_buffa = ""
                 v-first = no.        
          output stream s-temp TO VALUE(temp_fid). 
          {gl/outstr.i v-hdr 1 70}.
          PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
    end.

    for each account
        where account.company eq cocode
          and account.actnum  ge facct
          and account.actnum  le tacct
        no-lock
        by substr(account.actnum,start-lvl) with width 132:

        subac = if subac-lvl eq 1 then account.n1 else
              if subac-lvl eq 2 then account.n2 else
              if subac-lvl eq 3 then account.n3 else
              if subac-lvl eq 4 then account.n4 else account.n5.
        if subac lt fsubac or subac gt tsubac then next.

        ptd-value = 0.
        view frame r-top.
      
        run gl/gl-open1.p (recid(account), vyear, tran-date, tran-period,
                           output cyr).
 
        for each glhist no-lock
            where glhist.company eq account.company
              and glhist.actnum  eq account.actnum
              and glhist.tr-date ge vdate 
              and glhist.tr-date le tran-date:

          assign
           ptd-value = ptd-value + glhist.tr-amt
           tot-ptd   = tot-ptd   + glhist.tr-amt
           cyr       = cyr + glhist.tr-amt.
        end.

        for each gltrans no-lock
            where gltrans.company eq account.company
              and gltrans.actnum  eq account.actnum
              and gltrans.tr-date ge vdate 
              and gltrans.tr-date le tran-date:

          assign
           ptd-value = ptd-value + gltrans.tr-amt
           tot-ptd   = tot-ptd   + gltrans.tr-amt
           cyr       = cyr + gltrans.tr-amt.
        end.

        if not suppress-zero or cyr ne 0 or ptd-value ne 0 then
        do:
           display skip(1)
                account.actnum + "  " + account.dscr format "x(45)"
                      label "Account Number           Description"
                ptd-value  format "->>>,>>>,>>9.99" label "PTD      "
                cyr format "->>>,>>>,>>>,>>9.99" label "YTD       "
                dadj cadj bsht incs
                with centered width 132 STREAM-IO.

IF tb_excel THEN 
   EXPORT STREAM excel DELIMITER ","
       account.actnum
       account.dscr
       ptd-value
       cyr
       SKIP.

           if v-download then
           do:
              assign str_buffa = "".
              assign str_buffa = trim(account.actnum) + v-comma 
                           + trim(account.dscr)   + v-comma
                           + trim(string(ptd-value,'->>>>>>>>9.99')) + v-comma
                           + trim(string(cyr,'->>>>>>>>9.99'))       + v-comma 
                           + v-comma + v-comma + v-comma.
              PUT STREAM s-temp UNFORMATTED str_buffa SKIP.
           end.
        END.
      
        tcyr = tcyr + cyr.      
    end. /* each account */
    
    put skip(1) "===============" to 61 "===================" to 81 skip
        "TRIAL BALANCE:" AT 10 tot-ptd format "->>>,>>>,>>9.99" to 61
                                  tcyr format "->>>,>>>,>>>,>>9.99" to 81
        " " dadj " " cadj " " bsht " " incs skip(1).

    if tcyr eq 0 then cError = "TRIAL BALANCE IN BALANCE".
    else              cError = "TRIAL BALANCE NOT IN BALANCE".
  
    /*if v-download then  */
       output stream s-temp close.


 end.
 
  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     
 END. 



 end procedure.
