

/*------------------------------------------------------------------------
    File        : glmnclo.p
    Purpose     : Close Month
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttGLCloseMonth NO-UNDO
        FIELD clmon AS CHAR.
        
       

DEFINE DATASET dsGLCloseMonth FOR ttGLCloseMonth.

    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtrnsDate      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  ip_oktogo        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLCloseMonth.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmAction      = ? THEN ASSIGN     prmAction      = "". 
     IF prmtrnsDate    = ? THEN ASSIGN     prmtrnsDate    = "". 
     IF prmperiod      = ? THEN ASSIGN     prmperiod      = 0. 
     IF prmOut         = ? THEN ASSIGN     prmOut         = "". 




DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

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
 tran-date = TODAY .


 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  ASSIGN  
      init-dir    = v-webrootpath .

def var save_id as recid.
def var time_stamp as ch.

def var start-date as date initial 01/01/1901 NO-UNDO.
def var end-date as date initial 01/01/1901 NO-UNDO.
def var tot-all  as dec format "->>>,>>>,>>>,>>9.99" NO-UNDO.
def var tot-tx   like tot-all NO-UNDO.
def var tot-act  like tot-all NO-UNDO.
def var tot-jrnl like tot-all NO-UNDO.
def var open-amt like tot-all NO-UNDO.
def var net-inc  as dec NO-UNDO.
def var per-open as inte format ">9" NO-UNDO.
def var per-status like period.pstat NO-UNDO.
def var fiscal-yr like period.yr NO-UNDO.

def buffer b-racct for account.
def buffer b-cacct for account.
DEF VAR udate AS DATE NO-UNDO.
DEF VAR uperiod AS INT NO-UNDO.
DEF VAR choice AS LOG NO-UNDO.

ASSIGN time_stamp = string(time,"hh:mmam") .

find first period where period.company eq cocode
                      and period.pst     le tran-date
                      and period.pend    ge tran-date
                      no-lock no-error.
  if avail period then tran-period = (period.pnum).

  find first company where company.company eq cocode.
  if not company.yend-per then do:
     cError = "PRIOR YEAR NOT CLOSED.  MUST CLOSE PRIOR YEAR!!!" .
     return.
  end.


  


  IF prmAction = "clmon" THEN DO:
MESSAGE "t1 " .
      ASSIGN
          tran-date     = date(prmtrnsDate)
          tran-period   = prmperiod .

       
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "VoidedCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "VoidedCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


  

  def buffer alt-period for period.
    
  
    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period THEN DO:
       IF NOT period.pstat THEN DO:
          cError = "Already Closed. " .
          RETURN.
       END.
       else do:
         find first alt-period
             where alt-period.company             eq cocode
               and alt-period.pst - period.pend   eq 1
               and (alt-period.pnum - period.pnum eq 1     or
                    (alt-period.pnum              eq 1 and
                     period.pnum eq company.num-per))
               and alt-period.pstat               eq yes
           no-lock no-error.
         if not avail alt-period then do:
           cError = "NEXT PERIOD NOT DEFINED.  MUST DEFINE NEXT PERIOD!!!".
           RETURN.
         end.
         /* CODE FOR VERIFYING CLOSE OF ALL PRIOR PERIODS */
         else do:
           find first alt-period where alt-period.company eq cocode
                                   and alt-period.pst     le tran-date
                                   and alt-period.pend    ge tran-date
                                 no-lock no-error.
           if avail alt-period then fiscal-yr = alt-period.yr.
           find first alt-period where alt-period.company eq cocode
                    and (alt-period.yr     lt fiscal-yr or
                        (alt-period.yr    eq fiscal-yr and
                         alt-period.pnum  lt period.pnum))
                    and alt-period.pstat   eq yes
                    no-lock no-error.
           if avail alt-period then do:
             ASSIGN per-open   = alt-period.pnum
                    per-status = alt-period.pstat.
             cError = "PRIOR MONTH(S) NOT CLOSED.  MUST CLOSE ALL PRIOR MONTHS!!!".
             RETURN.
           end.
           /*ELSE
           if period.pnum eq 1 AND ip-oktogo then do:
             cError = "YOU ARE ABOUT TO CLOSE PERIOD 1." skip(1)
                     "YOU MUST MAKE SURE THE PRIOR FISCAL YEAR END PROCEDURE HAS BEEN RUN!!!"
                     skip(2)
                     "Do You Want to Continue and Close the Month? " 
                     update choice .
           end.*/
         end.
       END.
       prmperiod = (period.pnum).
    END.

    ELSE DO:
      cError = "No Defined Period Exists".
      RETURN.
    END.

    run run-report.   

    lv-post =  IF prmOut = "Yes" THEN TRUE ELSE FALSE .

           IF lv-post THEN do: 

               RUN close-month.
               cError = "Closing G/L Period is completed. " .
           END.
        
        
   
  CREATE ttGLCloseMonth.
    ASSIGN ttGLCloseMonth.clmon = vTextFile .

    
  END.
/*****************************************************************************************/
PROCEDURE run-report :
form account.actnum label "Account Number"
     account.dscr   label "Account Description"
     gltrans.jrnl   label " Journal "
     gltrans.tr-amt format "(>>>,>>>,>>>,>>9.99)" label "Transaction"
     open-amt       label "Account Balance"
    with frame r-mclo down width 132 no-box column 10 STREAM-IO.

 {sys/form/r-topw.f}

/* {sys/inc/print1.i}*/

     if tmp-dir = "" then tmp-dir = v-webrootpath .
     assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

 {sys/inc/outprint.i VALUE(lines-per-page)}

 ASSIGN uperiod = tran-period
        udate = tran-date.
 find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.


 str-tit  = coname + " - " + loname.
 str-tit2 = "MONTHLY SUMMARY & G/L CLOSING" .
 str-tit3 = "Period " + string(uperiod,"99") + " - " +
               string(period.pst) + " to " + string(period.pend).
 x = (112 - length(str-tit)) / 2.
 str-tit  = fill(" ",x) + str-tit .
 x = (114 - length(str-tit2)) / 2.
 str-tit2 = fill(" ",x) + str-tit2 .
 x = (132 - length(str-tit3)) / 2.
 str-tit3 = fill(" ",x) + str-tit3 .

   display str-tit3 format "x(130)" skip(1) with frame r-top.
 
   
   for each account where account.company eq cocode no-lock with frame r-mclo:
      if line-counter gt page-size - 3 then page.
      open-amt = account.cyr-open.
      do i = 1 to uperiod:
         open-amt = open-amt + cyr[i].
      end.
      find first gltrans
          where gltrans.company eq cocode
            and gltrans.actnum  eq account.actnum
            and gltrans.tr-date ge period.pst
            and gltrans.tr-date le period.pend
            and gltrans.period  eq uperiod
          no-lock no-error.
      if open-amt eq 0 and not avail gltrans then next.
      display account.actnum
              account.dscr
              open-amt.
      down.
      tot-all = tot-all + open-amt.

      for each gltrans no-lock
          where gltrans.company eq account.company
            and gltrans.actnum  eq account.actnum
            and gltrans.tr-date ge period.pst
            and gltrans.tr-date le period.pend
            and gltrans.period  eq uperiod
          break by gltrans.jrnl with frame r-mclo:

         if line-counter gt page-size - 2 then page.

         assign
          tot-tx   = tot-tx   + tr-amt
          tot-all  = tot-all  + tr-amt
          tot-jrnl = tot-jrnl + tr-amt
          tot-act  = tot-act  + tr-amt.

         if last-of(gltrans.jrnl) then do:
            display "" @ account.actnum
                    "" @ account.dscr
                    gltrans.jrnl
                    tot-jrnl @ gltrans.tr-amt
                   "" @ open-amt.
            tot-jrnl = 0.
            down.
         end.
      end. /* each gltrans */

      display "" @ account.actnum
              "" @ account.dscr
              "" @ gltrans.jrnl
              tot-act @ gltrans.tr-amt
              (tot-act + open-amt) format "->>>,>>>,>>>,>>9.99" @ open-amt
              "*" with frame r-mclo.
      down 2.
      tot-act = 0.
   end. /* each account */

   display "" @ account.actnum
           "" @ account.dscr
           "TOTAL" @ gltrans.jrnl
           tot-tx  @ gltrans.tr-amt
           tot-all @ open-amt
           with frame r-mclo.
 
 
 

end procedure.

PROCEDURE close-month :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR li AS INT NO-UNDO.
   DEF VAR lv-rowid AS ROWID NO-UNDO.

   DEF BUFFER b-period FOR period.

   

   find first gl-ctrl where gl-ctrl.company eq cocode no-lock no-error.
   find first company where company.company eq cocode.
   find first b-racct
       where b-racct.company eq cocode
         and b-racct.actnum  eq gl-ctrl.ret
       no-lock no-error.
   if not avail b-racct then do on endkey undo, return:
      cError = "Unable to Find Retained Earnings Account from G/L Control File."
              .
      return.
   end.

   find first b-cacct
       where b-cacct.company eq cocode
         and b-cacct.actnum  eq gl-ctrl.contra
       no-lock no-error.
   if not avail b-cacct then do on endkey undo, return:
      cError = "Unable to Find Profit Contra Account from G/L Control File." .
      return.
   end.

   

   for each gltrans
       where gltrans.company eq cocode
         and gltrans.tr-date ge period.pst
         and gltrans.tr-date le period.pend
         and gltrans.period  eq uperiod
       transaction:
       

      find first account
          where account.company eq cocode
            and account.actnum  eq gltrans.actnum
          no-error.
      if avail account then do:
         account.cyr[uperiod] = account.cyr[uperiod] + gltrans.tr-amt.

         if index("RE",account.type) gt 0 then do:
            find first b-racct
                where b-racct.company eq cocode
                  and b-racct.actnum  eq gl-ctrl.ret.
                  
            b-racct.cyr[uperiod] = b-racct.cyr[uperiod] + gltrans.tr-amt.

            find first b-cacct
                where b-cacct.company eq cocode
                  and b-cacct.actnum  eq gl-ctrl.contra.

            b-cacct.cyr[uperiod] = b-cacct.cyr[uperiod] - gltrans.tr-amt.
         end.
      end.

      create glhist.
      assign
       glhist.company = gltrans.company
       glhist.actnum  = gltrans.actnum
       glhist.jrnl    = gltrans.jrnl
       glhist.period  = gltrans.period
       glhist.tr-dscr = gltrans.tr-dscr
       glhist.tr-date = gltrans.tr-date
       glhist.tr-num  = gltrans.trnum
       glhist.tr-amt  = gltrans.tr-amt.
       
      delete gltrans.
   end.

   for each cust where cust.company eq cocode transaction:
      assign
       cust.cost[1] = 0
       cust.comm[1] = 0.
       
      for each ar-ledger
          where ar-ledger.company eq cocode
            and ar-ledger.cust-no eq cust.cust-no
            and ar-ledger.tr-date gt period.pend
            and ar-ledger.ref-num begins "INV#"
          no-lock,

          first ar-inv
          where ar-inv.company eq cocode
            and ar-inv.posted  eq yes
            and ar-inv.cust-no eq cust.cust-no
            and ar-inv.inv-no  eq int(substr(ar-ledger.ref-num,6,
                                                length(ar-ledger.ref-num)))
          use-index posted no-lock:

         assign
          cust.cost[1] = cust.cost[1] +
                         if ar-inv.t-cost eq ? then 0 else ar-inv.t-cost
          cust.comm[1] = cust.comm[1] +
                         if ar-inv.t-comm eq ? then 0 else ar-inv.t-comm.
      end.                
   end.

   IF period.pnum EQ company.num-per THEN DO:
     lv-rowid = ROWID(period).

     FIND NEXT period
         WHERE period.company EQ cocode
           AND period.pstat   EQ YES
         NO-LOCK NO-ERROR.

     IF AVAIL period THEN DO:
       /* Cust Processing  */
       FOR EACH cust WHERE cust.company eq cocode:
         STATUS DEFAULT "Please Wait...Updating Customer: " + TRIM(cust.cust-no).

         {util/reopeny1.i 1 lyytd lyr 6}
    
         {util/reopeny1.i 0 ytd ytd 5}
       END.

       /* Vend Processing  */
       FOR EACH vend WHERE vend.company eq cocode:
         STATUS DEFAULT "Please Wait...Updating Vendor: " + TRIM(vend.vend-no).
    
         {util/reopeny2.i 1 lyytd last-year}
    
         {util/reopeny2.i 0 ytd-msf purch[13]}
       END. /* for each vend */
     END.

     FIND period WHERE ROWID(period) EQ lv-rowid NO-LOCK NO-ERROR.

     /*FIND FIRST b-period
         WHERE b-period.company EQ cocode
           AND b-period.yr      EQ period.yr
         NO-LOCK NO-ERROR.
     start-date = IF AVAIL b-period THEN b-period.pst ELSE ?.

     FIND LAST b-period
         WHERE b-period.company EQ cocode
           AND b-period.yr      EQ period.yr
         NO-LOCK NO-ERROR.
     end-date = IF AVAIL b-period THEN b-period.pend ELSE ?.

     /* Cust Processing  */
     IF start-date NE ? AND end-date NE ? THEN
     for each cust where cust.company eq cocode transaction:

       status default "Please Wait...Updating Customer: " + trim(cust.cust-no).

       cust.lyr-sales = 0.

       for each ar-ledger
           where ar-ledger.company eq cocode
             and ar-ledger.tr-date ge start-date
             and ar-ledger.tr-date le end-date
             and ar-ledger.cust-no eq cust.cust-no
             and ar-ledger.ref-num begins "INV#"
           no-lock:

         find first ar-inv
             where ar-inv.company eq cocode
               and ar-inv.cust-no eq cust.cust-no
               and ar-inv.posted  eq yes
               and ar-inv.inv-no eq int(substr(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num)))
             USE-INDEX posted no-lock no-error.

         if avail ar-inv THEN do:
           for each ar-invl where ar-invl.company eq cocode and
                                  ar-invl.cust-no eq ar-inv.cust-no and
                                  ar-invl.inv-no eq ar-inv.inv-no
                            use-index inv-no no-lock:
             if ar-invl.amt-msf ne 0 then
               assign cust.lyytd-msf = cust.lyytd-msf + ar-invl.amt-msf
                      cust.ytd-msf = cust.ytd-msf - ar-invl.amt-msf.
             ELSE do:
               find first itemfg where itemfg.company eq cocode and
                                       itemfg.i-no eq ar-invl.i-no
                                 use-index i-no no-lock no-error.
               if avail itemfg then
                 assign cust.lyytd-msf = cust.lyytd-msf +
                                         ((ar-invl.inv-qty / 1000) * itemfg.t-sqft)
                        cust.ytd-msf   = cust.ytd-msf -
                                         ((ar-invl.inv-qty / 1000) * itemfg.t-sqft).
             end.
           end.

/*         assign cust.sales[6] = cust.sales[6] + (ar-inv.net - ar-inv.tax-amt)
                  cust.sales[13] = cust.sales[13] - (ar-inv.net - ar-inv.tax-amt)
*/
           assign cust.lyr-sales = cust.lyr-sales + (ar-inv.net - ar-inv.tax-amt)
                  cust.ytd-sales = cust.ytd-sales - (ar-inv.net - ar-inv.tax-amt)
                  cust.cost[6]  = cust.cost[6] + ar-inv.t-cost
                  cust.cost[5] = cust.cost[5] - ar-inv.t-cost
                  cust.comm[6]  = cust.comm[6] + ar-inv.t-comm
                  cust.comm[5] = cust.comm[5] - ar-inv.t-comm.
         end. /* if avail ar-inv */
       end. /* for each ar-ledger INV */

       for each ar-ledger where ar-ledger.company eq cocode and
                                ar-ledger.tr-date ge start-date and
                                ar-ledger.tr-date le end-date and
                                ar-ledger.cust-no eq cust.cust-no and
                                ar-ledger.ref-num begins "Memo#" no-lock:

         find first ar-cash where ar-cash.company eq cocode and
                                  ar-cash.cust-no eq cust.cust-no and
                                  ar-cash.posted and
                                  ar-cash.check-no eq int(substr(ar-ledger.ref-num,6,8))
                            USE-INDEX posted no-lock no-error.

         for each ar-cashl where ar-cashl.company eq cocode and
                                 ar-cashl.c-no eq ar-cash.c-no
                           use-index c-no no-lock:
/*         assign cust.sales[6] = cust.sales[6] +
                                  (ar-cashl.amt-paid - ar-cashl.amt-disc)
                  cust.sales[5] = cust.sales[5] -
                                  (ar-cashl.amt-paid - ar-cashl.amt-disc).
*/
           assign cust.lyr-sales = cust.lyr-sales +
                                   (ar-cashl.amt-paid - ar-cashl.amt-disc)
                  cust.ytd-sales = cust.ytd-sales -
                                   (ar-cashl.amt-paid - ar-cashl.amt-disc).
         end.
       end. /* for each ar-ledger MEMO */
     end. /* for each cust */

     /* Vend Processing  */
     IF start-date NE ? AND end-date NE ? THEN
     for each vend where vend.company eq cocode transaction:

       status default "Please Wait...Updating Vendor: " + trim(vend.vend-no).

       vend.last-year = 0.

       for each ap-ledger where ap-ledger.company eq cocode and
                                ap-ledger.tr-date ge start-date and
                                ap-ledger.tr-date le end-date and
                                ap-ledger.vend-no eq vend.vend-no and
                                ap-ledger.refnum begins "INV#" no-lock:

         find first ap-inv where ap-inv.company eq cocode and
                                 ap-inv.vend-no eq vend.vend-no and
                                 ap-inv.posted  eq yes and
                                 ap-inv.inv-no eq substr(ap-ledger.refnum,6,length(ap-ledger.refnum))
                           USE-INDEX ap-inv no-lock no-error.

         if avail ap-inv THEN do:
           FOR each ap-invl where ap-invl.company eq cocode and
                                  ap-invl.inv-no eq ap-inv.inv-no and
                                  ap-invl.i-no eq ap-inv.i-no
                            use-index i-no no-lock:
             if ap-invl.amt-msf ne 0 then
               assign vend.lyytd = vend.lyytd + ap-invl.amt-msf
                      vend.ytd-msf = vend.ytd-msf - ap-invl.amt-msf.
             else do:
               find first itemfg where itemfg.company eq cocode and
                                       itemfg.i-no eq string(ap-invl.i-no)
                                 use-index i-no no-lock no-error.
               if avail itemfg then
                 assign vend.lyytd   = vend.lyytd +
                                       ((ap-invl.qty / 1000) * itemfg.t-sqft)
                        vend.ytd-msf = vend.ytd-msf -
                                       ((ap-invl.qty / 1000) * itemfg.t-sqft).
             end.
           end.
         end. /* if avail ap-inv */

         assign vend.purch[13] = vend.purch[13] - ap-ledger.amt
                vend.last-year = vend.last-year + ap-ledger.amt.
       end. /* for each ap-ledger INV */

       for each ap-ledger where ap-ledger.company eq cocode and
                                ap-ledger.tr-date ge start-date and
                                ap-ledger.tr-date le end-date and
                                ap-ledger.vend-no eq vend.vend-no and
                                (ap-ledger.refnum begins "Memo#" or
                                 ap-ledger.refnum begins "Chk#") no-lock:

         assign vend.purch[13] = vend.purch[13] - ap-ledger.amt
                vend.last-year = vend.last-year + ap-ledger.amt.
       end. /* for each ap-ledger MEMO */
     end. /* for each vend */

     status default "".*/
   end.

   do transaction:
      find first period
          where period.company eq cocode
            and period.pst     le tran-date
            and period.pend    ge tran-date
            and period.pnum    eq uperiod
            and period.pstat   eq yes
          exclusive-lock.
      period.pstat = false.
      if period.pnum eq company.num-per then company.yend-per = no.
   end.

   find next period
       where period.company eq cocode
         and period.pstat   eq yes
       no-lock.
   if avail period then ASSIGN tran-period = period.pnum
                               uperiod = period.pnum.

   

   cError = "Current accounting period changed ".

END PROCEDURE.
