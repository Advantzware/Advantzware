

/*------------------------------------------------------------------------
    File        : glyrclo.p
    Purpose     : Year Close 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttGLYearClose NO-UNDO
        FIELD yrclo AS CHAR.
        
       

DEFINE DATASET dsGLYearClose FOR ttGLYearClose.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmyear          AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmmove          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGLYearClose.

     IF prmUser      = ? THEN ASSIGN   prmUser     = "".   
     IF prmAction    = ? THEN ASSIGN   prmAction   = "". 
     IF prmyear      = ? THEN ASSIGN   prmyear     = 0. 
     IF prmmove      = ? THEN ASSIGN   prmmove     = "". 
     IF prmOut       = ? THEN ASSIGN   prmOut      = "". 




DEFINE VARIABLE tran-year AS INTEGER FORMAT "9999":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE tb_budget AS LOGICAL INITIAL no NO-UNDO.
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
 g_user    = prmUser.



 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  ASSIGN  
      init-dir    = v-webrootpath .

def var start-date as date initial 01/01/1901 NO-UNDO.
def var end-date as date initial 01/01/1901 NO-UNDO.
def var actbal as dec format "(>>>,>>>,>>>,>>9.99)" NO-UNDO.
def var new-bal as dec NO-UNDO.
def var ret-bal as dec NO-UNDO.
def var new-year as logical init no NO-UNDO.
   
def var save_id as recid NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var fisc-yr as int format "9999" NO-UNDO.
def var tot-type like actbal NO-UNDO.
def var v-err-msg as char format "x(60)" NO-UNDO.
def buffer tmp-per for period .
DEF VAR lv-close-year AS LOG NO-UNDO.
time_stamp = string(time, "HH:MMam") .



find first company where company.company eq cocode NO-LOCK.
  find first period where period.company eq cocode
                      and period.pstat   eq yes
                      no-lock no-error.
  ASSIGN tran-year = (if avail period then period.yr else year(today)) - 1
         fisc-yr = tran-year
         .

  if company.yend-per then do :
     cError = "This fical year is already closed. " .
     return.
  END.
  find first period where period.company eq cocode
                      and period.pstat   eq yes
                      and period.yr      eq fisc-yr
                      no-lock no-error.
  if avail period then do with frame error:
     cError = "ALL MONTHS IN CLOSING FISCAL YEAR ARE NOT CLOSED!!!!" .
     return.  
  end.

  find first period where period.company eq cocode
                      and period.pnum    eq 1
                      and period.yr      eq (fisc-yr + 1)
                      no-lock no-error.
  if not avail period then do:
     cError = "  There is no Period One for the Next Fiscal Year set up.".
     cError = "  Please add the next Fiscal Year from General Ledger, File Maintenance, Company File Open Periods (L-F-Company File-O), before you continue. "
        .
     RETURN.        
  end.



  


  IF prmAction = "yrclo" THEN DO:

      ASSIGN
          tran-year     = prmyear
          tb_budget     = IF prmmove = "Yes" THEN TRUE ELSE FALSE .

       
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "yrclo" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "yrclo" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        run run-report. 

        ASSIGN lv-post = IF prmout = "Yes" THEN TRUE ELSE FALSE.

        IF lv-post THEN do:
            RUN close-year.
            cError = "Completed closing year. " .
        END.
        
   
  CREATE ttGLYearClose.
    ASSIGN ttGLYearClose.yrclo = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 {sys/form/r-topw.f}


 /*{sys/inc/print1.i}*/

     if tmp-dir = "" then tmp-dir = v-webrootpath .
     assign list-name = tmp-dir + "\" + vTextFile
         init-dir = tmp-dir .

 {sys/inc/outprint.i VALUE(lines-per-page)}

 
 find first tmp-per
    where tmp-per.company eq cocode 
      and tmp-per.yr      eq (fisc-yr - 1)
    no-lock no-error.
if avail tmp-per then
  assign new-year = no.
else
  assign new-year = yes.

find first gl-ctrl where gl-ctrl.company eq cocode no-lock no-error.
find first account
    where account.company eq cocode
      and account.actnum  eq gl-ctrl.ret
    no-lock no-error.
ret-bal = account.cyr-open.

do i = 1 to company.num-per:
  ret-bal = ret-bal + cyr[i].
end.
release account.

   str-tit  = coname + " - " + loname.
   str-tit2 = "G/L YEAR END CLOSING" .
   str-tit3 = "For Year - " + string(fisc-yr,"9999").
   x = (112 - length(str-tit)) / 2.
   str-tit  = fill(" ",x) + str-tit .
   x = (116 - length(str-tit2)) / 2.
   str-tit2 = fill(" ",x) + str-tit2 .
   x = (132 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .

 
   display str-tit3 with frame r-top STREAM-IO.

   form
   account.actnum
   account.dscr space(2)
   account.type   format "X" space(2)
   actbal
   header "Account Number            Description                                   Type            Balance" skip fill("_",96) format "x(96)"
        with frame act no-box no-labels no-underline down width 100 column 20 STREAM-IO.

   for each account where account.company eq cocode and
                          index("RE", account.type) ne 0
                        /* (account.type eq "R" or account.type eq "E") DAR */
                          break by account.type with frame act:
      if line-counter > 58 then page.
      if first-of(account.type) then do:
         put skip(1).
         tot-type = 0.
      end.
      actbal = account.cyr-open.
      /*do i = 1 to 12:*/
      do i = 1 to company.num-per:
         actbal = actbal + cyr[i].
      end.
      display actnum account.dscr type actbal. /* DAR (total by account.type). */
      down 1 with frame act.
      tot-type = tot-type + actbal.
      ret-bal = ret-bal + actbal.
      if last-of(account.type) then do:
         display "         TOTAL" @ account.dscr
                 tot-type @ actbal.
         end.
      lv-close-year = YES.
   end.




END PROCEDURE.

PROCEDURE close-year :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lv-upd-company AS LOG NO-UNDO.




  for each account where account.company eq cocode:
      STATUS DEFAULT "Closing Account#: " + TRIM(account.actnum).
      new-bal = 0.
      if index("alct", account.type) ne 0 then
      do:
         new-bal = account.cyr-open.
         /*do i = 1 to 12:*/
         do i = 1 to company.num-per:
            new-bal = new-bal + cyr[i].
            lyr[i] = cyr[i].
            cyr[i] = 0.
         end.
         account.lyr-open = account.cyr-open.
         account.cyr-open = new-bal.
      end.
      if index("re", account.type) ne 0 then do:
         account.lyr-open = account.cyr-open.
         account.cyr-open = 0.
         /* do i = 1 to 12: */
         do i = 1 to company.num-per:
            lyr[i] = cyr[i].
            cyr[i] = 0.
         end.
      end.

      IF tb_budget THEN
      DO i = 1 TO EXTENT(account.bud):
        ASSIGN
         account.ly-bud[i] = account.bud[i]
         account.bud[i]    = 0.
      END.
  end.

  STATUS DEFAULT "".

  if new-year eq yes then          /*GEH-include opening balances if new yr*/
  for each account where account.company eq cocode and
                          (account.type eq "R" or
                          account.type eq "E"):
     ret-bal = ret-bal + cyr-open.
     STATUS DEFAULT "Closing Openning Balance Account#: " + TRIM(account.actnum).
  end. 

   find first period where period.company eq cocode
                       and period.yr      eq fisc-yr no-lock no-error.
   if avail period then do:
      start-date = period.pst.
      find last period where period.company eq cocode
                         and period.yr      eq fisc-yr
      no-lock no-error.      
      if avail period then end-date = period.pend.  
      else do:
           message "Can't Find Start and End Dates." VIEW-AS ALERT-BOX ERROR.
           RETURN.
      end.
   end.

   if start-date eq 01/01/1901 or end-date eq 01/01/1901 then undo, RETURN.

   do TRANSACTION:
      find first account where account.company eq cocode and
                               account.actnum eq gl-ctrl.ret exclusive-lock.
      account.cyr-open = ret-bal.
   end.

DO TRANSACTION:
  find first company where company.company eq cocode EXCLUSIVE-LOCK.
  company.yend-per = yes.
END.



END PROCEDURE.
