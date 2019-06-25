

/*------------------------------------------------------------------------
    File        : prntapdis.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttDisbursementRegister NO-UNDO
        FIELD apdis AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsDisbursementRegister FOR ttDisbursementRegister.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmapdis         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmpstdate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvndseq        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDisbursementRegister.

     IF prmUser      = ? THEN ASSIGN    prmUser       = "".   
     IF prmapdis     = ? THEN ASSIGN    prmapdis      = "". 
     IF prmpstdate   = ? THEN ASSIGN    prmpstdate    = "". 
     IF prmbegdate   = ? THEN ASSIGN    prmbegdate    = "". 
     IF prmenddate   = ? THEN ASSIGN    prmenddate    = "".
     IF prmvndseq    = ? THEN ASSIGN    prmvndseq     = "". 
     IF prmperiod    = ? THEN ASSIGN    prmperiod     = 0. 
     IF prmOut       = ? THEN ASSIGN    prmOut        = "".
     

DEFINE VARIABLE begin_date     AS DATE FORMAT "99/99/9999" INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE end_date       AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE tran-date      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE tran-period    AS INTEGER FORMAT ">>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Vendor#"  NO-UNDO.
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.



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
DEF VAR v-postable AS LOG NO-UNDO.

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
 g_user    = prmUser .


 

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
  ASSIGN  
      init-dir    = v-webrootpath .

def var v-descr as char format "x(15)".
def var save_id as recid.
def var time_stamp as ch.
def var qfirst as l.
def var post as logical format "Yes/No"
                        label "   Post to G/L & Vendor files?   " initial no.
def var g1 as dec.
def var t1 as dec.
def var g2 as dec.
def var t3 as dec.
def var v1 as dec.
def var v2 as dec.
def var t2 as dec.
def var xtrnum as int.
def var xap-acct as char.
def var xcs-acct as char.
def var sort-by-vend as log init yes format "Vendor/Sequence" no-undo.
def var v-s-date like ap-pay.check-date format "99/99/9999" no-undo.
def var v-e-date like ap-pay.check-date format "99/99/9999" init today no-undo.

def buffer tmp-period for period.
def buffer b-bank for bank.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

tran-date = TODAY.

do :
 find first ap-ctrl where ap-ctrl.company = cocode.
 if not available ap-ctrl then return.
 xap-acct = ap-ctrl.payables.
 xcs-acct = ap-ctrl.cash-act.
 release ap-ctrl.
end.

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "AUDITDIR"
    no-lock no-error.
  
  if not avail sys-ctrl then DO TRANSACTION:
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

IF prmapdis = "validateapdis" THEN DO:
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
      cError = "No Defined Period Exists for" .
    end.
END.



  
  IF prmapdis = "apdis" THEN DO:
     
        ASSIGN
       begin_date   =  date(prmbegdate)
       end_date     =  date(prmenddate)
       tran-date    =  TODAY                                
       tran-period  =  prmperiod                                
       rd_sort      =  prmvndseq .
                                   



        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "APDis" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "APDis" +
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
            cError = "No Defined Period Exists for" .
        end.

        assign 
         v-s-date = begin_date
         v-e-date = end_date
         sort-by-vend = rd_sort = "vendor#".

        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAIL gl-ctrl THEN DO:
                ASSIGN xtrnum        = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = xtrnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */


        run run-report.

        IF v-postable THEN DO:
            lv-post = IF prmout = "Yes" THEN TRUE ELSE FALSE.

            IF lv-post THEN do:
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                cError = "Posting Complete".

            END.
            ELSE RUN undo-trnum.
         END.
         ELSE do:
             cError = "No Cash Disbursements  available for posting...".
             RUN undo-trnum.
             RETURN.
         END.
  
       MESSAGE "testing " . 
   
  CREATE ttDisbursementRegister.
    ASSIGN ttDisbursementRegister.apdis = vTextFile .

    
  END.
/*****************************************************************************************/
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-chkno-posted LIKE bank.last-chk NO-UNDO.

  postit:
   do transaction on error undo:
      for each ap-dis where ap-dis.company = cocode and
          (ap-dis.check-date >= v-s-date and ap-dis.check-date <= v-e-date) and
                            not ap-dis.posted
      on error undo postit, leave postit:
         find vend where (vend.company = cocode) and vend.vend-no = ap-dis.vend-no
         no-lock no-error.
         find bank where bank.bank-code = ap-dis.bank-code and
                         bank.company = cocode no-error.

         for each ap-disl where ap-disl.d-no = ap-dis.d-no
                          break by ap-disl.check-no:

            if first-of(ap-disl.check-no) then
              i = 0.
            lv-chkno-posted = ap-disl.check-no.
            FIND FIRST ap-pay WHERE ap-pay.company  = cocode AND
                                    ap-pay.check-act = bank.actnum AND
                                    ap-pay.check-no = ap-dis.check-no NO-ERROR.

            IF NOT AVAILABLE ap-pay THEN DO:
              FIND LAST ap-pay USE-INDEX c-no NO-ERROR.
              IF AVAILABLE ap-pay THEN do:
                x = ap-pay.c-no.
              end.

              DO:
                CREATE ap-pay.
                ASSIGN
                ap-pay.company   = cocode
                ap-pay.check-act = bank.actnum
                ap-pay.check-amt = ap-dis.check-amt
                ap-pay.check-no  = ap-dis.check-no
                ap-pay.period    = tran-period
                ap-pay.c-no      = x + 1
                ap-pay.vend-no   = ap-dis.vend-no
                ap-pay.bank-code = ap-dis.bank-code
                ap-pay.d-no      = ap-dis.d-no.
              END.
            END.

            ASSIGN
            ap-pay.check-date = ap-dis.check-date
            ap-pay.man-check  = TRUE
            ap-pay.posted     = TRUE.

            IF ap-pay.check-date = ? THEN
               ap-pay.check-date = TODAY.

            FIND LAST ap-payl WHERE ap-payl.c-no = ap-pay.c-no USE-INDEX c-no
            NO-ERROR.
            IF AVAILABLE ap-payl THEN
              i = ap-payl.line + 1.
            ELSE
              i = 1.

            CREATE ap-payl.
            ASSIGN ap-payl.posted    = TRUE
                   ap-payl.c-no      = ap-pay.c-no
                   ap-payl.check-no  = ap-disl.check-no
                   ap-payl.line      = i
                   ap-payl.inv-no    = ""
                   ap-payl.d-no      = ap-disl.d-no
                   ap-payl.amt-disc  = 0
                   ap-payl.amt-paid  = ap-disl.amt
                   ap-payl.vend-no   = ap-dis.vend-no
                   ap-payl.man-check = TRUE
                   ap-payl.actnum    = ap-disl.actnum.


            bank.bal = bank.bal - ap-disl.amt.
            create gltrans.
            do :
              find first b-bank where b-bank.actnum = ap-disl.actnum
                exclusive-lock no-error.
              if available b-bank then b-bank.bal = b-bank.bal + ap-disl.amt.
            end.
            assign
            t1 = t1 + ap-disl.amt
            gltrans.company = cocode
            gltrans.actnum  = ap-disl.actnum
            gltrans.jrnl    = "CDISB"
            gltrans.tr-dscr = (if avail vend then vend.name else ap-dis.payee)
                              + " " + string(ap-dis.check-no)
            gltrans.tr-date = tran-date
            gltrans.tr-amt  = ap-disl.amt
            gltrans.period  = tran-period
            gltrans.trnum   = xtrnum
            ap-disl.posted  = true.
         end.  /* each line */

         /* Commented out per Julie's Request Task #02230003 
         if avail(vend) then
         do:
           assign
             vend.purch[tran-period]   = vend.purch[tran-period]   + t1
             vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
             vend.purch[13]        = vend.purch[13]   + t1
             vend.n-purch[13]      = vend.n-purch[13] + 1
             vend.acc-bal          = vend.acc-bal     + t1.
           if vend.acc-bal >= vend.hibal then
             assign vend.hibal = vend.acc-bal
                    vend.hibal-date = ap-dis.check-date.
           release vend.
         end.
         Commented out per Julie's Request Task #02230003 */

         create ap-ledger.
         assign
         ap-ledger.company  = cocode
         ap-ledger.vend-no  = ap-dis.vend-no
         ap-ledger.amt      = ap-dis.check-amt
         ap-ledger.refnum   = "CHK# " + string(ap-dis.check-no) +
                              " CD#" + bank.bank-code
         ap-ledger.ref-date = ap-dis.check-date
         ap-ledger.tr-date  = tran-date
         ap-ledger.trnum    = xtrnum
         t1                 = 0
         ap-dis.posted      = true.

         create gltrans.
         assign
         gltrans.company = cocode
         /***
         gltrans.actnum  = xcs-acct
         ***/
         gltrans.actnum  = bank.actnum
         gltrans.jrnl    = "CDISB"
         gltrans.tr-dscr = "CASH DISBURSEMENTS"
         gltrans.tr-date = tran-date
         gltrans.tr-amt  = -(ap-dis.check-amt)
         gltrans.period  = tran-period
         gltrans.trnum   = xtrnum.

         bank.last-chk = IF lv-chkno-posted >= bank.last-chk THEN lv-chkno-posted
                         ELSE bank.last-chk.
      end.
   end. /* postit: transaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
{sys/form/r-top3w.f}

form header
"VENDOR#  NAME                                   CHECK #      DATE          "
"AMOUNT       G/L DISTRIBUTION" skip fill("_",130) format "x(130)"
with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

time_stamp = string(time, "HH:MMam").
tmpstore   = fill("_",125).



assign
 str-tit  = coname + " - " + loname
 str-tit2 = "CASH DISBURSEMENTS  -  EDIT REGISTER " + string(xtrnum)
 str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (114 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}


   
   display "" with frame r-top.
   display "" with frame f-top.

   ASSIGN
    g1 = 0
    g2 = 0.

   if sort-by-vend then do:
     {ap/ap-dreg.i "vend-no"}
   end.

   else do:
     {ap/ap-dreg.i "d-no"}
   end.

   display  "** GRAND TOTAL  "  at 90  g2 to 128
   with no-labels no-underline STREAM-IO width 132 frame gt.

   hide frame f-top.

   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date) + " - " +
              "Summary by Account".
   x = (132 - length(str-tit3)) / 2.
   str-tit3 = fill(" ",x) + str-tit3 .
   page.
   form header
   "ACCCOUNT                                  DATE   VENDOR#  CHECK#"
   "LINE DESCRIPTION                QTY   UNIT PRICE          AMOUNT" skip
   fill("_",132) format "x(130)"
   with no-labels no-box no-underline frame f-top2 page-top STREAM-IO width 132.

   display "" with frame f-top2.

   for each ap-disl where not ap-disl.posted and ap-disl.company = cocode
            break by ap-disl.actnum by ap-disl.check-no
            with STREAM-IO width 132 no-labels:
      find ap-dis where ap-dis.company = cocode and
                        ap-dis.d-no = ap-disl.d-no and
                (ap-dis.check-date >= v-s-date and ap-dis.check-date <= v-e-date) and
                        NOT ap-dis.posted no-lock no-error.
      if NOT avail ap-dis then next.
      find vend WHERE vend.company = cocode and vend.vend-no = ap-dis.vend-no no-lock no-error.
      find bank where bank.bank-code = ap-dis.bank-code and
                      bank.company = cocode no-lock no-error.
      if first-of(ap-disl.actnum)
      then do:
         find first account where account.company = cocode and
                                  account.actnum  = ap-disl.actnum
                                  no-lock no-error.
         if avail account then v-descr = account.dscr.

         put ap-disl.actnum + " - " + v-descr format "x(39)" .
      end.
      put  ap-dis.check-date at 41     space(1)
           ap-dis.vend-no              space(1)
           ap-dis.check-no             space(1)
           ap-disl.line format ">>>9"  space(1)
           ap-disl.dscr format "x(20)" space(1)
           ap-disl.qty                 space(1)
           ap-disl.unit-pr             space(2)
           ap-disl.amt
           skip.
      accumulate ap-disl.amt (total by ap-disl.actnum).
      accumulate ap-disl.amt (total).
      if last-of(ap-disl.actnum) then do:
         put "** TOTAL "  to 116
             accum total by ap-disl.actnum ap-disl.amt to 129
                         skip(1).
      end.
      v-postable = YES.
   end.
   put "***** TOTAL FOR ALL ACCOUNTS " to 116
       accum total ap-disl.amt to 129.




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
  
  ASSIGN targetfile = lv-audit-dir + "\AP\VL2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VL2".

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
/* gdm - 11050906 */
REPEAT:
  FIND FIRST gl-ctrl EXCLUSIVE-LOCK
    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
  IF AVAIL gl-ctrl THEN DO:

    IF xtrnum = gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
    RELEASE gl-ctrl.
    LEAVE.
  END. /* IF AVAIL gl-ctrl */
END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.
