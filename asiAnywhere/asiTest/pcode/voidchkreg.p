

/*------------------------------------------------------------------------
    File        : voidchkreg.p
    Purpose     : Voided check Register
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttVoidedCheckRegister NO-UNDO
        FIELD voidchk AS CHAR.
        
       

DEFINE DATASET dsVoidedCheckRegister FOR ttVoidedCheckRegister.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmvoidchk       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPstDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVoidedCheckRegister.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmvoidchk     = ? THEN ASSIGN     prmvoidchk     = "". 
     IF prmPstDate     = ? THEN ASSIGN     prmPstDate     = "". 
     IF prmperiod      = ? THEN ASSIGN     prmperiod      = 0. 
     IF prmOut         = ? THEN ASSIGN     prmOut         = "". 




DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
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

{sys/inc/aplockbx.i}
def buffer xap-payl for ap-payl.    
    
DEF VAR v-invalid AS LOG NO-UNDO.    
def var v-postable as log init NO NO-UNDO.
def var time_stamp as character NO-UNDO.

def TEMP-TABLE xpayl NO-UNDO field recnum as recid.

def TEMP-TABLE w-disb NO-UNDO field w-actnum   like ap-payl.actnum
                    field w-amt-paid like ap-payl.amt-paid
                    field w-amt-disc like ap-payl.amt-disc.

{sys/form/r-top3w.f}

time_stamp = string(time, "HH:MMam").


tran-date = TODAY.

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
  
  v-invalid = no.
   ASSIGN
       tran-date     = date(prmPstDate)                               
       tran-period   = prmperiod. 
  
 IF prmvoidchk = "Validate" THEN DO:
    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then do:
       IF NOT period.pstat THEN DO:
          cError = "Period Already Closed. " .
          RETURN.
       END.
        prmperiod = (period.pnum).
    END.
    ELSE DO:
      cError = "No Defined Period Exists for tran-date" .
      RETURN.
    end.
 END.  /*validate rep*/


  IF prmvoidchk = "voidchk" THEN DO:
       
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

        
        
        DEF VAR lv-bank-file AS cha NO-UNDO.

       
       /* if v-invalid then return no-apply.
        assign tran-period tran-date .*/

        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAIL gl-ctrl THEN DO:
                ASSIGN v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum.
                RELEASE gl-ctrl.       
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */

        run run-report.

        IF v-postable THEN DO:    
            lv-post =  IF prmOut = "Yes" THEN TRUE ELSE FALSE .

           IF lv-post THEN do:  
                    RUN create-bank-file (OUTPUT lv-bank-file).
                    /*cError = "Check Register/Lock Box file is created into " +
                        aplockbx-path + lv-bank-file.*/
                RUN post-gl.
                RUN copy-report-to-audit-dir.
                cError = "Posting complete. ".
            END.

            ELSE RUN undo-trnum.
         END.
         ELSE do:
             cError = "No Void Checks available for posting...".
             RUN undo-trnum.
         END.

   
  CREATE ttVoidedCheckRegister.
    ASSIGN ttVoidedCheckRegister.voidchk = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE create-bank-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER op-data-file AS cha NO-UNDO.

  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR v-account AS CHAR NO-UNDO.
  DEF VAR v-ref AS cha NO-UNDO.
  DEF VAR v-check-date AS DATE NO-UNDO.
  DEF VAR v-check-date-string AS cha NO-UNDO.
  DEF VAR v-total-amt AS DEC NO-UNDO.
  
  ASSIGN targetfile = aplockbx-path +
                     "CheckRegister" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                      STRING(DAY(TODAY),"99") + STRING(TIME) + ".txt"
         dirname1 = aplockbx-path
         .

  IF SEARCH(dirname1) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
  END.


  OUTPUT TO VALUE(targetfile).
  PUT UNFORMATTED "01021226C3648091" SKIP.
  v-total-amt = 0.

  for each ap-pay where ap-pay.company = cocode and
                        ap-pay.cleared = ? and
                        ap-pay.reconciled = NO NO-LOCK:

      FIND FIRST vend WHERE vend.company = ap-pay.company
                        AND vend.vend-no = ap-pay.vend-no NO-LOCK NO-ERROR.
                         
      find first bank where bank.company = cocode and
                             bank.bank-code = ap-pay.bank-code NO-LOCK no-error.

      ASSIGN
      v-account = IF AVAIL bank THEN bank.bk-act ELSE ""
      v-ref = IF AVAIL vend THEN SUBSTRING(vend.name,1,12) ELSE ""
      v-check-date = ap-pay.check-date
      v-check-date-string = STRING(MONTH(v-check-date),"99") +
                            STRING(DAY(v-check-date),"99") + 
                            SUBstring(STRING(YEAR(v-check-date),"9999"),3,2)
      v-total-amt = v-total-amt + ap-pay.check-amt.
      PUT UNFORMATTED "V"
          ap-pay.check-no FORM "9999999999"
          v-account FORM "99999999999999"
          ap-pay.check-amt * 100 FORM "9999999999"
          v-ref FORM  "x(12)"
          v-check-date-string FORM "x(6)"
          SPACE(25)
          "38"  /*for void*/
          SKIP.

  END.
  PUT UNFORMATTED "T          "
      v-account FORM "99999999999999"
      v-total-amt * 100 FORM "9999999999"
      SKIP.

  OUTPUT CLOSE.
  op-data-file = TARGETfile.

END PROCEDURE.

PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */
  
  form HEADER
         "Check #" at 1
         "Date" at 17
         "Amount" at 33
         "Vendor" at 59 skip
         fill("_",132) format "x(130)" skip
         with STREAM-IO frame f-top width 132 no-box no-labels no-underline.

  ASSIGN
     time_stamp = string(time, "hh:mmam")
     tmpstore   = fill("_",125).             

 /*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}


    
  ASSIGN
   str-tit  = coname + " - " + loname                                    
   str-tit2 = "A/P VOIDED CHECK REGISTER " + STRING(v-trnum)
   str-tit3 = "Period " + string(tran-period,"99") + " " + string(tran-date)
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
     str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3 .

display "" with frame r-top.
display "" with frame f-top.

v-postable = NO.

for each ap-pay where ap-pay.company = cocode and
                      ap-pay.cleared = ? and
                      ap-pay.reconciled = no
                      NO-LOCK
                      break by ap-pay.check-no:
    find first vend where vend.company = cocode and
                          vend.vend-no = ap-pay.vend-no no-lock no-error.
    display ap-pay.check-no
            ap-pay.check-date
            ap-pay.check-amt
            ap-pay.vend-no
            vend.name when avail vend
            with frame report-lines width 132 no-box NO-LABELS STREAM-IO.
    down with frame report-lines.
    assign v-postable = YES.
end. /* for each ap-pay record */

END PROCEDURE.

PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR v-bank-amt AS DEC NO-UNDO.
 def var v-tot-amt-paid like ap-pay.check-amt NO-UNDO.
 def var v-tot-amt-disc like ap-payl.amt-disc NO-UNDO.

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
 ASSIGN uperiod = tran-period
        udate = tran-date.

 EMPTY TEMP-TABLE w-disb.

postit:
do transaction on error undo, leave:
   find first ap-ctrl where ap-ctrl.company = cocode no-lock no-error.

   for each ap-pay where ap-pay.company = cocode and
                         ap-pay.cleared = ? and
                         ap-pay.reconciled = no
                         on error undo postit, leave postit
                         BREAK BY ap-pay.bank-code:

       assign ap-pay.cleared = NO
              ap-pay.reconciled = ?
              v-tot-amt-paid = v-tot-amt-paid + ap-pay.check-amt
              v-bank-amt = v-bank-amt + ap-pay.check-amt.

       find first bank where bank.company = cocode and
                             bank.bank-code = ap-pay.bank-code no-error.
       if available bank then
          bank.bal = bank.bal + ap-pay.check-amt.

       create ap-ledger.
         assign
         ap-ledger.company = ap-pay.company
         ap-ledger.vend-no = ap-pay.vend-no
         ap-ledger.refnum = "VOIDED CHECK" + string(ap-pay.check-no, "zzzzzzz9")
         ap-ledger.ref-date = today
         ap-ledger.tr-date = udate
         ap-ledger.trnum = v-trnum
         ap-ledger.period = tran-period
         ap-ledger.amt = ap-ledger.amt - ap-pay.check-amt
         ap-ledger.actnum = bank.actnum.
       RELEASE ap-ledger.
         
       EMPTY TEMP-TABLE xpayl.

       for each ap-payl NO-LOCK where ap-payl.c-no = ap-pay.c-no:
           create xpayl.                    
           xpayl.recnum = recid(ap-payl).   

           find first ap-inv where ap-inv.company = cocode and
                                   ap-inv.vend-no = ap-pay.vend-no and
                                   ap-inv.inv-no = ap-payl.inv-no no-error.
           if available ap-inv then
           do:
              find vend where vend.company = cocode and
                              vend.vend-no = ap-inv.vend-no
                              exclusive-lock no-error.
              assign
              ap-inv.paid = ap-inv.paid - ap-payl.amt-paid
              ap-inv.disc-taken = ap-inv.disc-taken - ap-payl.amt-disc
              ap-inv.due = ap-inv.due + ap-payl.amt-paid + ap-payl.amt-disc.
              if available vend then
                 vend.acc-bal = vend.acc-bal + ap-payl.amt-paid +
                                       ap-payl.amt-disc.
           end. /* if avail ap-inv .. */
           RELEASE ap-inv.
           RELEASE vend.
           
           v-tot-amt-disc = v-tot-amt-disc + ap-payl.amt-disc.
           
           if ap-payl.d-no ne 0 then do:
             create w-disb.
             assign
              w-actnum   = ap-payl.actnum
              w-amt-paid = ap-payl.amt-paid
              w-amt-disc = ap-payl.amt-disc.
           end.
       end. /* for each ap-payl record */
       
       for each xpayl,
           first ap-payl where recid(ap-payl) eq xpayl.recnum
           no-lock:
           
          find last xap-payl where xap-payl.c-no eq ap-payl.c-no
              use-index c-no no-lock no-error.
          x = if avail xap-payl then xap-payl.line else 0.
          
          create xap-payl.

          assign
           xap-payl.c-no       = ap-payl.c-no
           xap-payl.line       = x + 1
           xap-payl.actnum     = ap-payl.actnum
           xap-payl.amt-disc   = -(ap-payl.amt-disc)
           xap-payl.amt-due    = ap-payl.amt-due + ap-payl.amt-paid +
                                                   ap-payl.amt-disc
           xap-payl.amt-paid   = -(ap-payl.amt-paid)
           xap-payl.check-no   = ap-payl.check-no
           xap-payl.due-date   = ap-payl.due-date
           xap-payl.inv-no     = ap-payl.inv-no
           xap-payl.man-check  = ap-payl.man-check
           xap-payl.memo       = ap-payl.memo
           xap-payl.posted     = ap-payl.posted
           xap-payl.vend-no    = ap-payl.vend-no.
          RELEASE xap-payl.
       end.  /* for each xpayl */
       IF LAST-OF(ap-pay.bank-code) THEN DO:
          create gltrans.
          ASSIGN gltrans.company = cocode
                 gltrans.actnum  = bank.actnum
                 gltrans.jrnl    = "APVOIDCK"
                 gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
                 gltrans.tr-date = udate
                 gltrans.tr-amt  = v-bank-amt
                 gltrans.period  = tran-period
                 gltrans.trnum   = v-trnum
                 v-bank-amt = 0.
          RELEASE gltrans.
       END.

       RELEASE bank.
   end. /* for each ap-pay record */

   FIND CURRENT ap-pay NO-LOCK NO-ERROR.

   /* old way -> changed to post by bank account
   create gltrans.
   assign
    gltrans.company = cocode
    gltrans.actnum  = bank.actnum
    gltrans.jrnl    = "APVOIDCK"
    gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
    gltrans.tr-date = udate
    gltrans.tr-amt  = v-tot-amt-paid
    gltrans.period  = tran-period
    gltrans.trnum   = v-trnum.
   */
   if v-tot-amt-disc ne 0 then do:
     create gltrans.
     assign
      gltrans.company = cocode
      gltrans.actnum  = ap-ctrl.discount
      gltrans.jrnl    = "APVOIDCK"
      gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
      gltrans.tr-date = udate
      gltrans.tr-amt  = v-tot-amt-disc
      gltrans.period  = tran-period
      gltrans.trnum   = v-trnum.
     RELEASE gltrans.
   end.
   
   for each w-disb break by w-actnum:
     assign
      v-tot-amt-paid = v-tot-amt-paid - w-amt-paid
      v-tot-amt-disc = v-tot-amt-disc - w-amt-disc.
      
     accumulate w-amt-paid (sub-total by w-actnum).
     accumulate w-amt-disc (sub-total by w-actnum).
     
     if last-of(w-actnum) then do:
       create gltrans.
       assign
        gltrans.company = cocode
        gltrans.actnum  = w-actnum
        gltrans.jrnl    = "APVOIDCK"
        gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
        gltrans.tr-date = udate
        gltrans.tr-amt  = ((accum sub-total by w-actnum w-amt-paid) +
                           (accum sub-total by w-actnum w-amt-disc)) * -1 
        gltrans.period  = tran-period
        gltrans.trnum   = v-trnum.
       RELEASE gltrans.
     end.
   end.

   create gltrans.
   assign
    gltrans.company = cocode
    gltrans.actnum  = ap-ctrl.payables
    gltrans.jrnl    = "APVOIDCK"
    gltrans.tr-dscr = "AP VOIDED CHECK REGISTER"
    gltrans.tr-date = udate
    gltrans.tr-amt  = (v-tot-amt-paid + v-tot-amt-disc) * -1
    gltrans.period  = tran-period
    gltrans.trnum   = v-trnum.
   RELEASE gltrans.

end. /* postit */


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
  
  ASSIGN targetfile = lv-audit-dir + "\AP\VC5\Run#"
                    + STRING(v-trnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\AP"
         dirname3 = lv-audit-dir + "\AP\VC5".

  if index(list-name,'P',1) > 0 then assign
   list-name = replace(list-name,'P:',"D:").

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
  
  
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF v-trnum EQ gl-ctrl.trnum THEN gl-ctrl.trnum = gl-ctrl.trnum - 1.
        RELEASE gl-ctrl.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
 

END PROCEDURE.
