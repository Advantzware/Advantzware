

/*------------------------------------------------------------------------
    File        : recreg.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttReconciliationRegister NO-UNDO
        FIELD recreg AS CHAR.
        
       

DEFINE DATASET dsReconciliationRegister FOR ttReconciliationRegister.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmrecreg        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbnkcod        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReconciliationRegister.

     IF prmUser        = ? THEN ASSIGN     prmUser        = "".   
     IF prmrecreg      = ? THEN ASSIGN     prmrecreg      = "". 
     IF prmendDate     = ? THEN ASSIGN     prmendDate     = "". 
     IF prmbnkcod      = ? THEN ASSIGN     prmbnkcod      = "". 
     IF prmOut         = ? THEN ASSIGN     prmOut         = "". 




DEFINE VARIABLE bank_code AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
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


DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

def new shared var v-trnum as INT NO-UNDO.
def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---"  NO-UNDO.
def var time_stamp as ch NO-UNDO.
def var v-postable as log init NO NO-UNDO.

DEF VAR v-invalid AS LOG NO-UNDO.


def var save_id as RECID NO-UNDO.
def var v-post as logical NO-UNDO.
def var v-matching-record as logical NO-UNDO.
def var v-vend-name like vend.name NO-UNDO.

{ap/reconcil.i NEW}


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

ASSIGN
       bank_code       =  prmbnkcod                                 
       end_date        =  DATE(prmendDate)  . 

  IF prmrecreg = "vailidate " THEN do:
      FIND LAST period NO-LOCK
           WHERE period.company EQ cocode
           AND period.pst     LE end_date
           AND period.pend    GE end_date
           NO-ERROR.

       IF NOT AVAIL period THEN
           DO:
           cError = "No Period exists for End Date," +
               "reconcilation cannot be completed..." .
           RETURN.
       END.
  END.


  IF prmrecreg = "recreg " THEN DO:

      ASSIGN
       bank_code       =  prmbnkcod                                 
       end_date        =  DATE(prmendDate)  .                              
      
       
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "ReconReg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "ReconReg" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        

        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
            IF AVAIL gl-ctrl THEN DO:
                ASSIGN v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END. /* IF AVAIL gl-ctrl */
        END. /* REPEAT */

       
          
        run run-report.

        IF v-postable THEN DO:    
            lv-post = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

            IF lv-post THEN do:  
                RUN post-gl.
                cError = "Posting Complete".
            END.  
        END.
        ELSE do:
            cError = "No A/P Check Reconciliation available for posting...".      
        END.

        IF NOT v-postable OR NOT lv-post THEN DO:
            REPEAT:
                FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                    WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
                IF AVAIL gl-ctrl THEN DO:
                    IF gl-ctrl.trnum EQ v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
                    FIND CURRENT gl-ctrl NO-LOCK.
                    LEAVE.
                 END. /* IF AVAIL gl-ctrl */
             END. /* REPEAT */
         END.


        
       

   
  CREATE ttReconciliationRegister.
    ASSIGN ttReconciliationRegister.recreg = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */
 
DEF VAR v-chk-tot AS DEC FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR v-jrn-tot AS DEC FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR v-dep-tot AS DEC FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR v-unc-tot AS DEC FORMAT "->,>>>,>>9.99" NO-UNDO.
DEF VAR v-bnk-tot AS DEC FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-bank-code AS CHARACTER  NO-UNDO.


  ASSIGN
  time_stamp = string(time, "hh:mmam")
  tmpstore   = fill("_",125) .
       .

  {sys/form/r-top3w.f}

  form HEADER SKIP
   "Check/Journal#"
   "Date" at 17
   "        Amount" AT 33
   "Vendor" at 59 skip
   FILL("-",130) FORMAT "x(130)"
   with frame r-top.

form tt-number FORMAT "x(13)"
     tt-date at 17 FORMAT "99/99/99"
     tt-amt AT 33 format "->>,>>>,>>9.99"
     tt-vend at 59 space(1)
     tt-name
     with frame f1 DOWN width 132 no-box NO-LABELS NO-ATTR-SPACE.

form skip(2) space(52) "(There are no reconciled checks/journals/deposits)"
     with frame no-matching-record width 132 no-box no-labels.

form skip(2) space(58) "(End of the report)"
     with frame end-of-report width 132 no-box no-labels.
      



  assign
   str-tit  = coname + " - " + loname
   str-tit2 = "A/P RECONCILED CHECK/JOURNAL/DEPOSIT REGISTER    " 
   str-tit3 = ""
   x = (112 - length(str-tit)) / 2
   str-tit  = fill(" ",x) + str-tit
   x = (114 - length(str-tit2)) / 2
   str-tit2 = fill(" ",x) + str-tit2
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3
   v-chk-tot = 0 
   v-bank-code =  bank_code .      

  /*{sys/inc/print1.i}*/
  if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + vTextFile
      init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}



FIND LAST period NO-LOCK
    WHERE period.company EQ cocode
      AND period.pst     LE end_date
      AND period.pend    GE end_date
    NO-ERROR.

IF AVAIL period THEN DO:
  RUN ap/reconcilrpt.p(INPUT v-bank-code ).
   
  VIEW FRAME r-top.

  FOR EACH reconcile WHERE tt-date LE end_date
      BREAK BY tt-bank BY tt-date BY tt-type BY tt-number:

    IF FIRST-OF(tt-bank) THEN PAGE.

    IF tt-type EQ 1 THEN tt-amt = tt-amt * -1.

    IF tt-cleared THEN DO: 
      IF tt-type EQ 1 THEN
        v-chk-tot = v-chk-tot + tt-amt.
      ELSE
      IF tt-type EQ 2 OR tt-type EQ 4 THEN
        v-dep-tot = v-dep-tot + tt-amt.
      ELSE
        v-jrn-tot = v-jrn-tot + tt-amt.

      DISPLAY tt-number
              tt-date
              tt-amt
              tt-vend
              tt-name
          WITH FRAME f1.
      DOWN WITH FRAME f1.

      v-postable = YES.
    END.

    ELSE v-unc-tot = v-unc-tot + tt-amt.

    IF tt-type EQ 1 THEN tt-amt = tt-amt * -1.

    IF LAST-OF(tt-bank) THEN DO:
      RELEASE account.
      FIND FIRST bank NO-LOCK
          WHERE bank.company   EQ cocode
            AND bank.bank-code EQ tt-bank
          NO-ERROR.
      IF AVAIL bank THEN
      FIND FIRST account NO-LOCK
          WHERE account.company EQ bank.company
            AND account.actnum  EQ bank.actnum
          NO-ERROR.
      
      IF AVAIL account THEN
        RUN gl/gl-open2.p (ROWID(account), period.pst, end_date, OUTPUT v-bnk-tot).

      ELSE v-bnk-tot = 0.

      PUT SKIP(1)
          "Bank:" tt-bank
          "Total Checks:"           TO 31
          v-chk-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP
          "Total Deposits:"         TO 31
          v-dep-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP
          "Total GL Entries:"       TO 31
          v-jrn-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP(1)
          "Beginning Book Balance:" TO 31
          v-bnk-tot                 AT 33 FORMAT "->>,>>>,>>9.99" 
          "Total in Transit:"       TO 31
          v-unc-tot                 AT 33 FORMAT "->>,>>>,>>9.99" SKIP
          "Ending Bank Balance:"    TO 31
          v-bnk-tot - v-unc-tot     AT 33 FORMAT "->>,>>>,>>9.99" SKIP(1).

      ASSIGN
       v-chk-tot = 0
       v-dep-tot = 0
       v-jrn-tot = 0
       v-unc-tot = 0.
    END.
  END. /* for each reconcile record */
END.

ELSE cError = "No Period exists for End Date," +
             "reconcilation cannot be completed..." .
     RETURN.

DISPLAY WITH FRAME end-of-report.




END PROCEDURE.

PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
postit:
DO TRANSACTION:
  FOR EACH reconcile WHERE tt-cleared AND tt-date LE end_date
      ON ERROR UNDO postit, LEAVE postit:

    IF tt-type EQ 1 THEN DO:
      FIND ap-pay WHERE ROWID(ap-pay) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ap-pay THEN ap-pay.reconciled = YES.
    END.

    ELSE
    IF tt-type EQ 2 THEN
    FOR EACH tt-cash WHERE tt-trnum EQ INT(SUBSTR(tt-number,4,10)),
        FIRST ar-cash
        WHERE ROWID(ar-cash)     EQ tt-cash.row-id
          AND ar-cash.reconciled EQ NO
          AND ar-cash.posted     EQ YES
          AND ar-cash.memo       EQ NO
          AND ar-cash.bank-code  EQ tt-bank
          EXCLUSIVE-LOCK:
      ar-cash.reconciled = YES.
    END.

    ELSE
    IF tt-type EQ 3 THEN DO:
      FIND gl-jrn WHERE ROWID(gl-jrn) EQ tt-rowid EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL gl-jrn THEN gl-jrn.reconciled = YES.
    END.

    ELSE
    IF tt-type EQ 4 THEN
    FOR EACH ar-mcash NO-LOCK WHERE ROWID(ar-mcash) EQ tt-rowid,
        FIRST ar-mcash-ref
        WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
          AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
          AND ar-mcash-ref.company  EQ "ar-mcash"
          EXCLUSIVE-LOCK
        USE-INDEX rec_key:
      ar-mcash-ref.val[1] = INT(YES).
    END.
  END.
END. /* do postit */


END PROCEDURE.
