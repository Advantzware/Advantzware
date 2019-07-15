

/*------------------------------------------------------------------------
    File        : recreport.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttReconciliationReport NO-UNDO
        FIELD recrpt AS CHAR
        FIELD extra  AS CHAR.
        
       

DEFINE DATASET dsReconciliationReport FOR ttReconciliationReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmrecrpt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbnkcod        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegchkdt      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmendchkdt      AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegchk        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmendchk        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegjno        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmendjno        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmdep           AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmjournl        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmprint         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmclrunrec      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmunclrunrec    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsrtvend       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmextra         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReconciliationReport.

     IF prmUser           = ? THEN ASSIGN     prmUser           = "".   
     IF prmrecrpt         = ? THEN ASSIGN     prmrecrpt         = "". 
     IF prmbnkcod         = ? THEN ASSIGN     prmbnkcod         = "". 
     IF prmbegvend        = ? THEN ASSIGN     prmbegvend        = "". 
     IF prmendvend        = ? THEN ASSIGN     prmendvend        = "". 
     IF prmbegchkdt       = ? THEN ASSIGN     prmbegchkdt       = "".   
     IF prmendchkdt       = ? THEN ASSIGN     prmendchkdt       = "". 
     IF prmbegchk         = ? THEN ASSIGN     prmbegchk         = 0. 
     IF prmendchk         = ? THEN ASSIGN     prmendchk         = 0. 
     IF prmbegjno         = ? THEN ASSIGN     prmbegjno         = 0. 
     IF prmendjno         = ? THEN ASSIGN     prmendjno         = 0.   
     IF prmdep            = ? THEN ASSIGN     prmdep            = "". 
     IF prmjournl         = ? THEN ASSIGN     prmjournl         = "". 
     IF prmprint          = ? THEN ASSIGN     prmprint          = "". 
     IF prmclrunrec       = ? THEN ASSIGN     prmclrunrec       = "". 
     IF prmunclrunrec     = ? THEN ASSIGN     prmunclrunrec     = "". 
     IF prmsrtvend        = ? THEN ASSIGN     prmsrtvend        = "". 
     IF prmextra          = ? THEN ASSIGN     prmextra          = "". 
     IF prmOut            = ? THEN ASSIGN     prmOut            = "".   



DEFINE VARIABLE bank_code       AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_chk-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_chk-no    AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE begin_j-no      AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE begin_vend      AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_chk-date    AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_chk-no      AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE end_j-no        AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE end_vend        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE rd_rec          AS CHARACTER INITIAL "Unreconciled" NO-UNDO.
DEFINE VARIABLE tb_clr          AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_dep          AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_jrn          AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE tb_unclr        AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_vend-sort    AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page  AS INTEGER FORMAT ">>" INITIAL 99 NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.
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

  assign
   begin_chk-date = DATE(01,01,YEAR(TODAY))
   end_chk-date   = DATE(12,31,YEAR(TODAY)).

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD check-act LIKE ap-pay.check-act
    FIELD check-no AS CHAR FORMAT "x(13)"
    FIELD vend-no AS CHAR FORMAT "X(30)"
    FIELD rpt-date LIKE ap-pay.check-date
    FIELD period LIKE ap-pay.period
    FIELD check-amt LIKE ap-pay.check-amt
    FIELD cleared AS CHAR
    FIELD misc-chk AS CHAR FORMAT "X(10)".

    




  IF prmrecrpt = "recrpt " THEN DO:

      ASSIGN
       bank_code           =  prmbnkcod                      
       begin_chk-date      =  date(prmbegchkdt)
       begin_chk-no        =  prmbegchk
       begin_j-no          =  prmbegjno
       begin_vend          =  prmbegvend
       end_chk-date        =  date(prmendchkdt)
       end_chk-no          =  prmendchk
       end_j-no            =  prmendjno
       end_vend            =  prmendvend
       rd_rec              =  prmprint
       tb_clr              =  IF prmclrunrec = "yes" THEN TRUE ELSE FALSE
       tb_dep              =  IF prmdep = "yes" THEN TRUE ELSE FALSE
       tb_jrn              =  IF prmjournl = "yes" THEN TRUE ELSE FALSE
       tb_unclr            =  IF prmunclrunrec = "yes" THEN TRUE ELSE FALSE
       tb_vend-sort        =  IF prmsrtvend = "yes" THEN TRUE ELSE FALSE .
                         

       
      
       
        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vTextFile2 AS CHAR NO-UNDO.
        vTextFile = "recrpt" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vTextFile2 =  "recrpt" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .


        run run-report. 


        
       

   
  CREATE ttReconciliationReport.
    ASSIGN ttReconciliationReport.recrpt = vTextFile .

    
  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* --------------------------------------------------- ap/ap-chkr.p 11/94 gb  */
/*                                                                            */
/* a/p - Check Reconciliation Report                                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3.f}

DEF BUFFER ar-mcash-ref FOR reftable.
DEF BUFFER b-ap-pay FOR ap-pay.

def var v-chkno like ap-chk.check-no format ">>>>>9" init 0 NO-UNDO.
def var v-chkno2 like v-chkno init 999999 NO-UNDO.
def var v-s-date like ap-chk.check-date format "99/99/9999" init "01/01/0001" NO-UNDO.
def var v-e-date like v-s-date init TODAY NO-UNDO.
def var v-bank-code like bank.bank-code no-undo.
def var v-rep-type as ch format "x" no-undo init "U".
def var v-head-1 as ch format 'x(20)' no-undo.
def var v-bank-act like bank.actnum no-undo.
def var v-bank-hld like bank.actnum no-undo.
def var v-tot-amt as dec init 0 format "->,>>>,>>9.99" no-undo.
def var save_id as RECID NO-UNDO.
def var time_stamp as ch NO-UNDO.
DEF VAR v-amt AS DEC NO-UNDO.
DEF VAR ll-skipped AS LOG NO-UNDO.

form tt-report.check-act    column-label "Bank Account #"
     tt-report.check-no     column-label "Chk/Jrnl#"
     tt-report.vend-no      column-label "Vendor#"
     tt-report.rpt-date     column-label "Date"
     tt-report.period       column-label "P"
     tt-report.check-amt    column-label "Amount"
     tt-report.cleared      column-label "CLR"
                            format "x(4)"

    with STREAM-IO width 102 frame a no-attr-space no-box down.

assign
 str-tit2 = "Reconciliation Report"
 {sys/inc/ctrtext.i str-tit2 56}
 
 v-bank-code  = bank_code 
 v-bank-act   = ""
 v-s-date     = begin_chk-date
 v-e-date     = end_chk-date
 v-chkno      = begin_chk-no
 v-chkno2     = end_chk-no 
 v-rep-type   = substr(rd_rec, 1,1).

/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
  assign list-name = tmp-dir + vTextFile
      init-dir = tmp-dir.



{sys/inc/outprint.i value(lines-per-page)}



display "" with frame r-top.



  EMPTY TEMP-TABLE tt-report.

  IF v-bank-code NE "" THEN DO:
    FIND FIRST bank NO-LOCK
        WHERE bank.company   EQ cocode
          AND bank.bank-code EQ v-bank-code
        NO-ERROR.
    v-bank-act = IF AVAIL bank THEN bank.actnum ELSE FILL("z",100).
  END.

  FOR EACH ap-pay
      WHERE ap-pay.company    EQ cocode
        AND ap-pay.vend-no    GE begin_vend
        AND ap-pay.vend-no    LE end_vend
        AND ap-pay.check-no   GE v-chkno
        AND ap-pay.check-no   LE v-chkno2
        AND (ap-pay.check-act EQ v-bank-act OR v-bank-act EQ "")
        AND (ap-pay.bank-code EQ v-bank-code OR v-bank-code EQ "")
        AND ap-pay.posted
        AND ((v-rep-type      EQ "U" AND NOT ap-pay.cleared) OR
             (v-rep-type      EQ "R" AND ap-pay.cleared) OR
             v-rep-type       EQ "A")
        AND ap-pay.memo       EQ NO
      USE-INDEX ap-pay
      
      TRANSACTION:

    RELEASE ap-ledger.
    FIND FIRST ap-ledger NO-LOCK
        WHERE ap-ledger.company  EQ ap-pay.company
          AND ap-ledger.vend-no  EQ ap-pay.vend-no
          AND ap-ledger.refnum   EQ "AC" + STRING(ap-pay.check-no, "999999")
          AND ap-ledger.ref-date EQ ap-pay.check-date
        USE-INDEX ap-ledger NO-ERROR.

    IF NOT AVAIL ap-ledger THEN
    DO:
       IF v-bank-code EQ "" THEN
          find first bank where
               bank.company = ap-pay.company AND
               bank.bank-code = ap-pay.bank-code
               NO-LOCK NO-ERROR.

       IF AVAIL bank THEN
          FIND FIRST ap-ledger NO-LOCK
               WHERE ap-ledger.company  EQ ap-pay.company
                 AND ap-ledger.vend-no  EQ ap-pay.vend-no
                 AND ap-ledger.refnum   EQ "CHK# " + string(ap-pay.check-no) +
                                           " CD#" + bank.bank-code
                 AND ap-ledger.ref-date EQ ap-pay.check-date
               USE-INDEX ap-ledger NO-ERROR.

       IF v-bank-code EQ "" THEN
          RELEASE bank.
    END.

    ll-skipped = ap-pay.d-no NE 0 AND
                 NOT CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no).

    IF AVAIL ap-ledger               AND
       ap-ledger.tr-date GE v-s-date AND
       ap-ledger.tr-date LE v-e-date THEN DO:
      CREATE tt-report.

      ASSIGN
       tt-report.rec-id    = RECID(ap-pay)
       tt-report.key-01    = ap-pay.check-act
       tt-report.key-02    = (IF tb_vend-sort THEN ap-pay.vend-no ELSE "")
       tt-report.key-03    = STRING(ap-pay.check-no,"9999999999")
       tt-report.check-act = ap-pay.check-act
       tt-report.check-no  = STRING(ap-pay.check-no)
       tt-report.vend-no   = ap-pay.vend-no
       tt-report.rpt-date  = ap-ledger.tr-date
       tt-report.period    = ap-pay.period
       tt-report.check-amt = IF NOT ll-skipped THEN ap-pay.check-amt * -1 ELSE ap-pay.check-amt
       tt-report.cleared   = IF ll-skipped     THEN "Void" ELSE
                             IF ap-pay.cleared THEN "***"  ELSE
                             IF tb_clr         THEN "CLR"  ELSE "".

      IF ap-pay.vend-no EQ "" THEN
      DO:
          FIND FIRST ap-dis WHERE
               ap-dis.d-no = ap-pay.d-no
               NO-LOCK NO-ERROR.

          IF AVAIL ap-dis THEN
          DO:
             tt-report.vend-no = ap-dis.payee.
             RELEASE ap-dis.
          END.
      END.

      IF tb_clr AND NOT ap-pay.cleared THEN ap-pay.cleared = YES.

      IF tb_unclr AND ap-pay.cleared THEN ap-pay.cleared = NO.
    END.

    IF ap-pay.reconciled EQ ? AND NOT ll-skipped THEN DO:
      FIND FIRST ap-ledger NO-LOCK
          WHERE ap-ledger.company EQ ap-pay.company
            AND ap-ledger.vend-no EQ ap-pay.vend-no
            AND ap-ledger.refnum  EQ "VOIDED CHECK" +
                                     STRING(ap-pay.check-no,"zzzzzzz9")
          NO-ERROR.
      IF AVAIL ap-ledger               AND
         ap-ledger.tr-date GE v-s-date AND
         ap-ledger.tr-date LE v-e-date THEN DO:

        CREATE tt-report.
        ASSIGN
         tt-report.rec-id    = RECID(ap-pay)
         tt-report.key-01    = ap-pay.check-act
         tt-report.key-02    = (IF tb_vend-sort THEN ap-pay.vend-no ELSE "")
         tt-report.key-03    = STRING(ap-pay.check-no,"9999999999")
         tt-report.check-act = ap-pay.check-act
         tt-report.check-no  = STRING(ap-pay.check-no)
         tt-report.vend-no   = ap-pay.vend-no
         tt-report.rpt-date  = ap-ledger.tr-date
         tt-report.period    = ap-ledger.period
         tt-report.check-amt = ap-pay.check-amt
         tt-report.cleared   = "VOID".

        IF ap-pay.vend-no EQ "" THEN
        DO:
           FIND FIRST ap-dis WHERE
                ap-dis.d-no = ap-pay.d-no
                NO-LOCK NO-ERROR.
        
           IF AVAIL ap-dis THEN
           DO:
              tt-report.vend-no = ap-dis.payee.
              RELEASE ap-dis.
           END.
        END.
      END.
    END.
  END.

  IF tb_dep THEN DO:
    FOR EACH ar-cash
        WHERE ar-cash.company    EQ cocode
          AND ar-cash.reconciled EQ NO
          AND ar-cash.posted     EQ YES
          AND ar-cash.memo       EQ NO
          AND (ar-cash.bank-code EQ v-bank-code OR v-bank-code EQ "")
          AND ar-cash.check-date GE v-s-date
          AND ar-cash.check-date LE v-e-date
          AND ((v-rep-type       EQ "U" AND NOT ar-cash.cleared) OR
               (v-rep-type       EQ "R" AND ar-cash.cleared) OR
               v-rep-type        EQ "A")
        USE-INDEX reconciled,
        FIRST ar-ledger NO-LOCK
        WHERE ar-ledger.company  EQ ar-cash.company
          AND ar-ledger.cust-no  EQ ar-cash.cust-no
          AND ar-ledger.ref-date EQ ar-cash.check-date
          AND ar-ledger.ref-num  EQ "CHK# " + STRING(ar-cash.check-no,"9999999999"),
        FIRST bank NO-LOCK
        WHERE bank.company   EQ ar-cash.company
          AND bank.bank-code EQ ar-cash.bank-code
          AND (bank.actnum   EQ v-bank-act OR v-bank-act EQ "")
        BREAK BY ar-cash.bank-code
              BY ar-ledger.tr-date
              BY ar-ledger.tr-num
      
        TRANSACTION:

      v-amt = v-amt + ar-cash.check-amt.

      IF LAST-OF(ar-ledger.tr-num) THEN DO:
        CREATE tt-report.
        ASSIGN
         tt-report.rec-id    = RECID(ar-cash)
         tt-report.key-01    = bank.actnum
         tt-report.key-02    = ""
         tt-report.key-03    = "Dep" + STRING(ar-ledger.tr-num,"9999999999")
         tt-report.check-act = bank.actnum
         tt-report.check-no  = "Deposit"
         tt-report.vend-no   = ""
         tt-report.rpt-date  = ar-ledger.tr-date
         tt-report.check-amt = v-amt
         tt-report.cleared   = IF ar-cash.cleared THEN "***" ELSE
                               IF tb_clr THEN "CLR" ELSE ""
         v-amt               = 0.
      END.

      IF tb_clr AND NOT ar-cash.cleared THEN ar-cash.cleared = YES.

      IF tb_unclr AND ar-cash.cleared THEN ar-cash.cleared = NO.
    END.

    FOR EACH ar-mcash NO-LOCK
        WHERE ar-mcash.company    EQ cocode
          AND ar-mcash.posted     EQ YES
          AND (ar-mcash.bank-code EQ v-bank-code OR v-bank-code EQ "")
          AND ar-mcash.check-date GE v-s-date
          AND ar-mcash.check-date LE v-e-date,
        FIRST ar-ledger NO-LOCK
        WHERE ar-ledger.company  EQ ar-mcash.company
          AND ar-ledger.cust-no  EQ ""
          AND ar-ledger.ref-date EQ ar-mcash.check-date
          AND ar-ledger.ref-num  EQ STRING(ar-mcash.m-no) + " " + ar-mcash.payer,
        FIRST ar-mcash-ref
        WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
          AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
          AND ar-mcash-ref.company  EQ "ar-mcash"
          AND ar-mcash-ref.val[1]   EQ 0
          AND ((v-rep-type       EQ "U" AND ar-mcash-ref.val[2] EQ 0) OR
               (v-rep-type       EQ "R" AND ar-mcash-ref.val[2] NE 0) OR
               v-rep-type        EQ "A")
        USE-INDEX rec_key,
        FIRST bank NO-LOCK
        WHERE bank.company   EQ ar-mcash.company
          AND bank.bank-code EQ ar-mcash.bank-code
          AND (bank.actnum   EQ v-bank-act OR v-bank-act EQ "")
        BREAK BY ar-mcash.bank-code
              BY ar-ledger.tr-date
        
        TRANSACTION:

      FIND FIRST reftable NO-LOCK
      WHERE reftable.reftable = "AR-MCASH"       
        AND reftable.company  = ar-mcash.company
        AND reftable.loc      = STRING(ar-mcash.m-no,">>>>>>9")
        AND reftable.code     = ar-mcash.rec_key NO-ERROR.

      CREATE tt-report.
      ASSIGN
       tt-report.rec-id    = RECID(ar-mcash)
       tt-report.key-01    = bank.actnum
       tt-report.key-02    = ""
       tt-report.key-03    = "Misc Cash"
       tt-report.check-act = bank.actnum
       tt-report.check-no  = "Misc Cash"
       tt-report.vend-no   = ""
       tt-report.rpt-date  = ar-ledger.tr-date
       tt-report.check-amt = ar-mcash.check-amt
       tt-report.cleared   = IF ar-mcash-ref.val[2] NE 0 THEN "***" ELSE
                             IF tb_clr THEN "CLR" ELSE ""
       
       tt-report.misc-chk  = IF AVAIL reftable AND 
                               (TRIM(reftable.code2) NE "" OR
                                reftable.code2 NE "0000000000")
                               THEN reftable.code2 ELSE "".

      IF tb_clr AND ar-mcash-ref.val[2] EQ 0 THEN ar-mcash-ref.val[2] = 1.
      IF tb_unclr AND ar-mcash-ref.val[2] NE 0 THEN ar-mcash-ref.val[2] = 0.
    END.
  END.

  v-amt = 0.

  IF tb_jrn THEN
  FOR EACH gl-jrn
      WHERE gl-jrn.company    EQ cocode
        AND gl-jrn.reconciled EQ NO
        AND gl-jrn.posted     EQ YES
        AND gl-jrn.tr-date    GE v-s-date
        AND gl-jrn.tr-date    LE v-e-date
        AND ((v-rep-type      EQ "U" AND NOT gl-jrn.cleared) OR
             (v-rep-type      EQ "R" AND gl-jrn.cleared) OR
             v-rep-type       EQ "A")
      USE-INDEX reconciled,
      EACH gl-jrnl NO-LOCK
      WHERE gl-jrnl.j-no    EQ gl-jrn.j-no
        AND (gl-jrnl.actnum EQ v-bank-act OR v-bank-act EQ ""),
      FIRST bank NO-LOCK
      WHERE bank.company EQ gl-jrn.company
        AND bank.actnum  EQ gl-jrnl.actnum
      BREAK BY gl-jrnl.actnum
            BY gl-jrnl.j-no
      
      TRANSACTION:

    v-amt = v-amt + gl-jrnl.tr-amt.

    IF LAST-OF(gl-jrnl.j-no) THEN DO:

      CREATE tt-report.
      ASSIGN
       tt-report.rec-id    = RECID(gl-jrn)
       tt-report.key-01    = bank.actnum
       tt-report.key-02    = ""
       tt-report.key-03    = STRING(gl-jrn.journal,"9999999999")
       tt-report.check-act = bank.actnum
       tt-report.check-no  = STRING(gl-jrn.journal)
       tt-report.vend-no   = "Journal"
       tt-report.rpt-date  = gl-jrn.tr-date
       tt-report.check-amt = v-amt
       tt-report.cleared   = IF gl-jrn.cleared THEN "***" ELSE
                             IF tb_clr THEN "CLR" ELSE ""
       v-amt               = 0.
    END.

    IF tb_clr AND NOT gl-jrn.cleared THEN gl-jrn.cleared = YES.

    IF tb_unclr AND gl-jrn.cleared THEN gl-jrn.cleared = NO.
  END.

  FOR EACH tt-report /*,
      FIRST ap-pay WHERE RECID(ap-pay) EQ tt-report.rec-id NO-LOCK*/
      BREAK BY tt-report.key-01
            BY tt-report.key-02
            BY tt-report.key-03
            BY tt-report.rpt-date:

    IF v-bank-hld NE tt-report.check-act THEN DO:
      IF v-bank-hld GT "" THEN PAGE.
      v-bank-hld = tt-report.check-act.
    END.

    DISPLAY tt-report.check-act
            tt-report.check-no
            tt-report.vend-no
            tt-report.rpt-date
            tt-report.period
            tt-report.check-amt
            tt-report.cleared
           SPACE(22)
            tt-report.misc-chk NO-LABEL
        WITH FRAME a.
    DOWN WITH FRAME a.

    v-tot-amt = v-tot-amt + tt-report.check-amt.
  END.  /* for each loop */

  DISPLAY "Total Amount:" AT 73 v-tot-amt WITH STREAM-IO width 102 FRAME tot NO-LABELS.




  
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
