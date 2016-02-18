
DEFINE INPUT PARAMETER ip-bank-code AS CHAR NO-UNDO. 
def var v-bank-act like bank.actnum no-undo.


{sys/inc/VAR.i SHARED}

{ap/reconcil.i}

DEF VAR ld AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR v-refnum AS CHAR NO-UNDO.

EMPTY TEMP-TABLE reconcile.

IF ip-bank-code NE "" THEN DO:
    FIND FIRST bank NO-LOCK
        WHERE bank.company   EQ cocode
          AND bank.bank-code EQ ip-bank-code
        NO-ERROR.
    v-bank-act = IF AVAIL bank THEN bank.actnum ELSE FILL("z",100).
END.

FOR EACH ap-pay NO-LOCK
    WHERE ap-pay.company    EQ cocode
      AND ap-pay.reconciled EQ NO
      AND ap-pay.memo       EQ NO
      AND (ap-pay.bank-code EQ ip-bank-code OR ip-bank-code EQ "")
    USE-INDEX si-reconciled,
    FIRST bank FIELDS(bank-code) NO-LOCK
    WHERE bank.company   EQ ap-pay.company
      AND bank.bank-code EQ ap-pay.bank-code:

    FIND FIRST vend NO-LOCK
        WHERE vend.company EQ ap-pay.company
          AND vend.vend-no EQ ap-pay.vend-no
        NO-ERROR.
   
    RELEASE ap-ledger.

    v-refnum = "AC" + STRING(ap-pay.check-no, "999999").

    FIND FIRST ap-ledger NO-LOCK
        WHERE ap-ledger.company  EQ ap-pay.company
          AND ap-ledger.vend-no  EQ ap-pay.vend-no
          AND ap-ledger.refnum   EQ v-refnum
          AND ap-ledger.ref-date EQ ap-pay.check-date
        USE-INDEX ap-ledger NO-ERROR.

    IF NOT AVAIL ap-ledger THEN
    DO:
       v-refnum = "CHK# " + string(ap-pay.check-no) +
                  " CD#" + IF AVAIL bank THEN bank.bank-code ELSE  "".

       FIND FIRST ap-ledger NO-LOCK
           WHERE ap-ledger.company  EQ ap-pay.company
             AND ap-ledger.vend-no  EQ ap-pay.vend-no
             AND ap-ledger.refnum   EQ v-refnum
             AND ap-ledger.ref-date EQ ap-pay.check-date
        USE-INDEX ap-ledger NO-ERROR.
   
    END.
    IF AVAIL ap-ledger THEN DO:
   
       IF CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN
          FIND FIRST ap-dis NO-LOCK
              WHERE ap-dis.company   EQ ap-pay.company
                AND ap-dis.check-no  EQ ap-pay.check-no
                AND ap-dis.bank-code EQ ap-pay.bank-code
                AND ap-dis.vend-no   EQ ap-pay.vend-no
              NO-ERROR.
      
       CREATE reconcile.
       ASSIGN
        tt-type    = 1
        tt-rowid   = ROWID(ap-pay)
        tt-number  = STRING(ap-pay.check-no)
        tt-date    = ap-ledger.tr-date
        tt-amt     = ap-pay.check-amt
        tt-bank    = ap-pay.bank-code
        tt-vend    = ap-pay.vend-no
        tt-name    = IF AVAIL ap-dis THEN ap-dis.payee
                     ELSE
                     IF AVAIL vend THEN vend.name ELSE "Not on File..."
        tt-cleared = ap-pay.cleared.
    END.
END.

FOR EACH ap-pay NO-LOCK
    WHERE ap-pay.company    EQ cocode
      AND ap-pay.reconciled EQ ?
      AND ap-pay.memo       EQ NO
      AND (ap-pay.bank-code EQ ip-bank-code OR ip-bank-code EQ "")
    USE-INDEX si-reconciled,
    FIRST bank FIELDS(bank-code) NO-LOCK
    WHERE bank.company   EQ ap-pay.company
      AND bank.bank-code EQ ap-pay.bank-code:

    FIND FIRST vend NO-LOCK
        WHERE vend.company EQ ap-pay.company
          AND vend.vend-no EQ ap-pay.vend-no
        NO-ERROR.
   
    RELEASE ap-ledger.

    v-refnum = "AC" + STRING(ap-pay.check-no, "999999").

    FIND FIRST ap-ledger NO-LOCK
        WHERE ap-ledger.company  EQ ap-pay.company
          AND ap-ledger.vend-no  EQ ap-pay.vend-no
          AND ap-ledger.refnum   EQ v-refnum
          AND ap-ledger.ref-date EQ ap-pay.check-date
        USE-INDEX ap-ledger NO-ERROR.

    IF NOT AVAIL ap-ledger THEN
    DO:
       v-refnum = "CHK# " + string(ap-pay.check-no) +
                  " CD#" + IF AVAIL bank  THEN bank.bank-code ELSE "".
      
       FIND FIRST ap-ledger NO-LOCK
           WHERE ap-ledger.company  EQ ap-pay.company
             AND ap-ledger.vend-no  EQ ap-pay.vend-no
             AND ap-ledger.refnum   EQ v-refnum
             AND ap-ledger.ref-date EQ ap-pay.check-date
           USE-INDEX ap-ledger NO-ERROR.
    END.
   
    IF AVAIL ap-ledger THEN DO:
   
       IF CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN
          FIND FIRST ap-dis NO-LOCK
              WHERE ap-dis.company   EQ ap-pay.company
                AND ap-dis.check-no  EQ ap-pay.check-no
                AND ap-dis.bank-code EQ ap-pay.bank-code
                AND ap-dis.vend-no   EQ ap-pay.vend-no
              NO-ERROR.
      
       CREATE reconcile.
       ASSIGN
        tt-type    = 1
        tt-rowid   = ROWID(ap-pay)
        tt-number  = STRING(ap-pay.check-no)
        tt-date    = ap-ledger.tr-date
        tt-amt     = ap-pay.check-amt
        tt-bank    = ap-pay.bank-code
        tt-vend    = ap-pay.vend-no
        tt-name    = IF AVAIL ap-dis THEN ap-dis.payee
                     ELSE
                     IF AVAIL vend THEN vend.name ELSE "Not on File..."
        tt-cleared = ap-pay.cleared.
    END.
   
    IF /*ap-pay.reconciled EQ ? AND*/
       CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN DO:
       
       v-refnum = "VOIDED CHECK" +
                  STRING(ap-pay.check-no,"zzzzzzz9").

       FIND FIRST ap-ledger NO-LOCK
            WHERE ap-ledger.company EQ ap-pay.company
              AND ap-ledger.vend-no EQ ap-pay.vend-no
              AND ap-ledger.refnum  EQ v-refnum
          NO-ERROR.
       IF AVAIL ap-ledger THEN DO:
         CREATE reconcile.
         ASSIGN
          tt-type    = 1
          tt-rowid   = ROWID(ap-pay)
          tt-number  = STRING(ap-pay.check-no)
          tt-date    = ap-ledger.tr-date
          tt-amt     = - ap-pay.check-amt
          tt-bank    = ap-pay.bank-code
          tt-vend    = ap-pay.vend-no
          tt-name    = IF AVAIL ap-dis THEN ap-dis.payee
                       ELSE
                       IF AVAIL vend THEN vend.name ELSE "Not on File..."
          tt-cleared = ap-pay.cleared.
       END.
    END.
END.

FOR EACH ar-cash NO-LOCK
    WHERE ar-cash.company    EQ cocode
      AND ar-cash.reconciled EQ NO
      AND ar-cash.posted     EQ YES
      AND ar-cash.memo       EQ NO
      AND(ar-cash.bank-code EQ ip-bank-code OR ip-bank-code EQ "")
    USE-INDEX reconciled,
    FIRST ar-ledger FIELDS(tr-num tr-date) NO-LOCK
    WHERE ar-ledger.company  EQ ar-cash.company
      AND ar-ledger.cust-no  EQ ar-cash.cust-no
      AND ar-ledger.ref-date EQ ar-cash.check-date
      AND ar-ledger.ref-num  EQ "CHK# " + STRING(ar-cash.check-no,"9999999999"),
    FIRST bank FIELDS(bank-code) NO-LOCK
    WHERE bank.company   EQ ar-cash.company
      AND bank.bank-code EQ ar-cash.bank-code
    BREAK BY ar-cash.bank-code
          BY ar-ledger.tr-date
          BY ar-ledger.tr-num:

  CREATE tt-cash.
  ASSIGN
   tt-cash.row-id = ROWID(ar-cash)
   tt-trnum       = ar-ledger.tr-num.

  ld = ld + ar-cash.check-amt.

  IF LAST-OF(ar-ledger.tr-num) THEN DO:
    CREATE reconcile.
    ASSIGN
     tt-type    = 2
     tt-rowid   = ROWID(ar-cash)
     tt-number  = "Dep" + STRING(tt-trnum,"9999999999")
     tt-date    = ar-ledger.tr-date
     tt-amt     = ld
     tt-bank    = ar-cash.bank-code
     tt-cleared = ar-cash.cleared
     ld         = 0.
  END.
END.

FOR EACH gl-jrn FIELDS(j-no company cleared journal tr-date) NO-LOCK
    WHERE gl-jrn.company    EQ cocode
      AND gl-jrn.reconciled EQ NO
      AND gl-jrn.posted     EQ YES
    USE-INDEX reconciled,
    EACH gl-jrnl FIELDS(tr-amt actnum dscr j-no) NO-LOCK WHERE
         gl-jrnl.j-no EQ gl-jrn.j-no 
         AND (gl-jrnl.actnum EQ v-bank-act OR v-bank-act EQ "") ,
    FIRST bank FIELDS(bank-code) NO-LOCK
    WHERE bank.company EQ gl-jrn.company
      AND bank.actnum  EQ gl-jrnl.actnum
    BREAK BY gl-jrnl.actnum
          BY gl-jrnl.j-no:
  /* BV - added break by for gl-jrnl.actnum because transfer entries where showing as 0
   04191311  */
  ld = ld + gl-jrnl.tr-amt.

  IF gl-jrn.cleared THEN ll = YES.

  IF LAST-OF(gl-jrnl.j-no) THEN DO:
    CREATE reconcile.
    ASSIGN
     tt-type    = 3
     tt-rowid   = ROWID(gl-jrn)
     tt-number  = STRING(gl-jrn.journal)
     tt-date    = gl-jrn.tr-date
     tt-amt     = ld
     tt-bank    = bank.bank-code
     tt-name    = gl-jrnl.dscr
     tt-cleared = ll
     ld         = 0
     ll         = NO.
  END.
END.

FOR EACH ar-mcash fields(rec_key) NO-LOCK
    WHERE ar-mcash.company EQ cocode
      AND ar-mcash.posted  EQ YES
      AND NOT CAN-FIND(FIRST ar-mcash-ref
                       WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
                         AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
                         AND ar-mcash-ref.company  EQ "ar-mcash"
                       USE-INDEX rec_key) 
    TRANSACTION:
  CREATE ar-mcash-ref.
  ASSIGN
   ar-mcash-ref.rec_key  = ar-mcash.rec_key
   ar-mcash-ref.reftable = "ar-mcash-ref"
   ar-mcash-ref.company  = "ar-mcash".
END.

FOR EACH ar-mcash NO-LOCK
    WHERE ar-mcash.company EQ cocode
      AND ar-mcash.posted  EQ YES 
      AND (ar-mcash.bank-code EQ ip-bank-code OR ip-bank-code EQ ""),
    FIRST ar-ledger FIELDS(tr-num tr-date) NO-LOCK
    WHERE ar-ledger.company  EQ ar-mcash.company
      AND ar-ledger.cust-no  EQ ""
      AND ar-ledger.ref-date EQ ar-mcash.check-date
      AND ar-ledger.ref-num  EQ STRING(ar-mcash.m-no) + " " + ar-mcash.payer,
    FIRST ar-mcash-ref NO-LOCK
    WHERE ar-mcash-ref.rec_key  EQ ar-mcash.rec_key
      AND ar-mcash-ref.reftable EQ "ar-mcash-ref"
      AND ar-mcash-ref.company  EQ "ar-mcash"
      AND ar-mcash-ref.val[1]   EQ INT(NO)
    USE-INDEX rec_key,
    FIRST bank FIELDS(bank-code) NO-LOCK
    WHERE bank.company   EQ ar-mcash.company
      AND bank.bank-code EQ ar-mcash.bank-code
    BREAK BY ar-mcash.bank-code
          BY ar-ledger.tr-date
          BY ar-ledger.tr-num:

  CREATE reconcile.
  ASSIGN
   tt-type    = 4
   tt-rowid   = ROWID(ar-mcash)
   tt-number  = "Misc Cash"
   tt-date    = ar-ledger.tr-date
   tt-amt     = ar-mcash.check-amt
   tt-bank    = ar-mcash.bank-code
   tt-name    = ar-mcash.payer
   tt-cleared = ar-mcash-ref.val[2] EQ 1.
END.
