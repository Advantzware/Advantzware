
{sys/inc/VAR.i SHARED}

{ap/reconcil.i}

DEF VAR ld AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR v-refnum AS CHAR NO-UNDO.
DEF BUFFER ar-mcash-ref2 FOR reftable.

EMPTY TEMP-TABLE reconcile.

FOR EACH ap-pay NO-LOCK WHERE 
    ap-pay.company    EQ cocode AND 
    ap-pay.reconciled EQ NO AND 
    ap-pay.memo       EQ NO:
        
    IF NOT CAN-FIND (FIRST bank WHERE 
        bank.company   EQ ap-pay.company AND 
        bank.bank-code EQ ap-pay.bank-code) THEN NEXT.    
    
    ASSIGN 
        v-refnum = "AC" + STRING(ap-pay.check-no, "999999").

    FIND FIRST ap-ledger NO-LOCK WHERE 
        ap-ledger.company  EQ ap-pay.company AND 
        ap-ledger.vend-no  EQ ap-pay.vend-no AND 
        ap-ledger.refnum   EQ v-refnum AND 
        ap-ledger.ref-date EQ ap-pay.check-date
        NO-ERROR.

    IF NOT AVAIL ap-ledger THEN DO:
        ASSIGN 
            v-refnum = "CHK# " + string(ap-pay.check-no) + " CD#" + ap-pay.bank-code.

        FIND FIRST ap-ledger NO-LOCK WHERE 
            ap-ledger.company  EQ ap-pay.company AND 
            ap-ledger.vend-no  EQ ap-pay.vend-no AND 
            ap-ledger.refnum   EQ v-refnum AND 
            ap-ledger.ref-date EQ ap-pay.check-date
            NO-ERROR.
    END.

    IF AVAIL ap-ledger THEN DO:
        IF CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN FIND FIRST ap-dis NO-LOCK WHERE 
            ap-dis.company   EQ ap-pay.company AND 
            ap-dis.check-no  EQ ap-pay.check-no AND 
            ap-dis.bank-code EQ ap-pay.bank-code AND 
            ap-dis.vend-no   EQ ap-pay.vend-no
            NO-ERROR.
        IF NOT AVAIL ap-dis THEN FIND vend NO-LOCK WHERE 
            vend.company EQ ap-pay.company AND 
            vend.vend-no EQ ap-pay.vend-no
            NO-ERROR.
      
        CREATE reconcile.
        ASSIGN
            reconcile.tt-type    = 1
            reconcile.tt-rowid   = ROWID(ap-pay)
            reconcile.tt-number  = STRING(ap-pay.check-no,"999999")
            reconcile.tt-date    = ap-ledger.tr-date
            reconcile.tt-amt     = ap-pay.check-amt
            reconcile.tt-bank    = ap-pay.bank-code
            reconcile.tt-vend    = ap-pay.vend-no
            reconcile.tt-name    = IF AVAIL ap-dis THEN ap-dis.payee ELSE
                                    IF AVAIL vend THEN vend.name ELSE "Not on File..."
            reconcile.tt-cleared = ap-pay.cleared.
    END.
END.

FOR EACH ap-pay NO-LOCK WHERE 
    ap-pay.company    EQ cocode AND 
    ap-pay.reconciled EQ ? AND 
    ap-pay.memo       EQ NO:
        
    IF NOT CAN-FIND (FIRST bank WHERE 
        bank.company   EQ ap-pay.company AND 
        bank.bank-code EQ ap-pay.bank-code) THEN NEXT.    
    
    ASSIGN 
        v-refnum = "AC" + STRING(ap-pay.check-no, "999999").

    FIND FIRST ap-ledger NO-LOCK WHERE 
        ap-ledger.company  EQ ap-pay.company AND 
        ap-ledger.vend-no  EQ ap-pay.vend-no AND 
        ap-ledger.refnum   EQ v-refnum AND 
        ap-ledger.ref-date EQ ap-pay.check-date
        NO-ERROR.

    IF NOT AVAIL ap-ledger THEN DO:
        ASSIGN 
            v-refnum = "CHK# " + string(ap-pay.check-no) + " CD#" + ap-pay.bank-code.
      
        FIND FIRST ap-ledger NO-LOCK WHERE 
            ap-ledger.company  EQ ap-pay.company AND 
            ap-ledger.vend-no  EQ ap-pay.vend-no AND 
            ap-ledger.refnum   EQ v-refnum AND 
            ap-ledger.ref-date EQ ap-pay.check-date
            NO-ERROR.
    END.
   
    IF AVAIL ap-ledger THEN DO:
        IF CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN FIND FIRST ap-dis NO-LOCK WHERE 
            ap-dis.company   EQ ap-pay.company AND 
            ap-dis.check-no  EQ ap-pay.check-no AND 
            ap-dis.bank-code EQ ap-pay.bank-code AND 
            ap-dis.vend-no   EQ ap-pay.vend-no
            NO-ERROR.
        IF NOT AVAIL ap-dis THEN FIND vend NO-LOCK WHERE 
            vend.company EQ ap-pay.company AND 
            vend.vend-no EQ ap-pay.vend-no
            NO-ERROR. 
      
        CREATE reconcile.
        ASSIGN
            reconcile.tt-type    = 1
            reconcile.tt-rowid   = ROWID(ap-pay)
            reconcile.tt-number  = STRING(ap-pay.check-no)
            reconcile.tt-date    = ap-ledger.tr-date
            reconcile.tt-amt     = ap-pay.check-amt
            reconcile.tt-bank    = ap-pay.bank-code
            reconcile.tt-vend    = ap-pay.vend-no
            reconcile.tt-name    = IF AVAIL ap-dis THEN ap-dis.payee ELSE
                                    IF AVAIL vend THEN vend.name ELSE "Not on File..."
            reconcile.tt-cleared = ap-pay.cleared.
    END.
   
    IF CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no) THEN DO:
        ASSIGN 
            v-refnum = "VOIDED CHECK" + STRING(ap-pay.check-no,"zzzzzzz9").

        FIND FIRST ap-ledger NO-LOCK WHERE 
            ap-ledger.company EQ ap-pay.company AND 
            ap-ledger.vend-no EQ ap-pay.vend-no AND 
            ap-ledger.refnum  EQ v-refnum
            NO-ERROR.
        IF AVAIL ap-ledger THEN DO:
            CREATE reconcile.
            ASSIGN
                reconcile.tt-type    = 1
                reconcile.tt-rowid   = ROWID(ap-pay)
                reconcile.tt-number  = STRING(ap-pay.check-no)
                reconcile.tt-date    = ap-ledger.tr-date
                reconcile.tt-amt     = - ap-pay.check-amt
                reconcile.tt-bank    = ap-pay.bank-code
                reconcile.tt-vend    = ap-pay.vend-no
                reconcile.tt-name    = IF AVAIL ap-dis THEN ap-dis.payee ELSE
                                        IF AVAIL vend THEN vend.name ELSE "Not on File..."
                reconcile.tt-cleared = ap-pay.cleared.
        END.
    END.
END.

FOR EACH ar-cash NO-LOCK WHERE 
    ar-cash.company    EQ cocode AND 
    ar-cash.reconciled EQ NO AND 
    ar-cash.posted     EQ YES AND 
    ar-cash.memo       EQ NO,
    FIRST ar-ledger NO-LOCK WHERE 
        ar-ledger.company  EQ ar-cash.company AND 
        ar-ledger.cust-no  EQ ar-cash.cust-no AND 
        ar-ledger.ref-date EQ ar-cash.check-date AND 
        ar-ledger.ref-num  EQ "CHK# " + STRING(ar-cash.check-no,"9999999999")
        BREAK BY ar-ledger.tr-num:
        
        IF NOT CAN-FIND (FIRST bank WHERE 
            bank.company   EQ ar-cash.company AND 
            bank.bank-code EQ ar-cash.bank-code) THEN NEXT.
            
        CREATE tt-cash.
        ASSIGN
            tt-cash.row-id = ROWID(ar-cash)
            tt-trnum       = ar-ledger.tr-num
            ld             = ld + ar-cash.check-amt.
    
        IF LAST-OF(ar-ledger.tr-num) THEN DO:

            CREATE reconcile.
            ASSIGN
                reconcile.tt-type    = 2
                reconcile.tt-rowid   = ROWID(ar-cash)
                reconcile.tt-number  = "Dep" + STRING(tt-trnum,"9999999999")
                reconcile.tt-date    = ar-ledger.tr-date
                reconcile.tt-amt     = ld
                reconcile.tt-bank    = ar-cash.bank-code
                reconcile.tt-cleared = ar-cash.cleared.
            ASSIGN 
                ld = 0.
        END.
END. 

FOR EACH gl-jrn NO-LOCK WHERE 
    gl-jrn.company    EQ cocode AND 
    gl-jrn.reconciled EQ NO AND 
    gl-jrn.posted     EQ YES,
    EACH gl-jrnl NO-LOCK WHERE
         gl-jrnl.j-no EQ gl-jrn.j-no
        BREAK BY gl-jrnl.actnum 
              BY gl-jrnl.j-no:

    FIND FIRST bank NO-LOCK WHERE 
        bank.company EQ gl-jrn.company AND 
        bank.actnum  EQ gl-jrnl.actnum
        NO-ERROR.
    IF NOT AVAIL bank THEN NEXT.

    ASSIGN 
        ld = ld + gl-jrnl.tr-amt.

    IF gl-jrn.cleared THEN ASSIGN 
        ll = YES.

    IF LAST-OF(gl-jrnl.j-no) THEN DO:
        CREATE reconcile.
        ASSIGN
            reconcile.tt-type    = 3
            reconcile.tt-rowid   = ROWID(gl-jrn)
            reconcile.tt-number  = STRING(gl-jrn.journal)
            reconcile.tt-date    = gl-jrn.tr-date
            reconcile.tt-amt     = ld
            reconcile.tt-bank    = IF AVAIL bank THEN bank.bank-code ELSE ""
            reconcile.tt-name    = gl-jrnl.dscr
            reconcile.tt-cleared = ll.
        ASSIGN 
            ld         = 0
            ll         = NO.
    END.
END.

FOR EACH ar-mcash NO-LOCK WHERE 
    ar-mcash.company EQ cocode AND 
    ar-mcash.posted  EQ YES:
    IF NOT CAN-FIND(FIRST ar-mcash-ref WHERE 
                        ar-mcash-ref.rec_key  EQ ar-mcash.rec_key AND 
                        ar-mcash-ref.reftable EQ "ar-mcash-ref" AND 
                        ar-mcash-ref.company  EQ "ar-mcash"
                        USE-INDEX rec_key) THEN DO TRANSACTION:
        CREATE ar-mcash-ref.
        ASSIGN
            ar-mcash-ref.rec_key  = ar-mcash.rec_key
            ar-mcash-ref.reftable = "ar-mcash-ref"
            ar-mcash-ref.company  = "ar-mcash".
        RELEASE ar-mcash-ref.
    END.
END.

FOR EACH ar-mcash NO-LOCK WHERE 
    ar-mcash.company EQ cocode AND 
    ar-mcash.posted  EQ YES,
    FIRST ar-ledger NO-LOCK WHERE 
        ar-ledger.company  EQ ar-mcash.company AND 
        ar-ledger.cust-no  EQ "" AND 
        ar-ledger.ref-date EQ ar-mcash.check-date AND 
        ar-ledger.ref-num  EQ STRING(ar-mcash.m-no) + " " + ar-mcash.payer,
    FIRST ar-mcash-ref2 NO-LOCK WHERE 
        ar-mcash-ref2.rec_key  EQ ar-mcash.rec_key AND 
        ar-mcash-ref2.reftable EQ "ar-mcash-ref" AND 
        ar-mcash-ref2.company  EQ "ar-mcash" AND 
        ar-mcash-ref2.val[1]   EQ 0:

    IF NOT CAN-FIND (FIRST bank WHERE 
        bank.company   EQ ar-mcash.company AND 
        bank.bank-code EQ ar-mcash.bank-code) THEN NEXT. 
        
    CREATE reconcile.
    ASSIGN
        reconcile.tt-type    = 4
        reconcile.tt-rowid   = ROWID(ar-mcash)
        reconcile.tt-number  = "Misc Cash"
        reconcile.tt-date    = ar-ledger.tr-date
        reconcile.tt-amt     = ar-mcash.check-amt
        reconcile.tt-bank    = ar-mcash.bank-code
        reconcile.tt-name    = ar-mcash.payer
        reconcile.tt-cleared = ar-mcash-ref2.val[2] EQ 1.
END.
