// sa-sls03.i

FOR EACH ar-inv NO-LOCK
    WHERE ar-inv.company  EQ cocode
      AND ar-inv.posted   EQ YES
      AND ar-inv.cust-no  EQ cust.cust-no
      AND ar-inv.inv-date GE {1}
      AND ar-inv.inv-date LE {2}       
      AND (ar-inv.type    NE "FC"
       OR  v-inc-fc       EQ YES)
    :
    CREATE tt-report.
    ASSIGN
        tt-report.term-id = ""
        tt-report.key-09  = cust.cust-no
        tt-report.key-10  = "ar-inv"
        tt-report.rec-id  = RECID(ar-inv)
        .
END.

FOR EACH ar-cash NO-LOCK
    WHERE ar-cash.company    EQ cocode
      AND ar-cash.cust-no    EQ cust.cust-no
      AND ar-cash.check-date GE {1}
      AND ar-cash.check-date LE {2}
      AND ar-cash.posted     EQ YES,
    EACH ar-cashl NO-LOCK
    WHERE ar-cashl.c-no    EQ ar-cash.c-no
      AND ar-cashl.posted  EQ YES
      AND ar-cashl.memo    EQ YES
      AND CAN-FIND(FIRST account
                   WHERE account.company EQ ar-cashl.company
                     AND account.actnum  EQ ar-cashl.actnum
                     AND account.type    EQ "R")
    :
    CREATE tt-report.
    ASSIGN
        tt-report.term-id = ""
        tt-report.key-09  = cust.cust-no
        tt-report.key-10  = "ar-cashl"
        tt-report.rec-id  = RECID(ar-cashl)
        .
END.
