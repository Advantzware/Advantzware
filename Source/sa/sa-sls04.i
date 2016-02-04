   
for each ar-inv
    where ar-inv.company  eq cocode
      and ar-inv.posted   eq yes
      and ar-inv.cust-no  eq cust.cust-no
      and ar-inv.inv-date ge {1}
      and ar-inv.inv-date le {2}       
      and (ar-inv.type    ne "FC" or v-inc-fc)
    no-lock
    
    transaction:

  FIND FIRST shipto WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ cust.cust-no
        AND shipto.ship-id EQ ar-inv.ship-id
        AND shipto.ship-state GE fsstate 
        AND shipto.ship-state LE tsstate NO-LOCK NO-ERROR.

  IF NOT AVAIL shipto THEN NEXT.

  create tt-report.
  assign
   tt-report.term-id = ""
   tt-report.key-09  = cust.cust-no
   tt-report.key-10  = "ar-inv"
   tt-report.rec-id  = recid(ar-inv).
end.

for each ar-cash
    where ar-cash.company    eq cocode
      and ar-cash.cust-no    eq cust.cust-no
      and ar-cash.check-date ge {1}
      and ar-cash.check-date le {2}
      and ar-cash.posted     eq yes
    no-lock,

    EACH ar-cashl
    WHERE ar-cashl.c-no    EQ ar-cash.c-no
      AND ar-cashl.posted  EQ YES
      AND ar-cashl.memo    EQ YES
      AND CAN-FIND(FIRST account
                   WHERE account.company EQ ar-cashl.company
                     AND account.actnum  EQ ar-cashl.actnum
                     AND account.type    EQ "R")
    NO-LOCK
    
    transaction:

   FIND FIRST ar-inv WHERE ar-inv.company EQ cocode
       AND ar-inv.inv-no EQ ar-cashl.inv-no NO-LOCK NO-ERROR.

   IF AVAIL ar-inv THEN
   FIND FIRST shipto WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ cust.cust-no
        AND shipto.ship-id EQ ar-inv.ship-id
        AND shipto.ship-state GE fsstate 
        AND shipto.ship-state LE tsstate NO-LOCK NO-ERROR.

  IF NOT AVAIL shipto THEN NEXT.

  create tt-report.
  assign
   tt-report.term-id = ""
   tt-report.key-09  = cust.cust-no
   tt-report.key-10  = "ar-cashl"
   tt-report.rec-id  = recid(ar-cashl).
end.

