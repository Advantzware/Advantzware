
   /*FIND FIRST reftable
       WHERE reftable.reftable EQ "shipto.mandatory-tax"
         AND reftable.company  EQ cocode
         AND reftable.loc      EQ ""
         AND reftable.code     EQ {1}
         AND reftable.code2    EQ {2}
       NO-LOCK NO-ERROR.
   IF AVAIL reftable AND reftable.val[1] EQ 1 THEN {3} = YES.*/
   FIND FIRST Shipto NO-LOCK
      Where Shipto.company eq cocode
        and shipto.cust-no eq {1}
        and shipto.ship-id eq {2} NO-ERROR .
    IF AVAIL shipto Then
            {3} = shipto.tax-mandatory .
     else  {3} = NO .