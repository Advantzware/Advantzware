
   FIND FIRST reftable
       WHERE reftable.reftable EQ "shipto.mandatory-tax"
         AND reftable.company  EQ cocode
         AND reftable.loc      EQ ""
         AND reftable.code     EQ {1}
         AND reftable.code2    EQ {2}
       NO-LOCK NO-ERROR.
   IF AVAIL reftable AND reftable.val[1] EQ 1 THEN {3} = YES.
