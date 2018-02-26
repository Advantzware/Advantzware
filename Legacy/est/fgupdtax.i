/* est/fgupdtax.i    update taxable info to itemfg */

 FIND FIRST cust WHERE cust.company = {1}.company
                   AND cust.cust-no = {1}.cust-no NO-LOCK NO-ERROR.
 IF AVAIL cust AND cust.SORT = "Y" THEN DO:
      IF AVAIL itemfg AND itemfg.i-no NE "" THEN DO:
          FIND FIRST reftable WHERE reftable.reftable EQ "FGTAXABLE"
                                AND reftable.company  EQ {1}.company
                                AND reftable.loc      EQ ""
                                AND reftable.code     EQ itemfg.i-no NO-ERROR.
          IF NOT AVAIL reftable THEN DO:
             CREATE reftable.
             ASSIGN reftable.reftable = "FGTAXABLE"
                    reftable.company  = {1}.company
                    reftable.loc      = ""
                    reftable.code     = itemfg.i-no.
                    reftable.val[1] = 1
                    .
           END.
      END.
 END.
