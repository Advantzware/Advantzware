/* est/fgupdtax.i    update taxable info to itemfg */

 FIND FIRST cust WHERE cust.company = {1}.company
                   AND cust.cust-no = {1}.cust-no NO-LOCK NO-ERROR.
 IF AVAIL cust AND cust.SORT = "Y" THEN DO:
      IF AVAIL itemfg AND itemfg.i-no NE "" THEN DO:
          ASSIGN itemfg.taxable = TRUE.
/* Removed FGTAXABLE */
      END.
 END.
