/* ce/updunit#.i */

IF {1}.est-type < 5 THEN
DO:
   FIND FIRST reftable WHERE
        reftable.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING({2},">")) AND
        reftable.company  EQ {1}.company AND
        reftable.loc      EQ {1}.est-no AND
        reftable.code     EQ STRING({1}.form-no,"9999999999") AND
        reftable.code2    EQ STRING({1}.blank-no,"9999999999")
        NO-ERROR.

   IF NOT AVAIL reftable THEN DO:
      CREATE reftable.
      ASSIGN
         reftable.reftable = "ce/v-est3.w Unit#" + TRIM(STRING({2},">"))
         reftable.company  = {1}.company
         reftable.loc      = {1}.est-no
         reftable.code     = STRING({1}.form-no,"9999999999")
         reftable.code2    = STRING({1}.blank-no,"9999999999").
   END.
   
   reftable.dscr = "".

   IF {2} = 0 THEN
      DO v-side-count = 1 TO 12:
         IF {1}.i-code2[v-side-count] NE "" THEN
            reftable.dscr = reftable.dscr + "F".
         ELSE
            reftable.dscr = reftable.dscr + " ".
      END.

   ELSE IF {2} = 1 THEN
      DO v-side-count = 13 TO 17:
         IF {1}.i-code2[v-side-count] NE "" THEN
            reftable.dscr = reftable.dscr + "F".
         ELSE
            reftable.dscr = reftable.dscr + " ".
      END.

   RELEASE reftable.
END.
