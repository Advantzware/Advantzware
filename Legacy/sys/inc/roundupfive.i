   /* sys/inc/roundupfive.i  */

   IF {1} LE 0 THEN
      {1} = 0.

   ELSE
   IF {1} LT 5 THEN
      {1} = 5.

   ELSE
   DO:
      IF (TRUNCATE({1},0) = {1}) THEN
         v-tmp-int = INTEGER (TRUNCATE({1},0)).
      ELSE
         v-tmp-int = integer(TRUNCATE({1},0) + 1).
     
      DO WHILE v-tmp-int MOD 5 NE 0:
         v-tmp-int = v-tmp-int + 1.
      END.
     
      {1} = v-tmp-int.
   END.
