/* ce/updunit#.i */

IF {1}.est-type < 5 THEN
DO:
    {1}.side = "".

      DO v-side-count = 1 TO 17:
         IF {1}.i-code2[v-side-count] NE "" THEN
            {1}.side[v-side-count] = "F".
         ELSE
            {1}.side[v-side-count] = " ".
      END.
END.
