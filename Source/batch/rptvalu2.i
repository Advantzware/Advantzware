/* batch/rptvalu2.i for logical */

WHEN "{2}" THEN DO:
   {3} = user-print.field-value[li] EQ "{5}".
   
END.
