/* batch/rptvalue.i */

WHEN "{2}" THEN DO:
   /* IF "{4}" = "LOGICAL" THEN {3} = {1} = "{5}".
    ELSE*/  {3} = {4}(user-print.field-value[li]).
END.
