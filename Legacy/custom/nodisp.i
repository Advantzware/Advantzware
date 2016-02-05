/*  custom/nodisp.i  hide 0 value */

/*IF int({1}.SCREEN-VALUE {2} ) = 0 THEN {1}:HIDDEN {2} = YES. */
IF {1} = 0 THEN 
    CASE {2} :
        WHEN 11 THEN HIDE {3}[{2}].
END.
