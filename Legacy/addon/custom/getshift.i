/* getshift.i */
/* Get a count of the number of matching shifts */
counter = 0.
FOR EACH {&file} NO-LOCK {&where}
    :
    IF (op-shift EQ '' AND
        ip-time GE {&file}.start_time AND ip-time LE {&file}.end_time) OR
       (op-shift EQ '' AND {&file}.start_time GT {&file}.end_time AND
        ip-time GE {&file}.start_time AND ip-time LE 86400) OR
       (op-shift EQ '' AND {&file}.start_time GT {&file}.end_time AND
        ip-time GE 0 AND ip-time LE {&file}.end_time) THEN
    ASSIGN
        counter   = counter + 1
        tmp-shift = {&file}.shift
        .
END. /* each {&file} */

/* If only one matching shift was found, use it */
IF counter EQ 1 THEN DO:
    ASSIGN 
        op-shift = tmp-shift.
    RETURN.
END.
ELSE DO:
    ASSIGN
        op-shift  = ""
        tmp-shift = ""
        .
     
    /* Take the first shift where ip-optype is specified and the start/end times are equal, */
    /* or take the last shift that matches the criteria, sorted by primary index which      */
    /* would be by shift code for the shifts table or machine/shift for the machshft table */
   
    FOR EACH {&file} NO-LOCK {&where}
        :
          
        IF (op-shift EQ '' AND
            ip-time GE {&file}.start_time AND ip-time LE {&file}.end_time) OR
           (op-shift EQ '' AND {&file}.start_time GT {&file}.end_time AND
            ip-time GE {&file}.start_time AND ip-time LE 86400) OR
           (op-shift EQ '' AND {&file}.start_time GT {&file}.end_time AND
            ip-time GE 0 AND ip-time LE {&file}.end_time) THEN DO:
            IF (ip-optype BEGINS 'START' AND {&file}.start_time EQ ip-time) OR 
               (ip-optype BEGINS 'END'   AND {&file}.end_time EQ ip-time) THEN DO:
                op-shift = {&file}.shift.
                RETURN.
            END.
            tmp-shift = {&file}.shift.
        END.
    END. /* each {&file} */

    IF tmp-shift EQ "" THEN DO:
        FOR EACH {&file} NO-LOCK {&where}:
            IF AVAILABLE shifts AND shifts.useSpecificDays      EQ TRUE /* Specific Days */ THEN NEXT.
                  
            IF (op-shift EQ '' AND
                ip-time GE {&file}.start_time AND ip-time LE {&file}.end_time) OR
               (op-shift EQ '' AND {&file}.start_time GT {&file}.end_time AND
                ip-time GE {&file}.start_time AND ip-time LE 86400) OR
               (op-shift EQ '' AND {&file}.start_time GT {&file}.end_time AND
                ip-time GE 0 AND ip-time LE {&file}.end_time) THEN DO:
                IF (ip-optype BEGINS 'START' AND {&file}.start_time EQ ip-time) OR 
                   (ip-optype BEGINS 'END'   AND {&file}.end_time EQ ip-time) THEN DO:
                    op-shift = {&file}.shift.
                    RETURN.
                END.
                tmp-shift = {&file}.shift.
            END.
        END. /* each {&file} */
    END.
    op-shift = tmp-shift .  /* get last shift within range */
END.  /* else */
