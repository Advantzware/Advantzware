/* addon/touch/do-brkem.i  for machemp*/
 
/*=== CHECK BREAK TIME FOR the shift ==== */
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ company_code
       AND sys-ctrl.name    EQ "TSBREAKS" /*Break time create*/
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
    CREATE sys-ctrl.
    ASSIGN 
        sys-ctrl.company = company_code
        sys-ctrl.name    = "TSBREAKS"
        sys-ctrl.descrip = "Automatically post breaks to machines?"
        .
    MESSAGE "System control record NOT found.  Would you LIKE to have Automatically post breaks to machines?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO  UPDATE sys-ctrl.log-fld.
END.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
    ASSIGN 
        lv-brk-st-time  = 0
        lv-brk-end-time = 0
        lv-got-break    = NO
        .
    FIND FIRST shift_break NO-LOCK USE-INDEX shift
         WHERE shift_break.company    EQ machemp.company
           AND shift_break.shift      EQ machemp.shift
           AND shift_break.start_time GE machemp.start_time
           AND shift_break.start_time LE stoptime
         NO-ERROR.
    DO WHILE AVAILABLE shift_break:
        IF NOT lv-got-break THEN lv-brk-st-time = shift_break.start_time.
        ASSIGN
            lv-brk-end-time = shift_break.end_time
            lv-got-break = YES
            .
        IF stoptime EQ shift_break.end_time THEN DO:
            ASSIGN 
                lv-shift_break_start_time = shift_break.start_time
                lv-shift_break_end_time   = shift_break.end_time
                .
            {addon/touch/crt-break.i}
            LEAVE.
        END.
        ELSE IF stoptime < shift_break.end_time THEN DO:
                ASSIGN 
                    lv-shift_break_start_time = shift_break.start_time
                    lv-shift_break_end_time   = stoptime.
                 {addon/touch/crt-break.i}
                LEAVE.
            END.               
            ELSE DO:
                ASSIGN 
                    lv-shift_break_start_time = shift_break.start_time
                    lv-shift_break_end_time   = shift_break.end_time.
                   {addon/touch/crt-break.i}
                FIND NEXT shift_break NO-LOCK USE-INDEX shift
                     WHERE shift_break.company    EQ machemp.company
                       AND shift_break.shift      EQ machemp.shift
                       AND shift_break.start_time GE machemp.start_time
                       AND shift_break.start_time LE stoptime
                     NO-ERROR.
            END.
    END. /* for each shift_break */
    /*=== end of break time proc */
    IF lv-got-break THEN machemp.end_time = lv-brk-st-time.
END.
