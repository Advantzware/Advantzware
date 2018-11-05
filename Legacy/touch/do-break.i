/* addon/touch/do-break.i */

ASSIGN
    lv-got-break        = NO
    lv-org-shift        = machtran.shift
    lv-next-shift-avail = NO
    lv-num-shift        = 0
    .
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
        lv-brk-st-time       = 0
        lv-brk-end-time      = 0
        lv-prev-brk-end-time = 0 
        lv-got-break         = NO
        starttime            = machtran.start_time
        .
    FIND FIRST shift_break NO-LOCK USE-INDEX shift
         WHERE shift_break.company EQ machtran.company
           AND shift_break.shift   EQ machtran.shift
           AND CAN-FIND(FIRST shifts
                        WHERE shifts.company           EQ shift_break.company
                          AND shifts.shift             EQ shift_break.shift)
                          AND (
                              (shift_break.start_time  LE machtran.start_time
                          AND  shift_break.end_time    GT machtran.start_time)
                                                    
                           OR (shift_break.start_time  LT machtran.end_time
                          AND shift_break.end_time     GE machtran.end_time)
                                                    
                           OR (shift_break.start_time  GE machtran.start_time
                          AND shift_break.end_time     LE machtran.end_time))
         NO-ERROR.
    DO WHILE AVAILABLE shift_break:
        IF NOT lv-got-break THEN
            ASSIGN
                lv-brk-st-time       = shift_break.start_time
                lv-prev-brk-end-time = shift_break.end_time
                .
        ASSIGN 
            lv-brk-end-time = shift_break.end_time
            lv-got-break    = YES
            lv-sht-charge   = shift_break.charge_code
            lv-num-shift    = lv-num-shift + 1
            .
        IF starttime GE shift_break.start_time AND 
            stoptime LE shift_break.end_time THEN DO: 
            lv-got-break = NO.
            LEAVE. /* break time and run hour is same just update time and charge code*/
        END.
        ELSE IF stoptime EQ shift_break.end_time THEN DO:
                ASSIGN 
                    lv-shift_break_start_time = shift_break.start_time
                    lv-shift_break_end_time   = shift_break.end_time
                    .
                {addon/touch/crt-break.i}
                machtran.end_time = lv-brk-st-time.
                {custom/calctime.i &file="machtran"}
                LEAVE.
            END.
            ELSE IF stoptime LT shift_break.end_time THEN DO:
                    ASSIGN 
                        lv-shift_break_start_time = shift_break.start_time
                        lv-shift_break_end_time   = stoptime
                        .
                    {addon/touch/crt-break.i}
                    {custom/calctime.i &file="machtran"}
                    LEAVE.
                END.               
                ELSE IF starttime GT shift_break.start_time THEN DO:
                        ASSIGN 
                            lv-shift_break_start_time = STARTtime
                            lv-shift_break_end_time   = shift_break.end_time /*stoptime*/ .
                        {addon/touch/crt-break.i}
                        {custom/calctime.i &file="machtran"}
                        LEAVE.
                    END.
                    ELSE DO:
                        ASSIGN 
                            lv-shift_break_start_time = shift_break.start_time
                            lv-shift_break_end_time   = shift_break.end_time
                            lv-prev-brk-end-time      = shift_break.end_time
                            .
                        {addon/touch/crt-break.i}
                        machtran.end_time = lv-brk-st-time.
                        {custom/calctime.i &file="machtran"}
                        RUN touch/upd-memp.p (ROWID(machtran)).
                        FIND NEXT shift_break NO-LOCK USE-INDEX shift
                            WHERE shift_break.company    EQ machtran.company
                              AND shift_break.shift      EQ machtran.shift
                              AND shift_break.start_time GE machtran.start_time
                              AND shift_break.start_time LT stoptime
                              AND CAN-FIND(FIRST shifts
                                           WHERE shifts.company EQ shift_break.company
                                             AND shifts.shift EQ shift_break.shift)
                            NO-ERROR.
                        ASSIGN
                            lv-next-shift-avail = AVAILABLE shift_break
                            lv-sht-charge       = IF AVAILABLE shift_break THEN shift_break.charge_code ELSE lv-sht-charge
                            .
                        IF AVAILABLE shift_break THEN lv-num-shift = lv-num-shift + 1.
                        IF AVAILABLE shift_break AND stoptime GT shift_break.end_time THEN DO:
                            /* break time is in middle */
                            ASSIGN 
                                machtran.start_time = lv-shift_break_end_time
                                machtran.end_time   = shift_break.start_time
                                .
                            {custom/calctime.i &file="machtran"}                      
                            CREATE machtran.
                            ASSIGN 
                                machtran.company       = company_code
                                machtran.machine       = machine_code
                                machtran.job_number    = job_number
                                machtran.job_sub       = INTEGER(job_sub)
                                machtran.form_number   = INTEGER(form_number)
                                machtran.blank_number  = INTEGER(blank_number)
                                machtran.pass_sequence = INTEGER(pass_sequence)
                                machtran.start_date    = v-today
                                machtran.start_time    = shift_break.start_time
                                machtran.end_date      = v-today
                                machtran.shift         = lv-org-shift
                                machtran.jobseq        = IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0
                                machtran.charge_code   = charge_code
                                machtran.posted        = NO
                                machtran-rowid         = ROWID(machtran)
                                .
                            RUN touch/crt-memp.p (company_code,machine_code, ROWID(machtran)).
                        END. 
                        ELSE IF AVAILABLE shift_break AND stoptime EQ shift_break.end_time THEN DO:
                                /* break time is in middle */
                                ASSIGN 
                                    machtran.start_time = lv-shift_break_end_time
                                    machtran.end_time   = shift_break.start_time
                                    .
                                {custom/calctime.i &file="machtran"}
                                IF lv-num-shift GE 2 THEN DO: /*more than one breaks */
                                    CREATE machtran.
                                    ASSIGN 
                                        machtran.company       = company_code
                                        machtran.machine       = machine_code
                                        machtran.job_number    = job_number
                                        machtran.job_sub       = INTEGER(job_sub)
                                        machtran.form_number   = INTEGER(form_number)
                                        machtran.blank_number  = INTEGER(blank_number)
                                        machtran.pass_sequence = INTEGER(pass_sequence)
                                        machtran.start_date    = v-today
                                        machtran.start_time    = starttime
                                        machtran.end_date      = v-today
                                        machtran.end_time      = lv-brk-st-time
                                        machtran.shift         = lv-org-shift
                                        machtran.jobseq        = IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0
                                        machtran.charge_code   = charge_code
                                        /*machtran.completed = v-completed */
                                        machtran.posted        = NO
                                        machtran-rowid         = ROWID(machtran)
                                        .
                                    {custom/calctime.i &file="machtran"} 
                                    RUN touch/crt-memp.p (company_code,machine_code, ROWID(machtran)).
                                END.
                                ASSIGN 
                                    lv-shift_break_start_time = shift_break.start_time
                                    lv-shift_break_end_time   = shift_break.end_time
                                    lv-brk-st-time            = shift_break.start_time
                                    lv-brk-end-time           = shift_break.end_time
                                    .
                                {addon/touch/crt-break.i}                      
                                LEAVE.
                            END.
                    END.
    END. /* for each shift_break */
    /*=== end of break time proc */
    
    IF lv-got-break AND
        starttime LT lv-brk-st-time AND
        stoptime  GT lv-brk-end-time THEN DO:
        /* break time is in middle */
        ASSIGN 
            machtran.start_time = lv-brk-end-time
            machtran.end_time   = IF machtran.shift EQ shiftvar THEN stoptime
                                  ELSE endtime
            .
            {custom/calctime.i &file="machtran"}
        RUN touch/upd-memp.p (ROWID(machtran)).
        CREATE machtran.
        ASSIGN
            machtran.company       = company_code
            machtran.machine       = machine_code
            machtran.job_number    = job_number
            machtran.job_sub       = INTEGER(job_sub)
            machtran.form_number   = INTEGER(form_number)
            machtran.blank_number  = INTEGER(blank_number)
            machtran.pass_sequence = INTEGER(pass_sequence)
            machtran.start_date    = v-today
            machtran.start_time    = starttime
            machtran.end_date      = v-today
            machtran.end_time      = lv-brk-st-time
            machtran.shift         = lv-org-shift
            machtran.jobseq        = IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0
            machtran.charge_code   = charge_code
            machtran.completed     = v-completed
            machtran.posted        = NO
            machtran-rowid         = ROWID(machtran)
            .
            {custom/calctime.i &file="machtran"}
        RUN touch/crt-memp.p (company_code,machine_code, ROWID(machtran)).
    END .
    ELSE IF lv-got-break AND stoptime LT lv-brk-end-time THEN DO:
            machtran.end_time = lv-brk-st-time.
            {custom/calctime.i &file="machtran"}
            RUN touch/upd-memp.p (ROWID(machtran)).
        END.
        ELSE IF lv-got-break AND stoptime GT lv-brk-end-time THEN DO:
                /* job starttime = break's starttime */
                ASSIGN
                    machtran.start_time = lv-brk-end-time
                    machtran.end_time = stoptime
                    .
                {custom/calctime.i &file="machtran"}
                RUN touch/upd-memp.p (ROWID(machtran)).
            END.
            ELSE IF lv-got-break AND stoptime = lv-brk-end-time THEN DO:
                    IF lv-num-shift LT 2 THEN DO: 
                        machtran.end_time = lv-brk-st-time.
                        {custom/calctime.i &file="machtran"}
                        RUN touch/upd-memp.p (ROWID(machtran)).
                    END.
                END.
                ELSE IF lv-got-break AND NOT lv-next-shift-avail THEN
                     machtran.end_time = lv-brk-end-time.
                    ELSE IF NOT lv-got-break AND
                            machtran.start_time GE lv-brk-st-time AND
                            machtran.end_time   LE lv-brk-end-time THEN DO:
                            /* job starttime = break's starttime */
                            ASSIGN 
                                machtran.end_time    = stoptime
                                machtran.charge_code = lv-sht-charge
                                .
                            {custom/calctime.i &file="machtran"}
                            RUN touch/upd-memp.p (ROWID(machtran)).
                        END.

END.
