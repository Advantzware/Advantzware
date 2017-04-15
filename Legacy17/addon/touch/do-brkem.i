 /* addon/touch/do-brkem.i  for machemp*/
 
 /*=== CHECK BREAK TIME FOR the shift ==== */
 FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ company_code
                      AND sys-ctrl.name    EQ "TSBREAKS" /*Break time create*/
                      NO-LOCK NO-ERROR.
 IF NOT AVAIL sys-ctrl THEN DO:
                  CREATE sys-ctrl.
                  ASSIGN sys-ctrl.company = company_code
                         sys-ctrl.name    = "TSBREAKS"
                         sys-ctrl.descrip = "Automatically post breaks to machines?".
                   MESSAGE "System control record NOT found.  Would you LIKE to have Automatically post breaks to machines?"
                           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO  UPDATE sys-ctrl.log-fld.
               END.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:

         ASSIGN lv-brk-st-time = 0
                lv-brk-end-time = 0
                lv-got-break = NO.

         FIND first shift_break USE-INDEX shift
              WHERE shift_break.company = machemp.company
                                AND shift_break.shift = machemp.shift
                                AND shift_break.START_time >= machemp.START_time
                                AND shift_break.START_time <= stoptime
                                NO-LOCK NO-ERROR.
         DO WHILE AVAIL shift_break:
               IF NOT lv-got-break THEN lv-brk-st-time = shift_break.START_time.
               lv-brk-end-time = shift_break.END_time.
               lv-got-break = YES.

               IF stoptime = shift_break.end_time THEN DO:
                  ASSIGN lv-shift_break_start_time = shift_break.START_time
                         lv-shift_break_end_time = shift_break.END_time.
                  {addon/touch/crt-break.i}
                 LEAVE.
               END.
               ELSE IF stoptime < shift_break.END_time THEN DO:
                 ASSIGN lv-shift_break_start_time = shift_break.START_time
                        lv-shift_break_end_time = stoptime.
                 {addon/touch/crt-break.i}
                 LEAVE.
               END.               
               ELSE DO:
                   ASSIGN lv-shift_break_start_time = shift_break.START_time
                          lv-shift_break_end_time = shift_break.END_time.
                   {addon/touch/crt-break.i}
                   FIND next shift_break USE-INDEX shift
                             WHERE shift_break.company = machemp.company
                                AND shift_break.shift = machemp.shift
                                AND shift_break.START_time >= machemp.START_time
                                AND shift_break.START_time <= stoptime
                                NO-LOCK NO-ERROR.
               END.
         END. /* for each shift_break */
         /*=== end of break time proc */
          IF lv-got-break THEN machemp.END_time = lv-brk-st-time.
END.
