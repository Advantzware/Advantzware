/* addon/touch/crt-break.i   */
   CREATE bf-machtran.
   ASSIGN bf-machtran.company = company_code
                       bf-machtran.machine = machine_code
                       bf-machtran.job_number = job_number
                       bf-machtran.job_sub = INTEGER(job_sub)
                       bf-machtran.form_number = INTEGER(form_number)
                       bf-machtran.blank_number = INTEGER(blank_number)
                       bf-machtran.pass_sequence = INTEGER(pass_sequence)
                       bf-machtran.start_date = TODAY
                       bf-machtran.start_time = lv-shift_break_START_time
                       bf-machtran.end_date = TODAY
                       bf-machtran.end_time = lv-shift_break_end_time
                       bf-machtran.shift = missingshift
                       bf-machtran.jobseq = IF AVAILABLE jobseq THEN jobseq.jobseq ELSE 0
                       bf-machtran.charge_code = shift_break.charge_code
                       bf-machtran.completed = v-completed
                       .
                       {custom/calctime.i &file="bf-machtran"}
