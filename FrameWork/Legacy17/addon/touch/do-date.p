 /* addon/touch/do-date.p */
 DEF INPUT-OUTPUT PARAM ip-machtran-recid AS RECID.
 DEF INPUT PARAM ip-completed AS LOG NO-UNDO.
 DEF BUFFER bf-machtran FOR machtran.

 FIND machtran WHERE RECID(machtran) = ip-machtran-recid.

 ASSIGN machtran.end_date = machtran.start_date + 1
        machtran.run_qty = 0
        machtran.waste_qty = 0
        machtran.end_time = 86400
        machtran.completed = ip-completed.
 {custom/calctime.i &file="machtran"}

 CREATE bf-machtran.
 ASSIGN bf-machtran.company = machtran.company
              bf-machtran.machine = machtran.machine
              bf-machtran.job_number = machtran.job_number
              bf-machtran.job_sub = machtran.job_sub
              bf-machtran.form_number = machtran.form_number
              bf-machtran.blank_number = machtran.blank_number
              bf-machtran.pass_sequence = machtran.pass_sequence
              bf-machtran.start_date = TODAY
              bf-machtran.start_time = 0
              bf-machtran.jobseq = machtran.jobseq
              bf-machtran.charge_code = machtran.charge_code        
              bf-machtran.completed = ip-completed
              bf-machtran.shift = machtran.shift
              ip-machtran-recid = RECID(bf-machtran).
    
     /*RUN Get-Shift(company_code,machine_code,machtran.start_time,job_sequence,OUTPUT machtran.shift).*/
              
