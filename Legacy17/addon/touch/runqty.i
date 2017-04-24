/* addon/touch/runqty.i */      
      FOR EACH bf-machtran WHERE bf-machtran.company = company_code AND
                               bf-machtran.machine = machine_code AND
                               bf-machtran.job_number = job_number AND
                               bf-machtran.job_sub = INTEGER(job_sub) AND
                               bf-machtran.form_number = INTEGER(form_number) AND
                               bf-machtran.blank_number = INTEGER(blank_number) AND
                               bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:        
         v-runqty = v-runqty + bf-machtran.RUN_qty.
      END.
