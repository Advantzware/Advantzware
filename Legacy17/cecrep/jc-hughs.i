/* program: cecrep/jc-hughs.i is an include in jcrep/tickrrpt.i which is an include in jcreo/r-ticket.w */
      
         DEF BUFFER b2-ef FOR ef.
         DEF BUFFER b2-eb FOR eb.


         FOR EACH b-reftable WHERE b-reftable.reftable   = "cecrep/d-hughes.w"
                               AND b-reftable.company    = cocode
                               AND int(b-reftable.code2) = 0
                               AND  b-reftable.val[1]    = 0:
            DELETE b-reftable.
         END.

         FOR EACH job-hdr WHERE job-hdr.company               EQ cocode
                            and (production OR
                                 job-hdr.ftick-prnt eq reprint OR
                                 PROGRAM-NAME(2) MATCHES "*r-tickt2*")
                            AND job-hdr.job-no                GE SUBSTR(fjob-no,1,6)
                            AND job-hdr.job-no                LE SUBSTR(tjob-no,1,6)
                            AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
                                TRIM(job-hdr.job-no) +
                                STRING(job-hdr.job-no2,"99")  GE fjob-no
                            AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
                                TRIM(job-hdr.job-no) +
                                STRING(job-hdr.job-no2,"99")  LE tjob-no,
            FIRST job WHERE job.company                   EQ cocode
                        AND job.job                       EQ job-hdr.job
                        AND job.job-no                    EQ job-hdr.job-no
                        AND job.job-no2                   EQ job-hdr.job-no2
                        AND job.stat                      NE "H"
                        AND (tb_app-unprinted EQ NO OR
                            (tb_app-unprinted AND job.pr-printed = NO
                             AND job.opened = YES AND job.cs-to-pr = YES)) NO-LOCK,
            FIRST est WHERE est.company = job.company
                        AND est.est-no                    EQ job.est-no
                        AND est.est-type                  GT 4 NO-LOCK,
            FIRST cust WHERE cust.company                 EQ cocode
                         AND cust.cust-no                 EQ job-hdr.cust-no NO-LOCK,
            FIRST itemfg WHERE itemfg.company             EQ cocode
                           AND itemfg.i-no                EQ job-hdr.i-no NO-LOCK
                      BREAK BY job.job-no
                            BY job.job-no2:

            /* create a temp form table for each form that belongs for this est */
            IF AVAIL est THEN DO:
               FOR EACH b2-ef WHERE b2-ef.company = est.company AND b2-ef.est-no EQ est.est-no NO-LOCK.
                  CREATE t-ef-form.
                  ASSIGN 
                     t-ef-form.form-no = b2-ef.form-no.
               END.
            END.
            ELSE DO:
               CREATE t-ef-form.
               ASSIGN 
                  t-ef-form.form-no = job-hdr.frm.
            END.

         /* for each temp form create a temp ref table for each blank that belongs to form */
         FOR EACH t-ef-form WHERE (t-ef-form.form-no = job-hdr.frm OR est.est-type <> 8),
            EACH b2-eb NO-LOCK WHERE b2-eb.company   = job-hdr.company
                                 AND b2-eb.est-no    = job-hdr.est-no 
                                 AND b2-eb.form-no   = t-ef-form.form-no
                                 AND (b2-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
                            BREAK BY b2-eb.form-no  
                                  BY b2-eb.blank-no:   
               IF FIRST-OF(b2-eb.blank-no) THEN DO:
                  IF b2-eb.form-no > 0 AND b2-eb.blank-no > 0 THEN DO:
                     FIND FIRST b-reftable WHERE b-reftable.reftable = "cecrep/d-hughes.w"
                                             AND b-reftable.company  = cocode
                                             AND b-reftable.loc      = TRIM(job-hdr.job-no)
                                             AND b-reftable.code     = STRING(job-hdr.job-no2,"9999999999")
                                             AND b-reftable.code2    = STRING(b2-eb.form-no,"999")
                                             AND b-reftable.val[1]   = b2-eb.blank-no NO-ERROR.
                     IF NOT AVAILABLE b-reftable THEN DO:
                        CREATE b-reftable.
                        ASSIGN
                           b-reftable.reftable = "cecrep/d-hughes.w"
                           b-reftable.company  = cocode
                           b-reftable.loc      = TRIM(job-hdr.job-no)
                           b-reftable.code     = STRING(job-hdr.job-no2,"9999999999")
                           b-reftable.code2    = STRING(b2-eb.form-no,"999")
                           b-reftable.val[1]   = b2-eb.blank-no.
                        END.
        
                        RUN cecrep/d-hughes.w(INPUT ROWID(job-hdr),
                                              INPUT ROWID(b-reftable), 
                                              INPUT b2-eb.stock-no).
                  END.
               END.
         END.
