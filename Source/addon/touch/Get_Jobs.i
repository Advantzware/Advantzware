/* Get_Jobs.i - rstark - 3.20.2019 - used in addon/touch/jobs.w Procedure Get_Jobs */

    DO i = 1 TO 2:
        ASSIGN
            lv-stat[1] = lv-stat[2]
            lv-stat[2] = IF i EQ 1 THEN "C" ELSE "Z"
            .
        FOR EACH job NO-LOCK
            WHERE job.company EQ company_code
              AND job.stat    GT lv-stat[1]
              AND job.stat    LT lv-stat[2]  
            BREAK {&sortBy} BY job.job-no BY job.job-no2
            :         
            IF FIRST-OF(job.job-no2) AND CAN-FIND(FIRST job-hdr
                                                  WHERE job-hdr.company EQ job.company 
                                                    AND job-hdr.job     EQ job.job
                                                    AND job-hdr.job-no  EQ job.job-no
                                                    AND job-hdr.job-no2 EQ job.job-no2) THEN DO:
                FOR EACH job-mch NO-LOCK
                    WHERE job-mch.company EQ job.company
                      AND job-mch.job EQ job.job
                      AND job-mch.job-no EQ job.job-no
                      AND job-mch.job-no2 EQ job.job-no2
                      AND (job-mch.m-code EQ machine_code
                       OR LOOKUP(job-mch.m-code,machine_list) GT 0)
                      AND job-mch.run-complete EQ NO
                      AND (lTSShowPending EQ YES
                       OR (lTSShowPending EQ NO
                      AND job-mch.start-date NE ?))
                    BREAK BY job-mch.frm BY job-mch.blank-no
                    :
                    IF FIRST-OF(job-mch.blank-no) THEN DO:
                        lv-form-completed = YES.
                        FOR EACH bf-jobmch OF job NO-LOCK
                            WHERE (bf-jobmch.m-code EQ machine_code
                               OR LOOKUP(bf-jobmch.m-code,machine_list) GT 0)
                            :
                            lv-form-completed = IF NOT bf-jobmch.run-complete THEN NO ELSE lv-form-completed.
                        END. /* each bf-jobmch */
                        IF NOT CAN-FIND(FIRST cmpltjob
                                        WHERE cmpltjob.company      EQ company_code
                                          AND cmpltjob.machine      EQ machine_code
                                          AND cmpltjob.job_number   EQ job-mch.job-no
                                          AND cmpltjob.job_sub      EQ job-mch.job-no2
                                          AND cmpltjob.FORM_number  EQ job-mch.frm
                                          AND cmpltjob.blank_number EQ job-mch.blank-no)                    
                                          AND NOT lv-form-completed THEN DO:
                            IF NOT CAN-FIND(FIRST tt-job
                                            WHERE tt-job.job-no  EQ job.job-no
                                              AND tt-job.job-no2 EQ job.job-no2) THEN DO: 
                                CREATE tt-job.
                                ASSIGN 
                                    tt-job.job-no  = job.job-no
                                    tt-job.job-no2 = job.job-no2
                                    tt-job.sortKey = {&sortKey}
                                    .
                            END. /* if not can-find */
                        END. /* if not can-find */
                    END. /* if first-of(blank-no) */
                END. /* each job-mch */
            END. /* if first-of(job-no2) */
        END. /* each job */
    END. /* do i */
