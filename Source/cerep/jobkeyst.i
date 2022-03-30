    
    
    FOR EACH job-hdr
        WHERE job-hdr.company               EQ cocode
          AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"999")  GE fjob-no
          AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"999")  LE tjob-no
          AND job-hdr.job-no2 GE fjob-no2
          AND job-hdr.job-no2 LE tjob-no2
          AND (production OR
               job-hdr.ftick-prnt           EQ reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          AND CAN-FIND(FIRST job WHERE job.company EQ cocode
                                   AND job.job     EQ job-hdr.job
                                   AND job.job-no  EQ job-hdr.job-no
                                   AND job.job-no2 EQ job-hdr.job-no2
                                   AND job.stat    NE "H"
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production)
                                   AND (tb_app-unprinted EQ NO OR
                                        (tb_app-unprinted AND job.pr-printed = NO AND
                                         job.opened = YES AND job.cs-to-pr = YES)))
        USE-INDEX job-no {1},

        FIRST est
        WHERE est.company  EQ job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          AND est.est-type LE 4  
        NO-LOCK
