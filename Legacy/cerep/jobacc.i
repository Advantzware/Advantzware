    
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company               EQ cocode
          AND job-hdr.job-no                GE SUBSTR(fjob-no,1,6)
          AND job-hdr.job-no                LE SUBSTR(tjob-no,1,6)
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  GE fjob-no
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  LE tjob-no
          and (production OR
               job-hdr.ftick-prnt           eq v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H"
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production))
        USE-INDEX job-no,

        FIRST est NO-LOCK
        WHERE est.company  EQ job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          AND est.est-type LE 4  
