    
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company               EQ cocode
          AND (production OR job-hdr.ftick-prnt EQ reprint OR
              PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          AND FILL(" ",9 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"999")  GE fjob-no
          AND FILL(" ",9 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"999")  LE tjob-no
          AND job-hdr.job-no2 GE fjob-no2
          AND job-hdr.job-no2 LE tjob-no2
        USE-INDEX job-no,

        FIRST job
        WHERE job.company                   EQ cocode
          AND job.job                       EQ job-hdr.job
          AND job.job-no                    EQ job-hdr.job-no
          AND job.job-no2                   EQ job-hdr.job-no2
          AND job.stat                      NE "H"
          AND (job.pr-printed EQ reprint OR NOT production)
          AND (tb_app-unprinted EQ NO OR
               (tb_app-unprinted AND job.pr-printed = NO
                AND job.opened = YES AND job.cs-to-pr = YES))
        USE-INDEX job NO-LOCK,
        
        FIRST est
        WHERE est.company = job.company
          AND est.est-no                    EQ job.est-no
          AND est.est-type                  GT 4
        USE-INDEX est-no2 NO-LOCK,

        FIRST cust
        WHERE cust.company                  EQ cocode
          AND cust.cust-no                  EQ job-hdr.cust-no
        no-lock,

        FIRST itemfg
        WHERE itemfg.company                EQ cocode
          AND itemfg.i-no                   EQ job-hdr.i-no
        NO-LOCK,

        EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999")
          AND ( (reftable.code2    EQ job-hdr.i-no
                 AND reftable.val[12]  EQ job-hdr.frm
                 AND reftable.val[13]  EQ job-hdr.blank-no)  OR 
                     est.est-type = 6)
          
        BREAK BY job.job-no
              BY job.job-no2
              BY reftable.val[12]
              BY reftable.val[13]:
            
