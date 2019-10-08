for each job-hdr NO-LOCK
        where job-hdr.company               eq cocode
          and (production OR job-hdr.ftick-prnt eq reprint OR
              PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
        USE-INDEX job-no,

        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
        USE-INDEX job NO-LOCK,
        
        first est
        where est.company = job.company
          AND est.est-no                    eq job.est-no
          and est.est-type                  gt 4
        USE-INDEX est-no2 NO-LOCK,
                                      
        EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999")
          AND ( (reftable.code2    EQ job-hdr.i-no
                 AND reftable.val[12]  EQ job-hdr.frm
                 AND reftable.val[13]  EQ job-hdr.blank-no)  OR 
                     est.est-type = 6)
          
        break by job.job-no
              by job.job-no2
              BY reftable.val[12]
              BY reftable.val[13]:
   
