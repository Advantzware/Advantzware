/* jc/j-jobhdr.i */

CASE browse-order :
     WHEN 2 THEN DO:
        OPEN QUERY {&browse-name}
             FOR EACH ASI.job  NO-LOCK WHERE job.company = g_company AND string(job.start-date,"99999999") BEGINS (auto_find) ,
                 EACH ASI.job-hdr OF ASI.job NO-LOCK
                                 BY job.start-date  .

    END.
    when 3 THEN DO:
        OPEN QUERY {&browse-name}
             FOR EACH ASI.job  NO-LOCK WHERE  job.company = g_company ,
                 EACH ASI.job-hdr OF ASI.job
                                  WHERE job-hdr.i-no BEGINS auto_find NO-LOCK
                                 BY job-hdr.i-no  .

    END.
    when 4 THEN DO:
        OPEN QUERY {&browse-name} FOR EACH ASI.job  NO-LOCK WHERE  job.company = g_company ,
                                  EACH ASI.job-hdr OF ASI.job
                                  WHERE trim(job-hdr.est-no) BEGINS auto_find NO-LOCK
                                  BY job-hdr.est-no
                  .
    END.
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.
