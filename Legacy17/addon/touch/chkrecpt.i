/*addon/touch/chkrecpt.i} */

FOR EACH job-hdr NO-LOCK WHERE job-hdr.company = company_code
                           AND job-hdr.job-no = job_number
                           AND job-hdr.job-no2 = INT(job_sub),
    EACH itemfg NO-LOCK WHERE itemfg.company = job-hdr.company
                          AND itemfg.i-no = job-hdr.i-no:
    
    IF itemfg.isaset THEN 
    FOR EACH ASI.fg-set NO-LOCK WHERE fg-set.company = itemfg.company
                                  AND fg-set.set-no = itemfg.i-no :
       CREATE tt-comp.
       ASSIGN tt-comp.i-no = itemfg.i-no.
    END.
END.
FOR EACH tt-comp,
    EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company = company_code
                            AND fg-rcpth.i-no = tt-comp.i-no
                            AND fg-rcpth.job-no = job_number
                            AND fg-rcpth.job-no2 = int(job_sub)
                            AND fg-rcpth.rita-code = "R",
    EACH ASI.fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code :

END.
