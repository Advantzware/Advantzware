/* jc/j-machtran.i */

CASE browse-order :
     WHEN 1 THEN DO:
        OPEN QUERY {&browse-name}
            FOR EACH machtran NO-LOCK
                WHERE machtran.company EQ gcompany
                  AND machtran.posted EQ NO
                  AND machtran.machine BEGINS auto_find
                BY machtran.machine.
    END.
    WHEN 2 THEN DO:
        OPEN QUERY {&browse-name}
            FOR EACH machtran NO-LOCK
                WHERE machtran.company EQ gcompany
                  AND  machtran.posted EQ NO
                  AND (machtran.job_number BEGINS auto_find
                   OR  machtran.job_number MATCHES '*' + auto_find + '*')
                BY machtran.job_number.
    END.
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
END CASE.
