/* gl/j-gljrn.i */

CASE browse-order :
     WHEN 1 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH ASI.gl-jrn WHERE gl-jrn.company = g_company 
                                    AND ASI.gl-jrn.journal > 0 
                                    AND gl-jrn.posted EQ NO
                                    AND gl-jrn.recur EQ NO
                                    AND TRIM(STRING(gl-jrn.journal)) BEGINS auto_find
              NO-LOCK BY gl-jrn.journal.
    END.
    WHEN 2 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH ASI.gl-jrn WHERE gl-jrn.company = g_company 
                                    AND ASI.gl-jrn.journal > 0 
                                    AND gl-jrn.posted EQ NO
                                    AND gl-jrn.recur EQ NO                                    AND STRING(gl-jrn.tr-date) BEGINS auto_find
                                  NO-LOCK BY gl-jrn.tr-date.
                  .
    END.
    
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.

