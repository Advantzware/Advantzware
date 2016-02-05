/* gl/j-gljrnr.i */

CASE browse-order :
     WHEN 1 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH ASI.gl-jrn WHERE gl-jrn.company EQ g_company
                                    AND gl-jrn.journal GT 0 
                                    AND gl-jrn.posted  EQ NO
                                    AND gl-jrn.recur   EQ YES
                                    AND TRIM(STRING(gl-jrn.journal)) BEGINS auto_find
              NO-LOCK {&sortby-1}.
    END.
    WHEN 2 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH ASI.gl-jrn WHERE gl-jrn.company EQ g_company
                                    AND gl-jrn.journal GT 0 
                                    AND gl-jrn.posted  EQ NO
                                    AND gl-jrn.recur   EQ YES
                                    AND STRING(gl-jrn.freq) BEGINS auto_find
              NO-LOCK {&sortby-2}.
                  .
    END.
    
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.

