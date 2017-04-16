/* oe/j-ord.i */

CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH oe-ord
                  WHERE oe-ord.company                           EQ g_company
                    AND TRIM(STRING(oe-ord.ord-no,">>>>>>>>>>")) BEGINS auto_find
                  NO-LOCK,
                  EACH oe-ordl OF oe-ord OUTER-JOIN  
                  NO-LOCK               BY oe-ord.ord-no DESC
                  .
    END.
    when 2 THEN DO:
        OPEN QUERY {&browse-name} 
              FOR EACH oe-ord NO-LOCK  WHERE oe-ord.company = g_company AND string(oe-ord.ord-date,"99/99/9999") BEGINS auto_find,
                  each oe-ordl OF oe-ord OUTER-JOIN
                  NO-LOCK               BY oe-ord.ord-date
                  .
    END.
    when 3 THEN DO:
        OPEN QUERY {&browse-name} 
              FOR EACH oe-ord NO-LOCK  WHERE oe-ord.company = g_company AND oe-ord.cust-no BEGINS auto_find,
                  each oe-ordl OF oe-ord  OUTER-JOIN
                  NO-LOCK               BY oe-ord.cust-no
                  .
    END.
    when 4 THEN DO:
        OPEN QUERY {&browse-name} 
              FOR EACH oe-ord NO-LOCK WHERE oe-ord.company = g_company,
                  each oe-ordl OF oe-ord OUTER-JOIN WHERE TRIM(oe-ordl.est-no) BEGINS auto_find 
                  NO-LOCK               BY oe-ordl.est-no
                  .
    END.
    when 5 THEN DO:
        OPEN QUERY {&browse-name} 
              FOR EACH oe-ord NO-LOCK  WHERE oe-ord.company = g_company,
                  each oe-ordl OF oe-ord OUTER-JOIN WHERE TRIM(oe-ordl.job-no) BEGINS auto_find 
                  NO-LOCK               BY oe-ordl.job-no
                  .
    END.
    when 6 THEN DO:
        OPEN QUERY {&browse-name} 
              FOR EACH oe-ord NO-LOCK  WHERE oe-ord.company = g_company,
                  each oe-ordl OF oe-ord OUTER-JOIN WHERE oe-ordl.i-no BEGINS auto_find 
                  NO-LOCK               BY oe-ordl.i-no
                  .
    END.
    when 7 THEN DO:
        OPEN QUERY {&browse-name} 
              FOR EACH oe-ord NO-LOCK  WHERE oe-ord.company = g_company AND oe-ord.stat BEGINS auto_find,
                  each oe-ordl OF oe-ord  OUTER-JOIN
                  NO-LOCK               BY LOOKUP(oe-ord.stat,"A,H") DESC
                                        BY oe-ord.ord-no             DESC
                  .
    END.
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.
