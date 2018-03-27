/* ar/j-cash.i */

CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ar-cash WHERE ar-cash.company = g_company 
                         AND NOT ar-cash.posted AND NOT ar-cash.memo
                         AND ar-cash.cust-no  BEGINS  auto_find NO-LOCK
                           BY ar-cash.cust-no  .
    END.
    when 2 THEN DO:
        OPEN QUERY {&browse-name}   FOR EACH ar-cash WHERE ar-cash.company = g_company 
                      AND ar-cash.posted AND NOT ar-cash.memo BY ar-cash.cust-no.
              
    END.   
    /*
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    */
END CASE.
