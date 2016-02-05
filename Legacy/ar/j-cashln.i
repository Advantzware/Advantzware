/* ar/j-cashln.i */

CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ar-cash WHERE ar-cash.company = g_company 
                         AND ar-cash.posted
                         AND ar-cash.cust-no  BEGINS  auto_find NO-LOCK,
                         FIRST ASI.ar-cashl OF ASI.ar-cash WHERE ar-cashl.on-account NO-LOCK
                           BY ar-cash.cust-no  .
    END.
    when 2 THEN DO:
        OPEN QUERY {&browse-name}   FOR EACH ar-cash WHERE ar-cash.company = g_company 
                      AND ar-cash.posted,
            FIRST ASI.ar-cashl OF ASI.ar-cash NO-LOCK
             BY ar-cash.cust-no.
              
    END.   
    /*
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    */
   
END CASE.
