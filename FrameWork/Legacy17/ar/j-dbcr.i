/* ar/j-dbcr.i */

CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ar-cash WHERE ar-cash.company = g_company 
                         AND ASI.ar-cash.check-no >= 90000001
                         and ar-cash.check-no <= 99999999
                         AND NOT ar-cash.posted
                         AND ar-cash.cust-no  BEGINS  auto_find NO-LOCK {&SORTBY-PHRASE}
                           BY ar-cash.cust-no  .
    END.
    when 2 THEN DO:
        OPEN QUERY {&browse-name}   FOR EACH ar-cash WHERE ar-cash.company = g_company 
                             AND ASI.ar-cash.check-no >= 90000001
                             and ar-cash.check-no <= 99999999          
            AND ar-cash.posted {&SORTBY-PHRASE} BY ar-cash.cust-no.
              
    END.   
    /*
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    */
END CASE.
