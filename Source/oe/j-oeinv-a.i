/* oe/j-oeinv-a.i */

CASE browse-order :
     WHEN 1 THEN DO:
         OPEN QUERY {&browse-name} 
             FOR EACH ASI.inv-head WHERE inv-head.company = g_company
                                     AND inv-head.multi-invoice EQ NO 
                                     AND inv-head.inv-no >= int(auto_find) NO-LOCK
                                     {&sortby-phrase} BY inv-head.inv-no.
    END.
    WHEN 2 THEN DO:
         OPEN QUERY {&browse-name} 
             FOR EACH ASI.inv-head WHERE inv-head.company = g_company
                                     AND inv-head.multi-invoice EQ NO 
                                     AND inv-head.bol-no >= int(auto_find) NO-LOCK
                                     {&sortby-phrase} BY inv-head.bol-no.
                  
    END.
    WHEN 3 THEN DO:
             OPEN QUERY {&browse-name} 
                 FOR EACH ASI.inv-head WHERE inv-head.company = g_company
                                         AND inv-head.multi-invoice EQ NO 
                                         AND inv-head.cust-no BEGINS auto_find NO-LOCK
                                         {&sortby-phrase} BY inv-head.cust-no.

    END.
    WHEN 4 THEN DO:
                 OPEN QUERY {&browse-name} 
                     FOR EACH ASI.inv-head WHERE inv-head.company = g_company
                                             AND inv-head.multi-invoice EQ NO 
                                             AND inv-head.cust-name BEGINS auto_find NO-LOCK
                                             {&sortby-phrase} BY inv-head.cust-name.

    END.
    
END CASE.

