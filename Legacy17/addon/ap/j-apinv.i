/* ap/j-apinv.i */

CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-inv WHERE ap-inv.company = g_company
                                       and not ap-inv.posted AND ap-inv.vend-no BEGINS auto_find
              NO-LOCK BY ap-inv.vend-no .   
    END.
    when 2 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-inv WHERE ap-inv.company = g_company
                                       and NOT ap-inv.posted AND ap-inv.inv-no BEGINS auto_find
                                       NO-LOCK BY ap-inv.inv-no    .  
              
    END.   
    when 3 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-inv WHERE ap-inv.company = g_company
                                       and ap-inv.posted NO-LOCK BY ap-inv.vend-no    .  
              
    END.   
    /*
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    */
   
END CASE.
