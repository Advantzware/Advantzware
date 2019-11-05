/* ar/j-inv.i */

CASE browse-order :

    when 1 THEN DO: /* invoice#*/
       OPEN QUERY {&browse-name}  FOR EACH ASI.ar-inv WHERE ASI.ar-inv.company = g_company 
                                                        AND ar-inv.posted = TG_posted 
                                                        AND string(ar-inv.inv-no) BEGINS auto_find
                  NO-LOCK {&SORTBY-PHRASE}
                  BY ar-inv.inv-no MAX-ROWS 100 .
    END.
    when 2 THEN DO: /*cust-no*/ 
           OPEN QUERY {&browse-name}  FOR EACH ASI.ar-inv WHERE ASI.ar-inv.company = g_company 
                                                            AND ar-inv.posted = TG_posted 
                                                            AND ar-inv.cust-no BEGINS auto_find

                  NO-LOCK {&SORTBY-PHRASE}
                  BY ar-inv.cust-no MAX-ROWS ( if auto_find eq "" then 100 else 500) .
    END.
    when 3 THEN DO: /*cust-name*/ 
           OPEN QUERY {&browse-name}  FOR EACH ASI.ar-inv WHERE ASI.ar-inv.company = g_company 
                                                            AND ar-inv.posted = TG_posted 
                                                            AND ar-inv.cust-name BEGINS auto_find

                  NO-LOCK {&SORTBY-PHRASE}
                  BY ar-inv.cust-name MAX-ROWS ( if auto_find eq "" then 100 else 500) .
    END.
END CASE.
