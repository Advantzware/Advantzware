/* ar/j-inv-d.i */

CASE browse-order :

    when 1 THEN DO: /* invoice#*/
       OPEN QUERY {&browse-name}  FOR EACH ASI.ar-inv WHERE ASI.ar-inv.company = g_company 
                                                        AND ar-inv.posted = TG_posted 
                                                        AND string(ar-inv.inv-no) BEGINS auto_find
                  NO-LOCK {&SORTBY-PHRASE} DESC
                  BY ar-inv.inv-no.
    END.
    when 2 THEN DO: /*cust-no*/ 
           OPEN QUERY {&browse-name}  FOR EACH ASI.ar-inv WHERE ASI.ar-inv.company = g_company 
                                                            AND ar-inv.posted = TG_posted 
                                                            AND ar-inv.cust-no BEGINS auto_find

                  NO-LOCK {&SORTBY-PHRASE} DESC
                  BY ar-inv.cust-no.
    END.
END CASE.
