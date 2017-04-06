/* rfq/j-i */
case browse-order:
    WHEN 1 THEN DO:
       OPEN QUERY {&browse-name}   
         for EACH rfq WHERE company = g_company and 
                            loc = g_loc and rfq-no >= int(auto_find) NO-LOCK,
             EACH rfqitem OF rfq WHERE rfqitem.seq < 999
                  outer-join NO-LOCK by rfq-no DESC.

    END.
    WHEN 2 THEN DO:
       OPEN QUERY {&browse-name}   
         for EACH rfq WHERE company = g_company and 
                            loc = g_loc and cust-no begins auto_find NO-LOCK,
             EACH rfqitem OF rfq WHERE rfqitem.seq < 999
                  outer-join NO-LOCK by cust-no DESC.

    END.
    WHEN 3 THEN DO:
       OPEN QUERY {&browse-name}   
         for EACH rfq WHERE company = g_company and 
                            loc = g_loc NO-LOCK,
             EACH rfqitem OF rfq where trim(rfqitem.est-no) begins auto_find
                     and rfqitem.seq < 999
                  NO-LOCK by rfqitem.est-no DESC.

    END.

end case.

