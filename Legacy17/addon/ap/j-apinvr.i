/* ap/j-apinv.i */

CASE browse-order :
    WHEN 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-inv
                                        WHERE ap-inv.company EQ cocode
                                          AND ap-inv.posted  EQ NO
                                          AND ap-inv.recur   EQ YES
                                          AND ap-inv.vend-no BEGINS auto_find
                                        NO-LOCK
                                        BY ap-inv.vend-no.
    END.
    WHEN 2 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-inv
                                        WHERE ap-inv.company EQ cocode
                                          AND ap-inv.posted  EQ NO
                                          AND ap-inv.recur   EQ YES
                                          AND ap-inv.inv-no  BEGINS auto_find
                                        NO-LOCK
                                        BY ap-inv.inv-no.
    END.   
    WHEN 3 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-inv
                                        WHERE ap-inv.company EQ cocode
                                          AND ap-inv.posted  EQ NO
                                          AND ap-inv.recur   EQ YES
                                        NO-LOCK
                                        BY ap-inv.vend-no.
    END.
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.
