/* po/j-po-ord.i */

CASE browse-order:
    WHEN 1 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH po-ord WHERE po-ord.company = g_company AND STRING(po-ord.po-no) BEGINS auto_find NO-LOCK,
                  EACH po-ordl WHERE 
                       po-ordl.company EQ po-ord.company AND
                       po-ordl.po-no   EQ po-ord.po-no OUTER-JOIN NO-LOCK
                  BY po-ord.po-no DESC.
    END.
    WHEN 3 THEN DO:
             OPEN QUERY {&browse-name} 
                  FOR EACH po-ord WHERE po-ord.company = g_company  NO-LOCK,
                      EACH po-ordl WHERE
                           po-ordl.company EQ po-ord.company AND
                           po-ordl.po-no   EQ po-ord.po-no AND
                           STRING(po-ordl.i-no) BEGINS auto_find OUTER-JOIN
                      NO-LOCK
                      BY po-ordl.i-no.
    END.
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.
