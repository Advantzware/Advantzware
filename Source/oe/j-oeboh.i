/* oe/j-oeboh.i */

CASE browse-order :
     WHEN 1 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.company = g_company AND ASI.oe-bolh.posted = no and oe-bolh.deleted = no,
                  each oe-boll WHERE oe-boll.company EQ oe-bolh.company
                                 AND oe-boll.b-no EQ oe-bolh.b-no
                                 AND string(oe-bolh.bol-no) BEGINS auto_find
                  NO-LOCK               BY oe-bolh.bol-no
                  .
    END.
    WHEN 2 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH oe-bolh NO-LOCK  WHERE oe-bolh.company = g_company AND ASI.oe-bolh.posted = no and oe-bolh.deleted = no,
                  each oe-boll WHERE oe-boll.company EQ oe-bolh.company
                                 AND oe-boll.b-no EQ oe-bolh.b-no
                                 AND STRING(oe-boll.ord-no) BEGINS auto_find 
                  NO-LOCK               BY oe-boll.ord-no
                  .
    END.
    
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.

