/* names/j-name.i */

CASE browse-order :
     WHEN 4 THEN DO:
        OPEN QUERY {&browse-name}
            FOR EACH asi  NO-LOCK WHERE (
                asi.industry matches ("*" + auto_find + "*") OR auto_find = "")
              BY asi.industry
               .

    END.
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.
