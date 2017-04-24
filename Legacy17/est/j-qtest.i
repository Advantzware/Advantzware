/* oe/j-qtest.i */

CASE browse-order :
    WHEN 1 THEN DO:
       OPEN QUERY {&browse-name} 
            FOR EACH quotehd OF est
                WHERE quotehd.company EQ g_company
                  AND TRIM(STRING(quotehd.q-no,">>>>>>>>>>")) BEGINS auto_find
                NO-LOCK,
                EACH cust OF quotehd NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK,
                EACH quoteqty OF quoteitm NO-LOCK
                BY quotehd.q-no DESC.
    END.

    WHEN 3 THEN DO:
      OPEN QUERY {&browse-name} 
            FOR EACH quotehd OF est
                WHERE quotehd.company EQ g_company
                  AND STRING(quotehd.quo-date,"99/99/9999") BEGINS auto_find
                NO-LOCK,
                EACH cust OF quotehd NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK,
                EACH quoteqty OF quoteitm NO-LOCK
                BY quotehd.quo-date.  
    END.

    WHEN 4 THEN DO:
      OPEN QUERY {&browse-name} 
            FOR EACH quotehd OF est
                WHERE quotehd.company EQ g_company
                  AND /*TRIM(STRING(quotehd.est-no,">>>>>>>>>>")) BEGINS auto_find*/
                      INT(quotehd.est-no) >= INT(auto_find) 
                NO-LOCK,
                EACH cust OF quotehd NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK,
                EACH quoteqty OF quoteitm NO-LOCK
                BY quotehd.est-no.
    END.
    
    OTHERWISE DO:
      {&open-query-{&browse-name}} 
    END.
    
END CASE.
