/* oe/j-quote.i */

CASE browse-order :
    WHEN 1 THEN DO:
       OPEN QUERY {&BROWSE-NAME} 
            FOR EACH quotehd 
                WHERE quotehd.company EQ g_company
                  AND quotehd.loc     EQ g_loc
                  AND quotehd.q-no    GE INT(auto_find)
                USE-INDEX q-no NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK
                BY quotehd.q-no DESC.
       IF auto_find NE '' THEN DO:
         GET LAST {&BROWSE-NAME}.
         IF AVAILABLE quotehd THEN DO:
           qRowID = ROWID(quotehd).
           REPOSITION {&BROWSE-NAME} TO ROWID qRowID NO-ERROR.
           APPLY 'VALUE-CHANGED':U TO {&BROWSE-NAME}.
         END.
       END.
    END.

    WHEN 2 THEN DO:
       OPEN QUERY {&BROWSE-NAME} 
            FOR EACH quotehd 
                WHERE quotehd.company EQ g_company
                  AND quotehd.loc     EQ g_loc
                  AND quotehd.cust-no BEGINS auto_find
                USE-INDEX cust-no NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK
                BY quotehd.cust-no
                BY quotehd.q-no DESC.
    END.

    WHEN 3 THEN DO:
      auto_date = DATE(auto_find) NO-ERROR.
      IF ERROR-STATUS:ERROR OR auto_date EQ ? THEN auto_date = 01/01/0001.

      OPEN QUERY {&BROWSE-NAME} 
            FOR EACH quotehd 
                WHERE quotehd.company  EQ g_company
                  AND quotehd.loc      EQ g_loc
                  AND quotehd.quo-date GE auto_date
                USE-INDEX qdate NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK
                BY quotehd.quo-date
                BY quotehd.q-no DESC.  
    END.

    WHEN 4 THEN DO:
      OPEN QUERY {&BROWSE-NAME} 
            FOR EACH quotehd 
                WHERE quotehd.company EQ g_company
                  AND quotehd.loc     EQ g_loc
                  AND quotehd.est-no  GE FILL(" ",8 - LENGTH(TRIM(auto_find))) +
                                         TRIM(auto_find)
                USE-INDEX quote NO-LOCK,
                EACH quoteitm OF quotehd NO-LOCK
                BY quotehd.est-no
                BY quotehd.q-no DESC.
    END.
    
    OTHERWISE DO:
      {&open-query-{&BROWSE-NAME}} 
    END.
    
END CASE.
