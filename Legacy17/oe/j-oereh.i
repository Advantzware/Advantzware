/* oe/j-oereh.i */

CASE browse-order :
    WHEN 1 THEN DO:
       IF auto_find EQ "" THEN
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND TRIM(STRING(oe-rell.ord-no,">>>>>>>>>>")) BEGINS auto_find
                  OUTER-JOIN USE-INDEX r-no NO-LOCK
                  BY oe-rell.ord-no BY oe-relh.release# BY oe-rell.i-no
                  .
       ELSE
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND TRIM(STRING(oe-rell.ord-no,">>>>>>>>>>")) BEGINS auto_find
                  USE-INDEX r-no NO-LOCK
                  BY oe-rell.ord-no BY oe-relh.release# BY oe-rell.i-no
                  .
    END.
    WHEN 2 THEN DO:
       IF auto_find EQ "" THEN
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND oe-rell.po-no   BEGINS auto_find 
                  OUTER-JOIN USE-INDEX r-no NO-LOCK
                  BY oe-rell.po-no BY oe-relh.release# BY oe-rell.i-no
                  .
       ELSE
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND oe-rell.po-no   BEGINS auto_find 
                  USE-INDEX r-no NO-LOCK
                  BY oe-rell.po-no BY oe-relh.release# BY oe-rell.i-no
                  .
    END.
    WHEN 3 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                    AND TRIM(STRING(oe-relh.release#,">>>>>>>>>>")) BEGINS auto_find
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                  OUTER-JOIN USE-INDEX r-no NO-LOCK
                  BY oe-relh.release# BY oe-rell.i-no
                  .
    END.
    WHEN 4 THEN DO:
       IF auto_find EQ "" THEN
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND TRIM(oe-rell.job-no) BEGINS auto_find 
                  OUTER-JOIN USE-INDEX r-no NO-LOCK
                  BY oe-rell.job-no BY oe-relh.release# BY oe-rell.i-no
                  .
       ELSE
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND TRIM(oe-rell.job-no) BEGINS auto_find 
                  USE-INDEX r-no NO-LOCK
                  BY oe-rell.job-no BY oe-relh.release# BY oe-rell.i-no
                  .
    END.
    WHEN 5 THEN DO:
       IF auto_find EQ "" THEN
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND oe-rell.i-no    BEGINS auto_find 
                  OUTER-JOIN USE-INDEX r-no NO-LOCK 
                  BY oe-rell.i-no BY oe-relh.release# BY oe-rell.i-no
                  .
       ELSE
         OPEN QUERY {&browse-name} 
              FOR EACH oe-relh
                  WHERE {&where}
                  USE-INDEX delpost NO-LOCK,
                  EACH oe-rell
                  WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND oe-rell.i-no    BEGINS auto_find 
                  USE-INDEX r-no NO-LOCK 
                  BY oe-rell.i-no BY oe-relh.release# BY oe-rell.i-no
                  .
    END.
    
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.
