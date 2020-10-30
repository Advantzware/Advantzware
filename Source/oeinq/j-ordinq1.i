/* j-ordinq1.i */

IF ll-sort-asc THEN DO:
  IF fi_cad-no NE '' THEN
  {&open-query-cad} {&sortby-phrase-asc}.
  ELSE
  {&open-query} {&sortby-phrase-asc}.
END. /* if ll-sort-asc */
ELSE DO:
  IF fi_cad-no NE '' THEN DO:
    IF lv-sort-by EQ "ord-date" THEN 
        {&open-query-cad} {&sortby-phrase-desc1}.
    ELSE
        {&open-query-cad} {&sortby-phrase-desc}.     
  END.
  ELSE DO:
      IF lv-sort-by EQ "ord-date" THEN 
        {&open-query} {&sortby-phrase-desc1}.
      ELSE  
        {&open-query} {&sortby-phrase-desc}.  
  END.
END. /* else */
