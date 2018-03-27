/* j-ordinq1.i */

IF ll-sort-asc THEN DO:
  IF fi_cad-no NE '' THEN
  {&open-query-cad} {&sortby-phrase-asc}.
  ELSE
  {&open-query} {&sortby-phrase-asc}.
END. /* if ll-sort-asc */
ELSE DO:
  IF fi_cad-no NE '' THEN
  {&open-query-cad} {&sortby-phrase-desc}.
  ELSE
  {&open-query} {&sortby-phrase-desc}.
END. /* else */
