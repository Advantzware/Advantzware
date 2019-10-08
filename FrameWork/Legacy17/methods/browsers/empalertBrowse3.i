/* empalertBrowse3.i */

CASE browse-order:
  WHEN 1 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH empalert NO-LOCK,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empalert.table_rec_key
       AND {&emailTable}.{&emailTable}-no BEGINS auto_find,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.user_id EQ empalert.user-id
       BY {&emailTable}.{&emailTable}-no.
  WHEN 2 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH empalert NO-LOCK,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empalert.table_rec_key
       AND {&emailTable}.name BEGINS auto_find,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.user_id EQ empalert.user-id
       BY {&emailTable}.name.
  WHEN 3 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH empalert NO-LOCK,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empalert.table_rec_key,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.user_id BEGINS auto_find AND
             {&emailTable2}.user_id EQ empalert.USER-ID
       BY empalert.user-id.
  WHEN 4 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH empalert NO-LOCK,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empalert.table_rec_key,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.USER_id EQ empalert.USER-ID AND
             {&emailTable2}.user_name BEGINS auto_find
       BY {&emailTable2}.user_name.
END CASE.
