/* emailBrowse4.i */

CASE browse-order:
  WHEN 1 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod NO-LOCK,
       EACH empAlert NO-LOCK WHERE empAlert.rec_key EQ emaildtl.table_rec_key,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empAlert.table_rec_key
       AND {&emailTable}.{&emailTable}-no BEGINS auto_find,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.USER_ID EQ empAlert.USER-ID
       BY {&emailTable}.{&emailTable}-no.
  WHEN 2 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod  NO-LOCK,
       EACH empAlert NO-LOCK WHERE empAlert.rec_key EQ emaildtl.table_rec_key,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empAlert.table_rec_key
       AND {&emailTable}.name BEGINS auto_find,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.USER_ID EQ empAlert.USER-ID
       BY {&emailTable}.name.
  WHEN 3 THEN
  DO:
  
  OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod  NO-LOCK,
       EACH empAlert NO-LOCK WHERE empAlert.rec_key EQ emaildtl.table_rec_key,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empAlert.table_rec_key,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.USER_ID EQ empAlert.USER-ID AND
             {&emailTable2}.user_id BEGINS auto_find
       BY {&emailTable2}.user_id.
  END.
  WHEN 4 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod NO-LOCK,
       EACH empAlert NO-LOCK WHERE empAlert.rec_key EQ emaildtl.table_rec_key,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ empAlert.table_rec_key,
       FIRST {&emailTable2} NO-LOCK WHERE {&emailTable2}.user_id EQ empAlert.USER-ID AND
             {&emailTable2}.user_name BEGINS auto_find
       BY {&emailTable2}.user_name.
END CASE.
