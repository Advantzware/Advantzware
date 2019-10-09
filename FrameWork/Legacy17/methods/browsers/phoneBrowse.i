/* phoneBrowse.i */

CASE browse-order:
  WHEN 1 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH phone NO-LOCK,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key
       AND {&emailTable}.{&emailTable}-no BEGINS auto_find
       BY {&emailTable}.{&emailTable}-no.
  WHEN 2 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH phone NO-LOCK,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key
       AND {&emailTable}.name BEGINS auto_find
       BY {&emailTable}.name.
  WHEN 3 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH phone NO-LOCK WHERE phone.attention BEGINS auto_find,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key
       BY phone.attention.
  WHEN 4 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH phone NO-LOCK WHERE phone.titlcode BEGINS auto_find,
       FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key
       BY phone.titlcode.
END CASE.
/*
CASE browse-order:
  WHEN 1 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH {&emailTable} NO-LOCK
       WHERE {&emailTable}.{&emailTable}-no BEGINS auto_find,
       EACH phone NO-LOCK WHERE phone.table_rec_key EQ {&emailTable}.rec_key
       BY {&emailTable}.{&emailTable}-no.
  WHEN 2 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH {&emailTable} NO-LOCK
       WHERE {&emailTable}.name BEGINS auto_find,
       EACH phone NO-LOCK WHERE phone.table_rec_key EQ {&emailTable}.rec_key
       BY {&emailTable}.name.
  WHEN 3 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH {&emailTable} NO-LOCK,
       EACH phone NO-LOCK WHERE phone.table_rec_key EQ {&emailTable}.rec_key
       AND phone.attention BEGINS auto_find
       BY phone.attention.
  WHEN 4 THEN
  OPEN QUERY {&BROWSE-NAME} FOR EACH {&emailTable} NO-LOCK,
       EACH phone NO-LOCK WHERE phone.table_rec_key EQ {&emailTable}.rec_key
       AND phone.titlcode BEGINS auto_find
       BY phone.titlcode.
END CASE.
*/
