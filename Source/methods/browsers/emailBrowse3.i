/* emailBrowse.i */
/* dgd 05/06/2007 - Fixed alignments for readability. */

CASE browse-order:
  WHEN 1 THEN
  OPEN  QUERY {&BROWSE-NAME} 
    FOR  EACH emaildtl OF emailcod        NO-LOCK,
         EACH phone                       NO-LOCK 
        WHERE phone.rec_key EQ emaildtl.table_rec_key,
        FIRST {&emailTable}               NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
          AND {&emailTable}.cust-no BEGINS auto_find
           BY {&emailTable}.cust-no.
  WHEN 2 THEN
  OPEN  QUERY {&BROWSE-NAME} 
    FOR  EACH emaildtl OF emailcod        NO-LOCK,
         EACH phone                       NO-LOCK 
        WHERE phone.rec_key EQ emaildtl.table_rec_key,
        FIRST {&emailTable}               NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
          AND {&emailTable}.ship-name BEGINS auto_find
           BY {&emailTable}.ship-name.
  WHEN 3 THEN
  OPEN  QUERY {&BROWSE-NAME} 
    FOR  EACH emaildtl OF emailcod        NO-LOCK,
         EACH phone                       NO-LOCK 
        WHERE phone.rec_key EQ emaildtl.table_rec_key
          AND phone.attention BEGINS auto_find,
        FIRST {&emailTable}               NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
           BY phone.attention.
  WHEN 4 THEN
  OPEN  QUERY {&BROWSE-NAME} 
     FOR EACH emaildtl OF emailcod        NO-LOCK,
         EACH phone                       NO-LOCK 
        WHERE phone.rec_key EQ emaildtl.table_rec_key
          AND phone.titlcode BEGINS auto_find,
        FIRST {&emailTable}               NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
           BY phone.titlcode.
END CASE.
