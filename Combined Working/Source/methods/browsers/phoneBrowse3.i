/* phoneBrowse3.i */
/* dgd 05/16/2007 - Copied from methods\browsers\PhoneBrowse.i. */

CASE browse-order:
  WHEN 1 THEN
   OPEN QUERY {&BROWSE-NAME} 
     FOR EACH phone 
        WHERE phone.table_rec_key > '' NO-LOCK,
        FIRST {&emailTable} NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
          AND {&emailTable}.cust-no BEGINS auto_find
           BY {&emailTable}.cust-no.
  WHEN 2 THEN
  OPEN QUERY {&BROWSE-NAME} 
     FOR EACH phone 
        WHERE phone.table_rec_key > '' NO-LOCK,
        FIRST {&emailTable} NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
          AND {&emailTable}.sold-name BEGINS auto_find
           BY {&emailTable}.sold-name.
  WHEN 3 THEN
   OPEN QUERY {&BROWSE-NAME} 
     FOR EACH phone NO-LOCK 
        WHERE phone.table_rec_key > ''
          AND phone.attention BEGINS auto_find,
        FIRST {&emailTable} NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
           BY phone.attention.
  WHEN 4 THEN
   OPEN QUERY {&BROWSE-NAME} 
     FOR EACH phone NO-LOCK 
        WHERE phone.titlcode BEGINS auto_find,
        FIRST {&emailTable} NO-LOCK 
        WHERE {&emailTable}.rec_key EQ phone.table_rec_key
           BY phone.titlcode.
END CASE.
