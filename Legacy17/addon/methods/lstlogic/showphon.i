/* showphon.i */

/*
&db_table = DATABASE.table-name
&col = column where phone frame begins
&frame-name = frame name for phone

NOTE: this include uses the logical variable 'show-phones'
*/

IF show-phones THEN
DO:
  IF CAN-FIND(FIRST phone
      WHERE phone.table_rec_key = {&db_table}.rec_key) THEN
  PUT UNFORMATTED "** PHONE ********************************" AT {&col} SKIP.
  FOR EACH phone
      WHERE phone.table_rec_key = {&db_table}.rec_key NO-LOCK
      WITH FRAME {&frame-name} WIDTH 132 COLUMN {&col} NO-BOX STREAM-IO:
    DISPLAY
      phone.titlcode COLUMN-LABEL "Title"
      phone.attention
      phone.phone_ctry_code
      phone.phone_city_code
      phone.phone
      phone.phone_ext
      phone.fax_ctry_code
      phone.fax_city_code
      phone.fax
      phone.e_mail.
  END.
END.
