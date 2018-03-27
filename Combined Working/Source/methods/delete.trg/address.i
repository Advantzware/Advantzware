/* address.i */

IF CAN-FIND(FIRST phone WHERE phone.table_rec_key = {&TABLENAME}.rec_key) THEN
DELETE FROM phone WHERE phone.table_rec_key = {&TABLENAME}.rec_key.
