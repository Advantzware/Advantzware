/* emaildtl.i */

IF CAN-FIND(FIRST emaildtl WHERE emaildtl.table_rec_key EQ {&TABLENAME}.rec_key) THEN
DELETE FROM emaildtl WHERE emaildtl.table_rec_key = {&TABLENAME}.rec_key.
