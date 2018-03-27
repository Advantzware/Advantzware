/* emailcod.i */

IF CAN-FIND(FIRST emaildtl WHERE emaildtl.emailcod EQ {&TABLENAME}.emailcod) THEN
DELETE FROM emaildtl WHERE emaildtl.emailcod = {&TABLENAME}.emailcod.
