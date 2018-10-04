/* delete.i */

{methods/audittrg.i}

DELETE FROM rec_key WHERE rec_key.rec_key = {&TABLENAME}.rec_key.
IF CAN-FIND(FIRST notes WHERE notes.rec_key = {&TABLENAME}.rec_key) THEN
DELETE FROM notes WHERE notes.rec_key = {&TABLENAME}.rec_key.
IF CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = {&TABLENAME}.rec_key) THEN
DELETE FROM mfvalues WHERE mfvalues.rec_key = {&TABLENAME}.rec_key.


