/* delete.i */

{methods/audittrg.i}

FOR EACH rec_key EXCLUSIVE-LOCK
    WHERE rec_key.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE rec_key.
END. /* each rec_key */

IF CAN-FIND(FIRST notes WHERE notes.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH notes EXCLUSIVE-LOCK
    WHERE notes.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE notes.
END. /* each notes */

IF CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH mfvalues EXCLUSIVE-LOCK
    WHERE mfvalues.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE mfvalues.
END. /* each mfvalues */
