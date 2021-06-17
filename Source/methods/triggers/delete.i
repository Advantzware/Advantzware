/* delete.i */

{methods/audittrg.i}

IF CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH mfvalues EXCLUSIVE-LOCK
    WHERE mfvalues.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE mfvalues.
END. /* each mfvalues */

IF CAN-FIND(FIRST tag WHERE tag.linkRecKey EQ {&TABLENAME}.rec_key) THEN
FOR EACH tag EXCLUSIVE-LOCK WHERE
    tag.linkRecKey EQ {&TABLENAME}.rec_key
    :
    DELETE tag.
END. /* each tag */

IF CAN-FIND(FIRST notes WHERE notes.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH notes EXCLUSIVE-LOCK
    WHERE notes.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE notes.
END. /* each notes */
