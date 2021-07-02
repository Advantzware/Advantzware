/* delete.i */

{methods/audittrg.i}

IF CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH mfvalues EXCLUSIVE-LOCK
    WHERE mfvalues.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE mfvalues.
END. /* each mfvalues */

DEFINE BUFFER bf-tag FOR tag.
/* IF CAN-FIND(FIRST tag WHERE tag.linkRecKey EQ {&TABLENAME}.rec_key) THEN */
FOR EACH bf-tag EXCLUSIVE-LOCK WHERE
    bf-tag.linkRecKey EQ {&TABLENAME}.rec_key
    :
    DELETE bf-tag.
END. /* each tag */

IF CAN-FIND(FIRST notes WHERE notes.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH notes EXCLUSIVE-LOCK
    WHERE notes.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE notes.
END. /* each notes */
