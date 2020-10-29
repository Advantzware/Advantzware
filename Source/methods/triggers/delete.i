/* delete.i */

{methods/audittrg.i}

/*IF CAN-FIND(FIRST rec_key WHERE rec_key.rec_key EQ {&TABLENAME}.rec_key) THEN*/
/*FOR EACH rec_key EXCLUSIVE-LOCK                                              */
/*    WHERE rec_key.rec_key EQ {&TABLENAME}.rec_key                            */
/*    :                                                                        */
/*    DELETE rec_key.                                                          */
/*END. /* each rec_key */                                                      */

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

&IF LOOKUP("{&TABLENAME}","pc-prdd,pc-prdh") NE 0 &THEN
IF CAN-FIND(FIRST job
            WHERE job.rec_key EQ {&TABLENAME}.rec_key) THEN
RETURN.
&ENDIF

IF CAN-FIND(FIRST notes WHERE notes.rec_key EQ {&TABLENAME}.rec_key) THEN
FOR EACH notes EXCLUSIVE-LOCK
    WHERE notes.rec_key EQ {&TABLENAME}.rec_key
    :
    DELETE notes.
END. /* each notes */
