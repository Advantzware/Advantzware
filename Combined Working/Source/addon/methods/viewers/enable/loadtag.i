/* loadtag.i */
DEF VAR lAdding AS LOGICAL NO-UNDO INIT NO.

/* Determine if adding record or not. */
RUN get-attribute('ADM-NEW-RECORD':U).
ASSIGN lAdding = (IF RETURN-VALUE = 'yes' THEN YES ELSE NO).

MESSAGE 'avail rfidtag? ' AVAILABLE rfidtag SKIP
        'asi? ' userid("nosweat") = "asi" SKIP
        'adding?' lAdding SKIP
        'enable field? ' AVAILABLE rfidtag AND userid("nosweat") = "asi" AND NOT lAdding
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Enable rfidtag when record is available and user is ASI and not adding record. */
IF AVAILABLE rfidtag AND userid("nosweat") = "asi" AND NOT lAdding THEN
    ENABLE v-rfidtag WITH FRAME {&FRAME-NAME}.
ELSE
    DISABLE v-rfidtag WITH FRAME {&FRAME-NAME}.

/* If adding new record, clear out current RFID tag. */
IF lAdding THEN
    ASSIGN v-rfidtag:SCREEN-VALUE = "".
