/* util/updstate.p */

    SESSION:SET-WAIT-STATE("general").

FOR EACH statecod NO-LOCK:
    FIND FIRST state WHERE state.state = statecod.statecod NO-ERROR.
    IF NOT AVAIL state THEN DO:
       CREATE state.
       ASSIGN state.state = statecod.statecod
            state.NAME = statecod.DESCRIPTION.
    END.
END.

SESSION:SET-WAIT-STATE("").
MESSAGE "Update completed." VIEW-AS ALERT-BOX.
