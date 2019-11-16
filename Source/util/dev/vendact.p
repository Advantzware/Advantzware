/* util/vendact.p */

MESSAGE "Are you sure you want to update blank GL account description?" VIEW-AS ALERT-BOX
    QUESTION BUTTON YES-NO UPDATE v-ans AS LOG .
IF v-ans THEN DO:

FOR EACH vend:
    FIND FIRST account WHERE account.company = vend.company
                            AND account.actnum = vend.actnum NO-LOCK NO-ERROR.
    IF AVAIL account AND vend.actdscr = ""
        THEN vend.actdscr = account.dscr.

END.

    MESSAGE "Update completed..." VIEW-AS ALERT-BOX.
END.
