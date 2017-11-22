    IF KEYFUNCTION(LASTKEY) = 'HELP' THEN DO:
        if not retry then do:
            APPLY LASTKEY.
                /* runs applhelp.p */
            IF LIB_RECID_RET <> ? THEN READKEY pause 0.
                /* sets lastkey to -1, now a termkey.i trigger */
        end.
    END.
