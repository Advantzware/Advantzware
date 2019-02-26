/* util/del001 */
/* delete inventory location '001' records that were created in error */

FIND FIRST loc WHERE loc.loc = '001' NO-LOCK NO-ERROR.

IF NOT AVAIL loc THEN DO:
    FOR EACH itemfg-loc WHERE itemfg-loc.loc EQ '001' EXCLUSIVE-LOCK.
        DELETE itemfg-loc.
    END.
END.

MESSAGE "Done!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
