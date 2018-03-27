/* sys/util/delshpto.p blank customer and delete orphant shipto sold to record */
/*
MESSAGE "Are you sure you want to delete invalid Customer, Ship to and Sold to records?"
    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:
*/
DISABLE TRIGGERS FOR LOAD OF cust.

SESSION:SET-WAIT-STATE("general").

FOR EACH cust WHERE cust-no = "":
    DELETE cust.
END.

FOR EACH shipto :
    FIND FIRST cust WHERE cust.company = shipto.company 
                      AND cust.cust-no = shipto.cust-no NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DELETE shipto.
END.

FOR EACH soldto :
    FIND FIRST cust WHERE cust.company = soldto.company 
                      AND cust.cust-no = soldto.cust-no NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DELETE soldto.
END.
SESSION:SET-WAIT-STATE("").


