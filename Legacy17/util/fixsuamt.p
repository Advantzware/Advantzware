/* util/fixsuamt.p  */
MESSAGE "Are you sure you want to update surcharge amount?" VIEW-AS ALERT-BOX
    QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:
OUTPUT TO value("c:\tmp\surchage." + string(time)).

FOR EACH surcharge.
    EXPORT surcharge.
    surcharge.amt = surcharge.amt * 1000.
END.
OUTPUT CLOSE.

MESSAGE "Completed." VIEW-AS ALERT-BOX.

END.
