/* del-bcst.p   delete blank customer  */
/*
MESSAGE "Are you sure you want to delete invalid estimates" 
      VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN DO:
*/

SESSION:SET-WAIT-STATE("general").
PAUSE 0.
DISABLE TRIGGERS FOR LOAD OF cust.

FOR EACH cust WHERE cust.cust-no = "".
    DELETE cust.
END.
SESSION:SET-WAIT-STATE("").

