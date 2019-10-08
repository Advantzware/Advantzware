/* util/del-note.p  Delete invalid note */

MESSAGE "Are you sure you want to delete Invalid Notes?"
    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN DO:
   SESSION:SET-WAIT-STATE("general").

   FOR EACH notes WHERE notes.rec_key = "".
       DELETE notes.
   END.

   SESSION:SET-WAIT-STATE("").

   MESSAGE "Completed. " VIEW-AS ALERT-BOX.
END.
