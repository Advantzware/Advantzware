/* sys/util/del-data.p  Delete invalid data */

MESSAGE "Are you sure you want to delete Invalid data?"
    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN DO:
   SESSION:SET-WAIT-STATE("general").

   RUN util/delshpto.p.
   RUN util/del-est.p.
   RUN util/updefcal.p .

   SESSION:SET-WAIT-STATE("").

   MESSAGE "Completed. " VIEW-AS ALERT-BOX.
END.
