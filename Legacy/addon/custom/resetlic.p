/* addon/custom/resetlic.p */


MESSAGE "Are you sure you want to reset liscense for addon?" VIEW-AS ALERT-BOX
   WARNING BUTTON YES-NO UPDATE v-ans AS LOG.

IF v-ans THEN DO:
   FOR EACH prgrms:
       WIDGET_font[13] = ?.
   END.
END.
