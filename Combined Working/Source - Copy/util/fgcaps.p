/* util/fgcaps.p  Convert FG item's i-no to capital */

MESSAGE "Are you ready to capitalize all item#?"
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN DO:
   DISABLE TRIGGERS FOR LOAD OF itemfg.

   FOR EACH itemfg:
       itemfg.i-no = CAPS(itemfg.i-no).
   END.

END.

MESSAGE "Procedure is completed." VIEW-AS ALERT-BOX.

