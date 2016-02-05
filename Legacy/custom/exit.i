/* exit.i */

/* SYSTEM-HELP "<HelpFileName.hlp>" QUIT. */

&IF "{&asi-exit}" <> "" &THEN
   
   IF "{&asi-exit}" NE "local-exit2" THEN
   DO:
      RUN {&asi-exit} NO-ERROR .
      IF error-status:ERROR THEN RETURN.
   END.
   ELSE
   DO:
       DEF VAR op-delete-choice AS LOG NO-UNDO.
       RUN local-exit2(OUTPUT op-delete-choice).
       IF op-delete-choice EQ NO THEN RETURN.
   END.
&ENDIF
