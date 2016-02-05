/* est.i */
{custom/checkuse.i}
&IF '{&proc-enable}' NE '' &THEN
   RUN {&proc-enable} .
&ENDIF
&IF '{&est-pack}' NE '' &THEN
  IF eb.form-no EQ 0 THEN DO:
    RUN dispatch ("cancel-record").
    RETURN "ADM-ERROR":U.
  END.
  ELSE RUN update-ink .  /* in cec/v-est3 for corrugated box */
&ENDIF
