/* settings.i */

&IF "{&{1}FLD{2}}" NE "" AND "{&DBFIELD{2}}" NE "" &THEN
&Global-define {1}VALUE{2} {&{1}FLD{2}} = {&DBFIELD{2}}
  &IF "{&{1}-DEFAULT-{2}}" = "yes" &THEN
  &Scoped-define SELF-NAME {&{1}FLD{2}}
  {methods/listobjs/defaults.i}
  &ENDIF
&ENDIF
