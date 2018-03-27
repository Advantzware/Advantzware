/* addsttgs.i */

&IF "{&ADDFLD-{1}}" NE "" &THEN
  &IF "{&ADD-DEFAULT-{1}}" = "yes" &THEN
  &Scoped-define SELF-NAME {&ADDFLD-{1}}
  {methods/listobjs/attribs.i}
  &ENDIF
&ENDIF
