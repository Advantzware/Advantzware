/* setvalue.i */

&IF "{&SETVALUE}" NE "no" &THEN
  &IF "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}" NE "" &THEN
  &Scoped-define TABLENAME {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
  &ELSE
  &Scoped-define TABLENAME {&FIRST-EXTERNAL-TABLE}
  &ENDIF

  &IF LOOKUP("{&TABLENAME}","{&NORECKEY}"," ") EQ 0 &THEN
  IF AVAILABLE {&TABLENAME} THEN
  DO:    
    FIND CURRENT {&TABLENAME} NO-LOCK.
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
      "({&TABLENAME}.rec_key,{methods/headers/{&TABLENAME}.i})"}
    {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
      "(CAN-FIND(FIRST notes WHERE notes.rec_key = {&TABLENAME}.rec_key))"}
    {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
      "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = {&TABLENAME}.rec_key))"}
  END.
  &ENDIF
&ENDIF

&IF "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}" NE "" &THEN
RUN Set-Focus.
&ENDIF

{methods/winReSizeSetValue.i}
