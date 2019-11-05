/* enable.i */

&IF "{&F1}" NE "" &THEN
ENABLE {&F1} WITH FRAME {&FRAME-NAME}.
&ENDIF
{methods/run_link.i "RECORD-SOURCE" "Disable-Navigation"}

&IF INDEX("{custom/enable.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
{methods/viewers/enable/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF

&IF DEFINED(TIME-FIELDS) NE 0 &THEN
ASSIGN
    start_AMPM:HIDDEN = DYNAMIC-FUNCTION("sfHideAMPM")
    end_AMPM:HIDDEN   = DYNAMIC-FUNCTION("sfHideAMPM")
    .
&IF INDEX("{&TIME-FIELDS}","lunch_") NE 0 &THEN
ASSIGN
    lunch_start_AMPM:HIDDEN = DYNAMIC-FUNCTION("sfHideAMPM")
    lunch_end_AMPM:HIDDEN   = DYNAMIC-FUNCTION("sfHideAMPM")
    .
&ENDIF
&ENDIF
