/* enable.i */

&IF "{&F1}" NE "" &THEN
ENABLE {&F1} WITH FRAME {&FRAME-NAME}.
DISPLAY {&F1} WITH FRAME {&FRAME-NAME}.
&ENDIF
{methods/run_link.i "RECORD-SOURCE" "Disable-Navigation"}

&IF INDEX("{custom/enable.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
{methods/viewers/enable/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
