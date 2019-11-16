/* enable.i */

&IF "{&F1}" NE "" &THEN
ENABLE {&F1} WITH FRAME {&FRAME-NAME}.
&ENDIF
{methods/run_link.i "RECORD-SOURCE" "Disable-Navigation"}

&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/enable.i}"," ") NE 0 &THEN 
{methods/viewers/enable/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF

&IF DEFINED(TIME-FIELDS) NE 0 &THEN
ASSIGN
    start_AMPM:HIDDEN = DYNAMIC-FUNCTION("sfCommon_HideAMPM")
    end_AMPM:HIDDEN   = DYNAMIC-FUNCTION("sfCommon_HideAMPM")
    .
&IF INDEX("{&TIME-FIELDS}","lunch_") NE 0 &THEN
ASSIGN
    lunch_start_AMPM:HIDDEN = DYNAMIC-FUNCTION("sfCommon_HideAMPM")
    lunch_end_AMPM:HIDDEN   = DYNAMIC-FUNCTION("sfCommon_HideAMPM")
    .
&ENDIF
&ENDIF
