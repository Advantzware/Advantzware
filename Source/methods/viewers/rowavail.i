/* rowavail.i */

&IF "{&DISPLAY-FIELD}" NE "" &THEN
DEFINE VARIABLE fieldcnt AS INTEGER NO-UNDO.

DO fieldcnt = 1 TO NUM-ENTRIES("{&DISPLAY-FIELD}"," "):
    RUN Display-Field (ENTRY(fieldcnt,"{&DISPLAY-FIELD}"," ")).
END.
&ENDIF

&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/rowavail.i}"," ") NE 0 &THEN 
{methods/viewers/rowavail/{&FIRST-EXTERNAL-TABLE}.i}
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
