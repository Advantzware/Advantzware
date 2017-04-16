/* rowavail.i */

&IF "{&DISPLAY-FIELD}" NE "" &THEN
DEFINE VARIABLE fieldcnt AS INTEGER NO-UNDO.

DO fieldcnt = 1 TO NUM-ENTRIES("{&DISPLAY-FIELD}"," "):
  RUN Display-Field (ENTRY(fieldcnt,"{&DISPLAY-FIELD}"," ")).
END.
&ENDIF

&IF INDEX("{custom/rowavail.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
{methods/viewers/rowavail/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
