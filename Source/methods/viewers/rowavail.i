/* rowavail.i */

&IF "{&DISPLAY-FIELD}" NE "" &THEN
DEFINE VARIABLE fieldcnt AS INTEGER NO-UNDO.
DO fieldcnt = 1 TO NUM-ENTRIES("{&DISPLAY-FIELD}"," "):
  RUN Display-Field (ENTRY(fieldcnt,"{&DISPLAY-FIELD}"," ")).
END.
&ENDIF
 /*  YSK  not working for tables begins string here.   ex) mach because of machsft*/
/*&IF INDEX("{custom/rowavail.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN   */
&IF lookup("{&FIRST-EXTERNAL-TABLE}","{custom/rowavail.i}"," ") NE 0 &THEN 
{methods/viewers/rowavail/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
