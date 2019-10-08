/* deleteAfter.i */

&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/deleteAfter.i}") NE 0 &THEN
{methods/viewers/delete/{&FIRST-EXTERNAL-TABLE}After.i}
&ENDIF
