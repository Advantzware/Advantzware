/* deleteAfter.i */

&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}",TRIM("{custom/deleteAfter.i}")) NE 0 &THEN
{methods/viewers/delete/{&FIRST-EXTERNAL-TABLE}After.i}
&ENDIF
