/* winView.i */

&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/winView.i}"," ") NE 0 &THEN
{methods/windows/winView/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
