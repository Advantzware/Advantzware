/* winView.i */

&IF INDEX("{custom/winView.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
{methods/windows/winView/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
