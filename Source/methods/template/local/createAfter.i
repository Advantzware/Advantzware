&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/createAfter.i}"," ") NE 0 &THEN
{methods/viewers/createAfter/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF