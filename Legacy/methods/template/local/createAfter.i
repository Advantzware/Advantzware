&IF INDEX("{custom/createAfter.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
{methods/viewers/createAfter/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF