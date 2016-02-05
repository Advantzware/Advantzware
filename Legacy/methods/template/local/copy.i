/* copy.i */

&IF INDEX("{custom/copy.i}","{&FIRST-EXTERNAL-TABLE}") NE 0 &THEN
copy-record = yes.
{methods/viewers/copy/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
