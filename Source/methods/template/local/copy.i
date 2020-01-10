/* copy.i */

&IF LOOKUP("{&FIRST-EXTERNAL-TABLE}","{custom/copy.i}"," ") NE 0 &THEN
copy-record = yes.
{methods/viewers/copy/{&FIRST-EXTERNAL-TABLE}.i}
&ENDIF
