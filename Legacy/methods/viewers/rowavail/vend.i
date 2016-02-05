/* vend.i */

&IF '{&VENDOR-TOTALS}' NE '' &THEN
{custom/getperd.i}
IF gperiod NE 0 THEN
ASSIGN
  ptd-purch:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vend.purch[gperiod])
  total-msf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vend.ptd-msf[gperiod]).
&ENDIF
