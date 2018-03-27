/* vend.i */

&IF '{&VENDOR-TOTALS}' NE '' &THEN
DISABLE ptd-purch total-msf WITH FRAME {&FRAME-NAME}.
ASSIGN
  vend.purch[gperiod] = ptd-purch
  vend.ptd-msf[gperiod] = total-msf.
&ENDIF
