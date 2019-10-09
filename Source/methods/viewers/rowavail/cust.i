/* cust.i */

&IF '{&CUSTOMER-TOTALS}' NE '' &THEN
{custom/getperd.i}
IF gperiod NE 0 AND AVAILABLE cust THEN
ASSIGN
  ptd-sales:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cust.sales[gperiod])
  total-msf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cust.ptd-msf[gperiod]).
DO WITH FRAME {&FRAME-NAME}:
  {custom/cust-tot.i}
END.
&ENDIF
