/* vend.i */

&IF '{&VENDOR-TOTALS}' NE '' &THEN
ENABLE ptd-purch total-msf WITH FRAME {&FRAME-NAME}.
APPLY 'ENTRY' TO ptd-purch.
&ENDIF

&IF '{&vend-maint}' NE '' &THEN
    RUN '{&vend-maint}'.  /* in vend's viewer */
&ENDIF
