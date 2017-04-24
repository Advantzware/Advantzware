/* sys-ctrl.i */

&IF '{&post-enable}' NE '' &THEN
    RUN {&post-enable}.
&ENDIF
