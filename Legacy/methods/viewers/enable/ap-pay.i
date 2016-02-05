/* ap-pay.i */

&IF '{&enable-appay}' NE '' &THEN
    RUN {&enable-appay}.

&ENDIF
