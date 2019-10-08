/* terms.i */

ENABLE termsCOD WITH FRAME {&FRAME-NAME}.

&IF '{&proc-enable}' NE '' &THEN
   RUN {&proc-enable}.
&endif