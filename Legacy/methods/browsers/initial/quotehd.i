/* quotehd.i */

&IF '{&runValueChanged}' EQ 'YES' &THEN
APPLY "value-changed" TO browse-order IN FRAME {&FRAME-NAME}.
&ENDIF
