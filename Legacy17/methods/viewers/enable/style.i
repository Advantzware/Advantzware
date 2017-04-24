/* style.i */

&IF '{&style-maint}' NE '' &THEN
    run ENABLE-style-field .  /* in style's viewer */
&ENDIF

&IF '{&style-formular}' NE '' &THEN
    run ENABLE-style-formular .
&ENDIF
