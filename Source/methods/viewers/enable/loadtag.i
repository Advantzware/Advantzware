/* loadtag.i */

&IF '{&post-enable}' NE '' &THEN
    run {&post-enable} .
&ENDIF

