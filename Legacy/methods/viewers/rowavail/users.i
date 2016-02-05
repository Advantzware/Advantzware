/* users.i */
&IF '{&users-rowavail}' NE '' &THEN
    run {&users-rowavail}. 
&ENDIF
