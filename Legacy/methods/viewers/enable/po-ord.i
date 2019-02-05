/* po-ord.i */

&IF defined(check-dropship) > 0 &THEN
    RUN check-dropship.
&ENDIF


&IF defined(post-enable) > 0 &THEN
    RUN {&post-enable}.
&ENDIF

IF adm-new-record THEN 
  lNewOrd = TRUE.
