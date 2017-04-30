/* mmty.i */

/*  to disable/enable if widget is not hiden  */
&if defined(enable-detail) ne 0 &then
    RUN {&enable-detail} .
&endif
