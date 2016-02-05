/* b-oeboll.i */

/* {custom/getcmpny.i} */

/*
&IF '{&cellColumnBrowserName}' EQ 'oeinqb-oeboll' &THEN
   RUN setCellColumns.
&ENDIF */

IF INDEX(PROGRAM-NAME(4), "oe/w-oeinv.w") > 0 THEN
   li-cost:VISIBLE IN BROWSE {&browse-name} = YES.

{methods/winReSizeLocInit.i}
