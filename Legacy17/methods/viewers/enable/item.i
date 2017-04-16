/* item.i */
/*
IF NOT item_yield:HIDDEN IN FRAME {&FRAME-NAME} THEN
ENABLE item_yield WITH FRAME {&FRAME-NAME}.
*/

/*  to disable/enable if widget is not hiden 
&if defined(item-corr) ne 0 &then
   &scoped-define mat-types-enable yes
   {cec/mattypes.i} 
&else
   &scoped-define mat-types-enable yes
   {custom/mattypes.i} 
&endif
*/

IF "{&enable-item}" NE "" THEN RUN {&enable-item}.
