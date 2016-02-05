/* e-item-vend.i */

find item of e-item no-lock no-error.
if e-item-vend.vend-no:screen-value in frame {&frame-name} = "" and
   index("BWFL",item.mat-type) > 0 
then ENABLE e-item.roll-w[1 for 30] WITH FRAME {&FRAME-NAME}.
else disable e-item.roll-w[1 for 30] WITH FRAME {&FRAME-NAME}.
