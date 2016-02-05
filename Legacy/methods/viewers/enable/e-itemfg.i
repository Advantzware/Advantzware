/* e-itemfg.i */

if avail e-itemfg-vend and (e-itemfg-vend.vend-no:screen-value in frame {&frame-name} = ''
          or e-itemfg.std-uom:screen-value in frame {&frame-name} = '')
then ENABLE e-itemfg.std-uom WITH FRAME {&FRAME-NAME}.
else disable e-itemfg.std-uom WITH FRAME {&FRAME-NAME}.
