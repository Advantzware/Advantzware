/* e-item.i */

if avail e-item-vend and (e-item-vend.vend-no:screen-value in frame {&frame-name} = ''
          or e-item.std-uom:screen-value in frame {&frame-name} = '')
then ENABLE e-item.std-uom WITH FRAME {&FRAME-NAME}.
else disable e-item.std-uom WITH FRAME {&FRAME-NAME}.
