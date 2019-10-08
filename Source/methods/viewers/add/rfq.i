/* rfq.i */
 def var new-rfq# like rfq.rfq-no no-undo.
  
  find first rfq-ctrl no-error.
  if avail rfq-ctrl then do:
     new-rfq# = rfq-ctrl.rfq-num.
     rfq-ctrl.rfq-num = rfq-ctrl.rfq-num + 1.
  end.
  
  assign rfq.rfq-no:screen-value in frame {&frame-name} = string(new-rfq#)
         rfq.ship-name:screen-value in frame {&frame-name} = ""
         rfq.ship-addr[1]:screen-value in frame {&frame-name} = ""
         rfq.ship-addr[2]:screen-value in frame {&frame-name} = ""
         rfq.ship-city:screen-value in frame {&frame-name} = ""
         rfq.ship-state:screen-value in frame {&frame-name} = ""
         rfq.ship-zip:screen-value in frame {&frame-name} = ""
         rfq.req-date:screen-value in frame {&frame-name} = string(today)
         .
    
