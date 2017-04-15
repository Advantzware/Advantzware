/* cust-markup.i */  
  
DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE cust-markup THEN
    ASSIGN
        custName:SCREEN-VALUE   = getCustName   (cust-markup.cust-no:SCREEN-VALUE)
        procatDscr:SCREEN-VALUE = getProCatDscr (cust-markup.procat:SCREEN-VALUE)
        styleDscr:SCREEN-VALUE  = getStyleDscr  (cust-markup.style:SCREEN-VALUE)
        .
END.
