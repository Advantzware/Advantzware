/* ar/invduedt.i  calculate due-date */
IF NOT AVAIL terms THEN FIND FIRST terms WHERE terms.t-code = ar-inv.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
IF AVAIL terms THEN 
    ASSIGN ar-inv.due-date:SCREEN-VALUE = STRING(date(ar-inv.inv-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + terms.net-days).
                           
   
