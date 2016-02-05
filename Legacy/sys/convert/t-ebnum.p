/* t-ebnum.p */

DISABLE TRIGGERS FOR LOAD OF eb.

DEF TEMP-TABLE tt-eb LIKE eb.

DEF VAR lv-num-up LIKE eb.num-up NO-UNDO.

INPUT FROM c:\tmp\eb.dAT NO-ECHO.

REPEAT:
    CREATE tt-eb.
    SET tt-eb.company                 
        tt-eb.loc                     
        tt-eb.est-no                  
        tt-eb.e-num                   
        tt-eb.form-no                 
        tt-eb.cust-seq                
        tt-eb.blank-no                
        tt-eb.num-up 
        tt-eb.num-wid                 
        tt-eb.num-len                 
        tt-eb.num-dep
        .
    
    lv-num-up = (IF tt-eb.num-wid EQ 0 THEN 1 ELSE tt-eb.num-wid) *
                (IF tt-eb.num-len EQ 0 THEN 1 ELSE tt-eb.num-len) *
                (IF tt-eb.num-dep EQ 0 THEN 1 ELSE tt-eb.num-dep).

    IF tt-eb.num-up <> lv-num-up
    THEN DO:
      FIND FIRST eb WHERE eb.e-num = tt-eb.e-num and
                          eb.form-no = tt-eb.form-no
                       and eb.cust-seq = tt-eb.cust-seq
                       and eb.blank-no = tt-eb.blank-no NO-ERROR.
                       
      ASSIGN eb.num-up  = tt-eb.num-up
             eb.num-wid = tt-eb.num-up
             eb.num-len = 1
             eb.num-dep = 1
             .
        
      DISP tt-eb.est-no tt-eb.num-wid tt-eb.num-len tt-eb.num-dep tt-eb.num-up WITH FRAME dis DOWN .
    END.
END.

