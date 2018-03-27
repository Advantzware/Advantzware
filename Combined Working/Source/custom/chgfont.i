 /* custom/chgfont.i */
 IF SELF:SCREEN-VALUE BEGINS "L" THEN 
    ASSIGN lv-font-no = "13"
           lines-per-page = 55
           lv-font-name = "Courier New Size=9 (13CPI)".
    
 ELSE    ASSIGN lv-font-no = "11"
                lines-per-page = 99
                lv-font-name = "Courier New Size=7 (17 cpi for 132 CLMN REPORT)".
 
 DISPL lv-font-no lines-per-page lv-font-name WITH FRAME {&FRAME-NAME}.
