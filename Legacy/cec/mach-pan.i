
IF v-cepanel-log AND style.type EQ "B" THEN 
DO:
    ASSIGN
        v-int   = 1
        v-panel = 1
        v-prev  = ""
        v-next  = ""
        v-dec   = 0.

    IF v-cepanel-cha EQ "WminLmin" THEN
    DO WHILE AVAILABLE mach AND v-panel LE 20 AND v-int LE length(style.formula[2]):
        ASSIGN
            v-prev = IF v-int EQ 1 THEN "+"
              ELSE substr(style.formula[2],v-int - 1,1)
    
            v-next = IF v-int EQ length(style.formula[2]) THEN "+"
              ELSE substr(style.formula[2],v-int + 1,1).
              
        IF v-prev EQ "" THEN v-prev = "+".
        IF v-next EQ "" THEN v-next = "+".
      
        IF substr(style.formula[2],v-int,1) EQ "+" THEN v-panel = v-panel + 1.
  
        ELSE
            IF xeb.k-len-array2[v-panel] NE 0                     AND
                v-prev EQ "+"                                      AND
                v-next EQ "+"                                      THEN 
            DO:
    
                IF substr(style.formula[2],v-int,1) EQ "l"    AND
                    xeb.k-len-array2[v-panel] LT mach.min-pan-l THEN tt-mach-exc.reason = "Minimum Score Length". 
        
                ELSE
                    IF substr(style.formula[2],v-int,1) EQ "w"    AND
                        xeb.k-len-array2[v-panel] LT mach.min-pan-w THEN tt-mach-exc.reason = "Minimum Score Width".
            END.
    
        v-int = v-int + 1.

        IF tt-mach-exc.reason NE "" THEN RELEASE mach.
    END.

    ELSE 
    DO:
        DO WHILE AVAILABLE mach AND v-panel LE 20 AND v-int LE length(style.formula[2]):
            ASSIGN
                v-prev = IF v-int EQ 1 THEN "+"
                ELSE substr(style.formula[2],v-int - 1,1)
    
                v-next = IF v-int EQ length(style.formula[2]) THEN "+"
                ELSE substr(style.formula[2],v-int + 1,1).
              
            IF v-prev EQ "" THEN v-prev = "+".
            IF v-next EQ "" THEN v-next = "+".
 
            IF substr(style.formula[2],v-int,1) EQ "+" THEN v-panel = v-panel + 1.
  
            ELSE
                IF substr(style.formula[2],v-int,1) NE "J" THEN 
                DO:
                    v-dec = xeb.k-len-array2[v-panel].

                    IF v-dec NE 0 AND v-prev EQ "+" AND v-next EQ "+" THEN
                        IF v-dec LT mach.min-pan-w THEN
                            tt-mach-exc.reason = "Minimum Panel (Head-Head)".

                        ELSE
                            IF v-dec GT mach.max-pan-w THEN
                                tt-mach-exc.reason = "Maximum Panel (Head-Head)".
                END.

            v-int = v-int + 1.
       
            IF tt-mach-exc.reason NE "" THEN RELEASE mach.
        END.

        ASSIGN
            v-int   = 1
            v-panel = 1
            v-prev  = ""
            v-next  = ""
            v-dec   = xeb.k-wid-array2[1].

        DO WHILE AVAILABLE mach AND v-panel LE 2 AND v-int LE length(style.formula[1]):
            ASSIGN
                v-prev = IF v-int EQ 1 THEN "+"
                ELSE substr(style.formula[1],v-int - 1,1)
    
                v-next = IF v-int EQ length(style.formula[1]) THEN "+"
                ELSE substr(style.formula[1],v-int + 1,1).
              
            IF v-prev EQ "" THEN v-prev = "+".
            IF v-next EQ "" THEN v-next = "+".

            IF substr(style.formula[1],v-int,1) EQ "+" THEN
                ASSIGN
                    v-panel = v-panel + 1
                    v-dec   = xeb.k-wid-array2[v-panel] + v-dec.

            IF v-dec NE 0 AND v-panel EQ 2 AND v-prev EQ "+" AND v-next EQ "+" THEN
                IF v-dec LT mach.min-pan-l THEN                  
                    tt-mach-exc.reason = "Minimum Slot/Score Panel".

                ELSE
                    IF v-dec GT mach.max-pan-l THEN                  
                        tt-mach-exc.reason = "Maximum Slot/Score Panel".

            v-int = v-int + 1.

            IF tt-mach-exc.reason NE "" THEN RELEASE mach.
        END.
    END.
END.
