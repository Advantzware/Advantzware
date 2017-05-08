
IF v-cepanel-log AND style.type EQ "B" THEN  /*only if CEPANEL LOGICAL = YES and the style is "B"*/ 
DO:
    ASSIGN
        iCharCounter     = 1
        iPanel           = 1
        cCharPrev        = ""
        cCharNext        = ""
        dDimensionToTest = 0.

    /*    IF v-cepanel-cha EQ "WminLmin" THEN - Char value of CEPANEL deprecated*/

    /* Loop through each character in the style.formula[2] (Style "L" formula eg. L+W+L+W+J+S for RSC)*/
    DO WHILE AVAILABLE mach AND iPanel LE 20 AND iCharCounter LE LENGTH(style.formula[2]):
        ASSIGN  /* pick the previous and next character in formula */
            cCharPrev = IF iCharCounter EQ 1 THEN "+" ELSE SUBSTRING(style.formula[2],iCharCounter - 1,1)    
            cCharNext = IF iCharCounter EQ LENGTH(style.formula[2]) THEN "+" ELSE SUBSTRING(style.formula[2],iCharCounter + 1,1).
              
        IF cCharPrev EQ "" THEN cCharPrev = "+".
        IF cCharNext EQ "" THEN cCharNext = "+".
      
        IF SUBSTRING(style.formula[2],iCharCounter,1) EQ "+" THEN 
            iPanel = iPanel + 1.
        ELSE 
        DO:
            IF xeb.k-len-array2[iPanel] NE 0 /*.k-len-array2 = Panels for the blank length not 0*/ 
                AND cCharPrev EQ "+" AND cCharNext EQ "+" THEN  /*valid letter representing a panel dimension*/
            DO:
                IF substr(style.formula[2],iCharCounter,1) EQ "L"    AND
                    xeb.k-len-array2[iPanel] LT mach.min-pan-l THEN tt-mach-exc.reason = "Minimum Score Length". 
                ELSE
                    IF substr(style.formula[2],iCharCounter,1) EQ "W"    AND
                        xeb.k-len-array2[iPanel] LT mach.min-pan-w THEN tt-mach-exc.reason = "Minimum Score Width".
                
            END.
        END.
        iCharCounter = iCharCounter + 1.

        IF tt-mach-exc.reason NE "" THEN RELEASE mach.
    END.
    
    ASSIGN
        iCharCounter     = 1
        iPanel           = 1
        cCharPrev        = ""
        cCharNext        = ""
        dDimensionToTest = 0.
        
    DO WHILE AVAILABLE mach AND iPanel LE 20 AND iCharCounter LE length(style.formula[2]):
        ASSIGN
            cCharPrev = IF iCharCounter EQ 1 THEN "+"
                ELSE substr(style.formula[2],iCharCounter - 1,1)
    
            cCharNext = IF iCharCounter EQ length(style.formula[2]) THEN "+"
                ELSE substr(style.formula[2],iCharCounter + 1,1).
              
        IF cCharPrev EQ "" THEN cCharPrev = "+".
        IF cCharNext EQ "" THEN cCharNext = "+".
 
        IF substr(style.formula[2],iCharCounter,1) EQ "+" THEN iPanel = iPanel + 1.
  
        ELSE
            IF substr(style.formula[2],iCharCounter,1) NE "J" THEN 
            DO:
                dDimensionToTest = xeb.k-len-array2[iPanel].

                IF dDimensionToTest NE 0 AND cCharPrev EQ "+" AND cCharNext EQ "+" THEN
                    IF dDimensionToTest LT mach.min_hd_hd THEN
                        tt-mach-exc.reason = "Minimum Panel (Head-Head)".

                    ELSE
                        IF dDimensionToTest GT mach.max_hd_hd THEN
                            tt-mach-exc.reason = "Maximum Panel (Head-Head)".
            END.

        iCharCounter = iCharCounter + 1.
       
        IF tt-mach-exc.reason NE "" THEN RELEASE mach.
    END.

    ASSIGN
        iCharCounter     = 1
        iPanel           = 1
        cCharPrev        = ""
        cCharNext        = ""
        dDimensionToTest = xeb.k-wid-array2[1].

    DO WHILE AVAILABLE mach AND iPanel LE 2 AND iCharCounter LE length(style.formula[1]):
        ASSIGN
            cCharPrev = IF iCharCounter EQ 1 THEN "+"
                ELSE substr(style.formula[1],iCharCounter - 1,1)
    
            cCharNext = IF iCharCounter EQ length(style.formula[1]) THEN "+"
                ELSE substr(style.formula[1],iCharCounter + 1,1).
              
        IF cCharPrev EQ "" THEN cCharPrev = "+".
        IF cCharNext EQ "" THEN cCharNext = "+".

        IF substr(style.formula[1],iCharCounter,1) EQ "+" THEN
            ASSIGN
                iPanel           = iPanel + 1
                dDimensionToTest = xeb.k-wid-array2[iPanel] + dDimensionToTest.

        IF dDimensionToTest NE 0 AND iPanel EQ 2 AND cCharPrev EQ "+" AND cCharNext EQ "+" THEN
            IF dDimensionToTest LT mach.min_slot_score THEN                  
                tt-mach-exc.reason = "Minimum Slot/Score Panel".

            ELSE
                IF dDimensionToTest GT mach.max_slot_score THEN                  
                    tt-mach-exc.reason = "Maximum Slot/Score Panel".

        iCharCounter = iCharCounter + 1.

        IF tt-mach-exc.reason NE "" THEN RELEASE mach.
    END.
END.

