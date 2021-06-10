
DO lv-int = 0 TO 1:
    ASSIGN
        v-lscore-c = ""
        len-score  = ""
        .

    DO x = 1 TO 10:
        IF lv-val[(lv-int * 10) + x] NE 0 THEN
            v-lscore-c = v-lscore-c 
                       + IF lv-val[(lv-int * 10) + x] GT 9999 THEN
                             TRIM(STRING(lv-val[(lv-int * 10) + x],">>>>>"))
                         ELSE IF lv-val[(lv-int * 10) + x] GT 999 THEN
                             TRIM(STRING(lv-val[(lv-int * 10) + x],">>>>"))
                         ELSE 
                             TRIM(STRING(lv-val[(lv-int * 10) + x],">>>.99")).
    
        /* print score type for Premier */
        IF v-score-types AND lv-typ[(lv-int * 10) + x] NE "" THEN 
            v-lscore-c = v-lscore-c + lv-typ[(lv-int * 10) + x] + " ".     
        ELSE 
            v-lscore-c = v-lscore-c + " ".
    END.
    
    ASSIGN
        v-lscore-c = TRIM(v-lscore-c)
        len-score  = v-lscore-c
        .

    IF v-lscore-c NE "" THEN DO:
        /* Remove any additional sapces  */
        DO WHILE INDEX(len-score, "  ") GT 0:
            len-score = REPLACE(len-score, "  ", " ").
        END.
        
        /* To make sure there is couple of spaces between each score */
        len-score = REPLACE(len-score, " ", "  ").
        
        v-test-scr = INDEX(len-score, " ") EQ 0.
    END.

