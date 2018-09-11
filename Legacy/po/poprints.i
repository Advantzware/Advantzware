
 DO:
  ASSIGN
   lv-val     = 0
   lv-typ     = "".

  DO x = 1 TO 20:
   IF po-ordl.scorePanels[i] NE 0 or po-ordl.scoreType[i] NE "" THEN ASSIGN
     lv-val[x] = po-ordl.scorePanels[x]
     lv-typ[x] = SUBSTR(po-ordl.scoreType[x],1).
  END.

  DO lv-int = 0 TO 1:
    ASSIGN
     v-lscore-c = ""
     len-score  = "".

    DO x = 1 TO 10:
      IF lv-val[(lv-int * 10) + x] NE 0 THEN
        v-lscore-c = v-lscore-c + TRIM(IF lv-val[(lv-int * 10) + x] GT 9999 THEN
                                         STRING(lv-val[(lv-int * 10) + x],">>>>>")
                                       ELSE
                                       IF lv-val[(lv-int * 10) + x] GT 999 THEN
                                         STRING(lv-val[(lv-int * 10) + x],">>>>")
                                       ELSE 
                                         STRING(lv-val[(lv-int * 10) + x],">>>.99")).

      /* print score type for Premier */
      IF v-score-types AND lv-typ[(lv-int * 10) + x] NE "" THEN 
        v-lscore-c = v-lscore-c + lv-typ[(lv-int * 10) + x] + " ".     
      ELSE v-lscore-c = v-lscore-c + " ".
    END.
  
    IF v-lscore-c NE "" THEN DO:
      v-space = NO.

      DO x = 1 TO LENGTH(v-lscore-c):
        IF SUBSTR(v-lscore-c,x,1) NE " " THEN
          ASSIGN
           len-score = len-score + SUBSTR(v-lscore-c,x,1)
           v-space   = YES.
         
        ELSE
        IF v-space THEN
          ASSIGN
           len-score = len-score + "  "
           v-space   = NO.
      END.

      v-test-scr = YES.
               
      DO x = 1 TO LENGTH(TRIM(len-score)):
        IF SUBSTR(TRIM(len-score),x,1) EQ " " THEN v-test-scr = NO.
      END.
