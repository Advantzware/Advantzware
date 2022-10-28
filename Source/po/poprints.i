
RUN po/POProcs.p PERSISTENT SET hdPOProcs.

DO: /* Replaces old IF THEN DO start */
ASSIGN
   lv-val     = 0
   lv-typ     = "".
   
   RUN PO_GetLineScoresAndTypes IN hdPOProcs (
            INPUT  po-ordl.company,
            INPUT  po-ordl.po-no,
            INPUT  po-ordl.line,
            INPUT  cScorePanelType,
            OUTPUT lv-val,
            OUTPUT lv-typ
            ).

    RUN PO_GetLineScoresAndTypes IN hdPOProcs (
            INPUT  po-ordl.company,
            INPUT  po-ordl.po-no,
            INPUT  po-ordl.line,
            INPUT  "W",
            OUTPUT cWidScoreValue,
            OUTPUT cWidScoreType
            ).

   DELETE PROCEDURE hdPOProcs.   

DO lv-int = 0 TO 1:
    ASSIGN
     v-lscore-c = ""
     len-score  = ""
     clscoreWidth = ""
     cWidScore = "".

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
    
    DO x = 1 TO 10:
      IF cWidScoreValue[(lv-int * 10) + x] NE 0 THEN
        clscoreWidth = clscoreWidth + TRIM(IF cWidScoreValue[(lv-int * 10) + x] GT 9999 THEN
                                         STRING(cWidScoreValue[(lv-int * 10) + x],">>>>>")
                                       ELSE
                                       IF cWidScoreValue[(lv-int * 10) + x] GT 999 THEN
                                         STRING(cWidScoreValue[(lv-int * 10) + x],">>>>")
                                       ELSE 
                                         STRING(cWidScoreValue[(lv-int * 10) + x],">>>.99")).

      /* print score type for Premier */
      IF v-score-types AND cWidScoreType[(lv-int * 10) + x] NE "" THEN 
        clscoreWidth = clscoreWidth + cWidScoreType[(lv-int * 10) + x] + " ".     
      ELSE clscoreWidth = clscoreWidth + " ".
    END.
  
    IF v-lscore-c NE "" OR clscoreWidth NE "" THEN DO:
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
      
      DO x = 1 TO LENGTH(clscoreWidth):
        IF SUBSTR(clscoreWidth,x,1) NE " " THEN
          ASSIGN
           cWidScore = cWidScore + SUBSTR(clscoreWidth,x,1)
           v-space   = YES.         
        ELSE
        IF v-space THEN
          ASSIGN
           cWidScore = cWidScore + "  "
           v-space   = NO.
      END.

      v-test-scr = YES.
               
      DO x = 1 TO LENGTH(TRIM(len-score)):
        IF SUBSTR(TRIM(len-score),x,1) EQ " " THEN v-test-scr = NO.
      END.
