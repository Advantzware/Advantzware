/* winReSize.i - browse */

&IF DEFINED(winReSize) NE 0 &THEN
PROCEDURE winReSize:
  DEFINE INPUT PARAMETER ipRowDiff AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipColDiff AS DECIMAL NO-UNDO.
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
    winReSize = YES.
  IF (ipRowDiff > 0 OR ipRowDiff = 0) AND (ipColDiff > 0 OR ipColDiff = 0) THEN
  ASSIGN   
    &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
      FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
    &ELSEIF '{&sizeOption}' EQ 'WIDTH' &THEN
      FRAME {&FRAME-NAME}:WIDTH = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ELSE
      FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
      FRAME {&FRAME-NAME}:WIDTH = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ENDIF
    NO-ERROR.
  ELSE IF (ipRowDiff > 0 OR ipRowDiff = 0) AND (ipColDiff < 0 OR ipColDiff = 0) THEN
  ASSIGN
    &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff  
    &ELSE
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
    &ENDIF
    NO-ERROR.
  ELSE IF (ipRowDiff < 0 OR ipRowDiff = 0) AND (ipColDiff > 0 OR ipColDiff = 0) THEN
  ASSIGN
    &IF '{&sizeOption}' EQ 'WIDTH' &THEN
            FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ELSE
            FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ENDIF
    NO-ERROR.
            
    ASSIGN
    currentWidget = FRAME {&FRAME-NAME}:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD
    NO-ERROR.
    
  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'BROWSE' THEN DO:
      ASSIGN
      &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
        currentWidget:HEIGHT = currentWidget:HEIGHT + ipRowDiff
      &ELSEIF '{&sizeOption}' EQ 'WIDTH' &THEN
        currentWidget:WIDTH = currentWidget:WIDTH + ipColDiff
      &ELSE
        currentWidget:HEIGHT = currentWidget:HEIGHT + ipRowDiff
        currentWidget:WIDTH = currentWidget:WIDTH + ipColDiff
      &ENDIF
        NO-ERROR.
      &IF DEFINED(repositionBrowse) NE 0 &THEN
      APPLY 'HOME':U TO currentWidget.
      &ENDIF
    END. /* if browse type */
    &IF DEFINED(browseOnly) EQ 0 &THEN
      ELSE IF currentWidget:PRIVATE-DATA NE "NoWinReSize" THEN
           currentWidget:ROW = currentWidget:ROW + ipRowDiff.
    &ENDIF
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  
  IF (ipRowDiff < 0 OR ipRowDiff = 0)  AND (ipColDiff < 0 OR ipColDiff = 0) THEN
  ASSIGN
    &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
    &ELSEIF '{&sizeOption}' EQ 'WIDTH' &THEN
            FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ELSE
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
            FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ENDIF
    NO-ERROR.
  ELSE IF (ipRowDiff > 0 OR ipRowDiff = 0) AND (ipColDiff < 0 OR ipColDiff = 0) THEN
  ASSIGN
    &IF '{&sizeOption}' EQ 'WIDTH' &THEN
            FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ELSE           
            FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH + ipColDiff
    &ENDIF
    NO-ERROR.
  ELSE IF (ipRowDiff < 0 OR ipRowDiff = 0)  AND (ipColDiff > 0 OR ipColDiff = 0) THEN
  ASSIGN
    &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
    &ELSE
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
    &ENDIF
    NO-ERROR.
END PROCEDURE.
&ENDIF
