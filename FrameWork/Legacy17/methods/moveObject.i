/* moveObject.i */

&IF DEFINED(h_Object{1}) NE 0 &THEN
  IF VALID-HANDLE({&h_Object{1}}) AND
     NOT CAN-DO(winObjects,'{&h_Object{1}}') THEN DO:
    ASSIGN 
      currentWidget = hWinKitFrame
      currentWidget = currentWidget:FIRST-CHILD
      currentWidget = currentWidget:FIRST-CHILD
      .
    DO WHILE currentWidget NE ?:
      IF currentWidget:INSTANTIATING-PROCEDURE EQ {&h_Object{1}} AND
         NOT CAN-DO(winObjects,'{&h_Object{1}}') THEN DO:
        IF CAN-DO('{&moveRight}','{&h_Object{1}}') THEN DO: /* move right */
            IF VALID-OBJECT (oFormControl) THEN
            currentWidget:X = oWinKitControl:Width - currentWidget:WIDTH-PIXELS.
            ELSE
            RUN set-position IN {&h_Object{1}} (currentWidget:ROW,currentWidget:COL + colDiff) NO-ERROR.
        END. /* if moveright */
        ELSE DO: /* move down */
            IF VALID-OBJECT (oFormControl) THEN
            currentWidget:Y = currentwidget:Y + iPageRowDiff.
            ELSE 
            RUN set-position IN {&h_Object{1}} (currentWidget:ROW + rowDiff,currentWidget:COL) NO-ERROR.
        END. /* else */
        winObjects = winObjects + '{&h_Object{1}}' + ','.
      END.
      currentWidget = currentWidget:NEXT-SIBLING.
    END.
  END.
&ENDIF
