/* moveObject.i */

&IF DEFINED(h_Object{1}) NE 0 &THEN
  IF VALID-HANDLE({&h_Object{1}}) AND
     NOT CAN-DO(winObjects,'{&h_Object{1}}') THEN DO:
    ASSIGN /*
      currentWidget = {&WINDOW-NAME}:HANDLE
      currentWidget = currentWidget:FIRST-CHILD */
      currentWidget = FRAME {&FRAME-NAME}:HANDLE
      currentWidget = currentWidget:FIRST-CHILD
      currentWidget = currentWidget:FIRST-CHILD.
    DO WHILE currentWidget NE ?:
      IF currentWidget:INSTANTIATING-PROCEDURE EQ {&h_Object{1}} AND
         NOT CAN-DO(winObjects,'{&h_Object{1}}') AND rowDiff NE 0 THEN DO:
        IF CAN-DO('{&moveRight}','{&h_Object{1}}') THEN /* move right */
        RUN set-position IN {&h_Object{1}} (currentWidget:ROW,currentWidget:COL + colDiff) NO-ERROR.
        ELSE /* move down */
        RUN set-position IN {&h_Object{1}} (currentWidget:ROW + rowDiff,currentWidget:COL) NO-ERROR.
        winObjects = winObjects + '{&h_Object{1}}' + ','.
      END.
      currentWidget = currentWidget:NEXT-SIBLING.
    END.
  END.
&ENDIF
