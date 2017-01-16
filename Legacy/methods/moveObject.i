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
         NOT CAN-DO(winObjects,'{&h_Object{1}}') AND iPagerowDiff NE 0 THEN DO:

        IF CAN-DO('{&moveRight}','{&h_Object{1}}') THEN /* move right */
            currentWidget:x = oWinKitControl:Width - currentWidget:WIDTH-PIXELS .
        ELSE /* move down */
            currentWidget:y = currentwidget:y + iPageRowDiff .
        winObjects = winObjects + '{&h_Object{1}}' + ','.
      END.
      currentWidget = currentWidget:NEXT-SIBLING.
    END.
  END.
&ENDIF
