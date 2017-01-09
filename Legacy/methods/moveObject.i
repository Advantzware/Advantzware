/* moveObject.i */

&IF DEFINED(h_Object{1}) NE 0 &THEN
  IF VALID-HANDLE({&h_Object{1}}) AND
     NOT CAN-DO(winObjects,'{&h_Object{1}}') THEN DO:
    ASSIGN /*
      currentWidget = {&WINDOW-NAME}:HANDLE
      currentWidget = currentWidget:FIRST-CHILD */
      currentWidget = hWinKitFrame /*FRAME {&FRAME-NAME}:HANDLE*/
      currentWidget = currentWidget:FIRST-CHILD
      currentWidget = currentWidget:FIRST-CHILD.

    DO WHILE currentWidget NE ?:


      IF currentWidget:INSTANTIATING-PROCEDURE EQ {&h_Object{1}} AND
         NOT CAN-DO(winObjects,'{&h_Object{1}}') AND iPagerowDiff NE 0 THEN DO:

        IF CAN-DO('{&moveRight}','{&h_Object{1}}') THEN /* move right */
            currentWidget:x = oWinKitControl:Width - currentWidget:WIDTH-PIXELS .
        //RUN set-position IN {&h_Object{1}} (currentWidget:ROW,currentWidget:COL + iPagecolDiff) NO-ERROR.
        ELSE /* move down */
            currentWidget:y = currentwidget:y + iPageRowDiff .
        //RUN set-position IN {&h_Object{1}} (currentWidget:ROW + iPagerowDiff,currentWidget:COL) NO-ERROR.
        winObjects = winObjects + '{&h_Object{1}}' + ','.
      END.

/*MESSAGE  currentWidget:INSTANTIATING-PROCEDURE:FILE-NAME {&h_Object{1}}:FILE-NAME SKIP currentWidget:x*/
/*VIEW-AS ALERT-BOX.                                                                                    */

      currentWidget = currentWidget:NEXT-SIBLING.
    END.
  END.
&ENDIF
