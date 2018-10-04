/* farmTab.i */

  DEFINE INPUT PARAM ipPurchased AS LOGICAL NO-UNDO.

  DEFINE VARIABLE tabNo AS INTEGER NO-UNDO.
  
  {methods/run_link.i "PAGE-SOURCE" "get-attribute" "('FOLDER-LABELS')"}
  tabNo = LOOKUP('Farm',RETURN-VALUE,'|').
  IF tabNo NE 0 THEN DO:
     IF ipPurchased THEN RUN enable-folder-page IN WIDGET-HANDLE(char-hdl) (tabNo) NO-ERROR.
     ELSE RUN disable-folder-page IN WIDGET-HANDLE(char-hdl) (tabNo) NO-ERROR.
  END.
