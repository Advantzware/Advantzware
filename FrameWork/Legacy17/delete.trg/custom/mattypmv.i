/* mattypmv.i */

ASSIGN
  col-coord = group{&group#}-text:COLUMN - {&col-coord}
  row-coord = group{&group#}-text:ROW - {&row-coord}.
IF col-coord NE 0 OR row-coord NE 0 THEN
DO:
  ASSIGN
    current-widget = FRAME {&FRAME-NAME}:HANDLE
    current-widget = current-widget:FIRST-CHILD
    current-widget = current-widget:FIRST-CHILD.
  DO WHILE current-widget NE ?:
    IF current-widget:PRIVATE-DATA = 'group{&group#}' THEN
    DO:
      ASSIGN
        current-widget:COLUMN = current-widget:COLUMN - col-coord + 2
        current-widget:ROW = current-widget:ROW - row-coord.
        IF NOT CAN-DO('TEXT,TOGGLE-BOX',current-widget:TYPE) THEN
      DO:
        label-widget = current-widget:SIDE-LABEL-HANDLE.
        IF VALID-HANDLE(label-widget) THEN
        ASSIGN
          label-widget:COLUMN = label-widget:COLUMN - col-coord + 2
          label-widget:ROW = label-widget:ROW - row-coord.
      END.
    END.
    current-widget = current-widget:NEXT-SIBLING.
  END.
END.
