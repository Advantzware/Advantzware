/* ttblWidgetHide.i - used to hide widget-handle temp-table records */

IF CAN-FIND(FIRST {1} WHERE {1}.idx GT {2}) THEN
FOR EACH {1} NO-LOCK WHERE {1}.idx GT {2}:
  ASSIGN
    &IF '{1}' EQ 'JobWidget' &THEN
    {1}.isSelected = NO
    &ENDIF
    {1}.{1}:HIDDEN = YES
    {1}.{1}:WIDTH-PIXELS = 1.
END.
