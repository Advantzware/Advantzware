/* ttblWidgetShow.i - used to show widget-handle temp-table records */

IF CAN-FIND(FIRST {1} WHERE {1}.idx LE {2}) THEN
FOR EACH {1} NO-LOCK WHERE {1}.idx LE {2}:
  ASSIGN
    {1}.{1}:HIDDEN = {3} {4}
    ldummy = {1}.{1}:MOVE-TO-TOP().
END.
