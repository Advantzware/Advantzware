/* ttblWidgetDefine.i - used to define widget-handle temp-tables */

DEFINE TEMP-TABLE {1} NO-UNDO
  FIELD idx AS INTEGER
  FIELD {1} AS WIDGET-HANDLE
  &IF '{2}' NE '' &THEN
  FIELD {2} AS INTEGER
  &ENDIF
  &IF '{3}' NE '' &THEN
  FIELD {3} AS INTEGER
  &ENDIF
  &IF '{4}' NE '' &THEN
  FIELD {4} AS LOGICAL
  &ENDIF
    INDEX {1} IS PRIMARY UNIQUE idx
    &IF '{4}' NE '' &THEN
    INDEX {4} {4}
    &ENDIF
  .
