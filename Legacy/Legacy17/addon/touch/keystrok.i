/* keystrok.i */

DEFINE INPUT PARAMETER keystroke AS CHARACTER NO-UNDO.

IF INDEX(keystroke,'(ON)') NE 0 THEN
caps_lock = YES.
ELSE
IF INDEX(keystroke,'(OFF)') NE 0 THEN
caps_lock = NO.
ELSE
DO WITH FRAME {&FRAME-NAME}:
  IF NOT caps_lock THEN
  keystroke = LC(keystroke).
  CASE keystroke:
    WHEN 'ALPHA' THEN
    {methods/run_link.i "CONTAINER" "Display_Keyboard" "('alphabet.',THIS-PROCEDURE:HANDLE)"}
    WHEN 'BACKSPACE' THEN
    IF field_value NE '' THEN
    field_value = SUBSTR(field_value,1,LENGTH(field_value) - 1).
    WHEN 'CLEAR' THEN
    field_value = ''.
    WHEN 'DQ' THEN /* Double Quote */
    field_value = field_value + '"'.
    WHEN 'QWERTY' THEN
    {methods/run_link.i "CONTAINER" "Display_Keyboard" "('keyboard.',THIS-PROCEDURE:HANDLE)"}
    WHEN 'SPACE' THEN
    field_value = field_value + '`'.
    WHEN 'SORT' THEN
    {methods/run_link.i "CONTAINER" "Display_Keyboard" "('sortpad.',THIS-PROCEDURE:HANDLE)"}
    OTHERWISE
    field_value = field_value + keystroke.
  END CASE.
  IF VALID-HANDLE(h_field) THEN
  h_field:SCREEN-VALUE = REPLACE(field_value,'`',' ').
 
END.

&IF '{&KEYSTROKE}' = 'ALLMACHS' OR
    '{&KEYSTROKE}' = 'ALLJOBS'  OR
    '{&KEYSTROKE}' = 'EMPLOYEE' &THEN
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  DO i = 1 TO SL:NUM-ITEMS:
    IF SL:ENTRY(i) BEGINS h_field:SCREEN-VALUE THEN
    LEAVE.
  END.

 IF i GT SL:NUM-ITEMS THEN
  DO:
    MESSAGE 'NO MATCH FOUND BEGINNING' SKIP(1) h_field:SCREEN-VALUE
        VIEW-AS ALERT-BOX.
    RUN Key_Stroke ('BACKSPACE').
    RETURN.
  END.
  ASSIGN
    SL:SCREEN-VALUE = SL:ENTRY(i)
    item = i.
END.
&ELSEIF '{&KEYSTROKE}' = 'PASSWORD' &THEN
h_field:SCREEN-VALUE = FILL('*',LENGTH(field_value)).
&ENDIF
