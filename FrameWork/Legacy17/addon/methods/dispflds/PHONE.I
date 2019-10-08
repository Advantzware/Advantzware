/* phone.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.titlcode" THEN
DO:
  FIND titlcode
      WHERE titlcode.titlcode = {&FIRST-EXTERNAL-TABLE}.titlcode:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  titlcode_description = IF NOT AVAILABLE titlcode THEN ""
                         ELSE titlcode.description.
  DISPLAY titlcode_description.
END.
