/* zipcode.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.state" THEN
DO:
  FIND statecod
      WHERE statecod.statecod = {&FIRST-EXTERNAL-TABLE}.state:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  statecod_description = IF NOT AVAILABLE statecod THEN ""
                         ELSE statecod.description.
  DISPLAY statecod_description.
END.
