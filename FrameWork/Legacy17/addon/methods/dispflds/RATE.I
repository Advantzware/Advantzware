/* rate.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.machine" THEN
DO:
  FIND mach
      WHERE mach.company = gcompany
        AND mach.m-code = {&FIRST-EXTERNAL-TABLE}.machine:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  mach_m-dscr = IF NOT AVAILABLE mach THEN ""
                ELSE mach.m-dscr.
  DISPLAY mach_m-dscr.
END.
WHEN "{&FIRST-EXTERNAL-TABLE}.shift" THEN
DO:
  FIND shifts
      WHERE shifts.company = gcompany
        AND shifts.shift = {&FIRST-EXTERNAL-TABLE}.shift:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  shifts_description = IF NOT AVAILABLE shifts THEN ""
                       ELSE shifts.description.
  DISPLAY shifts_description.
END.
