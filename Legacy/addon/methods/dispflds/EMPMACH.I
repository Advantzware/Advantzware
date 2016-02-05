/* empmach.i */

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
