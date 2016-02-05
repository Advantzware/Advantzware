/* employee.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.emp_type" THEN
DO:
  FIND emp_type
      WHERE emp_type.emp_type = {&FIRST-EXTERNAL-TABLE}.emp_type:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  emp_type_description = IF NOT AVAILABLE emp_type THEN ""
                         ELSE emp_type.description.
  DISPLAY emp_type_description.
END.
