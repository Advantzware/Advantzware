/* machemp.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.employee" THEN
DO:
  FIND employee
      WHERE employee.company = gcompany
        AND employee.employee = {&FIRST-EXTERNAL-TABLE}.employee:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  employee_name = IF NOT AVAILABLE employee THEN ""
                  ELSE employee.first_name + ' ' + employee.last_name.
  DISPLAY employee_name.
END.
