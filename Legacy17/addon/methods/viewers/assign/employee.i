/* employee.i */

DEFINE VARIABLE next-employee AS CHARACTER NO-UNDO.
DEFINE BUFFER buf-employee FOR employee.
DEFINE BUFFER buf-rate FOR rate.

IF adm-new-record THEN
DO WITH FRAME {&FRAME-NAME}:
  IF employee.employee = '' THEN
  DO WHILE TRUE:
     next-employee = STRING(NEXT-VALUE(employee,EMPTRACK),'99999').
     IF CAN-FIND(buf-employee WHERE buf-employee.company = employee.company
                                AND buf-employee.employee = next-employee) THEN
     NEXT.
     employee.employee = next-employee.
     DISPLAY employee.employee.
     LEAVE.
  END.
  employee.passwd = ENCODE('') .
  IF copy-record THEN
  DO:
    MESSAGE 'Copy Employee Rate Records Also?' VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE copy-record.
    IF copy-record THEN
    FOR EACH buf-rate NO-LOCK WHERE buf-rate.company = copy-employee_company
                                AND buf-rate.employee = copy-employee_employee:
      CREATE rate.
      BUFFER-COPY buf-rate EXCEPT buf-rate.rec_key TO rate
          ASSIGN rate.employee = {&FIRST-EXTERNAL-TABLE}.employee.
    END.
    copy-record = FALSE.
  END.
END.
IF verify-passwd NE '' THEN
ASSIGN
  employee.passwd = verify-passwd
                    
/*
  employee.passwd = IF LENGTH(verify-passwd) = 0 THEN ENCODE('')
                    ELSE ENCODE(verify-passwd)
*/
  verify-passwd = ''.
DISABLE pass-word WITH FRAME {&FRAME-NAME}.
