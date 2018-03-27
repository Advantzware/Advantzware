/* outputClose.i */

IF NOT ipExcel AND ipShowParametersPage THEN DO:
  PAGE.
  PUT UNFORMATTED SKIP(1) 'Parameters:' AT 5 SKIP(1)
    'Date Range: ' AT 10 STRING(fromDate,'99.99.9999')
    'To: ' AT 50 STRING(toDate,'99.99.9999')
    'Due Date Range: ' AT 10 STRING(fromDueDate,'99.99.9999')
    'To: ' AT 50 STRING(toDueDate,'99.99.9999')
    'Job: ' AT 10 jobValueLo 'To: ' AT 50 jobValueHi
    'Resource: ' AT 10 resourceValueLo 'To: ' AT 50 resourceValueHi.
  DO i = 1 TO {&udfExtent}:
    IF udfLabel[i] NE 'Unused' THEN
    PUT UNFORMATTED udfLabel[i] AT 10 ': ' udfValueLo[i]
      'To: ' AT 50 udfValueHi[i].
  END.
  DO i = 1 TO {&userExtent}:
    IF userLabel[i] NE 'Not Used' THEN
    PUT UNFORMATTED userLabel[i] AT 10 ': ' userValueLo[i]
      'To: ' AT 50 userValueHi[i].
  END.
  &IF INDEX('{&asiPrintNames}','{&printProgramName}') NE 0 &THEN
    PUT UNFORMATTED 'Departments: ' AT 10 departmentValue.
  &ENDIF
  PUT UNFORMATTED SKIP(2) 'Sorted By: ' AT 10 filterSortBy.
END.
OUTPUT CLOSE.
SESSION:SET-WAIT-STATE('').
