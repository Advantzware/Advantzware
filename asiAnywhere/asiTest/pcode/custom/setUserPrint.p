/* setUserPrint.p */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipProgramID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFieldList AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFieldValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

FIND FIRST user-print EXCLUSIVE-LOCK
     WHERE user-print.company EQ ipCompany
       AND user-print.program-id EQ ipProgramID
       AND user-print.user-id EQ USERID('NoSweat')
       AND user-print.batch EQ '' NO-ERROR.
IF AVAILABLE user-print THEN
DO i = 1 TO EXTENT(user-print.field-name):
  IF NOT CAN-DO(ipFieldList,user-print.field-name[i]) THEN NEXT.
  j = LOOKUP(user-print.field-name[i],ipFieldList).
  user-print.field-value[i] = ENTRY(j,ipFieldValue).
END. /* do i */
ELSE DO:
  CREATE user-print.
  ASSIGN
    user-print.company = ipCompany
    user-print.program-id = ipProgramID
    user-print.user-id = USERID('NoSweat')
    j = NUM-ENTRIES(ipFieldList).
  DO i = 1 TO j:
    ASSIGN
      user-print.field-name[i] = ENTRY(i,ipFieldList)
      user-print.field-value[i] = ENTRY(i,ipFieldValue).
  END.
END. /* else */
