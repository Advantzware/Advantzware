/* setUserPrint.p */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipProgramID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFieldList AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipFieldValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEF VAR v-userid AS CHAR NO-UNDO.

v-userid = USERID('NoSweat').

IF CAN-FIND(FIRST user-print WHERE
   user-print.company EQ ipCompany AND
   user-print.program-id EQ ipProgramID AND
   user-print.user-id EQ v-userid AND
   user-print.batch EQ '') THEN
   DO:
      REPEAT:
         FIND FIRST user-print WHERE 
              user-print.company EQ ipCompany AND
              user-print.program-id EQ ipProgramID AND
              user-print.user-id EQ v-userid AND
              user-print.batch EQ ''
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

         IF AVAIL user-print THEN
            LEAVE.
      END.

      DO i = 1 TO EXTENT(user-print.field-name):
         IF NOT CAN-DO(ipFieldList,user-print.field-name[i]) THEN
            NEXT.
     
         ASSIGN
            j = LOOKUP(user-print.field-name[i],ipFieldList)
            user-print.field-value[i] = ENTRY(j,ipFieldValue).
      END. /* do i */

      FIND CURRENT user-print NO-LOCK NO-ERROR.
   END.

ELSE DO:
   CREATE user-print.
   ASSIGN
      user-print.company = ipCompany
      user-print.program-id = ipProgramID
      user-print.user-id = v-userid
      j = NUM-ENTRIES(ipFieldList).
   DO i = 1 TO j:
      ASSIGN
         user-print.field-name[i] = ENTRY(i,ipFieldList)
         user-print.field-value[i] = ENTRY(i,ipFieldValue).
   END.
END. /* else */
