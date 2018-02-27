IF NOT CAN-FIND(FIRST reftable WHERE
   reftable.reftable EQ "job.close-checked" AND
   reftable.company  EQ STRING({1}.company,"x(10)") +
                        STRING({1}.job,"9999999999")) THEN DO:
  CREATE reftable.
  ASSIGN
   reftable.reftable = "job.close-checked"
   reftable.company  = STRING({1}.company,"x(10)") +
                       STRING({1}.job,"9999999999").
END.
ELSE
   REPEAT:
      FIND FIRST reftable WHERE
           reftable.reftable EQ "job.close-checked" AND
           reftable.company  EQ STRING({1}.company,"x(10)") +
                                STRING({1}.job,"9999999999")
           EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF AVAIL reftable THEN
         LEAVE.
   END.

ASSIGN
 reftable.loc   = USERID("nosweat")
 reftable.code  = STRING(TODAY,"99/99/9999")
 reftable.code2 = STRING(TIME,"99999").