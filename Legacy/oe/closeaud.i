
FIND FIRST reftable
    WHERE reftable.reftable EQ "oe-ord.close-checked"
      AND reftable.company  EQ STRING({1}.company,"x(10)") +
                               STRING({1}.ord-no,"9999999999")
    NO-ERROR.
IF NOT AVAIL reftable THEN DO:
  CREATE reftable.
  ASSIGN
   reftable.reftable = "oe-ord.close-checked"
   reftable.company  = STRING({1}.company,"x(10)") +
                       STRING({1}.ord-no,"9999999999").
END.

ASSIGN
 reftable.loc   = USERID("nosweat")
 reftable.code  = STRING(TODAY,"99/99/9999")
 reftable.code2 = STRING(TIME,"99999").
