
DEF VAR li-coloraud AS INT NO-UNDO.

RELEASE reftable.
IF old-{&TABLENAME}.rec_key NE "" THEN
FIND FIRST reftable
    WHERE reftable.rec_key  EQ old-{&TABLENAME}.rec_key
      AND reftable.reftable EQ "COLOR AUDIT"
      AND reftable.company  EQ "{&TABLENAME}"
    USE-INDEX rec_key NO-ERROR.
IF NOT AVAIL reftable THEN DO:
  CREATE reftable.
  ASSIGN
   reftable.rec_key  = {&TABLENAME}.rec_key
   reftable.reftable = "COLOR AUDIT"
   reftable.company  = "{&TABLENAME}"
   reftable.loc      = USERID("nosweat")
   reftable.code     = STRING(TODAY,"99/99/9999")
   reftable.code2    = STRING(TIME,"99999").

  DO li-coloraud = 1 TO 100:
    IF TRIM(PROGRAM-NAME(li-coloraud)) NE "" AND
       TRIM(PROGRAM-NAME(li-coloraud)) NE ?  THEN
      reftable.dscr = reftable.dscr                   +
                      TRIM(PROGRAM-NAME(li-coloraud)) +
                      "," NO-ERROR.
    IF ERROR-STATUS:ERROR THEN LEAVE.
  END.
END.

