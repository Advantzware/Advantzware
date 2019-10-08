
RELEASE reftable.
IF old-{&TABLENAME}.rec_key NE "" THEN
FIND FIRST reftable
    WHERE reftable.rec_key  EQ old-{&TABLENAME}.rec_key
      AND reftable.reftable EQ "FIBRE AUDIT"
      AND reftable.company  EQ "{&TABLENAME}"
    USE-INDEX rec_key NO-ERROR.
IF NOT AVAIL reftable THEN DO:
  CREATE reftable.
  reftable.reftable = "FIBRE AUDIT".
END.
ASSIGN
 reftable.rec_key = {&TABLENAME}.rec_key
 reftable.company = "{&TABLENAME}"
 reftable.loc     = USERID("nosweat")
 reftable.code    = STRING(TODAY,"99/99/9999")
 reftable.code2   = STRING(TIME,"99999").
