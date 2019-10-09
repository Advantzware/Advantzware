/* po/pouserid.i */
FIND FIRST reftable WHERE reftable.reftable EQ "POUserid"
                      AND reftable.company  EQ cocode
                      AND reftable.loc      EQ STRING(po-ord.po-no)
                      NO-LOCK NO-ERROR.
IF NOT AVAIL reftable THEN
   FIND FIRST reftable WHERE reftable.reftable EQ "POUserid"
                      AND reftable.company  EQ cocode
                      AND reftable.loc      EQ STRING(po-ord.po-no)
                      AND reftable.code = USERID('nosweat')
                      NO-LOCK NO-ERROR.
IF NOT AVAIL reftable THEN DO :
   CREATE reftable.
   ASSIGN reftable.reftable = "POUserid"
          reftable.company = cocode
          reftable.loc = STRING(po-ord.po-no)
          reftable.code = USERID('nosweat').
END.
