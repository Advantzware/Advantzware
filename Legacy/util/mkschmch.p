/* ESP - not in use anymore
FOR EACH mach NO-LOCK:
  FIND FIRST reftable
      WHERE reftable.reftable EQ "mach.sch-m-code"
        AND reftable.company  EQ mach.company
        AND reftable.loc      EQ mach.loc
        AND reftable.code     EQ mach.m-code
      NO-ERROR.
  IF NOT AVAIL reftable THEN CREATE reftable.
  IF NEW reftable OR reftable.code2 EQ "" THEN
    ASSIGN
     reftable.reftable = "mach.sch-m-code"
     reftable.company  = mach.company
     reftable.loc      = mach.loc
     reftable.code     = mach.m-code
     reftable.code2    = mach.m-code.
END. */
