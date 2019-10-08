
FIND FIRST op-lock
    WHERE op-lock.reftable EQ "est.op-lock"
      AND op-lock.company  EQ {1}.company
      AND op-lock.loc      EQ {1}.loc
      AND op-lock.code     EQ TRIM({1}.est-no)
    NO-ERROR.
