
CREATE op-lock.
ASSIGN
 op-lock.reftable = "est.op-lock"
 op-lock.company  = {1}.company
 op-lock.loc      = {1}.loc
 op-lock.code     = TRIM({1}.est-no).
