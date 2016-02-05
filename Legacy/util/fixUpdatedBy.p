
DEF BUFFER bf-fg-rctd FOR fg-rctd.
FOR EACH fg-rctd WHERE
                CAN-FIND(FIRST reftable
                    WHERE reftable.reftable EQ "fg-rctd.user-id"
                      AND reftable.company  EQ fg-rctd.company
                      AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999"))
                NO-LOCK:
  FIND FIRST reftable
                    WHERE reftable.reftable EQ "fg-rctd.user-id"
                      AND reftable.company  EQ fg-rctd.company
                      AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
                    NO-LOCK NO-ERROR.
  IF AVAIL reftable AND (fg-rctd.created-by NE reftable.code OR fg-rctd.updated-by NE reftable.code2) THEN DO:
    FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ ROWID(fg-rctd)
      EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN
      bf-fg-rctd.created-by = reftable.code
      bf-fg-rctd.updated-by	 = reftable.code2.
    FIND CURRENT bf-fg-rctd NO-LOCK.
    RELEASE bf-fg-rctd.
  END.
    
END.
