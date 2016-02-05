    
    DO lj = 1 TO 2:
      FOR EACH reftable
          WHERE reftable.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING(lj - 1,">"))
            AND reftable.company  EQ eb.company
            AND reftable.loc      EQ eb.est-no
            AND reftable.code     EQ STRING(eb.form-no,"9999999999")
            AND reftable.code2    EQ STRING(eb.blank-no,"9999999999")
          NO-LOCK:

        FIND FIRST b-ref
            WHERE b-ref.reftable EQ reftable.reftable
              AND b-ref.company  EQ b-eb.company
              AND b-ref.loc      EQ b-eb.est-no
              AND b-ref.code     EQ STRING(b-eb.form-no,"9999999999")
              AND b-ref.code2    EQ STRING(b-eb.blank-no,"9999999999")
            NO-ERROR.
        IF NOT AVAIL b-ref THEN DO:
          CREATE b-ref.
          ASSIGN
           b-ref.reftable = reftable.reftable
           b-ref.company  = b-eb.company
           b-ref.loc      = b-eb.est-no
           b-ref.code     = STRING(b-eb.form-no,"9999999999")
           b-ref.code2    = STRING(b-eb.blank-no,"9999999999").
        END.

        DO li = 1 TO 12:
          b-ref.val[li] = reftable.val[li].
        END.

        b-ref.dscr = reftable.dscr.

        LEAVE.
      END.
    END.
