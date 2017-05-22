/* Methods/viewers/delete/shifts.i */
    DEF BUFFER bf-reftable for reftable.
    DEF BUFFER bf-shift_break FOR shift_break.
    for each bf-reftable WHERE bf-reftable.reftable EQ "ShiftDays"
       AND bf-reftable.CODE EQ shifts.rec_key
       EXCLUSIVE-LOCK:

       DELETE bf-reftable.

    END.

    FOR EACH bf-shift_break WHERE bf-shift_break.company EQ shifts.company
        AND bf-shift_break.shift EQ shifts.shift
      EXCLUSIVE-LOCK:

      DELETE bf-shift_break.

    END.
