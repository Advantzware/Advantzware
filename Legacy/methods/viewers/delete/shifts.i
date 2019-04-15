/* Methods/viewers/delete/shifts.i */
    DEF BUFFER bf-reftable for reftable.
    for each bf-reftable WHERE bf-reftable.reftable EQ "ShiftDays"
       AND bf-reftable.CODE EQ shifts.rec_key
       EXCLUSIVE-LOCK:

       DELETE bf-reftable.

    END.