/* Methods/viewers/delete/shifts.i */
    DEF BUFFER bf-shift_break FOR shift_break.    

    FOR EACH bf-shift_break WHERE bf-shift_break.company EQ shifts.company
        AND bf-shift_break.shift EQ shifts.shift
      EXCLUSIVE-LOCK:

      DELETE bf-shift_break.

    END.
