/* ttblshft.i */

FOR EACH {&table} NO-LOCK {&where}:
  CREATE ttbl_shifts.
  ASSIGN
    ttbl_shifts.shift = {&table}.shift
    ttbl_shifts.real_shift = {&table}.shift
    ttbl_shifts.start_time = {&table}.start_time
    ttbl_shifts.end_time = {&table}.end_time.
  IF {&table}.start_time GT {&table}.end_time THEN
  DO:
    ttbl_shifts.end_time = 86400.
    CREATE ttbl_shifts.
    ASSIGN
      ttbl_shifts.shift = {&table}.shift + {&table}.shift
      ttbl_shifts.real_shift = {&table}.shift
      ttbl_shifts.start_time = 0
      ttbl_shifts.end_time = {&table}.end_time.
  END.
END.
