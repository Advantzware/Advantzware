/* machshft.i */

WHEN "fi_shift" THEN DO WITH FRAME {&FRAME-NAME}:
  FIND shifts
      WHERE shifts.company = gcompany
        AND shifts.shift = fi_shift:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  shifts_description = IF NOT AVAILABLE shifts THEN ""
                       ELSE shifts.description.
  DISPLAY shifts_description.
  IF AVAIL shifts THEN DO:
    {custom/get_time2.i
      &field="shifts.start_time"
      &hour="start_hour"
      &minute="start_minute"
      &second="start_second"
      &ampm="start_ampm"
    }
    {custom/get_time2.i
      &field="shifts.end_time"
      &hour="end_hour"
      &minute="end_minute"
      &second="end_second"
      &ampm="end_ampm"
    }
  END.
  DISPLAY {&TIME-FIELDS} .

END.
