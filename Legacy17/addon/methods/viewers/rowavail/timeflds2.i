/* timeflds2.i */

IF AVAILABLE {&FIRST-EXTERNAL-TABLE} THEN
DO WITH FRAME {&FRAME-NAME}:
  {custom/get_time2.i
      &field="{&FIRST-EXTERNAL-TABLE}.start_time"
      &hour="start_hour"
      &minute="start_minute"
      &second="start_second"
      &ampm="start_ampm"
  }
  {custom/get_time2.i
      &field="{&FIRST-EXTERNAL-TABLE}.end_time"
      &hour="end_hour"
      &minute="end_minute"
      &second="end_second"
      &ampm="end_ampm"
  }
  DISPLAY {&TIME-FIELDS}.
END.
