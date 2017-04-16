/* timeflds.i */

IF AVAILABLE {&FIRST-EXTERNAL-TABLE} THEN
DO WITH FRAME {&FRAME-NAME}:
  {custom/get_time.i
      &field="{&FIRST-EXTERNAL-TABLE}.start_time"
      &hour="start_hour"
      &minute="start_minute"
      &ampm="start_ampm"
  }
  {custom/get_time.i
      &field="{&FIRST-EXTERNAL-TABLE}.end_time"
      &hour="end_hour"
      &minute="end_minute"
      &ampm="end_ampm"
  }
  DISPLAY {&TIME-FIELDS}.
END.
