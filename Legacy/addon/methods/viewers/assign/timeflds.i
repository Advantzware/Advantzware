/* timeflds.i */

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN {&TIME-FIELDS}.
  {custom/set_time.i
      &field="{&FIRST-EXTERNAL-TABLE}.start_time"
      &hour="start_hour"
      &minute="start_minute"
      &ampm="start_ampm"
  }
  {custom/set_time.i
      &field="{&FIRST-EXTERNAL-TABLE}.end_time"
      &hour="end_hour"
      &minute="end_minute"
      &ampm="end_ampm"
  }
  DISABLE {&TIME-FIELDS}.
END.
