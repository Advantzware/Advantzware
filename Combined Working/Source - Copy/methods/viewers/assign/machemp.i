/* machemp.i */

{methods/viewers/assign/timeflds.i}
DO WITH FRAME {&FRAME-NAME}:
  IF machemp.end_date = ? OR machemp.start_date = ? THEN
  machemp.total_time = 0.
  ELSE
  {custom/calctime.i &file="machemp"}
  DISPLAY STRING(machemp.total_time,'HH:MM') WHEN machemp.total_time NE 0
          @ machemp.total_time
          '' WHEN machemp.total_time = 0 @ machemp.total_time.
  DISABLE Btn_Set_Rate.
END.
