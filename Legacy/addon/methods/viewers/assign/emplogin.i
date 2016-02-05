/* emplogin.i */

{methods/viewers/assign/timeflds.i}
DO WITH FRAME {&FRAME-NAME}:
  IF emplogin.end_date = ? OR emplogin.start_date = ? THEN
  emplogin.total_time = 0.
  ELSE
  {custom/calctime.i &file="emplogin"}
  DISPLAY STRING(emplogin.total_time,'HH:MM') WHEN emplogin.total_time NE 0
          @ emplogin.total_time
          '' WHEN emplogin.total_time = 0 @ emplogin.total_time.
END.
