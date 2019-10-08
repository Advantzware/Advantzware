/* emplogin.i */

{methods/viewers/rowavail/timeflds.i}
IF AVAILABLE emplogin THEN
DO WITH FRAME {&FRAME-NAME}:
  DISPLAY STRING(emplogin.total_time,'HH:MM') WHEN emplogin.total_time NE 0
          @ emplogin.total_time
          '' WHEN emplogin.total_time = 0 @ emplogin.total_time.
END.
