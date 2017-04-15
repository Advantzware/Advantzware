/* machtran.i */

{methods/viewers/rowavail/timeflds.i}
IF AVAILABLE machtran THEN
DO WITH FRAME {&FRAME-NAME}:
  DISPLAY STRING(machtran.total_time,'HH:MM') WHEN machtran.total_time NE 0
          @ machtran.total_time
          '' WHEN machtran.total_time = 0 @ machtran.total_time.
END.
