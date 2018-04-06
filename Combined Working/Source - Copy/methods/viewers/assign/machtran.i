/* machtran.i */

{methods/viewers/assign/timeflds.i}
DO WITH FRAME {&FRAME-NAME}:
  IF machtran.end_date = ? OR machtran.start_date = ? THEN
  machtran.total_time = 0.
  ELSE
  {custom/calctime.i &file="machtran"}
  DISPLAY STRING(machtran.total_time,'HH:MM') WHEN machtran.total_time NE 0
          @ machtran.total_time
          '' WHEN machtran.total_time = 0 @ machtran.total_time.
END.
RUN updateRouting (machtran.company,machtran.machine,machtran.job_number,
                   machtran.job_sub,machtran.form_number,machtran.blank_number).
