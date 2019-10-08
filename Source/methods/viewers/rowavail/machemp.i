/* machemp.i */

{methods/viewers/rowavail/timeflds.i}
IF AVAILABLE machemp THEN
DO WITH FRAME {&FRAME-NAME}:
  DISPLAY STRING(machemp.total_time,'HH:MM') WHEN machemp.total_time NE 0
          @ machemp.total_time
          '' WHEN machemp.total_time = 0 @ machemp.total_time.
END.

{custom\viewrate.i}
DO:
  assign machemp.rate:HIDDEN = no.
  ENABLE Btn_Set_Rate WITH FRAME {&FRAME-NAME}.
END.
ELSE
DO:
  assign machemp.rate:HIDDEN = yes.
  DISABLE Btn_Set_Rate WITH FRAME {&FRAME-NAME}.
END.
