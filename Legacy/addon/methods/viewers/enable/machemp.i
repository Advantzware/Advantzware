/* machemp.i */

{methods/viewers/enable/timeflds.i}
ENABLE Btn_Set_Rate WITH FRAME {&FRAME-NAME}.

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

IF "{&enable-proc}" NE "" THEN RUN {&enable-proc}.
