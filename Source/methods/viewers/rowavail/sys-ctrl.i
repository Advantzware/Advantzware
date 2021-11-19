/* sys-ctrl.i */

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
    InActiveMessage:HIDDEN = sys-ctrl.isActive.
    InActiveMessage:SCREEN-VALUE = "This System Control Parameter is NO Longer Valid and has been converted to NK6 System Configurations."
    .
END.
