/* viewrhlp.i */

ON HELP OF FRAME {&FRAME-NAME}
DO:
  RUN Run_applhelp IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
  RETURN NO-APPLY.
END.
