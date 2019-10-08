/* jobDeselection.i - used in jobReset in boardProc.i */

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  RUN showLightBulb ('').
  IF flashLight THEN
  RUN showFlashLight ('').
  DISABLE btnToolTip btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
  btnToolTip:TOOLTIP = ''.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF
