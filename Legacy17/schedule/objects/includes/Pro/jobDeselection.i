/* jobDeselection.i - used in jobReset in boardProc.i */

  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  RUN showLightBulb ('').
  IF flashLight THEN
  RUN showFlashLight ('').
  DISABLE btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
  RUN LockWindowUpdate (0,OUTPUT i).
