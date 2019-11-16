/* jobSelection.i - used in jobSelection in boardProc.i */

  FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA) NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN
  RETURN NO-APPLY.
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  RUN showLightBulb (ttblJob.job).
  IF flashLight THEN
  RUN showFlashLight (ttblJob.job).
  &IF '{&Board}' NE 'Basic' &THEN
  IF btnSave:SENSITIVE IN FRAME {&FRAME-NAME} THEN 
  ENABLE btnToolTip btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
  ELSE 
  DISABLE btnToolTip btnComplete btnJobNotes WITH FRAME {&FRAME-NAME}.
  ASSIGN
    btnComplete:PRIVATE-DATA = STRING(ROWID(ttblJob))
    btnJobNotes:PRIVATE-DATA = STRING(ROWID(ttblJob))
    currentJob = ipWidget:HANDLE
    btnToolTip:TOOLTIP = currentJob:TOOLTIP 
    currentResource = ''
    .
  &ENDIF
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF
