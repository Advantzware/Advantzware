/* showFlashLight.i - used in procedure showFlashLight in boardProc.i */

/*------------------------------------------------------------------------------
  Purpose:     show/hide flashLight color
  Parameters:  job value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE saveRowID AS ROWID NO-UNDO.

  IF AVAILABLE ttblJob THEN
  saveRowID = ROWID(ttblJob).
  FOR EACH jobWidget NO-LOCK WHERE jobWidget.isSelected EQ YES:
    FOR EACH ttblJob EXCLUSIVE-LOCK WHERE ttblJob.widgetIdx EQ jobWidget.idx:
      ASSIGN
        ttblJob.jobBGColor = jobBGColor()
        ttblJob.jobFGColor = jobFGColor()
        ttblJob.statusLabel = jobStatus()
        jobWidget.jobBGColor = ttblJob.jobBGColor
        jobWidget.jobFGColor = ttblJob.jobFGColor
        .
    END.
  END. /* each jobWidget */
  FOR EACH jobWidget NO-LOCK WHERE jobWidget.idx LE jobIdx:
    IF jobWidget.jobWidget:NAME EQ ipJob OR
       jobWidget.jobWidget:BGCOLOR NE jobWidget.jobBGColor THEN
    ASSIGN
      jobWidget.isSelected = jobWidget.jobWidget:NAME EQ ipJob
      jobWidget.jobWidget:BGCOLOR = IF jobWidget.jobWidget:NAME EQ ipJob THEN flashLightColor
                                    ELSE jobWidget.jobBGColor
      jobWidget.jobWidget:FGCOLOR = IF jobWidget.jobWidget:NAME EQ ipJob THEN 0
                                    ELSE jobWidget.jobFGColor
      .
    IF jobWidget.jobWidget:NAME EQ ipJob THEN
    DO WITH FRAME {&FRAME-NAME}:
      jobWidget.jobWidget:MOVE-TO-TOP().
      &IF '{&Board}' EQ 'Pro' &THEN
      IF intervals:LOOKUP(intervals:SCREEN-VALUE) LE lockButtons THEN DO:
        {{&includes}/ttblWidgetFind.i "lockWidget" jobWidget.idx}
        lockWidget.lockWidget:MOVE-TO-TOP().
      END.
      &ENDIF
      &IF '{&Board}' NE 'Basic' &THEN
      /* noteIcon */
      IF intervals:LOOKUP(intervals:SCREEN-VALUE) LE noteButtons THEN DO:
        {{&includes}/ttblWidgetFind.i "noteWidget" jobWidget.idx}
        noteWidget.noteWidget:MOVE-TO-TOP().
      END.
      &ENDIF
    END.
  END. /* each jobWidget */
  IF saveRowID NE ? THEN
  FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ saveRowID.
