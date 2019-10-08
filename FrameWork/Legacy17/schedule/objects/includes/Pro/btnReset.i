/* btnReset.i */

  RUN setScreenStatus.
  RUN msgFrame ('Reset Jobs').
  RUN getScenario.
  RUN jobDowntimeSpan.
  ASSIGN
    buildDowntime = NO
    currentOrder = 0.
  EMPTY TEMP-TABLE ttblJob-do.
  /* DISABLE btnRedo btnUndo WITH FRAME {&FRAME-NAME}. */
  RUN buildBoard (YES).
