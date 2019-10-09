/* localChangePage.i - used in procedure local-change-page in schedule.w */

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  currentPage = INTEGER(RETURN-VALUE).
  IF currentPage EQ 1 THEN
  DO:
    ENABLE btnAutoMonitor {&boardButtons} WITH FRAME {&FRAME-NAME}.
    RUN setMenuItems (YES).
    btnDataCollection:HIDDEN = NOT CONNECTED('emptrack').
  END.
  ELSE
  DO:
    HIDE btnAutoMonitor {&boardButtons} NO-PAUSE.
    RUN setMenuItems (NO).
  END.
