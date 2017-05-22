/* buildDowntime.i */

/*------------------------------------------------------------------------------
  Purpose:     build downtime widget rectangles
  Parameters:  <none>
  Notes:       3 scenarios: resource based on day of week, or date, or calendar
               wide and day of week
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE xValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE yValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE wValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE spixel AS INTEGER NO-UNDO.
  DEFINE VARIABLE epixel AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  &SCOPED-DEFINE ttblFile boardDowntime
  &SCOPED-DEFINE startDate {&ttblFile}.startDate
  &SCOPED-DEFINE endDate {&ttblFile}.endDate

  RUN msgFrame ('Building Downtime').
  IF NOT buildDowntime THEN
  DO:
    buildDowntime = YES.
    RETURN.
  END.
  downtimeIdx = 0.
  DO i = 1 TO resourceIdx:
    currentWidget = resourceWidget[i].
    FOR EACH boardDowntime NO-LOCK
        WHERE boardDowntime.resource EQ currentWidget:NAME
          AND boardDowntime.startDate GE intDate[1]
          AND boardDowntime.endDate LE intDate[24]:
      {{&includes}/buildInclude.i}
      downtimeIdx = downtimeIdx + 1.
      /* create downtime rectangle */
      RUN createDowntime (downtimeIdx,xValue,yValue,wValue,hpixels).
    END.
  END. /* do i */
  RUN hideDowntime (downtimeIdx).
