/* getDowntime.i */

/*------------------------------------------------------------------------------
  Purpose:     load downtime values from data files
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN msgFrame ('Load Downtimes').
  /* needed for backward compatibility starting in v1.042 */
  IF SEARCH('{&data}/' + ID + '/downtimes.dat') NE ? THEN
  OS-RENAME VALUE(SEARCH('{&data}/' + ID + '/downtimes.dat'))
            VALUE(clientDat + '{&data}/' + ID + '/downtimes.Actual.dat').
  FOR EACH ttblDowntime EXCLUSIVE-LOCK WHERE ttblDowntime.dayID NE 0:
    DELETE ttblDowntime.
  END.
  INPUT FROM VALUE(SEARCH(clientDat + '{&data}/' + ID + '/downtimes.' + scenario + '.dat')) NO-ECHO.
  REPEAT:
    IMPORT tempDowntime.
    /* IF NOT proOpts[3] THEN
    DO:
      MESSAGE proOptsMsg(3) VIEW-AS ALERT-BOX.
      LEAVE.
    END. */
    CREATE ttblDowntime.
    BUFFER-COPY tempDowntime TO ttblDowntime.
    ttblDowntime.startDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.startTime).
    ttblDowntime.endDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.endTime).
  END.
  INPUT CLOSE.
