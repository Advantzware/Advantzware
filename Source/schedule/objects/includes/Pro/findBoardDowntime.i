/* findBoardDowntime.i - used in procedure jobStacker in boardProc.i */

    DO WHILE TRUE:
      FIND boardDowntime NO-LOCK
          WHERE boardDowntime.resource EQ ipResource
            AND boardDowntime.startDateTime LE lvStartDateTime
            AND boardDowntime.endDateTime GT lvStartDateTime NO-ERROR.
      IF AVAILABLE boardDowntime THEN
      DO:
        ASSIGN
          lvStartDate = boardDowntime.endDate
          lvStartTime = boardDowntime.endTime.
        IF boardDowntime.endTime EQ 86400 THEN
        ASSIGN
          lvStartDate = lvStartDate + 1
          lvStartTime = 0.
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime).
      END. /* if avail */
      ELSE LEAVE.
    END. /* do while */
