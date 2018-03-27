/* jobEndMove.i - used in procedure jobEndMove in pro/boardProc.i &
                  procedure updateJob in resourceDetail.w */

    DEFINE VARIABLE lvBoardDatePrompt AS LOGICAL NO-UNDO.

    lvBoardDatePrompt = {2}.
    IF invalidMove(ttblJob.job,ttblJob.resourceSequence,
                   lvStartDateTime,ROWID(ttblJob)) THEN
    DO:
      MESSAGE 'Job Can Not Be Moved Before Prior Resource Sequence!' VIEW-AS ALERT-BOX TITLE 'INVALID MOVE'.
      RUN jobReset ({1}).
      RETURN.
    END.
    ELSE
    IF ttblJob.jobLocked THEN
    DO:
      MESSAGE 'Locked Job Can Not Be Moved!' VIEW-AS ALERT-BOX TITLE 'INVALID MOVE'.
      RUN jobReset ({1}).
      RETURN.
    END.

    IF checkJobConflict(checkResource,lvStartDateTime,lvEndDateTime,ROWID(ttblJob)) OR
       checkDowntimeConflict(checkResource,lvStartDateTime,lvEndDateTime) THEN
    DO WHILE TRUE:
      conflictChoice = ''.

      IF checkJobConflict(checkResource,lvStartDateTime,lvEndDateTime,ROWID(ttblJob)) THEN
      DO:
        IF jobBlock THEN
        DO:
          MESSAGE 'Conflict with Existing Job!' VIEW-AS ALERT-BOX TITLE 'MOVE BLOCKED'.
          RUN jobReset ({1}).
          RETURN.
        END.
        ELSE
        IF jobWarning THEN
        MESSAGE 'Conflict with Existing Job!' VIEW-AS ALERT-BOX TITLE 'WARNING'.
        ELSE
        IF jobPrompt THEN
        DO:
          RUN VALUE('{&prompts}/jobConflict.w') (OUTPUT conflictChoice).
          IF conflictChoice NE '' THEN
          CASE conflictChoice:
            WHEN 'FirstAvailable' THEN /* job conflict */
            DO:
              RUN firstAvailable (checkResource,ROWID(ttblJob),ttblJob.timeSpan,
                                  INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                                  INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                                  INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
              lvBoardDatePrompt = YES.
              NEXT.
            END.
            WHEN 'MoveExisting' THEN /* job conflict */
            DO:
              RUN moveExisting (checkResource,lvStartDateTime,lvEndDateTime,
                                ROWID(ttblJob),ttblJob.jobSequence,YES,lvBoardDatePrompt).
              LEAVE.
            END.
            WHEN 'ShiftJobs' THEN /* job conflict */
            DO:
              RUN shiftJobs (checkResource,lvStartDateTime,lvEndDateTime,
                             ROWID(ttblJob),YES,lvBoardDatePrompt).
              LEAVE.
            END.
            WHEN 'LeaveConflict' THEN
            DO:
              ASSIGN
                ttblJob.startDate = lvStartDate
                ttblJob.startTime = lvStartTime
                ttblJob.endDate = lvEndDate
                ttblJob.endTime = lvEndTime
                ttblJob.startDateTime = lvStartDateTime
                ttblJob.endDateTime = lvEndDateTime
                ttblJob.downtimeSpan = lvDowntimeSpan
                ttblJob.lagTime = lvLagTime.
              LEAVE.
            END.
            WHEN 'CancelMove' THEN
            DO:
              RUN jobReset ({1}).
              RETURN.
            END.
          END CASE.
        END. /* if jobprompt */
      END. /* if checkJobConflict */

      IF checkDowntimeConflict(checkResource,lvStartDateTime,lvEndDateTime) THEN
      DO:
        IF downtimeBlock THEN
        DO:
          MESSAGE 'Conflict with Downtime!' VIEW-AS ALERT-BOX TITLE 'MOVE BLOCKED'.
          RUN jobReset ({1}).
          RETURN.
        END.
        ELSE
        IF downtimeWarning THEN
        MESSAGE 'Conflict with Downtime!' VIEW-AS ALERT-BOX TITLE 'WARNING'.
        ELSE
        IF downtimePrompt THEN
        DO:
          RUN VALUE('{&prompts}/downtimeConflict.w') (OUTPUT conflictChoice).
          IF conflictChoice NE '' THEN
          CASE conflictChoice:
            WHEN 'AfterAvailable' THEN /* downtime conflict */
            DO:
              RUN afterDowntime (checkResource,ttblJob.timeSpan,
                                 INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                                 INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                                 INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
              NEXT.
            END.
            WHEN 'PriorAvailable' THEN /* downtime conflict */
            DO:
              RUN beforeDowntime (checkResource,ROWID(ttblJob),ttblJob.timeSpan,
                                  INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                                  INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                                  INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
              NEXT.
            END.
            WHEN 'LeaveConflict' THEN
            DO:
              RUN moveExisting (checkResource,lvStartDateTime,lvEndDateTime,
                                ROWID(ttblJob),ttblJob.jobSequence,NO,lvBoardDatePrompt).
              LEAVE.
            END.
            WHEN 'CancelMove' THEN
            DO:
              RUN jobReset ({1}).
              RETURN.
            END.
          END CASE.
        END. /* if downtimeprompt */
      END. /* if checkDowntimeConflict */
      ELSE
      IF conflictChoice NE 'LeaveConflict' THEN
      RUN moveExisting (checkResource,lvStartDateTime,lvEndDateTime,
                        ROWID(ttblJob),ttblJob.jobSequence,NO,lvBoardDatePrompt).
      LEAVE.
    END. /* while true */
    ELSE
    DO:
      MESSAGE 'Shift Sequence of Jobs?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE shiftJobs AS LOGICAL.
      IF shiftJobs THEN
      DO: 
        lvBoardDatePrompt = YES.
        RUN shiftJobs (checkResource,lvStartDateTime,lvEndDateTime,
                       ROWID(ttblJob),YES,lvBoardDatePrompt).
      END. /* if shiftjobs */
    END. /* else while true */
