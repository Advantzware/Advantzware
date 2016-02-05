/* unknownResource.i - used in procedure unknownResource in boardProc.i */

/*------------------------------------------------------------------------------
  Purpose:     move any jobs with unknown (?) resource values, result of move
               existing
  Parameters:  start date & time (yyyymmdd.99999)
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE continue AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE svEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE svEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE svEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE yy AS INTEGER NO-UNDO.
  DEFINE VARIABLE mm AS INTEGER NO-UNDO.
  DEFINE VARIABLE dd AS INTEGER NO-UNDO.
  
  IF CAN-FIND(FIRST bufferJob WHERE bufferJob.resource EQ ?) THEN
  FOR EACH bufferJob EXCLUSIVE-LOCK
      WHERE bufferJob.resource EQ ? BY bufferJob.resourceSequence:
    ASSIGN
      bufferJob.resource = bufferJob.origResource
      bufferJob.origResource = ''
      lvStartDateTime = ipStartDateTime
      yy = INTEGER(SUBSTR(STRING(ipStartDateTime),1,4))
      mm = INTEGER(SUBSTR(STRING(ipStartDateTime),5,2))
      dd = INTEGER(SUBSTR(STRING(ipStartDateTime),7,2))
      lvStartTime = INTEGER(SUBSTR(STRING(ipStartDateTime),10))
      lvStartDate = DATE(mm,dd,yy).
    RUN newEnd (bufferJob.timeSpan,lvStartDate,lvStartTime,
                OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
      svEndDateTime = lvEndDateTime
      svEndDate = lvEndDate
      svEndTime = lvEndTime.
    RUN firstAvailable (bufferJob.resource,ROWID(bufferJob),bufferJob.timeSpan,
                        INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                        INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                        INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
    IF lvStartDate EQ ? THEN
    DO:
      ASSIGN
        lvStartDate = svEndDate
        lvStartTime = svEndTime
        lvStartDateTime = svEndDateTime.
      RUN newEnd (bufferJob.timeSpan,lvStartDate,lvStartTime,
                  OUTPUT lvEndDate,OUTPUT lvEndTime).
      lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
    END.
    RUN downtimeSpan (bufferJob.resource,bufferJob.timeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
    RUN jobMoveHistory (ROWID(bufferJob),lvStartDate,lvStartTime,
                        lvEndDate,lvEndTime,bufferJob.jobLocked,lvDowntimeSpan).
    RUN packBoard. /*('Job,,' + bufferJob.job + ',' + STRING(lvStartDate) + ',' +
                      STRING(lvStartTime) + ',' + STRING(ROWID(bufferJob)),
                      OUTPUT continue)*/
  END.
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.
