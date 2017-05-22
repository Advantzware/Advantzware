/* getPriorJobResource.i - used in procedure getPriorResource in board.i &
                           resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     find prior job resource
  Parameters:  job, resource sequence and start date & time (yyyymmdd.99999)
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipResourceSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER iopStartDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopStartTime AS INTEGER NO-UNDO.

  DEFINE BUFFER priorJob FOR ttblJob.
  
  IF ipResourceSequence EQ 1 THEN RETURN.
  FIND LAST priorJob NO-LOCK USE-INDEX resourceSequence
       WHERE priorJob.job EQ ipJob
         AND priorJob.resourceSequence LT ipResourceSequence NO-ERROR.
  IF NOT AVAILABLE priorJob OR priorJob.endDateTime LE ipStartDateTime THEN RETURN.
  ASSIGN
    iopStartDate = priorJob.endDate
    iopStartTime = priorJob.endTime.
  IF priorJob.lagTime NE 0 THEN
  RUN addTime (priorJob.startDate,priorJob.startTime,priorJob.lagTime,
               OUTPUT iopStartDate,OUTPUT iopStartTime).
