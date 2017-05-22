/* getPriorJobSequence.i - used in procedure getPriorJobSequence in board.i &
                           resourceDetail.w */
/*------------------------------------------------------------------------------
  Purpose:     find a prior seq for a resource
  Parameters:  resource, job sequence and start date & time (yyyymmdd.99999)
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER iopStartDate AS DATE NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER iopStartTime AS INTEGER NO-UNDO.

  DEFINE BUFFER priorJob FOR ttblJob.

  IF ipJobSequence EQ 1 THEN RETURN.
  FIND LAST priorJob NO-LOCK
       WHERE priorJob.resource EQ ipResource
         AND priorJob.jobSequence LT ipJobSequence NO-ERROR.
  IF NOT AVAILABLE priorJob OR
     priorJob.endDateTime LE ipStartDateTime THEN RETURN.
  ASSIGN
    iopStartDate = priorJob.endDate
    iopStartTime = priorJob.endTime.
