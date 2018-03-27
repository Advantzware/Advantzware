/* shiftJobs.i - used in procedure shiftJobs in boardProc.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     shift jobs in latter sequence after job move
  Parameters:  resource, start date & time, end date & time, job rowid
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipConflict AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardDatePrompt AS LOGICAL NO-UNDO.

  DEFINE VARIABLE startSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE endSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE addSequence AS INTEGER NO-UNDO.

  RUN shiftJobSequence (ipRowID,ipResource,ipStartDateTime,ipConflict,ipBoardDatePrompt).
  RUN setJobPriority.
  RUN setJobDateTime (ipRowID,ipBoardDatePrompt).
