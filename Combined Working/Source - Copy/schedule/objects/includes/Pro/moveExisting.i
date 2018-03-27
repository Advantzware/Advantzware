/* moveExisting.i - used in procedure moveExisting in boardProc.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     move any existing job from under a newly moved job
  Parameters:  resource, start date & time, end date & time, job rowid & job seq
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDateTime AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipJobSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipConflict AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardDatePrompt AS LOGICAL NO-UNDO.

  DEFINE VARIABLE startSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE endSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE addSequence AS INTEGER NO-UNDO.

  RUN newJobSequence (ipRowID,ipResource,ipStartDateTime,
                      ipJobSequence,ipConflict,ipBoardDatePrompt,
                      OUTPUT startSequence,OUTPUT endSequence,OUTPUT addSequence).
  RUN adjustSequence (ipRowID,ipResource,startSequence,endSequence,addSequence).
  RUN setJobPriority.
  RUN setJobDateTime (ipRowID,ipBoardDatePrompt).
