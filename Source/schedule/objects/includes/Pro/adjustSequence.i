/* adjustSequence.i - used in procedure adjustSequence in boardProc.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:     adjust job sequence values when job moved
  Parameters:  rowid, resource, starting sequence, ending sequence & adder value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipEndSequence AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipAddSequence AS INTEGER NO-UNDO.

  FOR EACH buffJob EXCLUSIVE-LOCK
      WHERE buffJob.resource EQ ipResource
        AND buffJob.jobSequence GE ipStartSequence
        AND buffJob.jobSequence LE ipEndSequence
        AND ROWID(buffJob) NE ipRowID BY buffJob.jobSequence DESCENDING:
    buffJob.jobSequence = buffJob.jobSequence + ipAddSequence.
  END. /* each buffjob */
