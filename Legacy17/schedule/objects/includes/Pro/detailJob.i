/* detailJob.i */

/*------------------------------------------------------------------------------
  Purpose:     run the detail window persistent
  Parameters:  rowid of job, and rowid's from load process (comma delimited)
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipRowIDs AS CHARACTER NO-UNDO.

  DEFINE VARIABLE yCoord AS INTEGER NO-UNDO.
  
  &IF '{&Board}' EQ 'Pro' &THEN
  IF NOT detailWindow THEN RETURN.
  &ENDIF
  /* IF NOT proOpts[4] THEN
  DO:
    MESSAGE proOptsMsg(4) VIEW-AS ALERT-BOX.
    RETURN.
  END. */
  sharedRowIDs = ipRowIDs.
  IF NOT VALID-HANDLE(jobHandle) THEN
  DO:
    RUN VALUE(findProgram('{&viewers}/',ID,'/jobDetail.w')) PERSISTENT SET jobHandle.
    RUN adm-initialize IN jobHandle.
  END.
  RUN setPopup IN containerHandle (3,jobHandle).
  yCoord = IF popupBottom THEN FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 145 ELSE ?.
  RUN job IN jobHandle (ipRowID,THIS-PROCEDURE,yCoord).
