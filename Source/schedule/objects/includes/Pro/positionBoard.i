/* positionBoard.i - used in procedure positionBoard in boardProc.i */

/*------------------------------------------------------------------------------
  Purpose:     reposition board based on date change from resource viewer
  Parameters:  new date
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipShowBoard AS LOGICAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF boardDate:SCREEN-VALUE NE STRING(ipDate,'99/99/9999') OR ipShowBoard THEN
    DO:
      boardDate:SCREEN-VALUE = STRING(ipDate).
      APPLY 'RETURN' TO boardDate.
    END.
    IF VALID-HANDLE(currentJob) THEN
    currentJob:SELECTED = NO.
    IF ipRowID EQ ? THEN RETURN.
    IF btnResourceList:SENSITIVE THEN
    DO:
      FIND buffJob NO-LOCK WHERE ROWID(buffJob) EQ ipRowID.
      APPLY 'CHOOSE' TO btnResourceList.
      resourceList:SCREEN-VALUE = ENTRY(LOOKUP(buffJob.resource,
                                        resourceList:LIST-ITEM-PAIRS) + 1,
                                        resourceList:LIST-ITEM-PAIRS).
      APPLY 'DEFAULT-ACTION' TO resourceList.
    END.
    FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ ipRowID.
    {{&includes}/ttblWidgetFind.i "jobWidget" ttblJob.widgetIdx}
    jobWidget.jobWidget:SELECTED = YES.
    APPLY 'SELECTION' TO jobWidget.jobWidget.
  END.
