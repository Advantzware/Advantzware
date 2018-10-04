/* getResources.i - used in procedure getResources in downtime.w, pendingJobs &
                    findJob.w */

  DEFINE VARIABLE resource AS CHARACTER NO-UNDO.

  resources:LIST-ITEMS IN FRAME {&FRAME-NAME} = resources:ENTRY(1).
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/resources.dat')) NO-ECHO.
  REPEAT WITH FRAME {&FRAME-NAME}:
    IMPORT ^ resource.
    IF CAN-DO(resources:LIST-ITEMS,resource) THEN NEXT.
    resources:ADD-LAST(resource).
  END.
  INPUT CLOSE.
  resources:SCREEN-VALUE = resources:ENTRY(1).
