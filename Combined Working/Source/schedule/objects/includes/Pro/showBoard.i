/* showBoard.i - used in procedure showBoard */

  DO WITH FRAME {&FRAME-NAME}:
    IF intervals:LOOKUP(intervals:SCREEN-VALUE) LE lockButtons THEN
    {{&includes}/ttblWidgetShow.i "lockWidget" lockIdx NO}

    IF intervals:LOOKUP(intervals:SCREEN-VALUE) LE noteButtons THEN
    {{&includes}/ttblWidgetShow.i "noteWidget" noteIdx NO} /* noteIcon */

    IF VALID-HANDLE(capacityViewHandle) THEN
    RUN reopenBrowse IN capacityViewHandle.

    IF VALID-HANDLE(jobBrowseHandle) THEN
    RUN reopenBrowse IN jobBrowseHandle.

    IF VALID-HANDLE(jobSequencerHandle) THEN
    RUN reopenBrowse IN jobSequencerHandle.

    IF VALID-HANDLE(moveResourceHandle) THEN
    RUN reopenBrowse IN moveResourceHandle.

    IF VALID-HANDLE(pendingHandle) THEN
    RUN reopenBrowse IN pendingHandle.

    IF VALID-HANDLE(pendingJobsHandle) THEN
    RUN reopenBrowse IN pendingJobsHandle.

    IF VALID-HANDLE(resourceHandle) THEN
    RUN reopenBrowse IN resourceHandle.
  END.
