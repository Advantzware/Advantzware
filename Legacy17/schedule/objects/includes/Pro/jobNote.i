/* jobNote.i - used in procedure jobNote for Pro & View versions */

  DEFINE INPUT PARAMETER ipWidget AS WIDGET-HANDLE NO-UNDO.

  DEFINE VARIABLE noteExists AS LOGICAL NO-UNDO.

  RUN {&prompts}/jobNotes.w (TO-ROWID(ipWidget:PRIVATE-DATA),'{&Board}').
  FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ TO-ROWID(ipWidget:PRIVATE-DATA) NO-ERROR.
  IF AVAILABLE ttblJob THEN
  ASSIGN
    noteExists = CAN-FIND(FIRST jobNotes WHERE jobNotes.jobRowID EQ TO-ROWID(ENTRY(2,ttblJob.rowIDs))
                                           AND jobNotes.jobStatus EQ NO
                                           AND jobNotes.deleteNote EQ NO)
    ldummy = ipWidget:LOAD-IMAGE-UP(IF noteExists THEN '{&images}/noteTack.bmp' ELSE '{&images}/noteTackRed.bmp')
    ldummy = ipWidget:MOVE-TO-TOP().
