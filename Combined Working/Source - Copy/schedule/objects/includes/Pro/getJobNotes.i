/* getJobNotes.i - used in procedure getJobNotes in Pro & View versions */

  DEFINE VARIABLE lvJobRowID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvNoteDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvNoteTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvNoteText AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvNoteKey AS CHARACTER NO-UNDO.

  RUN msgFrame ('Load Job Notes').
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/jobNotes.dat')) NO-ECHO.
  REPEAT:
    IMPORT lvJobRowID lvNoteDate lvNoteTime lvNoteText lvNoteKey.
    IF CAN-FIND(FIRST jobNotes WHERE jobNotes.jobRowID EQ TO-ROWID(lvJobRowID)
                                 AND jobNotes.noteDate EQ lvNoteDate
                                 AND jobNotes.noteTime EQ lvNoteTime) THEN NEXT.
    CREATE jobNotes.
    ASSIGN
      jobNotes.jobRowID = TO-ROWID(lvJobRowID)
      jobNotes.noteDate = lvNoteDate
      jobNotes.noteTime = lvNoteTime
      jobNotes.noteText = lvNoteText
      jobNotes.noteKey = lvNoteKey.
  END.
  INPUT CLOSE.
