/* buildJob.i */

/* noteIcon */
IF intervals:LOOKUP(intervals:SCREEN-VALUE) LE noteButtons AND
   xValue LT FRAME {&FRAME-NAME}:WIDTH-PIXELS - 14 THEN DO:
  noteIdx = noteIdx + 1.
  RUN createNote (intervals:LOOKUP(intervals:SCREEN-VALUE) LE lockButtons AND
                  xValue LT FRAME {&FRAME-NAME}:WIDTH-PIXELS - 14,
                  noteIdx,xValue,yValue,ROWID(ttblJob),
                  CAN-FIND(FIRST jobNotes WHERE jobNotes.jobRowID EQ TO-ROWID(ENTRY(2,ttblJob.rowIDs))
                                            AND jobNotes.jobStatus EQ NO
                                            AND jobNotes.deleteNote EQ NO)).
END.
