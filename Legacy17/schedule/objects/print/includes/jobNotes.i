/* jobNotes.i */

  ASSIGN
    noteRowID = TO-ROWID(ENTRY(2,{&useTable}.rowIDs))
    noteFound = NO
    noteCnt = 0.
  
  FOR EACH jobNotes NO-LOCK WHERE jobNotes.jobRowID EQ noteRowID
                              AND jobNotes.jobStatus EQ NO
                              AND jobNotes.deleteNote EQ NO
                    USE-INDEX rptOrder:
    IF ipExcel THEN DO:
       /* if 0 then need to skip down a line, more than one note exists */
      IF noteCnt NE 0 THEN DO:
        PUT UNFORMATTED SKIP.
        /* fill columns with blanks to pad out for next note */
        DO i = 1 TO excelCnt:
          PUT UNFORMATTED '"",'.
        END. /* do i */
      END. /* if ne 0 */
      PUT UNFORMATTED
        '"' jobNotes.noteDate '",'
        '"' STRING(jobNotes.noteTime,'HH:MM:SSam') '",'
        '"' jobNotes.noteText '"'.
      noteCnt = noteCnt + 1.
    END. /* else */
    ELSE /* not excel show note */
    PUT UNFORMATTED 'Note: ' AT 1 STRING(jobNotes.noteDate) ' @ '
      STRING(jobNotes.noteTime,'HH:MM:SSam') ' - ' jobNotes.noteText SKIP.
    noteFound = YES.
  END. /* each jobNotes */
  
  IF ipExcel THEN PUT UNFORMATTED SKIP.

  IF NOT ipExcel AND skipLine AND noteFound THEN DO:
    PUT UNFORMATTED SKIP(1).
    skipLine = NO.
  END.
