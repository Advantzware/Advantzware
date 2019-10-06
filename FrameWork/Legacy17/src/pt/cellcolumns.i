/* cellColumns.i */

&SCOPED-DEFINE maxColumns 200

DEFINE VARIABLE cellName AS CHARACTER NO-UNDO EXTENT {&maxColumns}.
DEFINE VARIABLE cellWidth AS DECIMAL NO-UNDO EXTENT {&maxColumns}.

PROCEDURE setCellColumns:
  DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT {&maxColumns}.
  DEFINE VARIABLE userName AS CHARACTER NO-UNDO EXTENT {&maxColumns}.
  DEFINE VARIABLE userWidth AS DECIMAL NO-UNDO EXTENT {&maxColumns}.
  DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  /* save original settings in order to do default restore */
  numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
  DO i = 1 TO numColumns:
    ASSIGN
      cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
      cellName[i] = cellColumn[i]:NAME
      cellWidth[i] = cellColumn[i]:WIDTH-PIXELS.
  END. /* do i */
  /* find the unique program and user record holding column settings */
  /* column field-name,column width,column field-name,column width, ... */
  FIND FIRST z_join-value NO-LOCK
       WHERE z_join-value.file-name EQ ENTRY(2,PROGRAM-NAME(1),' ')
         AND z_join-value.key-list EQ USERID('ptdb1') NO-ERROR.
  IF AVAILABLE z_join-value THEN DO:
    i = 0.
    /* get user cell column order and size */
    DO j = 1 TO NUM-ENTRIES(z_join-value.rec-image) - 1 BY 2:
      ASSIGN
        i = i + 1
        userName[i] = ENTRY(j,z_join-value.rec-image)
        userWidth[i] = DECIMAL(ENTRY(j + 1,z_join-value.rec-image)).
    END. /* do j */
    /* change default columns to user order and size */
    DO i = 1 TO numColumns:
      cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
      DO j = 1 TO numColumns:
        /* check if any column order or size changes exist */
        IF cellColumn[i]:NAME EQ userName[j] AND (i NE j OR
           cellColumn[i]:WIDTH-PIXELS NE userWidth[j]) THEN DO:
          cellColumn[i]:WIDTH-PIXELS = userWidth[j]. /* set column size */
          IF i NE j THEN /* move columns */
          {&BROWSE-NAME}:MOVE-COLUMN(i,j) IN FRAME {&FRAME-NAME}.
          /* backup one and recheck, so we don't skip anything */
          IF i NE 1 THEN i = i - 1.
        END. /* if changed */
      END. /* do j */
    END. /* do i */
  END. /* if avail */
END PROCEDURE.

PROCEDURE saveCellColumns:
  DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  /* find the unique program and user record holding column settings */
  /* column field-name,column width,column field-name,column width, ... */
  FIND FIRST z_join-value EXCLUSIVE-LOCK
       WHERE z_join-value.file-name EQ ENTRY(2,PROGRAM-NAME(1),' ')
         AND z_join-value.key-list EQ USERID('ptdb1') NO-ERROR.
  IF NOT AVAILABLE z_join-value THEN DO:
    CREATE z_join-value.
    ASSIGN
      z_join-value.file-name = ENTRY(2,PROGRAM-NAME(1),' ')
      z_join-value.key-list = USERID('ptdb1').
  END. /* if not avail */
  ASSIGN
    numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}
    z_join-value.rec-image = ''.
  DO i = 1 TO numColumns:
    z_join-value.rec-image = z_join-value.rec-image +
      {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):NAME + ',' +
      STRING({&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS) + ','.
  END. /* do i */
  RELEASE z_join-value.
  MESSAGE 'Browser Changes Saved!' VIEW-AS ALERT-BOX.
END PROCEDURE.

PROCEDURE restoreCellColumns:
  DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT {&maxColumns}.
  DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  
  FIND FIRST z_join-value EXCLUSIVE-LOCK
       WHERE z_join-value.file-name EQ ENTRY(2,PROGRAM-NAME(1),' ')
         AND z_join-value.key-list EQ USERID('ptdb1') NO-ERROR.
  IF AVAILABLE z_join-value THEN DELETE z_join-value.
  /* change columns back to default settings */
  numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
  DO i = 1 TO numColumns:
    cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
    DO j = 1 TO numColumns:
      IF cellColumn[i]:NAME EQ cellName[j] AND (i NE j OR
         cellColumn[i]:WIDTH-PIXELS NE cellWidth[j]) THEN DO:
        cellColumn[i]:WIDTH-PIXELS = cellWidth[j].
        IF j NE i THEN
        {&BROWSE-NAME}:MOVE-COLUMN(i,j) IN FRAME {&FRAME-NAME}.
        /* backup one and recheck, so we don't skip anything */
        IF i NE 1 THEN i = i - 1.
      END. /* if changed */
    END. /* do j */
  END. /* do i */
  MESSAGE 'Browser Defaults Restored.' VIEW-AS ALERT-BOX.
END PROCEDURE.
