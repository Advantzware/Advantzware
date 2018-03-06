/* setCellColumns.i */

DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT 200.
DEFINE VARIABLE columnWidth AS DECIMAL NO-UNDO EXTENT 200.
DEFINE VARIABLE cellColumnDat AS CHARACTER NO-UNDO.

/* create a &SCOPED-DEFINE cellColumnDat value prior to this include
   if another file name is desired to store user cell column order */
&IF DEFINED(cellColumnDat) EQ 0 &THEN
&SCOPED-DEFINE cellColumnDat {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
&ENDIF

cellColumnDat = './users/' + USERID('nosweat') + '/{&cellColumnDat}.dat'.

PROCEDURE setCellColumns:
  DEFINE VARIABLE userColumn AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-index AS INT NO-UNDO.

  IF SEARCH(cellColumnDat) NE ? THEN DO:
     /* get user cell column order */
     INPUT FROM VALUE(cellColumnDat) NO-ECHO.
     REPEAT:
        IMPORT userColumn[j] columnWidth[j].
        j = j + 1.
     END. /* repeat */
     INPUT CLOSE.
     /* change default columns to user order */
     DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
     END.
    
     j = j - 1.
     DO i = 1 TO j:
     
        DO k = 1 TO j:
           IF userColumn[i] EQ cellColumn[k]:NAME THEN
              LEAVE.
        END.

        /* 25841 - handle condition where the column def in the .dat file no longer exists in the browser */
        IF NOT VALID-HANDLE(cellColumn[k]) THEN
            LEAVE.
        /* 25841 - end */
        
        IF columnWidth[i] NE cellColumn[k]:WIDTH-PIXELS THEN
           cellColumn[k]:WIDTH-PIXELS = columnWidth[i].

        IF userColumn[i] NE cellColumn[i]:NAME THEN DO:
    
           {&BROWSE-NAME}:MOVE-COLUMN(k,i) IN FRAME {&FRAME-NAME}.
          
           DO v-index = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
              cellColumn[v-index] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(v-index).
           END.
        END.
     END. /* do i */

  END. /* search */
  /* read new order to check for changes when exiting */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    ASSIGN
      cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
      columnWidth[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS.
  END. /* do i */
END PROCEDURE.

PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  /* check for any columns changes */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    IF cellColumn[i]:NAME EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):NAME AND
       columnWidth[i] EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS THEN NEXT.
    MESSAGE 'Save Column Changes?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE saveChanges AS LOGICAL.
    IF saveChanges THEN DO:
      OUTPUT TO VALUE(cellColumnDat).
      DO j = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        EXPORT {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):NAME {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):WIDTH-PIXELS.
      END. /* do j */
      OUTPUT CLOSE.
    END. /* if savechanges */
    LEAVE.
  END. /* do i */

  &IF DEFINED(xlocal-destroy) &THEN
    RUN xlocal-destroy.
  &ENDIF
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.
