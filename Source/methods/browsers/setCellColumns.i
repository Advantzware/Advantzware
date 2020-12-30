/* setCellColumns.i */

DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT 200.
DEFINE VARIABLE columnWidth AS DECIMAL NO-UNDO EXTENT 200.
DEFINE VARIABLE cellColumnDat AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAutoSave AS LOGICAL NO-UNDO.

/* create a &SCOPED-DEFINE cellColumnDat value prior to this include
   if another file name is desired to store user cell column order */
&IF DEFINED(cellColumnDat) EQ 0 &THEN
&SCOPED-DEFINE cellColumnDat {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
&ENDIF

cellColumnDat = './users/' + USERID('ASI') + '/{&cellColumnDat}.dat'.



/* **********************  Internal Procedures  *********************** */


PROCEDURE pmoveDatFileToDb PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Move data of .DAT file into database.
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount        AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE lUserResponse AS LOGICAL.
  
  DEFINE BUFFER bf-userColumn FOR userColumn.

  FIND FIRST users NO-LOCK 
       WHERE users.user_id  EQ USERID("ASI")
         AND users.isActive EQ YES 
       NO-ERROR.
  IF AVAILABLE users THEN DO:
     RUN displayMessageQuestion (
         INPUT  "59",
         OUTPUT lUserResponse
         ). 
        
     IF lUserResponse THEN DO:
        INPUT FROM VALUE(cellColumnDat) NO-ECHO.
        MAINLOOP:
        DO TRANSACTION:
            REPEAT ON ERROR UNDO MAINLOOP, LEAVE MAINLOOP:           
                CREATE bf-userColumn.  
                IMPORT bf-userColumn.colName bf-userColumn.colWidth.
                ASSIGN 
                    bf-userColumn.colPosition = iCount
                    bf-userColumn.usrId       = USERID("ASI")
                    bf-userColumn.programName = "{&cellColumnDat}"          
                    iCount = iCount + 1
                    .                              
            END. /* REPEAT */
        END. /* DO TRANSACTION */
        INPUT CLOSE.                     
     END. /* IF lUserResponse */  
                 
     OS-DELETE VALUE(SEARCH(cellColumnDat)).                                       
  END.
END PROCEDURE.

PROCEDURE setCellColumns:
  DEFINE VARIABLE userColumn AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-index AS INT NO-UNDO.
  
  lAutoSave = YES.
  
  IF SEARCH(cellColumnDat) NE ? THEN
      RUN pmoveDatFileToDb.
      
  IF CAN-FIND(FIRST userColumn WHERE userColumn.usrid EQ USERID('ASI') 
                AND userColumn.programName EQ "{&cellColumnDat}" ) THEN DO:       
                    
     /* change default columns to user order */
     DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
     END.
     
     FOR EACH userColumn NO-LOCK
         WHERE userColumn.usrId       EQ USERID('ASI') 
           AND userColumn.programName EQ "{&cellColumnDat}":
         IF userColumn.colPosition = j THEN              
             ASSIGN 
                 userColumn[j]  = userColumn.colName        
                 columnWidth[j] = userColumn.colWidth               
                 j = j + 1
                 .  
      END.
    
     j = j - 1.
     OUTERLOOP:
     DO i = 1 TO j:
     
        DO k = 1 TO j:
            IF NOT VALID-HANDLE(cellColumn[k]) THEN
                LEAVE.
            IF userColumn[i] EQ cellColumn[k]:NAME THEN
                LEAVE.
        END.

        /* 25841 - handle condition where the column def in the .dat file no longer exists in the browser */
        IF NOT VALID-HANDLE(cellColumn[k]) THEN DO:
            lAutoSave = YES.
            LEAVE OUTERLOOP.
            
        END.
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

  END. /* IF CAN-FIND */
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
  DEFINE VARIABLE saveChanges AS LOG INITIAL NO NO-UNDO.
  
  DEFINE BUFFER bf-userColumn FOR userColumn.

  /* check for any columns changes */
  DO i = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    IF cellColumn[i]:NAME EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):NAME AND
       columnWidth[i] EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS 
       AND NOT lAutoSave THEN NEXT. 
    IF NOT lAutoSave THEN 
        MESSAGE 'Save Column Changes?' VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO UPDATE saveChanges.
    IF saveChanges OR lAutoSave THEN DO:
      IF CAN-FIND (FIRST userColumn WHERE userColumn.usrid EQ USERID('ASI') 
                     AND userColumn.programName EQ "{&cellColumnDat}" ) THEN DO:
         FOR EACH bf-userColumn EXCLUSIVE-LOCK 
             WHERE bf-userColumn.usrid       EQ USERID('ASI')
               AND bf-userColumn.programName EQ "{&cellColumnDat}":
             DELETE bf-userColumn.
         END.    
      END.       
      
      DO j = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
    
        CREATE bf-userColumn.
        ASSIGN 
            bf-userColumn.colName     = {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):NAME
            bf-userColumn.colWidth    = {&BROWSE-NAME}:GET-BROWSE-COLUMN(j):WIDTH-PIXELS
            bf-userColumn.colPosition = j
            bf-userColumn.usrId       = USERID("ASI")
            bf-userColumn.programName = "{&cellColumnDat}"
            .
        RELEASE bf-userColumn.   
      END. /* do j */
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
