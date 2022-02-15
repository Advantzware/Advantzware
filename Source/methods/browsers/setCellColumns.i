/* setCellColumns.i */

DEFINE VARIABLE cellColumn      AS WIDGET-HANDLE NO-UNDO EXTENT 200.
DEFINE VARIABLE columnWidth     AS DECIMAL       NO-UNDO EXTENT 200.
DEFINE VARIABLE columnVisible   AS LOGICAL       NO-UNDO EXTENT 200.
DEFINE VARIABLE columnLabel     AS CHARACTER     NO-UNDO EXTENT 200.
DEFINE VARIABLE cellColumnDat   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lAutoSave       AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cCharHandle     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE hdProgram       AS HANDLE        NO-UNDO.  
DEFINE VARIABLE cWindowProgram  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cCurrentProgram AS CHARACTER     NO-UNDO.

DEFINE VARIABLE oUserColumn AS system.UserColumn NO-UNDO.

DEFINE MENU POPUP-MENU-br_table TITLE "Settings"
       MENU-ITEM m_Customize    LABEL "Customize"      ACCELERATOR "CTRL-S".

IF "{&BROWSE-NAME}" NE "" THEN
    BROWSE {&BROWSE-NAME}:POPUP-MENU = MENU POPUP-MENU-br_table:HANDLE.

/* create a &SCOPED-DEFINE cellColumnDat value prior to this include
   if another file name is desired to store user cell column order */
&IF DEFINED(cellColumnDat) EQ 0 &THEN
&SCOPED-DEFINE cellColumnDat {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
&ENDIF

cellColumnDat = './users/' + USERID('ASI') + '/{&cellColumnDat}.dat'.

ON "CTRL-S" OF FRAME {&FRAME-NAME} ANYWHERE
DO:
    
    RUN pOpenBrowseCustomizer.
    
    RETURN NO-APPLY.
END.

ON CHOOSE OF MENU-ITEM m_Customize /* Customize */
DO:
    RUN pOpenBrowseCustomizer.
    
    RETURN NO-APPLY.
END.
/* **********************  Internal Procedures  *********************** */


PROCEDURE pMoveDatFileToDb PRIVATE:
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

PROCEDURE pOpenBrowseCustomizer PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lApplyChanges AS LOGICAL NO-UNDO.
    
    IF cellColumnDat NE "" AND "{&BROWSE-NAME}" NE "" THEN DO:
        /* Fetch the updated changes from browse before displaying, as user still have an option to reposition and resize columns  */
        oUserColumn:SetUserColumnFromBrowse().
        
        RUN windows/dUserColumn.w (oUserColumn, OUTPUT lApplyChanges).
        
        IF lApplyChanges THEN
            oUserColumn:UpdateBrowse().
    END.
END PROCEDURE.

PROCEDURE setCellColumns:
  DEFINE VARIABLE userColumn AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE v-index AS INT NO-UNDO.

  DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

  lAutoSave = YES.
  
  {methods/run_link.i "CONTAINER-SOURCE" "GetProgramAndPage" "(OUTPUT cWindowProgram)"}
  
  ASSIGN
      cCurrentProgram = PROGRAM-NAME(1)
      cCurrentProgram = REPLACE(cCurrentProgram, "\", "/")
      cCurrentProgram = SUBSTRING(cCurrentProgram,1,INDEX(cCurrentProgram,".") - 1)
      cCurrentProgram = ENTRY(NUM-ENTRIES(cCurrentProgram," "), cCurrentProgram," ")
      .
          
  cCurrentProgram = cWindowProgram + "/" + cCurrentProgram.
  
  IF SEARCH(cellColumnDat) NE ? THEN
      RUN pMoveDatFileToDb.
  
  /* Update existing records to new program name */
  IF CAN-FIND(FIRST userColumn WHERE userColumn.usrid EQ USERID('ASI') 
                AND userColumn.programName EQ "{&cellColumnDat}" ) THEN DO:       
     FOR EACH userColumn EXCLUSIVE-LOCK
         WHERE userColumn.usrId       EQ USERID('ASI') 
           AND userColumn.programName EQ cCurrentProgram:
         DELETE userColumn.   
     END.

     FOR EACH userColumn EXCLUSIVE-LOCK
         WHERE userColumn.usrId       EQ USERID('ASI') 
           AND userColumn.programName EQ "{&cellColumnDat}":
         userColumn.programName = cCurrentProgram.    
     END.
  END.
  
  IF NOT VALID-OBJECT(oUserColumn) THEN
    oUserColumn = NEW system.UserColumn (BROWSE {&BROWSE-NAME}:HANDLE, cCurrentProgram).
  
  oUserColumn:UpdateBrowse().
  
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
  
  /* check for any columns changes */
  IF NOT lAutoSave THEN 
      MESSAGE 'Save Column Changes?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE saveChanges.
  IF saveChanges OR lAutoSave THEN
      oUserColumn:SetUserColumnFromBrowse().
    
  IF VALID-OBJECT (oUserColumn) THEN
      DELETE OBJECT oUserColumn.
      
  &IF DEFINED(xlocal-destroy) &THEN
    RUN xlocal-destroy.
  &ENDIF
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.
