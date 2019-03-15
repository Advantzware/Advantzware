&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: userFoldr.w

  Description: User Folder Viewer

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.17.2018

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define program-id userFoldr.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/sortByDefs.i}

DEFINE VARIABLE cFolderFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserFolder AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAdmin      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rRowID      AS ROWID     NO-UNDO.

DEFINE BUFFER bTaskResult FOR taskResult.

lAdmin = CAN-DO("ASI,NoSweat",USERID("ASI")).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME taskResultBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES taskResult

/* Definitions for BROWSE taskResultBrowse                              */
&Scoped-define FIELDS-IN-QUERY-taskResultBrowse taskResult.fileDateTime ~
taskResult.fileType taskResult.user-id taskResult.viewed ~
taskResult.archived taskResult.folderFile 
&Scoped-define ENABLED-FIELDS-IN-QUERY-taskResultBrowse 
&Scoped-define QUERY-STRING-taskResultBrowse FOR EACH taskResult ~
      WHERE lAdmin EQ YES ~
OR (lAdmin EQ NO ~
AND TaskResult.user-id EQ USERID("ASI")) NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-taskResultBrowse OPEN QUERY taskResultBrowse FOR EACH taskResult ~
      WHERE lAdmin EQ YES ~
OR (lAdmin EQ NO ~
AND TaskResult.user-id EQ USERID("ASI")) NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-taskResultBrowse taskResult
&Scoped-define FIRST-TABLE-IN-QUERY-taskResultBrowse taskResult


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-taskResultBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS taskResultBrowse btnArchive btnView ~
btnDelete btnRefresh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnArchive 
     IMAGE-UP FILE "AOA/images/element_copy.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Archive" 
     SIZE 5 BY .95 TOOLTIP "Archive to User Folder".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 5 BY .95 TOOLTIP "Delete Folder File".

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "AOA/images/aoaapply.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Refresh" 
     SIZE 5 BY .95 TOOLTIP "Refresh Folder Files".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "AOA/images/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "View" 
     SIZE 5 BY .95 TOOLTIP "View Folder File".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY taskResultBrowse FOR 
      taskResult SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE taskResultBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS taskResultBrowse C-Win _STRUCTURED
  QUERY taskResultBrowse NO-LOCK DISPLAY
      taskResult.fileDateTime FORMAT "99/99/9999 HH:MM:SS.SSS":U
            LABEL-BGCOLOR 14
      taskResult.fileType FORMAT "x(8)":U LABEL-BGCOLOR 14
      taskResult.user-id FORMAT "x(10)":U LABEL-BGCOLOR 14
      taskResult.viewed FORMAT "yes/no":U LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
      taskResult.archived FORMAT "99/99/9999 HH:MM:SS.SSS":U LABEL-BGCOLOR 14
      taskResult.folderFile FORMAT "x(256)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 28.57
         TITLE "Task Results".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     taskResultBrowse AT ROW 1 COL 1 WIDGET-ID 200
     btnArchive AT ROW 1 COL 20 HELP
          "Click to Delete Folder File" WIDGET-ID 8
     btnView AT ROW 1 COL 14 HELP
          "Click to View Selected Folder File" WIDGET-ID 6
     btnDelete AT ROW 1 COL 8 HELP
          "Click to Delete Folder File" WIDGET-ID 4
     btnRefresh AT ROW 1 COL 2 HELP
          "Click to Refresh Folder Files" WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "User Folder"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB taskResultBrowse 1 DEFAULT-FRAME */
ASSIGN 
       taskResultBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE taskResultBrowse
/* Query rebuild information for BROWSE taskResultBrowse
     _TblList          = "ASI.taskResult"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _Where[1]         = "lAdmin EQ YES
OR (lAdmin EQ NO
AND TaskResult.user-id EQ USERID(""ASI""))"
     _FldNameList[1]   > ASI.taskResult.fileDateTime
"taskResult.fileDateTime" ? ? "datetime" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.taskResult.fileType
"taskResult.fileType" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.taskResult.user-id
"taskResult.user-id" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.taskResult.viewed
"taskResult.viewed" ? ? "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "?" ? ? 5 no 0 no no
     _FldNameList[5]   > ASI.taskResult.archived
"taskResult.archived" ? ? "datetime" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.taskResult.folderFile
"taskResult.folderFile" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE taskResultBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User Folder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Folder */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* User Folder */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnArchive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnArchive C-Win
ON CHOOSE OF btnArchive IN FRAME DEFAULT-FRAME /* Archive */
DO:
    IF NOT AVAILABLE taskResult THEN
    RETURN NO-APPLY.
    ASSIGN
        cFolderFile = taskResult.folderFile
        cFolderFile = REPLACE(cFolderFile,"TaskResults","users/"
                    + taskResult.user-id
                    + "/Jasper")
                    .
    IF SEARCH(cFolderFile) EQ ? THEN DO TRANSACTION:
        FIND CURRENT taskResult EXCLUSIVE-LOCK.
        CREATE bTaskResult.
        BUFFER-COPY taskResult EXCEPT rec_key TO bTaskResult.
        ASSIGN
            bTaskResult.folderFile = cFolderFile
            bTaskResult.archived   = DATETIME(TODAY,TIME)
            bTaskResult.viewed     = NO
            taskResult.viewed      = YES
            rRowID                 = ROWID(bTaskResult)
            .
        FIND CURRENT taskResult NO-LOCK.
        OS-COPY VALUE(taskResult.folderFile) VALUE(bTaskResult.folderFile).
        RELEASE bTaskResult.
        {&OPEN-QUERY-taskResultBrowse}
        REPOSITION taskResultBrowse TO ROWID rRowID.
    END. /* if search */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    IF NOT AVAILABLE taskResult THEN
    RETURN NO-APPLY.
    MESSAGE
        "Permanantly Remove ~"" + taskResult.folderFile + "~"?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lDelete AS LOGICAL.
    IF lDelete THEN DO TRANSACTION:
        OS-DELETE VALUE(taskResult.folderFile).
        FIND CURRENT taskResult EXCLUSIVE-LOCK.
        DELETE taskResult.
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* if delete */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh C-Win
ON CHOOSE OF btnRefresh IN FRAME DEFAULT-FRAME /* Refresh */
DO:
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME /* View */
DO:
    IF AVAILABLE TaskResult THEN DO:
        IF SEARCH(taskResult.folderFile) NE ? THEN DO TRANSACTION:
            OS-COMMAND NO-WAIT start VALUE(taskResult.folderFile).
            FIND CURRENT TaskResult EXCLUSIVE-LOCK.
            TaskResult.viewed = YES.
            FIND CURRENT TaskResult NO-LOCK.
            BROWSE TaskResultBrowse:REFRESH().
        END. /* if search */
        ELSE
        MESSAGE
            "Result File: ~"" + taskResult.folderFile +
            "~" cannot be found!"
        VIEW-AS ALERT-BOX ERROR.
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME taskResultBrowse
&Scoped-define SELF-NAME taskResultBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL taskResultBrowse C-Win
ON DEFAULT-ACTION OF taskResultBrowse IN FRAME DEFAULT-FRAME /* Task Results */
DO:
    APPLY "CHOOSE":U TO btnView.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL taskResultBrowse C-Win
ON START-SEARCH OF taskResultBrowse IN FRAME DEFAULT-FRAME /* Task Results */
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {&WINDOW-NAME}:TITLE = "~"" + USERID("ASI") + "~" " + {&WINDOW-NAME}:TITLE.
  RUN pGetSettings.
  RUN enable_UI.
  btnRefresh:MOVE-TO-TOP().
  btnDelete:MOVE-TO-TOP().
  btnView:MOVE-TO-TOP().
  btnArchive:MOVE-TO-TOP().
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pByArchived" "taskResult.archived"}
{methods/sortByProc.i "pByFileDateTime" "taskResult.fileDateTime"}
{methods/sortByProc.i "pByFileType" "taskResult.fileType"}
{methods/sortByProc.i "pByFolderFile" "taskResult.folderFile"}
{methods/sortByProc.i "pByUserID" "taskResult.user-id"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE taskResultBrowse btnArchive btnView btnDelete btnRefresh 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "archived" THEN
        RUN pByArchived.
        WHEN "fileDateTime" THEN
        RUN pByFileDateTime.
        WHEN "fileType" THEN
        RUN pByFileType.
        WHEN "folderFile" THEN
        RUN pByFolderFile.
        WHEN "user-id" THEN
        RUN pByUserID.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id = "{&program-id}"
            user-print.user-id    = USERID("ASI")
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        HIDE BROWSE {&BROWSE-NAME}.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            BROWSE {&BROWSE-NAME}:HEIGHT       = FRAME {&FRAME-NAME}:HEIGHT
                                               - BROWSE {&BROWSE-NAME}:ROW + 1
            BROWSE {&BROWSE-NAME}:WIDTH        = FRAME {&FRAME-NAME}:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW BROWSE {&BROWSE-NAME}.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

