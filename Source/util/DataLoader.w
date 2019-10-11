&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/DataLoader.w

  Description: Utility to Import/Export data from tables

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Porandla Mithun

  Created: 10/11/2019

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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cErrorMessage   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSuccessMessage AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 rsImportExport btSelectFile ~
fiExportFile btSelectPath fiPath slTables slTablesSelected btProcess 
&Scoped-Define DISPLAYED-OBJECTS rsImportExport fiExportFile fiPath ~
fiSelectionLabel fiSelectedLabel slTables slTablesSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAdd 
     LABEL "Add -->" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btProcess 
     LABEL "Start" 
     SIZE 24 BY 1.91
     FONT 6.

DEFINE BUTTON btRemove 
     LABEL "<-- Remove" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btRemoveAll 
     LABEL "<-- Remove All" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btSelectAll 
     LABEL "Select All -->" 
     SIZE 20 BY 1.14.

DEFINE BUTTON btSelectFile 
     LABEL "Select File" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btSelectPath 
     LABEL "Select Folder" 
     SIZE 17 BY 1.14.

DEFINE VARIABLE fiExportFile AS CHARACTER FORMAT "X(256)":U INITIAL "Select export table list file..." 
     LABEL "Table List File" 
     VIEW-AS FILL-IN 
     SIZE 88.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiPath AS CHARACTER FORMAT "X(256)":U INITIAL "Select path to import..." 
     LABEL "Export Path" 
     VIEW-AS FILL-IN 
     SIZE 88.6 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fiSelectedLabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE fiSelectionLabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE rsImportExport AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Import", 1,
"Export", 2
     SIZE 42 BY 1.19
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 133 BY 5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 133 BY 19.05.

DEFINE VARIABLE slTables AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 41 BY 16.19 NO-UNDO.

DEFINE VARIABLE slTablesSelected AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 41 BY 16.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsImportExport AT ROW 2.19 COL 11.6 NO-LABEL WIDGET-ID 18
     btSelectFile AT ROW 3.81 COL 114.8 WIDGET-ID 32
     fiExportFile AT ROW 3.86 COL 23.6 COLON-ALIGNED WIDGET-ID 30
     btSelectPath AT ROW 5.14 COL 114.8 WIDGET-ID 28
     fiPath AT ROW 5.19 COL 23.6 COLON-ALIGNED WIDGET-ID 26
     fiSelectionLabel AT ROW 7.91 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fiSelectedLabel AT ROW 7.91 COL 88.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     slTables AT ROW 9 COL 11 NO-LABEL WIDGET-ID 4
     slTablesSelected AT ROW 9 COL 90.2 NO-LABEL WIDGET-ID 6
     btAdd AT ROW 13 COL 61.2 WIDGET-ID 8
     btRemove AT ROW 15.29 COL 61.2 WIDGET-ID 16
     btSelectAll AT ROW 17.52 COL 61.2 WIDGET-ID 14
     btRemoveAll AT ROW 19.76 COL 61.2 WIDGET-ID 12
     btProcess AT ROW 26.95 COL 59.2 WIDGET-ID 22
     RECT-1 AT ROW 1.71 COL 4.6 WIDGET-ID 38
     RECT-2 AT ROW 7.19 COL 4.4 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 140.2 BY 28.57
         BGCOLOR 15 FONT 6 WIDGET-ID 100.


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
         TITLE              = "Table Backup/Restore"
         HEIGHT             = 28.57
         WIDTH              = 139.6
         MAX-HEIGHT         = 32.81
         MAX-WIDTH          = 168.4
         VIRTUAL-HEIGHT     = 32.81
         VIRTUAL-WIDTH      = 168.4
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
/* SETTINGS FOR BUTTON btAdd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btRemove IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btRemoveAll IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSelectAll IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiExportFile:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiPath:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiSelectedLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSelectionLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Table Backup/Restore */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Table Backup/Restore */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd C-Win
ON CHOOSE OF btAdd IN FRAME DEFAULT-FRAME /* Add --> */
DO:
    DEFINE VARIABLE cItemList AS CHARACTER NO-UNDO.
    
    IF slTables:SCREEN-VALUE EQ "" OR slTables:SCREEN-VALUE EQ ? THEN
        RETURN.
        
    cItemList = slTablesSelected.
    
    cItemList = IF cItemList EQ "" THEN
                    slTables:SCREEN-VALUE
                ELSE
                    cItemList + "," + slTables:SCREEN-VALUE.
                     
    slTablesSelected:ADD-LAST(cItemList).
    
    slTables:DELETE(slTables:SCREEN-VALUE).            
    
    APPLY 'VALUE-CHANGED' TO slTables.
    APPLY 'VALUE-CHANGED' TO slTablesSelected.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btProcess C-Win
ON CHOOSE OF btProcess IN FRAME DEFAULT-FRAME /* Start */
DO:
    DEFINE VARIABLE cBackupPath AS CHARACTER NO-UNDO.
    
    ASSIGN
        cErrorMessage   = ""
        cSuccessMessage = ""
        .
        
    IF slTablesSelected:NUM-ITEMS EQ 0 THEN DO:
        MESSAGE "No tables selected to" 
            (IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                "Import."
             ELSE
                "Export.")
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    FILE-INFO:FILE-NAME = fiPath:SCREEN-VALUE.
    
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
        MESSAGE "Please select a valid" fiPath:LABEL
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.    
    
    cBackupPath = fiPath:SCREEN-VALUE + "\Backup-"
                + STRING(MONTH(TODAY),"99") + "-"
                + STRING(DAY(TODAY),"99") + "-"
                + STRING(YEAR(TODAY),"9999")
                .                
    
    IF rsImportExport:SCREEN-VALUE EQ "1" THEN DO:
        MESSAGE "Warning! Do not use in Production environment." SKIP
            "This operation will delete"
            "the existing records for below table(s)." SKIP
            slTablesSelected:LIST-ITEMS SKIP
            "However, backup of the above table(s) records will be available"
            "in the" cBackupPath "directory." SKIP
            "Do you want to continue?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO-CANCEL
            TITLE "Continue Import?"
            UPDATE lImport AS LOGICAL.
        
        IF NOT lImport THEN
            RETURN.
             
        OS-CREATE-DIR VALUE(cBackupPath).

        SESSION:SET-WAIT-STATE("GENERAL").
        SELF:SENSITIVE = FALSE.
    
        RUN pProcessExport(
            INPUT cBackupPath
            ).
        
        IF cErrorMessage NE "" THEN DO:
            MESSAGE "Aborting import process." SKIP
                cErrorMessage 
                VIEW-AS ALERT-BOX ERROR.
            
            SESSION:SET-WAIT-STATE("").
            SELF:SENSITIVE = TRUE.
            
            RETURN.
        END.
        
        RUN pProcessImport(
            INPUT fiPath:SCREEN-VALUE
            ).

        SESSION:SET-WAIT-STATE("").
        SELF:SENSITIVE = TRUE.
    END.
    ELSE IF rsImportExport:SCREEN-VALUE EQ "2" THEN DO:
        MESSAGE "The following table(s) will be exported to"
            fiPath:SCREEN-VALUE SKIP
            slTablesSelected:LIST-ITEMS SKIP
            "Do you want to continue?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            TITLE "Continue Export?"
            UPDATE lExport AS LOGICAL.
        
        IF NOT lExport THEN
            RETURN.

        SESSION:SET-WAIT-STATE("GENERAL").
        SELF:SENSITIVE = FALSE.
                    
        RUN pProcessExport(
            INPUT fiPath:SCREEN-VALUE
            ).
            
        SESSION:SET-WAIT-STATE("").
        SELF:SENSITIVE = TRUE.            
    END.   

    ASSIGN
        slTables:LIST-ITEMS         = ""
        slTablesSelected:LIST-ITEMS = ""
        .    
    
    APPLY 'VALUE-CHANGED' TO slTables.
    APPLY 'VALUE-CHANGED' TO slTablesSelected.
    
    MESSAGE cSuccessMessage SKIP
            cErrorMessage VIEW-AS ALERT-BOX.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRemove C-Win
ON CHOOSE OF btRemove IN FRAME DEFAULT-FRAME /* <-- Remove */
DO:
    DEFINE VARIABLE cItemList AS CHARACTER NO-UNDO.
    
    IF slTablesSelected:SCREEN-VALUE EQ "" OR 
       slTablesSelected:SCREEN-VALUE EQ ? THEN
        RETURN.
        
    cItemList = slTables.
    
    cItemList = IF cItemList EQ "" THEN
                    slTablesSelected:SCREEN-VALUE
                ELSE
                    cItemList + "," + slTablesSelected:SCREEN-VALUE.
                     
    slTables:ADD-LAST(cItemList).
    
    slTablesSelected:DELETE(slTablesSelected:SCREEN-VALUE).            
    
    APPLY 'VALUE-CHANGED' TO slTables.
    APPLY 'VALUE-CHANGED' TO slTablesSelected.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRemoveAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRemoveAll C-Win
ON CHOOSE OF btRemoveAll IN FRAME DEFAULT-FRAME /* <-- Remove All */
DO:
    DEFINE VARIABLE cItemList AS CHARACTER NO-UNDO.
    
    IF slTablesSelected:NUM-ITEMS EQ 0 THEN
        RETURN.
        
    cItemList = slTables.
    
    cItemList = IF cItemList EQ "" THEN
                    slTablesSelected:LIST-ITEMS
                ELSE
                    cItemList + "," + slTablesSelected:LIST-ITEMS.
                                
    slTables:ADD-LAST(cItemList).

    slTablesSelected:LIST-ITEMS = "".
            
    APPLY 'VALUE-CHANGED' TO slTables.
    APPLY 'VALUE-CHANGED' TO slTablesSelected.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelectAll C-Win
ON CHOOSE OF btSelectAll IN FRAME DEFAULT-FRAME /* Select All --> */
DO:
    DEFINE VARIABLE cItemList AS CHARACTER NO-UNDO.
    
    IF slTables:NUM-ITEMS EQ 0 THEN
        RETURN.
        
    cItemList = slTablesSelected.
    
    cItemList = IF cItemList EQ "" THEN
                    slTables:LIST-ITEMS
                ELSE
                    cItemList + "," + slTables:LIST-ITEMS.
                                
    slTablesSelected:ADD-LAST(cItemList).

    slTables:LIST-ITEMS = "".
            
    APPLY 'VALUE-CHANGED' TO slTables.
    APPLY 'VALUE-CHANGED' TO slTablesSelected.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelectFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelectFile C-Win
ON CHOOSE OF btSelectFile IN FRAME DEFAULT-FRAME /* Select File */
DO:
    DEFINE VARIABLE cFileName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOkPressed AS LOGICAL   NO-UNDO INITIAL TRUE.
    
    OPEN-FILE-BLOCK:    
    REPEAT:
        SYSTEM-DIALOG GET-FILE cFileName
          TITLE   "Choose table list file ..."
          FILTERS "All Files (*.*)"   "*.*"
          MUST-EXIST
          USE-FILENAME
          UPDATE lOkPressed.
  
        IF lOkPressed THEN DO:      
            fiExportFile:SCREEN-VALUE = cFileName.
            
            RUN pExportTableList.
        END.
        
        LEAVE OPEN-FILE-BLOCK.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelectPath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelectPath C-Win
ON CHOOSE OF btSelectPath IN FRAME DEFAULT-FRAME /* Select Folder */
DO:
    DEFINE VARIABLE cFolderPath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOkPressed  AS LOGICAL   NO-UNDO INITIAL TRUE.
    
    OPEN-FILE-BLOCK:    
    REPEAT:
        SYSTEM-DIALOG GET-DIR cFolderPath
          TITLE   "Select path to import/export ..."
          UPDATE lOkPressed.
  
        IF lOkPressed THEN DO:
            fiPath:SCREEN-VALUE = cFolderPath.
            
            IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                RUN pImportTableList.
        END.
                
        LEAVE OPEN-FILE-BLOCK.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsImportExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsImportExport C-Win
ON VALUE-CHANGED OF rsImportExport IN FRAME DEFAULT-FRAME
DO:
    RUN pUpdateLabels.
    RUN pToggleExportFilePicker.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slTables C-Win
ON VALUE-CHANGED OF slTables IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        btAdd:SENSITIVE       = SELF:SCREEN-VALUE NE "" AND SELF:SCREEN-VALUE NE ?
        btSelectAll:SENSITIVE = SELF:NUM-ITEMS GT 0
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slTablesSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slTablesSelected C-Win
ON VALUE-CHANGED OF slTablesSelected IN FRAME DEFAULT-FRAME
DO:
    ASSIGN
        btRemove:SENSITIVE    = SELF:SCREEN-VALUE NE "" AND SELF:SCREEN-VALUE NE ?
        btRemoveAll:SENSITIVE = SELF:NUM-ITEMS GT 0
        .  
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
    RUN enable_UI.
    RUN pInit.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY rsImportExport fiExportFile fiPath fiSelectionLabel fiSelectedLabel 
          slTables slTablesSelected 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 rsImportExport btSelectFile fiExportFile btSelectPath 
         fiPath slTables slTablesSelected btProcess 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExportTableList C-Win 
PROCEDURE pExportTableList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTableList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable     AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF fiExportFile:SCREEN-VALUE EQ "" THEN
        RETURN.
        
    INPUT FROM VALUE(fiExportFile:SCREEN-VALUE).
    REPEAT:
        IMPORT cTable.

        FIND FIRST _file NO-LOCK
             WHERE _file._File-Name EQ cTable
               AND _file._Tbl-Type  EQ "T"
             NO-ERROR.
        IF AVAILABLE _file THEN DO:
            IF LOOKUP(cTable, cTableList) GT 0 THEN
                NEXT.
                
            cTableList = IF cTableList EQ "" THEN
                             cTable
                         ELSE
                             cTableList + "," + cTable.
        END.
    END.
    INPUT CLOSE.
    
    IF cTableList EQ "" THEN DO:
        MESSAGE "No valid tables provided in the export table list file"
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    slTables:LIST-ITEMS = cTableList.
    
    slTablesSelected:LIST-ITEMS = "".
    
    APPLY 'VALUE-CHANGED' TO slTables.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateExportScript C-Win 
PROCEDURE pGenerateExportScript :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcTableDataFilePath AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cScript        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableDataFile AS CHARACTER NO-UNDO.

ASSIGN
    cScript        = ipcTableDataFilePath + "\" + ipcTableName + ".p"
    cTableDataFile = ipcTableDataFilePath + "\" + ipcTableName + ".d"
    .

FILE-INFO:FILE-NAME = ipcTableDataFilePath.
IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
    oplSuccess = FALSE.
    RETURN.
END.

OUTPUT TO VALUE(cScript).
    PUT UNFORMATTED "DEFINE INPUT  PARAMETER ipcTableDataFilePath AS CHARACTER NO-UNDO." SKIP.
    PUT UNFORMATTED "DEFINE INPUT  PARAMETER ipcTableDataFile     AS CHARACTER NO-UNDO." SKIP.
    PUT UNFORMATTED "DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   NO-UNDO." SKIP.
    PUT UNFORMATTED "FILE-INFO:FILE-NAME = ipcTableDataFilePath." SKIP.
    PUT UNFORMATTED "IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:" SKIP.
    PUT UNFORMATTED "    oplSuccess = FALSE." SKIP.
    PUT UNFORMATTED "    RETURN." SKIP.
    PUT UNFORMATTED "END." SKIP.
    PUT UNFORMATTED "OUTPUT TO VALUE(ipcTableDataFile)." SKIP.
    PUT UNFORMATTED "    FOR EACH " ipcTableName ":" SKIP.
    PUT UNFORMATTED "        EXPORT " ipcTableName "." SKIP.
    PUT UNFORMATTED "    END." SKIP.
    PUT UNFORMATTED "OUTPUT CLOSE." SKIP.
    PUT UNFORMATTED "oplSuccess = TRUE." SKIP.   
OUTPUT CLOSE. 

IF SEARCH(cScript) NE ? THEN DO:
    RUN VALUE(cScript)(
            INPUT  ipcTableDataFilePath,
            INPUT  cTableDataFile,
            OUTPUT oplSuccess
        ).
        
    OS-DELETE VALUE(cScript).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateImportScript C-Win 
PROCEDURE pGenerateImportScript :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcTableDataFilePath AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTableName         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cScript        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableDataFile AS CHARACTER NO-UNDO.

ASSIGN
    cScript        = ipcTableDataFilePath + "\" + ipcTableName + ".p"
    cTableDataFile = ipcTableDataFilePath + "\" + ipcTableName + ".d"
    .

FILE-INFO:FILE-NAME = ipcTableDataFilePath.
IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
    oplSuccess = FALSE.
    RETURN.
END.

OUTPUT TO VALUE(cScript).
    PUT UNFORMATTED "DEFINE INPUT  PARAMETER ipcTableDataFilePath AS CHARACTER NO-UNDO." SKIP.
    PUT UNFORMATTED "DEFINE INPUT  PARAMETER ipcTableDataFile     AS CHARACTER NO-UNDO." SKIP.
    PUT UNFORMATTED "DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   NO-UNDO." SKIP.
    PUT UNFORMATTED "FILE-INFO:FILE-NAME = ipcTableDataFilePath." SKIP.
    PUT UNFORMATTED "IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:" SKIP.
    PUT UNFORMATTED "    oplSuccess = FALSE." SKIP.
    PUT UNFORMATTED "    RETURN." SKIP.
    PUT UNFORMATTED "END." SKIP.
    PUT UNFORMATTED "FOR EACH " ipcTableName ":" SKIP.
    PUT UNFORMATTED "   DELETE " ipcTableName "." SKIP.
    PUT UNFORMATTED "END." SKIP.
    PUT UNFORMATTED "INPUT FROM VALUE(ipcTableDataFile)." SKIP.
    PUT UNFORMATTED "DO TRANSACTION ON ERROR UNDO, LEAVE:" SKIP.
    PUT UNFORMATTED "    REPEAT:" SKIP.
    PUT UNFORMATTED "        CREATE " ipcTablename "." SKIP.
    PUT UNFORMATTED "        IMPORT " ipcTablename "." SKIP.
    PUT UNFORMATTED "    END." SKIP.
    PUT UNFORMATTED "END." SKIP.
    PUT UNFORMATTED "INPUT CLOSE." SKIP.
    PUT UNFORMATTED "oplSuccess = TRUE." SKIP.   
OUTPUT CLOSE. 

IF SEARCH(cScript) NE ? THEN DO:
    RUN VALUE(cScript)(
            INPUT  ipcTableDataFilePath,
            INPUT  cTableDataFile,
            OUTPUT oplSuccess
        ).
        
    OS-DELETE VALUE(cScript).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pImportTableList C-Win 
PROCEDURE pImportTableList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTable     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableList AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
    
    INPUT FROM OS-DIR (fiPath:SCREEN-VALUE).    
    REPEAT:
        IMPORT cFile.

        FILE-INFO:FILE-NAME = fiPath:SCREEN-VALUE + "\" + cFile.
  
        IF FILE-INFO:FILE-TYPE EQ "FRW" THEN DO:
            IF R-INDEX(cFile,".d") GT 0 THEN DO:
                cTable = ENTRY(1,cFile,".d").
                
                FIND FIRST _file NO-LOCK
                     WHERE _file._File-Name EQ cTable
                       AND _file._Tbl-Type  EQ "T"
                     NO-ERROR.
                IF AVAILABLE _file THEN DO:
                    IF LOOKUP(cTable, cTableList) GT 0 THEN
                        NEXT.
                        
                    cTableList = IF cTableList EQ "" THEN
                                     cTable
                                 ELSE
                                     cTableList + "," + cTable.
                END.
            END.
        END.
    END.
    INPUT CLOSE.
    
    IF cTableList EQ "" THEN DO:
        MESSAGE "No data files available to import in the directory selected"
            VIEW-AS ALERT-BOX ERROR.
        
        RETURN.
    END.
    
    slTables:LIST-ITEMS = cTableList.
    
    slTablesSelected:LIST-ITEMS = "".
    
    APPLY 'VALUE-CHANGED' TO slTables.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit C-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    RUN pUpdateLabels.
    RUN pToggleExportFilePicker.                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessExport C-Win 
PROCEDURE pProcessExport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcExportPath AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iIndex   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTable   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DO iIndex = 1 TO slTablesSelected:NUM-ITEMS:
        cTable = ENTRY(iIndex, slTablesSelected:LIST-ITEMS).

        STATUS DEFAULT "Exporting " + cTable + " table data".
        
        RUN pGenerateExportScript (
                INPUT  ipcExportPath,
                INPUT  cTable,
                OUTPUT lSuccess
            ).

        IF lSuccess THEN
            cSuccessMessage = cSuccessMessage + cTable + ": Successfully exported ~n".
        ELSE
            cErrorMessage   = cErrorMessage + cTable + ": Error exporting ~n".
    END.
    
    STATUS DEFAULT "Export complete".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessImport C-Win 
PROCEDURE pProcessImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcImportPath  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iIndex   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTable   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    DO iIndex = 1 TO slTablesSelected:NUM-ITEMS:
        cTable = ENTRY(iIndex, slTablesSelected:LIST-ITEMS).

        STATUS DEFAULT "Importing " + cTable + " table data".
        
        RUN pGenerateImportScript (
                INPUT  ipcImportPath,
                INPUT  cTable,
                OUTPUT lSuccess
            ).

        IF lSuccess THEN
            cSuccessMessage = cSuccessMessage + cTable + ": Successfully imported ~n".
        ELSE
            cErrorMessage   = cErrorMessage + cTable + ": Error importing ~n".
    END.
    
    STATUS DEFAULT "Import complete".    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pToggleExportFilePicker C-Win 
PROCEDURE pToggleExportFilePicker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF rsImportExport:SCREEN-VALUE EQ "1" THEN
        ASSIGN
            fiExportFile:HIDDEN = TRUE
            btSelectFile:HIDDEN = TRUE
            .
    ELSE
        ASSIGN
            fiExportFile:HIDDEN = FALSE
            btSelectFile:HIDDEN = FALSE
            .        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateLabels C-Win 
PROCEDURE pUpdateLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    ASSIGN
        fiPath:LABEL                  = (IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                                            "Import "
                                         ELSE
                                            "Export ") + "Path"
        fiPath:SCREEN-VALUE           = "Select path to "
                                      + (IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                                             "Import"
                                         ELSE
                                            "Export")
                                      + "..."
        fiSelectionLabel:SCREEN-VALUE = "Available tables to "
                                      + (IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                                            "Import"
                                         ELSE
                                            "Export")
        fiSelectedLabel:SCREEN-VALUE  = "Tables selected to "
                                      + (IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                                            "Import"
                                         ELSE
                                            "Export")
        btProcess:LABEL               = "Start "
                                      + (IF rsImportExport:SCREEN-VALUE EQ "1" THEN
                                            "Import"
                                         ELSE
                                            "Export")
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

