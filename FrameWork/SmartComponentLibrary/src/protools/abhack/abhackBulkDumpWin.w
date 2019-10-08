&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
{protools/abhack/ABHackResourcesTT.i &SHARED="NEW SHARED"}


/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER phMainabhackwin          AS HANDLE      NO-UNDO.
DEFINE INPUT PARAMETER pcGlobalResCat           AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pcDumpedResourceFileRoot AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pcImportDirName          AS CHARACTER   NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE ttosFile NO-UNDO LABEL "ttosFile (to load ABHack Global Definition Description files)"
 FIELD cFileName     AS CHARACTER
 FIELD cFullPathName AS CHARACTER
 INDEX cFullPathName IS PRIMARY UNIQUE cFullPathName.

DEFINE TEMP-TABLE ttDirToLoad NO-UNDO LABEL "ttDirToLoad (sub directory to load afterward)"
 FIELD cDirName AS CHARACTER
 INDEX cDirName cDirName.
 
DEFINE STREAM dirStream. /* used by IMPORT FROM OS-FIR */

DEFINE VARIABLE gcMonEditor    AS CHARACTER
     VIEW-AS EDITOR SIZE 40 BY 2.86 NO-UNDO.
DEFINE FRAME fMon gcMonEditor.

DEFINE VARIABLE ghParser      AS HANDLE    NO-UNDO.
DEFINE VARIABLE giDirLoadMon  AS INTEGER   NO-UNDO.
DEFINE VARIABLE glStopPressed AS LOGICAL   NO-UNDO.


FUNCTION removeLineComments RETURNS CHARACTER
 (pcLine   AS CHARACTER 
 ,piCIndex AS INTEGER ) IN phMainabhackwin.

FUNCTION createTTorDBBuffer RETURNS HANDLE
/* Create a buffer against TT or DB, based on the buffer name and all the resource we have in memory
   for the particular editor */  
  (phEditor     AS HANDLE
  ,pcBufferName AS CHARACTER
  ,pcOptn       AS CHARACTER ) IN phMainabhackwin.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS gcRootDir glIncludeSubDirs ~
glCleanUpDeletedFiles btnGo glOnlyCleanUpDeletedFiles gcMainMon ~
gBulkDumpEditor 
&Scoped-Define DISPLAYED-OBJECTS gcRootDir glIncludeSubDirs ~
glCleanUpDeletedFiles giFile giTotalFile glOnlyCleanUpDeletedFiles ~
gcMainMon gBulkDumpEditor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGo AUTO-GO 
     LABEL "Process Bulk Dump" 
     SIZE 23 BY 1.14 TOOLTIP "Process Bulk Dump for the Directory Tree".

DEFINE BUTTON BtnStop 
     LABEL "Stop" 
     SIZE 8 BY 1.14.

DEFINE VARIABLE gBulkDumpEditor AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 49 BY 6.91 NO-UNDO.

DEFINE VARIABLE gcMainMon AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 110 BY 13.1
     BGCOLOR 0 FGCOLOR 10  NO-UNDO.

DEFINE VARIABLE gcRootDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Root Dir" 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1 TOOLTIP "Directory drop target" DROP-TARGET NO-UNDO.

DEFINE VARIABLE giFile AS INTEGER FORMAT "z,zzz,zz9":U INITIAL 0 
     LABEL "Processed files" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE giTotalFile AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE glCleanUpDeletedFiles AS LOGICAL INITIAL yes 
     LABEL "Clean up deleted fles" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 TOOLTIP "For now, applies only for abhack database" NO-UNDO.

DEFINE VARIABLE glIncludeSubDirs AS LOGICAL INITIAL yes 
     LABEL "Include sub dirs" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.4 BY .81 NO-UNDO.

DEFINE VARIABLE glOnlyCleanUpDeletedFiles AS LOGICAL INITIAL no 
     LABEL "Only clean up deleted files" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     gcRootDir AT ROW 1.24 COL 14 COLON-ALIGNED
     glIncludeSubDirs AT ROW 1.24 COL 82
     glCleanUpDeletedFiles AT ROW 1.95 COL 82
     btnGo AT ROW 2.43 COL 2
     BtnStop AT ROW 2.43 COL 26
     giFile AT ROW 2.43 COL 50 COLON-ALIGNED
     giTotalFile AT ROW 2.43 COL 66 COLON-ALIGNED NO-LABEL
     glOnlyCleanUpDeletedFiles AT ROW 2.67 COL 82
     gcMainMon AT ROW 3.86 COL 1 NO-LABEL
     gBulkDumpEditor AT ROW 5.29 COL 10 NO-LABEL
     "/" VIEW-AS TEXT
          SIZE 2 BY .95 AT ROW 2.43 COL 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.8 BY 16
         FONT 4
         DEFAULT-BUTTON btnGo.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Bulk Dump of ABHack Global Ressource Descriptions"
         HEIGHT             = 16
         WIDTH              = 110.8
         MAX-HEIGHT         = 39.91
         MAX-WIDTH          = 133.4
         VIRTUAL-HEIGHT     = 39.91
         VIRTUAL-WIDTH      = 133.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BtnStop IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN giFile IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN giTotalFile IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bulk Dump of ABHack Global Ressource Descriptions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bulk Dump of ABHack Global Ressource Descriptions */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Bulk Dump of ABHack Global Ressource Descriptions */
DO:
  IF SELF:WIDTH-PIXEL > FRAME fMain:WIDTH-PIXELS THEN 
   FRAME fMain:WIDTH-PIXELS = SELF:WIDTH-PIXEL.

  IF SELF:HEIGHT-PIXEL > FRAME fMain:HEIGHT-PIXELS THEN 
   FRAME fMain:HEIGHT-PIXELS = SELF:HEIGHT-PIXEL.
   
  gcMainMon:WIDTH-PIXELS  = SELF:WIDTH-PIXELS - gcMainMon:X.
  gcMainMon:HEIGHT-PIXELS = SELF:HEIGHT-PIXELS - gcMainMon:Y.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGo C-Win
ON CHOOSE OF btnGo IN FRAME fMain /* Process Bulk Dump */
DO:
    btnStop:SENSITIVE = YES.    
    glStopPressed = NO.
    
    gcMainMon:CURSOR-OFFSET = gcMainMon:LENGTH NO-ERROR.
    
    RUN loadFilesToProcess.
    
    IF glStopPressed THEN RETURN.
    
    IF glOnlyCleanUpDeletedFiles = NO THEN RUN bulkDumpAll.
    
    /* 21-SEP-2007 sla: beware to never RUN cleanUpDeletedFiles 
     if ttosFile was not loaded with glIncludeSubDirs */
    IF   glCleanUpDeletedFiles
     AND glIncludeSubDirs  /* should always be set if glCleanUpDeletedFiles is TRUE */
     THEN DO:
        gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " about to cleanup deleted files").
        RUN cleanUpDeletedFiles IN ghParser (REPLACE(gcRootDir, "\", "/")
                                            ,TABLE ttosFile
                                            ,gcMainMon:HANDLE
                                            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " cleanUpDeletedFiles returned the following error" + RETURN-VALUE).
    END.
    
    gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " Bulk dump job complete").
    
    btnStop:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnStop C-Win
ON CHOOSE OF BtnStop IN FRAME fMain /* Stop */
DO:
    glStopPressed = YES.
    SELF:SENSITIVE = NO.
    
    gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + ": Generation of ABHack Global Description Files interrupted (Stop pressed)").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gcRootDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcRootDir C-Win
ON DROP-FILE-NOTIFY OF gcRootDir IN FRAME fMain /* Root Dir */
DO:
    DEFINE VARIABLE cFileName AS CHARACTER   NO-UNDO.
    
    cFileName = SELF:GET-DROPPED-FILE(1).
    
    FILE-INFO:FILE-NAME = cFileName.
    
    IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
    
    IF INDEX(FILE-INFO:FILE-TYPE, "F") > 0 THEN DO:
        cFileName = REPLACE(cFileName, "\", "/").
        ENTRY(NUM-ENTRIES(cFileName, "/"), cFileName, "/") = "".
        cFileName = RIGHT-TRIM(cFileName, "/").
    END.

    FILE-INFO:FILE-NAME = cFileName.
    
    IF FILE-INFO:FULL-PATHNAME = ? 
     OR INDEX(FILE-INFO:FILE-TYPE, "D") = 0 THEN DO:
        MESSAGE cFileName " is of file type " FILE-INFO:FILE-TYPE "!!!" SKIP
         "Drop a valid directory or file"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    gcRootDir = REPLACE(cFileName, "\", "/").
    gcRootDir = cFileName.
    DISPLAY gcRootDir WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcRootDir C-Win
ON VALUE-CHANGED OF gcRootDir IN FRAME fMain /* Root Dir */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glCleanUpDeletedFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glCleanUpDeletedFiles C-Win
ON VALUE-CHANGED OF glCleanUpDeletedFiles IN FRAME fMain /* Clean up deleted fles */
DO:
  ASSIGN {&SELF-NAME}.
  
  glOnlyCleanUpDeletedFiles:SENSITIVE = {&SELF-NAME}.
  IF {&SELF-NAME} = NO THEN DO:
      glOnlyCleanUpDeletedFiles:CHECKED = NO.
      APPLY "value-changed" TO glOnlyCleanUpDeletedFiles.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glIncludeSubDirs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glIncludeSubDirs C-Win
ON VALUE-CHANGED OF glIncludeSubDirs IN FRAME fMain /* Include sub dirs */
DO:
  ASSIGN {&SELF-NAME}.
  
  glCleanUpDeletedFiles:SENSITIVE = {&SELF-NAME}.
  IF {&SELF-NAME} = NO THEN DO:
      glCleanUpDeletedFiles:CHECKED = NO.
      APPLY "value-changed" TO glCleanUpDeletedFiles.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME glOnlyCleanUpDeletedFiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL glOnlyCleanUpDeletedFiles C-Win
ON VALUE-CHANGED OF glOnlyCleanUpDeletedFiles IN FRAME fMain /* Only clean up deleted files */
DO:
  ASSIGN {&SELF-NAME}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN initialize.
  RUN enable_UI.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bulkDumpAll C-Win 
PROCEDURE bulkDumpAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER ttosFile FOR ttosFile.
DEFINE BUFFER ttEdt    FOR ttEdt.

DEFINE VARIABLE hDummyEditor AS HANDLE    NO-UNDO.
DEFINE VARIABLE lCompileOK   AS LOGICAL   NO-UNDO.

hDummyEditor = gBulkDumpEditor:HANDLE IN FRAME {&FRAME-NAME}.
giFile = 0.

FOR EACH ttosFile
 ON STOP UNDO, RETRY:
 
    IF RETRY OR glStopPressed THEN DO:
        IF glStopPressed = NO THEN APPLY "CHOOSE" TO BtnStop IN FRAME {&FRAME-NAME}.
        LEAVE.
    END.
    
    IF NOT VALID-HANDLE(phMainabhackwin) THEN DO:
        gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + ": It seems the main abhackwin window has been closed (lost its handle) !!  Process about to be aborted").
        RETRY.
    END.

    IF giDirLoadMon <> TIME THEN DO:
        giDirLoadMon = TIME.
        giFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(giFile).
        PROCESS EVENTS. /* flush the UI plus catch a STOP pressed event */
    END.

    giFile = giFile + 1.
    gcMainMon:INSERT-STRING("~n~n" + STRING(TIME, "hh:mm:ss") + ": Processing " + ttosFile.cFullPathName).
    
    FIND FIRST ttEdt WHERE ttEdt.hEditor = hDummyEditor NO-ERROR.
    IF NOT AVAILABLE ttEdt THEN DO:
        CREATE ttEdt.
        ASSIGN ttEdt.hEditor = hDummyEditor.
    END.
    
    ASSIGN
     ttEdt.cFileName     = ttosFile.cFileName
     ttEdt.cFullPathName = ttosFile.cFullPathName
     ttEdt.hWin          = {&WINDOW-NAME}.
    
    RUN loadGlobalResources IN ghParser (hDummyEditor, YES, OUTPUT lCompileOK).
        
    RUN clearGlobalResources IN ghParser (hDummyEditor). /* do it now */
    
    gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + ":  " + gcMonEditor:SCREEN-VALUE IN FRAME fMon).
END.

giFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(giFile).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY gcRootDir glIncludeSubDirs glCleanUpDeletedFiles giFile giTotalFile 
          glOnlyCleanUpDeletedFiles gcMainMon gBulkDumpEditor 
      WITH FRAME fMain IN WINDOW C-Win.
  ENABLE gcRootDir glIncludeSubDirs glCleanUpDeletedFiles btnGo 
         glOnlyCleanUpDeletedFiles gcMainMon gBulkDumpEditor 
      WITH FRAME fMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize C-Win 
PROCEDURE initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN protools/abhack/abhackParser.p PERSISTENT SET ghParser
 (THIS-PROCEDURE
 ,gcMonEditor:HANDLE IN FRAME fMon).

ASSIGN
 gcGlobalResCat           = pcGlobalResCat
 gcDumpedResourceFileRoot = pcDumpedResourceFileRoot
 gcImportDirName          = pcImportDirName.

ASSIGN
 FILE-INFO:FILE-NAME              = "."
 gcRootDir                        = FILE-INFO:FULL-PATHNAME
 {&WINDOW-NAME}:MIN-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
 {&WINDOW-NAME}:MIN-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS.

gBulkDumpEditor:MOVE-TO-BOTTOM() IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadDirs C-Win 
PROCEDURE loadDirs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cExtension    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullPathFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOSAttribute  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRelativeFile AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttosFile       FOR ttosFile. /* local scope only please */
DEFINE BUFFER ttDirToLoad    FOR ttDirToLoad.
DEFINE BUFFER otherdirToLoad FOR ttDirToLoad.

FOR EACH ttDirToLoad: 
    /* 21-SEP-2007 sla: protection against directory names with accents (usaually not used for source code) */
    FILE-INFO:FILE-NAME = ttDirToLoad.cDirName.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
        DELETE ttDirToLoad.    
        NEXT.
    END.
    
    INPUT STREAM dirStream FROM OS-DIR (ttDirToLoad.cDirName).
    
    dirLoop:
    REPEAT ON STOP UNDO, RETRY:
        IF RETRY THEN DO:
            APPLY "CHOOSE" TO BtnStop IN FRAME {&FRAME-NAME}.
            LEAVE dirLoop.
        END.

        IF giDirLoadMon <> TIME THEN DO:
            giDirLoadMon = TIME.
            giTotalFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(giTotalFile).
            PROCESS EVENTS. /* flush the UI plus catch a STOP pressed event */
        END.
    
        IF glStopPressed THEN LEAVE.
        
        IMPORT STREAM dirStream cRelativeFile cFullPathFile cOSAttribute.
        IF INDEX(cOSAttribute, "D") > 0 THEN DO:
            IF cRelativeFile MATCHES "*~~~." THEN NEXT.
            IF glIncludeSubDirs THEN DO:
                CREATE otherdirToLoad.
                otherdirToLoad.cDirName = cFullPathFile.
            END.
            NEXT dirLoop.
        END.
        
        IF INDEX(cOSAttribute, "F") = 0 THEN NEXT. /* not a regular file... ? */

        /* 15-AUG-2007 sla: new abhackBulkDumpIgnoreDir.txt directive */
        IF cRelativeFile MATCHES "*abhackBulkDumpIgnoreDir~~~.txt"THEN DO:
            gcMainMon:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " abhackBulkDumpIgnoreDir.txt file found in directory " +  QUOTER(ttDirToLoad.cDirName)) IN FRAME {&FRAME-NAME}.
            /* remove subdirectories of this directory that we just added before find the abhackBulkDumpIgnoreDir.txt file */
            FOR EACH otherdirToLoad WHERE
                 otherdirToLoad.cDirName BEGINS ttDirToLoad.cDirName
             AND ROWID(otherdirToLoad) <> ROWID(ttDirToLoad): /* this one will be deleted at the end of the loop */
                DELETE otherdirToLoad. 
            END.
            LEAVE dirLoop.
        END.
        
        cExtension = ENTRY(NUM-ENTRIES(cRelativeFile, "."), cRelativeFile, ".").
        
        IF LOOKUP(cExtension, "p,w,i,cls") = 0 THEN NEXT dirLoop.
        IF cRelativeFile MATCHES "*_cl~~~.w" THEN NEXT dirLoop. /* SDO client proxy */
        
        CREATE ttosFile.
        ASSIGN
         giTotalFile            = giTotalFile + 1
         ttosFile.cFileName     = REPLACE(cRelativeFile, "\", "/")
         ttosFile.cFullPathName = REPLACE(cFullPathFile, "\", "/").
    END.    
    INPUT STREAM dirStream CLOSE.
    DELETE ttDirToLoad.
    IF glStopPressed THEN RETURN.
END. /* FOR EACH ttDirToLoad: */


    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadFilesToProcess C-Win 
PROCEDURE loadFilesToProcess :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER ttosFile    FOR ttosFile.
    DEFINE BUFFER ttDirToLoad FOR ttDirToLoad.
    
    FILE-INFO:FILE-NAME = gcRootDir.
    IF FILE-INFO:FULL-PATHNAME = ?
     THEN DO:
         MESSAGE "The Root Director is not a valid Directory" SKIP
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         RETURN.
    END.
    
    EMPTY TEMP-TABLE ttosFile.
    EMPTY TEMP-TABLE ttDirToLoad.
    
    giTotalFile = 0.

    gcMainMon:INSERT-STRING(STRING(TIME, "hh:mm:ss") + " Preparing file list to Bulk Process") IN FRAME {&FRAME-NAME}.

    CREATE ttDirToLoad.
    ttDirToLoad.cDirName = gcRootDir. 

    RUN loadDirs.

    giTotalFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(giTotalFile).
END PROCEDURE.

/* slaSpecialBreakPoint */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

