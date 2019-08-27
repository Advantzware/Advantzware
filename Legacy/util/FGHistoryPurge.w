&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File:             util/fgHistoryPurge.w
  Description:      Utility to Purge fg-rcpth and fg-rdtl records with summary txns 
  Input Parms:      <none>
  Output Parms:     <none>
  Author:           Brad Vigrass
  Created:          00/00/2018
------------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i new shared}
{fg/ttFGBins.i "NEW SHARED"}

DEFINE STREAM sOutput. 

DEFINE VARIABLE v-process       AS LOG NO-UNDO.
DEFINE VARIABLE cTimeString     AS CHARACTER NO-UNDO.
DEFINE VARIABLE glAtOnce        AS LOGICAL INITIAL YES.
DEFINE VARIABLE glPurge         AS LOGICAL INITIAL YES.
DEFINE VARIABLE glMakeCounts    AS LOGICAL.
DEFINE VARIABLE gcOutputFile    AS CHARACTER.
DEFINE VARIABLE hdFGBinBuild    AS HANDLE.
DEFINE VARIABLE giCounter       AS INTEGER.
DEFINE VARIABLE giTimer         AS INTEGER.
DEFINE VARIABLE gcAsOf          AS DATE INITIAL 12/31/2010.
DEFINE VARIABLE gcCompany       AS CHARACTER.
DEFINE VARIABLE gcFGItemIDStart AS CHARACTER.
DEFINE VARIABLE gcFGItemIDEnd   AS CHARACTER.
DEFINE VARIABLE gcSaveDataFolder AS CHARACTER.
DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.
DEFINE VARIABLE rec-val         AS RECID NO-UNDO.

ASSIGN 
    cocode = gcompany
    locode = gloc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiAsOfDate fiBeginItem fiEndItem ~
fiOutputFile fiOutputFolder fiPurgeFolder tgPurge btn-process btn-cancel ~
btnCalendar-1 btnBrowseFolder btnBrowseFolder-2 RECT-17 
&Scoped-Define DISPLAYED-OBJECTS fiAsOfDate fiBeginItem fiEndItem ~
fiOutputFile fiOutputFolder fiPurgeFolder tgPurge 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "Ca&ncel" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
    LABEL "&Start Process" 
    SIZE 18 BY 1.14.

DEFINE BUTTON btnBrowseFolder 
    LABEL "Select Log Folder" 
    SIZE 30 BY 1 TOOLTIP "Browse for Log Folder".

DEFINE BUTTON btnBrowseFolder-2 
    LABEL "Select Data Folder" 
    SIZE 30 BY 1 TOOLTIP "Browse for Log Folder".

DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U NO-FOCUS FLAT-BUTTON
    LABEL "" 
    SIZE 4.6 BY 1.24 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE fiAsOfDate AS DATE FORMAT "99/99/9999":U 
    LABEL "As Of Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginItem AS CHARACTER FORMAT "X(256)":U 
    LABEL "Beginning Item" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndItem AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Item" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiOutputFile AS CHARACTER FORMAT "X(256)":U 
    LABEL "Output File Name" 
    VIEW-AS FILL-IN 
    SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE fiOutputFolder AS CHARACTER FORMAT "X(256)":U 
    LABEL "Output Folder" 
    VIEW-AS FILL-IN 
    SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE fiPurgeFolder AS CHARACTER FORMAT "X(256)":U 
    LABEL "Save Data Folder" 
    VIEW-AS FILL-IN 
    SIZE 72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 125 BY 11.43.

DEFINE VARIABLE tgPurge AS LOGICAL INITIAL NO 
    LABEL "Purge Data?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    fiAsOfDate AT ROW 6.71 COL 19 COLON-ALIGNED WIDGET-ID 8
    fiBeginItem AT ROW 8.14 COL 19 COLON-ALIGNED WIDGET-ID 10
    fiEndItem AT ROW 8.14 COL 64.2 COLON-ALIGNED WIDGET-ID 12
    fiOutputFile AT ROW 13.91 COL 19 COLON-ALIGNED WIDGET-ID 20
    fiOutputFolder AT ROW 12.67 COL 19 COLON-ALIGNED WIDGET-ID 18
    fiPurgeFolder AT ROW 11.43 COL 19 COLON-ALIGNED WIDGET-ID 16
    tgPurge AT ROW 9.57 COL 21.2 WIDGET-ID 14
    btn-process AT ROW 16.95 COL 34
    btn-cancel AT ROW 16.95 COL 66.6
    btnCalendar-1 AT ROW 6.62 COL 38 WIDGET-ID 22
    btnBrowseFolder AT ROW 12.67 COL 94.2 WIDGET-ID 24
    btnBrowseFolder-2 AT ROW 11.43 COL 94.2 WIDGET-ID 26
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .62 AT ROW 5.05 COL 6
    "" VIEW-AS TEXT
    SIZE 2.2 BY .95 AT ROW 1.95 COL 88
    BGCOLOR 11 
    RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 126 BY 17.71.

DEFINE FRAME FRAME-B
    "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
    SIZE 79 BY .95 AT ROW 2.91 COL 26
    BGCOLOR 11 FGCOLOR 12 FONT 5
    "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
    SIZE 84 BY .95 AT ROW 1.95 COL 21
    BGCOLOR 11 FGCOLOR 12 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 125 BY 3.81
    BGCOLOR 11 .


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
        TITLE              = "Purge FG History"
        HEIGHT             = 17.71
        WIDTH              = 126.2
        MAX-HEIGHT         = 19.76
        MAX-WIDTH          = 126.2
        VIRTUAL-HEIGHT     = 19.76
        VIRTUAL-WIDTH      = 126.2
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN 
    FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME Custom                                                    */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN 
    XXTABVALXX = FRAME FRAME-B:MOVE-AFTER-TAB-ITEM (tgPurge:HANDLE IN FRAME FRAME-A)
    XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (btn-process:HANDLE IN FRAME FRAME-A)
    /* END-ASSIGN-TABS */.

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
    btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME FRAME-A
   3                                                                    */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge FG History */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge FG History */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:

        IF cocode = "" THEN cocode = "001".
        RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CLOSE OF btn-process IN FRAME FRAME-A /* Start Process */
    DO:
        DELETE OBJECT hdFGBinBuild.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder C-Win
ON CHOOSE OF btnBrowseFolder IN FRAME FRAME-A /* Select Log Folder */
    DO:
        RUN pFolderBrowse(fiOutputFolder:HANDLE, "").  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder-2 C-Win
ON CHOOSE OF btnBrowseFolder-2 IN FRAME FRAME-A /* Select Data Folder */
    DO:
        RUN pFolderBrowse(fiPurgeFolder:HANDLE, "").  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME FRAME-A
    DO:
        {methods/btnCalendar.i fiAsOfDate}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAsOfDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAsOfDate C-Win
ON HELP OF fiAsOfDate IN FRAME FRAME-A /* As Of Date */
    DO:
        {methods/calendar.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeginItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeginItem C-Win
ON HELP OF fiBeginItem IN FRAME FRAME-A /* Beginning Item */
    DO:
        RUN windows/l-itemf2.w (cocode,"",FOCUS:SCREEN-VALUE,"", OUTPUT char-val, OUTPUT rec-val).
        fiBeginItem:SCREEN-VALUE = ENTRY(1, char-val).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEndItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEndItem C-Win
ON HELP OF fiEndItem IN FRAME FRAME-A /* Ending Item */
    DO:
        RUN windows/l-itemf2.w (cocode,"",FOCUS:SCREEN-VALUE,"", OUTPUT char-val, OUTPUT rec-val).
        fiEndItem:SCREEN-VALUE = ENTRY(1, char-val).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgPurge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgPurge C-Win
ON VALUE-CHANGED OF tgPurge IN FRAME FRAME-A /* Purge Data? */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN tgPurge.
            IF tgPurge THEN
                ENABLE fiPurgeFolder.
            ELSE 
                DISABLE fiPurgeFolder.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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
   
    RUN fg/FgBinBuild.p PERSISTENT SET hdFGBinBuild.
  
    RUN enable_UI.
    DO WITH FRAME {&FRAME-NAME}:
        DISABLE fiPurgeFolder.
  
        ASSIGN
            cTimeString = STRING(MTIME, "HH:MM:SS")
            fiAsOfDate:SCREEN-VALUE     = "12/31/2010"
            fiPurgeFolder:SCREEN-VALUE   = ".\custfiles\Dumps"
            fiOutputFolder:SCREEN-VALUE = "c:\tmp"
            fiOutputFile:SCREEN-VALUE  = "FGHistory_Purge_" 
                                              + STRING(DAY(TODAY), "99") 
                                              + STRING(MONTH(TODAY), "99") 
                                              + STRING(YEAR(TODAY), "9999") + "_" 
                                              + SUBSTRING(cTimeString, 1, 2)
                                              + SUBSTRING(cTimeString, 4, 2)
                                              + SUBSTRING(cTimeString, 7, 2)
                                              + ".csv"

            .
    END.
    {methods/nowait.i}
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
    DISPLAY fiAsOfDate fiBeginItem fiEndItem fiOutputFile fiOutputFolder 
        fiPurgeFolder tgPurge 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE fiAsOfDate fiBeginItem fiEndItem fiOutputFile fiOutputFolder 
        fiPurgeFolder tgPurge btn-process btn-cancel btnCalendar-1 
        btnBrowseFolder btnBrowseFolder-2 RECT-17 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW FRAME FRAME-B IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExportTempTable C-Win 
PROCEDURE pExportTempTable PRIVATE :
/*------------------------------------------------------------------------------ 
     Purpose: Exports the contents of any temp-table into CSV    
     Notes: 
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
  
    DEFINE VARIABLE hQuery  AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cTTName AS CHARACTER NO-UNDO. 
    
    ASSIGN 
        cTTName = iphTT:NAME. 
    IF iplHeader THEN 
    DO:
        OUTPUT STREAM sOutput to VALUE(ipcFileName). 
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):COLUMN-LABEL + ",". 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END.
    ELSE 
        OUTPUT STREAM sOutput to VALUE(ipcFileName) APPEND. 
    
    CREATE QUERY hQuery. 
    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE). 
    hQuery:QUERY-PREPARE("FOR EACH " + cTTName). 
    hQuery:QUERY-OPEN().
    REPEAT:   
        hQuery:GET-NEXT().   
        IF hQuery:QUERY-OFF-END THEN LEAVE.   
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED  
                '"' iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):buffer-value '",'. 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END. 
    OUTPUT STREAM sOutput CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFolderBrowse C-Win 
PROCEDURE pFolderBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFolderEntry AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFolder  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault AS CHARACTER NO-UNDO.

    SYSTEM-DIALOG GET-DIR cFolder 
        TITLE "Select Folder for Log File"
        UPDATE lOK.
 
    IF lOK THEN iphdFolderEntry:SCREEN-VALUE = cFolder.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessAtOnce C-Win 
PROCEDURE pProcessAtOnce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    EMPTY TEMP-TABLE ttFGBins.
    FOR EACH itemfg NO-LOCK 
        WHERE itemfg.company EQ gcCompany
        AND itemfg.i-no GE gcFGItemIDStart
        AND itemfg.i-no LE gcFGItemIDEnd
        :
        IF glPurge THEN 
            RUN BuildBinsForItemAndPurge IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter, gcSaveDataFolder).
        ELSE 
            RUN BuildBinsForItem IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter).
    END.
    RUN pExportTempTable(TEMP-TABLE ttFGBins:HANDLE, gcOutputFile, YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessWithReset C-Win 
PROCEDURE pProcessWithReset PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pExportTempTable(TEMP-TABLE ttFGBins:HANDLE, gcOutputFile, YES).
    FOR EACH itemfg NO-LOCK 
        WHERE itemfg.company EQ gcCompany
        AND itemfg.i-no GE gcFGItemIDStart
        AND itemfg.i-no LE gcFGItemIDEnd
        :
        EMPTY TEMP-TABLE ttFGBins.
        IF glPurge THEN 
            RUN BuildBinsForItemAndPurge IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter, gcSaveDataFolder).
        ELSE 
            RUN BuildBinsForItem IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter).
        RUN pExportTempTable(TEMP-TABLE ttFGBins:HANDLE, gcOutputFile, NO).
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFolderSlash AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN fiAsOfDate 
            fiBeginItem 
            fiEndItem 
            fiOutputFile 
            fiOutputFolder 
            fiPurgeFolder 
            tgPurge
            .
    END.
    IF fiAsOfDate EQ ? THEN 
    DO:
        MESSAGE "As of date must not be blank." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    IF fiEndItem LT fiBeginItem THEN 
    DO:
        MESSAGE "Ending item must be greater than beginnning item." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    IF fiOutputFile EQ "" THEN 
    DO:
        MESSAGE "Output file name cannot be blank." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    IF fiOutputFolder EQ "" THEN 
    DO:
        MESSAGE "Output folder name cannot be blank." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    IF fiPurgeFolder EQ "" AND tgPurge THEN 
    DO:
        MESSAGE "Save Data Folder cannot be blank." VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    OS-CREATE-DIR VALUE(fiPurgeFolder).
    FILE-INFO:FILE-NAME = fiPurgeFolder.
    IF FILE-INFO:FULL-PATHNAME = ? THEN 
    DO:
        MESSAGE "Save Data Folder could not be created." VIEW-AS ALERT-BOX.
        RETURN.  
    END.
    
    OS-CREATE-DIR VALUE(fiOutputFolder).
    FILE-INFO:FILE-NAME = fiOutputFolder.
    IF FILE-INFO:FULL-PATHNAME = ? THEN 
    DO:
        MESSAGE "Output Folder could not be created." VIEW-AS ALERT-BOX.
        RETURN.  
    END.
    
    ASSIGN 
        giTimer         = TIME
        glAtOnce        = YES
        glPurge         = tgPurge
        gcOutputFile    = fiOutputFolder + "/" + fiOutputFile
        gcAsOf          = fiAsOfDate
        gcCompany       = cocode
        gcFGItemIDStart = fiBeginItem
        gcFGItemIDEnd   = fiEndItem
        glMakeCounts    = glPurge
        giCounter       = 0
        gcSaveDataFolder = fiPurgeFolder
        .
    cFolderSlash = SUBSTRING(gcSaveDataFolder, LENGTH(gcSaveDataFolder), 1).
    IF cFolderSlash NE "/" AND cFolderSlash NE "\" THEN
        gcSaveDataFolder = gcSaveDataFolder + "/".
    IF glAtOnce THEN 
    DO:
        RUN pProcessAtOnce.
    END.
    ELSE 
    DO:
        RUN pProcessWithReset.
    END.
    
    IF glMakeCounts THEN RUN CreateCycleCountTransactions IN hdFGBinBuild (gcAsOf).
    MESSAGE "Records processed: " giCounter SKIP 
        "Time: " TIME - giTimer
        VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

