&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTypes AS CHARACTER NO-UNDO.

{util\ttImport.i NEW SHARED}
DEFINE VARIABLE ghdImportProcs AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME brPreview

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttImportData

/* Definitions for BROWSE brPreview                                     */
&Scoped-define FIELDS-IN-QUERY-brPreview ttImportData.lValid ttImportData.cImportNote ttImportData.cData[1] ttImportData.cData[2] ttImportData.cData[3] ttImportData.cData[4] ttImportData.cData[5] ttImportData.cData[6] ttImportData.cData[7] ttImportData.cData[8] ttImportData.cData[9] ttImportData.cData[10] ttImportData.cData[11] ttImportData.cData[12] ttImportData.cData[13] ttImportData.cData[14] ttImportData.cData[15] ttImportData.cData[16] ttImportData.cData[17] ttImportData.cData[18] ttImportData.cData[19] ttImportData.cData[20] ttImportData.cData[21] ttImportData.cData[22] ttImportData.cData[23] ttImportData.cData[24] ttImportData.cData[25] ttImportData.cData[26] ttImportData.cData[27] ttImportData.cData[28] ttImportData.cData[29] ttImportData.cData[30] ttImportData.cData[31] ttImportData.cData[32] ttImportData.cData[33] ttImportData.cData[34] ttImportData.cData[35] ttImportData.cData[36] ttImportData.cData[37] ttImportData.cData[38] ttImportData.cData[39] ttImportData.cData[40] ttImportData.cData[41] ttImportData.cData[42] ttImportData.cData[43] ttImportData.cData[44] ttImportData.cData[45] ttImportData.cData[46] ttImportData.cData[47] ttImportData.cData[48] ttImportData.cData[49] ttImportData.cData[50]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brPreview   
&Scoped-define SELF-NAME brPreview
&Scoped-define QUERY-STRING-brPreview FOR EACH ttImportData WHERE NOT ttImportData.lHeader
&Scoped-define OPEN-QUERY-brPreview OPEN QUERY {&SELF-NAME} FOR EACH ttImportData WHERE NOT ttImportData.lHeader .
&Scoped-define TABLES-IN-QUERY-brPreview ttImportData
&Scoped-define FIRST-TABLE-IN-QUERY-brPreview ttImportData


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-brPreview}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 cbType fiFileName btnBrowse ~
lHeaderRow lFieldValidation rdDuplicates btnLoad brPreview fiLogFolder ~
btnBrowseFolder lLogOnly btnProcess btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cbType fiFileName lHeaderRow ~
lFieldValidation rdDuplicates fiLogFolder lLogOnly 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetDefaultImportFolder C-Win 
FUNCTION fGetDefaultImportFolder RETURNS CHARACTER
    (ipcCompany AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLogFile C-Win 
FUNCTION fGetLogFile RETURNS CHARACTER
    ( ipcFolder AS CHARACTER, ipcBase AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLogFolder C-Win 
FUNCTION fGetLogFolder RETURNS CHARACTER
    ( ipcCompany AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBrowse 
     LABEL "Find File" 
     SIZE 30 BY 1.

DEFINE BUTTON btnBrowseFolder 
     LABEL "Select Log Folder" 
     SIZE 30 BY 1 TOOLTIP "Browse for Log Folder".

DEFINE BUTTON btnCancel 
     LABEL "Ca&ncel" 
     SIZE 30 BY 2.

DEFINE BUTTON btnLoad 
     LABEL "1. &Load Import File" 
     SIZE 30 BY 2.

DEFINE BUTTON btnProcess 
     LABEL "2. Process &Import" 
     SIZE 30 BY 2.

DEFINE VARIABLE cbType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "",""""
     DROP-DOWN-LIST
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE fiFileName AS CHARACTER FORMAT "X(256)" 
     LABEL "Import File" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1.

DEFINE VARIABLE fiLogFolder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Put Log in Folder" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 TOOLTIP "Enter the file folder where the log will be generated" NO-UNDO.

DEFINE VARIABLE rdDuplicates AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Overwrite Data", 1,
"Skip Records", 2
     SIZE 56 BY .71 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 7.14.

DEFINE VARIABLE lFieldValidation AS LOGICAL INITIAL yes 
     LABEL "Perform Field Level Validation (may be slow)" 
     VIEW-AS TOGGLE-BOX
     SIZE 61 BY .81 NO-UNDO.

DEFINE VARIABLE lHeaderRow AS LOGICAL INITIAL yes 
     LABEL "First Row is Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE lLogOnly AS LOGICAL INITIAL no 
     LABEL "Generate Log Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brPreview FOR 
      ttImportData SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brPreview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brPreview C-Win _FREEFORM
  QUERY brPreview DISPLAY
      ttImportData.lValid    FORMAT "Yes/No" COLUMN-LABEL "Valid" 
    ttImportData.cImportNote    FORMAT "x(30)" COLUMN-LABEL "Reason"
    ttImportData.cData[1] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[2] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[3] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[4] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[5] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[6] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[7] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[8] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[9] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[10] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[11] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[12] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[13] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[14] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[15] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[16] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[17] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[18] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[19] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[20] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[21] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[22] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[23] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[24] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[25] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[26] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[27] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[28] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[29] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[30] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[31] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[32] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[33] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[34] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[35] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[36] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[37] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[38] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[39] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[40] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[41] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[42] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[43] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[44] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[45] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[46] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[47] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[48] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[49] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[50] COLUMN-LABEL "" FORMAT "x(100)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 157 BY 12.62
         FONT 6 ROW-HEIGHT-CHARS .7 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     cbType AT ROW 1.95 COL 19 COLON-ALIGNED WIDGET-ID 6
     fiFileName AT ROW 3.38 COL 19 COLON-ALIGNED HELP
          "Enter file name to import order"
     btnBrowse AT ROW 3.38 COL 90 WIDGET-ID 20
     lHeaderRow AT ROW 4.81 COL 21 WIDGET-ID 4
     lFieldValidation AT ROW 6 COL 21 WIDGET-ID 24
     rdDuplicates AT ROW 7.19 COL 37 NO-LABEL WIDGET-ID 30
     btnLoad AT ROW 8.38 COL 4 WIDGET-ID 18
     brPreview AT ROW 10.76 COL 4 WIDGET-ID 100
     fiLogFolder AT ROW 23.86 COL 26 COLON-ALIGNED WIDGET-ID 14
     btnBrowseFolder AT ROW 23.86 COL 97 WIDGET-ID 22
     lLogOnly AT ROW 25.05 COL 7 WIDGET-ID 12
     btnProcess AT ROW 26.71 COL 5
     btnCancel AT ROW 26.71 COL 131
     "Duplicates:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 7.19 COL 21 WIDGET-ID 34
     RECT-19 AT ROW 1.24 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 167.4 BY 28.19
         FONT 6.


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
         TITLE              = "Importer"
         HEIGHT             = 28.19
         WIDTH              = 161.8
         MAX-HEIGHT         = 28.19
         MAX-WIDTH          = 169.2
         VIRTUAL-HEIGHT     = 28.19
         VIRTUAL-WIDTH      = 169.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 6
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB brPreview btnLoad FRAME-A */
ASSIGN 
       fiFileName:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brPreview
/* Query rebuild information for BROWSE brPreview
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttImportData WHERE NOT ttImportData.lHeader .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brPreview */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Importer */
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
ON WINDOW-CLOSE OF C-Win /* Importer */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowse C-Win
ON CHOOSE OF btnBrowse IN FRAME FRAME-A /* Find File */
DO:
        RUN pFileBrowse(fiFileName:HANDLE).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder C-Win
ON CHOOSE OF btnBrowseFolder IN FRAME FRAME-A /* Select Log Folder */
DO:
        RUN pFolderBrowse(fiLogFolder:HANDLE).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME FRAME-A /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad C-Win
ON CHOOSE OF btnLoad IN FRAME FRAME-A /* 1. Load Import File */
DO:
        DEFINE VARIABLE lProcess AS LOGICAL INIT NO NO-UNDO.
   
        
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        
        RUN pLoad (fiFileName:HANDLE, cbType).
   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProcess
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProcess C-Win
ON CHOOSE OF btnProcess IN FRAME FRAME-A /* 2. Process Import */
DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        RUN pRunProcess(lLogOnly).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbType C-Win
ON VALUE-CHANGED OF cbType IN FRAME FRAME-A /* Import Type */
DO:
        ASSIGN {&DISPLAYED-OBJECTS}.
        RUN pSetType(cbType).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFileName C-Win
ON HELP OF fiFileName IN FRAME FRAME-A /* Import File */
DO:
        RUN pFileBrowse(fiFileName:HANDLE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brPreview
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
    RUN util/ImportProcs.p PERSISTENT SET ghdImportProcs.
    RUN enable_UI.
    fiLogFolder:SCREEN-VALUE = fGetLogFolder(ipcCompany).
    RUN pInitializeTypes(cbType:HANDLE, ipcTypes).
    
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
  DISPLAY cbType fiFileName lHeaderRow lFieldValidation rdDuplicates fiLogFolder 
          lLogOnly 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-19 cbType fiFileName btnBrowse lHeaderRow lFieldValidation 
         rdDuplicates btnLoad brPreview fiLogFolder btnBrowseFolder lLogOnly 
         btnProcess btnCancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckContinue C-Win 
PROCEDURE pCheckContinue :
/*------------------------------------------------------------------------------
         Purpose: Prompts to continue and returns logical
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER iplGo AS LOGICAL NO-UNDO.

    MESSAGE "Are you sure you want to " ipcMessage "?"
        VIEW-AS ALERT-BOX  QUESTION BUTTON YES-NO UPDATE iplGo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileBrowse C-Win 
PROCEDURE pFileBrowse :
/*------------------------------------------------------------------------------
                 Purpose: Launches window lookup file browser and sets screen value of passed object
                 Notes:
                ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFileEntry AS HANDLE.

    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault  AS CHARACTER NO-UNDO.
        
    cDefault = fGetDefaultImportFolder(ipcCompany).
    SYSTEM-DIALOG GET-FILE cFileName 
        TITLE "Select File to Import"
        FILTERS "Valid File Types (*.csv,*.xls,*.xlsx)" "*.csv,*.xls,*.xlsx",
        "Excel Files (*.xls,*.xlsx)" "*.xls,*.xlsx",
        "Comma Separated Files (*.csv)" "*.csv",
        "All Files    (*.*) " "*.*"
        INITIAL-DIR  cDefault
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK.
      
    IF lOK THEN iphdFileEntry:SCREEN-VALUE = cFileName.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFolderBrowse C-Win 
PROCEDURE pFolderBrowse :
/*------------------------------------------------------------------------------
         Purpose: Launches Windows Lookup for folder browser and sets creen value of passed object
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFolderEntry AS HANDLE.
    
    DEFINE VARIABLE cFolder AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK     AS LOGICAL   NO-UNDO.
        
    SYSTEM-DIALOG GET-DIR cFolder 
        TITLE "Select Folder for Log File"
        UPDATE lOK.
      
    IF lOK THEN iphdFolderEntry:SCREEN-VALUE = cFolder.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitializeTypes C-Win 
PROCEDURE pInitializeTypes :
/*------------------------------------------------------------------------------
                     Purpose: Initializes the Type Option Selection
                     Notes:
                    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdCombo AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcTypesList AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cLabelList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValueList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jIndex     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cInitial   AS CHARACTER NO-UNDO.

    ASSIGN 
        cLabelList = 'Accounts Payable (VU1),Accounts Receivable (AU1),Customers (AF1),Customer ShipTos (AF1 - ShipTo Tab),' +
                'Finished Goods (IF1),General Ledger Accounts (GF2),Vendors (VF1)' 
        cValueList = 'AP,AR,Cust,ShipTo,FG,GL,Vend'.

    IF ipcTypesList EQ '' OR ipcTypesList EQ 'ALL' THEN
        ASSIGN 
            ipcTypesList = cValueList.

    iphdCombo:LIST-ITEM-PAIRS = ?.

    DO iIndex = 1 TO NUM-ENTRIES(ipcTypesList):
        jIndex = LOOKUP(ENTRY(iIndex,ipcTypesList),cValueList).
        IF jIndex GT 0 THEN 
            iphdCombo:ADD-LAST (ENTRY(jIndex,cLabelList),ENTRY(jIndex,cValueList)).
    END. 

    ASSIGN 
        iphdCombo:INNER-LINES  = iphdCombo:NUM-ITEMS
        iphdCombo:SCREEN-VALUE = iphdCombo:ENTRY (1)
        .
    RUN pSetType(ENTRY(1,ipcTypesList)).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoad C-Win 
PROCEDURE pLoad :
/*------------------------------------------------------------------------------
         Purpose: Loads the contents of the import file into temp-table
         Notes: Validates file first
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdImportFileName AS HANDLE.
    DEFINE INPUT PARAMETER cType AS CHARACTER.
    
    DEFINE VARIABLE lGo               AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cFile             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUpdateDuplicates AS LOGICAL   NO-UNDO.
    
    cFile = iphdImportFileName:SCREEN-VALUE.
    lUpdateDuplicates = rdDuplicates EQ 1.
    
    RUN pValidFile(cFile, OUTPUT lGo).
    IF lGo THEN 
    DO:
        SESSION:SET-WAIT-STATE("general").   
        RUN pSetType IN ghdImportProcs (cbType).
        RUN pConvertExceltoCSV IN ghdImportProcs (cFile, OUTPUT cFile).
        RUN pCheckContinue("load import data from " + cFile, OUTPUT lGo).
        IF lGo THEN 
            RUN pLoad IN ghdImportProcs (ipcCompany, cFile, lHeaderRow, lUpdateDuplicates, lFieldValidation, OUTPUT lGo).
        IF lGo THEN 
        DO:
            RUN pShowPreview.
        END.
    END.
    ELSE 
    DO:
        MESSAGE "Import file " cFile " not found."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY "ENTRY" TO iphdImportFileName. 
    END.
    SESSION:SET-WAIT-STATE(""). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunProcess C-Win 
PROCEDURE pRunProcess :
/*------------------------------------------------------------------------------
     Purpose:  Executes the Load of the Data in Preview
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER iplGenerateLogOnly AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lProcess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iUpdated AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAdded AS INTEGER NO-UNDO.
    
    SESSION:SET-WAIT-STATE("general").   
    IF NOT CAN-FIND(FIRST ttImportData WHERE ttImportData.lValid) THEN 
        MESSAGE "No valid data to import" VIEW-AS ALERT-BOX.       
    ELSE DO:
        MESSAGE "Are you ready to process the import file and update or add records in the system?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lProcess.
        IF lProcess THEN DO:
            cLogFile = fGetLogFile(fiLogFolder, cbType).
            RUN pGenerateLog IN ghdImportProcs (cLogFile).
            IF NOT iplGenerateLogOnly THEN DO:
                RUN pProcessImport IN ghdImportProcs(OUTPUT iUpdated, OUTPUT iAdded).
            END.
            MESSAGE "Import process completed." SKIP 
                iUpdated " records updated" SKIP 
                iAdded " records added" SKIP 
                "View log file for details: " cLogFile
                VIEW-AS ALERT-BOX.
            EMPTY TEMP-TABLE ttImportData.
            RUN pShowPreview.
        END.
    END.
    SESSION:SET-WAIT-STATE(""). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetType C-Win 
PROCEDURE pSetType :
/*------------------------------------------------------------------------------
     Purpose:  Sets the type of the Import
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTypeToSet AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdBrowse AS HANDLE.
    DEFINE VARIABLE hdColumn AS HANDLE.
    DEFINE VARIABLE iWidth   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFormat  AS CHARACTER NO-UNDO.

    RUN pSetType IN ghdImportProcs (ipcTypeToSet).
    hdBrowse = brPreview:HANDLE IN FRAME {&FRAME-NAME}.

    FOR EACH ttImportMap
        WHERE ttImportMap.cType EQ ipcTypeToSet:
        hdColumn = hdBrowse:GET-BROWSE-COLUMN (ttImportMap.iImportIndex + 2).
        iWidth = ttImportMap.iColumnWidth.
        IF iWidth EQ 0 THEN 
        DO:
            ASSIGN 
                cFormat = TRIM(ttImportMap.cColumnFormat,"x")
                cFormat = TRIM(cFormat,"(")
                cFormat = TRIM(cFormat,")")
                .
            iWidth = INT(cFormat) NO-ERROR.
            IF iWidth EQ 0 OR iWidth EQ ? OR iWidth GT 200 THEN 
                iWidth = 20.
            ELSE 
                iWidth = iWidth * 3.
        END.
        
        ASSIGN 
            hdColumn:LABEL        = ttImportMap.cColumnLabel
            hdColumn:WIDTH-PIXELS = iWidth
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowPreview C-Win 
PROCEDURE pShowPreview :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        {&OPEN-QUERY-brPreview}

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidFile C-Win 
PROCEDURE pValidFile :
/*------------------------------------------------------------------------------
         Purpose: Returns logical if file or folder is available
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTestFile AS CHARACTER.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL.
    
    FILE-INFO:FILE-NAME = ipcTestFile.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
        oplValid = NO.
    ELSE 
        oplValid = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetDefaultImportFolder C-Win 
FUNCTION fGetDefaultImportFolder RETURNS CHARACTER
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
         Purpose: Returns the character value for ImportFolder NK1 
         Notes: Default path when launching lookup on import file
        ------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'ImportFolder',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    IF lFound THEN 
        RETURN cReturn.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLogFile C-Win 
FUNCTION fGetLogFile RETURNS CHARACTER
    ( ipcFolder AS CHARACTER, ipcBase AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO.

    cLogFile = ipcFolder + "\" + ipcBase + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME) + '.log'.
    
    RETURN cLogFile.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLogFolder C-Win 
FUNCTION fGetLogFolder RETURNS CHARACTER
    ( ipcCompany AS CHARACTER) :
    /*------------------------------------------------------------------------------
         Purpose: Returns the character value for ImportFolder NK1 
         Notes: Default path when launching lookup on import file
        ------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'ImportLog',
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    IF lFound THEN 
    DO:
        IF cReturn EQ ''THEN 
            cReturn = 'C:'.
        ASSIGN 
            cReturn = TRIM(cReturn)
            cReturn = RIGHT-TRIM(cReturn, '\')
            . 
        RETURN cReturn.
    END.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

