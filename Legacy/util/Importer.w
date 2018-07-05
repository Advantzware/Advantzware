&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ipcTypes AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipriContext AS ROWID NO-UNDO.

{util\ttImport.i NEW SHARED}
DEFINE VARIABLE ghdImportProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghdArtiosProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE gcFileType     AS CHARACTER NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-brPreview ttImportData.lValid ttImportData.cImportNote ttImportData.cData[1] ttImportData.cData[2] ttImportData.cData[3] ttImportData.cData[4] ttImportData.cData[5] ttImportData.cData[6] ttImportData.cData[7] ttImportData.cData[8] ttImportData.cData[9] ttImportData.cData[10] ttImportData.cData[11] ttImportData.cData[12] ttImportData.cData[13] ttImportData.cData[14] ttImportData.cData[15] ttImportData.cData[16] ttImportData.cData[17] ttImportData.cData[18] ttImportData.cData[19] ttImportData.cData[20] ttImportData.cData[21] ttImportData.cData[22] ttImportData.cData[23] ttImportData.cData[24] ttImportData.cData[25] ttImportData.cData[26] ttImportData.cData[27] ttImportData.cData[28] ttImportData.cData[29] ttImportData.cData[30] ttImportData.cData[31] ttImportData.cData[32] ttImportData.cData[33] ttImportData.cData[34] ttImportData.cData[35] ttImportData.cData[36] ttImportData.cData[37] ttImportData.cData[38] ttImportData.cData[39] ttImportData.cData[40] ttImportData.cData[41] ttImportData.cData[42] ttImportData.cData[43] ttImportData.cData[44] ttImportData.cData[45] ttImportData.cData[46] ttImportData.cData[47] ttImportData.cData[48] ttImportData.cData[49] ttImportData.cData[50] ttImportData.cData[51] ttImportData.cData[52] ttImportData.cData[53] ttImportData.cData[54] ttImportData.cData[55] ttImportData.cData[56] ttImportData.cData[57] ttImportData.cData[58] ttImportData.cData[59] ttImportData.cData[60] ttImportData.cData[61] ttImportData.cData[62] ttImportData.cData[63] ttImportData.cData[64] ttImportData.cData[65] ttImportData.cData[66] ttImportData.cData[67] ttImportData.cData[68] ttImportData.cData[69] ttImportData.cData[70] ttImportData.cData[71] ttImportData.cData[72] ttImportData.cData[73] ttImportData.cData[74] ttImportData.cData[75] ttImportData.cData[76] ttImportData.cData[77] ttImportData.cData[78] ttImportData.cData[79] ttImportData.cData[80] ttImportData.cData[81] ttImportData.cData[82] ttImportData.cData[83] ttImportData.cData[84] ttImportData.cData[85] ttImportData.cData[86] ttImportData.cData[87] ttImportData.cData[88] ttImportData.cData[89] ttImportData.cData[90] ttImportData.cData[91] ttImportData.cData[92] ttImportData.cData[93] ttImportData.cData[94] ttImportData.cData[95] ttImportData.cData[96] ttImportData.cData[97] ttImportData.cData[98] ttImportData.cData[99] ttImportData.cData[100]   
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
&Scoped-Define ENABLED-OBJECTS RECT-19 cbType btnTemplate ~
lIncludeHelpInTemplate fiFileName btnBrowse lHeaderRow lFieldValidation ~
rdDuplicates rdBlanks btnLoad brPreview fiLogFolder btnBrowseFolder ~
lLogOnly lLogOnlyErrors btnProcess btnCancel 
&Scoped-Define DISPLAYED-OBJECTS cbType lIncludeHelpInTemplate fiFileName ~
lHeaderRow lFieldValidation rdDuplicates rdBlanks fiLogFolder lLogOnly ~
lLogOnlyErrors 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetFile C-Win 
FUNCTION fGetFile RETURNS CHARACTER
    ( ipcFolder AS CHARACTER, ipcBase AS CHARACTER, ipcFileType AS CHARACTER) FORWARD.

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

DEFINE BUTTON btnTemplate 
     LABEL "Generate Template" 
     SIZE 30 BY 1.

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

DEFINE VARIABLE rdBlanks AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Write blank and 0 fields", 1,
"Ignore blank and 0 fields", 2
     SIZE 80 BY .71 NO-UNDO.

DEFINE VARIABLE rdDuplicates AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Overwrite data for duplicates", 1,
"Skip Records if duplicate exists", 2
     SIZE 82 BY .71 NO-UNDO.

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

DEFINE VARIABLE lIncludeHelpInTemplate AS LOGICAL INITIAL no 
     LABEL "Include Help Row in Template" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE lLogOnly AS LOGICAL INITIAL no 
     LABEL "Generate Log Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE lLogOnlyErrors AS LOGICAL INITIAL no 
     LABEL "Only Log Errors" 
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
    ttImportData.cImportNote    FORMAT "x(50)" COLUMN-LABEL "Reason"
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
    ttImportData.cData[51] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[52] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[53] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[54] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[55] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[56] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[57] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[58] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[59] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[60] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[61] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[62] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[63] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[64] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[65] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[66] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[67] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[68] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[69] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[70] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[71] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[72] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[73] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[74] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[75] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[76] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[77] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[78] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[79] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[80] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[81] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[82] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[83] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[84] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[85] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[86] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[87] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[88] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[89] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[90] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[91] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[92] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[93] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[94] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[95] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[96] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[97] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[98] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[99] COLUMN-LABEL "" FORMAT "x(100)"
    ttImportData.cData[100] COLUMN-LABEL "" FORMAT "x(100)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 157 BY 12.62
         FGCOLOR 0 FONT 6 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     cbType AT ROW 1.95 COL 19 COLON-ALIGNED WIDGET-ID 6
     btnTemplate AT ROW 1.95 COL 90 WIDGET-ID 36
     lIncludeHelpInTemplate AT ROW 2.05 COL 121 WIDGET-ID 38
     fiFileName AT ROW 3.38 COL 19 COLON-ALIGNED HELP
          "Enter file name to import order"
     btnBrowse AT ROW 3.38 COL 90 WIDGET-ID 20
     lHeaderRow AT ROW 4.57 COL 21 WIDGET-ID 4
     lFieldValidation AT ROW 5.52 COL 21 WIDGET-ID 24
     rdDuplicates AT ROW 6.48 COL 21 NO-LABEL WIDGET-ID 30
     rdBlanks AT ROW 7.43 COL 21 NO-LABEL WIDGET-ID 42
     btnLoad AT ROW 8.38 COL 4 WIDGET-ID 18
     brPreview AT ROW 10.76 COL 4 WIDGET-ID 100
     fiLogFolder AT ROW 23.86 COL 26 COLON-ALIGNED WIDGET-ID 14
     btnBrowseFolder AT ROW 23.86 COL 97 WIDGET-ID 22
     lLogOnly AT ROW 25.05 COL 7 WIDGET-ID 12
     lLogOnlyErrors AT ROW 25.05 COL 39 WIDGET-ID 40
     btnProcess AT ROW 26.71 COL 5
     btnCancel AT ROW 26.71 COL 131
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
       brPreview:SEPARATOR-FGCOLOR IN FRAME FRAME-A      = 8.

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
        RUN pFileBrowse(fiFileName:HANDLE, gcFileType).  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowseFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowseFolder C-Win
ON CHOOSE OF btnBrowseFolder IN FRAME FRAME-A /* Select Log Folder */
DO:
        RUN pFolderBrowse(fiLogFolder:HANDLE, "").  
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
    DEFINE VARIABLE lIgnoreBlanks AS LOGICAL NO-UNDO.
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        lIgnoreBlanks = rdBlanks EQ 2.
        RUN pRunProcess(lLogOnly, lLogOnlyErrors, lIgnoreBlanks).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTemplate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTemplate C-Win
ON CHOOSE OF btnTemplate IN FRAME FRAME-A /* Generate Template */
DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.      
        RUN pExportTemplate(cbType, lIncludeHelpInTemplate).  
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
        RUN pFileBrowse(fiFileName:HANDLE, gcFileType).
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
    RUN est/ArtiosProcs.p PERSISTENT SET ghdArtiosProcs.
    RUN enable_UI.
    fiLogFolder:SCREEN-VALUE = fGetLogFolder(ipcCompany).
    RUN pInitializeTypes(cbType:HANDLE, ipcTypes).
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
    DELETE OBJECT ghdImportProcs.
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
  DISPLAY cbType lIncludeHelpInTemplate fiFileName lHeaderRow lFieldValidation 
          rdDuplicates rdBlanks fiLogFolder lLogOnly lLogOnlyErrors 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-19 cbType btnTemplate lIncludeHelpInTemplate fiFileName btnBrowse 
         lHeaderRow lFieldValidation rdDuplicates rdBlanks btnLoad brPreview 
         fiLogFolder btnBrowseFolder lLogOnly lLogOnlyErrors btnProcess 
         btnCancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExportTemplate C-Win 
PROCEDURE pExportTemplate :
/*------------------------------------------------------------------------------
             Purpose: Generates a template in the required format for input
             Notes:
            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeHelp AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

    cFile = fGetDefaultImportFolder (ipcCompany).
    cFile = fGetFile (cFile, ipcType, ".csv").
    RUN pGenerateTemplate IN ghdImportProcs (INPUT YES, INPUT iplIncludeHelp, INPUT ipriContext, INPUT-OUTPUT cFile).
    IF cFile NE "" THEN 
    DO:
        fiFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cFile.
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFile)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileBrowse C-Win 
PROCEDURE pFileBrowse :
/*------------------------------------------------------------------------------
                             Purpose: Launches window lookup file browser and sets screen value of passed object
                             Notes:
                            ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdFileEntry AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault  AS CHARACTER NO-UNDO.
        
    cDefault = fGetDefaultImportFolder(ipcCompany).
    CASE ipcType:
        WHEN "ARD" THEN 
            DO:
                RUN GetArtiosDir IN ghdArtiosProcs (ipcCompany, OUTPUT cDefault).
                SYSTEM-DIALOG GET-FILE cFileName 
                    TITLE "Select .ard File to Import"
                    FILTERS "Artios .ard Files (*.ard)" "*.ard",
                    "All Files    (*.*) " "*.*"
                    INITIAL-DIR  cDefault
                    MUST-EXIST
                    USE-FILENAME
                    UPDATE lOK.
            END.
        WHEN "FOL" THEN 
            DO:
                RUN pFolderBrowse(iphdFileEntry,ipcType).
            END.
        OTHERWISE 
        DO: 
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
        END.
    END CASE.
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
    DEFINE INPUT PARAMETER iphdFolderEntry AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFolder  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cDefault AS CHARACTER NO-UNDO.
        
    CASE ipcType:
        WHEN "" THEN
            DO:
                RUN GetArtiosDir IN ghdArtiosProcs (ipcCompany, OUTPUT cDefault).
                SYSTEM-DIALOG GET-DIR cFolder 
                    TITLE "Select Artios Project Folder"
                    INITIAL-DIR cDefault
                    UPDATE lOK.
            END.
        OTHERWISE
        DO:
            SYSTEM-DIALOG GET-DIR cFolder 
                TITLE "Select Folder for Log File"
                UPDATE lOK.
        END.  
    END CASE.
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

    DEFINE VARIABLE iIndex   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jIndex   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cInitial AS CHARACTER NO-UNDO.

    IF ipcTypesList EQ '' OR ipcTypesList EQ 'ALL' THEN
        ASSIGN 
            ipcTypesList = gcTypeList.

    iphdCombo:LIST-ITEM-PAIRS = ?.

    DO iIndex = 1 TO NUM-ENTRIES(ipcTypesList):
        jIndex = LOOKUP(ENTRY(iIndex,ipcTypesList),gcTypeList).
        IF jIndex GT 0 THEN 
            iphdCombo:ADD-LAST (ENTRY(jIndex,gcTypeLabels),ENTRY(jIndex,gcTypeList)).
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
    DEFINE INPUT PARAMETER ipcType AS CHARACTER.
    
    DEFINE VARIABLE lGo               AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cFile             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUpdateDuplicates AS LOGICAL   NO-UNDO.
    
    cFile = iphdImportFileName:SCREEN-VALUE.
    lUpdateDuplicates = rdDuplicates EQ 1.
    
    RUN pValidFile(cFile, OUTPUT lGo).
    IF lGo THEN 
    DO:
        SESSION:SET-WAIT-STATE("general").   
        RUN pSetType IN ghdImportProcs (ipcType).
        RUN pConvertExceltoCSV IN ghdImportProcs (cFile, OUTPUT cFile).
        RUN pCheckContinue("load import data from " + cFile, OUTPUT lGo).
        IF lGo THEN 
            RUN pLoad IN ghdImportProcs (ipcCompany, ipcLocation, cFile, lHeaderRow, lUpdateDuplicates, lFieldValidation, gcFileType, OUTPUT lGo).
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
    DEFINE INPUT PARAMETER iplLogErrorsOnly AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lProcess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iUpdated AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iAdded   AS INTEGER   NO-UNDO.
    
    SESSION:SET-WAIT-STATE("general").   
    IF NOT CAN-FIND(FIRST ttImportData WHERE ttImportData.lValid) THEN 
        MESSAGE "No valid data to import" VIEW-AS ALERT-BOX.       
    ELSE 
    DO:
        MESSAGE "Are you ready to process the import file and update or add records in the system?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lProcess.
        IF lProcess THEN 
        DO:
            cLogFile = fGetFile(fiLogFolder, cbType,".log").
            RUN pGenerateLog IN ghdImportProcs (cLogFile, iplLogErrorsOnly).
            IF NOT iplGenerateLogOnly THEN 
            DO:
                RUN pProcessImport IN ghdImportProcs(iplIgnoreBlanks, OUTPUT iUpdated, OUTPUT iAdded).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDisplay C-Win 
PROCEDURE pSetDisplay :
/*------------------------------------------------------------------------------
 Purpose: Sets display elements of the UI based on type integer
 Notes:  0 = default, 1 = Artios, 2 = Updates Allowed
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipiType AS INTEGER NO-UNDO.

DEFINE VARIABLE lHideOptions AS LOGICAL NO-UNDO.
DEFINE VARIABLE cLoadLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProcessLabel AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHideUpdateOptions AS LOGICAL NO-UNDO.

    CASE ipiType:
        WHEN 1 THEN 
        DO:
            ASSIGN 
                lHideOptions = YES
                lHideUpdateOptions = YES
                cLoadLabel = "1. Load ARD File"
                cProcessLabel = "2. Create Estimate"
                .
                
        END.
        OTHERWISE 
        DO:
            ASSIGN 
                lHideOptions = NO
                lHideUpdateOptions = ipiType NE 2
                cLoadLabel = "1. Load Import File"
                cProcessLabel = "2. Process Import"
                .
            
        END.    
    END CASE.
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
        btnLoad:LABEL = cLoadLabel
        btnProcess:LABEL = cProcessLabel
        lHeaderRow:HIDDEN = lHideOptions
        btnTemplate:HIDDEN = lHideOptions
        lIncludeHelpInTemplate:HIDDEN = lHideOptions
        rdDuplicates:HIDDEN = lHideUpdateOptions
        lFieldValidation:HIDDEN = lHideOptions
        rdBlanks:HIDDEN = lHideUpdateOptions
        .
    
END.
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

    DEFINE VARIABLE hdBrowse     AS HANDLE.
    DEFINE VARIABLE hdColumn     AS HANDLE.
    DEFINE VARIABLE iWidth       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFormat      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColumnIndex AS INTEGER   NO-UNDO.

    CASE ipcTypeToSet:
        WHEN "ttImportEstimateARD" THEN  
            DO:
                RUN pSetDisplay(1).    
                gcFileType = "ARD".
            END.
        WHEN "ttImportEstimateARDP" THEN 
            DO:
                RUN pSetDisplay(1).
                gcFileType = "FOL".
            END.
        OTHERWISE 
        DO:
            IF LOOKUP(ipcTypeToSet,gcUpdatesAllowedTypes) GT 0 THEN
                RUN pSetDisplay(2). 
            ELSE                 
                RUN pSetDisplay(0).
            gcFileType = "CSV".
        END.
    END CASE.        
    RUN pSetType IN ghdImportProcs (ipcTypeToSet).
    hdBrowse = brPreview:HANDLE IN FRAME {&FRAME-NAME}.
    
    DO iColumnIndex = 3 TO hdBrowse:NUM-COLUMNS:
        hdColumn = hdBrowse:GET-BROWSE-COLUMN (iColumnIndex).
        hdColumn:LABEL = "".
    END.
    FOR EACH ttImportMap:
        hdColumn = hdBrowse:GET-BROWSE-COLUMN (ttImportMap.iImportIndex + 2).
        iWidth = ttImportMap.iColumnWidth.
        
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
    DO:
        IF cReturn EQ ''THEN 
            cReturn = 'C:'.
        ASSIGN 
            cReturn = TRIM(cReturn)
            cReturn = RIGHT-TRIM(cReturn, '\')
            . 
        RETURN cReturn .
    END.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetFile C-Win 
FUNCTION fGetFile RETURNS CHARACTER
    ( ipcFolder AS CHARACTER, ipcBase AS CHARACTER, ipcFileType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO.
    
    ipcBase = REPLACE(ipcBase,"ttImport","").
    cLogFile = ipcFolder + "\" + ipcBase + "_" + STRING(YEAR(TODAY)) + "_" + STRING(MONTH(TODAY)) + "_" + STRING(DAY(TODAY)) + "_" + STRING(TIME) + ipcFileType.
  
    
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

