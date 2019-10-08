&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME winMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS winMain 
/*------------------------------------------------------------------------

  File:                 XRefAnalcodeViewer.w ( Had to be renamed because
                                               protools/EnableSourceEditor.i 
                                               appears to be testing on the 
                                               filename. )

  Description:          Analyse XRef files

  Input Parameters:     <none>

  Output Parameters:    <none>

  Author:               Matt Verrinder
  
  Created:              ages ago!
  Modified:             2006-12-28 Jan Keirse <jan.keirse@tvh.be> 
                        Modified to support 
                          * syntax highlighting 
                          * search functionality
                          * made resizable
                          * Go from line of code to related Xrefs (more or less)
                          * Added Analyze(ipcFilename as character) procedure to analyze on demand
                            (for integration in ABHack)
                          
                        
                                               
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE ttXRef NO-UNDO
  FIELD SeqNo      AS INTEGER
  FIELD ProcName   AS CHAR
  FIELD SourceFile AS CHAR
  FIELD LineNo     AS INTEGER
  FIELD RefType    AS CHAR
  FIELD ObjectId   AS CHAR
  FIELD Other2   AS CHAR
  FIELD Other3   AS CHAR
  FIELD Other4   AS CHAR
  FIELD Other5   AS CHAR
  FIELD Other6   AS CHAR
  INDEX idxLineNo IS PRIMARY SeqNo
  INDEX idxRefType RefType.

DEFINE TEMP-TABLE ttUsedFields NO-UNDO LABEL ""
 FIELD cfield AS CHARACTER
 INDEX cfield IS UNIQUE cfield.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmMain
&Scoped-define BROWSE-NAME brwXRef

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttXRef

/* Definitions for BROWSE brwXRef                                       */
&Scoped-define FIELDS-IN-QUERY-brwXRef SourceFile LineNo RefType ObjectId Other2   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwXRef   
&Scoped-define SELF-NAME brwXRef
&Scoped-define QUERY-STRING-brwXRef FOR EACH ttXRef NO-LOCK
&Scoped-define OPEN-QUERY-brwXRef OPEN QUERY {&SELF-NAME} FOR EACH ttXRef NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brwXRef ttXRef
&Scoped-define FIRST-TABLE-IN-QUERY-brwXRef ttXRef


/* Definitions for FRAME frmMain                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-frmMain ~
    ~{&OPEN-QUERY-brwXRef}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnImport cmbRefType fiObjectID gcFieldList ~
giBetweenDown giBetweenUp btnClose brwXRef selDetails fiSearch btnSearch ~
btnAnalysis edt fiNotFound 
&Scoped-Define DISPLAYED-OBJECTS cmbRefType fiObjectID gcFieldList ~
giBetweenDown giBetweenUp selDetails fiSearch edt fiNotFound 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR winMain AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAnalysis 
     LABEL "^" 
     SIZE 5 BY 1.14 TOOLTIP "Show the xref information for this line".

DEFINE BUTTON btnClose 
     LABEL "Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnImport 
     LABEL "Analyse Program..." 
     SIZE 21.2 BY 1.14.

DEFINE BUTTON btnSearch 
     LABEL "Search" 
     SIZE 10 BY 1.14.

DEFINE VARIABLE cmbRefType AS CHARACTER FORMAT "X(256)":U 
     LABEL "RefType" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 25
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE edt AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL LARGE
     SIZE 172 BY 19.29
     FONT 0 NO-UNDO.

DEFINE VARIABLE fiNotFound AS CHARACTER FORMAT "X(256)":U INITIAL "NOT FOUND" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiObjectID AS CHARACTER FORMAT "X(256)":U 
     LABEL "ObjectId" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fiSearch AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 81 BY 1 NO-UNDO.

DEFINE VARIABLE gcFieldList AS CHARACTER FORMAT "X(10000)":U 
     LABEL "usedFieldList" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Press Return to build a FIELDS list based on the used field"
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE giBetweenDown AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Line between" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Lowest line number to filter the ACCESS Fields list.  Ignored if 0"
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE giBetweenUp AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "and" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Highest line number to filter the ACCESS Fields list.  Ignored if 0"
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE selDetails AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 7.33 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwXRef FOR 
      ttXRef SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwXRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwXRef winMain _FREEFORM
  QUERY brwXRef DISPLAY
      SourceFile FORMAT "x(200)" WIDTH-CHARS 40
 LineNo     FORMAT ">>>>>>>>" 
 RefType    FORMAT "x(15)" 
 ObjectId   FORMAT "x(200)" WIDTH-CHARS 40
 Other2     FORMAT "X(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 135 BY 7.38
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmMain
     btnImport AT ROW 1.24 COL 2 HELP
          " Analyse program"
     cmbRefType AT ROW 1.24 COL 31 COLON-ALIGNED HELP
          " Reference filter"
     fiObjectID AT ROW 1.24 COL 62 COLON-ALIGNED
     gcFieldList AT ROW 1.24 COL 105 COLON-ALIGNED
     giBetweenDown AT ROW 1.24 COL 150 COLON-ALIGNED
     giBetweenUp AT ROW 1.24 COL 163 COLON-ALIGNED
     btnClose AT ROW 1.24 COL 218 HELP
          " Close window"
     brwXRef AT ROW 2.67 COL 2 HELP
          " Xref data, double-click to show source"
     selDetails AT ROW 2.67 COL 138 HELP
          " XRef details" NO-LABEL
     fiSearch AT ROW 10.29 COL 8.6 COLON-ALIGNED
     btnSearch AT ROW 10.29 COL 92
     btnAnalysis AT ROW 10.29 COL 117
     edt AT ROW 11.71 COL 1 HELP
          " Source" NO-LABEL
     fiNotFound AT ROW 10.52 COL 101 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 232.8 BY 36.81
         FONT 4.


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
  CREATE WINDOW winMain ASSIGN
         HIDDEN             = YES
         TITLE              = "Program Analysis"
         HEIGHT             = 30.1
         WIDTH              = 172.2
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
/* SETTINGS FOR WINDOW winMain
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frmMain
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB brwXRef btnClose frmMain */
ASSIGN 
       brwXRef:COLUMN-RESIZABLE IN FRAME frmMain       = TRUE
       brwXRef:COLUMN-MOVABLE IN FRAME frmMain         = TRUE.

ASSIGN 
       gcFieldList:HIDDEN IN FRAME frmMain           = TRUE.

ASSIGN 
       giBetweenDown:HIDDEN IN FRAME frmMain           = TRUE.

ASSIGN 
       giBetweenUp:HIDDEN IN FRAME frmMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(winMain)
THEN winMain:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwXRef
/* Query rebuild information for BROWSE brwXRef
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttXRef NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwXRef */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME winMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winMain winMain
ON CTRL-F OF winMain /* Program Analysis */
ANYWHERE DO:
  APPLY "entry"  TO fiSearch IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winMain winMain
ON END-ERROR OF winMain /* Program Analysis */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winMain winMain
ON WINDOW-CLOSE OF winMain /* Program Analysis */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winMain winMain
ON WINDOW-RESIZED OF winMain /* Program Analysis */
DO:
  IF SELF:WIDTH-PIXELS > FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = SELF:WIDTH-PIXELS.

  IF SELF:HEIGHT-PIXEL > FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN 
   FRAME {&FRAME-NAME}:HEIGHT-PIXELS = SELF:HEIGHT-PIXEL.


  ASSIGN btnClose:X = SELF:WIDTH-PIXELS - 80
    brwXref:WIDTH-PIXELS = (SELF:WIDTH-PIXELS - 15 ) * 0.7149 
    selDetails:X = brwXref:WIDTH-PIXELS + 10
    selDetails:WIDTH-PIXELS = (SELF:WIDTH-PIXELS - 15) * 0.2859
    edt:WIDTH-PIXELS = SELF:WIDTH-PIXELS - 10
    edt:HEIGHT-PIXELS = SELF:HEIGHT-PIXELS - 228.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frmMain
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmMain winMain
ON F9 OF FRAME frmMain
ANYWHERE
DO:
  APPLY "choose" TO btnSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwXRef
&Scoped-define SELF-NAME brwXRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwXRef winMain
ON DEFAULT-ACTION OF brwXRef IN FRAME frmMain
DO:
    /* 05-APR-2007 sla: moved code from mouse-doubleclick to default-action
    and the return key */
    IF AVAILABLE(ttXRef) THEN
        RUN ImportListing (ttXRef.SourceFile, ttXRef.LineNo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwXRef winMain
ON RETURN OF brwXRef IN FRAME frmMain
DO:
    /* 05-APR-2007 sla: moved code from mouse-doubleclick to default-action
    and the return key */
    IF AVAILABLE(ttXRef) THEN
        RUN ImportListing (ttXRef.SourceFile, ttXRef.LineNo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwXRef winMain
ON ROW-DISPLAY OF brwXRef IN FRAME frmMain
DO:
    IF AVAILABLE(ttXRef) AND ttXRef.RefType = "SEARCH" AND ttXRef.Other3 = "WHOLE-INDEX" THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
          ttXRef.SourceFile:BGCOLOR IN BROWSE brwXRef = 12
          ttXRef.LineNo:BGCOLOR = 12
          ttXRef.RefType:BGCOLOR = 12
          ttXRef.ObjectId:BGCOLOR = 12.
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwXRef winMain
ON VALUE-CHANGED OF brwXRef IN FRAME frmMain
DO:
    IF AVAILABLE(ttXRef) THEN 
    DO:
        ASSIGN selDetails:LIST-ITEMS = "".
        
        selDetails:ADD-LAST("Procedure Name: " + ttXRef.ProcName).
        selDetails:ADD-LAST("Source File: " + ttXRef.SourceFile).
        selDetails:ADD-LAST("Line No: " + STRING(ttXRef.LineNo)).
        selDetails:ADD-LAST("Reference Type: " + ttXRef.RefType).
        
        CASE ttXRef.RefType:
            WHEN "COMPILE" 
             OR WHEN "RUN" THEN 
            DO:
                selDetails:ADD-LAST("Procedure Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "STRING" THEN 
            DO:
                selDetails:ADD-LAST("CharString: " + ttXRef.ObjectId).
                selDetails:ADD-LAST("MaxLength: " + ttXRef.Other2).
                selDetails:ADD-LAST("Justification: " + ttXRef.Other3).
                selDetails:ADD-LAST("Trans: " + ttXRef.Other4).
                selDetails:ADD-LAST("Format: " + ttXRef.Other5).
            END.
            
            WHEN "INCLUDE" THEN 
            DO:
                selDetails:ADD-LAST("Include File: " + ttXRef.ObjectId).
            END.
            
            WHEN "CREATE" OR WHEN "DELETE" THEN 
            DO:
                selDetails:ADD-LAST("Table Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "REFERENCE" THEN 
            DO:
                selDetails:ADD-LAST("Table Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "SORT-ACCESS"
             OR WHEN "ACCESS"
             OR WHEN "UPDATE" THEN 
            DO:
                IF ttXRef.Other2 = "" THEN
                    selDetails:ADD-LAST("Variable Name: " + ttXRef.ObjectId).
                ELSE
                DO:
                    selDetails:ADD-LAST("Table Name: " + ttXRef.ObjectId).
                    selDetails:ADD-LAST("Field Name: " + ttXRef.Other2).
                END.
            END.
            
            WHEN "SEARCH" THEN 
            DO:
                selDetails:ADD-LAST("Table Name: " + ttXRef.ObjectId).
                selDetails:ADD-LAST("Index Name: " + ttXRef.Other2).
                selDetails:ADD-LAST("Whole Index: " + ttXRef.Other3).
            END.
            
            WHEN "NEW-SHR-VARIABLE"
             OR WHEN "GLOBAL-VARIABLE" THEN 
            DO:
                selDetails:ADD-LAST("Variable Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "NEW-SHR-FRAME"
             OR WHEN "SHR-FRAME" THEN 
            DO:
                selDetails:ADD-LAST("Frame Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "NEW-SHR-WORKTABLE"
             OR WHEN "SHR-WORKTABLE" THEN 
            DO:
                selDetails:ADD-LAST("Worktable Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "FUNCTION" THEN 
            DO:
                selDetails:ADD-LAST("Function Name: " + ttXRef.ObjectId).
            END.
            
            WHEN "PROCEDURE" THEN DO:
                selDetails:ADD-LAST("Procedure Name: " + ttXRef.ObjectId).
            END.
            
            OTHERWISE DO:
                selDetails:ADD-LAST("Unknown Reference Type").
            END.
        END CASE.
    END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAnalysis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnalysis winMain
ON CHOOSE OF btnAnalysis IN FRAME frmMain /* ^ */
DO:
  DEFINE VARIABLE iLine AS INTEGER     NO-UNDO.
  
  IF SELF:LABEL = "*" THEN DO:
      OPEN QUERY brwXRef FOR EACH ttXRef BY SeqNo.
      SELF:LABEL = "^".
      RETURN.
  END.
  
  IF SELF:LABEL = "^" THEN SELF:LABEL = "*".
  
  iLine = edt:CURSOR-LINE.
  FIND FIRST ttxref NO-LOCK WHERE ttxref.lineno  = iLine NO-ERROR.
  
  DO WHILE NOT AVAIL ttxref AND iLine > 1:
    iLine = iLine - 1.
    FIND FIRST ttxref NO-LOCK WHERE ttxref.lineno  = iLine NO-ERROR.
  END.
  
  IF AVAIL ttxref THEN iLine = ttxref.lineno.
  OPEN QUERY brwXRef FOR EACH ttXRef WHERE ttXref.lineno = iLine BY SeqNo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose winMain
ON CHOOSE OF btnClose IN FRAME frmMain /* Close */
DO:
    APPLY "WINDOW-CLOSE":U TO {&WINDOW-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport winMain
ON CHOOSE OF btnImport IN FRAME frmMain /* Analyse Program... */
DO:
    DEF VAR vOkPressed   AS LOGICAL NO-UNDO.
    DEF VAR vProgramName AS CHAR NO-UNDO.
    DEF VAR vXRefFile    AS CHAR NO-UNDO.
    DEF VAR vSXRefFile   AS CHAR NO-UNDO.
    DEF VAR vListingFile AS CHAR NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE vProgramName
      FILTERS "Progress Source" "*.w,*.p",
              "All Files" "*.*"
      INITIAL-FILTER 1
      MUST-EXIST
      TITLE "Select Source File..."
      UPDATE vOkPressed.
            
    IF vOkPressed THEN 
    DO:
        SESSION:SET-WAIT-STATE("GENERAL").
        ASSIGN {&WINDOW-NAME}:TITLE = SUBSTITUTE("Program Analysis - &1", vProgramName).
        RUN AnalyseProgram (vProgramName).
        SESSION:SET-WAIT-STATE("").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch winMain
ON CHOOSE OF btnSearch IN FRAME frmMain /* Search */
DO:
  fiNotFound:visible =  NOT edt:SEARCH( fiSearch:SCREEN-VALUE,FIND-NEXT-OCCURRENCE + FIND-SELECT
                            + FIND-WRAP-AROUND).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbRefType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbRefType winMain
ON VALUE-CHANGED OF cmbRefType IN FRAME frmMain /* RefType */
DO:
    RUN RefreshBrowser.
    ASSIGN
     gcFieldList:VISIBLE   = SELF:SCREEN-VALUE = "ACCESS"
     giBetweenDown:VISIBLE = gcFieldList:VISIBLE
     giBetweenUp:VISIBLE   = gcFieldList:VISIBLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiObjectID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiObjectID winMain
ON VALUE-CHANGED OF fiObjectID IN FRAME frmMain /* ObjectId */
DO:
  RUN refreshBrowser.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSearch winMain
ON RETURN OF fiSearch IN FRAME frmMain /* Search */
DO:
  APPLY "choose" TO btnSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME gcFieldList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gcFieldList winMain
ON RETURN OF gcFieldList IN FRAME frmMain /* usedFieldList */
DO:
  DEFINE BUFFER bttxref FOR ttxref.
  
  IF NOT AVAILABLE ttxref THEN RETURN.
  fiObjectID:SCREEN-VALUE = ttxref.ObjectId.
  ASSIGN giBetweenDown.
  ASSIGN giBetweenUp.
  gcFieldList = "".
  
  EMPTY TEMP-TABLE ttusedfields.
  
  FOR EACH bttxref WHERE bttxref.RefType = "ACCESS" 
   AND bttxref.ObjectId = fiObjectID:SCREEN-VALUE
   AND bttxref.LineNo >= giBetweenDown
   AND IF giBetweenUp = 0 THEN YES ELSE bttxref.LineNo <= giBetweenUp:
      FIND FIRST ttusedfields WHERE ttusedfields.cfield = bttxref.Other2 NO-ERROR.
      IF NOT AVAILABLE ttusedfields THEN DO:
          CREATE ttusedfields.
          ASSIGN ttusedfields.cfield = bttxref.Other2.
      END.
  END.
  
  
  FOR EACH ttusedfields:
      gcFieldList = gcFieldList + " " + ttusedfields.cfield.
  END.
  gcFieldList = SUBSTRING(gcFieldList, 2).
  gcFieldList:SCREEN-VALUE = gcFieldList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK winMain 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ASSIGN {&WINDOW-NAME}:STATUS-AREA-FONT = FRAME {&FRAME-NAME}:FONT.

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
   DEFINE VARIABLE hEdt AS HANDLE      NO-UNDO.
   hEdt = edt:HANDLE. /* extra variable to allow usage with 9.1C */
  {protools/abhack/EnableSourceEditor.i hedt}
  
  
  RUN enable_UI.
  gcFieldList:VISIBLE = NO.
  giBetweenDown:VISIBLE = NO.
  giBetweenUp:VISIBLE = NO.
  fiNotFound:VISIBLE = FALSE.
    &IF DEFINED(UIB_is_Running) NE 0 &THEN
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
    &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ADEPersistent winMain 
PROCEDURE ADEPersistent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnalyseProgram winMain 
PROCEDURE AnalyseProgram :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM ipProgramName AS CHAR NO-UNDO.
    
    DEF VAR vXRefFile   AS CHAR INITIAL "XRef.txt" NO-UNDO.
    DEF VAR vSXRefFile  AS CHAR INITIAL "SXRef.txt" NO-UNDO.
    DEF VAR vDebugFile  AS CHAR INITIAL "Debug.txt" NO-UNDO.
    
    DEF VAR vCount      AS INT NO-UNDO.
    DEF VAR vFatalError AS LOGICAL NO-UNDO.
    
    
    FOR EACH ttXRef:
        DELETE ttXRef.
    END.
    

    IF SEARCH(ipProgramName) <> ? THEN DO:
        ASSIGN vFatalError = FALSE.
        
        COMPILE 
          VALUE(ipProgramName) 
          SAVE
          XREF VALUE(vXRefFile)
          STRING-XREF VALUE(vSXRefFile)
          DEBUG-LIST VALUE(vDebugFile)
          NO-ERROR.
          
        IF COMPILER:ERROR THEN DO WITH FRAME {&FRAME-NAME}:
            DO vCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                selDetails:ADD-LAST(REPLACE(ERROR-STATUS:GET-MESSAGE(vCount), "~n", ",")).
                IF ERROR-STATUS:GET-NUMBER(vCount) <> 2884 
                THEN ASSIGN vFatalError = TRUE.
            END.
            
            DISPLAY selDetails.
            
            RUN ImportListing (COMPILER:FILE-NAME, COMPILER:ERROR-ROW).
            
            MESSAGE
             "Compilation error(s)." SKIP
             VIEW-AS ALERT-BOX ERROR TITLE "Compilation Error...".
        END.
        
        IF NOT vFatalError THEN
        DO:
            RUN ImportXRef (vXRefFile).
            /*
            RUN ImportSXRef (vSXRefFile).
            */
            RUN ImportListing (ipProgramName, 1).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE analyze winMain 
PROCEDURE analyze :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icFilename AS CHARACTER   NO-UNDO.
SESSION:SET-WAIT-STATE("GENERAL").
ASSIGN {&WINDOW-NAME}:TITLE = SUBSTITUTE("Program Analysis - &1", icFilename).
RUN AnalyseProgram (icFilename).
SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI winMain  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(winMain)
  THEN DELETE WIDGET winMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI winMain  _DEFAULT-ENABLE
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
  DISPLAY cmbRefType fiObjectID gcFieldList giBetweenDown giBetweenUp selDetails 
          fiSearch edt fiNotFound 
      WITH FRAME frmMain IN WINDOW winMain.
  ENABLE btnImport cmbRefType fiObjectID gcFieldList giBetweenDown giBetweenUp 
         btnClose brwXRef selDetails fiSearch btnSearch btnAnalysis edt 
         fiNotFound 
      WITH FRAME frmMain IN WINDOW winMain.
  {&OPEN-BROWSERS-IN-QUERY-frmMain}
  VIEW winMain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportListing winMain 
PROCEDURE ImportListing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM ipListingFile AS CHAR NO-UNDO.
    DEF INPUT PARAM ipGotoLine    AS INT NO-UNDO.
    
    DEF VAR vStartOffset AS INT NO-UNDO.
    DEF VAR vEndOffset   AS INT NO-UNDO.
                                            
    ASSIGN ipListingFile = SEARCH(ipListingFile).
    IF ipListingFile <> ? THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF edt:PRIVATE-DATA <> ipListingFile AND edt:READ-FILE(ipListingFile)
        THEN
            ASSIGN edt:PRIVATE-DATA = ipListingFile.

        /* 05-APR-2007 sla: revision to select a given line, and to manage to make it
         appear at the middle of the view port */
        IF ipGotoLine > 0 THEN 
        DO:
            edt:CURSOR-LINE = ipGotoLine + edt:HEIGHT-CHARS / 1.3 NO-ERROR. /* the point of this first assign is to position the current line a at the third of the view port */
            edt:CURSOR-LINE = ipGotoLine NO-ERROR. /* then go back to the wanted line */
              
            edt:SOURCE-COMMAND('deselect', '').
            edt:SOURCE-COMMAND('select_line', '').
        END.                        
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportXRef winMain 
PROCEDURE ImportXRef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER ipXRefFile AS CHAR NO-UNDO.
    
    DEF VAR vRefList   AS CHAR NO-UNDO.
    DEF VAR vProcList  AS CHAR NO-UNDO.
    DEF VAR vLastProc  AS CHAR NO-UNDO.
    DEF VAR vLastFile  AS CHAR NO-UNDO.
    DEF VAR vFilename  AS CHAR NO-UNDO.
    DEF VAR vCount     AS INTEGER NO-UNDO.
    
    INPUT FROM VALUE(ipXRefFile) NO-ECHO.
    
    REPEAT:
        CREATE ttXRef.
        ASSIGN
          vCount      = vCount + 1
          ttXRef.SeqNo = vCount.
        IMPORT DELIMITER " " ttXRef EXCEPT SeqNo.

        IF ttXRef.ProcName <> vLastProc THEN 
        DO:
            IF ttXRef.ProcName <> vLastFile THEN 
            DO:
                ASSIGN
                  vProcList = vProcList + ttXRef.ProcName + ","
                  vLastProc = ttXRef.ProcName
                  vLastFile = ttXRef.SourceFile.
            END.
        END.
        
        IF NOT CAN-DO(vRefList, ttXRef.RefType) THEN 
        DO:
            ASSIGN vRefList = vRefList + ttXRef.RefType + ",".
        END.
        
/*
        IF ttXRef.RefType = "Include" THEN DO:
            ASSIGN 
              vFilename = TRIM(ttXRef.ObjectId)
              vFilename = ENTRY(1, vFilename, " ").
            RUN ImportListing (vFilename, 1).
        END.
*/
    END.
    
    INPUT CLOSE.

    FOR EACH ttXRef WHERE ttXRef.ProcName = "".
        DELETE ttXRef.
    END.
    
    ASSIGN 
      vRefList = "WHOLE-INDEX" + "," + vRefList
      cmbRefType:LIST-ITEMS IN FRAME {&FRAME-NAME} = vRefList + "-"
      cmbRefType:SCREEN-VALUE                      = "-".
    
    RUN RefreshBrowser.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowser winMain 
PROCEDURE RefreshBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN cmbRefType.
      
        IF cmbRefType = "-" 
        THEN 
            OPEN QUERY brwXRef
              FOR EACH ttXRef NO-LOCK
                WHERE ttXref.objectId BEGINS fiObjectId:SCREEN-VALUE
                BY SeqNo.
        ELSE IF cmbRefType = "WHOLE-INDEX" THEN
          OPEN QUERY brwXRef
            FOR EACH ttXRef NO-LOCK 
              WHERE ttXRef.RefType = "SEARCH"
                AND ttXref.Other3 = "WHOLE-INDEX"
                AND ttXref.objectId BEGINS fiObjectId:SCREEN-VALUE
                  BY SeqNo.
        ELSE
            OPEN QUERY brwXRef
              FOR EACH ttXRef NO-LOCK
                WHERE ttXRef.RefType = cmbRefType
                  AND ttXref.objectId BEGINS fiObjectId:SCREEN-VALUE
                  BY SeqNo.
    END.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEditorFont winMain 
PROCEDURE setEditorFont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piFont AS INTEGER     NO-UNDO.

edt:FONT IN FRAME {&FRAME-NAME} = piFont.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

