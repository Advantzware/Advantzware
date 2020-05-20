&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/qryDesign.w

  Description: Dynamic Subject Query Graphical Designer

  Input Parameters: Subject ID

  Output Parameters: <none>

  Author: Ron Stark

  Created: 10.21.2019

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

&Scoped-define program-id qryDesign.
&Scoped-define calcType 9
&Scoped-define fieldType 10
&Scoped-define indexType 6
&Scoped-define keywordType 15
&Scoped-define ofType 1
&Scoped-define paramType 11
&Scoped-define tableType 2

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER ipiSubjectID    AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER iphCallingPrgm  AS HANDLE  NO-UNDO.
DEFINE INPUT  PARAMETER iphDynCalcField AS HANDLE  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSave         AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE dCol      AS DECIMAL   NO-UNDO INITIAL 1.
DEFINE VARIABLE cPoolName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dRow      AS DECIMAL   NO-UNDO INITIAL 1.
DEFINE VARIABLE dWidth    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE hNewAdd   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hWidget   AS HANDLE    NO-UNDO.
DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFGColor  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iFont     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCancel   AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttQuery NO-UNDO
    FIELD objWidget    AS HANDLE
    FIELD objCol       AS DECIMAL
    FIELD objRow       AS DECIMAL
    FIELD objType      AS INTEGER
    FIELD objValue     AS CHARACTER
    FIELD objLabel     AS CHARACTER
    FIELD objDataType  AS CHARACTER
    FIELD objCalcProc  AS CHARACTER
    FIELD objCalcParam AS CHARACTER
    FIELD objFormula   AS CHARACTER
        INDEX ttWidget IS PRIMARY objWidget
        .

cPoolName = "QueryPool" + STRING(TIME).

{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnField btnCalcField btnUseIndex ~
btnConstant tableType btnOF ofType fieldType keywordType parameterColor ~
calcfieldType btnTable useindexType btnWhere forFillIn btnAnd btnBegins ~
btnEQ btnGE btnGT btnKeyword btnLE btnLT btnMatches btnNE btnOR btnParam ~
btnSave btnUndo 
&Scoped-Define DISPLAYED-OBJECTS tableType ofType fieldType keywordType ~
parameterColor calcfieldType useindexType forFillIn 

/* Custom List Definitions                                              */
/* saveObjects,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define saveObjects RECT-SAVE btnSave btnUndo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fArrow C-Win 
FUNCTION fArrow RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCalcWidgetValues C-Win 
FUNCTION fCalcWidgetValues RETURNS LOGICAL
  (ipiBGColor AS INTEGER, ipcValue AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCreateTtQuery C-Win 
FUNCTION fCreateTtQuery RETURNS LOGICAL
  (iphWidget   AS HANDLE,    ipdCol      AS DECIMAL,   ipdRow       AS DECIMAL,
   ipiType     AS INTEGER,   ipcValue    AS CHARACTER, ipcLabel     AS CHARACTER,
   ipcDataType AS CHARACTER, ipcCalcProc AS CHARACTER, ipcCalcParam AS CHARACTER,
   ipcFormula  AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFGColor C-Win 
FUNCTION fFGColor RETURNS INTEGER
  (ipiBGColor AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFont C-Win 
FUNCTION fFont RETURNS INTEGER
  (ipiBGColor AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTtQuery C-Win 
FUNCTION fGetTtQuery RETURNS LOGICAL
  (iphWidget AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTableList C-Win 
FUNCTION fTableList RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fWidth C-Win 
FUNCTION fWidth RETURNS DECIMAL
  (ipcValue AS CHARACTER, ipiFont AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAnd  NO-FOCUS FLAT-BUTTON
     LABEL "AND" 
     SIZE 7.6 BY 1.14 TOOLTIP "AND"
     FONT 6.

DEFINE BUTTON btnBegins  NO-FOCUS FLAT-BUTTON
     LABEL "BEGINS" 
     SIZE 16 BY 1.14 TOOLTIP "BEGINS"
     FONT 6.

DEFINE BUTTON btnCalcField  NO-FOCUS FLAT-BUTTON
     LABEL "Calculated" 
     SIZE 16 BY 1.14 TOOLTIP "CalcField"
     FONT 6.

DEFINE BUTTON btnConstant  NO-FOCUS FLAT-BUTTON
     LABEL "Constant" 
     SIZE 16 BY 1.14 TOOLTIP "Constant"
     FONT 6.

DEFINE BUTTON btnEQ  NO-FOCUS FLAT-BUTTON
     LABEL "EQ" 
     SIZE 7.6 BY 1.14 TOOLTIP "EQ"
     FONT 6.

DEFINE BUTTON btnField  NO-FOCUS FLAT-BUTTON
     LABEL "Table.Field" 
     SIZE 16 BY 1.14 TOOLTIP "Field"
     FONT 6.

DEFINE BUTTON btnGE  NO-FOCUS FLAT-BUTTON
     LABEL "GE" 
     SIZE 7.6 BY 1.14 TOOLTIP "GE"
     FONT 6.

DEFINE BUTTON btnGT  NO-FOCUS FLAT-BUTTON
     LABEL "GT" 
     SIZE 7.6 BY 1.14 TOOLTIP "GT"
     FONT 6.

DEFINE BUTTON btnKeyword  NO-FOCUS FLAT-BUTTON
     LABEL "KEYWORD" 
     SIZE 16 BY 1.14 TOOLTIP "KEYWORD"
     FONT 6.

DEFINE BUTTON btnLE  NO-FOCUS FLAT-BUTTON
     LABEL "LE" 
     SIZE 7.6 BY 1.14 TOOLTIP "LE"
     FONT 6.

DEFINE BUTTON btnLT  NO-FOCUS FLAT-BUTTON
     LABEL "LT" 
     SIZE 7.6 BY 1.14 TOOLTIP "LT"
     FONT 6.

DEFINE BUTTON btnMatches  NO-FOCUS FLAT-BUTTON
     LABEL "MATCHES" 
     SIZE 16 BY 1.14 TOOLTIP "MATCHES"
     FONT 6.

DEFINE BUTTON btnNE  NO-FOCUS FLAT-BUTTON
     LABEL "NE" 
     SIZE 7.6 BY 1.14 TOOLTIP "NE"
     FONT 6.

DEFINE BUTTON btnOF  NO-FOCUS FLAT-BUTTON
     LABEL "OF Table" 
     SIZE 16 BY 1.14 TOOLTIP "OF"
     FONT 6.

DEFINE BUTTON btnOR  NO-FOCUS FLAT-BUTTON
     LABEL "OR" 
     SIZE 7.6 BY 1.14 TOOLTIP "OR"
     FONT 6.

DEFINE BUTTON btnParam  NO-FOCUS FLAT-BUTTON
     LABEL "Parameter" 
     SIZE 16 BY 1.14 TOOLTIP "Parameter"
     FONT 6.

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE BUTTON btnTable  NO-FOCUS FLAT-BUTTON
     LABEL "Table" 
     SIZE 16 BY 1.14 TOOLTIP "EACH"
     FONT 6.

DEFINE BUTTON btnUndo 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Undo" 
     SIZE 8 BY 1.91 TOOLTIP "Undo Changes".

DEFINE BUTTON btnUseIndex  NO-FOCUS FLAT-BUTTON
     LABEL "USE-INDEX" 
     SIZE 16 BY 1.14 TOOLTIP "USE-INDEX"
     FONT 6.

DEFINE BUTTON btnWhere  NO-FOCUS FLAT-BUTTON
     LABEL "WHERE" 
     SIZE 16 BY 1.14 TOOLTIP "WHERE"
     FONT 6.

DEFINE VARIABLE calcfieldType AS CHARACTER FORMAT "X(256)":U INITIAL " [|Calculated Field|]" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 9 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fieldType AS CHARACTER FORMAT "X(256)":U INITIAL " Table.Field" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 10 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE forFillIn AS CHARACTER FORMAT "X(256)":U INITIAL " FOR" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 2 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE keywordType AS CHARACTER FORMAT "X(256)":U INITIAL " Keyword / Constant" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE VARIABLE ofType AS CHARACTER FORMAT "X(256)":U INITIAL " OF" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 1 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE parameterColor AS CHARACTER FORMAT "X(256)":U INITIAL " [[Parameter]]" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 11 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE tableType AS CHARACTER FORMAT "X(256)":U INITIAL " EACH / FIRST / LAST Table" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 2 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE useindexType AS CHARACTER FORMAT "X(256)":U INITIAL " Use-Index" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 6 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 159 BY 1.43
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 18 BY 23.33
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-SAVE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 18 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnField AT ROW 11 COL 3 WIDGET-ID 298
     btnCalcField AT ROW 22.67 COL 3 WIDGET-ID 316
     btnUseIndex AT ROW 25.76 COL 3 WIDGET-ID 312
     btnConstant AT ROW 23.86 COL 3 WIDGET-ID 338
     tableType AT ROW 1.24 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     btnOF AT ROW 6 COL 3 WIDGET-ID 300
     ofType AT ROW 1.24 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 340
     fieldType AT ROW 1.24 COL 61 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     keywordType AT ROW 1.24 COL 76 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     parameterColor AT ROW 1.24 COL 103 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     calcfieldType AT ROW 1.24 COL 121 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     btnTable AT ROW 4.81 COL 3 WIDGET-ID 296
     useindexType AT ROW 1.24 COL 144 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     btnWhere AT ROW 7.91 COL 3 WIDGET-ID 302
     forFillIn AT ROW 2.57 COL 12 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     btnAnd AT ROW 9.1 COL 3 WIDGET-ID 304
     btnBegins AT ROW 18.38 COL 3 WIDGET-ID 308
     btnEQ AT ROW 14.81 COL 3 WIDGET-ID 320
     btnGE AT ROW 17.19 COL 11.4 WIDGET-ID 330
     btnGT AT ROW 16 COL 11.4 WIDGET-ID 326
     btnKeyword AT ROW 12.91 COL 3 WIDGET-ID 318
     btnLE AT ROW 17.19 COL 3 WIDGET-ID 328
     btnLT AT ROW 16 COL 3 WIDGET-ID 324
     btnMatches AT ROW 19.57 COL 3 WIDGET-ID 310
     btnNE AT ROW 14.81 COL 11.4 WIDGET-ID 322
     btnOR AT ROW 9.1 COL 11.4 WIDGET-ID 306
     btnParam AT ROW 21.48 COL 3 WIDGET-ID 314
     btnSave AT ROW 27.67 COL 3 HELP
          "Update/Save" WIDGET-ID 248
     btnUndo AT ROW 27.67 COL 11 HELP
          "Undo Changes" WIDGET-ID 282
     "Add . . ." VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 4.1 COL 3 WIDGET-ID 334
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "Query:" VIEW-AS TEXT
          SIZE 8 BY .86 AT ROW 2.57 COL 6 WIDGET-ID 336
          FONT 6
     "Color Legend:" VIEW-AS TEXT
          SIZE 16 BY 1 AT ROW 1.24 COL 4 WIDGET-ID 4
          BGCOLOR 15 FONT 6
     RECT-1 AT ROW 1 COL 2 WIDGET-ID 10
     RECT-SAVE AT ROW 27.43 COL 2 WIDGET-ID 246
     RECT-2 AT ROW 3.86 COL 2 WIDGET-ID 332
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.86
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME designFrame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21 ROW 2.5
         SIZE 40 BY 9.52
         FGCOLOR 1  WIDGET-ID 200.


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
         TITLE              = "Query Design"
         HEIGHT             = 28.86
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         ALWAYS-ON-TOP      = yes
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME designFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnUndo IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       calcfieldType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fieldType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       forFillIn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       keywordType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ofType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       parameterColor:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-SAVE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       tableType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       useindexType:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME designFrame
                                                                        */
ASSIGN 
       FRAME designFrame:BOX-SELECTABLE   = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME designFrame
/* Query rebuild information for FRAME designFrame
     _Query            is NOT OPENED
*/  /* FRAME designFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Query Design */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Query Design */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Query Design */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME designFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL designFrame C-Win
ON MOUSE-SELECT-CLICK OF FRAME designFrame
DO:
    IF VALID-HANDLE(hNewAdd) THEN
    RUN pCreateNewAdd (LAST-EVENT:COL, LAST-EVENT:ROW).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL designFrame C-Win
ON RIGHT-MOUSE-CLICK OF FRAME designFrame
DO:
    fArrow().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnd C-Win
ON CHOOSE OF btnAnd IN FRAME DEFAULT-FRAME /* AND */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBegins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBegins C-Win
ON CHOOSE OF btnBegins IN FRAME DEFAULT-FRAME /* BEGINS */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalcField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalcField C-Win
ON CHOOSE OF btnCalcField IN FRAME DEFAULT-FRAME /* Calculated */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnConstant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnConstant C-Win
ON CHOOSE OF btnConstant IN FRAME DEFAULT-FRAME /* Constant */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEQ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEQ C-Win
ON CHOOSE OF btnEQ IN FRAME DEFAULT-FRAME /* EQ */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnField C-Win
ON CHOOSE OF btnField IN FRAME DEFAULT-FRAME /* Table.Field */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGE C-Win
ON CHOOSE OF btnGE IN FRAME DEFAULT-FRAME /* GE */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGT C-Win
ON CHOOSE OF btnGT IN FRAME DEFAULT-FRAME /* GT */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKeyword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKeyword C-Win
ON CHOOSE OF btnKeyword IN FRAME DEFAULT-FRAME /* KEYWORD */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLE C-Win
ON CHOOSE OF btnLE IN FRAME DEFAULT-FRAME /* LE */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLT C-Win
ON CHOOSE OF btnLT IN FRAME DEFAULT-FRAME /* LT */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMatches C-Win
ON CHOOSE OF btnMatches IN FRAME DEFAULT-FRAME /* MATCHES */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNE C-Win
ON CHOOSE OF btnNE IN FRAME DEFAULT-FRAME /* NE */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOF C-Win
ON CHOOSE OF btnOF IN FRAME DEFAULT-FRAME /* OF Table */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOR C-Win
ON CHOOSE OF btnOR IN FRAME DEFAULT-FRAME /* OR */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnParam C-Win
ON CHOOSE OF btnParam IN FRAME DEFAULT-FRAME /* Parameter */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    RUN pSaveQuery (NO). /* none debug mode */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON RIGHT-MOUSE-CLICK OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    RUN pSaveQuery (YES). /* debug mode */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTable C-Win
ON CHOOSE OF btnTable IN FRAME DEFAULT-FRAME /* Table */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUndo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUndo C-Win
ON CHOOSE OF btnUndo IN FRAME DEFAULT-FRAME /* Undo */
DO:
    RUN pBuildQuery (ipiSubjectID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUseIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUseIndex C-Win
ON CHOOSE OF btnUseIndex IN FRAME DEFAULT-FRAME /* USE-INDEX */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWhere C-Win
ON CHOOSE OF btnWhere IN FRAME DEFAULT-FRAME /* WHERE */
DO:
    RUN pNewAdd (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

ON DELETE-CHARACTER ANYWHERE
DO:
    RUN pDelete.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN pSetFrameGrid (FRAME designFrame:HANDLE).
  RUN pGetSettings.
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
  DISPLAY tableType ofType fieldType keywordType parameterColor calcfieldType 
          useindexType forFillIn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnField btnCalcField btnUseIndex btnConstant tableType btnOF ofType 
         fieldType keywordType parameterColor calcfieldType btnTable 
         useindexType btnWhere forFillIn btnAnd btnBegins btnEQ btnGE btnGT 
         btnKeyword btnLE btnLT btnMatches btnNE btnOR btnParam btnSave btnUndo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME designFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-designFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildQuery C-Win 
PROCEDURE pBuildQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSubjectID AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iBGColor  AS INTEGER   NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    DELETE WIDGET-POOL cPoolName NO-ERROR.
    CREATE WIDGET-POOL cPoolName PERSISTENT.

    EMPTY TEMP-TABLE ttQuery.
    ASSIGN
        dCol = 1
        dRow = 0
        .
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ ipiSubjectID
         NO-ERROR.
    IF NOT AVAILABLE dynSubject THEN RETURN.
    {&WINDOW-NAME}:TITLE = "Query Design for Subject: "
                             + STRING(dynSubject.subjectID) + " - "
                             + dynSubject.subjectTitle
                             .
    FOR EACH dynSubjectTable NO-LOCK
        WHERE dynSubjectTable.subjectID EQ dynSubject.subjectID
        BREAK BY dynSubjectTable.sortOrder
        :
        RUN pCreateQryWidget (
            dynSubjectTable.tableFind + " " +
            dynSubjectTable.tableDB   + "." +
            dynSubjectTable.tableName,
            dynSubjectTable.tableDscr,
            "",
            "",
            "",
            "",
            {&tableType},
            OUTPUT hWidget
            ).
        FOR EACH dynSubjectWhere NO-LOCK
            WHERE dynSubjectWhere.subjectID  EQ dynSubjectTable.subjectID
              AND dynSubjectWhere.whereTable EQ dynSubjectTable.tableName
               BY dynSubjectWhere.sortOrder
            :
            iBGColor = IF dynSubjectWhere.whereElement BEGINS "[["  THEN {&paramType}
                  ELSE IF dynSubjectWhere.whereElement BEGINS "[|"  THEN {&calcType}
                  ELSE IF dynSubjectWhere.whereElement BEGINS "OF " THEN {&ofType}
                  ELSE IF dynSubjectWhere.dataType NE "" THEN {&fieldType}
                  ELSE {&keywordType}.
            RUN pCreateQryWidget (
                REPLACE(dynSubjectWhere.whereElement,"OF ","OF " + dynSubjectTable.tableDB + "."),
                dynSubjectWhere.fieldLabel,
                dynSubjectWhere.dataType,
                dynSubjectWhere.calcProc,
                dynSubjectWhere.calcParam,
                "",
                iBGColor,
                OUTPUT hWidget
                ).
        END. /* each dynSubjectWhere */
        IF dynSubjectTable.useIndex NE "" THEN
        RUN pCreateQryWidget (
            "USE-INDEX " + dynSubjectTable.useIndex,
            "",
            "",
            "",
            "",
            "",
            {&indexType},
            OUTPUT hWidget
            ).
    END. /* each dynSubjectTable */
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcField C-Win 
PROCEDURE pCalcField :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iphWidget AS HANDLE  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCancel AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cCalcParam   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcProc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldLabel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormula     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamList   AS CHARACTER NO-UNDO.

    FOR EACH dynSubjectParamSet NO-LOCK
        WHERE dynSubjectParamSet.subjectID EQ ipiSubjectID,
        EACH dynParamSetDtl NO-LOCK
        WHERE dynParamSetDtl.paramSetID EQ dynSubjectParamSet.paramSetID
          AND dynParamSetDtl.paramLabel NE ""
        :
        cParamList = cParamList
                   + dynParamSetDtl.paramLabel + " ("
                   + dynParamSetDtl.paramName + "),"
                   + dynParamSetDtl.paramLabel + "|"
                   + dynParamSetDtl.paramName + ","
                   .
    END. /* each dynSubjectParamSet */
    fGetTtQuery (iphWidget).
    IF AVAILABLE ttQuery THEN
    ASSIGN
        cFieldLabel = ttQuery.objLabel
        cCalcProc   = ttQuery.objCalcProc
        cCalcParam  = ttQuery.objCalcParam
        cFormula    = ttQuery.objFormula
        .
    cParamList = TRIM(cParamList,",").
    ASSIGN
        cFieldName   = iphWidget:SCREEN-VALUE
        cFieldName   = REPLACE(cFieldName,"[|","")
        cFieldName   = REPLACE(cFieldName,"|]","")
        cFieldFormat = "x(256)"
        .
    RUN AOA/dynCalcField.w (
        iphDynCalcField,
        INPUT-OUTPUT cFieldName,
        INPUT-OUTPUT cFieldLabel,
        INPUT-OUTPUT cDataType,
        INPUT-OUTPUT cFieldFormat,
        INPUT-OUTPUT cCalcProc,
        INPUT-OUTPUT cCalcParam,
        INPUT-OUTPUT cFormula,
        ?,
        cParamList,
        OUTPUT oplCancel
        ).
    IF oplCancel EQ NO AND AVAILABLE ttQuery THEN
    ASSIGN
        ttQuery.objLabel     = cFieldLabel
        ttQuery.objCalcProc  = cCalcProc
        ttQuery.objCalcParam = cCalcParam
        ttQuery.objFormula   = cFormula
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateNewAdd C-Win 
PROCEDURE pCreateNewAdd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdCol AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow AS INTEGER NO-UNDO.

    DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBGColor AS INTEGER   NO-UNDO.

    ASSIGN
        dCol = ipdCol
        dRow = ipdRow
        iBGColor = {&keywordType}
        cValue   = hNewAdd:TOOLTIP
        lCancel  = NO
        .
    CASE hNewAdd:TOOLTIP:
        WHEN "CalcField" THEN
        iBGColor = {&calcType}.
        WHEN "Constant" THEN
        cValue = "~"Enter Constant Value~"".
        WHEN "EACH" THEN
        iBGColor = {&tableType}.
        WHEN "Field" THEN
        iBGColor = {&fieldType}.
        WHEN "Keyword" THEN
        cValue = "TRUE".
        WHEN "Parameter" THEN
        iBGColor = {&paramType}.
        WHEN "OF" THEN
        iBGColor = {&ofType}.
        WHEN "USE-INDEX" THEN
        iBGColor = {&indexType}.
        OTHERWISE
        cValue = hNewAdd:TOOLTIP.
    END.
    RUN pCreateQryWidget (cValue, "", "", "", "", "", iBGColor, OUTPUT hWidget).
    CASE hNewAdd:TOOLTIP:
        WHEN "CalcField" THEN
        RUN pCalcField (hWidget, OUTPUT lCancel).
        WHEN "Constant" OR WHEN "Keyword" THEN
        RUN AOA/qryKeyword.w (hWidget, OUTPUT lCancel).
        WHEN "EACH" OR WHEN "OF" THEN DO:
            RUN AOA/qryTable.w (hWidget, hNewAdd:TOOLTIP, OUTPUT lCancel).
            ttQuery.objLabel = ENTRY(2,hWidget:PRIVATE-DATA,"|").
        END.
        WHEN "Field" THEN DO:
            RUN AOA/qryField.w (hWidget, fTableList(), OUTPUT lCancel).
            ASSIGN
                ttQuery.objLabel    = ENTRY(2,hWidget:PRIVATE-DATA,"|")
                ttQuery.objDataType = ENTRY(3,hWidget:PRIVATE-DATA,"|")
                .
        END.
        WHEN "Parameter" THEN
        RUN AOA/qryParam.w (hWidget, ipiSubjectID, OUTPUT lCancel).
        WHEN "USE-INDEX" THEN DO:
            RUN AOA/qryIndex.w (hWidget, fTableList(), OUTPUT lCancel).
            ttQuery.objLabel = ENTRY(2,hWidget:PRIVATE-DATA,"|").
        END.
    END.
    IF lCancel THEN DO:
        DELETE WIDGET hWidget.
        DELETE ttQuery.
    END. /* if cancel */
    ELSE DO:
        hWidget:WIDTH = fWidth(hWidget:SCREEN-VALUE, hWidget:FONT).
        RUN pRefresh.
    END. /* else */
    fArrow().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateQryWidget C-Win 
PROCEDURE pCreateQryWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDataType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCalcProc  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCalcParam AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormula   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBGColor   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.
    
    fCalcWidgetValues(ipiBGColor, ipcValue).
    CREATE FILL-IN ophWidget IN WIDGET-POOL cPoolName
        ASSIGN
            FRAME = FRAME designFrame:HANDLE
            NAME = STRING(dCol) + "." + STRING(dRow)
            DATA-TYPE = "CHARACTER"
            FORMAT = "x(256)"
            FONT = iFont
            COL = dCol
            ROW = dRow
            WIDTH = dWidth
            HEIGHT = 1
            SCREEN-VALUE = ipcValue
            HIDDEN = NO
            SENSITIVE = YES
            READ-ONLY = YES
            BGCOLOR = ipiBGColor
            FGCOLOR = iFGColor
            MOVABLE = YES
            SELECTABLE = YES
            TOOLTIP = ipcLabel
            PRIVATE-DATA = STRING(ipiBGColor) + "|"
                         + ipcLabel + "|"
                         + ipcDataType + "|"
                         + ipcCalcProc + "|"
                         + ipcCalcParam
        TRIGGERS:
          ON DESELECTION
            PERSISTENT RUN pDeselection IN THIS-PROCEDURE (ophWidget).
          ON END-MOVE
            PERSISTENT RUN pEndMove IN THIS-PROCEDURE.
          ON LEFT-MOUSE-DBLCLICK
            PERSISTENT RUN pMouseClick IN THIS-PROCEDURE (ophWidget).
          ON SELECTION
            PERSISTENT RUN pSelection IN THIS-PROCEDURE (ophWidget).
        END TRIGGERS.
    ophWidget:MOVE-TO-TOP().
    fCreateTtQuery(
        ophWidget,
        dCol,
        dRow,
        ipiBGColor,
        ipcValue,
        ipcLabel,
        ipcDataType,
        ipcCalcProc,
        ipcCalcParam,
        ipcFormula
        ).
    dCol = dCol + ophWidget:WIDTH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDelete C-Win 
PROCEDURE pDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    DEFINE VARIABLE hDelete AS HANDLE NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    ASSIGN
        hWidget = FRAME designFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:SELECTED THEN DO:
            ASSIGN
                hDelete = hWidget
                hWidget = hWidget:NEXT-SIBLING
                .
            fGetTtQuery(hDelete).
            IF AVAILABLE ttQuery THEN
            DELETE ttQuery.
            DELETE WIDGET hDelete.
        END. /* if selected */
        ELSE
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    RUN pRefresh.
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeselection C-Win 
PROCEDURE pDeselection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        iphWidget:BGCOLOR = INTEGER(ENTRY(1,iphWidget:PRIVATE-DATA,"|"))
        iphWidget:FGCOLOR = fFGColor(iphWidget:BGCOLOR) 
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEndMove C-Win 
PROCEDURE pEndMove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    ASSIGN
        hWidget = FRAME designFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:SELECTED THEN DO:
            fGetTtQuery(hWidget).
            ASSIGN
                ttQuery.objCol = hWidget:COL
                ttQuery.objRow = hWidget:ROW
                .
        END. /* if selected */
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    RUN pRefresh.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMouseClick C-Win 
PROCEDURE pMouseClick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    &Scoped-define joins AND,OR
    &Scoped-define comparisons EQ,NE,LT,LE,GT,GE,BEGINS,CONTAINS,LIST,MATCHES,RANGE
    
    DEFINE VARIABLE iType AS INTEGER NO-UNDO.    

    iType = INTEGER(ENTRY(1,iphWidget:PRIVATE-DATA,"|")).
    fGetTtQuery (iphWidget).
    CASE iType:
        WHEN {&calcType} THEN
        RUN pCalcField (iphWidget, OUTPUT lCancel).
        WHEN {&fieldType} THEN DO:
            RUN AOA/qryField.w (iphWidget, fTableList(), OUTPUT lCancel).
            ASSIGN
                ttQuery.objLabel    = ENTRY(2,iphWidget:PRIVATE-DATA,"|")
                ttQuery.objDataType = ENTRY(3,iphWidget:PRIVATE-DATA,"|")
                .

        END.
        WHEN {&keywordType} THEN DO:
            IF CAN-DO("{&joins}",iphWidget:SCREEN-VALUE) THEN
            iphWidget:SCREEN-VALUE = IF iphWidget:SCREEN-VALUE EQ "AND" THEN "OR"
                                ELSE IF iphWidget:SCREEN-VALUE EQ "OR"  THEN "AND"
                                ELSE iphWidget:SCREEN-VALUE.
            ELSE
            IF CAN-DO("{&comparisons}",iphWidget:SCREEN-VALUE) THEN
            RUN AOA/qryCompareKywd.w (iphWidget, OUTPUT lCancel).
            ELSE
            IF iphWidget:SCREEN-VALUE EQ "WHERE" THEN .
            ELSE
            RUN AOA/qryKeyword.w (iphWidget, OUTPUT lCancel).
        END. /* keyword */
        WHEN {&ofType} OR WHEN {&tableType} THEN DO:
            RUN AOA/qryTable.w (iphWidget, ENTRY(1,iphWidget:SCREEN-VALUE," "), OUTPUT lCancel).
            ttQuery.objLabel = ENTRY(2,iphWidget:PRIVATE-DATA,"|").
        END.
        WHEN {&paramType} THEN
        RUN AOA/qryParam.w (iphWidget, ipiSubjectID, OUTPUT lCancel).
        WHEN {&indexType} THEN DO:
            RUN AOA/qryIndex.w (iphWidget, fTableList(), OUTPUT lCancel).
            ttQuery.objLabel = ENTRY(2,iphWidget:PRIVATE-DATA,"|").
        END.
    END CASE.
    iphWidget:WIDTH = fWidth(iphWidget:SCREEN-VALUE, iphWidget:FONT).
    RUN pRefresh.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNewAdd C-Win 
PROCEDURE pNewAdd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphAddNew AS HANDLE NO-UNDO.

    FRAME designFrame:LOAD-MOUSE-POINTER("images/fill_in.cur").
    hNewAdd = iphAddNew.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefresh C-Win 
PROCEDURE pRefresh :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    ASSIGN
        dCol = 1
        dRow = 0
        .
    FOR EACH ttQuery
        BY ttQuery.objRow
        BY ttQuery.objCol
        :
        ttQuery.objValue = ttQuery.objWidget:SCREEN-VALUE.
        fCalcWidgetValues(ttQuery.objType, ttQuery.objValue).
        ASSIGN
            ttQuery.objWidget:COL = dCol
            ttQuery.objWidget:ROW = dRow
            ttQuery.objCol        = dCol
            ttquery.objRow        = dRow
            dCol = dCol + ttQuery.objWidget:WIDTH
            .
    END. /* each ttquery */
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveQuery C-Win 
PROCEDURE pSaveQuery :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplDebug AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cTableDB    LIKE dynSubjectTable.tableDB     NO-UNDO.
    DEFINE VARIABLE cTableFind  LIKE dynSubjectTable.tableFind   NO-UNDO.
    DEFINE VARIABLE cTableName  LIKE dynSubjectWhere.tableName   NO-UNDO.
    DEFINE VARIABLE cUseIndex   LIKE dynSubjectTable.useIndex    NO-UNDO.
    DEFINE VARIABLE cWhereTable LIKE dynSubjectWhere.whereTable  NO-UNDO.
    DEFINE VARIABLE jdx           AS INTEGER                     NO-UNDO.
    DEFINE VARIABLE lIsCalc     LIKE dynSubjectWhere.isCalcField NO-UNDO.
    
    DEFINE BUFFER bttQuery FOR ttQuery.

    IF iplDebug THEN DO:
        OUTPUT TO c:\tmp\dynSubject.txt.
        FOR EACH dynSubjectTable NO-LOCK
            WHERE dynSubjectTable.subjectID EQ ipiSubjectID
               BY dynSubjectTable.sortOrder
            :
            DISPLAY
                dynSubjectTable EXCEPT subjectID rec_key
                    WITH STREAM-IO WIDTH 160 TITLE "***** dynSubjectTable *****".
        END. /* each dynSubjectTable */
        FOR EACH dynSubjectTable NO-LOCK
            WHERE dynSubjectTable.subjectID EQ ipiSubjectID,
            EACH dynSubjectWhere NO-LOCK
            WHERE dynSubjectWhere.subjectID  EQ ipiSubjectID
              AND dynSubjectWhere.whereTable EQ dynSubjectTable.tableName 
            BREAK BY dynSubjectTable.sortOrder
                  BY dynSubjectWhere.sortOrder
            :
            DISPLAY
                dynSubjectWhere EXCEPT subjectID rec_key
                    WITH STREAM-IO WIDTH 230 TITLE "***** dynSubjectWhere *****".
        END. /* each dynSubjectTable */
        OUTPUT CLOSE.
        OUTPUT TO c:\tmp\ttQuery.txt.
    END. /* if debug */
    ELSE
    DO TRANSACTION:
        FOR EACH dynSubjectTable EXCLUSIVE-LOCK
            WHERE dynSubjectTable.subjectID EQ ipiSubjectID
            :
            FOR EACH dynSubjectWhere EXCLUSIVE-LOCK
                WHERE dynSubjectWhere.subjectID  EQ ipiSubjectID
                  AND dynSubjectWhere.whereTable EQ dynSubjectTable.tableName 
                :
                DELETE dynSubjectWhere.
            END. /* each dynSubjectWhere */
            DELETE dynSubjectTable.
        END. /* each dynSubjectTable */
    END. /* do trans */
    FOR EACH ttQuery
        WHERE ttQuery.objType EQ {&tableType}
           BY ttQuery.objRow
           BY ttQuery.objCol
        :
        ASSIGN
            cTableFind = ENTRY(1,ttQuery.objValue," ")
            cTableName = ENTRY(2,ttQuery.objValue,".")
            cTableDB   = ENTRY(2,ENTRY(1,ttQuery.objValue,".")," ")
            jdx        = jdx + 1
            .
        FIND FIRST bttQuery
             WHERE bttQuery.objType  EQ {&indexType}
               AND bttQuery.objLabel EQ cTableName
             NO-ERROR.
        cUseIndex = IF AVAILABLE bttQuery THEN ENTRY(2,bttQuery.objValue," ") ELSE "".
        IF iplDebug THEN
        DISPLAY
            jdx              FORMAT ">>9" LABEL "Order"
            cTableFind
            cTableName
            ttQuery.objLabel FORMAT "x(50)" LABEL "Table Description"
            cUseIndex
            cTableDB
            ttQuery.objType  FORMAT ">9"
            ttQuery.objValue FORMAT "x(30)"
                WITH STREAM-IO WIDTH 160 TITLE "***** ttQuery *****".
        ELSE DO:
            CREATE dynSubjectTable.
            ASSIGN
                dynSubjectTable.subjectID = ipiSubjectID
                dynSubjectTable.sortOrder = jdx
                dynSubjectTable.tableDB   = cTableDB
                dynSubjectTable.tableDscr = ttQuery.objLabel
                dynSubjectTable.tableFind = cTableFind
                dynSubjectTable.tableName = cTableName
                dynSubjectTable.useIndex  = cUseIndex
                .
        END. /* else */
    END. /* each ttquery */
    jdx = 0.
    FOR EACH ttQuery
        BY ttQuery.objRow
        BY ttQuery.objCol
        :
        IF ttQuery.objType EQ {&tableType} THEN DO:
            ASSIGN
                cWhereTable = ENTRY(2,ttQuery.objValue,".")
                jdx = 0
                .
            NEXT.
        END.
        ELSE IF ttQuery.objType EQ {&indexType} THEN NEXT.
        ASSIGN
            jdx        = jdx + 1
            lIsCalc    = ttQuery.objType EQ {&calcType}
            cTableName = IF lIsCalc THEN ""
                    ELSE IF INDEX(ttQuery.objValue,".") NE 0 THEN ENTRY(1,ttQuery.objValue,".")
                    ELSE cWhereTable
                    .
        IF iplDebug THEN
        DISPLAY
            jdx                  FORMAT ">>9" LABEL "Order"
            cTableName
            cWhereTable
            ttQuery.objLabel     FORMAT "x(30)" LABEL "Field Label"
            ttQuery.objValue     FORMAT "x(30)" LABEL "Where Element"
            ttQuery.objDataType  FORMAT "x(9)"  LABEL "Data Type"
            lIsCalc
            ttQuery.objCalcProc  FORMAT "X(26)" LABEL "Calculated Field Procedure"
            ttQuery.objCalcParam FORMAT "X(60)" LABEL "Calculated Field Procedure Parameters"
            ttQuery.objType      FORMAT ">9"
                WITH STREAM-IO WIDTH 230 TITLE "***** ttQuery *****".
        ELSE DO:
            CREATE dynSubjectWhere.
            ASSIGN
                dynSubjectWhere.subjectID    = ipiSubjectID
                dynSubjectWhere.sortOrder    = jdx
                dynSubjectWhere.tableName    = cTableName
                dynSubjectWhere.whereTable   = cWhereTable
                dynSubjectWhere.fieldLabel   = ttQuery.objLabel
                dynSubjectWhere.whereElement = ttQuery.objValue
                dynSubjectWhere.dataType     = ttQuery.objDataType
                dynSubjectWhere.isCalcField  = lIsCalc
                dynSubjectWhere.calcProc     = ttQuery.objCalcProc
                dynSubjectWhere.calcParam    = ttQuery.objCalcParam
                .
        END. /* else */
    END. /* each ttquery */
    IF iplDebug THEN DO:
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT notepad.exe c:\tmp\dynSubject.txt.
        OS-COMMAND NO-WAIT notepad.exe c:\tmp\ttQuery.txt.
    END. /* if debug */
    ELSE DO:
        oplSave = YES.
        MESSAGE
            "Dynamic Query Saved"
        VIEW-AS ALERT-BOX.
    END. /* else */

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
    FIND CURRENT user-print NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSelection C-Win 
PROCEDURE pSelection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    IF VALID-HANDLE(hNewAdd) THEN
    RUN pCreateNewAdd (iphWidget:COL - .01, iphWidget:ROW).
    ELSE
    ASSIGN
        iphWidget:BGCOLOR = 14
        iphWidget:FGCOLOR = 1
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetFrameGrid C-Win 
PROCEDURE pSetFrameGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.

    ASSIGN
        iphFrame:GRID-FACTOR-VERTICAL    = 1
        iphFrame:GRID-FACTOR-HORIZONTAL  = 10
        iphFrame:GRID-UNIT-WIDTH-PIXELS  = 3
        iphFrame:GRID-UNIT-HEIGHT-PIXELS = 21
        iphFrame:GRID-SNAP               = YES
        iphFrame:GRID-VISIBLE            = YES
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
        HIDE FRAME designFrame.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.86 THEN
        {&WINDOW-NAME}:HEIGHT = 28.86.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            FRAME designFrame:VIRTUAL-HEIGHT   = FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT
                                               - FRAME designFrame:ROW + 1
            FRAME designFrame:VIRTUAL-WIDTH    = FRAME {&FRAME-NAME}:VIRTUAL-WIDTH
                                               - FRAME designFrame:COL + 1
            FRAME designFrame:HEIGHT           = FRAME designFrame:VIRTUAL-HEIGHT
            FRAME designFrame:WIDTH            = FRAME designFrame:VIRTUAL-WIDTH
            .
        RUN pBuildQuery (ipiSubjectID).
        VIEW FRAME {&FRAME-NAME}.
        VIEW FRAME designFrame.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fArrow C-Win 
FUNCTION fArrow RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FRAME designFrame:LOAD-MOUSE-POINTER("arrow").
    hNewAdd = ?.
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCalcWidgetValues C-Win 
FUNCTION fCalcWidgetValues RETURNS LOGICAL
  (ipiBGColor AS INTEGER, ipcValue AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN
        iFont    = fFont(ipiBGColor)
        iFGColor = fFGColor(ipiBGColor)
        dWidth   = fWidth(ipcValue, iFont)
        .
    IF ipiBGColor EQ {&tableType} THEN
    ASSIGN
        dCol = 1
        dRow = dRow + 1
        .
    ELSE IF CAN-DO("AND,OR,WHERE",ipcValue) THEN DO:
        CASE ipcValue:
            WHEN "AND" THEN
            dCol = 11.
            WHEN "BY" THEN
            dCol = 12.
            WHEN "OR" THEN
            dCol = 12.
            WHEN "WHERE" THEN
            dCol = 8.
        END CASE.
        dRow = dRow + 1.
    END. /* if */
    ELSE IF ipcValue BEGINS "USE-INDEX" THEN
    ASSIGN
        dCol = 8
        dRow = dRow + 1
        .
    ELSE IF dCol + dWidth GT FRAME designFrame:WIDTH THEN
    ASSIGN
        dCol = 17
        dRow = dRow + 1
        .
    IF dRow EQ 0 THEN dRow = 1.
    /* auto expand window/frame size if too small */
    IF dRow + 1 GT FRAME designFrame:HEIGHT THEN
    ASSIGN
        {&WINDOW-NAME}:HEIGHT              = {&WINDOW-NAME}:HEIGHT + 1
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
        FRAME designFrame:VIRTUAL-HEIGHT   = FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT
                                           - FRAME designFrame:ROW + 1
        FRAME designFrame:HEIGHT           = FRAME designFrame:VIRTUAL-HEIGHT
        .
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCreateTtQuery C-Win 
FUNCTION fCreateTtQuery RETURNS LOGICAL
  (iphWidget   AS HANDLE,    ipdCol      AS DECIMAL,   ipdRow       AS DECIMAL,
   ipiType     AS INTEGER,   ipcValue    AS CHARACTER, ipcLabel     AS CHARACTER,
   ipcDataType AS CHARACTER, ipcCalcProc AS CHARACTER, ipcCalcParam AS CHARACTER,
   ipcFormula  AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    fGetTtQuery(iphWidget).
    IF NOT AVAILABLE ttQuery THEN DO:
        CREATE ttQuery.
        ttQuery.objWidget = iphWidget.
    END. /* if not avail */
    ASSIGN
        ttQuery.objCol       = ipdCol
        ttQuery.objRow       = ipdRow
        ttQuery.objType      = ipiType
        ttQuery.objValue     = ipcValue
        ttQuery.objLabel     = ipcLabel
        ttQuery.objDataType  = ipcDataType
        ttQuery.objCalcProc  = ipcCalcProc
        ttQuery.objCalcParam = ipcCalcParam
        ttQuery.objFormula   = ipcFormula
        .
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFGColor C-Win 
FUNCTION fFGColor RETURNS INTEGER
  (ipiBGColor AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN IF (ipiBGColor GE 0 AND ipiBGColor LE 7) OR ipiBGColor EQ 9 THEN 15 ELSE 1.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFont C-Win 
FUNCTION fFont RETURNS INTEGER
  (ipiBGColor AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN IF ipiBGColor EQ 1  OR
              ipiBGColor EQ 2  OR
              ipiBGColor EQ 6  OR
              ipiBGColor EQ 15 THEN 6
         ELSE FRAME designFrame:FONT.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTtQuery C-Win 
FUNCTION fGetTtQuery RETURNS LOGICAL
  (iphWidget AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FIND FIRST ttQuery
         WHERE ttQuery.objWidget EQ iphWidget
         NO-ERROR.
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTableList C-Win 
FUNCTION fTableList RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTableList AS CHARACTER NO-UNDO.

    DEFINE BUFFER ttQuery FOR ttQuery.
    
    FOR EACH ttQuery
        WHERE ttQuery.objType EQ {&tableType}
        :
        cTableList = cTableList + ENTRY(2,ttQuery.objValue," ") + ",".
    END. /* each ttquery */
    RETURN TRIM(cTableList,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fWidth C-Win 
FUNCTION fWidth RETURNS DECIMAL
  (ipcValue AS CHARACTER, ipiFont AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN FONT-TABLE:GET-TEXT-WIDTH-CHARS(ipcValue, ipiFont) + 1.5.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

