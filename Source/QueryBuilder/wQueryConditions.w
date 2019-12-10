&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wQueryConditions.w
    Purpose     : Modify conditions of a query

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

{queryLib.i &reference-only=reference-only}

DEFINE INPUT  PARAMETER phParent AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER phFrame  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rcQuery cbAndOr cbFieldLeft cbOperator ~
cbFieldRight fiConditions edQuery btnQuote btnString btnBracket btnDate ~
btnDateTime btnDecimal btnGE btnInteger btnLE btnSubstring btnTime ~
btnBegins btnOr btnAnd btnContains btnEq btnGT btnLT btnMatches btnNE ~
btnToday fiPreview 
&Scoped-Define DISPLAYED-OBJECTS cbAndOr cbFieldLeft cbOperator ~
cbFieldRight fiConditions edQuery edResult fiPreview 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cbAndOr cbFieldLeft cbOperator cbFieldRight btnQuote ~
btnString btnBracket btnDate btnDateTime btnDecimal btnGE btnInteger btnLE ~
btnSubstring btnTime btnBegins btnOr btnAnd btnContains btnEq btnGT btnLT ~
btnMatches btnNE btnToday 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAnd  NO-FOCUS
     LABEL "and" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnBegins  NO-FOCUS
     LABEL "begins" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnBracket  NO-FOCUS
     LABEL "()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnContains  NO-FOCUS
     LABEL "contains" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnDate  NO-FOCUS
     LABEL "date()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnDateTime  NO-FOCUS
     LABEL "datetime()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnDecimal  NO-FOCUS
     LABEL "decimal()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnEq  NO-FOCUS
     LABEL "=" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnGE  NO-FOCUS
     LABEL ">=" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnGT  NO-FOCUS
     LABEL ">" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnInteger  NO-FOCUS
     LABEL "integer()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnLE  NO-FOCUS
     LABEL "<=" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnLT  NO-FOCUS
     LABEL "<" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnMatches  NO-FOCUS
     LABEL "matches" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 80 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnNE  NO-FOCUS
     LABEL "<>" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnOr  NO-FOCUS
     LABEL "or" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnQuote  NO-FOCUS
     LABEL "~"~"" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 40 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnString  NO-FOCUS
     LABEL "string()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnSubstring  NO-FOCUS
     LABEL "substring()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnTime  NO-FOCUS
     LABEL "time" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnToday  NO-FOCUS
     LABEL "today" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 100 BY 24 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE VARIABLE cbAndOr AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Where" 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE-PIXELS 50 BY 21 TOOLTIP "preceding AND or OR for the expression" NO-UNDO.

DEFINE VARIABLE cbFieldLeft AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     DROP-DOWN-LIST
     SIZE-PIXELS 335 BY 21 TOOLTIP "field used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE cbFieldRight AS CHARACTER 
     CONTEXT-HELP-ID 0
     VIEW-AS COMBO-BOX SORT INNER-LINES 20
     DROP-DOWN
     SIZE-PIXELS 330 BY 21 TOOLTIP "field used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE cbOperator AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "","=","<>",">",">=","<","<=","begins","matches","contains" 
     DROP-DOWN-LIST
     SIZE-PIXELS 80 BY 21 TOOLTIP "operator used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE edQuery AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 75 BY 12.05
     FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE edResult AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE fiConditions AS CHARACTER 
     CONTEXT-HELP-ID 0
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 375 BY 280 TOOLTIP "enter the extra conditions for the query"
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fiPreview AS CHARACTER FORMAT "X(256)":U INITIAL "Preview" 
      VIEW-AS TEXT 
     SIZE 10.6 BY .67
     FGCOLOR 7 FONT 0 NO-UNDO.

DEFINE RECTANGLE rcQuery
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 76.2 BY 12.33
     BGCOLOR 2 FGCOLOR 2 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbAndOr AT Y 10 X 45 COLON-ALIGNED WIDGET-ID 10
     cbFieldLeft AT Y 10 X 105 COLON-ALIGNED NO-LABEL WIDGET-ID 220
     cbOperator AT Y 10 X 445 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     cbFieldRight AT Y 10 X 530 COLON-ALIGNED NO-LABEL WIDGET-ID 224
     fiConditions AT Y 40 X 115 NO-LABEL WIDGET-ID 130
     edQuery AT ROW 2.91 COL 100 NO-LABEL WIDGET-ID 250
     btnQuote AT Y 123 X 65 WIDGET-ID 72
     edResult AT ROW 15.19 COL 100 NO-LABEL WIDGET-ID 244
     btnString AT Y 45 X 885 WIDGET-ID 228
     btnBracket AT Y 123 X 25 WIDGET-ID 28
     btnDate AT Y 148 X 885 WIDGET-ID 234
     btnDateTime AT Y 174 X 885 WIDGET-ID 240
     btnDecimal AT Y 122 X 885 WIDGET-ID 230
     btnGE AT Y 97 X 65 WIDGET-ID 236
     btnInteger AT Y 96 X 885 WIDGET-ID 232
     btnLE AT Y 97 X 25 WIDGET-ID 238
     btnSubstring AT Y 70 X 885 WIDGET-ID 226
     btnTime AT Y 226 X 885 WIDGET-ID 242
     btnBegins AT Y 175 X 25 WIDGET-ID 74
     btnOr AT Y 149 X 65 WIDGET-ID 24
     btnAnd AT Y 149 X 25 WIDGET-ID 22
     btnContains AT Y 201 X 25 WIDGET-ID 116
     btnEq AT Y 45 X 26 WIDGET-ID 62
     btnGT AT Y 72 X 66 WIDGET-ID 66
     btnLT AT Y 71 X 26 WIDGET-ID 64
     btnMatches AT Y 227 X 25 WIDGET-ID 114
     btnNE AT Y 45 X 66 WIDGET-ID 68
     btnToday AT Y 200 X 885 WIDGET-ID 122
     fiPreview AT ROW 14.19 COL 158.6 COLON-ALIGNED NO-LABEL WIDGET-ID 256
     rcQuery AT ROW 2.76 COL 99.4 WIDGET-ID 254
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200 BY 15.71 WIDGET-ID 100.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Query Conditions"
         HEIGHT             = 16.95
         WIDTH              = 201.2
         MAX-HEIGHT         = 20.14
         MAX-WIDTH          = 206.4
         VIRTUAL-HEIGHT     = 20.14
         VIRTUAL-WIDTH      = 206.4
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnAnd IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnBegins IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnBracket IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnBracket:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "(&1)".

/* SETTINGS FOR BUTTON btnContains IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnDate IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnDate:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "DATE(&1)".

/* SETTINGS FOR BUTTON btnDateTime IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnDateTime:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "DATETIME(&1)".

/* SETTINGS FOR BUTTON btnDecimal IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnDecimal:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "DECIMAL()".

/* SETTINGS FOR BUTTON btnEq IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnGE IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnGT IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnInteger IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnInteger:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "INTEGER(&1)".

/* SETTINGS FOR BUTTON btnLE IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnLT IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnMatches IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnNE IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnOr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnQuote IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnString IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnString:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "STRING(&1)".

/* SETTINGS FOR BUTTON btnSubstring IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnSubstring:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "SUBSTRING(&1,start,length)".

/* SETTINGS FOR BUTTON btnTime IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnToday IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbAndOr IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbFieldLeft IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbFieldRight IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbOperator IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       edQuery:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       edQuery:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR EDITOR edResult IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       edResult:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiConditions:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

ASSIGN 
       rcQuery:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.24
       COLUMN          = 176
       HEIGHT          = 1.43
       WIDTH           = 6
       WIDGET-ID       = 248
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnBracket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBracket C-Win
ON CHOOSE OF btnBracket IN FRAME DEFAULT-FRAME /* () */
, btnQuote, btnString, btnSubstring, btnInteger, btnDecimal, btnDate, btnDatetime
DO:
  DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iPos  AS INTEGER   NO-UNDO.
  
  IF fiConditions:SELECTION-TEXT = "" THEN
  DO:
    cText = SUBSTITUTE(' &1 ', REPLACE(SELF:PRIVATE-DATA,'&1','')).
    fiConditions:INSERT-STRING(cText).
    iPos = INDEX(cText,'(').
    fiConditions:CURSOR-OFFSET = fiConditions:CURSOR-OFFSET - LENGTH(cText) + iPos.
  END.
  ELSE
    fiConditions:REPLACE-SELECTION-TEXT(SUBSTITUTE(SELF:PRIVATE-DATA, TRIM(fiConditions:SELECTION-TEXT))).

  APPLY 'value-changed' TO fiConditions.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEq C-Win
ON CHOOSE OF btnEq IN FRAME DEFAULT-FRAME /* = */
, btnNe, btnLT, btnGT, btnLE, btnGE, btnAnd, btnOr, btnBegins, btnContains, btnMatches, btnToday, btnTime
DO:
  /* No text selected */
  IF fiConditions:SELECTION-TEXT = "" THEN
    fiConditions:INSERT-STRING(SUBSTITUTE(' &1 ', SELF:LABEL)).
  ELSE
    fiConditions:REPLACE-SELECTION-TEXT(SUBSTITUTE(' &1 ', SELF:LABEL)).
    
  APPLY 'value-changed' TO fiConditions.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAndOr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAndOr C-Win
ON RETURN OF cbAndOr IN FRAME DEFAULT-FRAME /* Where */
DO:
  APPLY 'entry' TO cbFieldLeft.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFieldLeft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFieldLeft C-Win
ON RETURN OF cbFieldLeft IN FRAME DEFAULT-FRAME
DO:
  APPLY 'entry' TO cbOperator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFieldRight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFieldRight C-Win
ON RETURN OF cbFieldRight IN FRAME DEFAULT-FRAME
DO:
  DEFINE BUFFER bLeft  FOR ttSchemaField.
  DEFINE BUFFER bRight FOR ttSchemaField.
  DEFINE BUFFER bField FOR ttField.
  
  DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

  FIND bLeft WHERE bLeft.cFullName = cbFieldLeft:SCREEN-VALUE NO-ERROR.
  IF NOT AVAILABLE bLeft THEN RETURN.
  
  FIND bRight WHERE bRight.cFullName = cbFieldRight:SCREEN-VALUE NO-ERROR.
  IF AVAILABLE bRight THEN 
    cValue = bRight.cFullName.
  ELSE 
    cValue = (IF bLeft.cDataType = 'character' THEN QUOTER(cbFieldRight:SCREEN-VALUE) ELSE cbFieldRight:SCREEN-VALUE).

  fiConditions:INSERT-STRING(LEFT-TRIM(SUBSTITUTE('&1 &2 &3 &4&5'
                                        , (IF cbAndOr:SCREEN-VALUE = ? THEN '' ELSE cbAndOr:SCREEN-VALUE)
                                        , cbFieldLeft:SCREEN-VALUE
                                        , cbOperator:SCREEN-VALUE
                                        , cValue
                                        , CHR(13)
                                        )
                              )
                         ).
                         
  APPLY 'value-changed' TO fiConditions.
  APPLY "entry" TO cbAndOr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbOperator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbOperator C-Win
ON RETURN OF cbOperator IN FRAME DEFAULT-FRAME
DO:
  APPLY 'entry' TO cbFieldRight.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  RUN testQuery.
  chCtrlFrame:pstimer:ENABLED = FALSE.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiConditions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiConditions C-Win
ON CTRL-A OF fiConditions IN FRAME DEFAULT-FRAME
DO:
  SELF:SET-SELECTION(1,LENGTH(SELF:SCREEN-VALUE) + NUM-ENTRIES(SELF:SCREEN-VALUE,'~n')).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiConditions C-Win
ON CTRL-D OF fiConditions IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  i = SELF:CURSOR-OFFSET.
  SELF:SET-SELECTION(0,0).
  SELF:CURSOR-OFFSET = i.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiConditions C-Win
ON VALUE-CHANGED OF fiConditions IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE ttQuery THEN ttQuery.conditions = TRIM(SELF:SCREEN-VALUE,'~n ').
  
  chCtrlFrame:pstimer:ENABLED = TRUE.
  chCtrlFrame:pstimer:INTERVAL = 1000.
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
  phFrame = FRAME {&FRAME-NAME}:HANDLE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "wQueryConditions.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wQueryConditions.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  RUN control_load.
  DISPLAY cbAndOr cbFieldLeft cbOperator cbFieldRight fiConditions edQuery 
          edResult fiPreview 
      WITH FRAME DEFAULT-FRAME.
  ENABLE rcQuery cbAndOr cbFieldLeft cbOperator cbFieldRight fiConditions 
         edQuery btnQuote btnString btnBracket btnDate btnDateTime btnDecimal 
         btnGE btnInteger btnLE btnSubstring btnTime btnBegins btnOr btnAnd 
         btnContains btnEq btnGT btnLT btnMatches btnNE btnToday fiPreview 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenInit C-Win 
PROCEDURE ScreenInit :
/* Bind the dataset to the screen
*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuery BIND.
  DEFINE VARIABLE iLightGray AS INTEGER NO-UNDO.

  /* UIB cannot handle this well */
  btnQuote:PRIVATE-DATA IN FRAME DEFAULT-FRAME = "~"&1~"".
  
  iLightGray = getLightGray().  
  edQuery:BGCOLOR = iLightGray.  
  fiPreview:BGCOLOR = iLightGray.
  
  fiPreview:MOVE-TO-TOP().  
  
END PROCEDURE. /* ScreenInit */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenValidate C-Win 
PROCEDURE ScreenValidate :
/* Update db with info from screen
*/
  DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&frame-name}:
  
    RUN testQuery.
    IF edResult:SCREEN-VALUE <> 'Query ok' THEN 
      pcError = 'Please verify the conditions, your query does not compile'.
    
  END.

END PROCEDURE. /* ScreenValidate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenHide C-Win 
PROCEDURE ScreenHide :
/* Update db with info from screen
*/
  DEFINE BUFFER bQuery FOR ttQuery.

  DO WITH FRAME {&frame-name}:
  
    FIND bQuery.    
    bQuery.conditions = fiConditions:SCREEN-VALUE.
    
  END.

END PROCEDURE. /* ScreenHide */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenShow C-Win 
PROCEDURE ScreenShow :
/* Get latest info from db and show
*/
  DEFINE BUFFER bQuery       FOR ttQuery.
  DEFINE BUFFER bTable       FOR ttTable.
  DEFINE BUFFER bSchemaTable FOR ttSchemaTable.
  DEFINE BUFFER bSchemaField FOR ttSchemaField.
  
  DO WITH FRAME {&frame-name}:
  
    /* Populate field combo with available fields */
    RUN getSchema(OUTPUT DATASET dsSchema BY-REFERENCE).
    
    cbFieldLeft:LIST-ITEMS = ''.
    FOR EACH bTable
      , EACH bSchemaTable
       WHERE bSchemaTable.cTableName = bTable.tableName
      , EACH bSchemaField
       WHERE bSchemaField.cTableName = bSchemaTable.cTableName
       BY bSchemaField.cTableName
       BY bSchemaField.cFieldName:
       
      cbFieldLeft:ADD-LAST(SUBSTITUTE('&1.&2', bSchemaField.cTableName, bSchemaField.cFieldName)).      
    END.
    cbFieldRight:LIST-ITEMS = cbFieldLeft:LIST-ITEMS.
  
    FIND bQuery.       
    fiConditions:SCREEN-VALUE = bQuery.conditions.

    rcQuery:HIDDEN = TRUE.
    APPLY 'value-changed' TO fiConditions.
    APPLY 'entry' TO cbAndOr.
  END.
  
END PROCEDURE. /* ScreenShow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testQuery C-Win 
PROCEDURE testQuery :
/* Try to open the query
*/
  DEFINE VARIABLE hQuery AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cError AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&frame-name}:
  
    RUN openQuery
      ( INPUT DATASET dsQuery
      , INPUT 0 /* all joins */   
      , INPUT TRUE /* include generic query conditions */
      , OUTPUT hQuery
      , OUTPUT cError
      ).
    
    IF VALID-HANDLE(hQuery) THEN
    DO:
      /* Attach query as a tooltip to the editor */
      edQuery:SCREEN-VALUE = getReadableQuery(hQuery:PREPARE-STRING). 
      edResult:SCREEN-VALUE = 'Query ok'.    
      rcQuery:BGCOLOR = 2.
    END.
    
    ELSE 
    DO:
      edResult:SCREEN-VALUE = ERROR-STATUS:GET-MESSAGE(1).
      rcQuery:BGCOLOR = 12.
    END.
    
    rcQuery:FGCOLOR = rcQuery:BGCOLOR.
    rcQuery:HIDDEN = FALSE.
    
    RUN closeQuery(hQuery).
  END.
  
END PROCEDURE. /* testQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

