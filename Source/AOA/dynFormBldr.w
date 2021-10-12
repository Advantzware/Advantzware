&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: dynFormBldr.w

  Description: Dynamic Form Builder

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.7.2021

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

&Scoped-define program-id dynFormBldr.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttFormElements NO-UNDO
    FIELD fieldTable  AS CHARACTER FORMAT "x(15)" LABEL "Table"
    FIELD fieldName   AS CHARACTER FORMAT "x(20)" LABEL "Name"
    FIELD fieldLabel  AS CHARACTER FORMAT "x(35)" LABEL "Label"
    FIELD fieldFormat AS CHARACTER FORMAT "x(20)" LABEL "Format"
    FIELD dataType    AS CHARACTER FORMAT "x(10)" LABEL "Data Type"
    FIELD ttHandle    AS HANDLE
    FIELD ttRowID     AS ROWID
        INDEX fieldLabel IS PRIMARY fieldLabel
        .
DEFINE TEMP-TABLE ttFormComponent NO-UNDO
    FIELD formDate       AS CHARACTER FORMAT "x(10)" LABEL "Current Date"
    FIELD formFrame      AS CHARACTER FORMAT "x(10)" LABEL "Frame"
    FIELD formImage      AS CHARACTER FORMAT "x(10)" LABEL "Image"
    FIELD formPage       AS CHARACTER FORMAT "x(10)" LABEL "Page Number"
    FIELD formPageXofY   AS CHARACTER FORMAT "x(10)" LABEL "Page X of Y"
    FIELD formRectangle  AS CHARACTER FORMAT "x(10)" LABEL "Rectangle"
    FIELD formText       AS CHARACTER FORMAT "x(10)" LABEL "Static Text"
    FIELD formTime       AS CHARACTER FORMAT "x(10)" LABEL "Time"
    FIELD formTotalPages AS CHARACTER FORMAT "x(10)" LABEL "Total Pages"
    .
{AOA/tempTable/ttFormTempTables.i}

DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hElements    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hTableFields AS HANDLE    NO-UNDO.
DEFINE VARIABLE lSuperAdmin  AS LOGICAL   NO-UNDO.

FIND FIRST dynForm NO-LOCK.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME browseBandElements

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynFormElement ttFormElements

/* Definitions for BROWSE browseBandElements                            */
&Scoped-define FIELDS-IN-QUERY-browseBandElements dynFormElement.sortOrder ~
dynFormElement.elementLabel dynFormElement.elementTable ~
dynFormElement.elementField dynFormElement.elementFormat ~
dynFormElement.dataType dynFormElement.bandType 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseBandElements 
&Scoped-define QUERY-STRING-browseBandElements FOR EACH dynFormElement ~
      WHERE dynFormElement.formID EQ dynForm.formID AND ~
dynFormElement.clientID EQ dynForm.clientID AND ~
dynFormElement.bandType EQ formElementTypes NO-LOCK ~
    BY dynFormElement.sortOrder INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browseBandElements OPEN QUERY browseBandElements FOR EACH dynFormElement ~
      WHERE dynFormElement.formID EQ dynForm.formID AND ~
dynFormElement.clientID EQ dynForm.clientID AND ~
dynFormElement.bandType EQ formElementTypes NO-LOCK ~
    BY dynFormElement.sortOrder INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browseBandElements dynFormElement
&Scoped-define FIRST-TABLE-IN-QUERY-browseBandElements dynFormElement


/* Definitions for BROWSE browseFormElements                            */
&Scoped-define FIELDS-IN-QUERY-browseFormElements ttFormElements.fieldLabel ttFormElements.fieldTable ttFormElements.fieldName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseFormElements   
&Scoped-define SELF-NAME browseFormElements
&Scoped-define QUERY-STRING-browseFormElements FOR EACH ttFormElements
&Scoped-define OPEN-QUERY-browseFormElements OPEN QUERY {&SELF-NAME} FOR EACH ttFormElements.
&Scoped-define TABLES-IN-QUERY-browseFormElements ttFormElements
&Scoped-define FIRST-TABLE-IN-QUERY-browseFormElements ttFormElements


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAddSelection btnMoveDown btnMoveUp ~
btnRemove btnRemoveSelection bandTitle bandPageHeader bandColumnHeader ~
bandDetail bandColumnFooter bandPageFooter bandLastPage bandSummary ~
formLayout elements formElementTypes browseFormElements browseBandElements 
&Scoped-Define DISPLAYED-OBJECTS formElementTypes elementsLabel 

/* Custom List Definitions                                              */
/* elementButton,List-2,List-3,List-4,List-5,List-6                     */
&Scoped-define elementButton btnAddSelection btnMoveDown btnMoveUp ~
btnRemove btnRemoveSelection 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDataType C-Win 
FUNCTION fDataType RETURNS CHARACTER
  (ipcDataType AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperPattern C-Win
FUNCTION fJasperPattern RETURNS CHARACTER 
  (ipcFormat AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddSelection 
     IMAGE-UP FILE "Graphics/16x16/next.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Selection" 
     SIZE 4.4 BY 1 TOOLTIP "Add Selections".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/16x16/navigate_down.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/16x16/navigate_up.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Up".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Remove".

DEFINE BUTTON btnRemoveSelection 
     IMAGE-UP FILE "Graphics/16x16/previous.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove Selection" 
     SIZE 4.4 BY 1 TOOLTIP "Remove Selections".

DEFINE VARIABLE elementsLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Form Elements" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE bandColumnFooter
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 22 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandColumnHeader
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 22 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandDetail
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 13 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandLastPage
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 17 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandPageFooter
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandPageHeader
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 20 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandSummary
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 16 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE bandTitle
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 12 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE elements
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 29 BY 1.19
     BGCOLOR 14 .

DEFINE RECTANGLE formLayout
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 9 BY 1.19
     BGCOLOR 14 .

DEFINE VARIABLE formElementTypes AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 40 BY 2.86 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseBandElements FOR 
      dynFormElement SCROLLING.

DEFINE QUERY browseFormElements FOR 
      ttFormElements SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseBandElements
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseBandElements C-Win _STRUCTURED
  QUERY browseBandElements NO-LOCK DISPLAY
      dynFormElement.sortOrder FORMAT ">>9":U
      dynFormElement.elementLabel FORMAT "x(30)":U
      dynFormElement.elementTable FORMAT "x(30)":U
      dynFormElement.elementField FORMAT "x(30)":U
      dynFormElement.elementFormat FORMAT "x(30)":U
      dynFormElement.dataType FORMAT "x(9)":U
      dynFormElement.bandType FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 113 BY 22.86
         TITLE "Band Elements".

DEFINE BROWSE browseFormElements
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseFormElements C-Win _FREEFORM
  QUERY browseFormElements DISPLAY
      ttFormElements.fieldLabel
      ttFormElements.fieldTable
      ttFormElements.fieldName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 22.86
         TITLE "Form Elements".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnAddSelection AT ROW 9.57 COL 42 HELP
          "Add Selections" WIDGET-ID 200
     btnMoveDown AT ROW 13.86 COL 42 HELP
          "Move Down" WIDGET-ID 62
     btnMoveUp AT ROW 11.48 COL 42 HELP
          "Move Up" WIDGET-ID 64
     btnRemove AT ROW 12.67 COL 42 HELP
          "Remove" WIDGET-ID 66
     btnRemoveSelection AT ROW 15.76 COL 42 HELP
          "Remove Selections" WIDGET-ID 198
     formElementTypes AT ROW 3.38 COL 2 NO-LABEL WIDGET-ID 52
     browseFormElements AT ROW 6.48 COL 2 WIDGET-ID 300
     browseBandElements AT ROW 6.48 COL 47 WIDGET-ID 400
     elementsLabel AT ROW 4.57 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     "Page Footer Band" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.48 COL 107 WIDGET-ID 204
          BGCOLOR 14 
     "Detail" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.48 COL 73 WIDGET-ID 202
          BGCOLOR 14 
     "Layout" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.48 COL 3 WIDGET-ID 36
          BGCOLOR 14 
     "Layout" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.48 COL 3 WIDGET-ID 36
          BGCOLOR 14 
     "Column Header Band" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.48 COL 47 WIDGET-ID 12
          BGCOLOR 14 
     "Last Page Band" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.48 COL 127 WIDGET-ID 28
          BGCOLOR 14 
     "Summary Band" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1.48 COL 145 WIDGET-ID 32
          BGCOLOR 14 
     "Form Element Types" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.67 COL 2 WIDGET-ID 54
     "Column Footer Band" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.48 COL 84 WIDGET-ID 20
          BGCOLOR 14 
     "Title Band" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 13 WIDGET-ID 4
          BGCOLOR 14 
     "Page Header Band" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.48 COL 26 WIDGET-ID 8
          BGCOLOR 14 
     "Column Header Band" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.48 COL 47 WIDGET-ID 12
          BGCOLOR 14 
     "Layout" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.48 COL 3 WIDGET-ID 36
          BGCOLOR 14 
     bandTitle AT ROW 1.24 COL 12 WIDGET-ID 2
     bandPageHeader AT ROW 1.24 COL 25 WIDGET-ID 6
     bandColumnHeader AT ROW 1.24 COL 46 WIDGET-ID 10
     bandDetail AT ROW 1.24 COL 69 WIDGET-ID 14
     bandColumnFooter AT ROW 1.24 COL 83 WIDGET-ID 18
     bandPageFooter AT ROW 1.24 COL 106 WIDGET-ID 22
     bandLastPage AT ROW 1.24 COL 126 WIDGET-ID 26
     bandSummary AT ROW 1.24 COL 144 WIDGET-ID 30
     formLayout AT ROW 1.24 COL 2 WIDGET-ID 34
     elements AT ROW 4.33 COL 47 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15  WIDGET-ID 100.


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
         TITLE              = "Dynamic Form Builder"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB browseFormElements formElementTypes DEFAULT-FRAME */
/* BROWSE-TAB browseBandElements browseFormElements DEFAULT-FRAME */
ASSIGN 
       browseBandElements:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2.

/* SETTINGS FOR BUTTON btnAddSelection IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnAddSelection:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMoveDown IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnMoveDown:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMoveUp IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnMoveUp:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnRemove IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnRemove:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnRemoveSelection IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnRemoveSelection:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN elementsLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       elementsLabel:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseBandElements
/* Query rebuild information for BROWSE browseBandElements
     _TblList          = "ASI.dynFormElement"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.dynFormElement.sortOrder|yes"
     _Where[1]         = "dynFormElement.formID EQ dynForm.formID AND
dynFormElement.clientID EQ dynForm.clientID AND
dynFormElement.bandType EQ formElementTypes"
     _FldNameList[1]   = ASI.dynFormElement.sortOrder
     _FldNameList[2]   = ASI.dynFormElement.elementLabel
     _FldNameList[3]   = ASI.dynFormElement.elementTable
     _FldNameList[4]   = ASI.dynFormElement.elementField
     _FldNameList[5]   = ASI.dynFormElement.elementFormat
     _FldNameList[6]   = ASI.dynFormElement.dataType
     _FldNameList[7]   = ASI.dynFormElement.bandType
     _Query            is NOT OPENED
*/  /* BROWSE browseBandElements */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseFormElements
/* Query rebuild information for BROWSE browseFormElements
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttFormElements.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE browseFormElements */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Form Builder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Form Builder */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF VALID-HANDLE(hElements) THEN
  RUN pCloseElements IN hElements.
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Dynamic Form Builder */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON MOUSE-SELECT-CLICK OF FRAME DEFAULT-FRAME
DO:
    FRAME {&FRAME-NAME}:LOAD-MOUSE-POINTER("arrow").
/*    IF VALID-HANDLE(hNewAdd) THEN                      */
/*    RUN pCreateNewAdd (LAST-EVENT:COL, LAST-EVENT:ROW).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandColumnFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandColumnFooter C-Win
ON MOUSE-SELECT-CLICK OF bandColumnFooter IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandColumnHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandColumnHeader C-Win
ON MOUSE-SELECT-CLICK OF bandColumnHeader IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandDetail C-Win
ON MOUSE-SELECT-CLICK OF bandDetail IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
    ASSIGN
        formElementTypes:SCREEN-VALUE = "Detail"
        formElementTypes:SENSITIVE    = FALSE
        formElementTypes
        .
    APPLY "VALUE-CHANGED":U TO formElementTypes.
    RUN pHideElementButtons (FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandLastPage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandLastPage C-Win
ON MOUSE-SELECT-CLICK OF bandLastPage IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandPageFooter C-Win
ON MOUSE-SELECT-CLICK OF bandPageFooter IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandPageHeader C-Win
ON MOUSE-SELECT-CLICK OF bandPageHeader IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandSummary
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandSummary C-Win
ON MOUSE-SELECT-CLICK OF bandSummary IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
    ASSIGN
        formElementTypes:SCREEN-VALUE = "Summary"
        formElementTypes:SENSITIVE    = FALSE
        formElementTypes
        .
    APPLY "VALUE-CHANGED":U TO formElementTypes.
    RUN pHideElementButtons (FALSE).
/*    hTableFields = summaryTableFields:HANDLE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bandTitle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bandTitle C-Win
ON MOUSE-SELECT-CLICK OF bandTitle IN FRAME DEFAULT-FRAME
DO:
    RUN pHideBandSections.
    ASSIGN
        formElementTypes:SCREEN-VALUE = "Header"
        formElementTypes:SENSITIVE    = FALSE
        formElementTypes
        .
    APPLY "VALUE-CHANGED":U TO formElementTypes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseBandElements
&Scoped-define SELF-NAME browseBandElements
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseBandElements C-Win
ON DEFAULT-ACTION OF browseBandElements IN FRAME DEFAULT-FRAME /* Band Elements */
DO:
    RUN pRemoveSelection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseFormElements
&Scoped-define SELF-NAME browseFormElements
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseFormElements C-Win
ON DEFAULT-ACTION OF browseFormElements IN FRAME DEFAULT-FRAME /* Form Elements */
DO:
    RUN pAddSelection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseFormElements C-Win
ON VALUE-CHANGED OF browseFormElements IN FRAME DEFAULT-FRAME /* Form Elements */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddSelection C-Win
ON CHOOSE OF btnAddSelection IN FRAME DEFAULT-FRAME /* Add Selection */
DO:
    RUN pAddSelection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown C-Win
ON CHOOSE OF btnMoveDown IN FRAME DEFAULT-FRAME
DO:
    RUN pMove (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp C-Win
ON CHOOSE OF btnMoveUp IN FRAME DEFAULT-FRAME
DO:
    RUN pMove (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME
DO:
    RUN pRemoveSelection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveSelection C-Win
ON CHOOSE OF btnRemoveSelection IN FRAME DEFAULT-FRAME /* Remove Selection */
DO:
    RUN pRemoveSelection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME elements
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL elements C-Win
ON MOUSE-SELECT-CLICK OF elements IN FRAME DEFAULT-FRAME
DO:
    RUN AOA/dynElement.w PERSISTENT SET hElements (lSuperAdmin).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME formElementTypes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL formElementTypes C-Win
ON VALUE-CHANGED OF formElementTypes IN FRAME DEFAULT-FRAME
DO:
    RUN pGetFormElements (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME formLayout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL formLayout C-Win
ON MOUSE-SELECT-CLICK OF formLayout IN FRAME DEFAULT-FRAME
DO:
    RUN pGenerateForm.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseBandElements
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
  RUN pSetFrameGrid (FRAME {&FRAME-NAME}:HANDLE).
  RUN spGetSessionParam ("Company", OUTPUT cCompany).
  RUN pGetSettings (USERID("ASI")).
  RUN enable_UI.
  RUN pHideBandSections.
  RUN pGetFormElementTypes.
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
  DISPLAY formElementTypes elementsLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnAddSelection btnMoveDown btnMoveUp btnRemove btnRemoveSelection 
         bandTitle bandPageHeader bandColumnHeader bandDetail bandColumnFooter 
         bandPageFooter bandLastPage bandSummary formLayout elements 
         formElementTypes browseFormElements browseBandElements 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddSelection C-Win 
PROCEDURE pAddSelection :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iSortOrder AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.

    DEFINE BUFFER bDynFormElement FOR dynFormElement.

    IF AVAILABLE ttFormElements AND
       NOT CAN-FIND(FIRST dynFormElement
                    WHERE dynFormElement.bandType     EQ formElementTypes
                      AND dynFormElement.elementTable EQ ttFormElements.fieldTable
                      AND dynFormElement.elementField EQ ttFormElements.fieldName) THEN
    DO WITH FRAME {&FRAME-NAME} TRANSACTION:
        FIND LAST bDynFormElement NO-LOCK
             WHERE bDynFormElement.bandType EQ formElementTypes
             USE-INDEX sortOrder NO-ERROR.
        IF AVAILABLE bDynFormElement THEN
        iSortOrder = bDynFormElement.sortOrder.  
        CREATE dynFormElement.
        ASSIGN
            iSortOrder                   = iSortOrder + 1
            dynFormElement.sortOrder     = iSortOrder
            dynFormElement.formID        = dynForm.formID
            dynFormElement.clientID      = dynForm.clientID
            dynFormElement.bandType      = formElementTypes
            dynFormElement.elementLabel  = ttFormElements.fieldLabel
            dynFormElement.elementTable  = ttFormElements.fieldTable
            dynFormElement.elementField  = ttFormElements.fieldName
            dynFormElement.elementFormat = ttFormElements.fieldFormat
            dynFormElement.dataType      = ttFormElements.dataType
            rRowID                       = ROWID(dynFormElement)
            .
        {&OPEN-QUERY-browseBandElements}
        REPOSITION browseBandElements TO ROWID rRowID.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCloseBuilder C-Win 
PROCEDURE pCloseBuilder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hElements) THEN
    RUN pCloseElements IN hElements.
    APPLY "CLOSE":U TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateForm C-Win 
PROCEDURE pGenerateForm :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cBandType   AS CHARACTER NO-UNDO INITIAL "Header,Detail,Summary".
    DEFINE VARIABLE cStyles     AS CHARACTER NO-UNDO INITIAL "Detail1_CH,Detail1_TD,Summary1_TH,Summary1_CH,Summary1_TD".
    DEFINE VARIABLE cStyleColor AS CHARACTER NO-UNDO INITIAL "#BFE1FF,#F0F8FF,#BFE1FF,#F0F8FF,#FFFFFF".
    DEFINE VARIABLE cTempTables AS CHARACTER NO-UNDO INITIAL "ttFormDetail,ttFormSummary,ttFormHeader".
    DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hColumn     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.

    DEFINE BUFFER bDynFormElement FOR dynFormElement.

    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ dynForm.subjectID
         NO-ERROR.
    IF NOT AVAILABLE dynSubject THEN RETURN.

    IF dynForm.externalForm NE "" THEN DO:
        OUTPUT TO VALUE(dynForm.externalForm).
        PUT UNFORMATTED
            '<?xml version="1.0" encoding="UTF-8"?>' SKIP
            '<!-- Created with Jaspersoft Studio version 6.6.0.final using JasperReports Library version 6.6.0  -->' SKIP
            '<jasperReport xmlns="http://jasperreports.sourceforge.net/jasperreports" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd" name="' REPLACE(dynSubject.subjectTitle," ","") '" pageWidth="595" pageHeight="842" columnWidth="535" leftMargin="20" rightMargin="20" topMargin="20" bottomMargin="20">' SKIP
            '    <property name="com.jaspersoft.studio.data.defaultdataadapter" value="' REPLACE(dynSubject.subjectTitle," ","") 'Adapter.xml"/>' SKIP
            '    <property name="com.jaspersoft.studio.data.defaultdataadapter" value="One Empty Record"/>' SKIP
            '    <style name="Title" forecolor="#FFFFFF" fontName="Times New Roman" fontSize="50" isBold="false"/>' SKIP
            '    <style name="SubTitle" forecolor="#CCCCCC" fontName="Times New Roman" fontSize="18" isBold="false"/>' SKIP
            '    <style name="Column header" forecolor="#666666" fontName="Times New Roman" fontSize="14" isBold="true"/>' SKIP
            '    <style name="Row" mode="Transparent" fontName="Times New Roman" pdfFontName="Times-Roman">' SKIP
            '        <conditionalStyle>' SKIP
            '            <conditionExpression><![CDATA[$V~{REPORT_COUNT}%2 == 0]]></conditionExpression>' SKIP
            '            <style mode="Opaque" backcolor="#F0EFEF"/>' SKIP
            '        </conditionalStyle>' SKIP
            '    </style>' SKIP
            .
        DO idx = 1 TO NUM-ENTRIES(cStyles):
            PUT UNFORMATTED
                '    <style name="' ENTRY(idx,cStyles) '" mode="Opaque" backcolor="' ENTRY(idx,cStyleColor) '">' SKIP
                '        <box>' SKIP
                '            <pen lineWidth="0.5" lineColor="#000000"/>' SKIP
                '            <topPen lineWidth="0.5" lineColor="#000000"/>' SKIP
                '            <leftPen lineWidth="0.5" lineColor="#000000"/>' SKIP
                '            <bottomPen lineWidth="0.5" lineColor="#000000"/>' SKIP
                '            <rightPen lineWidth="0.5" lineColor="#000000"/>' SKIP
                '        </box>' SKIP
                '    </style>' SKIP
                .
        END. /* do idx */
        DO idx = 1 TO NUM-ENTRIES(cTempTables):
            CASE ENTRY(idx,cTempTables):
                WHEN "ttFormDetail" THEN DO:
                    hBuffer = TEMP-TABLE ttFormDetail:DEFAULT-BUFFER-HANDLE.
                    PUT UNFORMATTED
                        '    <subDataset name="Detail1">' SKIP
                        '        <property name="net.sf.jasperreports.data.adapter" value="' REPLACE(dynSubject.subjectTitle," ","") 'Adapter.xml"/>' SKIP
                        '        <property name="net.sf.jasperreports.json.source" value="jsonFormFile"/>' SKIP
                        '        <parameter name="paramRecordID" class="java.lang.String"/>' SKIP
                        '        <queryString language="json">' SKIP
                        '            <![CDATA[' REPLACE(dynSubject.subjectTitle," ","_") '.Detail1(ttFormDetail__recordID == $P~{paramRecordID})]]>' SKIP
                        '        </queryString>' SKIP
                        .
                END.
                WHEN "ttFormSummary" THEN DO:
                    hBuffer = TEMP-TABLE ttFormSummary:DEFAULT-BUFFER-HANDLE.
                    PUT UNFORMATTED
                        '    <subDataset name="Summary1">' SKIP
                        '        <property name="net.sf.jasperreports.data.adapter" value="' REPLACE(dynSubject.subjectTitle," ","") 'Adapter.xml"/>' SKIP
                        '        <property name="net.sf.jasperreports.json.source" value="jsonFormFile"/>' SKIP
                        '        <queryString language="json">' SKIP
                        '            <![CDATA[' REPLACE(dynSubject.subjectTitle," ","_") '.Summary1]]>' SKIP
                        '        </queryString>' SKIP
                        .
                END.
                WHEN "ttFormHeader" THEN DO:
                    hBuffer = TEMP-TABLE ttFormHeader:DEFAULT-BUFFER-HANDLE.
                    PUT UNFORMATTED
/*                        '    <parameter name="jsonData" class="java.lang.String" isForPrompting="false"/>' SKIP*/
                        '    <parameter name="paramRecordID" class="java.lang.String" isForPrompting="false"/>' SKIP
                        '    <queryString language="json">' SKIP
                        '        <![CDATA[' REPLACE(dynSubject.subjectTitle," ","_") '.' REPLACE(dynSubject.subjectTitle," ","") ']]>' SKIP
                        '    </queryString>' SKIP
                        .
                END.
            END CASE.
            DO jdx = 1 TO hBuffer:NUM-FIELDS:
                hColumn = hBuffer:BUFFER-FIELD(jdx).
                IF CAN-DO("Parameters,rowType",hColumn:NAME) THEN NEXT.
                PUT UNFORMATTED
                    '        <field name="' hBuffer:NAME '__' hColumn:NAME '" class="java.lang.' fDataType(hColumn:DATA-TYPE) '">' SKIP
                    '            <property name="net.sf.jasperreports.json.field.expression" value="' hBuffer:NAME '__' hColumn:NAME '"/>' SKIP
                    '            <fieldDescription><![CDATA[' hBuffer:NAME '__' hColumn:NAME ']]></fieldDescription>' SKIP
                    '        </field>' SKIP
                    .
            END. /* do idx */
            CASE ENTRY(idx,cTempTables):
                WHEN "ttFormDetail" OR WHEN "ttFormSummary" THEN
                PUT UNFORMATTED '    </subDataset>' SKIP.
                WHEN "ttFormHeader" THEN DO:
                END.
            END CASE.
        END. /* do idx */
        PUT UNFORMATTED
            '    <background>' SKIP
            '        <band splitType="Stretch"/>' SKIP
            '    </background>' SKIP
            '    <title>' SKIP
            '        <band height="298" splitType="Stretch">' SKIP
            '            <image scaleImage="FillFrame">' SKIP
            '                <reportElement x="456" y="0" width="99" height="132"/>' SKIP
            '                <imageExpression><![CDATA["C:/Advantzware/v16/Resources/Graphics/advantzware_logo.jpg"]]></imageExpression>' SKIP
            '            </image>' SKIP
            '            <frame>' SKIP
            '                <reportElement mode="Opaque" x="0" y="0" width="451" height="95" backcolor="#4235FC"/>' SKIP
            '                <staticText>' SKIP
            '                    <reportElement style="Title" x="40" y="20" width="370" height="66"/>' SKIP
            '                    <textElement textAlignment="Center">' SKIP
            '                        <font isBold="false"/>' SKIP
            '                    </textElement>' SKIP
            '                    <text><![CDATA[' dynForm.formName ']]></text>' SKIP
            '                </staticText>' SKIP
            '            </frame>' SKIP
            '            <frame>' SKIP
            '                <reportElement mode="Opaque" x="0" y="100" width="451" height="32" forecolor="#000000" backcolor="#FA9A14"/>' SKIP
            '                <textField pattern="EEEEE dd MMMMM yyyy">' SKIP
            '                    <reportElement x="307" y="12" width="144" height="20" forecolor="#FFFFFF"/>' SKIP
            '                    <textElement textAlignment="Right" markup="html">' SKIP
            '                        <font size="12"/>' SKIP
            '                    </textElement>' SKIP
            '                    <textFieldExpression><![CDATA[new java.util.Date()]]></textFieldExpression>' SKIP
            '                </textField>' SKIP
            '                <textField>' SKIP
            '                    <reportElement x="40" y="12" width="141" height="20" forecolor="#FFFFFF"/>' SKIP
            '                    <textFieldExpression><![CDATA[$F~{ttFormHeader__invoiceIDString}]]></textFieldExpression>' SKIP
            '                </textField>' SKIP
            '                <staticText>' SKIP
            '                    <reportElement x="2" y="12" width="38" height="20" forecolor="#FFFFFF"/>' SKIP
            '                    <text><![CDATA[Invoice:]]></text>' SKIP
            '                </staticText>' SKIP
            '            </frame>' SKIP
            '        </band>' SKIP
            '    </title>' SKIP
            '    <pageHeader>' SKIP
            '        <band splitType="Stretch"/>' SKIP
            '    </pageHeader>' SKIP
            '    <columnHeader>' SKIP
            '        <band height="24" splitType="Stretch"/>' SKIP
            '    </columnHeader>' SKIP
            '    <detail>' SKIP
            '        <band height="89" splitType="Stretch">' SKIP
            .
        IF CAN-FIND(FIRST bDynFormElement
                    WHERE bDynFormElement.formID   EQ dynForm.formID
                      AND bDynFormElement.clientID EQ dynForm.clientID
                      AND bDynFormElement.bandType EQ "Detail") THEN DO:
            PUT UNFORMATTED
                '            <componentElement>' SKIP
                '                <reportElement key="" isPrintRepeatedValues="false" x="0" y="4" width="555" height="40" isRemoveLineWhenBlank="true">' SKIP
                '                    <property name="com.jaspersoft.studio.layout" value="com.jaspersoft.studio.editor.layout.VerticalRowLayout"/>' SKIP
                '                    <property name="com.jaspersoft.studio.table.style.column_header" value="Detail1_CH"/>' SKIP
                '                    <property name="com.jaspersoft.studio.table.style.detail" value="Detail1_TD"/>' SKIP
                '                    <property name="net.sf.jasperreports.export.headertoolbar.table.name" value=""/>' SKIP
                '                </reportElement>' SKIP
                '                <jr:table xmlns:jr="http://jasperreports.sourceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd">' SKIP
                '                    <datasetRun subDataset="Detail1">' SKIP
                '                        <datasetParameter name="paramRecordID">' SKIP
                '                            <datasetParameterExpression><![CDATA[$F~{ttFormHeader__recordID}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="JSON_INPUT_STREAM">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{JSON_INPUT_STREAM}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.source">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.source}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.sources">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.sources}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.date.pattern">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.date.pattern}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.number.pattern">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.number.pattern}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="JSON_LOCALE">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{JSON_LOCALE}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.locale.code">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.locale.code}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="JSON_TIME_ZONE">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{JSON_TIME_ZONE}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.timezone.id">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.timezone.id}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <connectionExpression><![CDATA[$P~{REPORT_CONNECTION}]]></connectionExpression>' SKIP
                '                    </datasetRun>' SKIP
                .
            FOR EACH bDynFormElement NO-LOCK
                WHERE bDynFormElement.formID   EQ dynForm.formID
                  AND bDynFormElement.clientID EQ dynForm.clientID
                  AND bDynFormElement.bandType EQ "Detail"
                   BY bDynFormElement.sortOrder
                :
                PUT UNFORMATTED
                    '                    <jr:column width="72">' SKIP
                    '                        <property name="com.jaspersoft.studio.components.table.model.column.name" value="ttFormDetail__' bDynFormElement.elementField '"/>' SKIP
                    '                        <jr:columnHeader style="Detail1_CH" height="14" rowSpan="1">' SKIP
                    '                            <staticText>' SKIP
                    '                                <reportElement x="0" y="0" width="72" height="14"/>' SKIP
                    '                                <textElement>' SKIP
                    '                                    <font isBold="true" isUnderline="true"/>' SKIP
                    '                                </textElement>' SKIP
                    '                                <text><![CDATA[' bDynFormElement.elementLabel ']]></text>' SKIP
                    '                            </staticText>' SKIP
                    '                        </jr:columnHeader>' SKIP
                    '                        <jr:columnFooter style="Detail1_CH" height="7" rowSpan="1"/>' SKIP
                    '                        <jr:detailCell style="Detail1_TD" height="14">' SKIP
                    '                            <textField isBlankWhenNull="true">' SKIP
                    '                                <reportElement x="0" y="0" width="72" height="14">' SKIP
                    '                                    <property name="com.jaspersoft.studio.spreadsheet.connectionID"/>' SKIP
                    '                                </reportElement>' SKIP
                    '                                <textFieldExpression><![CDATA[$F~{ttFormDetail__' bDynFormElement.elementField '}]]></textFieldExpression>' SKIP
                    '                            </textField>' SKIP
                    '                        </jr:detailCell>' SKIP
                    '                    </jr:column>' SKIP
                    .
            END. /* each bDynFormElement */
            PUT UNFORMATTED
                '                </jr:table>' SKIP
                '            </componentElement>' SKIP
                .
        END. /* if can-find */
        PUT UNFORMATTED
            '        </band>' SKIP
            '    </detail>' SKIP
            '    <columnFooter>' SKIP
            '        <band height="17" splitType="Stretch">' SKIP
            '            <line>' SKIP
            '                <reportElement positionType="FixRelativeToBottom" x="0" y="3" width="555" height="1"/>' SKIP
            '                <graphicElement>' SKIP
            '                    <pen lineWidth="0.5" lineColor="#999999"/>' SKIP
            '                </graphicElement>' SKIP
            '            </line>' SKIP
            '        </band>' SKIP
            '    </columnFooter>' SKIP
            '    <pageFooter>' SKIP
            '        <band height="75" splitType="Stretch">' SKIP
            '            <frame>' SKIP
            '                <reportElement mode="Opaque" x="0" y="51" width="555" height="24" forecolor="#D0B48E" backcolor="#000000"/>' SKIP
            '                <textField evaluationTime="Report">' SKIP
            '                    <reportElement style="Column header" x="513" y="0" width="40" height="20" forecolor="#FFFFFF"/>' SKIP
            '                    <textElement verticalAlignment="Middle">' SKIP
            '                        <font size="10" isBold="false"/>' SKIP
            '                    </textElement>' SKIP
            '                    <textFieldExpression><![CDATA[" " + $V~{PAGE_NUMBER}]]></textFieldExpression>' SKIP
            '                </textField>' SKIP
            '                <textField>' SKIP
            '                    <reportElement style="Column header" x="433" y="0" width="80" height="20" forecolor="#FFFFFF"/>' SKIP
            '                    <textElement textAlignment="Right" verticalAlignment="Middle">' SKIP
            '                        <font size="10" isBold="false"/>' SKIP
            '                    </textElement>' SKIP
            '                    <textFieldExpression><![CDATA["Page "+$V~{PAGE_NUMBER}+" of"]]></textFieldExpression>' SKIP
            '                </textField>' SKIP
            '                <textField pattern="EEEEE dd MMMMM yyyy">' SKIP
            '                    <reportElement style="Column header" x="2" y="1" width="197" height="20" forecolor="#FFFFFF"/>' SKIP
            '                    <textElement verticalAlignment="Middle">' SKIP
            '                        <font size="10" isBold="false"/>' SKIP
            '                    </textElement>' SKIP
            '                    <textFieldExpression><![CDATA[new java.util.Date()]]></textFieldExpression>' SKIP
            '                </textField>' SKIP
            '            </frame>' SKIP
            '        </band>' SKIP
            '    </pageFooter>' SKIP
            '    <summary>' SKIP
            '        <band height="92" splitType="Stretch">' SKIP
            .
        IF CAN-FIND(FIRST bDynFormElement
                    WHERE bDynFormElement.formID   EQ dynForm.formID
                      AND bDynFormElement.clientID EQ dynForm.clientID
                      AND bDynFormElement.bandType EQ "Summary") THEN DO:
            PUT UNFORMATTED
                '            <componentElement>' SKIP
                '                <reportElement x="0" y="10" width="555" height="72" isRemoveLineWhenBlank="true">' SKIP
                '                    <property name="com.jaspersoft.studio.layout" value="com.jaspersoft.studio.editor.layout.VerticalRowLayout"/>' SKIP
                '                    <property name="com.jaspersoft.studio.table.style.table_header" value="Summary1_TH"/>' SKIP
                '                    <property name="com.jaspersoft.studio.table.style.column_header" value="Summary1_CH"/>' SKIP
                '                    <property name="com.jaspersoft.studio.table.style.detail" value="Summary1_TD"/>' SKIP
                '                </reportElement>' SKIP
                '                <jr:table xmlns:jr="http://jasperreports.sourceforge.net/jasperreports/components" xsi:schemaLocation="http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd">' SKIP
                '                    <datasetRun subDataset="Summary1">' SKIP
                '                        <datasetParameter name="JSON_INPUT_STREAM">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{JSON_INPUT_STREAM}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.source">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.source}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.sources">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.sources}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.date.pattern">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.date.pattern}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.number.pattern">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.number.pattern}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="JSON_LOCALE">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{JSON_LOCALE}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.locale.code">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.locale.code}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="JSON_TIME_ZONE">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{JSON_TIME_ZONE}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <datasetParameter name="net.sf.jasperreports.json.timezone.id">' SKIP
                '                            <datasetParameterExpression><![CDATA[$P~{net.sf.jasperreports.json.timezone.id}]]></datasetParameterExpression>' SKIP
                '                        </datasetParameter>' SKIP
                '                        <connectionExpression><![CDATA[$P~{REPORT_CONNECTION}]]></connectionExpression>' SKIP
                '                    </datasetRun>' SKIP
                '                    <jr:columnGroup width="192">' SKIP
                '                        <property name="com.jaspersoft.studio.components.table.model.column.name" value="Columns [2]"/>' SKIP
                '                        <jr:tableHeader style="Summary1_TH" height="14" rowSpan="1">' SKIP
                '                            <staticText>' SKIP
                '                                <reportElement x="0" y="0" width="192" height="14"/>' SKIP
                '                                <textElement textAlignment="Center" verticalAlignment="Middle">' SKIP
                '                                    <font isBold="true"/>' SKIP
                '                                </textElement>' SKIP
                '                                <text><![CDATA[]]></text>' SKIP
                '                            </staticText>' SKIP
                '                        </jr:tableHeader>' SKIP
                .
            FOR EACH bDynFormElement NO-LOCK
                WHERE bDynFormElement.formID   EQ dynForm.formID
                  AND bDynFormElement.clientID EQ dynForm.clientID
                  AND bDynFormElement.bandType EQ "Summary"
                   BY bDynFormElement.sortOrder
                :
                PUT UNFORMATTED
                    '                        <jr:column width="48">' SKIP
                    '                            <property name="com.jaspersoft.studio.components.table.model.column.name" value="ttFormSummary__' bDynFormElement.elementField '"/>' SKIP
                    '                            <jr:tableHeader style="Summary1_TH" height="0" rowSpan="1"/>' SKIP
                    '                            <jr:tableFooter style="Summary1_TH" height="7" rowSpan="1"/>' SKIP
                    '                            <jr:columnHeader style="Summary1_CH" height="14" rowSpan="1">' SKIP
                    '                                <staticText>' SKIP
                    '                                    <reportElement x="0" y="0" width="48" height="14"/>' SKIP
                    '                                    <textElement>' SKIP
                    '                                        <font isBold="true" isUnderline="true"/>' SKIP
                    '                                    </textElement>' SKIP
                    '                                    <text><![CDATA[' bDynFormElement.elementLabel ']]></text>' SKIP
                    '                                </staticText>' SKIP
                    '                            </jr:columnHeader>' SKIP
                    '                            <jr:columnFooter style="Summary1_CH" height="0" rowSpan="1"/>' SKIP
                    '                            <jr:detailCell style="Summary1_TD" height="14">' SKIP
                    '                                <textField isBlankWhenNull="true">' SKIP
                    '                                    <reportElement x="0" y="0" width="48" height="14">' SKIP
                    '                                        <property name="com.jaspersoft.studio.spreadsheet.connectionID"/>' SKIP
                    '                                    </reportElement>' SKIP
                    '                                    <textFieldExpression><![CDATA[$F~{ttFormSummary__' bDynFormElement.elementField '}]]></textFieldExpression>' SKIP
                    '                                </textField>' SKIP
                    '                            </jr:detailCell>' SKIP
                    '                        </jr:column>' SKIP
                    .
            END. /* each bDynFormElement */
            PUT UNFORMATTED
                '                   </jr:columnGroup>' SKIP
                '                </jr:table>' SKIP
                '            </componentElement>' SKIP
                .
        END. /* if can-find */
        PUT UNFORMATTED
            '        </band>' SKIP
            '    </summary>' SKIP
            .
        PUT UNFORMATTED
            '</jasperReport>' SKIP
            .
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT notepad.exe VALUE(dynForm.externalForm).

        OUTPUT TO VALUE(REPLACE(dynForm.externalForm,".jrxml","Adapter.xml")).
        PUT UNFORMATTED
            '<?xml version="1.0" encoding="UTF-8" ?>' SKIP
            '<jsonDataAdapter class="net.sf.jasperreports.data.json.JsonDataAdapterImpl">' SKIP
            '  <name>Invoice Form Adapter</name>' SKIP
            '  <dataFile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="repositoryDataLocation">' SKIP
            '    <location>' REPLACE(dynSubject.subjectTitle," ","") '.json</location>' SKIP
            '  </dataFile>' SKIP
            '  <language>json</language>' SKIP
            '  <useConnection>true</useConnection>' SKIP
            '  <timeZone xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:java="http://java.sun.com" xsi:type="java:java.lang.String">America/New_York</timeZone>' SKIP
            '  <locale xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:java="http://java.sun.com" xsi:type="java:java.lang.String">en_US</locale>' SKIP
            '</jsonDataAdapter>' SKIP
            .
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT notepad.exe VALUE(REPLACE(dynForm.externalForm,".jrxml","Adapter.xml")).
        
        OUTPUT TO VALUE(REPLACE(dynForm.externalForm,".jrxml",".json")).
        DO idx = 1 TO NUM-ENTRIES(cBandType):
            CASE ENTRY(idx,cBandType):
                WHEN "Header" THEN DO:
                    hBuffer = TEMP-TABLE ttFormHeader:DEFAULT-BUFFER-HANDLE.
                    PUT UNFORMATTED
                        '~{' SKIP
                        '  "' REPLACE(dynSubject.subjectTitle," ","_") '": ~{' SKIP
                        '    "' REPLACE(dynSubject.subjectTitle," ","") '": [' SKIP
                        .
                END.
                WHEN "Detail" THEN DO:
                    hBuffer = TEMP-TABLE ttFormDetail:DEFAULT-BUFFER-HANDLE.
                    PUT UNFORMATTED
                        '    "' ENTRY(idx,cBandType) '1": [' SKIP
                        .
                END.
                WHEN "Summary" THEN DO:
                    hBuffer = TEMP-TABLE ttFormSummary:DEFAULT-BUFFER-HANDLE.
                    PUT UNFORMATTED
                        '    "' ENTRY(idx,cBandType) '1": [' SKIP
                        .
                END.
            END CASE.
            DO jdx = 1 TO hBuffer:NUM-FIELDS:
                hColumn = hBuffer:BUFFER-FIELD(jdx).
                IF CAN-DO("Parameters,rowType",hColumn:NAME) THEN NEXT.
                IF jdx EQ 1 THEN
                PUT UNFORMATTED '      ~{' SKIP.
                PUT UNFORMATTED
                    '        "'
                    hBuffer:NAME '__'
                    hColumn:NAME '": "'
                    REPLACE(hColumn:FORMAT,">","9") '"'
                    .
                IF jdx LT hBuffer:NUM-FIELDS THEN
                PUT UNFORMATTED ',' SKIP.
                ELSE
                PUT UNFORMATTED SKIP '      }' SKIP.
            END. /* do jdx */
            PUT UNFORMATTED '    ]'.
            IF idx LT NUM-ENTRIES(cBandType) THEN
            PUT UNFORMATTED ',' SKIP.
        END. /* do idx */
        PUT UNFORMATTED
            '  }' SKIP
            '}' SKIP
            .
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT notepad.exe VALUE(REPLACE(dynForm.externalForm,".jrxml",".json")).
    END. /* if externalform */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFormElements C-Win 
PROCEDURE pGetFormElements :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormElementType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttFormElements.
    CASE ipcFormElementType:
        WHEN "Component" THEN
        hBuffer = TEMP-TABLE ttFormComponent:DEFAULT-BUFFER-HANDLE.
        WHEN "Header" THEN
        hBuffer = TEMP-TABLE ttFormHeader:DEFAULT-BUFFER-HANDLE.
        WHEN "Detail" THEN
        hBuffer = TEMP-TABLE ttFormDetail:DEFAULT-BUFFER-HANDLE.
        WHEN "Summary" THEN
        hBuffer = TEMP-TABLE ttFormSummary:DEFAULT-BUFFER-HANDLE.
    END CASE.
    DO idx = 1 TO hBuffer:NUM-FIELDS:
        hColumn = hBuffer:BUFFER-FIELD(idx).
        IF CAN-DO("Parameters,recordID,rowType",hColumn:NAME) THEN NEXT.
        CREATE ttFormElements.
        ASSIGN
            ttFormElements.fieldTable  = hBuffer:NAME
            ttFormElements.fieldName   = hColumn:NAME
            ttFormElements.fieldLabel  = hColumn:LABEL
            ttFormElements.fieldFormat = hColumn:FORMAT
            ttFormElements.dataType    = hColumn:DATA-TYPE
            .
    END. /* do idx */
    {&OPEN-QUERY-browseFormElements}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFormElementTypes C-Win 
PROCEDURE pGetFormElementTypes :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    formElementTypes:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?.
    formElementTypes:ADD-LAST("Component").
    formElementTypes:ADD-LAST("Header").
    formElementTypes:ADD-LAST("Detail").
    formElementTypes:ADD-LAST("Summary").
    formElementTypes:SCREEN-VALUE = formElementTypes:ENTRY(1).
    APPLY "VALUE-CHANGED":U TO formElementTypes.

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
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ cCompany
                      AND user-print.program-id EQ "{&programID}"
                      AND user-print.user-id    EQ ipcUserID) THEN
    RUN pSaveSettings (ipcUserID).
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHideBandSections C-Win 
PROCEDURE pHideBandSections :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        RUN pHideElementButtons (TRUE).
        ASSIGN
            formElementTypes:SENSITIVE = TRUE
            hTableFields = ?
            .
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHideElementButtons C-Win 
PROCEDURE pHideElementButtons :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplHide AS LOGICAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            btnAddSelection:HIDDEN    = iplHide 
            btnMoveDown:HIDDEN        = iplHide
            btnMoveUp:HIDDEN          = iplHide
            btnRemove:HIDDEN          = iplHide
            btnRemoveSelection:HIDDEN = iplHide
            BROWSE browseBandElements:HIDDEN = iplHide
            .
        IF NOT iplHide THEN
        {&OPEN-QUERY-browseBandElements}
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitBuilder C-Win 
PROCEDURE pInitBuilder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    FIND FIRST dynForm NO-LOCK WHERE ROWID(dynForm) EQ iprRowID.
    ASSIGN
        lSuperAdmin = DYNAMIC-FUNCTION("sfIsUserSuperAdmin")
        {&WINDOW-NAME}:TITLE = "Dynamic Form Builder - Form: "
                             + STRING(dynForm.formID)
                             + (IF dynForm.clientID GT 0 THEN " - ID: "
                             + STRING(dynForm.clientID) ELSE "") + " - "
                             + dynForm.formName
                             .
    {&WINDOW-NAME}:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMove C-Win 
PROCEDURE pMove :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.

    DEFINE VARIABLE iCurrent AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo  AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID   AS ROWID   NO-UNDO.

    DEFINE BUFFER bDynFormElement FOR dynFormElement.

    IF NOT AVAILABLE dynFormElement THEN RETURN.
    /* first column, can't move up */
    IF dynFormElement.sortOrder EQ 1 AND ipiMove EQ -1 THEN RETURN.
    /* check if at bottom, can't move down */
    FOR EACH bDynFormElement NO-LOCK
        WHERE bDynFormElement.formID   EQ dynForm.formID
          AND bDynFormElement.clientID EQ dynForm.clientID
          AND bDynFormElement.bandType EQ formElementTypes 
           BY bDynFormElement.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    IF AVAILABLE bDynFormElement THEN DO:
        /* check if at bottom, can't move down */
        IF bDynFormElement.sortOrder EQ dynFormElement.sortOrder AND
           ipiMove EQ 1 THEN
        RETURN.
    END. /* if avail */
    ELSE RETURN.
    ASSIGN
        iCurrent = dynFormElement.sortOrder
        iMoveTo  = dynFormElement.sortOrder + ipiMove
        .
    FIND FIRST bDynFormElement
         WHERE bDynFormElement.formID    EQ dynForm.formID
           AND bDynFormElement.clientID  EQ dynForm.clientID
           AND bDynFormElement.bandType  EQ formElementTypes
           AND bDynFormElement.sortOrder EQ iMoveTo
         NO-ERROR.
    IF AVAILABLE bDynFormElement THEN
    DO TRANSACTION:
        FIND CURRENT dynFormElement EXCLUSIVE-LOCK.
        ASSIGN
            dynFormElement.sortOrder  = 0
            bDynFormElement.sortOrder = iCurrent
            dynFormElement.sortOrder  = iMoveTo
            .
        FIND CURRENT dynFormElement NO-LOCK.
        rRowID = ROWID(dynFormElement).
        {&OPEN-QUERY-browseBandElements}
        REPOSITION browseBandElements TO ROWID rRowID.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemoveSelection C-Win 
PROCEDURE pRemoveSelection :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF NOT AVAILABLE dynFormElement THEN RETURN.

    DO TRANSACTION:
        FIND CURRENT dynFormElement EXCLUSIVE-LOCK.
        DELETE dynFormElement.
    END. /* do trans */
    RUN pSetOrder.

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
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = cCompany
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date   = TODAY
        user-print.next-time   = TIME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetFrameGrid C-Win 
PROCEDURE pSetFrameGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.

    ASSIGN
        iphFrame:GRID-FACTOR-VERTICAL    = 4
        iphFrame:GRID-FACTOR-HORIZONTAL  = 10
        iphFrame:GRID-UNIT-WIDTH-PIXELS  = 5
        iphFrame:GRID-UNIT-HEIGHT-PIXELS = 5
        iphFrame:GRID-SNAP               = YES
        iphFrame:GRID-VISIBLE            = YES
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetOrder C-Win 
PROCEDURE pSetOrder :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DEFINE BUFFER bDynFormElement FOR dynFormElement.

    FOR EACH bDynFormElement EXCLUSIVE-LOCK
        WHERE bDynFormElement.formID   EQ dynForm.formID
          AND bDynFormElement.clientID EQ dynForm.clientID
          AND bDynFormElement.bandType EQ formElementTypes 
           BY bDynFormElement.sortOrder
        :
        ASSIGN
            idx = idx + 1
            bDynFormElement.sortOrder = idx
            .
    END. /* for each */
    {&OPEN-QUERY-browseBandElements}

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
        RUN pHideBandSections.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT    = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH     = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT            = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH             = {&WINDOW-NAME}:WIDTH
            BROWSE browseFormElements:HEIGHT      = FRAME {&FRAME-NAME}:HEIGHT
                                                  - BROWSE browseFormElements:ROW + .62
            .
        VIEW FRAME {&FRAME-NAME}.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDataType C-Win 
FUNCTION fDataType RETURNS CHARACTER
  (ipcDataType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.

    CASE ipcDataType:
        WHEN "Character" THEN
        cDataType = "String".
        WHEN "Decimal" THEN
        cDataType = "Double".
        WHEN "Integer" THEN
        cDataType = "Integer".
        OTHERWISE
        cDataType = "String".
    END CASE.
    RETURN cDataType.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

