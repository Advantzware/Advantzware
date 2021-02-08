&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: jobSequencer.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters: <none>

  Output Parameters: <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{{&includes}/sharedVars.i}
{{&viewers}/includes/sharedVars.i}
{{&includes}/ttblJob.i}

DEFINE VARIABLE boardHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cells AS HANDLE NO-UNDO EXTENT 600.
DEFINE VARIABLE col# AS INTEGER NO-UNDO.
DEFINE VARIABLE currentJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE detailWindow AS LOGICAL NO-UNDO.
DEFINE VARIABLE flashLight AS LOGICAL NO-UNDO INITIAL YES.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE resource# AS INTEGER NO-UNDO.
DEFINE VARIABLE saveSeq AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE resources NO-UNDO
  FIELD resource AS CHARACTER
  FIELD order AS INTEGER
    INDEX resources IS PRIMARY UNIQUE order resource.

DEFINE TEMP-TABLE ttblSeq NO-UNDO
  FIELD rowNo AS INTEGER LABEL 'Row' FORMAT 'zzz9'
  FIELD jobSequence LIKE ttblJob.jobSequence EXTENT 200
  FIELD job LIKE ttblJob.job COLUMN-LABEL 'Resource!Job' EXTENT 200
  FIELD resourceSequence LIKE ttblJob.resourceSequence EXTENT 200
  FIELD jobRowID AS ROWID EXTENT 200
    INDEX ttblSeq IS PRIMARY UNIQUE rowNo.

DEFINE BUFFER bTtblSeq FOR ttblSeq.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttblSeq

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttblSeq.rowNo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttblSeq INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttblSeq INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttblSeq
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttblSeq


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnResSequencer BROWSE-1 btnPendingReturn ~
btnStatus btnNotes btnDetail btnFlashLight btnUp btnDown btnSave btnRefresh ~
btnExit 
&Scoped-Define DISPLAYED-OBJECTS selectedValues 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnResSequencer 
&Scoped-define List-2 btnResSequencer 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD currentJob wWin 
FUNCTION currentJob RETURNS INTEGER
  (ipIdx AS INTEGER,ipType AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDetail 
     IMAGE-UP FILE "schedule/images/detailwinoff.bmp":U
     LABEL "Detail" 
     SIZE 5 BY 1.1 TOOLTIP "Turn Detail Window Display Off".

DEFINE BUTTON btnDown 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "&DN" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Down (Alt-D)".

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "E&xit" 
     SIZE 5 BY 1.1 TOOLTIP "Exit (Alt-X)"
     BGCOLOR 8 .

DEFINE BUTTON btnFlashLight 
     IMAGE-UP FILE "schedule/images/lightbulbon.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Turn Highlight On".

DEFINE BUTTON btnNotes 
     IMAGE-UP FILE "schedule/images/notetack.bmp":U
     LABEL "Job Notes" 
     SIZE 5 BY 1.1 TOOLTIP "Access Job Notes".

DEFINE BUTTON btnPendingReturn 
     IMAGE-UP FILE "schedule/images/pending.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Return Job to Pending".

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "&RF" 
     SIZE 5 BY 1.1 TOOLTIP "Refresh (Alt-R)".

DEFINE BUTTON btnResSequencer 
     IMAGE-UP FILE "schedule/images/setseq.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Access Resource Sequencer".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "&SV" 
     SIZE 5 BY 1.1 TOOLTIP "Save Sequences (Alt-S)".

DEFINE BUTTON btnStatus 
     IMAGE-UP FILE "schedule/images/save.bmp":U
     LABEL "Status" 
     SIZE 5 BY 1.1 TOOLTIP "Access Status Checkoff".

DEFINE BUTTON btnUp 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "&UP" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Up (Alt-U)".

DEFINE VARIABLE selectedValues AS CHARACTER FORMAT "X(256)":U INITIAL "  Loading ..." 
      VIEW-AS TEXT 
     SIZE 153 BY .95
     BGCOLOR 4 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 7 BY 5.24
     BGCOLOR 5 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 7 BY 5
     BGCOLOR 2 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 7 BY 2.62
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 7 BY 1.67
     BGCOLOR 4 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttblSeq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wWin _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttblSeq.rowNo LABEL-FONT 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 153 BY 27.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnResSequencer AT ROW 1.24 COL 2 HELP
          "Click to Access Resource Sequencer"
     BROWSE-1 AT ROW 1.95 COL 8 HELP
          "Select Column and Row"
     btnPendingReturn AT ROW 2.43 COL 2 HELP
          "Click to Return Job to Pending"
     btnStatus AT ROW 3.86 COL 2 HELP
          "Click to Access Status Checkoff"
     btnNotes AT ROW 5.05 COL 2 HELP
          "Click to Access Job Notes"
     btnDetail AT ROW 6.24 COL 2 HELP
          "Tune Detail Window Display On/Off"
     btnFlashLight AT ROW 7.43 COL 2 HELP
          "Turn Highlight On/Off"
     btnUp AT ROW 8.86 COL 2 HELP
          "Click to Move Current Column and Row Up"
     btnDown AT ROW 10.05 COL 2 HELP
          "Click to Move Current Column and Row Down"
     btnSave AT ROW 11.24 COL 2 HELP
          "Click to Save Sequences"
     btnRefresh AT ROW 12.43 COL 2 HELP
          "Click to Refresh"
     btnExit AT ROW 14.1 COL 2 HELP
          "Click to Exit"
     selectedValues AT ROW 1 COL 6 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 8.62 COL 1
     RECT-12 AT ROW 3.62 COL 1
     RECT-13 AT ROW 1 COL 1
     RECT-14 AT ROW 13.86 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Job Sequencer - Scheduler"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
                                                                        */
/* BROWSE-TAB BROWSE-1 btnResSequencer fMain */
ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME fMain     = 1
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME fMain = TRUE
       BROWSE-1:SEPARATOR-FGCOLOR IN FRAME fMain      = 0.

ASSIGN 
       btnDetail:PRIVATE-DATA IN FRAME fMain     = 
                "Turn Detail Window Display".

ASSIGN 
       btnFlashLight:PRIVATE-DATA IN FRAME fMain     = 
                "Turn Highlight".

/* SETTINGS FOR BUTTON btnResSequencer IN FRAME fMain
   1 2                                                                  */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-12 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-13 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-14 IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN selectedValues IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblSeq INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Job Sequencer - Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Job Sequencer - Scheduler */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN checkSaveSeq.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Job Sequencer - Scheduler */
DO:
  RUN winReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 wWin
ON ROW-DISPLAY OF BROWSE-1 IN FRAME fMain
DO:
  ASSIGN
    ttblSeq.rowNo:BGCOLOR IN BROWSE {&BROWSE-NAME} = 7
    ttblSeq.rowNo:FGCOLOR = 15.
  DO i = 2 TO col# BY 3:
    ASSIGN
      cells[i]:BGCOLOR = 11
      cells[i]:FONT = 6
      cells[i + 1]:BGCOLOR = currentJob(i,'BGCOLOR')
      cells[i + 1]:FGCOLOR = currentJob(i,'FGCOLOR')
      cells[i + 1]:FONT = currentJob(i,'FONT')
      cells[i + 1]:FORMAT = 'X(20)'
      cells[i + 2]:BGCOLOR = 8.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 wWin
ON START-SEARCH OF BROWSE-1 IN FRAME fMain
DO:
  IF NOT AVAILABLE ttblSeq THEN RETURN NO-APPLY.
  IF {&BROWSE-NAME}:CURRENT-COLUMN:PRIVATE-DATA EQ ? THEN RETURN NO-APPLY.
  IF saveSeq THEN
  DO:
    MESSAGE 'Before Changing Resource,' SKIP 'Save Current Sequence Changes?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE saveSeq.
    IF saveSeq THEN DO:
      RUN saveSeq.
      RETURN.
    END.
    ELSE
    RUN reopenBrowse.
  END. /* if saveseq */
  DO idx = 3 TO {&BROWSE-NAME}:NUM-COLUMNS BY 3:
    {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx):LABEL-BGCOLOR = ?.
  END.
  idx = INTEGER(ENTRY(2,{&BROWSE-NAME}:CURRENT-COLUMN:PRIVATE-DATA)).
  {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx):LABEL-BGCOLOR = 10.
  idx = INTEGER(ENTRY(1,{&BROWSE-NAME}:CURRENT-COLUMN:PRIVATE-DATA)).
  RUN selectRow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 wWin
ON VALUE-CHANGED OF BROWSE-1 IN FRAME fMain
DO:
  RUN selectRow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetail wWin
ON CHOOSE OF btnDetail IN FRAME fMain /* Detail */
DO:
  {{&includes}/{&Board}/btnDetail.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown wWin
ON CHOOSE OF btnDown IN FRAME fMain /* DN */
DO:
  RUN moveSeq (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fMain /* Exit */
DO:
  RUN checkSaveSeq.
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFlashLight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFlashLight wWin
ON CHOOSE OF btnFlashLight IN FRAME fMain
DO:
  ASSIGN
    flashLight = NOT flashLight
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT flashLight,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/lightBulb' +
                             TRIM(STRING(flashLight,'On/Off')) + '.bmp').
  BROWSE {&BROWSE-NAME}:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNotes wWin
ON CHOOSE OF btnNotes IN FRAME fMain /* Job Notes */
DO:
  RUN jobNotes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPendingReturn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPendingReturn wWin
ON CHOOSE OF btnPendingReturn IN FRAME fMain
DO:
  RUN pendingReturn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh wWin
ON CHOOSE OF btnRefresh IN FRAME fMain /* RF */
DO:
  saveSeq = NO.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResSequencer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResSequencer wWin
ON CHOOSE OF btnResSequencer IN FRAME fMain
DO:
  RUN resSequencer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave wWin
ON CHOOSE OF btnSave IN FRAME fMain /* SV */
DO:
  RUN saveSeq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStatus wWin
ON CHOOSE OF btnStatus IN FRAME fMain /* Status */
DO:
  RUN statusCheckoff.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp wWin
ON CHOOSE OF btnUp IN FRAME fMain /* UP */
DO:
  RUN moveSeq (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

{{&viewers}/includes/winTitle.i}

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildCells wWin 
PROCEDURE buildCells :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  resource# = 0.
  FOR EACH resources NO-LOCK WITH FRAME {&FRAME-NAME}:
    ASSIGN
      resource# = resource# + 1
      pHandle = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblSeq.jobSequence[' + STRING(resource#) + ']')
      pHandle:LABEL = 'Seq'
      pHandle:LABEL-FONT = 6
      pHandle:WIDTH-CHARS = 5
      pHandle:LABEL-BGCOLOR = 11
      pHandle:FORMAT = 'zzzz'
      pHandle:PRIVATE-DATA = STRING(resource#) + ',' + STRING(resource# * 3)
      pHandle = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblSeq.job[' + STRING(resource#) + ']')
      pHandle:LABEL = resources.resource + '!Job'
      pHandle:LABEL-FONT = 6
      pHandle:WIDTH-CHARS = 15
      pHandle:PRIVATE-DATA = STRING(resource#) + ',' + STRING(resource# * 3)
      pHandle = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblSeq.resourceSequence[' + STRING(resource#) + ']')
      pHandle:LABEL = 'ResSeq'
      pHandle:LABEL-FONT = 6
      pHandle:WIDTH-CHARS = 9
      pHandle:LABEL-BGCOLOR = 8
      pHandle:FORMAT = 'zzzz'
      pHandle:PRIVATE-DATA = STRING(resource#) + ',' + STRING(resource# * 3).
  END. /* each ttblresource */
  DO col# = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
    cells[col#] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(col#).
  END.
  col# = {&BROWSE-NAME}:NUM-COLUMNS.
  SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkSaveSeq wWin 
PROCEDURE checkSaveSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF saveSeq THEN
  DO WITH FRAME {&FRAME-NAME}:
    MESSAGE 'Changes have been made ...' SKIP
      'Save Job Sequence Changes?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE saveSeq.
    IF saveSeq THEN
    APPLY 'CHOOSE':U TO btnSave.
  END. /* if saveseq */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY selectedValues 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE btnResSequencer BROWSE-1 btnPendingReturn btnStatus btnNotes btnDetail 
         btnFlashLight btnUp btnDown btnSave btnRefresh btnExit 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initSelectedValues wWin 
PROCEDURE initSelectedValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    currentJob = ''
    selectedValues:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = '  Selected Resource: <None> - Job: <None>'
    selectedValues
    idx = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobNotes wWin 
PROCEDURE jobNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ttblSeq OR idx EQ 0 OR ttblSeq.jobSequence[idx] EQ 0 THEN RETURN.
  {{&includes}/{&Board}/btnJobNotes.i ttblSeq.jobRowID[idx]}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadSeq wWin 
PROCEDURE loadSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  EMPTY TEMP-TABLE ttblSeq.
  EMPTY TEMP-TABLE resources.
  ASSIGN
    currentJob = ''
    selectedValues:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '  Loading ...'
    selectedValues
    idx = 0.
  FOR EACH ttblResource EXCLUSIVE-LOCK BY ttblResource.order:
    CREATE resources.
    ASSIGN
      i = i + 1
      resources.order = i
      resources.resource = ttblResource.resource.
    FOR EACH ttblJob NO-LOCK WHERE ttblJob.resource EQ resources.resource:
      FIND ttblSeq EXCLUSIVE-LOCK WHERE ttblSeq.rowNo EQ ttblJob.jobSequence NO-ERROR.
      IF NOT AVAILABLE ttblSeq THEN
      DO:
        CREATE ttblSeq.
        ttblSeq.rowNo = ttblJob.jobSequence.
      END. /* if not avail */
      ASSIGN
        ttblSeq.jobSequence[i] = ttblJob.jobSequence
        ttblSeq.job[i] = ttblJob.job
        ttblSeq.resourceSequence[i] = ttblJob.resourceSequence
        ttblSeq.jobRowID[i] = ROWID(ttblJob).
    END. /* each ttbljob */
  END. /* each ttblresource */
  SESSION:SET-WAIT-STATE('').
  APPLY 'ENTRY':U TO {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize wWin 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  {&WINDOW-NAME}:WINDOW-STATE = 1.
  RUN loadSeq.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN winReSize.
  RUN buildCells.
  RUN initSelectedValues.
  BROWSE {&BROWSE-NAME}:REFRESH().
  APPLY 'CHOOSE':U TO btnFlashLight IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveSeq wWin 
PROCEDURE moveSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvJob AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvResourceSequence AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvRowID AS ROWID NO-UNDO.

  IF AVAILABLE ttblSeq AND idx NE 0 AND ttblSeq.jobSequence[idx] NE 0 THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND bTtblSeq EXCLUSIVE-LOCK
         WHERE bTtblSeq.rowNo EQ ttblSeq.rowNo + ipMove NO-ERROR.
    IF AVAILABLE bTtblSeq AND bTtblSeq.jobSequence[idx] NE 0 THEN
    DO:
      ASSIGN
        lvJob = ttblSeq.job[idx]
        lvResourceSequence = ttblSeq.resourceSequence[idx]
        lvRowID = ttblSeq.jobRowID[idx]
        ttblSeq.job[idx] = bTtblSeq.job[idx]
        ttblSeq.resourceSequence[idx] = bTtblSeq.resourceSequence[idx]
        ttblSeq.jobRowID[idx] = bTtblSeq.jobRowID[idx]
        bTtblSeq.job[idx] = lvJob
        bTtblSeq.resourceSequence[idx] = lvResourceSequence
        bTtblSeq.jobRowID[idx] = lvRowID
        lvRowID = ROWID(bTtblSeq)
        saveSeq = YES.
      BROWSE {&BROWSE-NAME}:SET-REPOSITIONED-ROW(bTtblSeq.rowNo,'CONDITIONAL').
      REPOSITION {&BROWSE-NAME} TO ROWID(lvRowID) NO-ERROR.
    END. /* avail bttblseq */
  END. /* avail ttblseq */
  APPLY 'ENTRY':U TO {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE passHandle wWin 
PROCEDURE passHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipBoard AS CHARACTER NO-UNDO.

  ASSIGN
    boardHandle = ipHandle
    boardType = ipBoard.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pendingReturn wWin 
PROCEDURE pendingReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE continue AS LOGICAL NO-UNDO.

  IF NOT AVAILABLE ttblSeq OR idx EQ 0 OR ttblSeq.jobSequence[idx] EQ 0 THEN RETURN.
  RUN pendingReturn IN boardHandle (ttblSeq.job[idx],OUTPUT continue).
  IF continue THEN RUN reopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowse wWin 
PROCEDURE reopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO idx = 3 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} BY 3:
    {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx):LABEL-BGCOLOR = ?.
  END.
  RUN loadSeq.
  {&OPEN-QUERY-{&BROWSE-NAME}}
  BROWSE {&BROWSE-NAME}:SET-REPOSITIONED-ROW(1,'CONDITIONAL').
  RUN initSelectedValues.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resSequencer wWin 
PROCEDURE resSequencer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE updateBoard AS LOGICAL NO-UNDO.

  RUN {&prompts}/resSequencer.w (OUTPUT updateBoard).
  IF updateBoard THEN RUN updateBoard (NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveSeq wWin 
PROCEDURE saveSeq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE updateBoard AS LOGICAL NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  FOR EACH ttblSeq NO-LOCK:
    DO i = 1 TO resource#:
      IF ttblSeq.jobSequence[i] EQ 0 THEN NEXT.
      FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ ttblSeq.jobRowID[i] NO-ERROR.
      IF NOT AVAILABLE ttblJob OR
         ttblJob.jobSequence EQ ttblSeq.jobSequence[i] THEN NEXT.
      ASSIGN
        ttblJob.jobSequence = ttblSeq.jobSequence[i]
        updateBoard = YES.
    END. /* do i */
  END. /* each ttblseq */
  SESSION:SET-WAIT-STATE('').
  IF updateBoard THEN
  DO:
    RUN updateBoard (YES).
    MESSAGE 'Job Sequence Save Complete!' VIEW-AS ALERT-BOX.
  END. /* if updateboard */
  saveSeq = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectRow wWin 
PROCEDURE selectRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    currentJob = ''.
    IF idx EQ 0 THEN RETURN.
    FIND resources NO-LOCK WHERE resources.order EQ idx NO-ERROR.
    IF NOT AVAILABLE resources THEN RETURN.
    RUN setObjectName IN boardHandle ('Resource',resources.resource).
    ASSIGN
      currentJob = ttblSeq.job[idx]
      selectedValues:SCREEN-VALUE = '  Selected Resource: ' + resources.resource +
                                    ' - Job: ' + currentJob.
    IF flashLight THEN
    BROWSE {&BROWSE-NAME}:REFRESH().
    IF NOT detailWindow THEN RETURN.
    FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ ttblSeq.jobRowID[idx] NO-ERROR.
    IF NOT AVAILABLE ttblJob THEN RETURN.
    RUN runDetailJob IN boardHandle (ROWID(ttblJob),ttblJob.rowIDs).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE statusCheckoff wWin 
PROCEDURE statusCheckoff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ttblSeq OR idx EQ 0 OR ttblSeq.jobSequence[idx] EQ 0 THEN RETURN.
  RUN checkSaveSeq.
  {{&includes}/{&Board}/btnComplete.i ttblSeq.jobRowID[idx] boardHandle}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateBoard wWin 
PROCEDURE updateBoard :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPackResource AS LOGICAL NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  FOR EACH ttblJob EXCLUSIVE-LOCK:
    IF NOT ttblJob.jobLocked AND NOT ttblJob.jobCompleted THEN
    ttblJob.startDateTime = ?.
  END. /* each ttbljob */
  RUN setJobDateTime IN boardHandle (?,NO).
  IF ipPackResource THEN
  RUN packResource IN boardHandle (resources.resource).
  RUN buildBoard IN boardHandle (YES).
  SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize wWin 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF {&WINDOW-NAME}:HEIGHT-PIXELS LT 600 THEN {&WINDOW-NAME}:HEIGHT-PIXELS = 600.
  IF {&WINDOW-NAME}:WIDTH-PIXELS LT 800 THEN {&WINDOW-NAME}:WIDTH-PIXELS = 800.
  ASSIGN
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    BROWSE {&BROWSE-NAME}:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS - 35
    BROWSE {&BROWSE-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 20
    selectedValues:WIDTH-PIXELS = BROWSE {&BROWSE-NAME}:WIDTH-PIXELS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION currentJob wWin 
FUNCTION currentJob RETURNS INTEGER
  (ipIdx AS INTEGER,ipType AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE isCurrent AS LOGICAL NO-UNDO.

  isCurrent = flashLight AND
              currentJob NE '' AND
              currentJob EQ ttblSeq.job[INT((ipIdx + 1) / 3)].
  CASE ipType:
    WHEN 'BGCOLOR' THEN
    RETURN IF isCurrent THEN 14 ELSE ?.
    WHEN 'FGCOLOR' THEN
    RETURN IF isCurrent THEN 12 ELSE ?.
    WHEN 'FONT' THEN
    RETURN IF isCurrent THEN 6 ELSE 2.
  END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

