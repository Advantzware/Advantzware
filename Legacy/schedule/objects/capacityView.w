&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: capacityView.w

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
{{&includes}/specialTime.i}

DEFINE VARIABLE boardHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cells AS HANDLE NO-UNDO EXTENT 600.
DEFINE VARIABLE col# AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE resources NO-UNDO
  FIELD resource AS CHARACTER
  FIELD order AS INTEGER
    INDEX resources IS PRIMARY UNIQUE order resource.

DEFINE TEMP-TABLE ttblView NO-UNDO
  FIELD boardDate AS DATE FORMAT '99.99.9999' COLUMN-LABEL 'Resource!Date'
  FIELD dayOfWeek AS CHARACTER FORMAT 'X(5)' COLUMN-LABEL 'Day'
  FIELD timeUsed AS CHARACTER EXTENT 200 FORMAT 'X(30)'
  FIELD timeAvail AS CHARACTER EXTENT 200 FORMAT 'X(12)'
    INDEX ttblView IS PRIMARY UNIQUE boardDate.

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
&Scoped-define INTERNAL-TABLES ttblView

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ttblView.boardDate ttblView.dayOfWeek   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ttblView
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH ttblView.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ttblView
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ttblView


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS boardDateStart btnCalendar-1 boardDateEnd ~
btnCalendar-2 btnRefresh btnExit includeDowntime BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS boardDateStart boardDateEnd ~
includeDowntime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnRefresh 
&Scoped-define List-2 btnRefresh 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD numericDateTime wWin 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "schedule/images/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "E&xit" 
     SIZE 5 BY 1.1 TOOLTIP "Exit (Alt-X)"
     BGCOLOR 8 .

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "schedule/images/refresh.bmp":U
     LABEL "&Refresh" 
     SIZE 5 BY 1.1 TOOLTIP "Refresh View (Alt-R)".

DEFINE VARIABLE boardDateEnd AS DATE FORMAT "99.99.9999":U 
     LABEL "&End" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Date Range End"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE boardDateStart AS DATE FORMAT "99.99.9999":U 
     LABEL "Date Range - &Start" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Date Range Start"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE includeDowntime AS LOGICAL INITIAL yes 
     LABEL "&Include Downtime" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ttblView SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 wWin _FREEFORM
  QUERY BROWSE-1 DISPLAY
      ttblView.boardDate LABEL-FONT 6
  ttblView.dayOfWeek LABEL-FONT 6 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     boardDateStart AT ROW 1.05 COL 30 COLON-ALIGNED HELP
          "Enter Start Date Range"
     btnCalendar-1 AT ROW 1.05 COL 48 HELP
          "Click to Access Popup Calendar"
     boardDateEnd AT ROW 1.05 COL 57 COLON-ALIGNED HELP
          "Enter End Date Range"
     btnCalendar-2 AT ROW 1.05 COL 75 HELP
          "Click to Access Popup Calendar"
     btnRefresh AT ROW 1.05 COL 81 HELP
          "Click to Refresh View"
     btnExit AT ROW 1.1 COL 109 HELP
          "Click to Exit"
     includeDowntime AT ROW 1.24 COL 87
     BROWSE-1 AT ROW 2.19 COL 1 HELP
          "Select Column and Row"
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
         TITLE              = "Capacity View - Scheduler"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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
/* BROWSE-TAB BROWSE-1 includeDowntime fMain */
ASSIGN 
       BROWSE-1:NUM-LOCKED-COLUMNS IN FRAME fMain     = 2
       BROWSE-1:ALLOW-COLUMN-SEARCHING IN FRAME fMain = TRUE
       BROWSE-1:SEPARATOR-FGCOLOR IN FRAME fMain      = 0.

/* SETTINGS FOR BUTTON btnRefresh IN FRAME fMain
   1 2                                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblView.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Capacity View - Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Capacity View - Scheduler */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-RESIZED OF wWin /* Capacity View - Scheduler */
DO:
  RUN winReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME boardDateEnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDateEnd wWin
ON HELP OF boardDateEnd IN FRAME fMain /* End */
DO:
  {{&includes}/calendar.i}
  APPLY 'LEAVE' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDateEnd wWin
ON LEAVE OF boardDateEnd IN FRAME fMain /* End */
DO:
  ASSIGN {&SELF-NAME}.
  IF boardDateStart EQ ? OR boardDateStart GT boardDateEnd THEN
  ASSIGN
    boardDateStart = boardDateEnd
    boardDateStart:SCREEN-VALUE = STRING(boardDateStart).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDateEnd wWin
ON RETURN OF boardDateEnd IN FRAME fMain /* End */
DO:
  APPLY 'LEAVE' TO SELF.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME boardDateStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDateStart wWin
ON HELP OF boardDateStart IN FRAME fMain /* Date Range - Start */
DO:
  {{&includes}/calendar.i}
  APPLY 'LEAVE' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDateStart wWin
ON LEAVE OF boardDateStart IN FRAME fMain /* Date Range - Start */
DO:
  ASSIGN {&SELF-NAME}.
  IF boardDateStart LT TODAY THEN
  ASSIGN
    boardDateStart:SCREEN-VALUE = STRING(TODAY)
    boardDateStart.
  IF boardDateEnd EQ ? OR boardDateEnd LT boardDateStart THEN
  ASSIGN
    boardDateEnd = boardDateStart
    boardDateEnd:SCREEN-VALUE = STRING(boardDateEnd).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDateStart wWin
ON RETURN OF boardDateStart IN FRAME fMain /* Date Range - Start */
DO:
  APPLY 'LEAVE' TO SELF.
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 wWin
ON ROW-DISPLAY OF BROWSE-1 IN FRAME fMain
DO:
  DO i = 2 TO col#:
    IF NOT VALID-HANDLE(cells[i]) THEN NEXT.
    IF i EQ 2 THEN cells[i]:BGCOLOR = 14.
    ELSE
    IF i MOD 2 EQ 0 THEN
    ASSIGN
      cells[i]:BGCOLOR = 10
      cells[i]:FGCOLOR = 0
      cells[i]:FONT = 6.
    ELSE
    ASSIGN
      cells[i]:FORMAT = 'X(9)'
      cells[i]:BGCOLOR = IF LENGTH(ttblView.timeUsed[INTEGER(i / 2 - 1)]) GE 8 THEN 1 ELSE 7
      cells[i]:FGCOLOR = 15
      cells[i]:FONT = 2.
    IF i GT 2 AND i MOD 2 EQ 0 THEN
    DO:
      IF ttblView.timeAvail[INTEGER(i / 2 - 1)] EQ '00:00' THEN
      ASSIGN
        cells[i]:BGCOLOR = 0
        cells[i]:FGCOLOR = 15
        cells[i]:FONT = 2.
      ELSE
      IF ttblView.timeAvail[INTEGER(i / 2 - 1)] EQ 'Pending' THEN
      ASSIGN
        cells[i]:BGCOLOR = 4
        cells[i]:FGCOLOR = 15
        cells[i]:FONT = 2.
      ELSE
      IF ttblView.timeAvail[INTEGER(i / 2 - 1)] EQ '24:00' THEN
      ASSIGN
        cells[i]:BGCOLOR = 2
        cells[i]:FGCOLOR = 15
        cells[i]:FONT = 2.
    END.
  END. /* do i */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 wWin
ON CHOOSE OF btnCalendar-1 IN FRAME fMain
DO:
  APPLY 'HELP' TO boardDateStart.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 wWin
ON CHOOSE OF btnCalendar-2 IN FRAME fMain
DO:
  APPLY 'HELP' TO boardDateEnd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit wWin
ON CHOOSE OF btnExit IN FRAME fMain /* Exit */
DO:
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh wWin
ON CHOOSE OF btnRefresh IN FRAME fMain /* Refresh */
DO:
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME includeDowntime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL includeDowntime wWin
ON VALUE-CHANGED OF includeDowntime IN FRAME fMain /* Include Downtime */
DO:
  ASSIGN {&SELF-NAME}.
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
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  i = 0.
  FOR EACH resources NO-LOCK WITH FRAME {&FRAME-NAME}:
    ASSIGN
      i = i + 1
      pHandle = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblView.timeUsed[' + STRING(i) + ']')
      pHandle:LABEL = resources.resource + '!Used'
      pHandle:WIDTH-CHARS = 12
      pHandle:LABEL-FONT = 6
      pHandle:LABEL-BGCOLOR = 8
      pHandle:LABEL-FGCOLOR = 0
      pHandle = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblView.timeAvail[' + STRING(i) + ']')
      pHandle:LABEL = resources.resource + '!Available'
      pHandle:WIDTH-CHARS = 12
      pHandle:LABEL-FONT = 6
      pHandle:LABEL-BGCOLOR = 2
      pHandle:LABEL-FGCOLOR = 15.
  END. /* each ttblresource */
  DO col# = 1 TO {&BROWSE-NAME}:NUM-COLUMNS:
    cells[col#] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(col#).
  END.
  col# = {&BROWSE-NAME}:NUM-COLUMNS.
  SESSION:SET-WAIT-STATE('').

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
  DISPLAY boardDateStart boardDateEnd includeDowntime 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE boardDateStart btnCalendar-1 boardDateEnd btnCalendar-2 btnRefresh 
         btnExit includeDowntime BROWSE-1 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadView wWin 
PROCEDURE loadView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE availTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE d AS DATE NO-UNDO.
  DEFINE VARIABLE days AS CHARACTER NO-UNDO INIT 'Sun,Mon,Tue,Wed,Thu,Fri,Sat'.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE t AS LOGICAL NO-UNDO EXTENT 1440.
  DEFINE VARIABLE usedTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvPending AS INTEGER NO-UNDO.

  DEFINE BUFFER ttblViewBuff FOR ttblView.
  
  SESSION:SET-WAIT-STATE('General').
  EMPTY TEMP-TABLE ttblView.
  EMPTY TEMP-TABLE resources.
  CREATE ttblViewBuff.
  ASSIGN
    ttblViewBuff.boardDate = 1.1.1950
    ttblViewBuff.dayOfWeek = ''
    ttblViewBuff.timeUsed = '00:00'
    ttblViewBuff.timeAvail = 'Pending'.
  FOR EACH ttblResource EXCLUSIVE-LOCK BY ttblResource.order:
    CREATE resources.
    ASSIGN
      i = i + 1
      resources.order = i
      resources.resource = ttblResource.resource
      lvPending = 0.
    FOR EACH pendingJob NO-LOCK
        WHERE pendingJob.resource EQ ttblResource.resource:
      lvPending = lvPending + pendingJob.timeSpan.
    END. /* each pendingjob */
    ASSIGN
      ttblViewBuff.timeUsed[i] = specialTime(lvPending)
      ttblViewBuff.timeUsed[i] = SUBSTR(ttblViewBuff.timeUsed[i],1,R-INDEX(ttblViewBuff.timeUsed[i],':') - 1).
    DO d = boardDateStart TO boardDateEnd:
      ASSIGN
        lvStartDateTime = numericDateTime(d,0)
        lvEndDateTime = numericDateTime(d,86400).
      FIND ttblView EXCLUSIVE-LOCK WHERE ttblView.boardDate EQ d NO-ERROR.
      IF NOT AVAILABLE ttblView THEN
      DO:
        CREATE ttblView.
        ASSIGN
          ttblView.boardDate = d
          ttblView.dayOfWeek = ENTRY(WEEKDAY(d),days).
      END. /* if not avail */
      t = NO.
      IF d EQ TODAY THEN
      DO:
        ASSIGN
          lvStartTime = 1
          lvEndTime = INTEGER(TIME / 60).
        DO j = lvStartTime TO lvEndTime:
          IF j LE EXTENT(t) THEN t[j] = YES.
        END. /* do j */
      END. /* check if today */
      IF includeDowntime THEN
      FOR EACH boardDowntime NO-LOCK
          WHERE boardDowntime.resource EQ resources.resource
            AND boardDowntime.startDate EQ d:
        ASSIGN
          lvStartTime = INTEGER(boardDowntime.startTime / 60)
          lvEndTime = INTEGER(boardDowntime.endTime / 60).
        IF lvStartTime EQ 0 THEN lvStartTime = 1.
        DO j = lvStartTime TO lvEndTime:
          IF j LE EXTENT(t) THEN t[j] = YES.
        END. /* do j */
      END. /* each boarddowntime */
      usedTime = 0.
      DO j = 1 TO EXTENT(t):
        IF t[j] THEN usedTime = usedTime + 1.
      END. /* do j */
      IF usedTime NE 1440 THEN
      FOR EACH ttblJob NO-LOCK WHERE ttblJob.resource EQ resources.resource
                                 AND ttblJob.startDateTime LE lvEndDateTime
                                 AND ttblJob.endDateTime GE lvStartDateTime:
        ASSIGN
          lvStartTime = IF ttblJob.startDateTime LT lvStartDateTime THEN 0
                        ELSE INTEGER(ttblJob.startTime / 60)
          lvEndTime = IF ttblJob.endDateTime GT lvEndDateTime THEN 1440
                      ELSE INTEGER(ttblJob.endTime / 60).
        IF lvStartTime EQ 0 THEN lvStartTime = 1.
        DO j = lvStartTime TO lvEndTime:
          IF j LE EXTENT(t) THEN t[j] = YES.
        END. /* do j */
      END. /* each ttbljob */
      usedTime = 0.
      DO j = 1 TO EXTENT(t):
        IF t[j] THEN usedTime = usedTime + 1.
      END. /* do j */
      ASSIGN
        usedTime = usedTime * 60
        availTime = 86400 - usedTime
        ttblView.timeUsed[i] = STRING(usedTime,'HH:MM')
        ttblView.timeAvail[i] = STRING(availTime,'HH:MM').
      IF ttblView.timeUsed[i] EQ '00:00' AND usedTime GT 0 THEN
      ttblView.timeUsed[i] = '24:00'.
      IF ttblView.timeAvail[i] EQ '00:00' AND availTime GT 0 THEN
      ttblView.timeAvail[i] = '24:00'.
    END. /* do d */
  END. /* each ttblresource */
  SESSION:SET-WAIT-STATE('').
  APPLY 'ENTRY':U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

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
  ASSIGN
    {&WINDOW-NAME}:WINDOW-STATE = 1
    boardDateStart = TODAY
    boardDateEnd = TODAY + 30.
  RUN loadView.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN winReSize.
  RUN buildCells.
  IF AVAILABLE ttblView THEN
  BROWSE {&BROWSE-NAME}:REFRESH().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowse wWin 
PROCEDURE reopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN loadView.
  {&OPEN-QUERY-{&BROWSE-NAME}}

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
    BROWSE {&BROWSE-NAME}:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS
    BROWSE {&BROWSE-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 25.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION numericDateTime wWin 
FUNCTION numericDateTime RETURNS DECIMAL
  (ipDate AS DATE,ipTime AS INTEGER) :
  {{&includes}/numericDateTime.i}

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

