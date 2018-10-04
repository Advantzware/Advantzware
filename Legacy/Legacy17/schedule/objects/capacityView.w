&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
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
     that this procedure"s triggers and internal procedures 
     will execute in this procedure"s storage, and that proper
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

DEFINE VARIABLE boardHandle AS HANDLE    NO-UNDO.
DEFINE VARIABLE boardType   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cells       AS HANDLE    NO-UNDO EXTENT 600.
DEFINE VARIABLE col#        AS INTEGER   NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE resources NO-UNDO
  FIELD order    AS INTEGER
  FIELD resource AS CHARACTER
  FIELD kicks    AS INTEGER
    INDEX resources IS PRIMARY UNIQUE order resource
    .

DEFINE TEMP-TABLE ttblView NO-UNDO
  FIELD boardDate  AS DATE      FORMAT "99.99.9999" COLUMN-LABEL "Resource!Date"
  FIELD dayOfWeek  AS CHARACTER FORMAT "x(5)"       COLUMN-LABEL "Day"
  FIELD colJobs    AS INTEGER   FORMAT ">>,>>9 " EXTENT 200
  FIELD colUsed    AS CHARACTER FORMAT "x(14)"   EXTENT 200
  FIELD colAvail   AS CHARACTER FORMAT "x(14)"   EXTENT 200
  FIELD timeUsed   AS CHARACTER FORMAT "x(14)"   EXTENT 200
  FIELD timeAvail  AS CHARACTER FORMAT "x(14)"   EXTENT 200
  FIELD kicksUsed  AS CHARACTER FORMAT "x(14)"   EXTENT 200
  FIELD kicksAvail AS CHARACTER FORMAT "x(14)"   EXTENT 200
    INDEX ttblView IS PRIMARY UNIQUE boardDate
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME viewBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttblView

/* Definitions for BROWSE viewBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-viewBrowse ttblView.boardDate ttblView.dayOfWeek   
&Scoped-define ENABLED-FIELDS-IN-QUERY-viewBrowse   
&Scoped-define SELF-NAME viewBrowse
&Scoped-define QUERY-STRING-viewBrowse FOR EACH ttblView
&Scoped-define OPEN-QUERY-viewBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttblView.
&Scoped-define TABLES-IN-QUERY-viewBrowse ttblView
&Scoped-define FIRST-TABLE-IN-QUERY-viewBrowse ttblView


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-viewBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS boardDateStart btnCalendar-1 boardDateEnd ~
btnCalendar-2 btnRefresh btnExit includeDowntime viewType viewBrowse 
&Scoped-Define DISPLAYED-OBJECTS boardDateStart boardDateEnd ~
includeDowntime viewType 

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
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
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

DEFINE VARIABLE viewType AS CHARACTER INITIAL "Time" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Time", "Time",
"Kicks", "Kicks"
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE includeDowntime AS LOGICAL INITIAL yes 
     LABEL "&Include Downtime" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY viewBrowse FOR 
      ttblView SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE viewBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS viewBrowse wWin _FREEFORM
  QUERY viewBrowse DISPLAY
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
     btnExit AT ROW 1.1 COL 155 HELP
          "Click to Exit"
     includeDowntime AT ROW 1.24 COL 93
     viewType AT ROW 1.24 COL 122 NO-LABEL WIDGET-ID 2
     viewBrowse AT ROW 2.19 COL 1 HELP
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB viewBrowse viewType fMain */
/* SETTINGS FOR BUTTON btnRefresh IN FRAME fMain
   1 2                                                                  */
ASSIGN 
       viewBrowse:NUM-LOCKED-COLUMNS IN FRAME fMain     = 2
       viewBrowse:SEPARATOR-FGCOLOR IN FRAME fMain      = 0.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE viewBrowse
/* Query rebuild information for BROWSE viewBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblView.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE viewBrowse */
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
  RUN reopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME viewBrowse
&Scoped-define SELF-NAME viewBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL viewBrowse wWin
ON ROW-DISPLAY OF viewBrowse IN FRAME fMain
DO:
  DO idx = 2 TO col#:
    IF NOT VALID-HANDLE(cells[idx]) THEN NEXT.
    jdx = INTEGER(idx / 3 - 1).
    IF idx EQ 2 THEN cells[idx]:BGCOLOR = 14. /* yellow */
    ELSE
    IF idx MOD 3 EQ 0 THEN
    ASSIGN /* light gray/light gray */
      cells[idx]:FGCOLOR = 8
      cells[idx]:BGCOLOR = 8
      cells[idx]:FONT    = 2
      .
    ELSE
    IF idx MOD 3 EQ 2 THEN
    ASSIGN /* black/light green/bold */
      cells[idx]:FGCOLOR = 0
      cells[idx]:BGCOLOR = 10
      cells[idx]:FONT    = 6
      .
    ELSE
    ASSIGN /* white/dark blue or gray */
      cells[idx]:FGCOLOR = 15
      cells[idx]:BGCOLOR = IF LENGTH(ttblView.timeUsed[jdx + 1]) GE 8 THEN 1 ELSE 7
      cells[idx]:FONT    = 2
      .
    IF idx GT 2 THEN DO:
      IF idx MOD 3 EQ 0 THEN DO:
        IF ttblView.colJobs[jdx + 1] GT 0 THEN
        ASSIGN /* black/light gray/bold */
          cells[idx]:FGCOLOR = 0
          cells[idx]:BGCOLOR = 8
          cells[idx]:FONT    = 6
          .
      END.
      ELSE
      IF idx MOD 3 EQ 1 THEN DO:
        IF viewType EQ "Time" THEN DO:
          IF ttblView.colUsed[jdx + 1] EQ "00:00" THEN
          ASSIGN /* gray/gray */
            cells[idx]:FGCOLOR = 7
            cells[idx]:BGCOLOR = 7
            cells[idx]:FONT    = 2
            .
        END.
        ELSE DO:
          IF INTEGER(ttblView.colUsed[jdx + 1]) EQ 0 THEN
          ASSIGN /* gray/gray */
            cells[idx]:FGCOLOR = 7
            cells[idx]:BGCOLOR = 7
            cells[idx]:FONT    = 2
            .
        END.
      END.
      ELSE
      IF idx MOD 3 EQ 2 THEN DO:
        IF viewType EQ "Time" THEN DO:
          IF ttblView.colAvail[jdx] EQ "Pending" THEN
          ASSIGN /* white/maroon */
            cells[idx]:FGCOLOR = 15
            cells[idx]:BGCOLOR = 4
            cells[idx]:FONT    = 2
            .
          ELSE
          IF ttblView.colAvail[jdx] EQ "00:00" THEN
          ASSIGN /* white/black */
            cells[idx]:FGCOLOR = 15
            cells[idx]:BGCOLOR = 0
            cells[idx]:FONT    = 2
            .
          ELSE
          IF ttblView.colAvail[jdx] EQ "24:00" THEN
          ASSIGN /* white/dark green */
            cells[idx]:FGCOLOR = 15
            cells[idx]:BGCOLOR = 2
            cells[idx]:FONT    = 2
            .
        END. /* if time */
        ELSE DO: /* kicks */
          IF INDEX(ttblView.colAvail[jdx],"/Hr") NE 0 THEN
          ASSIGN /* white/maroon */
            cells[idx]:FGCOLOR = 15
            cells[idx]:BGCOLOR = 4
            cells[idx]:FONT    = 2
            .
          ELSE
          IF INTEGER(ttblView.colAvail[jdx]) EQ 0 THEN
          ASSIGN /* white/black */
            cells[idx]:FGCOLOR = 15
            cells[idx]:BGCOLOR = 0
            cells[idx]:FONT    = 2
            .
          ELSE DO:
            FIND FIRST resources
                 WHERE resources.order EQ jdx
                 NO-ERROR.
            IF AVAILABLE resources AND
               INTEGER(ttblView.colAvail[jdx]) EQ resources.kicks * 24 THEN
            ASSIGN /* white/dark green */
              cells[idx]:FGCOLOR = 15
              cells[idx]:BGCOLOR = 2
              cells[idx]:FONT    = 2
              .
          END.
        END. /* else kicks */
      END. /* if idx mod 3 eq 2 */
    END. /* idx gt 2 */
  END. /* do i */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME viewType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL viewType wWin
ON VALUE-CHANGED OF viewType IN FRAME fMain
DO:
  ASSIGN {&SELF-NAME}.
  RUN reopenBrowse.
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
  DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
  DEFINE VARIABLE pHandle AS HANDLE  NO-UNDO.

  SESSION:SET-WAIT-STATE('General').
  idx = 0.
  FOR EACH resources NO-LOCK WITH FRAME {&FRAME-NAME}:
    ASSIGN
      idx                   = idx + 1
      pHandle               = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblView.colJobs[' + STRING(idx) + ']')
      pHandle:LABEL         = 'No of !Jobs '
      pHandle:WIDTH-CHARS   = 7
      pHandle:LABEL-FONT    = ?
      pHandle:LABEL-BGCOLOR = 1
      pHandle:LABEL-FGCOLOR = 15
      pHandle               = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblView.colUsed[' + STRING(idx) + ']')
      pHandle:LABEL         = resources.resource + '!Booked'
      pHandle:WIDTH-CHARS   = 14
      pHandle:LABEL-FONT    = 6
      pHandle:LABEL-BGCOLOR = 8
      pHandle:LABEL-FGCOLOR = 0
      pHandle               = {&BROWSE-NAME}:ADD-LIKE-COLUMN('ttblView.colAvail[' + STRING(idx) + ']')
      pHandle:LABEL         = resources.resource + '!Available'
      pHandle:WIDTH-CHARS   = 14
      pHandle:LABEL-FONT    = 6
      pHandle:LABEL-BGCOLOR = 2
      pHandle:LABEL-FGCOLOR = 15
      .
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
  DISPLAY boardDateStart boardDateEnd includeDowntime viewType 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE boardDateStart btnCalendar-1 boardDateEnd btnCalendar-2 btnRefresh 
         btnExit includeDowntime viewType viewBrowse 
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
  DEFINE VARIABLE cDays          AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
  DEFINE VARIABLE lUsed          AS LOGICAL   NO-UNDO EXTENT 1440.
  DEFINE VARIABLE iUsedKicks     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iAvailKicks    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iUsedTime      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iAvailTime     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iStartTime     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iEndTime       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dStartDateTime AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE dEndDateTime   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE iJobs          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iPending       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE jdx            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dDate          AS DATE      NO-UNDO.

  DEFINE BUFFER ttblViewBuff FOR ttblView.

  SESSION:SET-WAIT-STATE("General").
  EMPTY TEMP-TABLE ttblView.
  EMPTY TEMP-TABLE resources.
  CREATE ttblViewBuff.
  ASSIGN
    ttblViewBuff.boardDate  = 1.1.1950
    ttblViewBuff.dayOfWeek  = ""
    ttblViewBuff.timeUsed   = "00:00"
    ttblViewBuff.timeAvail  = "Pending"
    ttblViewBuff.kicksUsed  = STRING(0,">>,>>>,>>9")
    ttblViewBuff.kicksAvail = "?/Hr"
    .
  FOR EACH ttblResource
      BY ttblResource.order
      :
    CREATE resources.
    ASSIGN
      idx                = idx + 1
      resources.order    = idx
      resources.resource = ttblResource.resource
      resources.kicks    = ttblResource.kicks
      iPending           = 0
      .
    FOR EACH pendingJob
        WHERE pendingJob.resource EQ ttblResource.resource
        :
      ASSIGN
        iJobs      = iJobs      + 1
        iPending   = iPending   + pendingJob.timeSpan
        iUsedKicks = iUsedKicks + INTEGER(pendingJob.userField88)
        .
    END. /* each pendingjob */
    ASSIGN
      ttblViewBuff.colJobs[idx]    = iJobs
      ttblViewBuff.timeUsed[idx]   = specialTime(iPending)
      ttblViewBuff.timeUsed[idx]   = SUBSTR(ttblViewBuff.timeUsed[idx],1,R-INDEX(ttblViewBuff.timeUsed[idx],":") - 1)
      ttblViewBuff.kicksUsed[idx]  = STRING(INTEGER(iUsedKicks),">>,>>>,>>9")
      ttblViewBuff.kicksAvail[idx] = STRING(ttblResource.kicks,">>>,>>9") + "/Hr"
      iJobs                        = 0
      iUsedKicks                   = 0
      .
    DO dDate = boardDateStart TO boardDateEnd:
      ASSIGN
        dStartDateTime = numericDateTime(dDate,0)
        dEndDateTime   = numericDateTime(dDate,86400)
        .
      FIND FIRST ttblView
           WHERE ttblView.boardDate EQ dDate
           NO-ERROR.
      IF NOT AVAILABLE ttblView THEN DO:
        CREATE ttblView.
        ASSIGN
          ttblView.boardDate = dDate
          ttblView.dayOfWeek = ENTRY(WEEKDAY(dDate),cDays)
          .
      END. /* if not avail */
      lUsed = NO.
      IF dDate EQ TODAY THEN DO:
        ASSIGN
          iStartTime = 1
          iEndTime   = INTEGER(TIME / 60)
          .
        DO jdx = iStartTime TO iEndTime:
          IF jdx LE EXTENT(lUsed) THEN lUsed[jdx] = YES.
        END. /* do jdx */
      END. /* check if today */
      IF includeDowntime THEN
      FOR EACH boardDowntime
          WHERE boardDowntime.resource  EQ resources.resource
            AND boardDowntime.startDate EQ dDate
          :
        ASSIGN
          iStartTime = INTEGER(boardDowntime.startTime / 60)
          iEndTime   = INTEGER(boardDowntime.endTime / 60)
          .
        IF iStartTime EQ 0 THEN iStartTime = 1.
        DO jdx = iStartTime TO iEndTime:
          IF jdx LE EXTENT(lUsed) THEN lUsed[jdx] = YES.
        END. /* do jdx */
      END. /* each boarddowntime */
      iUsedTime = 0.
      DO jdx = 1 TO EXTENT(lUsed):
        IF lUsed[jdx] THEN iUsedTime = iUsedTime + 1.
      END. /* do jdx */
      IF iUsedTime NE 1440 THEN
      FOR EACH ttblJob 
          WHERE ttblJob.resource      EQ resources.resource
            AND ttblJob.startDateTime LE dEndDateTime
            AND ttblJob.endDateTime   GE dStartDateTime
          :
        ASSIGN
          iStartTime = IF ttblJob.startDateTime LT dStartDateTime THEN 0
                       ELSE INTEGER(ttblJob.startTime / 60)
          iEndTime   = IF ttblJob.endDateTime   GT dEndDateTime   THEN 1440
                       ELSE INTEGER(ttblJob.endTime   / 60)
          iJobs      = iJobs + 1
          .
        IF iStartTime EQ 0 THEN iStartTime = 1.
        DO jdx = iStartTime TO iEndTime:
          IF jdx LE EXTENT(lUsed) THEN
          ASSIGN
            lUsed[jdx] = YES
            iUsedKicks = iUsedKicks + INTEGER(ttblJob.userField88) / 60
            .
        END. /* do jdx */
      END. /* each ttbljob */
      iUsedTime = 0.
      DO jdx = 1 TO EXTENT(lUsed):
        IF lUsed[jdx] THEN iUsedTime = iUsedTime + 1.
      END. /* do jdx */
      IF resources.kicks NE 0 THEN
      iAvailKicks = resources.kicks * (1440 - iUsedTime) / 60.
      ASSIGN
        iUsedTime                = iUsedTime * 60
        iAvailTime               = 86400 - iUsedTime
        ttblView.colJobs[idx]    = iJobs
        ttblView.timeUsed[idx]   = STRING(iUsedTime,"HH:MM")
        ttblView.timeAvail[idx]  = STRING(iAvailTime,"HH:MM")
        ttblView.kicksUsed[idx]  = STRING(INTEGER(iUsedKicks),">>,>>>,>>9")
        ttblView.kicksAvail[idx] = STRING(INTEGER(iAvailKicks),">>,>>>,>>9")
        iJobs                    = 0
        iUsedKicks               = 0
        iAvailKicks              = 0
        .
      IF ttblView.timeUsed[idx] EQ "00:00"  AND iUsedTime  GT 0 THEN
      ttblView.timeUsed[idx]  = "24:00".
      IF ttblView.timeAvail[idx] EQ "00:00" AND iAvailTime GT 0 THEN
      ttblView.timeAvail[idx] = "24:00".
    END. /* do ddate */
  END. /* each ttblresource */
  FOR EACH ttblView:
    DO idx = 1 TO EXTENT(ttblView.colUsed):
      IF viewType EQ "Time" THEN
      ASSIGN
        ttblView.colUsed  = ttblView.timeUsed
        ttblView.colAvail = ttblView.timeAvail
        .
      ELSE
      ASSIGN
        ttblView.colUsed  = ttblView.kicksUsed
        ttblView.colAvail = ttblView.kicksAvail
        .
    END.
  END. /* each ttblview */
  SESSION:SET-WAIT-STATE("").
  APPLY "ENTRY":U TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

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
    boardDateEnd = TODAY + 30
    .
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
    BROWSE {&BROWSE-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 25
    .

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

