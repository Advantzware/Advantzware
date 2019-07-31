&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api/APIInboundEventsViewer.w

  Description: Display all the API Inbound events

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Vishnu Vellanki

  Created: 06th June 2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the wgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE lReTrigger    AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdOutputProcs AS HANDLE  NO-UNDO.

DEFINE TEMP-TABLE ttAPIInboundEvent NO-UNDO
    FIELDS retryEvent AS LOGICAL
    FIELDS apiRoute AS CHARACTER
    FIELDS requestedBy AS CHARACTER
    FIELDS requestDateTime AS DATETIME
    FIELDS success AS LOGICAL
    FIELDS eventID AS INTEGER
    FIELDS eventRowID AS ROWID
    .

DEFINE TEMP-TABLE ttPrintAPIInboundEvent NO-UNDO
    FIELDS apiRoute AS CHARACTER LABEL "API Route"
    FIELDS requestedBy AS CHARACTER LABEL "Requested By"
    FIELDS requestDateTime AS DATETIME LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS.SSS"
    FIELDS success AS LOGICAL LABEL "Success?" FORMAT "SUCCESS/FAILURE"
    FIELDS eventID AS INTEGER LABEL "Event ID"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAPIInboundEvent

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttAPIInboundEvent.retryEvent ttAPIInboundEvent.apiRoute ttAPIInboundEvent.requestDateTime ttAPIInboundEvent.success ttAPIInboundEvent.requestedby ttAPIInboundEvent.eventID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttAPIInboundEvent.retryEvent   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttAPIInboundEvent
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttAPIInboundEvent
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttAPIInboundEvent
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttAPIInboundEvent.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttAPIInboundEvent
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttAPIInboundEvent


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 btExport btExit btFilter fieventID ~
cbSuccess fiAPIId btAPIIDLookup btRestart btBeginRequestDateCal ~
fiBeginRequestDate btEndRequestDateCal fiEndRequestDate BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fieventID fieventIDlb cbSuccess fiAPIId ~
fiSuccessLabel fiAPIIdLabel fiBeginRequestDate fiEndRequestDate ~
fiBeginRequestDatelabel fiendRequestDatelabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAPIIDLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btBeginRequestDateCal 
     IMAGE-UP FILE "Graphics\16x16\calendar.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.24.

DEFINE BUTTON btEndRequestDateCal 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.24.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U
     LABEL "Exit" 
     SIZE 11 BY 2.62.

DEFINE BUTTON btExport 
     IMAGE-UP FILE "Graphics/32x32/file_excel.ico":U
     LABEL "Export" 
     SIZE 11 BY 2.62 TOOLTIP "Export Data".

DEFINE BUTTON btFilter 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Filter" 
     SIZE 9 BY 2.14.

DEFINE BUTTON btRestart 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U
     LABEL "Restart" 
     SIZE 9 BY 2.14 TOOLTIP "Retry Event(s)".

DEFINE VARIABLE cbSuccess AS CHARACTER FORMAT "X(256)":U INITIAL "ALL" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ALL","FAILED","SUCCESS" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiAPIId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.8 BY 1.14
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiAPIIdLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API Route:" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.14
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Begin Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiEndRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.14
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiendRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fieventID AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1.14
     FONT 35 NO-UNDO.

DEFINE VARIABLE fieventIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "Event ID:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 35 NO-UNDO.

DEFINE VARIABLE fiSuccessLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Success:" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FONT 35 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 119.4 BY 5.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttAPIInboundEvent SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttAPIInboundEvent.retryEvent COLUMN-LABEL "[ ] All" 
            WIDTH 8 VIEW-AS TOGGLE-BOX
      ttAPIInboundEvent.apiRoute COLUMN-LABEL "API Route" FORMAT "x(30)":U
            WIDTH 40
      ttAPIInboundEvent.requestDateTime COLUMN-LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 27
      ttAPIInboundEvent.success COLUMN-LABEL "Success" FORMAT "SUCCESS/FAILED":U
            WIDTH 27
      ttAPIInboundEvent.requestedby COLUMN-LABEL "Requested By" FORMAT "x(8)":U
            WIDTH 27
      ttAPIInboundEvent.eventID COLUMN-LABEL "Event ID" FORMAT "->,>>>,>>9":U
            WIDTH 27
      ENABLE ttAPIInboundEvent.retryEvent
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 157.6 BY 20.52
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExport AT ROW 1.57 COL 129.4 WIDGET-ID 50
     btExit AT ROW 1.57 COL 153.8 WIDGET-ID 2
     btFilter AT ROW 2.1 COL 116.2 WIDGET-ID 18
     fieventID AT ROW 2.24 COL 21.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     fieventIDlb AT ROW 2.29 COL 6.8 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     cbSuccess AT ROW 3.91 COL 93.6 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fiAPIId AT ROW 3.95 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     btAPIIDLookup AT ROW 3.95 COL 74 WIDGET-ID 48
     fiSuccessLabel AT ROW 3.95 COL 80.2 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fiAPIIdLabel AT ROW 4 COL 8.8 NO-LABEL WIDGET-ID 6
     btRestart AT ROW 4.86 COL 116.2 WIDGET-ID 26
     btBeginRequestDateCal AT ROW 5.62 COL 52.6 WIDGET-ID 44
     fiBeginRequestDate AT ROW 5.67 COL 35.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btEndRequestDateCal AT ROW 5.67 COL 104 WIDGET-ID 46
     fiEndRequestDate AT ROW 5.71 COL 86.6 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     fiBeginRequestDatelabel AT ROW 5.76 COL 7 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiendRequestDatelabel AT ROW 5.76 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     BROWSE-2 AT ROW 8.14 COL 7.4 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 14 WIDGET-ID 42
          FONT 35
     RECT-26 AT ROW 1.57 COL 7.6 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170 BY 28.57
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
         TITLE              = "API Inbound Events"
         HEIGHT             = 28.57
         WIDTH              = 170
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 fiendRequestDatelabel DEFAULT-FRAME */
ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN fiAPIIdLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiBeginRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiendRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fieventIDlb IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSuccessLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAPIInboundEvent
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Inbound Events */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Inbound Events */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttAPIInboundEvent THEN DO:
        RUN api\ResponseInboundDataViewer.w (
            ttAPIInboundEvent.eventRowID
            ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON START-SEARCH OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    IF SELF:CURRENT-COLUMN:NAME EQ "retryEvent" THEN DO:
        lReTrigger = NOT lReTrigger.
        
        FOR EACH ttAPIInboundEvent:
            ttAPIInboundEvent.retryEvent = lReTrigger.
        END.
        
        SELF:CURRENT-COLUMN:LABEL = IF lReTrigger THEN
                                        "[*] All"
                                    ELSE
                                        "[ ] All".
                                        
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}    
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAPIIDLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAPIIDLookup C-Win
ON CHOOSE OF btAPIIDLookup IN FRAME DEFAULT-FRAME
DO:
    APPLY "HELP" TO fiAPIId.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBeginRequestDateCal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBeginRequestDateCal C-Win
ON CHOOSE OF btBeginRequestDateCal IN FRAME DEFAULT-FRAME
DO: 
    DEFINE VARIABLE cCalendarDate AS CHARACTER NO-UNDO.

    RUN pChooseDate (
        OUTPUT cCalendarDate
        ).
 
    IF cCalendarDate NE '' THEN
        fiBeginRequestDate:SCREEN-VALUE = cCalendarDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEndRequestDateCal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEndRequestDateCal C-Win
ON CHOOSE OF btEndRequestDateCal IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cCalendarDate AS CHARACTER NO-UNDO.

    RUN pChooseDate (
        OUTPUT cCalendarDate
        ).
 
    IF cCalendarDate NE '' THEN
        fiEndRequestDate:SCREEN-VALUE = cCalendarDate.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Exit */
DO:
    IF VALID-HANDLE(hdOutputProcs) THEN
        DELETE PROCEDURE hdOutputProcs.
        
    APPLY "WINDOW-CLOSE" TO CURRENT-WINDOW.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExport C-Win
ON CHOOSE OF btExport IN FRAME DEFAULT-FRAME /* Export */
DO:
    DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFilePath     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSysCtrlName  AS CHARACTER NO-UNDO INITIAL "APIExport".
    
    RUN sys/ref/nk1look.p (
        g_company,            /* Company Code */
        cSysCtrlName,         /* sys-ctrl name */
        "C",                  /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        FALSE,                /* Use ship-to */
        FALSE,                /* ship-to vendor */
        "",                   /* ship-to vendor value */
        "",                   /* shi-id value */
        OUTPUT cFilePath,
        OUTPUT lRecFound
        ).
    
    IF NOT lRecFound OR cFilePath EQ "" THEN
        cFilePath = "C:\BA\Label\".
    
    cFullFilePath = IF fiAPIID:SCREEN-VALUE NE "" THEN
                        REPLACE(fiAPIID:SCREEN-VALUE,"/","")
                    ELSE
                        "ALL".
    
    cFullFilePath = cFilePath
                  + cFullFilePath
                  + STRING(YEAR(TODAY)) 
                  + STRING(MONTH(TODAY),"99") 
                  + STRING(DAY(TODAY),"99") 
                  + "_"
                  + REPLACE(STRING(TIME,"HH:MM:SS"),":","")
                  + ".csv".
                  
    EMPTY TEMP-TABLE ttPrintAPIInboundEvent.
                                                                
    FOR EACH ttAPIInboundEvent:
        CREATE ttPrintAPIInboundEvent.
        BUFFER-COPY ttAPIInboundEvent TO ttPrintAPIInboundEvent.
    END.
 
    RUN TempTableToCSV IN hdOutputProcs (
        INPUT TEMP-TABLE ttPrintAPIInboundEvent:HANDLE,
        INPUT cFullFilePath,
        INPUT TRUE /* Export Header */
        ).
                
    MESSAGE "Export complete. File saved to " cFullFilePath SKIP
        "Open file?"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO-CANCEL UPDATE lOpen AS LOGICAL.
    
    IF lOpen THEN
        OS-COMMAND SILENT VALUE(cFullFilePath).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFilter C-Win
ON CHOOSE OF btFilter IN FRAME DEFAULT-FRAME /* Filter */
DO:
    IF fiBeginRequestDate:SCREEN-VALUE NE ?  AND
       fiBeginRequestDate:SCREEN-VALUE NE "" AND
       fiBeginRequestDate:SCREEN-VALUE NE "/  /" AND
       fiEndRequestDate:SCREEN-VALUE NE ?  AND
       fiEndRequestDate:SCREEN-VALUE NE "" AND
       fiEndRequestDate:SCREEN-VALUE NE "/  /" AND
       DATE(fiEndRequestDate:SCREEN-VALUE) LT DATE(fiBeginRequestDate:SCREEN-VALUE) THEN DO:
        MESSAGE "Begin Request Date cannot be greater than End Request Date" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.       
    
    EMPTY TEMP-TABLE ttAPIInboundEvent.
    FOR EACH APIInboundEvent NO-LOCK
        WHERE (IF fieventID:SCREEN-VALUE EQ "0" THEN
                   TRUE
               ELSE
                  APIInboundEvent.eventID EQ INTEGER(fieventID:SCREEN-VALUE))
          AND (IF fiAPIID:SCREEN-VALUE EQ "ALL" OR fiAPIID:SCREEN-VALUE EQ "" THEN
                   TRUE
               ELSE
                   APIInboundEvent.apiRoute EQ fiAPIID:SCREEN-VALUE)
          AND (IF fiBeginRequestDate:SCREEN-VALUE EQ ?  OR
                  fiBeginRequestDate:SCREEN-VALUE EQ "" OR
                  fiBeginRequestDate:SCREEN-VALUE EQ "/  /" THEN
                  TRUE
               ELSE
                  DATE(APIInboundEvent.requestDateTime) GE DATE(fiBeginRequestDate:SCREEN-VALUE))
          AND (IF fiEndRequestDate:SCREEN-VALUE EQ ?  OR
                  fiEndRequestDate:SCREEN-VALUE EQ "" OR
                  fiEndRequestDate:SCREEN-VALUE EQ "/  /" THEN
                  TRUE
               ELSE
                  DATE(APIInboundEvent.requestDateTime) LE DATE(fiEndRequestDate:SCREEN-VALUE))
          AND (IF cbSuccess:SCREEN-VALUE = "ALL" THEN
                   TRUE
               ELSE IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN
                   APIInboundEvent.success = TRUE
               ELSE
                   APIInboundEvent.success = FALSE)
        BY APIInboundEvent.requestDateTime DESCENDING:
        CREATE ttAPIInboundEvent.
        BUFFER-COPY APIInboundEvent TO ttAPIInboundEvent.
        ttAPIInboundEvent.eventRowID = ROWID(APIInboundEvent).        
    END.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRestart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRestart C-Win
ON CHOOSE OF btRestart IN FRAME DEFAULT-FRAME /* Restart */
DO: 
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAPIId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAPIId C-Win
ON HELP OF fiAPIId IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        g_company, 
        "apiRoute", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 
    
    IF lookupField NE "" THEN DO:
        fiAPIId:SCREEN-VALUE = lookupField.
        
        APPLY "LEAVE" TO SELF.
    END.  
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

    /* Fetch current day's records to temp-table when launched */
    ASSIGN
        fiAPIID:SCREEN-VALUE            = "All"
        fiBeginRequestDate:SCREEN-VALUE = STRING(TODAY)
        fiEndRequestDate:SCREEN-VALUE   = STRING(TODAY)
        .
    APPLY "CHOOSE" TO btFilter.
    
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    
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
  DISPLAY fieventID fieventIDlb cbSuccess fiAPIId fiSuccessLabel fiAPIIdLabel 
          fiBeginRequestDate fiEndRequestDate fiBeginRequestDatelabel 
          fiendRequestDatelabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-26 btExport btExit btFilter fieventID cbSuccess fiAPIId 
         btAPIIDLookup btRestart btBeginRequestDateCal fiBeginRequestDate 
         btEndRequestDateCal fiEndRequestDate BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChooseDate C-Win 
PROCEDURE pChooseDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcCalendarDate AS CHARACTER NO-UNDO.

    RUN nosweat/popupcal2.w (OUTPUT opcCalendarDate).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

