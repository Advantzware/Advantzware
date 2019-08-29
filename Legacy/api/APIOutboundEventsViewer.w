&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: api/APIOutboundEventsViewer.w

  Description: Display all the API Outbound events

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Vishnu Vellanki

  Created: 06th June 2019

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE lReTrigger    AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE hdOutputProcs AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttAPIOutboundEvent NO-UNDO
    FIELDS retryEvent AS LOGICAL
    FIELDS apiID AS CHARACTER
    FIELDS callingProgram AS CHARACTER
    FIELDS requestDateTime AS DATETIME
    FIELDS success AS LOGICAL
    FIELDS eventRowID AS ROWID
    .

DEFINE TEMP-TABLE ttPrintAPIOutboundEvent NO-UNDO
    FIELDS apiID AS CHARACTER LABEL "API ID"
    FIELDS callingProgram AS CHARACTER LABEL "Calling Program"
    FIELDS requestDateTime AS DATETIME LABEL "Request Date"
    FIELDS success AS LOGICAL LABEL "Success?"
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
&Scoped-define INTERNAL-TABLES ttAPIOutboundEvent

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttAPIOutboundEvent.retryEvent ttAPIOutboundEvent.apiID ttAPIOutboundEvent.callingProgram ttAPIOutboundEvent.requestDateTime ttAPIOutboundEvent.success   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttAPIOutboundEvent.retryEvent   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttAPIOutboundEvent
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttAPIOutboundEvent
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttAPIOutboundEvent       BY ttAPIOutboundEvent.requestDateTime DESCENDING
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttAPIOutboundEvent       BY ttAPIOutboundEvent.requestDateTime DESCENDING.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttAPIOutboundEvent
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttAPIOutboundEvent


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-13 btTest btExit btFilter btAPIIDLookup ~
cbSuccess fiAPIId btExport btRestart btBeginRequestDateCal ~
btEndRequestDateCal fiBeginRequestDate fiEndRequestDate BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS cbSuccess fiAPIIDLabel fiAPIId ~
fiSuccessLabel fiBeginRequestDate fiEndRequestDate fiBeginRequestDatelabel ~
fiEndRequestDatelabel 

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
     SIZE 11 BY 2.62 TOOLTIP "Exit".

DEFINE BUTTON btExport 
     IMAGE-UP FILE "Graphics/32x32/file_excel.ico":U
     LABEL "Export" 
     SIZE 11 BY 2.62 TOOLTIP "Export Data".

DEFINE BUTTON btFilter 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
     LABEL "Filter" 
     SIZE 9 BY 2.14 TOOLTIP "Filter".

DEFINE BUTTON btRestart 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U
     LABEL "Restart" 
     SIZE 9 BY 2.14 TOOLTIP "Retry Event(s)".

DEFINE BUTTON btTest 
     IMAGE-UP FILE "Graphics/32x32/add.ico":U
     LABEL "Test" 
     SIZE 11 BY 2.62 TOOLTIP "Add Outbound Event".

DEFINE VARIABLE cbSuccess AS CHARACTER FORMAT "X(256)":U INITIAL "ALL" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ALL","FAILED","SUCCESS" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fiAPIId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "API ID"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiAPIIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API ID:" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Begin Request Date"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Begin Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "End Request Date"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 27.4 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiSuccessLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Success:" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 119 BY 6.05.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttAPIOutboundEvent SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      ttAPIOutboundEvent.retryEvent COLUMN-LABEL "[ ] All" 
            WIDTH 10 VIEW-AS TOGGLE-BOX
      ttAPIOutboundEvent.apiID COLUMN-LABEL "API ID" FORMAT "x(20)":U
            WIDTH 30
      ttAPIOutboundEvent.callingProgram COLUMN-LABEL "Calling Program" FORMAT "x(45)":U
            WIDTH 56
      ttAPIOutboundEvent.requestDateTime COLUMN-LABEL "Last Request Date" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 40
      ttAPIOutboundEvent.success COLUMN-LABEL "Success" FORMAT "SUCCESS/FAILED":U
      ENABLE ttAPIOutboundEvent.retryEvent
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 158 BY 21.91
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btTest AT ROW 1.48 COL 130 WIDGET-ID 20
     btExit AT ROW 1.48 COL 149 WIDGET-ID 2
     btFilter AT ROW 1.67 COL 110.6 WIDGET-ID 18
     btAPIIDLookup AT ROW 2.19 COL 46 WIDGET-ID 46
     cbSuccess AT ROW 2.19 COL 68.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fiAPIIDLabel AT ROW 2.24 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiAPIId AT ROW 2.24 COL 13.4 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fiSuccessLabel AT ROW 2.24 COL 54.2 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     btExport AT ROW 4.43 COL 130 WIDGET-ID 44
     btRestart AT ROW 4.57 COL 110.6 WIDGET-ID 26
     btBeginRequestDateCal AT ROW 4.95 COL 47.2 WIDGET-ID 40
     btEndRequestDateCal AT ROW 4.95 COL 100 WIDGET-ID 42
     fiBeginRequestDate AT ROW 5 COL 29.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiEndRequestDate AT ROW 5 COL 82.6 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiBeginRequestDatelabel AT ROW 5.05 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiEndRequestDatelabel AT ROW 5.05 COL 54.2 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     BROWSE-2 AT ROW 7.43 COL 2 WIDGET-ID 200
     " Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 4.2 WIDGET-ID 30
          FONT 6
     RECT-13 AT ROW 1.14 COL 2.2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "API Outbound Events"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 28.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.57
         VIRTUAL-WIDTH      = 160
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
/* BROWSE-TAB BROWSE-2 fiEndRequestDatelabel DEFAULT-FRAME */
ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN fiAPIIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBeginRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEndRequestDatelabel IN FRAME DEFAULT-FRAME
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
OPEN QUERY {&SELF-NAME} FOR EACH ttAPIOutboundEvent
      BY ttAPIOutboundEvent.requestDateTime DESCENDING.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* API Outbound Events */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* API Outbound Events */
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
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        RUN api\ResponseDataViewer.w (
            ttAPIOutboundEvent.eventRowID
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
        
        FOR EACH ttAPIOutboundEvent:
            ttAPIOutboundEvent.retryEvent = lReTrigger.
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
                        fiAPIID:SCREEN-VALUE
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
                  

    EMPTY TEMP-TABLE ttPrintAPIOutboundEvent.
    
    FOR EACH ttAPIOutboundEvent:
        CREATE ttPrintAPIOutboundEvent.
        BUFFER-COPY ttAPIOutboundEvent TO ttPrintAPIOutboundEvent.
    END.
    
    RUN TempTableToCSV IN hdOutputProcs (
        INPUT TEMP-TABLE ttPrintAPIOutboundEvent:HANDLE,
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
    
    EMPTY TEMP-TABLE ttAPIOutboundEvent.
    FOR EACH APIOutboundEvent NO-LOCK
        WHERE (IF fiAPIID:SCREEN-VALUE EQ "ALL" OR fiAPIID:SCREEN-VALUE EQ "" THEN
                   TRUE
               ELSE
                   APIOutboundEvent.apiID       EQ fiAPIID:SCREEN-VALUE)
          AND (IF fiBeginRequestDate:SCREEN-VALUE EQ ?  OR
                  fiBeginRequestDate:SCREEN-VALUE EQ "" OR
                  fiBeginRequestDate:SCREEN-VALUE EQ "/  /" THEN
                  TRUE
               ELSE
                  DATE(APIOutboundEvent.requestDateTime) GE DATE(fiBeginRequestDate:SCREEN-VALUE))
          AND (IF fiEndRequestDate:SCREEN-VALUE EQ ?  OR
                  fiEndRequestDate:SCREEN-VALUE EQ "" OR
                  fiEndRequestDate:SCREEN-VALUE EQ "/  /" THEN
                  TRUE
               ELSE
                  DATE(APIOutboundEvent.requestDateTime) LE DATE(fiEndRequestDate:SCREEN-VALUE))
          AND (IF cbSuccess:SCREEN-VALUE = "ALL" THEN
                   TRUE
               ELSE IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN
                   APIOutboundEvent.success = TRUE
               ELSE
                   APIOutboundEvent.success = FALSE)
        BY APIOutboundEvent.requestDateTime DESCENDING:
        CREATE ttAPIOutboundEvent.
        BUFFER-COPY APIOutboundEvent TO ttAPIOutboundEvent.
        ttAPIOutboundEvent.eventRowID = ROWID(APIOutboundEvent).        
    END.
    
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRestart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRestart C-Win
ON CHOOSE OF btRestart IN FRAME DEFAULT-FRAME /* Restart */
DO: 
    DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTotalEvents   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSuccessEvents AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFailureEvents AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER buf_ttAPIOutboundEvent FOR ttAPIOutboundEvent.
        
    FIND FIRST buf_ttAPIOutboundEvent
         WHERE buf_ttAPIOutboundEvent.retryEvent
           AND NOT buf_ttAPIOutboundEvent.success
         NO-ERROR.
    IF NOT AVAILABLE buf_ttAPIOutboundEvent THEN
        RETURN.
        
    SESSION:SET-WAIT-STATE("GENERAL").

    FOR EACH buf_ttAPIOutboundEvent
        WHERE buf_ttAPIOutboundEvent.retryEvent
         BY buf_ttAPIOutboundEvent.requestDateTime:
        ASSIGN
            buf_ttAPIOutboundEvent.retryEvent = FALSE
            .
  
        IF buf_ttAPIOutboundEvent.success THEN
            NEXT.
  
        RUN api/PrepareAndCallOutboundRequest.p (
            buf_ttAPIOutboundEvent.apiID,
            "APIOutboundEvent",
            STRING(buf_ttAPIOutboundEvent.eventRowID),
            TRUE, /* Re-trigger */
            OUTPUT lSuccess,
            OUTPUT cMessage
            ).
  
        FIND FIRST APIOutboundEvent NO-LOCK
             WHERE ROWID(APIOutboundEvent) EQ buf_ttAPIOutboundEvent.eventRowID NO-ERROR.
        IF AVAILABLE APIOutboundEvent THEN
            ASSIGN
                buf_ttAPIOutboundEvent.callingProgram  = APIOutboundEvent.callingProgram
                buf_ttAPIOutboundEvent.requestDateTime = APIOutboundEvent.requestDateTime
                buf_ttAPIOutboundEvent.success         = APIOutboundEvent.success
                .
  
        iTotalEvents = iTotalEvents + 1.
  
        IF lSuccess THEN
            iSuccessEvents = iSuccessEvents + 1.
        ELSE
            iFailureEvents = iFailureEvents + 1.
    END.
       
    SESSION:SET-WAIT-STATE("").
     
    MESSAGE "Outbound Event(s) updated" SKIP
            "Total Records attempted:" iTotalEvents SKIP
            "Total Success Records:" iSuccessEvents SKIP
            "Total Failed Records:" iFailureEvents
       VIEW-AS ALERT-BOX INFORMATION.
    
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTest C-Win
ON CHOOSE OF btTest IN FRAME DEFAULT-FRAME /* Test */
DO:
    RUN api/APIOutboundTest.w.
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
        "apiID", /* lookup field */
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
  DISPLAY cbSuccess fiAPIIDLabel fiAPIId fiSuccessLabel fiBeginRequestDate 
          fiEndRequestDate fiBeginRequestDatelabel fiEndRequestDatelabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-13 btTest btExit btFilter btAPIIDLookup cbSuccess fiAPIId 
         btExport btRestart btBeginRequestDateCal btEndRequestDateCal 
         fiBeginRequestDate fiEndRequestDate BROWSE-2 
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

