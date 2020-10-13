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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE lReTrigger           AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdOutputProcs        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdAPIInboundTestWin  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdAPIInboundTestProc AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdInboundProcs       AS HANDLE  NO-UNDO.

DEFINE TEMP-TABLE ttAPIInboundEvent NO-UNDO
    FIELDS retryEvent AS LOGICAL
    FIELDS apiRoute AS CHARACTER
    FIELDS requestedBy AS CHARACTER
    FIELDS requestDateTime AS DATETIME
    FIELDS success AS LOGICAL
    FIELDS apiInboundEventID AS INTEGER
    FIELDS eventRowID AS ROWID
    FIELDS errorMessage AS CHARACTER
    .

DEFINE TEMP-TABLE ttPrintAPIInboundEvent NO-UNDO
    FIELDS apiRoute AS CHARACTER LABEL "API Route"
    FIELDS requestedBy AS CHARACTER LABEL "Requested By"
    FIELDS requestDateTime AS DATETIME LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS.SSS"
    FIELDS success AS LOGICAL LABEL "Success?" FORMAT "SUCCESS/FAILURE"
    FIELDS apiInboundEventID AS INTEGER LABEL "Event ID"
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
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttAPIInboundEvent.retryEvent ttAPIInboundEvent.apiRoute ttAPIInboundEvent.requestDateTime ttAPIInboundEvent.success ttAPIInboundEvent.requestedby ttAPIInboundEvent.apiInboundEventID ttAPIInboundEvent.errorMessage   
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
&Scoped-Define ENABLED-OBJECTS RECT-26 btTest btExit fieventID btFilter ~
btAPIIDLookup fiAPIId cbSuccess btBeginRequestDateCal btEndRequestDateCal ~
fiBeginRequestDate fiEndRequestDate btRestart btExport fiRequestData ~
BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fieventIDlb fieventID fiAPIIdLabel fiAPIId ~
fiSuccessLabel cbSuccess fiBeginRequestDatelabel fiBeginRequestDate ~
fiendRequestDatelabel fiEndRequestDate fiRequestData 

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

DEFINE BUTTON btTest 
     IMAGE-UP FILE "Graphics/32x32/add.ico":U
     LABEL "Test" 
     SIZE 11 BY 2.62 TOOLTIP "Add Inbound Event".

DEFINE VARIABLE cbSuccess AS CHARACTER FORMAT "X(256)":U INITIAL "ALL" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ALL","FAILED","SUCCESS" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE fiAPIId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiAPIIdLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API Route:" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Begin Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiendRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fieventID AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fieventIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "Event ID:" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiRequestData AS CHARACTER FORMAT "X(256)":U 
     LABEL "Find Request Data" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiSuccessLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Success:" 
     VIEW-AS FILL-IN 
     SIZE 10.75 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 119.4 BY 5.48.

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
            WIDTH 30
      ttAPIInboundEvent.requestDateTime COLUMN-LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 27
      ttAPIInboundEvent.success COLUMN-LABEL "Success" FORMAT "SUCCESS/FAILED":U
            WIDTH 10
      ttAPIInboundEvent.requestedby COLUMN-LABEL "Requested By" FORMAT "x(8)":U
            WIDTH 17
      ttAPIInboundEvent.apiInboundEventID COLUMN-LABEL "Event ID" FORMAT "->,>>>,>>9":U
            WIDTH 10
      ttAPIInboundEvent.errorMessage COLUMN-LABEL "Response Result" FORMAT "x(256)":U
            WIDTH 100
      ENABLE ttAPIInboundEvent.retryEvent
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 157.6 BY 22.38
         FONT 34 ROW-HEIGHT-CHARS .9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btTest AT ROW 1.24 COL 123 WIDGET-ID 20
     btExit AT ROW 1.24 COL 149 WIDGET-ID 2
     fieventIDlb AT ROW 1.67 COL 13.4 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fieventID AT ROW 1.67 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     btFilter AT ROW 1.71 COL 111.4 WIDGET-ID 18
     btAPIIDLookup AT ROW 2.76 COL 77 WIDGET-ID 48
     fiAPIIdLabel AT ROW 2.81 COL 13.8 NO-LABEL WIDGET-ID 6
     fiAPIId AT ROW 2.81 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fiSuccessLabel AT ROW 2.81 COL 80.2 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbSuccess AT ROW 2.81 COL 91.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     btBeginRequestDateCal AT ROW 3.86 COL 42.4 WIDGET-ID 44
     btEndRequestDateCal AT ROW 3.86 COL 92 WIDGET-ID 46
     fiBeginRequestDatelabel AT ROW 3.95 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiBeginRequestDate AT ROW 3.95 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiendRequestDatelabel AT ROW 3.95 COL 52.8 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiEndRequestDate AT ROW 3.95 COL 75 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     btRestart AT ROW 4.1 COL 111.4 WIDGET-ID 26
     btExport AT ROW 4.1 COL 123 WIDGET-ID 50
     fiRequestData AT ROW 5.1 COL 25.2 COLON-ALIGNED WIDGET-ID 56
     BROWSE-2 AT ROW 6.95 COL 2 WIDGET-ID 200
     " Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 4.2 WIDGET-ID 52
          FONT 6
     RECT-26 AT ROW 1.24 COL 2 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


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
         WIDTH              = 160
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 33.57
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
/* BROWSE-TAB BROWSE-2 fiRequestData DEFAULT-FRAME */
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
        RUN api/ResponseInboundDataViewer.w (
            INPUT ttAPIInboundEvent.apiInboundEventID
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

    IF VALID-HANDLE(hdInboundProcs) THEN
        DELETE PROCEDURE hdInboundProcs.
        
    IF VALID-HANDLE(hdAPIInboundTestWin) THEN
        APPLY "WINDOW-CLOSE" TO hdAPIInboundTestWin.

    IF VALID-HANDLE(hdAPIInboundTestProc) THEN
        DELETE OBJECT hdAPIInboundTestProc.
                
    APPLY "CLOSE":U TO THIS-PROCEDURE.
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
    DEFINE VARIABLE lSuccess      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    
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
 
    RUN Output_TempTableToCSV IN hdOutputProcs (
        INPUT TEMP-TABLE ttPrintAPIInboundEvent:HANDLE,
        INPUT cFullFilePath,
        INPUT TRUE, /* Export Header */
        INPUT TRUE, /* Auto increment File name */
        OUTPUT lSuccess,
        OUTPUT cMessage
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
    DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData AS LONGCHAR  NO-UNDO.
    
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
                  APIInboundEvent.apiInboundEventID EQ INTEGER(fieventID:SCREEN-VALUE))
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
        cErrorMessage = IF APIInboundEvent.errorMessage EQ "" THEN
                            ""
                        ELSE IF INDEX(APIInboundEvent.errorMessage, "@@@") EQ 0 THEN
                            ENTRY(NUM-ENTRIES(REPLACE(APIInboundEvent.errorMessage," - ","~~"),"~~"),REPLACE(APIInboundEvent.errorMessage," - ","~~"),"~~")
                        ELSE IF INDEX(APIInboundEvent.errorMessage, "@@@") GT 1 THEN
                            ENTRY(NUM-ENTRIES(REPLACE(SUBSTRING(APIInboundEvent.errorMessage,1, INDEX(APIInboundEvent.errorMessage, "@@@") - 1)," - ","~~"),"~~"),REPLACE(SUBSTRING(APIInboundEvent.errorMessage,1, INDEX(APIInboundEvent.errorMessage, "@@@") - 1)," - ","~~"),"~~")
                        ELSE
                            APIInboundEvent.errorMessage
                        .
        
        IF fiRequestData:SCREEN-VALUE NE "" THEN DO:
            lcRequestData = APIInboundEvent.requestData.
            IF NOT lcRequestData MATCHES "*" + fiRequestData:SCREEN-VALUE + "*" THEN
                NEXT.
        END.
                          
        CREATE ttAPIInboundEvent.
        BUFFER-COPY APIInboundEvent TO ttAPIInboundEvent.
        ASSIGN
            ttAPIInboundEvent.eventRowID   = ROWID(APIInboundEvent) 
            ttAPIInboundEvent.errorMessage = cErrorMessage
            NO-ERROR. 
   
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
    
    DEFINE BUFFER buf_ttAPIInboundEvent FOR ttAPIInboundEvent.
         
    FIND FIRST buf_ttAPIInboundEvent
         WHERE buf_ttAPIInboundEvent.retryEvent
           AND NOT buf_ttAPIInboundEvent.success
         NO-ERROR.
         
    IF NOT AVAILABLE buf_ttAPIInboundEvent THEN
        RETURN.
     
    SESSION:SET-WAIT-STATE("GENERAL").

    FOR EACH buf_ttAPIInboundEvent
        WHERE buf_ttAPIInboundEvent.retryEvent
         BY buf_ttAPIInboundEvent.requestDateTime:
        ASSIGN
            buf_ttAPIInboundEvent.retryEvent = FALSE
            .
  
        IF buf_ttAPIInboundEvent.success THEN
            NEXT.
 
        RUN Inbound_ReTrigger IN hdInboundProcs (
            INPUT  buf_ttAPIInboundEvent.apiInboundEventID,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ) NO-ERROR.

        FIND FIRST APIInboundEvent EXCLUSIVE-LOCK
             WHERE ROWID(APIInboundEvent) EQ buf_ttAPIInboundEvent.eventRowID NO-ERROR.
        IF AVAILABLE APIInboundEvent THEN
            buf_ttAPIInboundEvent.success = APIInboundEvent.success.
        
        iTotalEvents = iTotalEvents + 1.
  
        IF buf_ttAPIInboundEvent.success THEN
            iSuccessEvents = iSuccessEvents + 1.
        ELSE
            iFailureEvents = iFailureEvents + 1.
    END.
    SESSION:SET-WAIT-STATE("").
     
    MESSAGE "Inbound Event(s) updated" SKIP
            "Total Records attempted:" iTotalEvents SKIP
            "Total Success Records:" iSuccessEvents SKIP
            "Total Failed Records:" iFailureEvents
       VIEW-AS ALERT-BOX INFORMATION.
    
    APPLY "CHOOSE" TO btFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTest C-Win
ON CHOOSE OF btTest IN FRAME DEFAULT-FRAME /* Test */
DO:
    DO WITH FRAME {&FRAME-NAME}:
    END.

    IF NOT VALID-HANDLE(hdAPIInboundTestProc) THEN DO:         
        RUN api/APIInboundTest.w PERSISTENT SET hdAPIInboundTestProc.
        
        hdAPIInboundTestWin = hdAPIInboundTestProc:CURRENT-WINDOW.
    END.
                                                 
    IF VALID-HANDLE(hdAPIInboundTestProc) AND
        VALID-HANDLE(hdAPIInboundTestWin) THEN DO:        

        IF hdAPIInboundTestWin:WINDOW-STATE EQ 2 THEN ASSIGN 
            hdAPIInboundTestWin:WINDOW-STATE = 3.
        
        hdAPIInboundTestWin:MOVE-TO-TOP().
    END.
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
{methods/template/brwcustom.i}
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
    RUN api/InboundProcs.p  PERSISTENT SET hdInboundProcs.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{sys/inc/f3help.i}

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
  DISPLAY fieventIDlb fieventID fiAPIIdLabel fiAPIId fiSuccessLabel cbSuccess 
          fiBeginRequestDatelabel fiBeginRequestDate fiendRequestDatelabel 
          fiEndRequestDate fiRequestData 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-26 btTest btExit fieventID btFilter btAPIIDLookup fiAPIId 
         cbSuccess btBeginRequestDateCal btEndRequestDateCal fiBeginRequestDate 
         fiEndRequestDate btRestart btExport fiRequestData BROWSE-2 
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

