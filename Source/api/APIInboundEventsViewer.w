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
{methods/defines/sortByDefs.i}

DEFINE VARIABLE lReTrigger           AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdOutputProcs        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdInboundProcs       AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdBuffer             AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdQuery              AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdUserField1Column   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdUserField2Column   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdUserField3Column   AS HANDLE  NO-UNDO.

DEFINE TEMP-TABLE ttAPIInboundEvent NO-UNDO
    FIELDS retryEvent              AS LOGICAL
    FIELDS apiRoute                AS CHARACTER LABEL "API Route"
    FIELDS requestedBy             AS CHARACTER LABEL "Requested By"
    FIELDS requestDateTime         AS DATETIME  LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS.SSS"
    FIELDS success                 AS LOGICAL   LABEL "Success?"
    FIELDS apiInboundEventID       AS INTEGER   LABEL "Event ID"
    FIELDS eventRowID              AS ROWID
    FIELDS errorMessage            AS CHARACTER LABEL "Request Message"
    FIELDS userField1              AS CHARACTER LABEL "User Field 1"
    FIELDS userField2              AS CHARACTER LABEL "User Field 2"
    FIELDS userField3              AS CHARACTER LABEL "User Field 3"
    FIELDS delayedProcessingStatus AS CHARACTER LABEL "Status"
    INDEX apiInboundEventID apiInboundEventID
    .

DEFINE TEMP-TABLE ttPrintAPIInboundEvent NO-UNDO
    FIELDS apiRoute                AS CHARACTER LABEL "API Route"
    FIELDS requestedBy             AS CHARACTER LABEL "Requested By"
    FIELDS requestDateTime         AS DATETIME  LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS.SSS"
    FIELDS success                 AS LOGICAL   LABEL "Success?"
    FIELDS apiInboundEventID       AS INTEGER   LABEL "Event ID"
    FIELDS errorMessage            AS CHARACTER LABEL "Request Message"
    FIELDS userField1              AS CHARACTER LABEL "User Field 1"
    FIELDS userField2              AS CHARACTER LABEL "User Field 2"
    FIELDS userField3              AS CHARACTER LABEL "User Field 3"
    FIELDS delayedProcessingStatus AS CHARACTER LABEL "Status"
    .

&SCOPED-DEFINE SORTBY-PHRASE BY ttAPIInboundEvent.requestDateTime DESC

CREATE QUERY hdQuery.
       
hdQuery:SET-BUFFERS(BUFFER APIInboundEvent:HANDLE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME ttAPIInboundEvent

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAPIInboundEvent

/* Definitions for BROWSE ttAPIInboundEvent                             */
&Scoped-define FIELDS-IN-QUERY-ttAPIInboundEvent ttAPIInboundEvent.retryEvent ttAPIInboundEvent.apiRoute ttAPIInboundEvent.requestDateTime ttAPIInboundEvent.delayedProcessingStatus ttAPIInboundEvent.errorMessage ttAPIInboundEvent.userField1 ttAPIInboundEvent.userField2 ttAPIInboundEvent.userField3 ttAPIInboundEvent.apiInboundEventID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttAPIInboundEvent ttAPIInboundEvent.retryEvent   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttAPIInboundEvent ttAPIInboundEvent
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttAPIInboundEvent ttAPIInboundEvent
&Scoped-define SELF-NAME ttAPIInboundEvent
&Scoped-define QUERY-STRING-ttAPIInboundEvent FOR EACH ttAPIInboundEvent ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-ttAPIInboundEvent OPEN QUERY {&SELF-NAME} FOR EACH ttAPIInboundEvent ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-ttAPIInboundEvent ttAPIInboundEvent
&Scoped-define FIRST-TABLE-IN-QUERY-ttAPIInboundEvent ttAPIInboundEvent


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-ttAPIInboundEvent}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 btExport btExit fieventID btFilter ~
btAPIIDLookup fiAPIId cbSuccess btBeginRequestDateCal btEndRequestDateCal ~
fiBeginRequestDate fiBeginHours fiBeginMins fiEndRequestDate fiEndHours ~
fiEndMins btRestart fiRequestData ttAPIInboundEvent 
&Scoped-Define DISPLAYED-OBJECTS fieventIDlb fieventID fiAPIIdLabel fiAPIId ~
fiSuccessLabel cbSuccess fiBeginRequestDatelabel fiBeginRequestDate ~
fiBeginHours fiBeginMins fiendRequestDatelabel fiEndRequestDate fiEndHours ~
fiEndMins fiRequestData fiMessage 

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
     LIST-ITEMS "ALL","FAILED","SUCCESS","QUEUED" 
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

DEFINE VARIABLE fiBeginHours AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginMins AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Begin Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndHours AS INTEGER FORMAT "99":U INITIAL 23 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndMins AS INTEGER FORMAT "99":U INITIAL 59 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiEndRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiendRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 21.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fieventID AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fieventIDlb AS CHARACTER FORMAT "X(256)":U INITIAL "Event ID:" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiRequestData AS CHARACTER FORMAT "X(256)":U 
     LABEL "Find Request Data" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiSuccessLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Status:" 
     VIEW-AS FILL-IN 
     SIZE 9.2 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 138 BY 5.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttAPIInboundEvent FOR 
      ttAPIInboundEvent SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttAPIInboundEvent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttAPIInboundEvent C-Win _FREEFORM
  QUERY ttAPIInboundEvent NO-LOCK DISPLAY
      ttAPIInboundEvent.retryEvent COLUMN-LABEL "[ ] All" 
            WIDTH 8 VIEW-AS TOGGLE-BOX
      ttAPIInboundEvent.apiRoute COLUMN-LABEL "API Route" FORMAT "x(30)":U
            WIDTH 35 LABEL-BGCOLOR 14
      ttAPIInboundEvent.requestDateTime COLUMN-LABEL "Request Date" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 26 LABEL-BGCOLOR 14
      ttAPIInboundEvent.delayedProcessingStatus COLUMN-LABEL "Status" FORMAT "x(15)":U
            WIDTH 12 LABEL-BGCOLOR 14
      ttAPIInboundEvent.errorMessage COLUMN-LABEL "Response Result" FORMAT "x(256)":U
            WIDTH 55 
      ttAPIInboundEvent.userField1 COLUMN-LABEL "User Field 1" FORMAT "x(256)":U
            WIDTH 20 LABEL-BGCOLOR 14
      ttAPIInboundEvent.userField2 COLUMN-LABEL "User Field 2" FORMAT "x(256)":U
            WIDTH 20 LABEL-BGCOLOR 14
      ttAPIInboundEvent.userField3 COLUMN-LABEL "User Field 3" FORMAT "x(256)":U
            WIDTH 25 LABEL-BGCOLOR 14
      ttAPIInboundEvent.apiInboundEventID COLUMN-LABEL "Event ID" FORMAT "->,>>>,>>9":U
            WIDTH 12 LABEL-BGCOLOR 14
      ENABLE ttAPIInboundEvent.retryEvent
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 226 BY 22.38
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExport AT ROW 1.24 COL 202 WIDGET-ID 50
     btExit AT ROW 1.24 COL 216.8 WIDGET-ID 2
     fieventIDlb AT ROW 1.67 COL 13.4 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fieventID AT ROW 1.67 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     btFilter AT ROW 1.71 COL 129.6 WIDGET-ID 18
     btAPIIDLookup AT ROW 2.76 COL 77 WIDGET-ID 48
     fiAPIIdLabel AT ROW 2.81 COL 13.8 NO-LABEL WIDGET-ID 6
     fiAPIId AT ROW 2.81 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fiSuccessLabel AT ROW 2.81 COL 81.8 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbSuccess AT ROW 2.81 COL 91.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     btBeginRequestDateCal AT ROW 3.86 COL 42.4 WIDGET-ID 44
     btEndRequestDateCal AT ROW 3.86 COL 108.2 WIDGET-ID 46
     fiBeginRequestDatelabel AT ROW 3.95 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiBeginRequestDate AT ROW 3.95 COL 25.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiBeginHours AT ROW 3.95 COL 46.6 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     fiBeginMins AT ROW 3.95 COL 52 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     fiendRequestDatelabel AT ROW 3.95 COL 69.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiEndRequestDate AT ROW 3.95 COL 91.2 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     fiEndHours AT ROW 3.95 COL 112.4 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     fiEndMins AT ROW 3.95 COL 117.8 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     btRestart AT ROW 4.1 COL 129.6 WIDGET-ID 26
     fiRequestData AT ROW 5.1 COL 25.2 COLON-ALIGNED WIDGET-ID 56
     fiMessage AT ROW 5.1 COL 78.8 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     ttAPIInboundEvent AT ROW 6.95 COL 2 WIDGET-ID 200
     ":" VIEW-AS TEXT
          SIZE 1 BY .62 AT ROW 4.1 COL 53 WIDGET-ID 60
     " Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 4.2 WIDGET-ID 52
          FONT 6
     ":" VIEW-AS TEXT
          SIZE 1 BY .62 AT ROW 4.1 COL 118.8 WIDGET-ID 68
     RECT-26 AT ROW 1.24 COL 2 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 227.4 BY 28.57
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
         WIDTH              = 227.4
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 227.4
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 227.4
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
/* BROWSE-TAB ttAPIInboundEvent fiMessage DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fiAPIIdLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fiBeginRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiendRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fieventIDlb IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSuccessLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ttAPIInboundEvent:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttAPIInboundEvent
/* Query rebuild information for BROWSE ttAPIInboundEvent
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAPIInboundEvent ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE ttAPIInboundEvent */
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
    DEFINE VARIABLE cErrorMessage  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData  AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cQuery         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCompanyFilter AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iBeginTime     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime       AS INTEGER   NO-UNDO.
    
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
    
    ASSIGN
        iBeginTime = (INTEGER(fiBeginHours:SCREEN-VALUE) * 60 +  INTEGER(fiBeginMins:SCREEN-VALUE)) * 60 * 1000
        iEndTime   = (INTEGER(fiEndHours:SCREEN-VALUE) * 60 +  INTEGER(fiEndMins:SCREEN-VALUE)) * 60 * 1000 + 60 * 1000 - 1
        .

    cQuery = "FOR EACH APIInboundEvent FIELDS (apiRoute requestDateTime success requestedBy apiInboundEventID errorMessage requestData userField1 userField2 userField3 delayedProcessingStatus) NO-LOCK WHERE TRUE ".
    
    IF fiEventID:SCREEN-VALUE NE "0" THEN
        cQuery = cQuery + " AND APIInboundEvent.apiInboundEventID EQ " + REPLACE(fiEventID:SCREEN-VALUE,",","").

    IF fiAPIID:SCREEN-VALUE NE "ALL" AND fiAPIID:SCREEN-VALUE NE "" THEN
        ASSIGN
            cQuery         = cQuery 
/*                           + " AND APIInboundEvent.company EQ '" + g_company + "'"*/
                           + " AND APIInboundEvent.apiRoute EQ '" + fiAPIID:SCREEN-VALUE + "'"
            lCompanyFilter = TRUE
            .
    
    IF fiBeginRequestDate:SCREEN-VALUE NE ?  AND
       fiBeginRequestDate:SCREEN-VALUE NE "" AND
       fiBeginRequestDate:SCREEN-VALUE NE "/  /" THEN
        cQuery = cQuery + " AND APIInboundEvent.requestDateTime GE '" + STRING(DATETIME(DATE(fiBeginRequestDate:SCREEN-VALUE), iBeginTime)) + "'".

    IF fiEndRequestDate:SCREEN-VALUE NE ?  AND
       fiEndRequestDate:SCREEN-VALUE NE "" AND
       fiEndRequestDate:SCREEN-VALUE NE "/  /" THEN
        cQuery = cQuery + " AND APIInboundEvent.requestDateTime LT '" + STRING(DATETIME(DATE(fiEndRequestDate:SCREEN-VALUE), iEndTime)) + "'".
        
    IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN
        cQuery = cQuery
               + " AND APIInboundEvent.success = TRUE AND APIInboundEvent.delayedProcessingStatus NE 'queued'".
    ELSE IF cbSuccess:SCREEN-VALUE = "FAILED" THEN
        cQuery = cQuery
               + " AND APIInboundEvent.success = FALSE".
    ELSE IF cbSuccess:SCREEN-VALUE = "QUEUED" THEN
        cQuery = cQuery
               + " AND APIInboundEvent.delayedProcessingStatus = 'queued'".

    ETIME(YES).
    
    fiMessage:SCREEN-VALUE = "Searching...".
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    hdQuery:QUERY-PREPARE(cQuery).
    hdQuery:QUERY-OPEN().
    
    DO WHILE hdQuery:GET-NEXT():                
        IF fiRequestData:SCREEN-VALUE NE "" THEN DO:
            /* Searching CLOB data takes a lot of processing time, 
               so search if any of the user fields contains the search criteria before */
            IF APIInboundEvent.userField1 BEGINS fiRequestData:SCREEN-VALUE OR
               APIInboundEvent.userField2 BEGINS fiRequestData:SCREEN-VALUE OR
               APIInboundEvent.userField2 BEGINS fiRequestData:SCREEN-VALUE THEN.
            ELSE DO:
                IF LENGTH(APIInboundEvent.requestData) LT LENGTH(fiRequestData:SCREEN-VALUE) THEN
                    NEXT.
    
                lcRequestData = APIInboundEvent.requestData.
                IF INDEX(lcRequestData, fiRequestData:SCREEN-VALUE) LE 0 THEN
                    NEXT.
            END.
        END.

        ASSIGN
            cErrorMessage = ENTRY(1, APIInboundEvent.errorMessage, "@@@")
            cErrorMessage = SUBSTRING(cErrorMessage, R-INDEX(cErrorMessage, " - " + STRING(APIInboundEvent.success, "SUCCESS/FAILURE") + " - ") + 13)
            .

        CREATE ttAPIInboundEvent.
        ASSIGN
            ttAPIInboundEvent.eventRowID              = ROWID(APIInboundEvent) 
            ttAPIInboundEvent.apiRoute                = APIInboundEvent.apiRoute
            ttAPIInboundEvent.requestDateTime         = APIInboundEvent.requestDateTime
            ttAPIInboundEvent.success                 = APIInboundEvent.success
            ttAPIInboundEvent.requestedBy             = APIInboundEvent.requestedBy
            ttAPIInboundEvent.apiInboundEventID       = APIInboundEvent.apiInboundEventID
            ttAPIInboundEvent.userField1              = APIInboundEvent.userField1
            ttAPIInboundEvent.userField2              = APIInboundEvent.userField2
            ttAPIInboundEvent.userField3              = APIInboundEvent.userField3
            ttAPIInboundEvent.delayedProcessingStatus = IF APIInboundEvent.delayedProcessingStatus EQ "queued" THEN
                                                            "QUEUED"
                                                        ELSE
                                                            STRING(APIInboundEvent.success, "SUCCESS/FAILED")
            ttAPIInboundEvent.errorMessage            = cErrorMessage
            NO-ERROR. 

        iCount = iCount + 1.
    END.   
    
    hdQuery:QUERY-CLOSE ().
    
    IF iCount EQ 0 THEN
        fiMessage:SCREEN-VALUE = "No records found!".
    ELSE    
        fiMessage:SCREEN-VALUE = STRING(iCount) + " Record(s) found in " + TRIM(STRING(ROUND(ETIME / 1000, 2),">>>>>>>>>>9.99")) + "s".

    RUN pUpdateUserFieldLabels (
        INPUT fiAPIID:SCREEN-VALUE
        ).
    
    SESSION:SET-WAIT-STATE("").
    
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
    
    fiMessage:SCREEN-VALUE = "Re-triggering requests...".
    
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
    
    fiMessage:SCREEN-VALUE = "Process complete!".
    
    MESSAGE "Inbound Event(s) updated" SKIP
            "Total Records attempted:" iTotalEvents SKIP
            "Total Success Records:" iSuccessEvents SKIP
            "Total Failed Records:" iFailureEvents
       VIEW-AS ALERT-BOX INFORMATION.
    
    APPLY "CHOOSE" TO btFilter.
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


&Scoped-define BROWSE-NAME ttAPIInboundEvent
&Scoped-define SELF-NAME ttAPIInboundEvent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttAPIInboundEvent C-Win
ON DEFAULT-ACTION OF ttAPIInboundEvent IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttAPIInboundEvent THEN DO:
        RUN api/ResponseInboundDataViewer.w (
            INPUT ttAPIInboundEvent.apiInboundEventID
            ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttAPIInboundEvent C-Win
ON START-SEARCH OF ttAPIInboundEvent IN FRAME DEFAULT-FRAME
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
    ELSE IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        
        IF cColumnLabel EQ cSaveLabel THEN
            lAscending = NOT lAscending.
        IF VALID-HANDLE(hSaveLabel) THEN
            hSaveLabel:LABEL-BGCOLOR = ?.
    
        ASSIGN
            hColumnLabel               = {&BROWSE-NAME}:CURRENT-COLUMN
            hColumnLabel:LABEL-BGCOLOR = 14
            hSaveLabel                 = hColumnLabel
            cSaveLabel                 = cColumnLabel
            .
        RUN pReopenBrowse.
    END.
    
    RETURN NO-APPLY.  
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
    DEFINE VARIABLE iBrowseColumn AS INTEGER NO-UNDO.
    
    RUN enable_UI.

    DO iBrowseColumn = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS :
        IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "userField1" THEN
            hdUserField1Column = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).
        ELSE IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "userField2" THEN
            hdUserField2Column = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).
        ELSE IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "userField3" THEN
            hdUserField3Column = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).            
    END.

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

&Scoped-define sdBrowseName ttAPIInboundEvent
{methods/sortByProc.i "pByAPIRoute" "ttAPIInboundEvent.apiRoute"}
{methods/sortByProc.i "pByUserField1" "ttAPIInboundEvent.userField1"}
{methods/sortByProc.i "pByUserField2" "ttAPIInboundEvent.userField2"}
{methods/sortByProc.i "pByUserField3" "ttAPIInboundEvent.userField3"}
{methods/sortByProc.i "pByEventID" "ttAPIInboundEvent.apiInboundEventID"}
{methods/sortByProc.i "pByDelayedProcessingStatus" "ttAPIInboundEvent.delayedProcessingStatus"}
{methods/sortByProc.i "pByRequestDate" "ttAPIInboundEvent.requestDateTime"}

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
          fiBeginRequestDatelabel fiBeginRequestDate fiBeginHours fiBeginMins 
          fiendRequestDatelabel fiEndRequestDate fiEndHours fiEndMins 
          fiRequestData fiMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-26 btExport btExit fieventID btFilter btAPIIDLookup fiAPIId 
         cbSuccess btBeginRequestDateCal btEndRequestDateCal fiBeginRequestDate 
         fiBeginHours fiBeginMins fiEndRequestDate fiEndHours fiEndMins 
         btRestart fiRequestData ttAPIInboundEvent 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReOpenBrowse C-Win 
PROCEDURE pReOpenBrowse PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "apiRoute" THEN
            RUN pByAPIRoute.
        WHEN "userField1" THEN
            RUN pByUserField1.
        WHEN "userField2" THEN
            RUN pByUserField2.
        WHEN "userField3" THEN
            RUN pByUserField3.
        WHEN "apiInboundEventID" THEN
            RUN pByEventID.
        WHEN "delayedProcessingStatus" THEN
            RUN pByDelayedProcessingStatus.
        WHEN "requestDateTime" THEN
            RUN pByRequestDate.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateUserFieldLabels C-Win 
PROCEDURE pUpdateUserFieldLabels PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPIRoute AS CHARACTER NO-UNDO.    
    
    DEFINE VARIABLE cUserField1Label AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField2Label AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField3Label AS CHARACTER NO-UNDO.
    
    CASE ipcAPIRoute:
        WHEN "/api/cXMLOrder" THEN
            ASSIGN
                cUserField1Label = "PO #"
                cUserField2Label = "Site #"
                cUserField3Label = "Part #"
                .
        OTHERWISE
            ASSIGN
                cUserField1Label = "User Field 1"
                cUserField2Label = "User Field 2"
                cUserField3Label = "User Field 3"
                .
    END.
    
    IF VALID-HANDLE(hdUserField1Column) THEN
        hdUserField1Column:LABEL = cUserField1Label.

    IF VALID-HANDLE(hdUserField2Column) THEN
        hdUserField2Column:LABEL = cUserField2Label.
 
    IF VALID-HANDLE(hdUserField3Column) THEN
        hdUserField3Column:LABEL = cUserField3Label.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

