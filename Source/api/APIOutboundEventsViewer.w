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
{methods/defines/sortByDefs.i}

DEFINE VARIABLE lReTrigger      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE hdOutputProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdOutboundProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdQuery         AS HANDLE  NO-UNDO.
DEFINE VARIABLE cCompany        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation       AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdUserField1Column AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdUserField2Column AS HANDLE  NO-UNDO.
DEFINE VARIABLE hdPrimaryIDColumn  AS HANDLE  NO-UNDO.

ASSIGN
    cCompany  = g_company
    cLocation = g_loc
    .
    
DEFINE TEMP-TABLE ttAPIOutboundEvent NO-UNDO
    FIELDS retryEvent         AS LOGICAL
    FIELDS company            AS CHARACTER
    FIELDS apiID              AS CHARACTER
    FIELDS clientID           AS CHARACTER
    FIELDS sourceTriggerID    AS CHARACTER
    FIELDS primaryID          AS CHARACTER
    FIELDS eventDescription   AS CHARACTER    
    FIELDS callingProgram     AS CHARACTER
    FIELDS userField1         AS CHARACTER
    FIELDS userField2         AS CHARACTER
    FIELDS requestDateTime    AS DATETIME
    FIELDS success            AS LOGICAL
    FIELDS apiOutboundEventID AS INTEGER
    FIELDS eventRowID         AS ROWID
    .

DEFINE TEMP-TABLE ttPrintAPIOutboundEvent NO-UNDO
    FIELDS company            AS CHARACTER LABEL "Company"
    FIELDS apiID              AS CHARACTER LABEL "API ID"
    FIELDS clientID           AS CHARACTER LABEL "Client ID"
    FIELDS sourceTriggerID    AS CHARACTER LABEL "Trigger ID"
    FIELDS primaryID          AS CHARACTER LABEL "Primary ID"
    FIELDS eventDescription   AS CHARACTER LABEL "Event Description"
    FIELDS callingProgram     AS CHARACTER LABEL "Calling Program"
    FIELDS requestDateTime    AS DATETIME  LABEL "Request Date"
    FIELDS success            AS LOGICAL   LABEL "Success?"
    FIELDS apiOutboundEventID AS INTEGER   LABEL "Outbound Event ID"
    .

&SCOPED-DEFINE SORTBY-PHRASE BY ttAPIOutboundEvent.requestDateTime DESC

CREATE QUERY hdQuery.
       
hdQuery:SET-BUFFERS(BUFFER APIOutboundEvent:HANDLE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME ttAPIOutboundEvent

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAPIOutboundEvent

/* Definitions for BROWSE ttAPIOutboundEvent                            */
&Scoped-define FIELDS-IN-QUERY-ttAPIOutboundEvent ttAPIOutboundEvent.retryEvent ttAPIOutboundEvent.apiID ttAPIOutboundEvent.clientID ttAPIOutboundEvent.sourceTriggerID ttAPIOutboundEvent.requestDateTime ttAPIOutboundEvent.success ttAPIOutboundEvent.primaryID ttAPIOutboundEvent.userField1 ttAPIOutboundEvent.userField2 ttAPIOutboundEvent.apiOutboundEventID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttAPIOutboundEvent ttAPIOutboundEvent.retryEvent   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttAPIOutboundEvent ~
ttAPIOutboundEvent
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttAPIOutboundEvent ttAPIOutboundEvent
&Scoped-define SELF-NAME ttAPIOutboundEvent
&Scoped-define QUERY-STRING-ttAPIOutboundEvent FOR EACH ttAPIOutboundEvent ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-ttAPIOutboundEvent OPEN QUERY {&SELF-NAME} FOR EACH ttAPIOutboundEvent ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-ttAPIOutboundEvent ttAPIOutboundEvent
&Scoped-define FIRST-TABLE-IN-QUERY-ttAPIOutboundEvent ttAPIOutboundEvent


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-ttAPIOutboundEvent}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-13 btExport btExit btFilter ~
btAPIIDLookup fiEventID fiPrimaryID fiAPIId btClientIDLookup ~
btTriggerIDLookup fiClientID fiTriggerID btRestart btBeginRequestDateCal ~
btEndRequestDateCal fiBeginRequestDate fiEndRequestDate cbSuccess ~
ttAPIOutboundEvent 
&Scoped-Define DISPLAYED-OBJECTS fiEventIDLabel fiEventID fiPrimaryIDLabel ~
fiPrimaryID fiAPIIDLabel fiAPIId fiClientIDLabel fiClientID ~
fiTriggerIDLabel fiTriggerID fiBeginRequestDate fiEndRequestDate ~
fiBeginRequestDatelabel fiEndRequestDatelabel fiSuccessLabel cbSuccess ~
fiMessage 

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

DEFINE BUTTON btClientIDLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btEndRequestDateCal 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.24.

DEFINE BUTTON btExit 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U
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

DEFINE BUTTON btTriggerIDLookup 
     IMAGE-UP FILE "Graphics/16x16/magnifying_glass.gif":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4.6 BY 1.1.

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
     SIZE 9 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.14 TOOLTIP "Begin Request Date"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiBeginRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Begin Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 24.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiClientID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.4 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiClientIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Client ID:" 
     VIEW-AS FILL-IN 
     SIZE 12.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndRequestDate AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.14 TOOLTIP "End Request Date"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEndRequestDatelabel AS CHARACTER FORMAT "X(256)":U INITIAL "End Request Date:" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEventID AS INT64 FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiEventIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Event ID:" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiMessage AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 62.8 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiPrimaryID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiPrimaryIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Primary ID:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiSuccessLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Success:" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTriggerID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTriggerIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Trigger ID:" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 134 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttAPIOutboundEvent FOR 
      ttAPIOutboundEvent SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttAPIOutboundEvent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttAPIOutboundEvent C-Win _FREEFORM
  QUERY ttAPIOutboundEvent NO-LOCK DISPLAY
      ttAPIOutboundEvent.retryEvent COLUMN-LABEL "[ ] All" 
            WIDTH 8 VIEW-AS TOGGLE-BOX
      ttAPIOutboundEvent.apiID COLUMN-LABEL "API ID" FORMAT "x(32)":U
            WIDTH 28 LABEL-BGCOLOR 14
      ttAPIOutboundEvent.clientID COLUMN-LABEL "Client ID" FORMAT "x(32)":U
            WIDTH 14
      ttAPIOutboundEvent.sourceTriggerID COLUMN-LABEL "Trigger ID" FORMAT "x(32)":U
            WIDTH 28
      ttAPIOutboundEvent.requestDateTime COLUMN-LABEL "Last Request Date" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 32 LABEL-BGCOLOR 14
      ttAPIOutboundEvent.success COLUMN-LABEL "Success" FORMAT "SUCCESS/FAILED":U
            WIDTH 12 LABEL-BGCOLOR 14
      ttAPIOutboundEvent.primaryID COLUMN-LABEL "Primary ID" FORMAT "x(32)":U
            WIDTH 16 LABEL-BGCOLOR 14
      ttAPIOutboundEvent.userField1 COLUMN-LABEL "User Field 1" FORMAT "x(32)":U
            WIDTH 16 LABEL-BGCOLOR 14
      ttAPIOutboundEvent.userField2 COLUMN-LABEL "User Field 2" FORMAT "x(32)":U
            WIDTH 16 LABEL-BGCOLOR 14
      ttAPIOutboundEvent.apiOutboundEventID COLUMN-LABEL "Event ID" FORMAT "9999999":U
            WIDTH 15 LABEL-BGCOLOR 14
      ENABLE ttAPIOutboundEvent.retryEvent
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 198.2 BY 22.33
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btExport AT ROW 1.71 COL 176.4 WIDGET-ID 44
     btExit AT ROW 1.71 COL 189.2 WIDGET-ID 2
     btFilter AT ROW 1.76 COL 124.4 WIDGET-ID 18
     btAPIIDLookup AT ROW 1.95 COL 116.6 WIDGET-ID 46
     fiEventIDLabel AT ROW 2 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fiEventID AT ROW 2 COL 14.4 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fiPrimaryIDLabel AT ROW 2 COL 31.8 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     fiPrimaryID AT ROW 2 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     fiAPIIDLabel AT ROW 2 COL 75 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fiAPIId AT ROW 2 COL 84.2 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     btClientIDLookup AT ROW 3.38 COL 49.2 WIDGET-ID 60
     btTriggerIDLookup AT ROW 3.38 COL 116.6 WIDGET-ID 62
     fiClientIDLabel AT ROW 3.43 COL 1.8 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fiClientID AT ROW 3.43 COL 14.4 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiTriggerIDLabel AT ROW 3.43 COL 65.2 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     fiTriggerID AT ROW 3.43 COL 78.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     btRestart AT ROW 4 COL 124.4 WIDGET-ID 26
     btBeginRequestDateCal AT ROW 4.91 COL 43.2 WIDGET-ID 40
     btEndRequestDateCal AT ROW 4.91 COL 87.2 WIDGET-ID 42
     fiBeginRequestDate AT ROW 4.95 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiEndRequestDate AT ROW 4.95 COL 70 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiBeginRequestDatelabel AT ROW 5 COL 1.8 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fiEndRequestDatelabel AT ROW 5 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     fiSuccessLabel AT ROW 5 COL 91.8 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbSuccess AT ROW 5 COL 103.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fiMessage AT ROW 5.1 COL 135.2 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     ttAPIOutboundEvent AT ROW 6.95 COL 1.8 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.14 COL 9 WIDGET-ID 30
          FONT 6
     RECT-13 AT ROW 1.48 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 199.8 BY 28.57
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
         WIDTH              = 199.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 199.8
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 199.8
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* BROWSE-TAB ttAPIOutboundEvent fiMessage DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fiAPIIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiBeginRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiClientIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEndRequestDatelabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEventIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiMessage IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPrimaryIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSuccessLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTriggerIDLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ttAPIOutboundEvent:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttAPIOutboundEvent
/* Query rebuild information for BROWSE ttAPIOutboundEvent
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAPIOutboundEvent ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE ttAPIOutboundEvent */
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


&Scoped-define SELF-NAME btClientIDLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btClientIDLookup C-Win
ON CHOOSE OF btClientIDLookup IN FRAME DEFAULT-FRAME
DO:
    APPLY "HELP" TO fiClientID.
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

    IF VALID-HANDLE(hdOutboundProcs) THEN
        DELETE PROCEDURE hdOutboundProcs.
                
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
    
    RUN Output_TempTableToCSV IN hdOutputProcs (
        INPUT TEMP-TABLE ttPrintAPIOutboundEvent:HANDLE,
        INPUT cFullFilePath,
        INPUT TRUE /* Export Header */,
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
    DEFINE VARIABLE cQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    
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

    cQuery = "FOR EACH APIOutboundEvent NO-LOCK WHERE TRUE ".
    
    IF fiEventID:SCREEN-VALUE NE "0" THEN
        cQuery = cQuery + " AND APIOutboundEvent.apiOutboundEventID EQ " + REPLACE(fiEventID:SCREEN-VALUE,",","").

    IF fiAPIID:SCREEN-VALUE NE "ALL" AND fiAPIID:SCREEN-VALUE NE "" THEN
        cQuery = cQuery 
               + " AND APIOutboundEvent.apiID EQ '" + fiAPIID:SCREEN-VALUE + "'".

    IF fiPrimaryID:SCREEN-VALUE NE "ALL" AND fiPrimaryID:SCREEN-VALUE NE "" THEN
        cQuery = cQuery
               + " AND APIOutboundEvent.primaryID EQ '" + fiPrimaryID:SCREEN-VALUE + "'".

    IF fiClientID:SCREEN-VALUE NE "ALL" AND fiClientID:SCREEN-VALUE NE "" THEN
        cQuery = cQuery
               + " AND APIOutboundEvent.clientID EQ '" + fiClientID:SCREEN-VALUE + "'".
        
    IF fiTriggerID:SCREEN-VALUE NE "ALL" AND fiTriggerID:SCREEN-VALUE NE "" THEN
        cQuery = cQuery
               + " AND APIOutboundEvent.sourceTriggerID EQ '" + fiTriggerID:SCREEN-VALUE + "'".
    
    IF fiBeginRequestDate:SCREEN-VALUE NE ?  AND
       fiBeginRequestDate:SCREEN-VALUE NE "" AND
       fiBeginRequestDate:SCREEN-VALUE NE "/  /" THEN
        cQuery = cQuery + " AND APIOutboundEvent.requestDateTime GE '" + STRING(DATETIME(DATE(fiBeginRequestDate:SCREEN-VALUE), 0)) + "'".

    IF fiEndRequestDate:SCREEN-VALUE NE ?  AND
       fiEndRequestDate:SCREEN-VALUE NE "" AND
       fiEndRequestDate:SCREEN-VALUE NE "/  /" THEN
        cQuery = cQuery + " AND APIOutboundEvent.requestDateTime LT '" + STRING(DATETIME(DATE(fiEndRequestDate:SCREEN-VALUE) + 1, 0)) + "'".
        
    IF cbSuccess:SCREEN-VALUE = "SUCCESS" THEN
        cQuery = cQuery
               + " AND APIOutboundEvent.success = TRUE".
    ELSE IF cbSuccess:SCREEN-VALUE = "FAILED" THEN
        cQuery = cQuery
               + " AND APIOutboundEvent.success = FALSE".

    
    fiMessage:SCREEN-VALUE = "Searching...".
    
    SESSION:SET-WAIT-STATE("GENERAL").
    
    hdQuery:QUERY-PREPARE(cQuery).

    ETIME(YES).
    
    hdQuery:QUERY-OPEN().
    
    DO WHILE hdQuery:GET-NEXT():
        IF APIOutboundEvent.company NE g_company THEN
            NEXT.
        
        CREATE ttAPIOutboundEvent.
        BUFFER-COPY APIOutboundEvent TO ttAPIOutboundEvent.
        ttAPIOutboundEvent.eventRowID = ROWID(APIOutboundEvent).        

        iCount = iCount + 1.
    END.   
    
    IF iCount EQ 0 THEN
        fiMessage:SCREEN-VALUE = "No records found!".
    ELSE    
        fiMessage:SCREEN-VALUE = STRING(iCount) + " Record(s) found in " + TRIM(STRING(ROUND(ETIME / 1000, 2),">>>>>>>>>>9.99")) + "s".

    RUN pUpdateUserFieldLabels (
        INPUT fiAPIID:SCREEN-VALUE
        ).
        
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

    SESSION:SET-WAIT-STATE("").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRestart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRestart C-Win
ON CHOOSE OF btRestart IN FRAME DEFAULT-FRAME /* Restart */
DO: 
    DEFINE VARIABLE lSuccess             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTotalEvents         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSuccessEvents       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFailureEvents       AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER buf_ttAPIOutboundEvent FOR ttAPIOutboundEvent.
        
    FIND FIRST buf_ttAPIOutboundEvent
         WHERE buf_ttAPIOutboundEvent.retryEvent
           AND NOT buf_ttAPIOutboundEvent.success
         NO-ERROR.
    IF NOT AVAILABLE buf_ttAPIOutboundEvent THEN
        RETURN.
        
    fiMessage:SCREEN-VALUE = "Re-triggering requests...".
    
    SESSION:SET-WAIT-STATE("GENERAL").

    FOR EACH buf_ttAPIOutboundEvent
        WHERE buf_ttAPIOutboundEvent.retryEvent
         BY buf_ttAPIOutboundEvent.requestDateTime:
        ASSIGN
            buf_ttAPIOutboundEvent.retryEvent = FALSE
            .
  
        IF buf_ttAPIOutboundEvent.success THEN
            NEXT.

        RUN Outbound_ReTrigger IN hdOutboundProcs (
            INPUT  buf_ttAPIOutboundEvent.apiOutboundEventID,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ) NO-ERROR.

        FIND FIRST APIOutboundEvent NO-LOCK
             WHERE ROWID(APIOutboundEvent) EQ buf_ttAPIOutboundEvent.eventRowID NO-ERROR.
        IF AVAILABLE APIOutboundEvent THEN
            ASSIGN
                buf_ttAPIOutboundEvent.callingProgram  = APIOutboundEvent.callingProgram
                buf_ttAPIOutboundEvent.requestDateTime = APIOutboundEvent.requestDateTime
                buf_ttAPIOutboundEvent.success         = APIOutboundEvent.success
                .
  
        iTotalEvents = iTotalEvents + 1.
  
        IF buf_ttAPIOutboundEvent.success THEN
            iSuccessEvents = iSuccessEvents + 1.
        ELSE
            iFailureEvents = iFailureEvents + 1.
    END.
       
    SESSION:SET-WAIT-STATE("").
    
    fiMessage:SCREEN-VALUE = "Process complete!".
    
    MESSAGE "Outbound Event(s) updated" SKIP
            "Total Records attempted:" iTotalEvents SKIP
            "Total Success Records:" iSuccessEvents SKIP
            "Total Failed Records:" iFailureEvents
       VIEW-AS ALERT-BOX INFORMATION.
    
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTriggerIDLookup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTriggerIDLookup C-Win
ON CHOOSE OF btTriggerIDLookup IN FRAME DEFAULT-FRAME
DO:
    APPLY "HELP" TO fiTriggerID.
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
        "", 
        "",  /* lookup field */
        142, /* Subject ID */
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


&Scoped-define SELF-NAME fiClientID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiClientID C-Win
ON HELP OF fiClientID IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        cCompany, 
        "apiID", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        fiClientId:SCREEN-VALUE = IF NUM-ENTRIES(returnFields, "|") GE 4 THEN
                                      ENTRY(4, returnFields, "|")
                                  ELSE
                                      "".
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTriggerID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTriggerID C-Win
ON HELP OF fiTriggerID IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
    RUN system/openlookup.p (
        cCompany, 
        "triggerID", /* lookup field */
        0,   /* Subject ID */
        "",  /* User ID */
        0,   /* Param value ID */
        OUTPUT returnFields, 
        OUTPUT lookupField, 
        OUTPUT recVal
        ). 

    IF lookupField NE "" THEN DO:
        ASSIGN
            fiTriggerID:SCREEN-VALUE = lookupField
            .
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttAPIOutboundEvent
&Scoped-define SELF-NAME ttAPIOutboundEvent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttAPIOutboundEvent C-Win
ON DEFAULT-ACTION OF ttAPIOutboundEvent IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttAPIOutboundEvent THEN DO:
        RUN api/ResponseDataViewer.w (
            ttAPIOutboundEvent.eventRowID
            ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttAPIOutboundEvent C-Win
ON START-SEARCH OF ttAPIOutboundEvent IN FRAME DEFAULT-FRAME
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

    FIND FIRST company NO-LOCK 
         WHERE company.company EQ cCompany
         NO-ERROR .
    IF AVAILABLE company THEN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                         + " - {&awversion}" + " - " 
                         + STRING(company.name) + " - " + cLocation.

    RUN enable_UI.

    DO iBrowseColumn = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS :
        IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "userField1" THEN
            hdUserField1Column = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).
        ELSE IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "userField2" THEN
            hdUserField2Column = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).
        ELSE IF {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn):NAME = "primaryID" THEN
            hdPrimaryIDColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iBrowseColumn).            
    END.

    /* Fetch current day's records to temp-table when launched */
    ASSIGN
        fiAPIID:SCREEN-VALUE            = "All"
        fiClientID:SCREEN-VALUE         = "All"
        fiTriggerID:SCREEN-VALUE        = "All"
        fiBeginRequestDate:SCREEN-VALUE = STRING(TODAY)
        fiEndRequestDate:SCREEN-VALUE   = STRING(TODAY)
        .
    APPLY "CHOOSE" TO btFilter.    
    
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName ttAPIOutboundEvent
{methods/sortByProc.i "pByAPIID" "ttAPIOutboundEvent.apiID"}
{methods/sortByProc.i "pByPrimaryID" "ttAPIOutboundEvent.primaryID"}
{methods/sortByProc.i "pByUserField1" "ttAPIOutboundEvent.userField1"}
{methods/sortByProc.i "pByUserField2" "ttAPIOutboundEvent.userField2"}
{methods/sortByProc.i "pByEventID" "ttAPIOutboundEvent.apiOutboundEventID"}
{methods/sortByProc.i "pBySuccess" "ttAPIOutboundEvent.success"}
{methods/sortByProc.i "pByRequestDate" "ttAPIOutboundEvent.requestDateTime"}

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
  DISPLAY fiEventIDLabel fiEventID fiPrimaryIDLabel fiPrimaryID fiAPIIDLabel 
          fiAPIId fiClientIDLabel fiClientID fiTriggerIDLabel fiTriggerID 
          fiBeginRequestDate fiEndRequestDate fiBeginRequestDatelabel 
          fiEndRequestDatelabel fiSuccessLabel cbSuccess fiMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-13 btExport btExit btFilter btAPIIDLookup fiEventID fiPrimaryID 
         fiAPIId btClientIDLookup btTriggerIDLookup fiClientID fiTriggerID 
         btRestart btBeginRequestDateCal btEndRequestDateCal fiBeginRequestDate 
         fiEndRequestDate cbSuccess ttAPIOutboundEvent 
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
PROCEDURE pReOpenBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "apiID" THEN
            RUN pByAPIID.
        WHEN "userField1" THEN
            RUN pByUserField1.
        WHEN "userField2" THEN
            RUN pByUserField2.
        WHEN "primaryID" THEN
            RUN pByPrimaryID.
        WHEN "apiOutboundEventID" THEN
            RUN pByEventID.
        WHEN "success" THEN
            RUN pBySuccess.
        WHEN "requestDateTime" THEN
            RUN pByRequestDate.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateUserFieldLabels C-Win
PROCEDURE pUpdateUserFieldLabels PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcAPIID AS CHARACTER NO-UNDO.    
    
    DEFINE VARIABLE cUserField1Label AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField2Label AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserField3Label AS CHARACTER NO-UNDO.

    ASSIGN
        cUserField1Label = "User Field 1"
        cUserField2Label = "User Field 2"
        cUserField3Label = "Primary ID"
        .
    
    CASE ipcAPIID:
        WHEN "CalculateTax" THEN
            ASSIGN
                cUserField1Label = "BOL #"
                cUserField2Label = "Order #"
                cUserField3Label = "Invoice #"
                .
    END.
    
    IF VALID-HANDLE(hdUserField1Column) THEN
        hdUserField1Column:LABEL = cUserField1Label.

    IF VALID-HANDLE(hdUserField2Column) THEN
        hdUserField2Column:LABEL = cUserField2Label.
 
    IF VALID-HANDLE(hdPrimaryIDColumn) THEN
        hdPrimaryIDColumn:LABEL = cUserField3Label.
     
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



