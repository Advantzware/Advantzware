&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: downtime.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 4.20.2004

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

{schedule/scopDir.i}
{{&includes}/defBoard.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE boardType AS CHARACTER NO-UNDO.
DEFINE VARIABLE buttonWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE containerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE correct-error AS LOGICAL NO-UNDO.
DEFINE VARIABLE currentList AS INTEGER NO-UNDO.
DEFINE VARIABLE dayName AS CHARACTER NO-UNDO EXTENT 7 INITIAL
  ['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'].
DEFINE VARIABLE downtimeModified AS LOGICAL NO-UNDO.
DEFINE VARIABLE ID AS CHARACTER NO-UNDO {{&includes}/initID.i}.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE rectWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE scenario AS CHARACTER NO-UNDO.
DEFINE VARIABLE selectWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE selectList AS WIDGET-HANDLE NO-UNDO EXTENT 56.

DEFINE TEMP-TABLE ttbl NO-UNDO
  FIELD listID AS INTEGER FORMAT 'z9'
  FIELD resource AS CHARACTER
  FIELD downtimeDate AS DATE FORMAT '99/99/9999'
  FIELD downtimeStart AS INTEGER
  FIELD downtimeEnd AS INTEGER
    INDEX ttbl IS PRIMARY UNIQUE
          listiD resource downtimeDate downtimeStart downtimeEnd.

DEFINE TEMP-TABLE dtResource NO-UNDO
  FIELD sortOrder AS INTEGER
  FIELD resource AS CHARACTER LABEL 'Resource' FORMAT 'X(20)'
    INDEX dtResource IS PRIMARY UNIQUE sortOrder resource.

DEFINE BUFFER buffTtbl FOR ttbl.
DEFINE BUFFER buffResource FOR dtResource.

PROCEDURE LockWindowUpdate EXTERNAL 'user32.dll':
  DEFINE INPUT PARAMETER intWindowHwnd AS LONG NO-UNDO.
  DEFINE RETURN PARAMETER intResult AS LONG NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME downtimeFrame
&Scoped-define BROWSE-NAME resourceBrowseSource

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dtResource buffResource ttbl

/* Definitions for BROWSE resourceBrowseSource                          */
&Scoped-define FIELDS-IN-QUERY-resourceBrowseSource dtResource.resource   
&Scoped-define ENABLED-FIELDS-IN-QUERY-resourceBrowseSource   
&Scoped-define SELF-NAME resourceBrowseSource
&Scoped-define QUERY-STRING-resourceBrowseSource FOR EACH dtResource
&Scoped-define OPEN-QUERY-resourceBrowseSource OPEN QUERY {&SELF-NAME} FOR EACH dtResource.
&Scoped-define TABLES-IN-QUERY-resourceBrowseSource dtResource
&Scoped-define FIRST-TABLE-IN-QUERY-resourceBrowseSource dtResource


/* Definitions for BROWSE resourceBrowseTarget                          */
&Scoped-define FIELDS-IN-QUERY-resourceBrowseTarget buffResource.resource   
&Scoped-define ENABLED-FIELDS-IN-QUERY-resourceBrowseTarget   
&Scoped-define SELF-NAME resourceBrowseTarget
&Scoped-define QUERY-STRING-resourceBrowseTarget FOR EACH buffResource   WHERE buffResource.resource NE dtResource.resource     AND buffResource.resource NE '<Calendar>'
&Scoped-define OPEN-QUERY-resourceBrowseTarget OPEN QUERY {&SELF-NAME} FOR EACH buffResource   WHERE buffResource.resource NE dtResource.resource     AND buffResource.resource NE '<Calendar>'.
&Scoped-define TABLES-IN-QUERY-resourceBrowseTarget buffResource
&Scoped-define FIRST-TABLE-IN-QUERY-resourceBrowseTarget buffResource


/* Definitions for BROWSE timeBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-timeBrowse dayOfWeek(ttbl.listID,ttbl.downtimeDate) calendarDate(ttbl.downtimeDate) STRING(ttbl.downtimeStart,'HH:MM am') STRING(ttbl.downtimeEnd,'HH:MM am')   
&Scoped-define ENABLED-FIELDS-IN-QUERY-timeBrowse   
&Scoped-define SELF-NAME timeBrowse
&Scoped-define QUERY-STRING-timeBrowse FOR EACH ttbl WHERE ttbl.resource EQ dtResource.resource
&Scoped-define OPEN-QUERY-timeBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttbl WHERE ttbl.resource EQ dtResource.resource.
&Scoped-define TABLES-IN-QUERY-timeBrowse ttbl
&Scoped-define FIRST-TABLE-IN-QUERY-timeBrowse ttbl


/* Definitions for FRAME downtimeFrame                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-5 btnYearDown ~
calendarYear btnYearUp calendarMonth copyToDate resources btnCopy 
&Scoped-Define DISPLAYED-OBJECTS calendarYear calendarMonth copyToDate ~
selectedDate resources startHour startMinute startAMPM endHour endMinute ~
endAMPM scenarioUsed 

/* Custom List Definitions                                              */
/* staticObjects,copyObjects,List-3,changeButtons,timeFields,timeButtons */
&Scoped-define staticObjects btnYearDown calendarYear btnYearUp ~
calendarMonth resources btnCopy 
&Scoped-define copyObjects resourceBrowseSource timeBrowse ~
resourceBrowseTarget RECT-2 btnCopyToDate copyToDate btnCalendar ~
btnSelectAll btnClearAll btnCopyDowntime btnDeleteDowntime btnExit 
&Scoped-define changeButtons btnSaveAll btnCancel 
&Scoped-define timeFields startHour startMinute startAMPM endHour endMinute ~
endAMPM 
&Scoped-define timeButtons btnSave btnRemove btnOpen 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD calendarDate sObject 
FUNCTION calendarDate RETURNS CHARACTER
  (ipDate AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dayOfWeek sObject 
FUNCTION dayOfWeek RETURNS CHARACTER
  (ipListID AS INTEGER,ipDate AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "&Cancel" 
     SIZE 9 BY 1.67 TOOLTIP "Cancel ALL Changes".

DEFINE BUTTON btnClearAll 
     LABEL "<< Clea&r All" 
     SIZE 16 BY 1.14 TOOLTIP "Clear All Downtime Selections".

DEFINE BUTTON btnCopy 
     LABEL "&Quick Copy/Delete" 
     SIZE 19 BY 1.19 TOOLTIP "Quick Copy and Delete Features".

DEFINE BUTTON btnCopyDowntime 
     LABEL ">> &Copy >>" 
     SIZE 16 BY 1.14 TOOLTIP "Copy Selected Downtime(s) to Target Resource".

DEFINE BUTTON btnCopyToDate 
     LABEL "Copy To Date" 
     SIZE 16 BY 1.14 TOOLTIP "Copy Selected Downtime(s) to Specified Date".

DEFINE BUTTON btnDeleteDowntime 
     LABEL "<< &Delete" 
     SIZE 16 BY 1.14 TOOLTIP "Delete Selected Downtime(s)".

DEFINE BUTTON btnExit 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "E&xit" 
     SIZE 16 BY 1.38 TOOLTIP "Exit and Return to Downtime Calendar Screen".

DEFINE BUTTON btnOpen 
     IMAGE-UP FILE "schedule/images/clock1.bmp":U
     LABEL "&Open" 
     SIZE 5 BY 1.14 TOOLTIP "Open Entire Day".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "&Remove" 
     SIZE 5 BY 1.14 TOOLTIP "Remove Time Entry".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "schedule/images/add.bmp":U
     LABEL "&Save" 
     SIZE 5 BY 1.14 TOOLTIP "Save Time Entry".

DEFINE BUTTON btnSaveAll 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "&Save" 
     SIZE 9 BY 1.67 TOOLTIP "Save ALL Changes".

DEFINE BUTTON btnSelectAll 
     LABEL "<< Select &All" 
     SIZE 16 BY 1.14 TOOLTIP "Select All Downtime Records".

DEFINE BUTTON btnYearDown 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "DN" 
     SIZE 4.6 BY 1 TOOLTIP "Year Down".

DEFINE BUTTON btnYearUp 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "UP" 
     SIZE 4.6 BY 1 TOOLTIP "Up Year".

DEFINE VARIABLE calendarYear AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 TOOLTIP "Calendar Year"
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE endAMPM AS CHARACTER FORMAT "X(2)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8.6 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE resources AS CHARACTER FORMAT "X(256)":U INITIAL "<Calendar>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "<Calendar>" 
     DROP-DOWN-LIST
     SIZE 19 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE startAMPM AS CHARACTER FORMAT "X(2)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "PM","AM" 
     DROP-DOWN-LIST
     SIZE 8.6 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE copyToDate AS DATE FORMAT "99.99.9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE endHour AS CHARACTER FORMAT "X(2)":U INITIAL "12" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE endMinute AS CHARACTER FORMAT "X(2)":U INITIAL "00" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE scenarioUsed AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scenario" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE selectedDate AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE startHour AS CHARACTER FORMAT "X(2)":U INITIAL "12" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE startMinute AS CHARACTER FORMAT "X(2)":U INITIAL "00" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE calendarMonth AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "January", 1,
"February", 2,
"March", 3,
"April", 4,
"May", 5,
"June", 6,
"July", 7,
"August", 8,
"September", 9,
"October", 10,
"November", 11,
"December", 12
     SIZE 19 BY 12.62
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 0    
     SIZE 21 BY 26.76
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 18 BY 10.24
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 21 BY 1.67
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 18 BY 4.05
     BGCOLOR 14 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY resourceBrowseSource FOR 
      dtResource SCROLLING.

DEFINE QUERY resourceBrowseTarget FOR 
      buffResource SCROLLING.

DEFINE QUERY timeBrowse FOR 
      ttbl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE resourceBrowseSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS resourceBrowseSource sObject _FREEFORM
  QUERY resourceBrowseSource DISPLAY
      dtResource.resource
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 26.76
         TITLE "Source Resources" ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE resourceBrowseTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS resourceBrowseTarget sObject _FREEFORM
  QUERY resourceBrowseTarget DISPLAY
      buffResource.resource
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 26.76
         TITLE "Target Resources" ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE timeBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS timeBrowse sObject _FREEFORM
  QUERY timeBrowse DISPLAY
      dayOfWeek(ttbl.listID,ttbl.downtimeDate) LABEL 'Day' FORMAT 'X(13)'
  calendarDate(ttbl.downtimeDate) LABEL 'Date' FORMAT 'X(12)'
  STRING(ttbl.downtimeStart,'HH:MM am') LABEL 'Start Time'
  STRING(ttbl.downtimeEnd,'HH:MM am') LABEL 'End Time'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 52 BY 26.76
         TITLE "Resource Downtime" ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME downtimeFrame
     resourceBrowseSource AT ROW 1 COL 23
     timeBrowse AT ROW 1 COL 56
     resourceBrowseTarget AT ROW 1 COL 126
     btnYearDown AT ROW 1.71 COL 2
     calendarYear AT ROW 1.71 COL 6 NO-LABEL
     btnYearUp AT ROW 1.71 COL 17
     btnCopyToDate AT ROW 2.19 COL 109
     calendarMonth AT ROW 2.91 COL 2 HELP
          "Select Calendar Month" NO-LABEL
     copyToDate AT ROW 3.62 COL 107 COLON-ALIGNED NO-LABEL
     btnCalendar AT ROW 4.81 COL 114 HELP
          "Click to Access Popup Calendar"
     btnSelectAll AT ROW 6.48 COL 109
     btnClearAll AT ROW 7.91 COL 109
     btnCopyDowntime AT ROW 10.29 COL 109
     btnDeleteDowntime AT ROW 12.67 COL 109
     btnExit AT ROW 14.81 COL 109
     selectedDate AT ROW 15.76 COL 2 NO-LABEL
     resources AT ROW 16.95 COL 2 HELP
          "Select Resource" NO-LABEL
     btnCopy AT ROW 18.38 COL 2 HELP
          "Click to Enable Copy Function"
     btnSaveAll AT ROW 20.05 COL 2 HELP
          "Click to Save ALL Changes"
     btnCancel AT ROW 20.05 COL 12 HELP
          "Click to Cancel ALL Changes"
     startHour AT ROW 20.05 COL 108 HELP
          "Enter Starting Hour" NO-LABEL
     startMinute AT ROW 20.05 COL 111 COLON-ALIGNED HELP
          "Enter Starting Minute" NO-LABEL
     startAMPM AT ROW 20.05 COL 116 COLON-ALIGNED NO-LABEL
     endHour AT ROW 21 COL 108 HELP
          "Enter Ending Hour" NO-LABEL
     endMinute AT ROW 21 COL 111 COLON-ALIGNED HELP
          "Enter Ending Minute" NO-LABEL
     endAMPM AT ROW 21 COL 116 COLON-ALIGNED NO-LABEL
     btnSave AT ROW 21.95 COL 111 HELP
          "Click to Save Time Entry"
     btnRemove AT ROW 21.95 COL 116 HELP
          "Click to Remove Time Entry"
     btnOpen AT ROW 21.95 COL 121 HELP
          "Click to Remove Entires and Open Entire Day"
     scenarioUsed AT ROW 1 COL 1.4
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 6.24 COL 108
     RECT-5 AT ROW 18.14 COL 1
     RECT-8 AT ROW 1.95 COL 108
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 26.76
         WIDTH              = 157.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME downtimeFrame
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB resourceBrowseSource 1 downtimeFrame */
/* BROWSE-TAB timeBrowse resourceBrowseSource downtimeFrame */
/* BROWSE-TAB resourceBrowseTarget timeBrowse downtimeFrame */
ASSIGN 
       FRAME downtimeFrame:HIDDEN           = TRUE
       FRAME downtimeFrame:HEIGHT           = 26.76
       FRAME downtimeFrame:WIDTH            = 157.4.

/* SETTINGS FOR BUTTON btnCalendar IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnCalendar:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME downtimeFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       btnCancel:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnClearAll IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnClearAll:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnCopy IN FRAME downtimeFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnCopyDowntime IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnCopyDowntime:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnCopyToDate IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnCopyToDate:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnDeleteDowntime IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnDeleteDowntime:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnExit IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnExit:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnOpen IN FRAME downtimeFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       btnOpen:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnRemove IN FRAME downtimeFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       btnRemove:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnSave IN FRAME downtimeFrame
   NO-ENABLE 6                                                          */
ASSIGN 
       btnSave:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnSaveAll IN FRAME downtimeFrame
   NO-ENABLE 4                                                          */
ASSIGN 
       btnSaveAll:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnSelectAll IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       btnSelectAll:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BUTTON btnYearDown IN FRAME downtimeFrame
   1                                                                    */
/* SETTINGS FOR BUTTON btnYearUp IN FRAME downtimeFrame
   1                                                                    */
/* SETTINGS FOR RADIO-SET calendarMonth IN FRAME downtimeFrame
   1                                                                    */
/* SETTINGS FOR COMBO-BOX calendarYear IN FRAME downtimeFrame
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN copyToDate IN FRAME downtimeFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX endAMPM IN FRAME downtimeFrame
   NO-ENABLE 5                                                          */
ASSIGN 
       endAMPM:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR FILL-IN endHour IN FRAME downtimeFrame
   NO-ENABLE ALIGN-L 5                                                  */
ASSIGN 
       endHour:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR FILL-IN endMinute IN FRAME downtimeFrame
   NO-ENABLE 5                                                          */
ASSIGN 
       endMinute:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME downtimeFrame
   2                                                                    */
ASSIGN 
       RECT-2:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME downtimeFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE resourceBrowseSource IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       resourceBrowseSource:HIDDEN  IN FRAME downtimeFrame                = TRUE.

/* SETTINGS FOR BROWSE resourceBrowseTarget IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       resourceBrowseTarget:HIDDEN  IN FRAME downtimeFrame                = TRUE.

/* SETTINGS FOR COMBO-BOX resources IN FRAME downtimeFrame
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN scenarioUsed IN FRAME downtimeFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN selectedDate IN FRAME downtimeFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR COMBO-BOX startAMPM IN FRAME downtimeFrame
   NO-ENABLE 5                                                          */
ASSIGN 
       startAMPM:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR FILL-IN startHour IN FRAME downtimeFrame
   NO-ENABLE ALIGN-L 5                                                  */
ASSIGN 
       startHour:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR FILL-IN startMinute IN FRAME downtimeFrame
   NO-ENABLE 5                                                          */
ASSIGN 
       startMinute:HIDDEN IN FRAME downtimeFrame           = TRUE.

/* SETTINGS FOR BROWSE timeBrowse IN FRAME downtimeFrame
   NO-ENABLE 2                                                          */
ASSIGN 
       timeBrowse:HIDDEN  IN FRAME downtimeFrame                = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME downtimeFrame
/* Query rebuild information for FRAME downtimeFrame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME downtimeFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE resourceBrowseSource
/* Query rebuild information for BROWSE resourceBrowseSource
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dtResource.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE resourceBrowseSource */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE resourceBrowseTarget
/* Query rebuild information for BROWSE resourceBrowseTarget
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH buffResource
  WHERE buffResource.resource NE dtResource.resource
    AND buffResource.resource NE '<Calendar>'.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE resourceBrowseTarget */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE timeBrowse
/* Query rebuild information for BROWSE timeBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttbl WHERE ttbl.resource EQ dtResource.resource.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE timeBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnCalendar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar sObject
ON CHOOSE OF btnCalendar IN FRAME downtimeFrame
DO:
  APPLY 'HELP' TO copyToDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel sObject
ON CHOOSE OF btnCancel IN FRAME downtimeFrame /* Cancel */
DO:
  downtimeModified = NO.
  HIDE {&changeButtons} IN FRAME {&FRAME-NAME}.
  ENABLE btnCopy WITH FRAME {&FRAME-NAME}.
  RUN getDownTimes.
  APPLY 'VALUE-CHANGED' TO calendarMonth.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearAll sObject
ON CHOOSE OF btnClearAll IN FRAME downtimeFrame /* << Clear All */
DO:
  IF timeBrowse:NUM-SELECTED-ROWS NE 0 THEN
  ldummy = timeBrowse:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy sObject
ON CHOOSE OF btnCopy IN FRAME downtimeFrame /* Quick Copy/Delete */
DO:
  IF boardType EQ '{&Board}' THEN
  RUN copyFunction (YES).
  ELSE
  MESSAGE 'This Feature Not Available' SKIP
          'in Scheduler' boardType VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyDowntime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyDowntime sObject
ON CHOOSE OF btnCopyDowntime IN FRAME downtimeFrame /* >> Copy >> */
DO:
  RUN copyDowntime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyToDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyToDate sObject
ON CHOOSE OF btnCopyToDate IN FRAME downtimeFrame /* Copy To Date */
DO:
  RUN copyToDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteDowntime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteDowntime sObject
ON CHOOSE OF btnDeleteDowntime IN FRAME downtimeFrame /* << Delete */
DO:
  RUN deleteDowntime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit sObject
ON CHOOSE OF btnExit IN FRAME downtimeFrame /* Exit */
DO:
  RUN copyFunction (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpen sObject
ON CHOOSE OF btnOpen IN FRAME downtimeFrame /* Open */
DO:
  IF selectList[currentList]:NUM-ITEMS NE 0 THEN
  DO:
    MESSAGE 'Remove Entries and Open Entire Day?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE openDay AS LOGICAL.
    IF NOT openDay THEN RETURN NO-APPLY.
  END.
  DO idx = 1 TO selectList[currentList]:NUM-ITEMS:
    FIND ttbl EXCLUSIVE-LOCK
         WHERE ROWID(ttbl) EQ TO-ROWID(ENTRY(3,selectList[currentList]:ENTRY(idx),'|')).
    DELETE ttbl.
  END.
  ASSIGN
    selectList[currentList]:LIST-ITEM-PAIRS = ?
    startHour:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '12'
    startMinute:SCREEN-VALUE = '00'
    startAMPM:SCREEN-VALUE = 'PM'
    endHour:SCREEN-VALUE = '12'
    endMinute:SCREEN-VALUE = '00'
    endAMPM:SCREEN-VALUE = 'PM'.
  APPLY 'CHOOSE':U TO btnSave.
  ENABLE {&changeButtons} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove sObject
ON CHOOSE OF btnRemove IN FRAME downtimeFrame /* Remove */
DO:
  IF currentList EQ 0 THEN
  RETURN NO-APPLY.
  DO idx = 1 TO selectList[currentList]:NUM-ITEMS:
    IF NOT selectList[currentList]:IS-SELECTED(idx) THEN NEXT.
    MESSAGE 'Delete Selected Entry?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE removeEntry AS LOGICAL.
    IF NOT removeEntry THEN
    RETURN NO-APPLY.
    FIND ttbl EXCLUSIVE-LOCK
         WHERE ROWID(ttbl) EQ TO-ROWID(ENTRY(3,selectList[currentList]:SCREEN-VALUE,'|')).
    DELETE ttbl.
    ldummy = selectList[currentList]:DELETE(idx).
    ENABLE {&changeButtons} WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME downtimeFrame /* Save */
DO:
  IF currentList EQ 0 THEN
  RETURN NO-APPLY.
  ASSIGN {&timeFields}.
  RUN addListItem.
  ENABLE {&changeButtons} WITH FRAME {&FRAME-NAME}.
  DISABLE btnCopy WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveAll sObject
ON CHOOSE OF btnSaveAll IN FRAME downtimeFrame /* Save */
DO:
  HIDE {&changeButtons} IN FRAME {&FRAME-NAME}.
  IF boardType EQ '{&Board}' THEN
  DO:
    downtimeModified = NO.
    ENABLE btnCopy WITH FRAME {&FRAME-NAME}.
    RUN saveDowntime.
    RUN saveBoard IN containerHandle.
    RUN loadDowntime IN containerHandle.
  END.
  ELSE
  MESSAGE 'This Feature Not Available' SKIP
          'in Scheduler' boardType VIEW-AS ALERT-BOX.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll sObject
ON CHOOSE OF btnSelectAll IN FRAME downtimeFrame /* << Select All */
DO:
  IF CAN-FIND(FIRST ttbl WHERE ttbl.resource EQ dtResource.resource) THEN
  ldummy = timeBrowse:SELECT-ALL().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnYearDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnYearDown sObject
ON CHOOSE OF btnYearDown IN FRAME downtimeFrame /* DN */
DO:
  ASSIGN
    idx = calendarYear:LOOKUP(calendarYear:SCREEN-VALUE)
    idx = idx + IF idx NE calendarYear:NUM-ITEMS THEN 1 ELSE 0
    calendarYear:SCREEN-VALUE = calendarYear:ENTRY(idx).
  APPLY 'VALUE-CHANGED' TO calendarYear.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnYearUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnYearUp sObject
ON CHOOSE OF btnYearUp IN FRAME downtimeFrame /* UP */
DO:
  ASSIGN
    idx = calendarYear:LOOKUP(calendarYear:SCREEN-VALUE)
    idx = idx - IF idx NE 1 THEN 1 ELSE 0
    calendarYear:SCREEN-VALUE = calendarYear:ENTRY(idx).
  APPLY 'VALUE-CHANGED' TO calendarYear.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME calendarMonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calendarMonth sObject
ON VALUE-CHANGED OF calendarMonth IN FRAME downtimeFrame
DO:
  ASSIGN {&SELF-NAME}.
  APPLY 'VALUE-CHANGED' TO resources.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME calendarYear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL calendarYear sObject
ON VALUE-CHANGED OF calendarYear IN FRAME downtimeFrame
DO:
  ASSIGN {&SELF-NAME}.
  APPLY 'VALUE-CHANGED' TO calendarMonth.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME copyToDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL copyToDate sObject
ON HELP OF copyToDate IN FRAME downtimeFrame
DO:
  {{&includes}/calendar.i}
  APPLY 'LEAVE' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL copyToDate sObject
ON LEAVE OF copyToDate IN FRAME downtimeFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME endHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL endHour sObject
ON LEAVE OF endHour IN FRAME downtimeFrame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {{&includes}/{&Board}/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME endMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL endMinute sObject
ON LEAVE OF endMinute IN FRAME downtimeFrame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/{&Board}/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME resourceBrowseSource
&Scoped-define SELF-NAME resourceBrowseSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resourceBrowseSource sObject
ON VALUE-CHANGED OF resourceBrowseSource IN FRAME downtimeFrame /* Source Resources */
DO:
  timeBrowse:TITLE = 'Downtimes for ' + dtResource.resource.
  {&OPEN-QUERY-timeBrowse}
  {&OPEN-QUERY-resourceBrowseTarget}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resources sObject
ON VALUE-CHANGED OF resources IN FRAME downtimeFrame
DO:
  ASSIGN {&SELF-NAME}
    selectedDate:SCREEN-VALUE = ''
    currentList = 0.
  HIDE {&timeFields} {&timeButtons} IN FRAME {&FRAME-NAME}.
  RUN setDays.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME startHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL startHour sObject
ON LEAVE OF startHour IN FRAME downtimeFrame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 12.
  {{&includes}/{&Board}/entryerr.i &error-message="Invalid Hour, range = 1 to 12"}
  IF INTEGER(SELF:SCREEN-VALUE) EQ 0 THEN
  ASSIGN
    startMinute:SCREEN-VALUE = '00'
    endHour:SCREEN-VALUE = '0'
    endMinute:SCREEN-VALUE = '00'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME startMinute
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL startMinute sObject
ON LEAVE OF startMinute IN FRAME downtimeFrame
DO:
  correct-error = INTEGER(SELF:SCREEN-VALUE) LT 0 OR INTEGER(SELF:SCREEN-VALUE) GT 59.
  {{&includes}/{&Board}/entryerr.i &error-message="Invalid Minute, range = 0 to 59"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addListItem sObject 
PROCEDURE addListItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE startTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE startStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE endTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE endStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE timeStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE intStr AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    {{&includes}/{&Board}/setTime.i &hour=startHour &minute=startMinute &ampm=startAMPM &field=startTime}
    {{&includes}/{&Board}/setTime.i &hour=endHour &minute=endMinute &ampm=endAMPM &field=endTime}
    {{&includes}/{&Board}/getTime.i &hour=startHour &minute=startMinute &ampm=startAMPM &field=startTime}
    {{&includes}/{&Board}/getTime.i &hour=endHour &minute=endMinute &ampm=endAMPM &field=endTime}
    DISPLAY startHour startMinute startAMPM endHour endMinute endAMPM.
    IF startTime GT endTime THEN
    DO:
      MESSAGE 'Invalid Time Entered, Start Time can not be Greater then End Time.' VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END.
    ELSE
    IF CAN-FIND(ttbl WHERE ttbl.listID EQ currentList
                       AND ttbl.resource EQ (IF currentList LE 7 THEN resources:ENTRY(1) ELSE resources)
                       AND ttbl.downtimeDate EQ (IF currentList LE 14 THEN ? ELSE DATE(selectedDate))
                       AND ttbl.downtimeStart EQ startTime
                       AND ttbl.downtimeEnd EQ endTime) THEN
    DO:
      MESSAGE 'Downtime Already Exists!' VIEW-AS ALERT-BOX WARNING.
      RETURN.
    END.
    CREATE ttbl.
    ASSIGN
      ttbl.listID = currentList
      ttbl.resource = IF currentList LE 7 THEN resources:ENTRY(1) ELSE resources
      ttbl.downtimeDate = IF currentList LE 14 THEN ? ELSE DATE(selectedDate)
      ttbl.downtimeStart = startTime
      ttbl.downtimeEnd = endTime
      startStr = startHour + ':' + startMinute + ' ' + startAMPM
      endStr = endHour + ':' + endMinute + ' ' + endAMPM
      timeStr = startStr + ' - ' + endStr
      intStr = STRING(startTime,'99999') + '|' + STRING(endTime,'99999') + '|' + STRING(ROWID(ttbl)).
      ldummy = selectList[currentList]:ADD-LAST(timeStr,intStr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE containerHandle sObject 
PROCEDURE containerHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER ipBoardType AS CHARACTER NO-UNDO.

  ASSIGN
    containerHandle = ipHandle
    boardType = ipBoardType.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyDowntime sObject 
PROCEDURE copyDowntime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF timeBrowse:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
    MESSAGE 'Copy "' + dtResource.resource + '" Selected Downtime(s) to "' +
                       buffResource.resource + '"?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE doCopy AS LOGICAL.
    IF NOT doCopy THEN RETURN.
    DO i = 1 TO timeBrowse:NUM-SELECTED-ROWS:
      ldummy = timeBrowse:FETCH-SELECTED-ROW(i).
      IF CAN-FIND(buffTtbl WHERE buffTtbl.listiD EQ ttbl.listiD
                             AND buffTtbl.resource EQ buffResource.resource
                             AND buffTtbl.downtimeDate EQ ttbl.downtimeDate
                             AND buffTtbl.downtimeStart EQ ttbl.downtimeStart
                             AND buffTtbl.downtimeEnd EQ ttbl.downtimeEnd) THEN
      NEXT.
      CREATE buffTtbl.
      BUFFER-COPY ttbl EXCEPT ttbl.listID ttbl.resource TO buffTtbl
        ASSIGN
          buffTtbl.listID = ttbl.listID  + (IF ttbl.listID LT 8 THEN 7 ELSE 0)
          buffTtbl.resource = buffResource.resource.
    END.
    MESSAGE 'Downtime Copy Complete!' VIEW-AS ALERT-BOX.
    downtimeModified = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyFunction sObject 
PROCEDURE copyFunction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHidden AS LOGICAL NO-UNDO.

  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN lockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  ASSIGN
    currentWidget = FRAME {&FRAME-NAME}:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD.
  DO WHILE currentWidget NE ?:
    IF currentWidget:PRIVATE-DATA NE ? THEN
    currentWidget:HIDDEN = ipHidden.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  IF ipHidden THEN /* show copy screen */
  DO WITH FRAME {&FRAME-NAME}:
    HIDE {&timeFields} {&timeButtons} {&changeButtons}.
    ENABLE {&copyObjects}.
    DISABLE {&staticObjects}.
    {&OPEN-QUERY-resourceBrowseSource}
    APPLY 'VALUE-CHANGED' TO resourceBrowseSource.
  END.
  ELSE /* show calendar screen */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&copyObjects}.
    HIDE {&copyObjects}.
    ENABLE {&staticObjects}.
    IF downtimeModified THEN
    ENABLE {&changeButtons}.
    APPLY 'VALUE-CHANGED' TO resources.
  END.
  RUN lockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyToDate sObject 
PROCEDURE copyToDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE downtimeID AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF timeBrowse:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
    MESSAGE 'Copy "' + dtResource.resource + '" Selected Downtime(s) to "' +
                       STRING(copyToDate,'99.99.9999') + '"?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE doCopy AS LOGICAL.
    IF NOT doCopy THEN RETURN.
    downtimeID = 13 + WEEKDAY(DATE(MONTH(copyToDate),1,YEAR(copyToDate))) + DAY(copyToDate).
    DO i = 1 TO timeBrowse:NUM-SELECTED-ROWS:
      ldummy = timeBrowse:FETCH-SELECTED-ROW(i).
      IF ttbl.downtimeDate EQ ? THEN NEXT.
      IF CAN-FIND(buffTtbl WHERE buffTtbl.listID EQ downtimeID
                             AND buffTtbl.resource EQ buffTtbl.resource
                             AND buffTtbl.downtimeDate EQ copyToDate
                             AND buffTtbl.downtimeStart EQ ttbl.downtimeStart
                             AND buffTtbl.downtimeEnd EQ ttbl.downtimeEnd) THEN
      NEXT.
      CREATE buffTtbl.
      BUFFER-COPY ttbl EXCEPT ttbl.listID ttbl.downtimeDate TO buffTtbl
        ASSIGN 
          buffTtbl.listID = downtimeID
          buffTtbl.downtimeDate = copyToDate.
    END.
    APPLY 'VALUE-CHANGED':U TO resourceBrowseSource.
    MESSAGE 'Downtime Copy To Date Complete!' VIEW-AS ALERT-BOX.
    downtimeModified = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObjects sObject 
PROCEDURE createObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE buttonType AS CHARACTER NO-UNDO EXTENT 2 INITIAL ['Calendar','Resource'].
  DEFINE VARIABLE xCoord AS INTEGER NO-UNDO INITIAL 107.
  DEFINE VARIABLE yCoord AS INTEGER NO-UNDO INITIAL 2.
  DEFINE VARIABLE rectWidth AS INTEGER NO-UNDO.
  DEFINE VARIABLE rectHeight AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE d AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DELETE WIDGET-POOL 'rectPool' NO-ERROR.
  DELETE WIDGET-POOL 'buttonPool' NO-ERROR.
  DELETE WIDGET-POOL 'selectPool' NO-ERROR.
  CREATE WIDGET-POOL 'rectPool' PERSISTENT.
  CREATE WIDGET-POOL 'buttonPool' PERSISTENT.
  CREATE WIDGET-POOL 'selectPool' PERSISTENT.
  
  ASSIGN
    rectWidth = INTEGER((FRAME {&FRAME-NAME}:WIDTH-PIXELS - xCoord) / 7)
    rectHeight = INTEGER((FRAME {&FRAME-NAME}:HEIGHT-PIXELS - yCoord) / 8).
  DO WHILE rectWidth * 7 GT FRAME {&FRAME-NAME}:WIDTH-PIXELS - xCoord:
    rectWidth = rectWidth - 1.
  END.
  DO WHILE rectHeight * 8 GT FRAME {&FRAME-NAME}:HEIGHT-PIXELS - yCoord:
    rectHeight = rectHeight - 1.
  END.

  IF rectWidth LE 100 THEN
  ASSIGN
    buttonType[1] = SUBSTR(buttonType[1],1,3)
    buttonType[2] = SUBSTR(buttonType[2],1,3).
  DO i = 1 TO 8:
    DO j = 1 TO 7:
      ASSIGN
        idx = (i - 1) * 7 + j
        d = idx - 14.
      IF d LT 1 THEN d = 0.
      
      CREATE RECTANGLE rectWidget IN WIDGET-POOL 'rectPool'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          X = xCoord
          Y = yCoord
          WIDTH-PIXELS = rectWidth
          HEIGHT-PIXELS = rectHeight
          BGCOLOR = IF i EQ 1 AND d EQ 0 THEN 11
               ELSE IF i EQ 2 AND d EQ 0 THEN 9
               ELSE ?
          FILLED = YES
          EDGE-PIXELS = 1
          HIDDEN = NO
          SENSITIVE = YES
          PRIVATE-DATA = STRING(d,'zz').
      
      CREATE BUTTON buttonWidget IN WIDGET-POOL 'buttonPool'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          FONT = 6
          X = xCoord + 3
          Y = yCoord + 3
          WIDTH-PIXELS = rectWidth - 6
          HEIGHT-PIXELS = 21
          LABEL = IF i LE 2 THEN buttonType[i] + ' ' + dayName[j] ELSE STRING(d,'zz')
          HIDDEN = NO
          SENSITIVE = YES
          PRIVATE-DATA = STRING(d,'zz')
      TRIGGERS:
          ON CHOOSE
             PERSISTENT RUN showSelectedDate IN THIS-PROCEDURE (buttonWidget:HANDLE,idx).
      END TRIGGERS.
      
      CREATE SELECTION-LIST selectWidget IN WIDGET-POOL 'selectPool'
        ASSIGN
          FRAME = FRAME {&FRAME-NAME}:HANDLE
          SCROLLBAR-VERTICAL = YES
          LIST-ITEM-PAIRS = ','
          X = xCoord + 3
          Y = yCoord + 5 + buttonWidget:HEIGHT-PIXELS
          WIDTH-PIXELS = rectWidth - 6
          HEIGHT-PIXELS = rectHeight - buttonWidget:HEIGHT-PIXELS - 9
          HIDDEN = NO
          SENSITIVE = YES
          PRIVATE-DATA = STRING(d,'zz').
      ASSIGN
        selectList[idx] = selectWidget:HANDLE
        ldummy = selectWidget:DELETE(1)
        ldummy = rectWidget:MOVE-TO-TOP()
        ldummy = buttonWidget:MOVE-TO-TOP()
        ldummy = selectWidget:MOVE-TO-TOP()
        xCoord = xCoord + rectWidget:WIDTH-PIXELS.
    END.
    ASSIGN
      yCoord = yCoord + rectWidget:HEIGHT-PIXELS
      xCoord = 107.
  END.
  APPLY 'VALUE-CHANGED' TO calendarMonth IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDowntime sObject 
PROCEDURE deleteDowntime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF timeBrowse:NUM-SELECTED-ROWS EQ 0 THEN RETURN.
    MESSAGE 'Delete "' + dtResource.resource + '" Selected Downtime(s)?' VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE doDelete AS LOGICAL.
    IF NOT doDelete THEN RETURN.
    DO i = 1 TO timeBrowse:NUM-SELECTED-ROWS:
      ldummy = timeBrowse:FETCH-SELECTED-ROW(i).
      DELETE ttbl.
    END.
    APPLY 'VALUE-CHANGED' TO resourceBrowseSource.
    MESSAGE 'Downtime Delete Complete!' VIEW-AS ALERT-BOX.
    downtimeModified = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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
  HIDE FRAME downtimeFrame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDownTimes sObject 
PROCEDURE getDownTimes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE ttbl.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/downtimes.' + scenario + '.dat')) NO-ECHO.
  REPEAT:
    CREATE ttbl.
    IMPORT ttbl.
  END.
  INPUT CLOSE.
  IF ttbl.resource EQ '' THEN
  DELETE ttbl.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getResources sObject 
PROCEDURE getResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  {{&includes}/getResources.i}
  EMPTY TEMP-TABLE dtResource.
  DO i = 1 TO resources:NUM-ITEMS:
    CREATE dtResource.
    ASSIGN
      dtResource.sortOrder = i
      dtResource.resource = resources:ENTRY(i).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ID sObject 
PROCEDURE ID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/id.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCalendar sObject 
PROCEDURE initCalendar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN ID.
  RUN getResources.
  RUN getDownTimes.
  RUN createObjects.
  scenarioUsed:SCREEN-VALUE IN FRAME {&FRAME-NAME} = scenario.
  APPLY 'VALUE-CHANGED' TO calendarMonth.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveDowntime sObject 
PROCEDURE saveDowntime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDowntimeFile AS CHARACTER NO-UNDO EXTENT 2.
  
  cDowntimeFile[1] = SEARCH('{&data}/' + ID + '/downtimes.' + scenario + '.dat'). 
  OUTPUT TO VALUE(cDowntimeFile[1]).
  FOR EACH ttbl NO-LOCK:
    EXPORT ttbl.
  END.
  OUTPUT CLOSE.
  /* place copy of downtime in start in directory for use by */
  /* Schedule Capacity Page Generation                       */
  IF scenario EQ 'Actual' THEN DO:
      ASSIGN 
        cDowntimeFile[2] = SEARCH('schedule/load.log')
        cDowntimeFile[2] = REPLACE(cDowntimeFile[2],'load.log','downtimes.'
                         + REPLACE(ID,'/','.')
                         + '.dat')
                         .
      OS-COPY VALUE(cDowntimeFile[1]) VALUE(cDowntimeFile[2]).
  END. /* if actual */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDays sObject 
PROCEDURE setDays :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE firstDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE lastDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE sDate AS DATE NO-UNDO.
  DEFINE VARIABLE dDate AS DATE NO-UNDO.
  DEFINE VARIABLE days AS CHARACTER NO-UNDO EXTENT 42.
  DEFINE VARIABLE iDate AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE startStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE endStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE timeStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE intStr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE yr AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    sDate = DATE(calendarMonth,1,calendarYear).
  END.
  dDate = sDate.
  DO WHILE TRUE:
    dDate = dDate + 1.
    IF MONTH(dDate) NE MONTH(sDate) THEN
    LEAVE.
  END.
  ASSIGN
    dDate = dDate - 1
    firstDay = WEEKDAY(sDate)
    lastDay = DAY(dDate)
    days = ''.
  IF iDate GT lastDay THEN
  iDate = lastDay.
  DO i = firstDay TO firstDay + lastDay - 1:
    ASSIGN
      j = j + 1
      days[i] = STRING(j).
  END.
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  ASSIGN
    currentWidget = FRAME {&FRAME-NAME}:HANDLE
    currentWidget = currentWidget:FIRST-CHILD
    currentWidget = currentWidget:FIRST-CHILD.
  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'RECTANGLE' AND
       currentWidget:PRIVATE-DATA NE '' AND
       currentWidget:PRIVATE-DATA NE ? THEN
    currentWidget:BGCOLOR =
         IF days[INTEGER(currentWidget:PRIVATE-DATA)] EQ '' THEN 0 ELSE
         IF INTEGER(currentWidget:PRIVATE-DATA) MOD 7 GT 1 THEN 14 ELSE 12.
    ELSE
    IF currentWidget:TYPE EQ 'BUTTON' AND
       currentWidget:PRIVATE-DATA NE '' AND
       currentWidget:PRIVATE-DATA NE ? THEN
    DO:
      ASSIGN
        currentWidget:LABEL = days[INTEGER(currentWidget:PRIVATE-DATA)]
        currentWidget:HIDDEN = NO
        currentWidget:SENSITIVE = YES.
      IF currentWidget:LABEL EQ '' THEN
      currentWidget:HIDDEN = YES.
    END.
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
  /* populate selection lists */
  DO i = 1 TO EXTENT(selectList):
    IF NOT VALID-HANDLE(selectList[i]) THEN NEXT.
    selectList[i]:LIST-ITEM-PAIRS = ?.
    IF selectList[i]:PRIVATE-DATA NE '' AND
       selectList[i]:PRIVATE-DATA NE ? THEN
    ASSIGN
      selectList[i]:HIDDEN = days[INTEGER(selectList[i]:PRIVATE-DATA)] EQ ''
      selectList[i]:SENSITIVE = days[INTEGER(selectList[i]:PRIVATE-DATA)] NE ''
      yr = calendarYear.
    FOR EACH ttbl NO-LOCK
        WHERE (ttbl.listID EQ i AND i LE 7)
           OR (ttbl.listID EQ i AND i LE 14 AND ttbl.resource EQ resources)
           OR (ttbl.listID EQ i AND ttbl.resource EQ resources
          AND  MONTH(ttbl.downtimeDate) EQ calendarMonth
          AND    DAY(ttbl.downtimeDate) EQ INTEGER(days[INTEGER(selectList[i]:PRIVATE-DATA)])
          AND   YEAR(ttbl.downtimeDate) EQ yr):
      {{&includes}/{&Board}/getTime.i &hour=startHour &minute=startMinute &ampm=startAMPM &field=ttbl.downtimeStart}
      {{&includes}/{&Board}/getTime.i &hour=endHour &minute=endMinute &ampm=endAMPM &field=ttbl.downtimeEnd}
      ASSIGN
        startStr = startHour + ':' + startMinute + ' ' + startAMPM
        endStr = endHour + ':' + endMinute + ' ' + endAMPM
        timeStr = startStr + ' - ' + endStr
        intStr = STRING(ttbl.downtimeStart,'99999') + '|' + STRING(ttbl.downtimeEnd,'99999') + '|' + STRING(ROWID(ttbl)).
        ldummy = selectList[i]:ADD-LAST(timeStr,intStr).
    END. /* each ttbl */
  END.
  RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSize sObject 
PROCEDURE setSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipHeight AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS DECIMAL NO-UNDO.

  DEFINE VARIABLE cHandle AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    calendarYear:LIST-ITEMS = ''.
    DO i = 2099 TO 1980 BY -1:
      ldummy = calendarYear:ADD-LAST(STRING(i,'9999')).
    END.
    ASSIGN
      FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ipHeight - 39
      FRAME {&FRAME-NAME}:WIDTH-PIXELS = ipWidth - 13
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = ipHeight - 39
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = ipWidth - 13
      RECT-1:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
      resourceBrowseSource:HEIGHT-PIXELS = ipHeight - 39
      timeBrowse:HEIGHT-PIXELS = ipHeight - 39
      resourceBrowseTarget:HEIGHT-PIXELS = ipHeight - 39
      calendarMonth = MONTH(TODAY)
      calendarYear = YEAR(TODAY)
      calendarYear:INNER-LINES = calendarYear:NUM-ITEMS.
    DISPLAY {&staticObjects}.
    RUN initCalendar.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showSelectedDate sObject 
PROCEDURE showSelectedDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipButton AS WIDGET NO-UNDO.
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF resources BEGINS '<' AND ipButton:LABEL BEGINS 'Resource' THEN
    RETURN.
    ASSIGN
      startAMPM:X = ipButton:X - startAMPM:WIDTH-PIXELS - 5
      startAMPM:Y = ipButton:Y + btnSave:HEIGHT-PIXELS + 5
      startMinute:X = startAMPM:X - startMinute:WIDTH-PIXELS
      startMinute:Y = startAMPM:Y
      startHour:X = startMinute:X - startHour:WIDTH-PIXELS
      startHour:Y = startMinute:Y
      endAMPM:X = startAMPM:X
      endAMPM:Y = startAMPM:Y + startAMPM:HEIGHT-PIXELS
      endMinute:X = startMinute:X 
      endMinute:Y = endAMPM:Y
      endHour:X = startHour:X
      endHour:Y = endMinute:Y
      btnSave:X = ipButton:X
      btnSave:Y = ipButton:Y
      btnRemove:X = ipButton:X + btnSave:WIDTH-PIXELS
      btnRemove:Y = ipButton:Y
      btnOpen:X = ipButton:X + btnSave:WIDTH-PIXELS + btnRemove:WIDTH-PIXELS
      btnOpen:Y = ipButton:Y
      ldummy = btnSave:MOVE-TO-TOP()
      ldummy = btnRemove:MOVE-TO-TOP()
      ldummy = startHour:MOVE-TO-TOP()
      ldummy = startMinute:MOVE-TO-TOP()
      ldummy = startAMPM:MOVE-TO-TOP()
      ldummy = endHour:MOVE-TO-TOP()
      ldummy = endMinute:MOVE-TO-TOP()
      ldummy = endAMPM:MOVE-TO-TOP()
      currentList = ipIdx
      selectedDate:SCREEN-VALUE =
        IF NUM-ENTRIES(ipButton:LABEL,' ') EQ 2 THEN ENTRY(2,ipButton:LABEL,' ')
        ELSE STRING(DATE(calendarMonth,INT(ipButton:LABEL),calendarYear),'99.99.9999')
      selectedDate.
    DISPLAY {&timeFields}.
    ENABLE {&timeFields} {&timeButtons}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION calendarDate sObject 
FUNCTION calendarDate RETURNS CHARACTER
  (ipDate AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF ipDate NE ? THEN STRING(ipDate,'99/99/9999') ELSE 'ALL'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dayOfWeek sObject 
FUNCTION dayOfWeek RETURNS CHARACTER
  (ipListID AS INTEGER,ipDate AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wDay AS INTEGER NO-UNDO.

  wDay = IF ipDate NE ? THEN WEEKDAY(ipDate)
    ELSE IF ipListID GT 7 THEN ipListID - 7
    ELSE ipListID.
  RETURN dayName[wDay].

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

