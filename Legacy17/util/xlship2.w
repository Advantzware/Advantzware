&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/xlship2.w

  Description: Import ship-to addresses from Excel into the ship-to table.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: S.Brooks

  Created: Feb 2012

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
DEFINE INPUT PARAMETER piCompany LIKE shipto.company NO-UNDO.
DEFINE INPUT PARAMETER pcCust-no LIKE shipto.cust-no NO-UNDO.
DEFINE INPUT PARAMETER pcCarrier LIKE shipto.carrier NO-UNDO.
DEFINE INPUT PARAMETER pcLoc     LIKE shipto.loc NO-UNDO.  /* warehouse */
DEFINE INPUT PARAMETER pcLoc-bin LIKE shipto.loc-bin NO-UNDO.
DEFINE INPUT PARAMETER pcZone    LIKE shipto.dest-code NO-UNDO.


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghParentFrame      AS HANDLE   NO-UNDO.
DEFINE VARIABLE gcFileName AS CHAR NO-UNDO 
    INIT "P:\Stacey\Import Shipto.xlsx".

DEFINE VARIABLE gcDefaultDir AS CHAR NO-UNDO INIT "".

DEFINE VARIABLE gcExcelTypeList AS CHAR NO-UNDO INIT "xls,.xls,xl,excel".
DEFINE VARIABLE gcExcelExtList  AS CHAR NO-UNDO INIT "xls,xlsx".
DEFINE VARIABLE gcTextTypeList  AS CHAR NO-UNDO INIT "txt,.txt,tx,text".
DEFINE VARIABLE gcTextExtList   AS CHAR NO-UNDO INIT "txt".
DEFINE VARIABLE gcCSVTypeList   AS CHAR NO-UNDO INIT "csv,.csv,comma-delimited".
DEFINE VARIABLE gcCSVExtList    AS CHAR NO-UNDO INIT "csv".
DEFINE VARIABLE gcColumnStyle   AS CHAR NO-UNDO INIT "".

/* Temp-table to hold data imported from the csv file. */
DEFINE TEMP-TABLE ttData
    FIELD RowNum      AS INT
    FIELD rRowid      AS ROWID
    FIELD ValidRec    AS LOG INIT NO
    FIELD DBU   AS CHAR
    FIELD Zone  AS CHAR
    FIELD ship-code LIKE shipto.ship-id
    FIELD FirstName AS CHAR
    FIELD Lastname AS CHAR
    FIELD Company AS CHAR
    FIELD Address1 AS CHAR
    FIELD Address2 AS CHAR
    FIELD City AS CHAR
    FIELD State AS CHAR
    FIELD Zip AS CHAR
    FIELD Phone AS CHAR FORMAT "x(10)"
    FIELD whse   AS CHAR.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ piCompany
      AND sys-ctrl.name    EQ 'CustShipToImp'
    NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN
  ASSIGN
   gcColumnStyle  = sys-ctrl.char-fld.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brData

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttData

/* Definitions for BROWSE brData                                        */
&Scoped-define FIELDS-IN-QUERY-brData ttData.RowNum ttData.DBU ttData.Zone ttData.Ship-Code ttData.FirstName ttData.Lastname ttData.Company ttData.Address1 ttData.Address2 ttData.City ttData.State ttData.Zip ttData.Phone ttData.Whse   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brData   
&Scoped-define SELF-NAME brData
&Scoped-define QUERY-STRING-brData FOR EACH ttData
&Scoped-define OPEN-QUERY-brData OPEN QUERY {&SELF-NAME} FOR EACH ttData.
&Scoped-define TABLES-IN-QUERY-brData ttData
&Scoped-define FIRST-TABLE-IN-QUERY-brData ttData


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brData}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-229 btnFile fiFilename fiHeaderRow ~
fiHeader btnImport fiColumn-DBU fiColumn-Address1 btnLoad fiColumn-Zone ~
fiColumn-Address2 BtnDone fiColumn-City fiColumn-whse fiColumn-Firstname ~
fiColumn-Shipto fiColumn-State fiColumn-Lastname btnRemove fiColumn-Phone ~
fiColumn-Zip fiColumn-Company brData btnDelete fiTotal fiErrors 
&Scoped-Define DISPLAYED-OBJECTS fiFilename fiHeaderRow fiHeader ~
fiColumn-DBU fiColumn-Address1 fiColumn-Zone fiColumn-Address2 ~
fiColumn-City fiColumn-whse fiColumn-Firstname fiColumn-Shipto ~
fiColumn-State fiColumn-Lastname fiColumn-Phone fiColumn-Zip ~
fiColumn-Company fiTotal fiErrors 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convertFileName C-Win 
FUNCTION convertFileName RETURNS CHARACTER
  ( INPUT pcfileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD csvFile C-Win 
FUNCTION csvFile RETURNS LOGICAL
  ( INPUT pcfileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExcelFile C-Win 
FUNCTION ExcelFile RETURNS LOGICAL
  ( INPUT pcfileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fsgetFileExtension C-Win 
FUNCTION fsgetFileExtension RETURNS CHARACTER
  ( INPUT pcFileType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fsParseDirName C-Win 
FUNCTION fsParseDirName RETURNS CHARACTER
  ( INPUT pcFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getData C-Win 
FUNCTION getData RETURNS CHAR
  ( INPUT pcElement AS CHAR,
    INPUT pcInputLine AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDirName C-Win 
FUNCTION getDirName RETURNS CHARACTER
  ( INPUT pcFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getExtension C-Win 
FUNCTION getExtension RETURNS CHARACTER
  ( INPUT pcFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFileName C-Win 
FUNCTION getFileName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHeaderRow C-Win 
FUNCTION getHeaderRow RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumErrors C-Win 
FUNCTION getNumErrors RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumRecs C-Win 
FUNCTION getNumRecs RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPosition C-Win 
FUNCTION getPosition RETURNS INTEGER
  ( INPUT pcField AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getShipCode C-Win 
FUNCTION getShipCode RETURNS CHARACTER
  ( INPUT pcCity AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getShipNo C-Win 
FUNCTION getShipNo RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD headerRowFound C-Win 
FUNCTION headerRowFound RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isHeaderRow C-Win 
FUNCTION isHeaderRow RETURNS LOGICAL
  ( INPUT pcLine AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isNumeric C-Win 
FUNCTION isNumeric RETURNS LOGICAL
  ( INPUT pcString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isShipCodeUsed C-Win 
FUNCTION isShipCodeUsed RETURNS LOGICAL
  ( INPUT cShipCode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD loaded C-Win 
FUNCTION loaded RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClear 
     LABEL "Clear" 
     SIZE 15 BY 1.14 TOOLTIP "Clear the data".

DEFINE BUTTON btnDelete 
     LABEL "Delete" 
     SIZE 15 BY 1.14 TOOLTIP "Delete Record".

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnFile 
     LABEL "<>" 
     SIZE 5 BY 1.19.

DEFINE BUTTON btnImport 
     LABEL "Import" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLoad 
     LABEL "Load" 
     SIZE 15 BY 1.14 TOOLTIP "Load data into the database".

DEFINE BUTTON btnRemove 
     LABEL "Remove" 
     SIZE 15 BY 1.14 TOOLTIP "Remove loaded data from the database".

DEFINE VARIABLE fiColumn-Address1 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Address 1" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Address 1~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Address2 AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Address 2" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Address 2~"" NO-UNDO.

DEFINE VARIABLE fiColumn-City AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"City~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Company AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Company~"" NO-UNDO.

DEFINE VARIABLE fiColumn-DBU AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "DBU" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"DBU~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Firstname AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"First Name~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Lastname AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Last Name~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Phone AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Phone~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Shipto AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Store ID" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiColumn-State AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"State~"" NO-UNDO.

DEFINE VARIABLE fiColumn-whse AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Warehouse~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Zip AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Zip" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Zip~"" NO-UNDO.

DEFINE VARIABLE fiColumn-Zone AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Zone" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 TOOLTIP "Search for ~"Zone~"" NO-UNDO.

DEFINE VARIABLE fiErrors AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Invalid Records" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Number of records with missing data" NO-UNDO.

DEFINE VARIABLE fiFilename AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1 NO-UNDO.

DEFINE VARIABLE fiHeader AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiHeaderRow AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Column Header Row" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE fiTotal AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Imported" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Number of records imported from the csv file" NO-UNDO.

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 20.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brData FOR 
      ttData SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brData C-Win _FREEFORM
  QUERY brData DISPLAY
      ttData.RowNum    FORMAT ">>9" COLUMN-LABEL "Row" 
    ttData.DBU         FORMAT "x(3)" COLUMN-LABEL "DBU"
    ttData.Zone        FORMAT "x(3)" COLUMN-LABEL "Zone"
    ttData.Ship-Code   FORMAT "x(10)" COLUMN-LABEL "Ship Code"
    ttData.FirstName   FORMAT "x(15)" COLUMN-LABEL "FirstName"
    ttData.Lastname    FORMAT "x(20)" COLUMN-LABEL "LastName"
    ttData.Company     FORMAT "x(20)" COLUMN-LABEL "Company"
    ttData.Address1    FORMAT "x(20)" COLUMN-LABEL "Address 1"
    ttData.Address2    FORMAT "x(20)" COLUMN-LABEL "Address 2"
    ttData.City        FORMAT "x(15)" COLUMN-LABEL "City"
    ttData.State       FORMAT "x(2)" COLUMN-LABEL "State"
    ttData.Zip         COLUMN-LABEL "Zip"
    ttData.Phone       COLUMN-LABEL "Phone"
    ttData.Whse        FORMAT "X(3)" COLUMN-LABEL "Whse"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104.2 BY 7.62
         TITLE "Imported Data" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnFile AT ROW 1.91 COL 97.6 WIDGET-ID 124
     fiFilename AT ROW 1.95 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiHeaderRow AT ROW 3.86 COL 22.6 COLON-ALIGNED WIDGET-ID 68
     fiHeader AT ROW 3.86 COL 28.8 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     btnImport AT ROW 4.86 COL 92 WIDGET-ID 128
     fiColumn-DBU AT ROW 5.76 COL 19 COLON-ALIGNED WIDGET-ID 130
     fiColumn-Address1 AT ROW 5.76 COL 40 COLON-ALIGNED WIDGET-ID 140
     btnLoad AT ROW 6.1 COL 92 WIDGET-ID 152
     fiColumn-Zone AT ROW 7 COL 19 COLON-ALIGNED WIDGET-ID 132
     fiColumn-Address2 AT ROW 7 COL 40 COLON-ALIGNED WIDGET-ID 142
     BtnDone AT ROW 7.29 COL 92 WIDGET-ID 156
     fiColumn-City AT ROW 8.19 COL 40 COLON-ALIGNED WIDGET-ID 144
     fiColumn-whse AT ROW 8.19 COL 59 COLON-ALIGNED WIDGET-ID 174
     fiColumn-Firstname AT ROW 8.24 COL 19 COLON-ALIGNED WIDGET-ID 134
     fiColumn-Shipto AT ROW 9.33 COL 59 COLON-ALIGNED WIDGET-ID 172
     fiColumn-State AT ROW 9.38 COL 40 COLON-ALIGNED WIDGET-ID 146
     fiColumn-Lastname AT ROW 9.43 COL 19 COLON-ALIGNED WIDGET-ID 136
     btnRemove AT ROW 9.52 COL 92 WIDGET-ID 166
     fiColumn-Phone AT ROW 10.52 COL 59 COLON-ALIGNED WIDGET-ID 168
     fiColumn-Zip AT ROW 10.57 COL 40 COLON-ALIGNED WIDGET-ID 148
     fiColumn-Company AT ROW 10.62 COL 19 COLON-ALIGNED WIDGET-ID 138
     btnClear AT ROW 10.76 COL 92 WIDGET-ID 88
     brData AT ROW 12.43 COL 3.8 WIDGET-ID 200
     btnDelete AT ROW 20.29 COL 5 WIDGET-ID 162
     fiTotal AT ROW 20.29 COL 40 COLON-ALIGNED WIDGET-ID 32 NO-TAB-STOP 
     fiErrors AT ROW 20.29 COL 69 COLON-ALIGNED WIDGET-ID 86 NO-TAB-STOP 
     RECT-229 AT ROW 1.24 COL 2 WIDGET-ID 164
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.6 BY 21.52
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


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
         TITLE              = "Ship-To Excel Import"
         HEIGHT             = 21.52
         WIDTH              = 110.6
         MAX-HEIGHT         = 21.52
         MAX-WIDTH          = 125.4
         VIRTUAL-HEIGHT     = 21.52
         VIRTUAL-WIDTH      = 125.4
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brData btnClear DEFAULT-FRAME */
ASSIGN 
       brData:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       brData:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

/* SETTINGS FOR BUTTON btnClear IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiColumn-Address1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Address2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-City:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Company:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-DBU:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Firstname:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Lastname:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Phone:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Shipto:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-State:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-whse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Zip:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiColumn-Zone:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiErrors:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiHeader:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiHeaderRow:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiTotal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brData
/* Query rebuild information for BROWSE brData
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttData
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brData */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ship-To Excel Import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ship-To Excel Import */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME DEFAULT-FRAME /* Clear */
DO:
  RUN Clear-Data.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  RUN Delete-Record.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Done */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFile C-Win
ON CHOOSE OF btnFile IN FRAME DEFAULT-FRAME /* <> */
DO:
    RUN Select-File.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport C-Win
ON CHOOSE OF btnImport IN FRAME DEFAULT-FRAME /* Import */
DO:
  RUN Import-Data.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad C-Win
ON CHOOSE OF btnLoad IN FRAME DEFAULT-FRAME /* Load */
DO:
  RUN Process-Data.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME /* Remove */
DO:
  RUN Remove-Data.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiErrors
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiErrors C-Win
ON MOUSE-SELECT-DBLCLICK OF fiErrors IN FRAME DEFAULT-FRAME /* Invalid Records */
DO:
/*   RUN View-Message ("Record numbers that contain errors: " + getErrorList()). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFilename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilename C-Win
ON ENTRY OF fiFilename IN FRAME DEFAULT-FRAME
DO:
  IF  gcColumnStyle NE "Premier" THEN
      ASSIGN fiColumn-Shipto:HIDDEN = TRUE
             fiColumn-whse:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brData
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  APPLY 'entry' TO fiFilename.
  RUN init-proc.
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear-Data C-Win 
PROCEDURE Clear-Data :
/*------------------------------------------------------------------------------
  Purpose:     Clear the temp-table and data fields.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      EMPTY TEMP-TABLE ttData.


      /* Clear the column number values. */
      ASSIGN fiHeaderRow:SCREEN-VALUE = ""
             fiHeader:SCREEN-VALUE = ""
             fiColumn-DBU:SCREEN-VALUE = ""
             fiColumn-Zone:SCREEN-VALUE = ""
             fiColumn-FirstName:SCREEN-VALUE = ""
             fiColumn-LastName:SCREEN-VALUE = ""
             fiColumn-Company:SCREEN-VALUE = ""
             fiColumn-Address1:SCREEN-VALUE = ""
             fiColumn-Address2:SCREEN-VALUE = ""
             fiColumn-City:SCREEN-VALUE = ""
             fiColumn-State:SCREEN-VALUE = ""
             fiColumn-Zip:SCREEN-VALUE = ""
             fiColumn-Phone:SCREEN-VALUE = ""
             fiColumn-Shipto:SCREEN-VALUE = ""
             fiColumn-whse:SCREEN-VALUE = "".


      /* Re-open the query. */
      RUN Show-Results.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Convert-File C-Win 
PROCEDURE Convert-File :
/*------------------------------------------------------------------------------
  Purpose:     Convert an excel file to a csv format.
  Parameters:  Input - Excel file name
               Output - Csv file name
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcInputFile AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAMETER pcOutputFile AS CHAR NO-UNDO INIT "".

  DEF VAR chExcel     AS COM-HANDLE NO-UNDO.
  DEF VAR chWorkBook  AS COM-HANDLE NO-UNDO.
  DEF VAR chWorkSheet AS COM-HANDLE NO-UNDO.

  /* Start Excel */
  CREATE "Excel.Application" chExcel.
  ASSIGN chExcel:Visible = FALSE.

  /* Open the file. */
  chExcel:Workbooks:Open(pcInputFile,2,TRUE,,,,TRUE).

  /* Get the sheet. */
  ASSIGN chWorkbook = chExcel:WorkBooks:Item(1)
         chWorkSheet = chExcel:Sheets:Item(2).

  /* Convert the filename. */
  ASSIGN pcOutputFile = convertFileName(pcInputFile). 

  /* Delete if already exists. */
  OS-COMMAND SILENT DEL VALUE(pcOutputFile).
/*  IF SEARCH(pcInFile) <> ? THEN
    OS-DELETE value(pcInfile) NO-ERROR. */

  /* Turn off alerts */
  chExcel:DisplayAlerts = FALSE.

  /* Save the new file in csv format. */
  chWorkBook:SaveAs(pcOutputFile,6,,,,,,, TRUE).

  /* Close the workbook. */
  chWorkBook:Close().

  /* Release objects. */
  RELEASE OBJECT chWorkSheet NO-ERROR.
  RELEASE OBJECT chWorkBook NO-ERROR.

  /* Quit Excel */
  chExcel:QUIT.
  RELEASE OBJECT chExcel.

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Delete-Record C-Win 
PROCEDURE Delete-Record :
/*------------------------------------------------------------------------------
  Purpose:     Delete the current selected record from the temp-table.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE ttData THEN DO:
      MESSAGE "No record selected."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  IF AVAILABLE ttData THEN
      DELETE ttData.

  RUN Show-Results.

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fiFilename fiHeaderRow fiHeader fiColumn-DBU fiColumn-Address1 
          fiColumn-Zone fiColumn-Address2 fiColumn-City fiColumn-whse 
          fiColumn-Firstname fiColumn-Shipto fiColumn-State fiColumn-Lastname 
          fiColumn-Phone fiColumn-Zip fiColumn-Company fiTotal fiErrors 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-229 btnFile fiFilename fiHeaderRow fiHeader btnImport 
         fiColumn-DBU fiColumn-Address1 btnLoad fiColumn-Zone fiColumn-Address2 
         BtnDone fiColumn-City fiColumn-whse fiColumn-Firstname fiColumn-Shipto 
         fiColumn-State fiColumn-Lastname btnRemove fiColumn-Phone fiColumn-Zip 
         fiColumn-Company brData btnDelete fiTotal fiErrors 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Import-CSV C-Win 
PROCEDURE Import-CSV :
/*------------------------------------------------------------------------------
  Purpose:     Import data from the csv file into our local temp-table.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcInputLine     AS CHAR NO-UNDO INIT "".
    DEFINE VARIABLE vi              AS INT  NO-UNDO INIT 0.
    DEFINE VARIABLE vcShip-Code     AS CHAR NO-UNDO INIT "".
    DEFINE VARIABLE vcWhse           AS CHAR NO-UNDO INIT "".

    DEFINE VARIABLE vcFileName AS CHAR NO-UNDO INIT "".

/*     /* If the import positions are not entered, then abort. */       */
/*     IF NOT validImportEntries() THEN RETURN.                         */
/*     /* If the data in the input file is not correct, then abort. */  */
/*     IF NOT validImportData() THEN RETURN.                            */

    SESSION:SET-WAIT-STATE ("General").

    /* Get the filename. */
    ASSIGN vcFileName = getFileName().

    /* Empty the zip4 temp-table. */
    EMPTY TEMP-TABLE ttData.

    /* Define the input data to come from the source file name. */
    INPUT FROM VALUE(vcFileName).

    /* Start a repeating block to import each line of the file. */
    import-block:
    REPEAT ON ERROR UNDO, LEAVE:

       /* Import the current line. */
        IMPORT UNFORMATTED vcInputLine.

        /* Increment row counter. */
        ASSIGN vi = (vi + 1).

        /* If this is the header row, then set header values and skip. */
        IF isHeaderRow(vcInputLine) THEN DO:
            RUN Set-Header-Values (vi,vcInputLine).
            NEXT import-block.
        END.

        /* If header row has not been found yet, then keep reading. */
        IF NOT headerRowFound() THEN NEXT import-block.

        /* Create a temp-table record for this input line. */
        CREATE ttData.
        /* Get the data from the imported line based on each fields specified column. */
        ASSIGN ttData.RowNum     = vi
               ttData.DBU        = getData("DBU",vcInputLine)
               ttData.Zone       = getData("Zone",vcInputLine)
               ttData.FirstName  = getData("FirstName",vcInputLine)
               ttData.LastName   = getData("LastName",vcInputLine)
               ttData.Company    = getData("Company",vcInputLine)
               ttData.Address1   = getData("Address1",vcInputLine)
               ttData.Address2   = getData("Address2",vcInputLine)
               ttData.City       = getData("City",vcInputLine)
               ttData.State      = getData("State",vcInputLine)
               ttData.Zip        = getData("Zip",vcInputLine)
               ttData.Phone      = getData("Phone",vcInputLine)
               ttData.ValidRec   = TRUE.
        IF gcColumnStyle EQ "Premier" THEN
             ASSIGN
               vcWhse             = getData("whse",vcInputLine)
               vcShip-Code       = getData("ShipTo",vcInputLine).

        IF ttData.ValidRec = TRUE AND gcColumnStyle NE "Premier" THEN
            ASSIGN ttData.Ship-code = getShipCode(ttData.City).
        ELSE DO:
            /* If importing ship-code, confirm it is an unused code */
            IF isShipCodeUsed(vcShip-Code) THEN
                ttData.ValidRec = FALSE.

            ASSIGN ttData.Ship-code = vcShip-Code
                   ttData.whse       = vcWhse.
        END.
        /* Limit num records for testing. */
/*         IF getNumRecs() = 3 THEN LEAVE import-block. */

    END. /* import-block */

    /* Close the input file. */
    INPUT CLOSE.

    /* Display results. */
    RUN Show-Results.


    SESSION:SET-WAIT-STATE ("").

    /* Inform user if column header row not found. */
    IF NOT headerRowFound() THEN DO:
        MESSAGE "Column header row was not found."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    IF getNumRecs() > 0 THEN
        MESSAGE getNumRecs() " records were imported." SKIP
                "Review your data." SKIP
                "If ok, select LOAD to load data into the database." SKIP
                "Use DELETE to delete bad records before loading." SKIP
                "Use CLEAR to clear all data to start a new import." SKIP
                "Click DONE when finished."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*     RUN Report-Errors.  */


    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Import-Data C-Win 
PROCEDURE Import-Data :
/*------------------------------------------------------------------------------
  Purpose:     Start the import process.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcFileName AS CHAR NO-UNDO INIT "".

  DO WITH FRAME {&FRAME-NAME}:

      /* If the input file is excel format. */
      IF ExcelFile(getFileName()) THEN DO:

          /* Convert excel file to csv format. */
          RUN Convert-File (INPUT getFileName(),
                            OUTPUT vcFileName).

          /* Set the display file name as the new file name. */
          ASSIGN fiFileName:SCREEN-VALUE = TRIM(vcFileName).
      END.

      /* Import csv file. */
      RUN Import-CSV.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Import-Excel C-Win 
PROCEDURE Import-Excel :
/*------------------------------------------------------------------------------
  Purpose:     Import data directly from Excel spreadsheet.
               This doesn't work correctly so we are converting it to csv instead.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEF VAR vchExcel     AS COM-HANDLE NO-UNDO.                                    */
/* DEF VAR vchWorkBook  AS COM-HANDLE NO-UNDO.                                    */
/* DEF VAR vchWorkSheet AS COM-HANDLE NO-UNDO.                                    */
/*                                                                                */
/* DEF VAR vRow AS INT NO-UNDO INIT 0.                                            */
/* DEF VAR vError AS LOGICAL NO-UNDO.                                             */
/* DEFINE VARIABLE vcFileName AS CHAR NO-UNDO INIT "".                            */
/* DEFINE VARIABLE cRange AS CHAR NO-UNDO INIT "".                                */
/*                                                                                */
/*     /* Get the filename. */                                                    */
/*     ASSIGN vcFileName = getFileName().                                         */
/*                                                                                */
/*     CREATE "Excel.Application":U vchExcel.                                     */
/*                                                                                */
/*     ASSIGN vchExcel:VISIBLE = FALSE                                            */
/*            vchWorkBook      = vchExcel:WorkBooks:OPEN(vcFileName)              */
/*            vchWorkSheet     = vchExcel:Sheets:ITEM(1).                         */
/*                                                                                */
/*     Excel-Block:                                                               */
/*     REPEAT TRANSACTION:                                                        */
/*       ASSIGN vError = FALSE                                                    */
/*              vRow   = vRow + 1.                                                */
/*                                                                                */
/*       ASSIGN cRange = "A" + STRING(vRow).                                      */
/*                                                                                */
/*       MESSAGE cRange SKIP                                                      */
/*               vchWorkSheet:RANGE(cRange):TEXT                                  */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
/*                                                                                */
/*       /* Go past the column header row. */                                     */
/*       IF vRow <= getHeaderRow() THEN NEXT Excel-Block.                         */
/*                                                                                */
/*                                                                                */
/*       /**  QUIT WHEN REACH A BLANK LINE  **/                                   */
/*       IF vchWorkSheet:RANGE(cRange):TEXT = "" THEN DO:                         */
/*           MESSAGE "Empty Data on Row: " vRow SKIP                              */
/*               "Data: " vchWorkSheet:RANGE(cRange):TEXT                         */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                               */
/*           LEAVE Excel-Block.                                                   */
/*       END.                                                                     */
/*                                                                                */
/*       MESSAGE "Found: " vchWorkSheet:Range(cRange):TEXT                        */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
/*                                                                                */
/*        CREATE ttData.                                                          */
/*       /* IMPORT FROM EXCEL */                                                  */
/*       ASSIGN ttData.RowNum    = vRow                                           */
/*              ttData.DBU       = vchWorkSheet:Range("A":U + STRING(vRow)):TEXT  */
/*              ttData.Zone      = vchWorkSheet:Range("B":U + STRING(vRow)):TEXT  */
/*              ttData.FirstName = vchWorkSheet:Range("C":U + STRING(vRow)):TEXT  */
/*              ttData.LastName  = vchWorkSheet:Range("D":U + STRING(vRow)):TEXT  */
/*              NO-ERROR.                                                         */
/*                                                                                */
/*                                                                                */
/*       /**  WAS THERE AN ERROR READING THE SPREADSHEET?  **/                    */
/*       IF ERROR-STATUS:ERROR THEN DO:                                           */
/*         ASSIGN vError = YES.                                                   */
/*         MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.                 */
/*       END.                                                                     */
/*     END.  /**  END OF REPEAT TRANSACTION - GetExcel-BLK  **/                   */
/*                                                                                */
/*     vchWorkBook:CLOSE.                                                         */
/*     RELEASE OBJECT vchWorkSheet.                                               */
/*     RELEASE OBJECT vchWorkBook.                                                */
/*                                                                                */
/*     vchExcel:QUIT.                                                             */
/*     RELEASE OBJECT vchExcel.                                                   */
/*                                                                                */
/*                                                                                */
/*     /* Display results. */                                                     */
/*     RUN Show-Results.                                                          */
/*                                                                                */
/*     RETURN.                                                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Proc C-Win 
PROCEDURE Init-Proc :
/*------------------------------------------------------------------------------
  Purpose:     Perform initialization tasks
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " " + pcCust-no.
      ASSIGN fiFileName:SCREEN-VALUE = gcFileName.
  END.

  /* Set the starting directory for file lookup. */
  ASSIGN gcDefaultDir = getDirName(gcFileName).

  /* Open the query and display data. */
  RUN Show-Results.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process-Data C-Win 
PROCEDURE Process-Data :
/*------------------------------------------------------------------------------
  Purpose:     Load the temp-table data into the database.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viShipNo LIKE shipto.ship-no NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:

      /* Prompt user to load the data. */
      MESSAGE "Load " (getNumRecs() - getNumErrors()) " records into the Database?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lLoad AS LOGICAL.

      /* If not, then abort. */
      IF NOT lLoad THEN RETURN.

      SESSION:SET-WAIT-STATE ("General").

      DO TRANSACTION ON ERROR UNDO, RETURN:

          /* Process each temp-table record. */
          FOR EACH ttData NO-LOCK WHERE validRec = TRUE:

              ASSIGN viShipNo = getShipNo().


              /* Create database record. */
              CREATE shipto.

              ASSIGN shipto.company      = trim(piCompany)
                     shipto.cust-no      = trim(pcCust-no)
                     shipto.ship-no      = viShipNo
                     shipto.loc          = trim(pcLoc)
                     shipto.loc-bin      = trim(pcLoc-bin)
                     shipto.dest-code    = trim(pcZone)
                     shipto.carrier      = trim(pcCarrier)
                     shipto.ship-name    = trim(ttData.Company)
                     shipto.contact      = (trim(ttData.FirstName) + " " + 
                                            trim(ttData.LastName))
                     shipto.ship-addr[1] = trim(ttData.Address1)
                     shipto.ship-addr[2] = trim(ttData.Address2)
                     shipto.ship-city    = trim(ttData.City)
                     shipto.ship-state   = trim(ttData.State)
                     shipto.ship-zip     = trim(ttData.Zip)
                     shipto.area-code    = TRIM(SUBSTRING(ttData.Phone,1,3))
                     shipto.phone        = trim(substring(ttData.Phone,4,7))
                     shipto.ship-id      = trim(ttData.Ship-code).

              /* If warehouse is sent in, use it */
              IF TRIM(ttData.whse) GT "" THEN
                shipto.loc = TRIM(ttData.whse).

              /* save rowid */
              ASSIGN ttData.rRowid = ROWID(shipto).

          END.
      END. /* DO TRANSACTION */

      /* Show final results on screen. */
      RUN Show-Results.

      SESSION:SET-WAIT-STATE ("").

  END. /* DO WITH FRAME */

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Remove-Data C-Win 
PROCEDURE Remove-Data :
/*------------------------------------------------------------------------------
  Purpose:     Remove data just added.
  Parameters:  <none>
  Notes:       rRowid
------------------------------------------------------------------------------*/

  /* Prompt user to load the data. */
  MESSAGE "This will delete the shipto records you just loaded from the database." SKIP 
          "Remove " (getNumRecs() - getNumErrors()) " records from the database?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lRemove AS LOGICAL.

  /* If not, then abort. */
  IF NOT lRemove THEN RETURN.

  SESSION:SET-WAIT-STATE ("General").

      DO TRANSACTION ON ERROR UNDO, RETURN:

      FOR EACH ttData:
          FIND shipto EXCLUSIVE-LOCK WHERE ROWID(shipto) = ttData.rRowid NO-ERROR.
          IF AVAIL shipto THEN 
              DELETE shipto.
      END.
  END.


  /* Show final results on screen. */
  RUN Show-Results.

  MESSAGE "Records Removed"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  SESSION:SET-WAIT-STATE ("").

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select-File C-Win 
PROCEDURE Select-File :
/*------------------------------------------------------------------------------
  Purpose:     Allow user to select an import file.
  Parameters:  <none>
  Notes:       Currently can only import from csv files.
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vcFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.


    DO WITH FRAME {&FRAME-NAME}:

        SYSTEM-DIALOG GET-FILE vcFileName
        TITLE "Choose Import File"
        FILTERS "CSV Files (*.csv,*.xls)" "*.csv,*.xls,*.xlsx" /* ,"Excel Files (*.xls)" "*.xls" */
        MUST-EXIST
        INITIAL-DIR gcDefaultDir
        USE-FILENAME
        UPDATE OKpressed.

        /* If user selected a file, then get the file name and save the 
           directory name for next select process. */
        IF OKpressed = TRUE THEN
        ASSIGN fiFileName:SCREEN-VALUE = vcFileName
               gcDefaultDir = SUBSTRING(vcFileName,1,(R-INDEX(vcFileName,"/") - 1)).

    END.

    RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Header-Values C-Win 
PROCEDURE Set-Header-Values :
/*------------------------------------------------------------------------------
  Purpose:     Display header values on the screen.
  Parameters:  Input - Current line number
               Input - Current line of data.
  Notes:       This runs when the input line containing the column headers is found.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piLine AS INT NO-UNDO.
  DEFINE INPUT PARAMETER pcLine AS CHAR NO-UNDO.

     IF isHeaderRow(pcLine) THEN DO WITH FRAME {&FRAME-NAME}:

           ASSIGN fiHeaderRow:SCREEN-VALUE = STRING(piLine)
                  fiHeader:SCREEN-VALUE = STRING(pcLine)
                  fiColumn-DBU:SCREEN-VALUE = STRING(LOOKUP("DBU",pcLine))
                  fiColumn-Zone:SCREEN-VALUE = STRING(LOOKUP("Zone",pcLine))
                  fiColumn-FirstName:SCREEN-VALUE = STRING(LOOKUP("First Name",pcLine))
                  fiColumn-LastName:SCREEN-VALUE = STRING(LOOKUP("Last Name",pcLine))
                  fiColumn-Company:SCREEN-VALUE = STRING(LOOKUP("Company",pcLine))
                  fiColumn-Address1:SCREEN-VALUE = STRING(LOOKUP("Address 1",pcLine))
                  fiColumn-Address2:SCREEN-VALUE = STRING(LOOKUP("Address 2",pcLine))
                  fiColumn-City:SCREEN-VALUE = STRING(LOOKUP("City",pcLine))
                  fiColumn-State:SCREEN-VALUE = STRING(LOOKUP("State",pcLine))
                  fiColumn-Zip:SCREEN-VALUE = STRING(LOOKUP("Zip",pcLine))
                  fiColumn-Phone:SCREEN-VALUE = STRING(LOOKUP("Phone",pcLine))
                  fiColumn-Shipto:SCREEN-VALUE = STRING(LOOKUP("Ship ID", pcLine))
                  fiColumn-whse:SCREEN-VALUE = STRING(LOOKUP("Whse", pcLine)).

     END.

     RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Show-Results C-Win 
PROCEDURE Show-Results :
/*------------------------------------------------------------------------------
  Purpose:     Display totals and enable/disable buttons.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      {&OPEN-QUERY-brData}

      ASSIGN fiTotal:SCREEN-VALUE  = STRING(getNumRecs())
             fiErrors:SCREEN-VALUE = STRING(getNumErrors())
             /* Enable Import button when there are no records in the temp-table. */
             btnImport:SENSITIVE   = (getNumRecs() = 0 AND NOT loaded())
             /* Enable delete button when there is at least one record in the imported data. */
             btnDelete:SENSITIVE   = (getNumRecs() > 0)
             /* Enable Load button when there is at least one imported record and
                there are no errors in the data and has not yet been loaded. */
             btnLoad:SENSITIVE     = (getNumRecs() > 0 AND getNumErrors() = 0 AND NOT loaded())
             /* Enable Remove button when records are present and they have been db loaded. */
             btnRemove:SENSITIVE   = (getNumRecs() > 0 AND loaded()) 
             /* Enable the Empty button when there is data in the temp-table. */
             btnClear:SENSITIVE    = (getNumRecs() > 0).

  END.

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convertFileName C-Win 
FUNCTION convertFileName RETURNS CHARACTER
  ( INPUT pcfileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcBaseFileName  AS CHAR NO-UNDO.


  ASSIGN vcBaseFileName = TRIM(ENTRY(1,pcfileName,".") + ".csv").

/*   ASSIGN vcBaseFileName = TRIM(fsparseDirName(pcFileName)) + "/" +  */
/*              TRIM(ENTRY(1,vcBaseFileName,".")) + ".csv".            */
/*                    + fsgetFileExtension(pcFileType)). */


  RETURN vcBaseFileName.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION csvFile C-Win 
FUNCTION csvFile RETURNS LOGICAL
  ( INPUT pcfileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcExt AS CHAR NO-UNDO INIT "".

  ASSIGN vcExt = TRIM(ENTRY(1,pcfileName,".")).

  IF LOOKUP(vcExt, gcCsvTypeList) > 0 THEN 
      RETURN TRUE.
  ELSE
      RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExcelFile C-Win 
FUNCTION ExcelFile RETURNS LOGICAL
  ( INPUT pcfileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vcExt AS CHAR NO-UNDO INIT "".

  ASSIGN vcExt = TRIM(ENTRY(2,pcfileName,".")).

  IF LOOKUP(vcExt,gcExcelExtList) > 0 THEN 
      RETURN TRUE.
  ELSE
      RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fsgetFileExtension C-Win 
FUNCTION fsgetFileExtension RETURNS CHARACTER
  ( INPUT pcFileType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Return an appropriate file extension for a file type.
    Notes:  
------------------------------------------------------------------------------*/
/*                                                                                        */
/*   /* If generating Text file then return the first text extension in the list. */      */
/*   IF LOOKUP(pcFileType, gcTextTypeList) > 0 THEN RETURN ENTRY(1,gcTextExtList,",").    */
/*                                                                                        */
/*                                                                                        */
/*   /* If generating Excel file then return the first excel extension in the list. */    */
/*   IF LOOKUP(pcFileType, gcExcelTypeList) > 0 THEN RETURN ENTRY(1,gcExcelExtList,",").  */
/*                                                                                        */
/*                                                                                        */
/*   /* If generating CSV file then return the first CSV extension in the list. */        */
/*   IF LOOKUP(pcFileType, gcCSVTypeList) > 0 THEN RETURN ENTRY(1,gcCSVExtList,",").      */
/*                                                                                        */

  /* If nothing found, then return blank extension. */
  RETURN "".   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fsParseDirName C-Win 
FUNCTION fsParseDirName RETURNS CHARACTER
  ( INPUT pcFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Parse and return the directory of a file name.
    Notes:  Example:  "c:/temp/work/abc.txt"  returns  "c:/temp/work"
------------------------------------------------------------------------------*/

  /* Return the string without the base filename and last slash. */
  RETURN SUBSTRING(pcFileName,1,(R-INDEX(pcFileName,"/") - 1)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getData C-Win 
FUNCTION getData RETURNS CHAR
  ( INPUT pcElement AS CHAR,
    INPUT pcInputLine AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Return the requested data from the input line.
    Notes:  
------------------------------------------------------------------------------*/

    RETURN ENTRY(getPosition(pcElement),pcInputLine).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDirName C-Win 
FUNCTION getDirName RETURNS CHARACTER
  ( INPUT pcFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    /* Return the string without the base filename and last slash. */
  RETURN SUBSTRING(pcFileName,1,(R-INDEX(pcFileName,"/") - 1)).


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getExtension C-Win 
FUNCTION getExtension RETURNS CHARACTER
  ( INPUT pcFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN TRIM(ENTRY(1,pcfileName,".")).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFileName C-Win 
FUNCTION getFileName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      IF fiFileName:SCREEN-VALUE <> "" THEN
          RETURN fiFileName:SCREEN-VALUE.
      ELSE
          RETURN gcFileName.

  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHeaderRow C-Win 
FUNCTION getHeaderRow RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      RETURN INT(fiHeaderRow:SCREEN-VALUE).
  END.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumErrors C-Win 
FUNCTION getNumErrors RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Count number of records with bad data.
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE BUFFER bufData FOR ttData.
   DEFINE VARIABLE vi AS INT NO-UNDO INIT 0.

   FOR EACH bufData NO-LOCK WHERE
            bufData.ValidRec = NO:
       ASSIGN vi = vi + 1.
   END.


  RETURN vi.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumRecs C-Win 
FUNCTION getNumRecs RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  getNumRecs
------------------------------------------------------------------------------*/
  DEFINE BUFFER bufData FOR ttData.
  DEFINE VARIABLE vi AS INT NO-UNDO INIT 0.

  FOR EACH bufData NO-LOCK:
      ASSIGN vi = vi + 1.
  END.


  RETURN vi.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPosition C-Win 
FUNCTION getPosition RETURNS INTEGER
  ( INPUT pcField AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      CASE pcField:
          WHEN "DBU" THEN RETURN INT(ficolumn-DBU:SCREEN-VALUE).
          WHEN "Zone" THEN RETURN INT(ficolumn-Zone:SCREEN-VALUE).
          WHEN "ShipTo" THEN RETURN INT(ficolumn-Shipto:SCREEN-VALUE).
          WHEN "FirstName" THEN RETURN INT(ficolumn-FirstName:SCREEN-VALUE).
          WHEN "LastName" THEN RETURN INT(ficolumn-LastName:SCREEN-VALUE).
          WHEN "Company" THEN RETURN INT(ficolumn-Company:SCREEN-VALUE).
          WHEN "Address1" THEN RETURN INT(ficolumn-Address1:SCREEN-VALUE).
          WHEN "Address2" THEN RETURN INT(ficolumn-Address2:SCREEN-VALUE).
          WHEN "City" THEN RETURN INT(ficolumn-City:SCREEN-VALUE).
          WHEN "State" THEN RETURN INT(ficolumn-State:SCREEN-VALUE).
          WHEN "Zip" THEN RETURN INT(ficolumn-Zip:SCREEN-VALUE).
          WHEN "Phone" THEN RETURN INT(ficolumn-Phone:SCREEN-VALUE).
          WHEN "ShipTo" THEN RETURN INT(ficolumn-shipto:SCREEN-VALUE).
          WHEN "Whse" THEN RETURN INT(ficolumn-whse:SCREEN-VALUE).
          OTHERWISE RETURN 0.
      END CASE.
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getShipCode C-Win 
FUNCTION getShipCode RETURNS CHARACTER
  ( INPUT pcCity AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Create ship-id code
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cShipCode AS CHAR NO-UNDO INIT "".
  DEFINE VARIABLE iCount AS INT NO-UNDO INIT 0.


  ASSIGN pcCity = REPLACE(pcCity, " ", "").

  REPEAT:
      ASSIGN iCount = iCount + 1.
      /* Build a potential ship-code using city prefix and counter. */
      ASSIGN cShipCode = SUBSTRING(pcCity,1,4) + STRING(iCount,"9999").
      /* If this ship code is not in the database and not in the temp-table,
         then use this ship code. */
      IF NOT isShipCodeUsed(cShipCode) THEN LEAVE.
  END.

  RETURN cShipCode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getShipNo C-Win 
FUNCTION getShipNo RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER bf-shipto FOR shipto.

FIND LAST bf-shipto
    WHERE bf-shipto.company EQ picompany
      AND bf-shipto.cust-no EQ pccust-no
/*       AND ROWID(bf-shipto)  NE ROWID(shipto)  */
    USE-INDEX ship-no NO-LOCK NO-ERROR.

RETURN (IF AVAIL bf-shipto THEN bf-shipto.ship-no ELSE 0) + 1.



/*   /* shipto.i */                                                          */
/* DEF BUFFER bf-shipto FOR shipto.                                          */
/* DEF BUFFER bf-cust FOR cust.                                              */
/*                                                                           */
/*                                                                           */
/* {methods/run_link.i "RECORD-SOURCE" "Get-Values"                          */
/*     "(OUTPUT op-company,OUTPUT op-cust-no)"}                              */
/*                                                                           */
/* ASSIGN                                                                    */
/*  shipto.company = op-company                                              */
/*  shipto.cust-no = op-cust-no.                                             */
/*                                                                           */
/* FIND LAST bf-shipto                                                       */
/*     WHERE bf-shipto.company EQ shipto.company                             */
/*       AND bf-shipto.cust-no EQ shipto.cust-no                             */
/*       AND ROWID(bf-shipto)  NE ROWID(shipto)                              */
/*     USE-INDEX ship-no NO-LOCK NO-ERROR.                                   */
/*                                                                           */
/* ASSIGN                                                                    */
/*  shipto.ship-no = (IF AVAIL bf-shipto THEN bf-shipto.ship-no ELSE 0) + 1  */
/*  shipto.ship-id = TRIM(STRING(shipto.ship-no,">>>>>>>>"))                 */
/*  shipto.ship-meth = cust.ship-part.                                       */
/*                                                                           */
/* RELEASE bf-shipto.                                                        */
/*                                                                           */
/* FIND FIRST bf-cust NO-LOCK                                                */
/*     WHERE bf-cust.company EQ shipto.company                               */
/*       AND bf-cust.active  EQ "X"                                          */
/*     NO-ERROR.                                                             */
/*                                                                           */
/* IF AVAIL bf-cust THEN                                                     */
/* FOR EACH bf-shipto                                                        */
/*     WHERE bf-shipto.company EQ bf-cust.company                            */
/*       AND bf-shipto.cust-no EQ bf-cust.cust-no                            */
/*     BREAK BY bf-shipto.ship-no DESC:                                      */
/*   IF bf-shipto.ship-id EQ bf-shipto.cust-no OR                            */
/*      LAST(bf-shipto.ship-no)                THEN DO:                      */
/*     ASSIGN                                                                */
/*      shipto.loc     = bf-shipto.loc                                       */
/*      shipto.loc-bin = bf-shipto.loc-bin.                                  */
/*     LEAVE.                                                                */
/*   END.                                                                    */
/* END.                                                                      */
/*                                                                           */
/* IF cust.sort EQ "Y" THEN shipto.tax-code = cust.tax-gr.                   */





END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION headerRowFound C-Win 
FUNCTION headerRowFound RETURNS LOGICAL
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  /* If header row already found, return true. */
  IF getHeaderRow() > 0 THEN 
      RETURN TRUE.
  ELSE
      RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isHeaderRow C-Win 
FUNCTION isHeaderRow RETURNS LOGICAL
  ( INPUT pcLine AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Determine if this line is the header row.
    Notes:  
------------------------------------------------------------------------------*/


     IF LOOKUP("DBU",pcLine) > 0 AND
         LOOKUP("Zone",pcLine) > 0 AND
         LOOKUP("First Name",pcLine) > 0 AND
         LOOKUP("Last Name",pcLine) > 0 AND
         LOOKUP("Company",pcLine) > 0 AND
         LOOKUP("Address 1",pcLine) > 0 AND
         LOOKUP("Address 2",pcLine) > 0 AND
         LOOKUP("City",pcLine) > 0 AND
         LOOKUP("State",pcLine) > 0 AND
         LOOKUP("Zip",pcLine) > 0 AND 
         LOOKUP("Phone",pcLine) > 0 AND
         (LOOKUP("Ship ID", pcLine) > 0 OR NOT gcColumnStyle EQ "Premier") AND
         (LOOKUP("Whse", pcLine) > 0 OR NOT gcColumnStyle EQ "Premier") THEN 
         RETURN TRUE.
     ELSE
         RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isNumeric C-Win 
FUNCTION isNumeric RETURNS LOGICAL
  ( INPUT pcString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Determine if a string value contains any alpha characters.
   Syntax:  
    Notes:  If string contains only numbers, then value is numeric,
            else if string contains alpha characters then value
            is not numeric.
------------------------------------------------------------------------------*/
    DEFINE VARIABLE vi         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE viLength   AS INTEGER   NO-UNDO.


    /* Get the length of the string. */
    viLength = LENGTH(pcString).

    /* Test each position to see if it is numeric or not. */
    DO vi = 1 TO viLength:
       /* If alpha characters are found, then return false. */
       IF ASC(SUBSTRING(pcString,vi,1)) < 48 OR
          ASC (SUBSTRING(pcString,vi,1)) > 57 THEN
          RETURN FALSE.
    END.


    /* Return true indicating value is numeric. */
    RETURN TRUE.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isShipCodeUsed C-Win 
FUNCTION isShipCodeUsed RETURNS LOGICAL
  ( INPUT cShipCode AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST shipto WHERE 
                              shipto.company = piCompany AND
                              shipto.cust-no = pcCust-no AND
                              shipto.ship-id = cShipCode) 
     AND NOT CAN-FIND (FIRST ttData WHERE ttData.ship-code = cShipCode) THEN
      RETURN FALSE.   /* Function return value. */
  ELSE 
      RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION loaded C-Win 
FUNCTION loaded RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Determine if data has been loaded.
    Notes:  If a rowid exists in at least one record, then it's been loaded.
------------------------------------------------------------------------------*/

  IF CAN-FIND(FIRST ttData WHERE rRowId <> ?) THEN
      RETURN TRUE.
  ELSE
      RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

