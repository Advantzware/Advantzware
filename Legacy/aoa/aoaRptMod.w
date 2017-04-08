&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aoa/aoaRptMod.w

  Description: AOA Report Modifier for Data PA

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.15.2016

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

&SCOPED-DEFINE aoaMultiplier 140

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE PAApplication  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE PAReportEngine AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hReport        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hScript        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cDataPAReport  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lOKPressed     AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cID            AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE ttSubject NO-UNDO
    FIELD ttOrder        AS INTEGER   LABEL "Order"         FORMAT ">>9"
    FIELD ttField        AS CHARACTER LABEL "Field"         FORMAT "x(20)"
    FIELD ttLabel        AS CHARACTER LABEL "Label"         FORMAT "x(20)"
    FIELD ttType         AS CHARACTER LABEL "Type"          FORMAT "x(10)"
    FIELD ttFormat       AS CHARACTER LABEL "Format"        FORMAT "x(20)"
    FIELD ttWidth        AS INTEGER   LABEL "Width"         FORMAT ">>>9"
    FIELD ttSize         AS INTEGER   LABEL "Size"          FORMAT ">>>>9"
        INDEX ttSubject IS PRIMARY ttOrder
        .

DEFINE TEMP-TABLE ttPageHeader NO-UNDO
    FIELD phOrder        AS INTEGER   LABEL "Order"         FORMAT ">>>9"
    FIELD phName         AS CHARACTER LABEL "Name"          FORMAT "x(30)"
    FIELD phCaption      AS CHARACTER LABEL "Caption"       FORMAT "x(20)"
    FIELD phLeft         AS INTEGER   LABEL "Position"      FORMAT ">>>>>9"
    FIELD phWidth        AS INTEGER   LABEL "Width"         FORMAT ">>>9"
    FIELD phAlignment    AS INTEGER   LABEL "Alignment"     FORMAT "9"
    FIELD phSectionItem  AS INTEGER   LABEL "SectionItem"   FORMAT ">>9"
    FIELD phControlItem  AS INTEGER   LABEL "ControlItem"   FORMAT ">>9"
        INDEX ttPageHeader IS PRIMARY phOrder
        .

DEFINE TEMP-TABLE ttDetail NO-UNDO
    FIELD dtOrder        AS INTEGER   LABEL "Order"         FORMAT ">>>9"
    FIELD dtName         AS CHARACTER LABEL "Name"          FORMAT "x(30)"
    FIELD dtDataField    AS CHARACTER LABEL "DataField"     FORMAT "x(20)"
    FIELD dtLeft         AS INTEGER   LABEL "Position"      FORMAT ">>>>>9"
    FIELD dtWidth        AS INTEGER   LABEL "Width"         FORMAT ">>>>>9"
    FIELD dtAlignment    AS INTEGER   LABEL "Alignment"     FORMAT "9"
    FIELD dtOutputFormat AS CHARACTER LABEL "OutputFormat"  FORMAT "x(30)"
    FIELD dtSectionItem  AS INTEGER   LABEL "SectionItem"   FORMAT ">>9"
    FIELD dtControlItem  AS INTEGER   LABEL "ControlItem"   FORMAT ">>9"
        INDEX ttDetail IS PRIMARY dtOrder
        .

DEFINE TEMP-TABLE ttSection NO-UNDO
    FIELD secOrder        AS INTEGER   LABEL "Order"        FORMAT ">>>9"
    FIELD secType         AS INTEGER   LABEL "Type"         FORMAT ">9"
    FIELD secSection      AS CHARACTER LABEL "Section"      FORMAT "x(30)"
    FIELD secName         AS CHARACTER LABEL "Name"         FORMAT "x(40)"
    FIELD secDataField    AS CHARACTER LABEL "DataField"    FORMAT "x(20)"
    FIELD secLeft         AS INTEGER   LABEL "Position"     FORMAT ">>>>>9"
    FIELD secWidth        AS INTEGER   LABEL "Width"        FORMAT ">>>9"
    FIELD secAlignment    AS INTEGER   LABEL "Alignment"    FORMAT "9"
    FIELD secOutputFormat AS CHARACTER LABEL "OutputFormat" FORMAT "x(30)"
    FIELD secSectionItem  AS INTEGER   LABEL "SectionItem"  FORMAT ">>9"
    FIELD secControlItem  AS INTEGER   LABEL "ControlItem"  FORMAT ">>9"
        INDEX ttSection IS PRIMARY secOrder
        .

DEFINE TEMP-TABLE ttParameter NO-UNDO
    FIELD pOrder          AS INTEGER   LABEL "Order"        FORMAT ">>>9"
    FIELD pName           AS CHARACTER LABEL "Name"         FORMAT "x(27)"
    FIELD pCaption        AS CHARACTER LABEL "Caption"      FORMAT "x(40)"
    FIELD pWidth          AS INTEGER   LABEL "Width"        FORMAT ">>>9"
    FIELD pSectionItem    AS INTEGER   LABEL "SectionItem"  FORMAT ">>9"
    FIELD pControlItem    AS INTEGER   LABEL "ControlItem"  FORMAT ">>9"
        INDEX ttParameter IS PRIMARY pOrder
        .

DEFINE TEMP-TABLE ttAOA NO-UNDO
    FIELD module  AS CHARACTER LABEL "Module"  FORMAT "x(2)"
    FIELD aoaFile AS CHARACTER
    FIELD progID  AS CHARACTER LABEL "Prog ID" FORMAT "x(15)"
    FIELD menuID  AS CHARACTER
        INDEX aoa IS PRIMARY UNIQUE module progID
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME ttDetail

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDetail ttPageHeader ttParameter ttAOA ~
ttSection ttSubject

/* Definitions for BROWSE ttDetail                                      */
&Scoped-define FIELDS-IN-QUERY-ttDetail ttDetail.dtOrder ttDetail.dtName ttDetail.dtDataField ttDetail.dtLeft ttDetail.dtWidth ttDetail.dtOutputFormat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttDetail   
&Scoped-define SELF-NAME ttDetail
&Scoped-define QUERY-STRING-ttDetail FOR EACH ttDetail
&Scoped-define OPEN-QUERY-ttDetail OPEN QUERY {&SELF-NAME} FOR EACH ttDetail.
&Scoped-define TABLES-IN-QUERY-ttDetail ttDetail
&Scoped-define FIRST-TABLE-IN-QUERY-ttDetail ttDetail


/* Definitions for BROWSE ttPageHeader                                  */
&Scoped-define FIELDS-IN-QUERY-ttPageHeader ttPageHeader.phOrder ttPageHeader.phName ttPageHeader.phCaption ttPageHeader.phLeft ttPageHeader.phWidth   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttPageHeader   
&Scoped-define SELF-NAME ttPageHeader
&Scoped-define QUERY-STRING-ttPageHeader FOR EACH ttPageHeader
&Scoped-define OPEN-QUERY-ttPageHeader OPEN QUERY {&SELF-NAME} FOR EACH ttPageHeader.
&Scoped-define TABLES-IN-QUERY-ttPageHeader ttPageHeader
&Scoped-define FIRST-TABLE-IN-QUERY-ttPageHeader ttPageHeader


/* Definitions for BROWSE ttParameter                                   */
&Scoped-define FIELDS-IN-QUERY-ttParameter ttParameter.pOrder ttParameter.pName ttParameter.pCaption   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttParameter   
&Scoped-define SELF-NAME ttParameter
&Scoped-define QUERY-STRING-ttParameter FOR EACH ttParameter
&Scoped-define OPEN-QUERY-ttParameter OPEN QUERY {&SELF-NAME} FOR EACH ttParameter.
&Scoped-define TABLES-IN-QUERY-ttParameter ttParameter
&Scoped-define FIRST-TABLE-IN-QUERY-ttParameter ttParameter


/* Definitions for BROWSE ttProgID                                      */
&Scoped-define FIELDS-IN-QUERY-ttProgID ttAOA.module ttAOA.progID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttProgID   
&Scoped-define SELF-NAME ttProgID
&Scoped-define QUERY-STRING-ttProgID FOR EACH ttAOA
&Scoped-define OPEN-QUERY-ttProgID OPEN QUERY {&SELF-NAME} FOR EACH ttAOA.
&Scoped-define TABLES-IN-QUERY-ttProgID ttAOA
&Scoped-define FIRST-TABLE-IN-QUERY-ttProgID ttAOA


/* Definitions for BROWSE ttSection                                     */
&Scoped-define FIELDS-IN-QUERY-ttSection ttSection.secSection ttSection.secName ttSection.secDataField ttSection.secLeft ttSection.secWidth ttSection.secOutputFormat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttSection   
&Scoped-define SELF-NAME ttSection
&Scoped-define QUERY-STRING-ttSection FOR EACH ttSection
&Scoped-define OPEN-QUERY-ttSection OPEN QUERY {&SELF-NAME} FOR EACH ttSection.
&Scoped-define TABLES-IN-QUERY-ttSection ttSection
&Scoped-define FIRST-TABLE-IN-QUERY-ttSection ttSection


/* Definitions for BROWSE ttSubject                                     */
&Scoped-define FIELDS-IN-QUERY-ttSubject ttSubject.ttOrder ttSubject.ttField ttSubject.ttLabel ttSubject.ttType ttSubject.ttFormat ttSubject.ttWidth ttSubject.ttSize   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttSubject   
&Scoped-define SELF-NAME ttSubject
&Scoped-define QUERY-STRING-ttSubject FOR EACH ttSubject
&Scoped-define OPEN-QUERY-ttSubject OPEN QUERY {&SELF-NAME} FOR EACH ttSubject.
&Scoped-define TABLES-IN-QUERY-ttSubject ttSubject
&Scoped-define FIRST-TABLE-IN-QUERY-ttSubject ttSubject


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-ttDetail}~
    ~{&OPEN-QUERY-ttPageHeader}~
    ~{&OPEN-QUERY-ttParameter}~
    ~{&OPEN-QUERY-ttProgID}~
    ~{&OPEN-QUERY-ttSection}~
    ~{&OPEN-QUERY-ttSubject}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOpenRPA btnSetName btnSave btnPublish ~
ttProgID ttPageHeader ttDetail autoSetParameters ttParameter ~
btnOnReportStart btnGroupHeaderOnFormat btnGroupFooterOnFormat ~
btnDetailOnFormat btnOnReportEnd btnUpdate ttSubject ttSection 
&Scoped-Define DISPLAYED-OBJECTS aoaProgramID aoaReportWidth aoaRptFile ~
aoaReportTitle autoSetParameters 

/* Custom List Definitions                                              */
/* aoaReportValue,List-2,List-3,List-4,List-5,List-6                    */
&Scoped-define aoaReportValue aoaProgramID aoaReportWidth aoaRptFile ~
aoaReportTitle 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetScript C-Win 
FUNCTION fGetScript RETURNS CHARACTER
  ( ipScriptDatFile AS CHARACTER, ipSection AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDetailOnFormat 
     LABEL "&Detail OnFormat" 
     SIZE 25 BY 1.19.

DEFINE BUTTON btnGroupFooterOnFormat 
     LABEL "Group &Footer OnFormat" 
     SIZE 25 BY 1.19.

DEFINE BUTTON btnGroupHeaderOnFormat 
     LABEL "Group &Header OnFormat" 
     SIZE 25 BY 1.19.

DEFINE BUTTON btnOnReportEnd 
     LABEL "OnReport&End" 
     SIZE 25 BY 1.19.

DEFINE BUTTON btnOnReportStart 
     LABEL "On&ReportStart" 
     SIZE 25 BY 1.19.

DEFINE BUTTON btnOpenRPA 
     LABEL "..." 
     SIZE 4.2 BY 1.

DEFINE BUTTON btnPublish 
     LABEL "&Publish" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSave 
     LABEL "&Save" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSetName 
     LABEL "Set &Names" 
     SIZE 14 BY 1.

DEFINE BUTTON btnSubReport 
     LABEL "SubRpt: None" 
     SIZE 25 BY 1.

DEFINE BUTTON btnUpdate 
     LABEL "Update Scripts && Publish" 
     SIZE 27 BY 1.

DEFINE VARIABLE aoaProgramID AS CHARACTER FORMAT "X(256)":U 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE aoaReportTitle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Report Title" 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE aoaReportWidth AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Report Width" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE aoaRptFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "AOA Report File" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE autoSetParameters AS LOGICAL INITIAL no 
     LABEL "Auto Set Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttDetail FOR 
      ttDetail SCROLLING.

DEFINE QUERY ttPageHeader FOR 
      ttPageHeader SCROLLING.

DEFINE QUERY ttParameter FOR 
      ttParameter SCROLLING.

DEFINE QUERY ttProgID FOR 
      ttAOA SCROLLING.

DEFINE QUERY ttSection FOR 
      ttSection SCROLLING.

DEFINE QUERY ttSubject FOR 
      ttSubject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttDetail C-Win _FREEFORM
  QUERY ttDetail DISPLAY
      ttDetail.dtOrder
    ttDetail.dtName
    ttDetail.dtDataField
    ttDetail.dtLeft
    ttDetail.dtWidth
    ttDetail.dtOutputFormat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 20.95
         TITLE "Detail Section".

DEFINE BROWSE ttPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttPageHeader C-Win _FREEFORM
  QUERY ttPageHeader DISPLAY
      ttPageHeader.phOrder
    ttPageHeader.phName
    ttPageHeader.phCaption
    ttPageHeader.phLeft
    ttPageHeader.phWidth
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 76 BY 20.95
         TITLE "PageHeader Section".

DEFINE BROWSE ttParameter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttParameter C-Win _FREEFORM
  QUERY ttParameter DISPLAY
      ttParameter.pOrder
     ttParameter.pName
     ttParameter.pCaption
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 39.29
         TITLE "Parameter (ReportHeader Section)".

DEFINE BROWSE ttProgID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttProgID C-Win _FREEFORM
  QUERY ttProgID DISPLAY
      ttAOA.module
    ttAOA.progID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 28 BY 19.76
         TITLE "Prog ID".

DEFINE BROWSE ttSection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttSection C-Win _FREEFORM
  QUERY ttSection DISPLAY
      ttSection.secSection
ttSection.secName
ttSection.secDataField
ttSection.secLeft
ttSection.secWidth
ttSection.secOutputFormat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 18.1
         TITLE "SubTotal/Total Sections".

DEFINE BROWSE ttSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttSubject C-Win _FREEFORM
  QUERY ttSubject DISPLAY
      ttSubject.ttOrder
    ttSubject.ttField
    ttSubject.ttLabel
    ttSubject.ttType
    ttSubject.ttFormat
    ttSubject.ttWidth
    ttSubject.ttSize
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 18.1
         TITLE "Business Subject Temp-Table".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     aoaProgramID AT ROW 1.24 COL 4 COLON-ALIGNED WIDGET-ID 18
     btnOpenRPA AT ROW 1.24 COL 25 WIDGET-ID 16
     btnSetName AT ROW 1.24 COL 31 WIDGET-ID 4
     btnSave AT ROW 1.24 COL 48 WIDGET-ID 6
     btnPublish AT ROW 1.24 COL 65 WIDGET-ID 20
     aoaReportWidth AT ROW 1.24 COL 94 COLON-ALIGNED WIDGET-ID 10
     aoaRptFile AT ROW 1.24 COL 123 COLON-ALIGNED WIDGET-ID 2
     btnSubReport AT ROW 1.24 COL 217 WIDGET-ID 28
     aoaReportTitle AT ROW 1.24 COL 253 COLON-ALIGNED WIDGET-ID 8
     ttProgID AT ROW 2.43 COL 1 WIDGET-ID 700
     ttPageHeader AT ROW 2.43 COL 30 WIDGET-ID 300
     ttDetail AT ROW 2.43 COL 107 WIDGET-ID 400
     autoSetParameters AT ROW 2.43 COL 218 HELP
          "Auto Set Parameters" WIDGET-ID 32
     ttParameter AT ROW 2.43 COL 242 WIDGET-ID 600
     btnOnReportStart AT ROW 3.38 COL 217 WIDGET-ID 14
     btnGroupHeaderOnFormat AT ROW 4.81 COL 217 WIDGET-ID 22
     btnGroupFooterOnFormat AT ROW 6.24 COL 217 WIDGET-ID 24
     btnDetailOnFormat AT ROW 7.67 COL 217 WIDGET-ID 12
     btnOnReportEnd AT ROW 9.1 COL 217 WIDGET-ID 26
     btnUpdate AT ROW 22.43 COL 2 WIDGET-ID 30
     ttSubject AT ROW 23.62 COL 1 WIDGET-ID 200
     ttSection AT ROW 23.62 COL 99 WIDGET-ID 500
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 320 BY 40.71 WIDGET-ID 100.


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
         TITLE              = "AOA Report Modifier"
         HEIGHT             = 40.71
         WIDTH              = 320
         MAX-HEIGHT         = 40.71
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 40.71
         VIRTUAL-WIDTH      = 320
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
/* BROWSE-TAB ttProgID aoaReportTitle DEFAULT-FRAME */
/* BROWSE-TAB ttPageHeader ttProgID DEFAULT-FRAME */
/* BROWSE-TAB ttDetail ttPageHeader DEFAULT-FRAME */
/* BROWSE-TAB ttParameter autoSetParameters DEFAULT-FRAME */
/* BROWSE-TAB ttSubject btnUpdate DEFAULT-FRAME */
/* BROWSE-TAB ttSection ttSubject DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN aoaProgramID IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN aoaReportTitle IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN aoaReportWidth IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN aoaRptFile IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON btnSubReport IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttDetail
/* Query rebuild information for BROWSE ttDetail
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDetail.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttDetail */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttPageHeader
/* Query rebuild information for BROWSE ttPageHeader
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttPageHeader.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttPageHeader */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttParameter
/* Query rebuild information for BROWSE ttParameter
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParameter.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttParameter */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttProgID
/* Query rebuild information for BROWSE ttProgID
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAOA.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttProgID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttSection
/* Query rebuild information for BROWSE ttSection
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSection.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttSection */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttSubject
/* Query rebuild information for BROWSE ttSubject
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubject.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttSubject */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AOA Report Modifier */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AOA Report Modifier */
DO:
  /* This event will close the window and terminate the procedure.  */
  
  RELEASE OBJECT PAReportEngine.
  RELEASE OBJECT PAApplication.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME autoSetParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL autoSetParameters C-Win
ON VALUE-CHANGED OF autoSetParameters IN FRAME DEFAULT-FRAME /* Auto Set Parameters */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetailOnFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetailOnFormat C-Win
ON CHOOSE OF btnDetailOnFormat IN FRAME DEFAULT-FRAME /* Detail OnFormat */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoa\vbScript\Rpt.Detail.OnFormat.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGroupFooterOnFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupFooterOnFormat C-Win
ON CHOOSE OF btnGroupFooterOnFormat IN FRAME DEFAULT-FRAME /* Group Footer OnFormat */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoa\vbScript\Rpt.GroupFooter.OnFormat.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGroupHeaderOnFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupHeaderOnFormat C-Win
ON CHOOSE OF btnGroupHeaderOnFormat IN FRAME DEFAULT-FRAME /* Group Header OnFormat */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoa\vbScript\Rpt.GroupHeader.OnFormat.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOnReportEnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOnReportEnd C-Win
ON CHOOSE OF btnOnReportEnd IN FRAME DEFAULT-FRAME /* OnReportEnd */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoa\vbScript\Rpt.OnReportEnd.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOnReportStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOnReportStart C-Win
ON CHOOSE OF btnOnReportStart IN FRAME DEFAULT-FRAME /* OnReportStart */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoa\vbScript\Rpt.OnReportStart.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenRPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenRPA C-Win
ON CHOOSE OF btnOpenRPA IN FRAME DEFAULT-FRAME /* ... */
DO:
    SYSTEM-DIALOG GET-FILE cID
      TITLE      "Choose Report to Open ..."
      FILTERS    "AOA Report Files (*.p)" "*.p"
      MUST-EXIST       
      USE-FILENAME
      UPDATE lOKpressed.

    IF lOKpressed THEN DO:
        cID = SEARCH(cID).
        RUN pOpenAOAProgram (cID).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPublish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPublish C-Win
ON CHOOSE OF btnPublish IN FRAME DEFAULT-FRAME /* Publish */
DO:
    RUN pPublish (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    ASSIGN {&aoaReportValue}
        hReport:Sections:Item("ReportHeader"):Controls:Item("ReportTitle"):Caption = aoaReportTitle
        hReport:Sections:Item("ReportHeader"):Controls:Item("ReportTitle"):Width   = 11520
        hReport:PrintWidth = aoaReportWidth
        .
    RUN pSetScript.
    RUN pSetReportFields.
    PAReportEngine:SaveReport(INPUT-OUTPUT hReport,aoaRptFile).
    MESSAGE "Report:" aoaReportTitle "Updates Saved" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSetName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetName C-Win
ON CHOOSE OF btnSetName IN FRAME DEFAULT-FRAME /* Set Names */
DO:
  RUN pSetNames.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubReport C-Win
ON CHOOSE OF btnSubReport IN FRAME DEFAULT-FRAME /* SubRpt: None */
DO:
    DEFINE VARIABLE hSubReport AS HANDLE NO-UNDO.
    /*hSubReport = PAReportEngine:SubReport.*/
    MESSAGE VALID-HANDLE(hSubReport) PAReportEngine:SubReportName
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    /*
    RUN aoa/aoaSubRpt.w (aoaProgramID, {&SELF-NAME}:PRIVATE-DATA).
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME DEFAULT-FRAME /* Update Scripts  Publish */
DO:
  RUN pUpdate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttProgID
&Scoped-define SELF-NAME ttProgID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttProgID C-Win
ON DEFAULT-ACTION OF ttProgID IN FRAME DEFAULT-FRAME /* Prog ID */
DO:
    ASSIGN
        cID = "aoa/" + ttAOA.progID + "p"
        cID = SEARCH(cID)
        .
    RUN pOpenAOAProgram (ttAOA.module, cID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttDetail
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
  RUN pReSize.
  RUN pCreateObjects.
  RUN pGetAOAFiles.
  RUN enable_UI.
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
  DISPLAY aoaProgramID aoaReportWidth aoaRptFile aoaReportTitle 
          autoSetParameters 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnOpenRPA btnSetName btnSave btnPublish ttProgID ttPageHeader 
         ttDetail autoSetParameters ttParameter btnOnReportStart 
         btnGroupHeaderOnFormat btnGroupFooterOnFormat btnDetailOnFormat 
         btnOnReportEnd btnUpdate ttSubject ttSection 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateObjects C-Win 
PROCEDURE pCreateObjects :
/*------------------------------------------------------------------------------
  Purpose:     Create the DataPA Application and DataPA Report Engine objects.
  Parameters:  <none>
  Notes:       This procedure creates its own DataPA Application object
               and assigns it to the PAApplication object. 
               This is for simplicity in this example, however it is recomended 
               that any application creates a single Application object for each 
               session and shares this with any procedures that use it. 
               For more details see http://support.datapa.com/ics/support/default.asp?deptID=5554&task=knowledge&questionID=15
------------------------------------------------------------------------------*/
    /* Create Objects */
    CREATE "DataPA.Application" PAApplication.
    CREATE "DataPAReportsEng.ManagedReport" PAReportEngine.
    PAReportEngine:ENABLE-EVENTS("Report").

    ASSIGN
        /* Prepare Application Object */
        PAApplication:Silent             = TRUE
        /* Prepare Report Engine */
        PAReportEngine:Subjectsrecordset = PAApplication:SubjectsRecordset
        PAReportEngine:LinksRecordset    = PAApplication:LinksRecordset
        PAReportEngine:HideErrorMessage  = TRUE
        PAReportEngine:Drilldown         = -1
        PAReportEngine:DrilldownPath     = ""
        PAReportEngine:Silent            = TRUE
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetAOAFiles C-Win 
PROCEDURE pGetAOAFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cModule   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAOAFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProgID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMenuID   AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = "aoa/datFiles/Report.dat".
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        IMPORT cModule cAOAFile cProgID cMenuID.
        CREATE ttAOA.
        ASSIGN
            ttAOA.module  = cModule
            ttAOA.aoaFile = cAOAFile
            ttAOA.progID  = cProgID
            ttAOA.menuID  = cMenuID
            .
    END. /* repeat */
    INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetReportFields C-Win 
PROCEDURE pGetReportFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcReportName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iOrder     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ix         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iy         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDataField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCaption   AS CHARACTER NO-UNDO.
    
    PAReportEngine:OpenReport(ipcReportName).
    hReport = PAReportEngine:Report.
    
    IF NOT VALID-HANDLE(hReport) THEN DO:
        MESSAGE "Report:" ipcReportName "- Failed to Open"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    EMPTY TEMP-TABLE ttPageHeader.
    EMPTY TEMP-TABLE ttDetail.
    EMPTY TEMP-TABLE ttSection.
    EMPTY TEMP-TABLE ttParameter.

    aoaReportWidth = hReport:PrintWidth.
    DISPLAY aoaReportWidth WITH FRAME {&FRAME-NAME}.
    DO ix = 0 TO hReport:Sections:Count - 1:
        cName = hReport:Sections:Item(ix):Name.
        DO iy = 0 TO hReport:Sections:Item(ix):Controls:Count - 1:
            IF hReport:Sections:Item(ix):Controls:Item(iy):Name BEGINS "SubRpt_" THEN DO WITH FRAME {&FRAME-NAME}:
                ASSIGN
                    btnSubReport:PRIVATE-DATA = hReport:Sections:Item(ix):Controls:Item(iy):ReportName
                    btnSubReport:LABEL = "SubRpt: " + btnSubReport:PRIVATE-DATA
                    .
                NEXT.
            END. /* if sub report */
            CASE cName:
                WHEN "ReportHeader" THEN DO:
                    IF hReport:Sections:Item(ix):Controls:Item(iy):Name EQ "ParametersLabel" THEN NEXT.
                    CREATE ttParameter.
                    ASSIGN
                        ttParameter.pOrder       = iOrder
                        ttParameter.pName        = hReport:Sections:Item(ix):Controls:Item(iy):Name
                        ttParameter.pCaption     = hReport:Sections:Item(ix):Controls:Item(iy):Caption
                        ttParameter.pSectionItem = ix
                        ttParameter.pControlItem = iy
                        iOrder                   = iOrder + 1
                        .
                END. /* report header */
                WHEN "PageHeader" THEN DO:
                    ASSIGN
                        cCaption = hReport:Sections:Item(ix):Controls:Item(iy):Caption
                        idx = INDEX(cCaption,CHR(183))
                        .
                    IF idx NE 0 THEN
                    cCaption = SUBSTR(cCaption,idx + 1).
                    CREATE ttPageHeader.
                    ASSIGN
                        ttPageHeader.phName        = hReport:Sections:Item(ix):Controls:Item(iy):Name
                        ttPageHeader.phCaption     = cCaption
                        ttPageHeader.phLeft        = hReport:Sections:Item(ix):Controls:Item(iy):Left 
                        ttPageHeader.phWidth       = hReport:Sections:Item(ix):Controls:Item(iy):Width
                        ttPageHeader.phAlignment   = hReport:Sections:Item(ix):Controls:Item(iy):Alignment
                        ttPageHeader.phSectionItem = ix
                        ttPageHeader.phControlItem = iy
                        .
                    FIND FIRST ttSubject WHERE ttSubject.ttLabel EQ ttPageHeader.phCaption NO-ERROR.
                    IF AVAILABLE ttSubject THEN
                    ttPageHeader.phOrder = ttSubject.ttOrder.
                END. /* page header */
                WHEN "Detail" THEN DO:
                    IF iy EQ 0 THEN NEXT.
                    ASSIGN
                        cDataField = hReport:Sections:Item(ix):Controls:Item(iy):DataField
                        idx = INDEX(cDataField,CHR(183))
                        .
                    IF idx NE 0 THEN
                    cDataField = SUBSTR(cDataField,idx + 1).
                    CREATE ttDetail.
                    ASSIGN
                        ttDetail.dtName         = hReport:Sections:Item(ix):Controls:Item(iy):Name
                        ttDetail.dtDataField    = cDataField
                        ttDetail.dtLeft         = hReport:Sections:Item(ix):Controls:Item(iy):Left
                        ttDetail.dtWidth        = hReport:Sections:Item(ix):Controls:Item(iy):Width
                        ttDetail.dtAlignment    = hReport:Sections:Item(ix):Controls:Item(iy):Alignment
                        ttDetail.dtOutputFormat = hReport:Sections:Item(ix):Controls:Item(iy):OutputFormat
                        ttDetail.dtSectionItem  = ix
                        ttDetail.dtControlItem  = iy
                        .
                    FIND FIRST ttSubject WHERE ttSubject.ttLabel EQ ttDetail.dtDataField NO-ERROR.
                    IF AVAILABLE ttSubject THEN
                    ttDetail.dtOrder = ttSubject.ttOrder.
                END. /* detail */
            END CASE.
            IF (hReport:Sections:Item(ix):Type EQ 1  OR
                hReport:Sections:Item(ix):Type EQ 4  OR
                hReport:Sections:Item(ix):Type EQ 5) AND
                hReport:Sections:Item(ix):Controls:Item(iy):MultiLine EQ FALSE THEN DO:
                CREATE ttSection.
                ASSIGN
                    ttSection.secOrder        = ix
                    ttSection.secSection      = cName
                    ttSection.secType         = hReport:Sections:Item(ix):Type
                    ttSection.secName         = hReport:Sections:Item(ix):Controls:Item(iy):Name
                    ttSection.secDataField    = hReport:Sections:Item(ix):Controls:Item(iy):DataField
                    ttSection.secLeft         = hReport:Sections:Item(ix):Controls:Item(iy):Left
                    ttSection.secWidth        = hReport:Sections:Item(ix):Controls:Item(iy):Width
                    ttSection.secAlignment    = hReport:Sections:Item(ix):Controls:Item(iy):Alignment
                    ttSection.secOutputFormat = hReport:Sections:Item(ix):Controls:Item(iy):OutputFormat
                    ttSection.secSectionItem  = ix
                    ttSection.secControlItem  = iy
                    .
            END. /* group footer/header/total */
        END. /* do iy */
    END. /* do ix */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTempTableFields C-Win 
PROCEDURE pGetTempTableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphTable AS HANDLE NO-UNDO.

    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
    
    IF NOT VALID-HANDLE(iphTable) THEN RETURN.

    EMPTY TEMP-TABLE ttSubject.

    iphTable = iphTable:DEFAULT-BUFFER-HANDLE.
    DO idx = 1 TO iphTable:NUM-FIELDS:
        IF CAN-DO("RECID,ROWID",iphTable:BUFFER-FIELD(idx):DATA-TYPE) THEN NEXT.
        IF iphTable:BUFFER-FIELD(idx):NAME EQ "rowType"     THEN NEXT.
        IF iphTable:BUFFER-FIELD(idx):NAME EQ "parameters"  THEN NEXT.
        IF iphTable:BUFFER-FIELD(idx):NAME EQ "recDataType" THEN NEXT.
        CREATE ttSubject.
        ASSIGN
            iOrder             = iOrder + 1
            ttSubject.ttOrder  = iOrder
            ttSubject.ttField  = iphTable:BUFFER-FIELD(idx):NAME
            ttSubject.ttLabel  = iphTable:BUFFER-FIELD(idx):LABEL
            ttSubject.ttType   = iphTable:BUFFER-FIELD(idx):DATA-TYPE
            ttSubject.ttFormat = iphTable:BUFFER-FIELD(idx):FORMAT
            ttSubject.ttWidth  = iphTable:BUFFER-FIELD(idx):WIDTH
            ttSubject.ttSize   = MAX(iphTable:BUFFER-FIELD(idx):WIDTH,
                              LENGTH(iphTable:BUFFER-FIELD(idx):LABEL)) * {&aoaMultiplier}
            .
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOpenAOAProgram C-Win 
PROCEDURE pOpenAOAProgram :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcModule AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScopDef AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAppSrv  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hAppSrv  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable   AS HANDLE    NO-UNDO.

    FILE-INFO:FILE-NAME = ipcID.
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        cTxt = "".
        IMPORT UNFORMATTED cTxt.
        IF NUM-ENTRIES(cTxt," ") LT 3 THEN NEXT.
        cScopDef = ENTRY(2,cTxt," ").
        CASE cScopDef:
            WHEN "aoaProgramID" THEN
            aoaProgramID = ENTRY(3,cTxt," ").
            WHEN "aoaType" THEN
            IF ENTRY(3,cTxt," ") NE "Report" THEN DO:
                MESSAGE "Program not an ~"AOA Report~""
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            WHEN "aoaTitle" THEN
            ASSIGN
                ENTRY(1,cTxt," ") = ""
                ENTRY(2,cTxt," ") = ""
                aoaReportTitle = TRIM(cTxt)
                aoaRptFile = SEARCH("aoa\reports\" + aoaReportTitle + ".rpa")
                cAppSrv = "aoa\appServer\aoa" + ipcModule + ".p"
                .
        END CASE.
    END. /* repeat */
    INPUT CLOSE.
    DISPLAY {&aoaReportValue} WITH FRAME {&FRAME-NAME}.

    RUN VALUE(cAppSrv) PERSISTENT SET hAppSrv.
    hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
    
    RUN pGetTempTableFields (hTable).
    RUN pGetReportFields (aoaRptFile).

    {&OPEN-QUERY-ttPageHeader}
    {&OPEN-QUERY-ttDetail}
    {&OPEN-QUERY-ttSubject}
    {&OPEN-QUERY-ttSection}
    {&OPEN-QUERY-ttParameter}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPublish C-Win 
PROCEDURE pPublish :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplShow AS LOGICAL NO-UNDO.

    DEFINE VARIABLE publishBat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE publishExe AS CHARACTER NO-UNDO.
    DEFINE VARIABLE publishLog AS CHARACTER NO-UNDO.
    DEFINE VARIABLE logText    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE txtLine    AS CHARACTER NO-UNDO.

    ASSIGN
        publishBat = SEARCH("aoa\publish\PublishReport.bat")
        publishExe = SEARCH("aoa\publish\PublishReport.exe")
        publishLog = REPLACE(publishBat,".bat",".log")
        .

    FILE-INFO:FILE-NAME = publishBat.
    OUTPUT TO VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    PUT UNFORMATTED
        publishExe " ~"" aoaRptFile "~" guest password > "
        REPLACE(publishBat,".bat",".log") SKIP.
    OUTPUT CLOSE.
    
    SESSION:SET-WAIT-STATE("General").
    OS-COMMAND SILENT VALUE(publishBat).
    SESSION:SET-WAIT-STATE("").

    FILE-INFO:FILE-NAME = publishLog.
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
       IMPORT UNFORMATTED txtLine.
       IF logText NE "" THEN logText = logText + CHR(10).
       logText = logText + txtLine.
    END.
    INPUT CLOSE.
    
    IF iplShow THEN DO:
        IF logText EQ "success" THEN
            MESSAGE "Published OK" VIEW-AS ALERT-BOX.
        ELSE
            MESSAGE "Published Failed: " + logText VIEW-AS ALERT-BOX ERROR.
    END. /* if iplshow */
    ELSE IF logText NE "success" THEN
         MESSAGE "Published Failed: " + logText
             VIEW-AS ALERT-BOX ERROR
             TITLE aoaRptFile.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReSize C-Win 
PROCEDURE pReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iDiff AS INTEGER NO-UNDO.

    ASSIGN
        {&WINDOW-NAME}:WINDOW-STATE = 1
        {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 50
        {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        iDiff = {&WINDOW-NAME}:HEIGHT-PIXELS - FRAME {&FRAME-NAME}:HEIGHT-PIXELS
        FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
        ttSubject:HEIGHT-PIXELS = ttSubject:HEIGHT-PIXELS + iDiff
        ttSection:HEIGHT-PIXELS = ttSection:HEIGHT-PIXELS + iDiff
        ttParameter:HEIGHT-PIXELS = ttParameter:HEIGHT-PIXELS + iDiff
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetNames C-Win 
PROCEDURE pSetNames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLeft   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.

    FOR EACH ttSubject:
        cName = CAPS(SUBSTR(ttSubject.ttField,1,1)) + SUBSTR(ttSubject.ttField,2).
        FIND FIRST ttPageHeader
             WHERE ttPageHeader.phOrder EQ ttSubject.ttOrder
             NO-ERROR.
        IF AVAILABLE ttPageHeader THEN DO:
            ASSIGN
                ttPageHeader.phName      = ttSubject.ttField + "_PageHeader"
                ttPageHeader.phCaption   = ttSubject.ttLabel
                ttPageHeader.phWidth     = ttSubject.ttSize
                ttPageHeader.phLeft      = iLeft
                .
            IF NOT CAN-DO("Character,Date",ttSubject.ttType) THEN
            ttPageHeader.phAlignment = 1.
        END. /* avail ttpageheader */
        FIND FIRST ttDetail
             WHERE ttDetail.dtOrder EQ ttSubject.ttOrder
             NO-ERROR.
        IF AVAILABLE ttDetail THEN DO:
            ASSIGN
                ttDetail.dtName         = ttSubject.ttField + "_Detail"
                ttDetail.dtDataField    = ttSubject.ttLabel
                ttDetail.dtWidth        = ttSubject.ttSize
                ttDetail.dtLeft         = iLeft
                ttDetail.dtOutputFormat = ""
                .
            IF ttSubject.ttType NE "Character" THEN DO:
                cFormat = "mm/dd/yyyy".
                IF ttSubject.ttType NE "Date" THEN DO:
                    ASSIGN
                        ttDetail.dtAlignment = 1
                        cFormat = REPLACE(ttSubject.ttFormat,">","#")
                        cFormat = REPLACE(cFormat,"<","#")
                        cFormat = REPLACE(cFormat,"9","0")
                        .
                    IF INDEX(cFormat,"-") NE 0 THEN
                    ASSIGN
                        cFormat = REPLACE(cFormat,"-","")
                        cFormat = cFormat + ";(" + cFormat + ")"
                        .
                END. /* if not date */
                ttDetail.dtOutputFormat = cFormat.
            END. /* if not character */
        END. /* avail ttdetail */
        FOR EACH ttSection
            WHERE (ttSection.secType EQ 1 OR ttSection.secType EQ 5)
              AND  ttSection.secDataField EQ ttSubject.ttLabel
            :
            ASSIGN
                ttSection.secName         = ttSubject.ttField + "_" + ttSection.secSection
                ttSection.secDataField    = ttSubject.ttLabel
                ttSection.secWidth        = ttSubject.ttSize
                ttSection.secLeft         = iLeft
                ttSection.secAlignment    = 1
                ttSection.secOutputFormat = cFormat
                .
        END. /* each ttsection */
        IF AVAILABLE ttPageHeader OR AVAILABLE ttDetail THEN
        iLeft = iLeft + ttSubject.ttSize + 50.
    END. /* each subject */
    
    IF autoSetParameters THEN DO:
        FIND FIRST ttParameter WHERE ttParameter.pOrder EQ 0 NO-ERROR.
        IF AVAILABLE ttParameter THEN
        ASSIGN
            ttParameter.pCaption = aoaReportTitle
            ttParameter.pWidth   = LENGTH(aoaReportTitle) * 180.
        
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ "001"
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ "NoSweat"
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ "001"
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ "ASI"
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF AVAILABLE user-print THEN DO:
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF user-print.field-name[idx] EQ "" THEN LEAVE.
                IF CAN-DO("svTitle,svAvailableColumns,svSelectedColumns,svShowParameters",user-print.field-name[idx]) THEN NEXT.
                GET NEXT ttParameter.
                IF NOT AVAILABLE ttParameter THEN LEAVE.
                ASSIGN
                    ttParameter.pName = user-print.field-name[idx] + "Label"
                    ttParameter.pCaption = IF user-print.field-label[idx] EQ ? THEN ttParameter.pCaption
                                           ELSE user-print.field-label[idx] + ":"
                    .
                GET NEXT ttParameter.
                IF NOT AVAILABLE ttParameter THEN LEAVE.
                ASSIGN
                    ttParameter.pName    = user-print.field-name[idx]
                    ttParameter.pCaption = user-print.field-name[idx]
                    .
            END. /* do idx */
        END. /* avail user-print */
        ELSE
            MESSAGE "No User-Print Record Exists to Set Parameters"
                VIEW-AS ALERT-BOX ERROR.
    END. /* if setparameternames */
    
    aoaReportWidth:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iLeft).

    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetReportFields C-Win 
PROCEDURE pSetReportFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ix AS INTEGER NO-UNDO.
    DEFINE VARIABLE iy AS INTEGER NO-UNDO.

    FOR EACH ttPageHeader:
        ASSIGN
            ix = ttPageHeader.phSectionItem
            iy = ttPageHeader.phControlItem
            hReport:Sections:Item(ix):Controls:Item(iy):Name         = ttPageHeader.phName
            hReport:Sections:Item(ix):Controls:Item(iy):Caption      = ttPageHeader.phCaption
            hReport:Sections:Item(ix):Controls:Item(iy):Left         = ttPageHeader.phLeft
            hReport:Sections:Item(ix):Controls:Item(iy):Width        = ttPageHeader.phWidth
            hReport:Sections:Item(ix):Controls:Item(iy):Alignment    = ttPageHeader.phAlignment
            hReport:Sections:Item(ix):Controls:Item(iy):Height       = 270
            hReport:Sections:Item(ix):Controls:Item(iy):Top          = 0
            .
    END. /* each ttpageheader */
    FOR EACH ttDetail:
        ASSIGN
            ix = ttDetail.dtSectionItem
            iy = ttDetail.dtControlItem
            hReport:Sections:Item(ix):Controls:Item(iy):Name         = ttDetail.dtName
         /* hReport:Sections:Item(ix):Controls:Item(iy):DataField    = ttDetail.dtDataField */
            hReport:Sections:Item(ix):Controls:Item(iy):Left         = ttDetail.dtLeft
            hReport:Sections:Item(ix):Controls:Item(iy):Width        = ttDetail.dtWidth
            hReport:Sections:Item(ix):Controls:Item(iy):Alignment    = ttDetail.dtAlignment
            hReport:Sections:Item(ix):Controls:Item(iy):OutputFormat = ttDetail.dtOutputFormat
            hReport:Sections:Item(ix):Controls:Item(iy):Height       = 270
            hReport:Sections:Item(ix):Controls:Item(iy):Top          = 0
            .
    END. /* each ttdetail */
    FOR EACH ttSection:
        ASSIGN
            ix = ttSection.secSectionItem
            iy = ttSection.secControlItem
            hReport:Sections:Item(ix):Controls:Item(iy):Name               = ttSection.secName
            hReport:Sections:Item(ix):Controls:Item(iy):DataField          = ttSection.secDataField
            .
        IF ttSection.secType NE 4 THEN
        ASSIGN
            hReport:Sections:Item(ix):Controls:Item(iy):Left               = ttSection.secLeft
            hReport:Sections:Item(ix):Controls:Item(iy):Width              = ttSection.secWidth
            hReport:Sections:Item(ix):Controls:Item(iy):Alignment          = ttSection.secAlignment
            hReport:Sections:Item(ix):Controls:Item(iy):OutputFormat       = ttSection.secOutputFormat
            hReport:Sections:Item(ix):Controls:Item(iy):Height             = 450
            hReport:Sections:Item(ix):Controls:Item(iy):Top                = 0
            hReport:Sections:Item(ix):Controls:Item(iy):VerticalAlignment  = 1
            hReport:Sections:Item(ix):Controls:Item(iy):Border:TopStyle    = IF ttSection.secType EQ 1 THEN 12 ELSE 7
            hReport:Sections:Item(ix):Controls:Item(iy):Border:BottomStyle = IF ttSection.secType EQ 1 THEN 12 ELSE 0
            .
    END. /* each ttsection */
    /* temp rename the object incase parameters moved
       around so to avoid duplicate which causes an error */
    FOR EACH ttParameter:
        ASSIGN
            ix = ttParameter.pSectionItem
            iy = ttParameter.pControlItem
            hReport:Sections:Item(ix):Controls:Item(iy):Name = ttParameter.pName + STRING(iy)
            .
    END. /* each ttparameter */
    FOR EACH ttParameter:
        ASSIGN
            ix = ttParameter.pSectionItem
            iy = ttParameter.pControlItem
            hReport:Sections:Item(ix):Controls:Item(iy):Name    = ttParameter.pName
            hReport:Sections:Item(ix):Controls:Item(iy):Caption = ttParameter.pCaption
            .
        IF ttParameter.pWidth NE 0 THEN
        hReport:Sections:Item(ix):Controls:Item(iy):Width = ttParameter.pWidth.
    END. /* each ttparameter */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetScript C-Win 
PROCEDURE pSetScript :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScript AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.

    ASSIGN
        /* Set OnReportStart Script */
        cScript = hReport:Script
        idx = INDEX(cScript,"Sub OnReportStart")
        idx = IF idx EQ 0 THEN LENGTH(cScript) ELSE idx - 2
        hReport:Script = SUBSTR(cScript,1,idx) + fGetScript("OnReportStart",?)
        /* Set Detail OnForamt Script */
        cScript = hReport:Sections:Item("Detail"):Script
        idx = INDEX(cScript,"Sub OnFormat")
        idx = IF idx EQ 0 THEN LENGTH(cScript) ELSE idx - 2
        hReport:Sections:Item("Detail"):Script = SUBSTR(cScript,1,idx) + fGetScript("Detail.OnFormat",?)
        .
    /* Set Group Header/Footer OnFormat Script */
    FOR EACH ttSection BREAK BY ttSection.secType:
        /* Set GroupHeader OnForamt Script */
        IF ttSection.secType EQ 4 THEN
        hReport:Sections:Item(ttSection.secSectionItem):Script = fGetScript("GroupHeader.OnFormat",ttSection.secSection).
        /* Set GroupFooter OnForamt Script */
        IF ttSection.secType EQ 5 THEN
        hReport:Sections:Item(ttSection.secSectionItem):Script = fGetScript("GroupFooter.OnFormat",ttSection.secSection).
    END. /* each ttsection */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdate C-Win 
PROCEDURE pUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ttAOA:
        ASSIGN
            cID = "aoa/" + ttAOA.progID + "p"
            cID = SEARCH(cID)
            .
        RUN pOpenAOAProgram (ttAOA.module, cID).
        RUN pSetScript.
        RUN pPublish (NO).
    END. /* each ttaoa */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetScript C-Win 
FUNCTION fGetScript RETURNS CHARACTER
  ( ipScriptDatFile AS CHARACTER, ipSection AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScript AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDatTxt AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = "aoa\vbScript\Rpt." + ipScriptDatFile + ".dat".
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cDatTxt.
        cScript = cScript + cDatTxt + CHR(10).
    END. /* repeat */
    INPUT CLOSE.
    IF ipSection NE ? THEN
    cScript = REPLACE(cScript,"%",ipSection).
    RETURN cScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

