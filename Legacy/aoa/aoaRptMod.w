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

&SCOPED-DEFINE aoaMultiplier 150

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE PAApplication  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE PAReportEngine AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hReport        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hScript        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cDataPAReport  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lOKPressed     AS LOGICAL    NO-UNDO.

DEFINE TEMP-TABLE ttSubject NO-UNDO
    FIELD ttOrder  AS INTEGER   LABEL "Order"  FORMAT ">>>9"
    FIELD ttField  AS CHARACTER LABEL "Field"  FORMAT "x(20)"
    FIELD ttLabel  AS CHARACTER LABEL "Label"  FORMAT "x(20)"
    FIELD ttType   AS CHARACTER LABEL "Type"   FORMAT "x(10)"
    FIELD ttFormat AS CHARACTER LABEL "Format" FORMAT "x(20)"
    FIELD ttWidth  AS INTEGER   LABEL "Width"  FORMAT ">>>9"
    FIELD ttSize   AS INTEGER   LABEL "Size"   FORMAT ">>>9"
        INDEX ttSubject IS PRIMARY ttOrder.

DEFINE TEMP-TABLE ttPageHeader NO-UNDO
    FIELD phOrder        AS INTEGER   LABEL "Order"        FORMAT ">>>9"
    FIELD phName         AS CHARACTER LABEL "Name"         FORMAT "x(30)"
    FIELD phCaption      AS CHARACTER LABEL "Caption"      FORMAT "x(20)"
    FIELD phLeft         AS INTEGER   LABEL "Position"     FORMAT ">>>>9"
    FIELD phWidth        AS INTEGER   LABEL "Width"        FORMAT ">>>9"
    FIELD phAlignment    AS INTEGER   LABEL "Alignment"    FORMAT "9"
    FIELD phSectionItem  AS INTEGER   LABEL "SectionItem"  FORMAT ">>9"
    FIELD phControlItem  AS INTEGER   LABEL "ControlItem"  FORMAT ">>9"
        INDEX ttPageHeader IS PRIMARY phOrder.

DEFINE TEMP-TABLE ttDetail NO-UNDO
    FIELD dtOrder        AS INTEGER   LABEL "Order"        FORMAT ">>>9"
    FIELD dtName         AS CHARACTER LABEL "Name"         FORMAT "x(30)"
    FIELD dtDataField    AS CHARACTER LABEL "DataField"    FORMAT "x(20)"
    FIELD dtLeft         AS INTEGER   LABEL "Position"     FORMAT ">>>>9"
    FIELD dtWidth        AS INTEGER   LABEL "Width"        FORMAT ">>>9"
    FIELD dtAlignment    AS INTEGER   LABEL "Alignment"    FORMAT "9"
    FIELD dtOutputFormat AS CHARACTER LABEL "OutputFormat" FORMAT "x(30)"
    FIELD dtSectionItem  AS INTEGER   LABEL "SectionItem"  FORMAT ">>9"
    FIELD dtControlItem  AS INTEGER   LABEL "ControlItem"  FORMAT ">>9"
        INDEX ttDetail IS PRIMARY dtOrder.

DEFINE TEMP-TABLE ttSection NO-UNDO
    FIELD secOrder        AS INTEGER   LABEL "Order"        FORMAT ">>>9"
    FIELD secType         AS INTEGER   LABEL "Type"         FORMAT ">9"
    FIELD secSection      AS CHARACTER LABEL "Section"      FORMAT "x(30)"
    FIELD secName         AS CHARACTER LABEL "Name"         FORMAT "x(40)"
    FIELD secDataField    AS CHARACTER LABEL "DataField"    FORMAT "x(20)"
    FIELD secLeft         AS INTEGER   LABEL "Position"     FORMAT ">>>>9"
    FIELD secWidth        AS INTEGER   LABEL "Width"        FORMAT ">>>9"
    FIELD secAlignment    AS INTEGER   LABEL "Alignment"    FORMAT "9"
    FIELD secOutputFormat AS CHARACTER LABEL "OutputFormat" FORMAT "x(30)"
    FIELD secSectionItem  AS INTEGER   LABEL "SectionItem"  FORMAT ">>9"
    FIELD secControlItem  AS INTEGER   LABEL "ControlItem"  FORMAT ">>9"
        INDEX ttSection IS PRIMARY secOrder.

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
&Scoped-define INTERNAL-TABLES ttDetail ttPageHeader ttSection ttSubject

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
    ~{&OPEN-QUERY-ttSection}~
    ~{&OPEN-QUERY-ttSubject}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOpenRPA btnSetName btnSave btnPublish ~
ttSubject ttPageHeader ttDetail ttSection btnOnReportStart ~
btnDetailOnFormat 
&Scoped-Define DISPLAYED-OBJECTS aoaProgramID aoaReportWidth aoaReportTitle ~
aoaRptFile 

/* Custom List Definitions                                              */
/* aoaReportValue,List-2,List-3,List-4,List-5,List-6                    */
&Scoped-define aoaReportValue aoaProgramID aoaReportWidth aoaReportTitle ~
aoaRptFile 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDetailOnFormat C-Win 
FUNCTION fDetailOnFormat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOnReportStart C-Win 
FUNCTION fOnReportStart RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDetailOnFormat 
     LABEL "Script: &Detail OnFormat" 
     SIZE 31 BY 1.19.

DEFINE BUTTON btnOnReportStart 
     LABEL "Script: On&ReportStart" 
     SIZE 31 BY 1.19.

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

DEFINE VARIABLE aoaProgramID AS CHARACTER FORMAT "X(256)":U 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE aoaReportTitle AS CHARACTER FORMAT "X(256)":U 
     LABEL "Report Title" 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE aoaReportWidth AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Report Width" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE aoaRptFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "AOA Report File" 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttDetail FOR 
      ttDetail SCROLLING.

DEFINE QUERY ttPageHeader FOR 
      ttPageHeader SCROLLING.

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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 19.76
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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 76 BY 19.76
         TITLE "PageHeader Section".

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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 19.29
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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 39.29
         TITLE "Business Subject Temp-Table".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     aoaProgramID AT ROW 1.24 COL 5 COLON-ALIGNED WIDGET-ID 18
     btnOpenRPA AT ROW 1.24 COL 24 WIDGET-ID 16
     btnSetName AT ROW 1.24 COL 29 WIDGET-ID 4
     btnSave AT ROW 1.24 COL 44 WIDGET-ID 6
     btnPublish AT ROW 1.24 COL 59 WIDGET-ID 20
     aoaReportWidth AT ROW 1.24 COL 86 COLON-ALIGNED WIDGET-ID 10
     aoaReportTitle AT ROW 1.24 COL 109 COLON-ALIGNED WIDGET-ID 8
     aoaRptFile AT ROW 1.24 COL 191 COLON-ALIGNED WIDGET-ID 2
     ttSubject AT ROW 2.43 COL 3 WIDGET-ID 200
     ttPageHeader AT ROW 2.43 COL 99 WIDGET-ID 300
     ttDetail AT ROW 2.43 COL 176 WIDGET-ID 400
     ttSection AT ROW 22.43 COL 141 WIDGET-ID 500
     btnOnReportStart AT ROW 23.62 COL 104 WIDGET-ID 14
     btnDetailOnFormat AT ROW 25.05 COL 104 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 283.2 BY 40.86 WIDGET-ID 100.


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
         HEIGHT             = 40.86
         WIDTH              = 283.2
         MAX-HEIGHT         = 40.86
         MAX-WIDTH          = 283.2
         VIRTUAL-HEIGHT     = 40.86
         VIRTUAL-WIDTH      = 283.2
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
/* BROWSE-TAB ttSubject aoaRptFile DEFAULT-FRAME */
/* BROWSE-TAB ttPageHeader ttSubject DEFAULT-FRAME */
/* BROWSE-TAB ttDetail ttPageHeader DEFAULT-FRAME */
/* BROWSE-TAB ttSection ttDetail DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN aoaProgramID IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN aoaReportTitle IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN aoaReportWidth IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN aoaRptFile IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
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


&Scoped-define SELF-NAME btnDetailOnFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetailOnFormat C-Win
ON CHOOSE OF btnDetailOnFormat IN FRAME DEFAULT-FRAME /* Script: Detail OnFormat */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoaReports\Rpt.Detail.OnFormat.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOnReportStart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOnReportStart C-Win
ON CHOOSE OF btnOnReportStart IN FRAME DEFAULT-FRAME /* Script: OnReportStart */
DO:
  OS-COMMAND NO-WAIT notepad.exe aoaReports\Rpt.OnReportStart.dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOpenRPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOpenRPA C-Win
ON CHOOSE OF btnOpenRPA IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cID AS CHARACTER NO-UNDO.

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
    RUN pPublish.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    ASSIGN {&aoaReportValue}
        hReport:Sections:Item("ReportHeader"):Controls:Item("ReportTitle"):Caption = aoaReportTitle
        hReport:Sections:Item("ReportHeader"):Controls:Item("ReportTitle"):Width   = LENGTH(aoaReportTitle) * {&aoaMultiplier} + 35
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
  {&WINDOW-NAME}:WINDOW-STATE = 1.
  RUN pCreateObjects.
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
  DISPLAY aoaProgramID aoaReportWidth aoaReportTitle aoaRptFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnOpenRPA btnSetName btnSave btnPublish ttSubject ttPageHeader 
         ttDetail ttSection btnOnReportStart btnDetailOnFormat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetReportFields C-Win 
PROCEDURE pGetReportFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcReportName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ix       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iy       AS INTEGER   NO-UNDO.
    
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

    aoaReportWidth = hReport:PrintWidth.
    DO ix = 0 TO hReport:Sections:Count - 1:
        cName = hReport:Sections:Item(ix):Name.
        DO iy = 0 TO hReport:Sections:Item(ix):Controls:Count - 1:
            CASE cName:
                WHEN "PageHeader" THEN DO:
                    CREATE ttPageHeader.
                    ASSIGN
                        ttPageHeader.phName        = hReport:Sections:Item(ix):Controls:Item(iy):Name
                        ttPageHeader.phCaption     = hReport:Sections:Item(ix):Controls:Item(iy):Caption
                        ttPageHeader.phLeft        = hReport:Sections:Item(ix):Controls:Item(iy):Left 
                        ttPageHeader.phWidth       = hReport:Sections:Item(ix):Controls:Item(iy):Width
                        ttPageHeader.phAlignment   = hReport:Sections:Item(ix):Controls:Item(iy):Alignment
                        ttPageHeader.phSectionItem = ix
                        ttPageHeader.phControlItem = iy
                        .
                    FIND FIRST ttSubject WHERE ttSubject.ttLabel EQ ttPageHeader.phCaption NO-ERROR.
                    IF AVAILABLE ttSubject THEN
                    ttPageHeader.phOrder = ttSubject.ttOrder.
                END.
                WHEN "Detail" THEN DO:
                    IF iy EQ 0 THEN NEXT.
                    CREATE ttDetail.
                    ASSIGN
                        ttDetail.dtName         = hReport:Sections:Item(ix):Controls:Item(iy):Name
                        ttDetail.dtDataField    = hReport:Sections:Item(ix):Controls:Item(iy):DataField
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
                END.
            END CASE.
            IF (hReport:Sections:Item(ix):Type EQ 1  OR
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
            END. /* groupfooter */
        END. /* do iy */
    END. /* do ix */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetScript C-Win 
PROCEDURE pGetScript :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcRptDatFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcScript     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDatTxt AS CHARACTER NO-UNDO.

    INPUT FROM VALUE(ipcRptDatFile) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cDatTxt.
        opcScript = opcScript + cDatTxt + CHR(10).
    END. /* repeat */
    INPUT CLOSE.
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
        IF iphTable:BUFFER-FIELD(idx):NAME EQ "rowType" THEN NEXT.
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
    DEFINE INPUT PARAMETER ipID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTxt     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cScopDef AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAppSrv  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hAppSrv  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable   AS HANDLE    NO-UNDO.

    INPUT FROM VALUE(ipID) NO-ECHO.
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
                idx = R-INDEX(ipID,"\")
                aoaRptFile = SEARCH("aoaReports\"
                           + SUBSTR(ipID,idx - 2,2) + "\"
                           + aoaReportTitle + ".rpa")
                cAppSrv = "aoaAppSrv\"
                        + ENTRY(NUM-ENTRIES(ipID,"\") - 1,ipID,"\")
                        + ".p"
                .
        END CASE.
    END. /* repeat */
    INPUT CLOSE.
    DISPLAY {&aoaReportValue} WITH FRAME {&FRAME-NAME}.

    RUN VALUE(cAppSrv) PERSISTENT SET hAppSrv.
    hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
    
    RUN pGetTempTableFields (hTable).
    RUN pGetReportFields (aoaRptFile).

    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}

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
    DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLeft    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFormat  AS CHARACTER NO-UNDO.

    FOR EACH ttSubject:
        cName = CAPS(SUBSTR(ttSubject.ttField,1,1)) + SUBSTR(ttSubject.ttField,2).
        FIND FIRST ttPageHeader
             WHERE ttPageHeader.phOrder EQ ttSubject.ttOrder
             NO-ERROR.
        IF AVAILABLE ttPageHeader THEN
        ASSIGN
            ttPageHeader.phName      = ttSubject.ttField + "_PageHeader"
            ttPageHeader.phCaption   = ttSubject.ttLabel
            ttPageHeader.phWidth     = ttSubject.ttSize
            ttPageHeader.phLeft      = iLeft
            ttPageHeader.phAlignment = IF CAN-DO("Character,Date",ttSubject.ttType) THEN 0 ELSE 1
            .
        FIND FIRST ttDetail
             WHERE ttDetail.dtOrder EQ ttSubject.ttOrder
             NO-ERROR.
        IF AVAILABLE ttDetail THEN DO:
            ASSIGN
                ttDetail.dtName         = ttSubject.ttField + "_Detail"
                ttDetail.dtDataField    = ttSubject.ttLabel
                ttDetail.dtWidth        = ttSubject.ttSize
                ttDetail.dtLeft         = iLeft
                ttDetail.dtAlignment    = 0
                ttDetail.dtOutputFormat = ""
                .
            IF ttSubject.ttType NE "Character" THEN DO:
                cFormat = "mm/dd/yyyy".
                IF ttSubject.ttType NE "Date" THEN DO:
                    ASSIGN
                        ttDetail.dtAlignment = 1
                        cFormat = REPLACE(ttSubject.ttFormat,">","#")
                        cFormat = REPLACE(cFormat,"9","0")
                        .
                    IF cFormat BEGINS "-" THEN
                    ASSIGN
                        cFormat = REPLACE(cFormat,"-","")
                        cFormat = cFormat + ";(" + cFormat + ")"
                        .
                END. /* if not date */
                ttDetail.dtOutputFormat = cFormat.
            END. /* if not character */
        END. /* avail ttdetail */
        FOR EACH ttSection
            WHERE ttSection.secDataField EQ ttSubject.ttLabel
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
    
    aoaReportWidth:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iLeft).

    ttPageHeader:REFRESH() NO-ERROR.
    ttDetail:REFRESH() NO-ERROR.
    ttSection:REFRESH() NO-ERROR.

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
            hReport:Sections:Item(ix):Controls:Item(iy):DataField    = ttDetail.dtDataField
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
        hReport:Script = SUBSTR(cScript,1,idx) + fOnReportStart()
        /* Set Detail OnForamt Script */
        cScript = hReport:Sections:Item("Detail"):Script
        idx = INDEX(cScript,"Sub OnFormat")
        idx = IF idx EQ 0 THEN LENGTH(cScript) ELSE idx - 2
        hReport:Sections:Item("Detail"):Script = SUBSTR(cScript,1,idx) + fDetailOnFormat()
        .
    /* Set SubTotal OnFormat Script */
    FOR EACH ttSection WHERE ttSection.secType EQ 5:
        hReport:Sections:Item(ttSection.secSectionItem):Script = CHR(10) +
            "Sub OnFormat" + CHR(10) +
            "  Rpt.Sections.Item(~"" + ttSection.secSection + 
            "~").Visible = Rpt.Sections.Item(~"Detail~").Visible" + CHR(10) +
            "End Sub" + CHR(10)
            .
    END. /* each ttsection */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDetailOnFormat C-Win 
FUNCTION fDetailOnFormat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScript AS CHARACTER NO-UNDO.
    
    RUN pGetScript ("aoaReports\Rpt.Detail.OnFormat.dat", OUTPUT cScript).
    RETURN cScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOnReportStart C-Win 
FUNCTION fOnReportStart RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cScript AS CHARACTER NO-UNDO.
    
    RUN pGetScript ("aoaReports\Rpt.OnReportStart.dat", OUTPUT cScript).
    RETURN cScript.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

