&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: xferJobData.w

  Description: Transfer Actual Data Collection to Estimate

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 4.1.2015

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE estOpWhere est-op.company EQ ipCompany ~
AND est-op.est-no EQ ipEstNo ~
AND est-op.line LT 500 ~
AND ((est-op.qty eq estQty and (iEstType eq 1 OR iEstType EQ 5 OR iEstType EQ 6 )) or ~
 (est-op.qty eq iv-eqty and (iEstType EQ 2 OR iEstType EQ 4 OR iEstType EQ 8)))


/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipEstNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipQuery AS HANDLE NO-UNDO.
&ELSE
DEFINE VARIABLE ipCompany AS CHARACTER NO-UNDO INIT '001'.
DEFINE VARIABLE ipEstNo AS CHARACTER NO-UNDO INIT '   12218'. /* 2297 */
DEFINE VARIABLE ipJobNo AS CHARACTER NO-UNDO INIT '040101'.
DEFINE VARIABLE ipJobNo2 AS INTEGER NO-UNDO INIT 0.
DEFINE VARIABLE ipQuery AS HANDLE NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "NEW SHARED"}

DEFINE VARIABLE jobhdrRowID AS ROWID NO-UNDO.
DEFINE VARIABLE estQty AS INTEGER NO-UNDO.
DEFINE VARIABLE iv-eqty LIKE est-qty.eqty NO-UNDO.
DEFINE VARIABLE iEstType AS INTEGER NO-UNDO.
DEFINE VARIABLE mCodeCol AS LOGICAL NO-UNDO.
DEFINE VARIABLE opSpeedCol AS LOGICAL NO-UNDO.
DEFINE VARIABLE mrHRCol AS LOGICAL NO-UNDO.
DEFINE VARIABLE opSpoilCol AS LOGICAL NO-UNDO.
DEFINE VARIABLE mrStdCol AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttblEstOp NO-UNDO
  FIELD s-num LIKE est-op.s-num LABEL 'S'
  FIELD d-seq LIKE est-op.d-seq
  FIELD b-num LIKE est-op.b-num LABEL 'B'
  FIELD p-num LIKE est-op.op-pass LABEL 'P'
  FIELD m-code LIKE est-op.m-code LABEL 'Machine' FORMAT 'x(10)'
  FIELD m-dscr LIKE est-op.m-dscr FORMAT 'x(24)'
  FIELD n-out LIKE est-op.n-out LABEL 'Out'
  FIELD run-hr LIKE job-mch.run-hr LABEL 'Run Hrs'
  FIELD mr-hr LIKE job-mch.run-hr LABEL 'MR Hrs'
  FIELD run-waste AS INTEGER FORMAT '->>>>>>9' LABEL 'Run Waste'
  FIELD mr-waste AS INTEGER FORMAT '->>>>>>9' LABEL 'MR Waste'
  FIELD wst-prct LIKE job-mch.wst-prct
  FIELD est-speed LIKE job-mch.speed
  FIELD std-hrs LIKE job-mch.run-hr
  FIELD std-mr-hrs LIKE job-mch.mr-hr
  FIELD run-hrs LIKE mch-act.hours
  FIELD act-qty LIKE mch-act.qty
  FIELD wst-qty LIKE mch-act.waste
  FIELD op-speed LIKE est-op.op-speed
  FIELD op-spoil LIKE est-op.op-spoil
  FIELD mCode AS LOGICAL LABEL ' =>'
  FIELD opSpeed AS LOGICAL LABEL '<='
  FIELD mrHR AS LOGICAL LABEL '<='
  FIELD opSpoil AS LOGICAL LABEL '<='
  FIELD mrStd AS LOGICAL LABEL '<='
  FIELD estOpRowID AS ROWID
        INDEX ttblEstOp IS PRIMARY
              s-num
              d-seq
              b-num
              p-num
              m-code
  .

&IF DEFINED(FWD-VERSION) EQ 0 &THEN
   {methods/lockWindowUpdate.i}
&ENDIF

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME estOpBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-op job-hdr ttblEstOp

/* Definitions for BROWSE estOpBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-estOpBrowse est-op.s-num est-op.b-num ~
est-op.op-pass est-op.m-code est-op.m-dscr est-op.n-out est-op.op-mr ~
est-op.op-waste est-op.op-speed est-op.op-spoil 
&Scoped-define ENABLED-FIELDS-IN-QUERY-estOpBrowse 
&Scoped-define QUERY-STRING-estOpBrowse FOR EACH est-op ~
      WHERE {&estOpWhere} NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-estOpBrowse OPEN QUERY estOpBrowse FOR EACH est-op ~
      WHERE {&estOpWhere} NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-estOpBrowse est-op
&Scoped-define FIRST-TABLE-IN-QUERY-estOpBrowse est-op


/* Definitions for BROWSE jobBrowse                                     */
&Scoped-define FIELDS-IN-QUERY-jobBrowse job-hdr.job-no job-hdr.job-no2 ~
job-hdr.qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-jobBrowse 
&Scoped-define QUERY-STRING-jobBrowse FOR EACH job-hdr ~
      WHERE job-hdr.company EQ ipCompany ~
AND job-hdr.est-no EQ ipEstNo ~
AND job-hdr.opened EQ FALSE NO-LOCK , ~
    FIRST job                                ~
        WHERE job.company EQ job-hdr.company ~
          AND job.job     EQ job-hdr.job     ~
          AND job.job-no  EQ job-hdr.job-no  ~
          AND job.job-no2 EQ job-hdr.job-no2 NO-LOCK BY job.job-no BY job.close-date DESC
&Scoped-define OPEN-QUERY-jobBrowse OPEN QUERY jobBrowse FOR EACH job-hdr ~
      WHERE job-hdr.company EQ ipCompany ~
AND job-hdr.est-no EQ ipEstNo ~
AND job-hdr.opened EQ FALSE NO-LOCK , ~
   FIRST job                                ~
        WHERE job.company EQ job-hdr.company ~
          AND job.job     EQ job-hdr.job     ~
          AND job.job-no  EQ job-hdr.job-no  ~
          AND job.job-no2 EQ job-hdr.job-no2 NO-LOCK BY job.job-no BY job.close-date DESC .
&Scoped-define TABLES-IN-QUERY-jobBrowse job-hdr job
&Scoped-define FIRST-TABLE-IN-QUERY-jobBrowse job-hdr
&SCOPED-DEFINE SECOND-TABLE-IN-QUERY-jobBrowse job


/* Definitions for BROWSE ttblEstOp                                     */
&Scoped-define FIELDS-IN-QUERY-ttblEstOp ttblEstOp.mCode ttblEstOp.s-num ttblEstOp.b-num ttblEstOp.p-num ttblEstOp.m-code ttblEstOp.m-dscr ttblEstOp.n-out ttblEstOp.run-hr ttblEstOp.mr-hr ttblEstOp.mrHR ttblEstOp.run-waste ttblEstOp.mr-waste ttblEstOp.mrStd ttblEstOp.op-speed ttblEstOp.opSpeed ttblEstOp.op-spoil ttblEstOp.opSpoil   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttblEstOp ttblEstOp.mCode  ttblEstOp.opSpeed  ttblEstOp.mrHR  ttblEstOp.opSpoil  ttblEstOp.mrStd   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttblEstOp ttblEstOp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttblEstOp ttblEstOp
&Scoped-define SELF-NAME ttblEstOp
&Scoped-define QUERY-STRING-ttblEstOp FOR EACH ttblEstOp
&Scoped-define OPEN-QUERY-ttblEstOp OPEN QUERY {&SELF-NAME} FOR EACH ttblEstOp.
&Scoped-define TABLES-IN-QUERY-ttblEstOp ttblEstOp
&Scoped-define FIRST-TABLE-IN-QUERY-ttblEstOp ttblEstOp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-estOpBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSubmitDownLeft btnSubmitDownRight ~
machineRect allJobRect jobRect quantityRect standardsRect allJobs radioQty ~
jobBrowse ttblEstOp btnSubmit estOpBrowse selectionFilters quantityText ~
jobText selectedJobs machineText standardsText 
&Scoped-Define DISPLAYED-OBJECTS allJobs radioQty selectionFilters ~
closeStart jobStart qtyStart closeEnd jobEnd qtyEnd quantityText jobText ~
selectedJobs machineText standardsText 

/* Custom List Definitions                                              */
/* Filters,List-2,List-3,List-4,List-5,List-6                           */
&Scoped-define Filters closeStart jobStart qtyStart closeEnd jobEnd qtyEnd ~
btnApplyDateFilter btnApplyJobFilter btnApplyQtyFilter 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApplyDateFilter 
     LABEL "Select Jobs Using Date Range" 
     SIZE 36 BY 1.14.

DEFINE BUTTON btnApplyJobFilter 
     LABEL "Select Jobs Using Job # Range" 
     SIZE 36 BY 1.14.

DEFINE BUTTON btnApplyQtyFilter 
     LABEL "Select Jobs Using Quantity Range" 
     SIZE 36 BY 1.14.

DEFINE BUTTON btnSubmit 
     LABEL "Transfer ~"Actual Job Data~" to ~"Estimate Standards~"" 
     SIZE 65 BY 1.14
     FONT 6.

DEFINE BUTTON btnSubmitDownLeft 
     IMAGE-UP FILE "Graphics/16x16/down.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.14.

DEFINE BUTTON btnSubmitDownRight 
     IMAGE-UP FILE "Graphics/16x16/down.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.14.

DEFINE VARIABLE jobEnd AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ending Job Number" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE jobStart AS CHARACTER FORMAT "X(256)":U 
     LABEL "Starting Job Number" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE closeEnd AS DATE FORMAT "99/99/9999":U 
     LABEL "Ending Closing Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE closeStart AS DATE FORMAT "99/99/9999":U 
     LABEL "Starting Closing Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE jobText AS CHARACTER FORMAT "X(256)":U INITIAL " Jobs" 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE machineText AS CHARACTER FORMAT "X(256)":U INITIAL " Actual Job Data" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE qtyEnd AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Ending Quantity" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE qtyStart AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Starting Quantity" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE quantityText AS CHARACTER FORMAT "X(256)":U INITIAL " Quantity" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE selectedJobs AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL " Selected" 
      VIEW-AS TEXT 
     SIZE 6 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE standardsText AS CHARACTER FORMAT "X(256)":U INITIAL " Estimate Standards" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE radioQty AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item 1", 1
     SIZE 10 BY .71 NO-UNDO.

DEFINE RECTANGLE allJobRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 1.43.

DEFINE RECTANGLE jobRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 28.1.

DEFINE RECTANGLE machineRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 10.71.

DEFINE RECTANGLE quantityRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 1.43.

DEFINE RECTANGLE standardsRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 10.71.

DEFINE VARIABLE allJobs AS LOGICAL INITIAL no 
     LABEL "ALL JOBS" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .95 NO-UNDO.

DEFINE VARIABLE selectionFilters AS LOGICAL INITIAL no 
     LABEL "Advanced Selection Filters" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY estOpBrowse FOR 
      est-op SCROLLING.

DEFINE QUERY jobBrowse FOR 
      job-hdr,
      job SCROLLING.

DEFINE QUERY ttblEstOp FOR 
      ttblEstOp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE estOpBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS estOpBrowse C-Win _STRUCTURED
  QUERY estOpBrowse NO-LOCK DISPLAY
      est-op.s-num COLUMN-LABEL "S" FORMAT ">9":U WIDTH 6.2
      est-op.b-num COLUMN-LABEL "B" FORMAT ">9":U WIDTH 2.2
      est-op.op-pass COLUMN-LABEL "P" FORMAT ">9":U
      est-op.m-code COLUMN-LABEL "Machine" FORMAT "x(10)":U
      est-op.m-dscr FORMAT "x(24)":U
      est-op.n-out COLUMN-LABEL "Out" FORMAT ">>9":U WIDTH 4
      est-op.op-mr COLUMN-LABEL "MR Hrs" FORMAT ">>9.99":U WIDTH 15.6
      est-op.op-waste COLUMN-LABEL "MR Waste" FORMAT "->>>>>9":U
            WIDTH 14.6
      est-op.op-speed FORMAT ">>>>9":U WIDTH 20.8
      est-op.op-spoil FORMAT ">>9.99":U WIDTH 11.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 123 BY 10.

DEFINE BROWSE jobBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS jobBrowse C-Win _STRUCTURED
  QUERY jobBrowse NO-LOCK DISPLAY
      job-hdr.job-no COLUMN-LABEL "Job" FORMAT "x(6)":U WIDTH 8.2
      job-hdr.job-no2 COLUMN-LABEL "" FORMAT ">9":U WIDTH 2.2
      job-hdr.qty FORMAT ">>,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 28 BY 27.38.

DEFINE BROWSE ttblEstOp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttblEstOp C-Win _FREEFORM
  QUERY ttblEstOp DISPLAY
      ttblEstOp.mCode VIEW-AS TOGGLE-BOX
   ttblEstOp.s-num
   ttblEstOp.b-num
   ttblEstOp.p-num
   ttblEstOp.m-code
   ttblEstOp.m-dscr
   ttblEstOp.n-out
   ttblEstOp.run-hr
   ttblEstOp.mr-hr
   ttblEstOp.mrHR VIEW-AS TOGGLE-BOX
   ttblEstOp.run-waste
   ttblEstOp.mr-waste
   ttblEstOp.mrStd VIEW-AS TOGGLE-BOX
   ttblEstOp.op-speed
   ttblEstOp.opSpeed VIEW-AS TOGGLE-BOX
   ttblEstOp.op-spoil
   ttblEstOp.opSpoil VIEW-AS TOGGLE-BOX
   ENABLE
   ttblEstOp.mCode
   ttblEstOp.opSpeed
   ttblEstOp.mrHR
   ttblEstOp.opSpoil
   ttblEstOp.mrStd
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 127 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSubmitDownLeft AT ROW 14.57 COL 85 WIDGET-ID 72
     btnSubmitDownRight AT ROW 14.57 COL 155 WIDGET-ID 74
     allJobs AT ROW 1.71 COL 6 HELP
          "Select ALL Jobs" WIDGET-ID 18
     radioQty AT ROW 1.95 COL 36 NO-LABEL WIDGET-ID 40
     jobBrowse AT ROW 3.86 COL 5 WIDGET-ID 200
     ttblEstOp AT ROW 3.86 COL 36 WIDGET-ID 300
     btnSubmit AT ROW 14.57 COL 90 WIDGET-ID 50
     estOpBrowse AT ROW 16.48 COL 36 WIDGET-ID 100
     selectionFilters AT ROW 26.95 COL 36 HELP
          "Show Selection Filters" WIDGET-ID 70
     closeStart AT ROW 27.91 COL 54 COLON-ALIGNED WIDGET-ID 58
     jobStart AT ROW 27.91 COL 98 COLON-ALIGNED WIDGET-ID 52
     qtyStart AT ROW 27.91 COL 141 COLON-ALIGNED HELP
          "Enter Starting Quantity to Filter" WIDGET-ID 64
     closeEnd AT ROW 29.1 COL 54 COLON-ALIGNED WIDGET-ID 60
     jobEnd AT ROW 29.1 COL 98 COLON-ALIGNED WIDGET-ID 54
     qtyEnd AT ROW 29.1 COL 141 COLON-ALIGNED HELP
          "Enter Ending Quantity to Filter" WIDGET-ID 66
     btnApplyDateFilter AT ROW 30.29 COL 36 WIDGET-ID 62
     btnApplyJobFilter AT ROW 30.29 COL 80 WIDGET-ID 56
     btnApplyQtyFilter AT ROW 30.29 COL 123 WIDGET-ID 68
     quantityText AT ROW 1.24 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     jobText AT ROW 3.14 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     selectedJobs AT ROW 3.14 COL 21 COLON-ALIGNED WIDGET-ID 48
     machineText AT ROW 3.14 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     standardsText AT ROW 15.76 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     machineRect AT ROW 16 COL 35 WIDGET-ID 14
     allJobRect AT ROW 1.48 COL 4 WIDGET-ID 16
     jobRect AT ROW 3.38 COL 4 WIDGET-ID 20
     quantityRect AT ROW 1.48 COL 35 WIDGET-ID 26
     standardsRect AT ROW 3.38 COL 35 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 164.2 BY 30.76.


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
         TITLE              = "Xfer Job Data - Estimate:"
         HEIGHT             = 30.76
         WIDTH              = 164.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB jobBrowse radioQty DEFAULT-FRAME */
/* BROWSE-TAB ttblEstOp jobBrowse DEFAULT-FRAME */
/* BROWSE-TAB estOpBrowse btnSubmit DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnApplyDateFilter IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnApplyDateFilter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnApplyJobFilter IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnApplyJobFilter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnApplyQtyFilter IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       btnApplyQtyFilter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN closeEnd IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       closeEnd:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN closeStart IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       closeStart:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX jobEnd IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       jobEnd:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX jobStart IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       jobStart:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN qtyEnd IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       qtyEnd:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN qtyStart IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       qtyStart:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE estOpBrowse
/* Query rebuild information for BROWSE estOpBrowse
     _TblList          = "asi.est-op"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "{&estOpWhere}"
     _FldNameList[1]   > asi.est-op.s-num
"est-op.s-num" "S" ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.est-op.b-num
"est-op.b-num" "B" ? "integer" ? ? ? ? ? ? no ? no no "2.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.est-op.op-pass
"est-op.op-pass" "P" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.est-op.m-code
"est-op.m-code" "Machine" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.est-op.m-dscr
"est-op.m-dscr" ? "x(24)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.est-op.n-out
"est-op.n-out" "Out" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > asi.est-op.op-mr
"est-op.op-mr" "MR Hrs" ? "decimal" ? ? ? ? ? ? no ? no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > asi.est-op.op-waste
"est-op.op-waste" "MR Waste" "->>>>>9" "integer" ? ? ? ? ? ? no ? no no "14.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.est-op.op-speed
"est-op.op-speed" ? ? "integer" ? ? ? ? ? ? no ? no no "20.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > asi.est-op.op-spoil
"est-op.op-spoil" ? ? "decimal" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE estOpBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE jobBrowse
/* Query rebuild information for BROWSE jobBrowse
     _TblList          = "asi.job-hdr,ASI.job WHERE ASI.job-hdr ..."
     _Options          = "NO-LOCK"
     _Where[1]         = "job-hdr.company EQ ipCompany
AND job-hdr.est-no EQ ipEstNo
AND job-hdr.opened EQ FALSE"
  _JoinCode[1]      = "ASI.job.company = ASI.job-hdr.company
  AND ASI.job.job = ASI.job-hdr.job
  AND ASI.job.job-no = ASI.job-hdr.job-no 
  AND job.job-no2 EQ job-hdr.job-no2 NO-LOCK BY job.job-no BY job.close-date DESC"
     _FldNameList[1]   > asi.job-hdr.job-no
"job-hdr.job-no" "Job" ? "character" ? ? ? ? ? ? no ? no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.job-hdr.job-no2
"job-hdr.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no "2.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = asi.job-hdr.qty
     _Query            is NOT OPENED
*/  /* BROWSE jobBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttblEstOp
/* Query rebuild information for BROWSE ttblEstOp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblEstOp.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE ttblEstOp */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Xfer Job Data - Estimate: */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Xfer Job Data - Estimate: */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MAXIMIZED OF C-Win /* Xfer Job Data - Estimate: */
DO:
  RUN winReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME allJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL allJobs C-Win
ON VALUE-CHANGED OF allJobs IN FRAME DEFAULT-FRAME /* ALL JOBS */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} THEN
  jobBrowse:SELECT-ALL().
  ELSE
  jobBrowse:DESELECT-ROWS().
  APPLY 'VALUE-CHANGED':U TO jobBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApplyDateFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApplyDateFilter C-Win
ON CHOOSE OF btnApplyDateFilter IN FRAME DEFAULT-FRAME /* Select Jobs Using Date Range */
DO:
  RUN applyDateFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApplyJobFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApplyJobFilter C-Win
ON CHOOSE OF btnApplyJobFilter IN FRAME DEFAULT-FRAME /* Select Jobs Using Job # Range */
DO:
  RUN applyJobFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApplyQtyFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApplyQtyFilter C-Win
ON CHOOSE OF btnApplyQtyFilter IN FRAME DEFAULT-FRAME /* Select Jobs Using Quantity Range */
DO:
  RUN applyQtyFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubmit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubmit C-Win
ON CHOOSE OF btnSubmit IN FRAME DEFAULT-FRAME /* Transfer "Actual Job Data" to "Estimate Standards" */
DO:
  RUN submitJobData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubmitDownLeft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubmitDownLeft C-Win
ON CHOOSE OF btnSubmitDownLeft IN FRAME DEFAULT-FRAME
DO:
  RUN submitJobData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubmitDownRight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubmitDownRight C-Win
ON CHOOSE OF btnSubmitDownRight IN FRAME DEFAULT-FRAME
DO:
  RUN submitJobData.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME jobBrowse
&Scoped-define SELF-NAME jobBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobBrowse C-Win
ON VALUE-CHANGED OF jobBrowse IN FRAME DEFAULT-FRAME
DO:
  RUN getJobAverages.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME radioQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL radioQty C-Win
ON VALUE-CHANGED OF radioQty IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    {&SELF-NAME}
    estQty = {&SELF-NAME}
    mCodeCol = NO
    opSpeedCol = NO
    mrHRCol = NO
    opSpoilCol = NO
    mrStdCol = NO
    .
  {&OPEN-QUERY-estOpBrowse}
  RUN createTtblEstOp.
  RUN getJobAverages.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectionFilters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectionFilters C-Win
ON VALUE-CHANGED OF selectionFilters IN FRAME DEFAULT-FRAME /* Advanced Selection Filters */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} THEN
  ENABLE {&Filters} WITH FRAME {&FRAME-NAME}.
  ELSE HIDE {&Filters}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttblEstOp
&Scoped-define SELF-NAME ttblEstOp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttblEstOp C-Win
ON START-SEARCH OF ttblEstOp IN FRAME DEFAULT-FRAME
DO:
  CASE ttblEstOp:CURRENT-COLUMN:NAME:
    WHEN 'mCode' THEN DO:
      ASSIGN
        mCodeCol = NOT mCodeCol
        opSpeedCol = mCodeCol
        mrHRCol = mCodeCol
        opSpoilCol = mCodeCol
        mrStdCol = mCodeCol
        .
      FOR EACH ttblEstOp:
        ASSIGN
          ttblEstOp.mCode = mCodeCol
          ttblEstOp.opSpeed = mCodeCol
          ttblEstOp.mrHR = mCodeCol
          ttblEstOp.opSpoil = mCodeCol
          ttblEstOp.mrStd = mCodeCol
          .
      END. /* each ttblestop */
    END. /* mcode */
    WHEN 'opSpeed' THEN DO:
      ASSIGN
        opSpeedCol = NOT opSpeedCol
        mCodeCol = NO
        .
      FOR EACH ttblEstOp:
        ASSIGN
          ttblEstOp.opSpeed = opSpeedCol
          ttblEstOp.mCode = NO
          .
      END. /* each ttblestop */
    END. /* opSpeed */
    WHEN 'mrHR' THEN DO:
      ASSIGN
        mrHRCol = NOT mrHRCol
        mCodeCol = NO
        .
      FOR EACH ttblEstOp:
        ASSIGN
          ttblEstOp.mrHR = mrHRCol
          ttblEstOp.mCode = NO
          .
      END. /* each ttblestop */
    END. /* mrhr */
    WHEN 'opSpoil' THEN DO:
      ASSIGN
        opSpoilCol = NOT opSpoilCol
        mCodeCol = NO
        .
      FOR EACH ttblEstOp:
        ASSIGN
          ttblEstOp.opSpoil = opSpoilCol
          ttblEstOp.mCode = NO
          .
      END. /* each ttblestop */
    END. /* opSpoil */
    WHEN 'mrStd' THEN DO:
      ASSIGN
        mrStdCol = NOT mrStdCol
        mCodeCol = NO
        .
      FOR EACH ttblEstOp:
        ASSIGN
          ttblEstOp.mrStd = mrStdCol
          ttblEstOp.mCode = NO
          .
      END. /* each ttblestop */
    END. /* mrStd */
  END CASE.
  {&OPEN-QUERY-ttblEstOp}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME estOpBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'VALUE-CHANGED':U OF mCode, opSpeed, mrHR, opSpoil, mrStd IN BROWSE ttblEstOp
DO:
  CASE ttblEstOp:CURRENT-COLUMN:NAME IN FRAME {&FRAME-NAME}:
    WHEN 'mCode' THEN
    ASSIGN
      ttblEstOp.mcode = ttblEstOp:CURRENT-COLUMN:SCREEN-VALUE = 'yes'
      ttblEstOp.opSpeed = ttblEstOp.mcode
      ttblEstOp.mrHR = ttblEstOp.mcode
      ttblEstOp.opSpoil = ttblEstOp.mcode
      ttblEstOp.mrStd = ttblEstOp.mcode
      mCodeCol = NO
      .
    WHEN 'opSpeed' THEN
    ttblEstOp.opSpeed = ttblEstOp:CURRENT-COLUMN:SCREEN-VALUE = 'yes'.
    WHEN 'mrHr' THEN
    ttblEstOp.mrHr = ttblEstOp:CURRENT-COLUMN:SCREEN-VALUE = 'yes'.
    WHEN 'opSpoil' THEN
    ttblEstOp.opSpoil = ttblEstOp:CURRENT-COLUMN:SCREEN-VALUE = 'yes'.
    WHEN 'mrStd' THEN
    ttblEstOp.mrStd = ttblEstOp:CURRENT-COLUMN:SCREEN-VALUE = 'yes'.
  END CASE.
  RUN setMCodeCol.
  {&OPEN-QUERY-ttblEstOp}
  RETURN.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  {&OPEN-QUERY-jobBrowse}

  FIND FIRST est NO-LOCK
       WHERE est.company EQ ipCompany
         AND est.est-no EQ ipEstNo
       NO-ERROR.
  IF AVAILABLE est THEN do:
      ASSIGN iEstType = est.est-type .
      RUN getQty.
  END.
  
  RUN createTtblEstOp.
  RUN enable_UI.
  RUN loadFilters.

  ASSIGN
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' ' + LEFT-TRIM(ipEstNo)
    jobRect:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 53
    jobBrowse:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 68
    .
  IF ipJobNo NE '' THEN DO:
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company EQ ipCompany
           AND job-hdr.est-no EQ ipEstNo
           AND job-hdr.job-no EQ ipJobNo
           AND job-hdr.job-no2 EQ ipJobNo2
           AND job-hdr.opened EQ FALSE
         NO-ERROR.
    IF AVAILABLE job-hdr THEN DO:
        MESSAGE job-hdr.opened
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
        jobhdrRowID = ROWID(job-hdr)
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                             + ' (Job: ' + ipJobNo + '-'
                             + STRING(ipJobNo2) + ')'
                             .
      REPOSITION jobBrowse TO ROWID jobhdrRowID.
      jobBrowse:SELECT-FOCUSED-ROW().
      RUN getJobAverages.
    END. /* avail job-hdr */
  END. /* if ipjobno ne '' */

  HIDE {&FILTERS}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyDateFilter C-Win 
PROCEDURE applyDateFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      closeStart
      closeEnd
      .
    jobBrowse:DESELECT-ROWS().
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ ipCompany
          AND job-hdr.est-no EQ ipEstNo
       ,FIRST job OF job-hdr NO-LOCK
        WHERE job.close-date NE ?
          AND job.close-date GE closeStart
          AND job.close-date LE closeEnd
        :
      jobhdrRowID = ROWID(job-hdr).
      REPOSITION jobBrowse TO ROWID jobhdrRowID.
      jobBrowse:SELECT-FOCUSED-ROW().
    END. /* each job-hdr */
    RUN getJobAverages.
  END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyJobFilter C-Win 
PROCEDURE applyJobFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE jobNoStart AS CHARACTER NO-UNDO.
  DEFINE VARIABLE jobNo2Start AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobNoEnd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE jobNo2End AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      jobStart
      jobEnd
      jobNoStart = ENTRY(1,jobStart,'-')
      jobNo2Start = INT(ENTRY(2,jobStart,'-'))
      jobNoEnd = ENTRY(1,jobEnd,'-')
      jobNo2End = INT(ENTRY(2,jobEnd,'-'))
      .
    jobBrowse:DESELECT-ROWS().
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ ipCompany
          AND job-hdr.est-no EQ ipEstNo
          AND job-hdr.job-no GE jobNoStart
          AND job-hdr.job-no LE jobNoEnd
        :
      IF job-hdr.job-no EQ jobNoStart AND
         job-hdr.job-no2 LT jobNo2Start THEN NEXT.
      IF job-hdr.job-no EQ jobNoEnd AND
         job-hdr.job-no2 GT jobNo2End THEN LEAVE.
      jobhdrRowID = ROWID(job-hdr).
      REPOSITION jobBrowse TO ROWID jobhdrRowID.
      jobBrowse:SELECT-FOCUSED-ROW().
    END. /* each job-hdr */
    RUN getJobAverages.
  END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyQtyFilter C-Win 
PROCEDURE applyQtyFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      qtyStart
      qtyEnd
      .
    jobBrowse:DESELECT-ROWS().
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ ipCompany
          AND job-hdr.est-no EQ ipEstNo
          AND job-hdr.qty GE qtyStart
          AND job-hdr.qty LE qtyEnd
        :
      jobhdrRowID = ROWID(job-hdr).
      REPOSITION jobBrowse TO ROWID jobhdrRowID.
      jobBrowse:SELECT-FOCUSED-ROW().
    END. /* each job-hdr */
    RUN getJobAverages.
  END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtblEstOp C-Win 
PROCEDURE createTtblEstOp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE ttblEstOp.

  FOR EACH est-op NO-LOCK WHERE {&estOpWhere}:
    CREATE ttblEstOp.
    ASSIGN
      ttblEstOp.s-num = est-op.s-num
      ttblEstOp.d-seq = est-op.d-seq
      ttblEstOp.b-num = est-op.b-num
      ttblEstOp.p-num = est-op.op-pass
      ttblEstOp.m-code = est-op.m-code
      ttblEstOp.m-dscr = est-op.m-dscr
      ttblEstOp.n-out = est-op.n-out
      ttblEstOp.estOpRowID = ROWID(est-op)
      .
  END. /* each est-op */


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
  DISPLAY allJobs radioQty selectionFilters closeStart jobStart qtyStart 
          closeEnd jobEnd qtyEnd quantityText jobText selectedJobs machineText 
          standardsText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSubmitDownLeft btnSubmitDownRight machineRect allJobRect jobRect 
         quantityRect standardsRect allJobs radioQty jobBrowse ttblEstOp 
         btnSubmit estOpBrowse selectionFilters quantityText jobText 
         selectedJobs machineText standardsText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getJobAverages C-Win 
PROCEDURE getJobAverages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE mrActHr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE runActHr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE mrWaste AS DECIMAL NO-UNDO.
  DEFINE VARIABLE runWaste AS DECIMAL NO-UNDO.
  DEFINE VARIABLE pctHr AS DECIMAL NO-UNDO INITIAL 1.00.
  DEFINE VARIABLE pctWaste AS DECIMAL NO-UNDO.
  DEFINE VARIABLE ct AS INTEGER NO-UNDO INITIAL 6.
  DEFINE VARIABLE rate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE mr-rate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE v-tspost-val AS CHARACTER NO-UNDO INITIAL 'Actual'.
  DEFINE VARIABLE opSpeeds AS DECIMAL NO-UNDO.
  DEFINE VARIABLE actQty AS DECIMAL NO-UNDO.
  DEFINE VARIABLE wstQty AS DECIMAL NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      selectedJobs:SCREEN-VALUE = STRING(jobBrowse:NUM-SELECTED-ROWS)
      selectedJobs
      .
    FOR EACH ttblEstOP:
      ASSIGN
        ttblEstOP.run-hr = 0
        ttblEstOP.mr-hr = 0
        ttblEstOP.run-waste = 0
        ttblEstOP.mr-waste = 0
        ttblEstOp.wst-prct = 0
        ttblEstOp.est-speed = 0
        ttblEstOp.std-hrs = 0
        ttblEstOp.std-mr-hrs = 0
        ttblEstOp.run-hrs = 0
        ttblEstOp.act-qty = 0
        ttblEstOp.wst-qty = 0
        ttblEstOp.op-speed = 0
        ttblEstOp.op-spoil = 0
        .
    END. /* each ttblestop */

    GET FIRST jobBrowse.

    DO idx = 1 TO jobBrowse:NUM-SELECTED-ROWS:
      BROWSE jobBrowse:FETCH-SELECTED-ROW(idx).
      FOR EACH job-mch NO-LOCK
          WHERE job-mch.company EQ job-hdr.company
            AND job-mch.job EQ job-hdr.job
         ,FIRST mach NO-LOCK
          WHERE mach.company EQ job-hdr.company
            AND mach.loc EQ g_loc
            AND mach.m-code EQ job-mch.m-code
          :

        {jc/jc-wipmr.i mach.run-crusiz mach.mr-crusiz "job-mch"}

        IF job-mch.frm EQ job-hdr.frm AND
          (job-mch.blank-no EQ job-hdr.blank-no OR
           job-mch.blank-no EQ 0) THEN
        ASSIGN pctWaste = IF job-mch.blank-no EQ 0 THEN job-hdr.sq-in * .01 ELSE 1.

        IF job-mch.j-no EQ 0 THEN DO:
          FIND FIRST ttblEstOp
               WHERE ttblEstOp.s-num EQ job-mch.frm
                 AND ttblEstOp.b-num EQ job-mch.blank-no
                 AND ttblEstOp.p-num EQ job-mch.pass
                 AND ttblEstOp.m-code EQ job-mch.m-code
                 NO-ERROR.
          IF NOT AVAILABLE ttblEstOp THEN
          FIND FIRST ttblEstOp
               WHERE ttblEstOp.s-num EQ job-mch.frm
                 AND ttblEstOp.p-num EQ job-mch.pass
                 AND ttblEstOp.m-code EQ job-mch.m-code
                 NO-ERROR.
          IF AVAILABLE ttblEstOp THEN DO:
            ASSIGN
              ttblEstOp.wst-prct = job-mch.wst-prct
              ttblEstOp.est-speed = job-mch.speed
              ttblEstOp.std-hrs = job-mch.run-hr
              ttblEstOp.std-mr-hrs = job-mch.mr-hr
              .
          END. /* avail ttblestop */
        END. /* j-no eq 0 */
      END. /* each job-mch */

      FOR EACH mch-act NO-LOCK
          WHERE mch-act.company EQ job-hdr.company
            AND mch-act.job EQ job-hdr.job
         ,FIRST mach NO-LOCK
          WHERE mach.company EQ job-hdr.company
            AND mach.loc EQ g_loc
            AND mach.m-code EQ mch-act.m-code
          BREAK BY mch-act.frm
                BY mch-act.blank-no
                BY mch-act.pass
                BY mch-act.m-code
          :

        {jc/jc-wipmr.i mch-act.crew mch-act.crew "mch-act"}

        IF mch-act.blank-no EQ 0 THEN
        ASSIGN pctWaste = IF mch-act.blank-no EQ 0 THEN job-hdr.sq-in * .01 ELSE 1.
        
        FIND FIRST job-code NO-LOCK
             WHERE job-code.code EQ mch-act.code
             NO-ERROR.
        IF NOT AVAILABLE job-code THEN NEXT.

        ASSIGN
          mrActHr = mrActHr + mch-act.hours * pctHr WHEN job-code.cat EQ 'MR'
          runActHr = runActHr + mch-act.hours * pctHr WHEN job-code.cat EQ 'RUN'
          .

        IF job-code.cat EQ 'MR' THEN
          IF ct LT 5 OR ct EQ 7 OR ct EQ 8 THEN
            mrWaste = mrWaste + mch-act.hours * mr-rate * pctWaste.
          ELSE
            mrWaste = mrWaste + (mch-act.qty + mch-act.waste) * pctWaste.
        ELSE IF CAN-DO('RUN,DT',job-code.cat) THEN DO:
          IF ct LT 5 OR ct EQ 7 OR ct EQ 8 THEN
            runWaste = runWaste + mch-act.hours * rate * pctWaste.
          ELSE
            IF ct EQ 5 THEN
              runWaste = runWaste + mch-act.qty * pctWaste.
            ELSE
              runWaste = runWaste + mch-act.waste * pctWaste.
          ASSIGN
            opSpeeds = opSpeeds + mch-act.hours * pctWaste
            actQty = actQty + mch-act.qty * pctWaste
            wstQty = wstQty + mch-act.waste * pctWaste
            .
        END. /* if run,dt */

        IF LAST-OF(mch-act.m-code) THEN DO:
          FIND FIRST ttblEstOp
               WHERE ttblEstOp.s-num EQ mch-act.frm
                 AND ttblEstOp.b-num EQ mch-act.blank-no
                 AND ttblEstOp.p-num EQ mch-act.pass
                 AND ttblEstOp.m-code EQ mch-act.m-code
               NO-ERROR.
          IF NOT AVAILABLE ttblEstOp THEN
          FIND FIRST ttblEstOp
               WHERE ttblEstOp.s-num EQ mch-act.frm
                 AND ttblEstOp.p-num EQ mch-act.pass
                 AND ttblEstOp.m-code EQ mch-act.m-code
               NO-ERROR.
          IF AVAILABLE ttblEstOp THEN DO:
            ASSIGN
              ttblEstOp.mr-hr = ttblEstOp.mr-hr + mrActHr
              ttblEstOp.run-hr = ttblEstOp.run-hr + runActHr
              ttblEstOp.mr-waste = ttblEstOp.mr-waste + mrWaste
              ttblEstOp.run-waste = ttblEstOp.run-waste + runWaste
              ttblEstOp.run-hrs = ttblEstOp.run-hrs + opSpeeds
              ttblEstOp.act-qty = ttblEstOp.act-qty + actQty
              ttblEstOp.wst-qty = ttblEstOp.wst-qty + wstQty
              .
          END. /* avail ttblestop */
          ASSIGN
            mrActHr = 0
            runActHr = 0
            mrWaste = 0
            runWaste = 0
            opSpeeds = 0
            actQty = 0
            wstQty = 0
            .
        END. /* last-of m-code */
      END. /* each mch-act */
    END. /* do idx */
    IF selectedJobs NE 0 THEN
    FOR EACH ttblEstOP:
      ASSIGN
        ttblEstOp.mr-hr = ttblEstOp.mr-hr / selectedJobs
        ttblEstOp.run-hr = ttblEstOp.run-hr / selectedJobs
        ttblEstOp.mr-waste = ttblEstOp.mr-waste / selectedJobs
        ttblEstOp.run-waste = ttblEstOp.run-waste / selectedJobs
        .
      IF ttblEstOp.run-hrs NE 0 THEN
      ttblEstOp.op-speed = ttblEstOp.act-qty / ttblEstOp.run-hrs.
      IF ttblEstOp.wst-qty + ttblEstOp.act-qty NE 0 THEN
      ttblEstOp.op-spoil = ttblEstOp.wst-qty / (ttblEstOp.wst-qty + ttblEstOp.act-qty) * 100.
      IF ttblEstOp.op-spoil LT 0 THEN ttblEstOp.op-spoil = 0.
    END. /* each ttblestop */
  END. /* do with frame */
  {&OPEN-QUERY-ttblEstOp}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQty C-Win 
PROCEDURE getQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE radioButtons AS CHARACTER NO-UNDO.
  DEFINE BUFFER xop FOR est-op.
  iv-eqty = 0.
  
  FOR EACH est-qty NO-LOCK
      WHERE est-qty.company EQ ipCompany
        AND est-qty.est-no EQ ipEstNo
      :
    radioButtons = radioButtons
                 + STRING(est-qty.eqty) + ','
                 + STRING(est-qty.eqty) + ','.
  END. /* each est-qty */
  
  
    IF iEstType EQ 1 THEN iv-eqty = INT(ENTRY(1,radioButtons)) .

    ELSE
    FOR EACH xop NO-LOCK
        WHERE xop.company EQ ipCompany
          AND xop.est-no  EQ ipEstNo
          AND xop.line    LT 500
        BY xop.qty:
      iv-eqty = xop.qty.
      LEAVE.
    END.

  ASSIGN
    radioButtons = TRIM(radioButtons,',')
    radioQty:RADIO-BUTTONS IN FRAME {&FRAME-NAME} = radioButtons
    radioQty:WIDTH-PIXELS = NUM-ENTRIES(radioButtons) / 2 * 60
    quantityRect:WIDTH-PIXELS = radioQty:WIDTH-PIXELS + 20
    qtyStart = INT(ENTRY(1,radioButtons))
    qtyEnd = INT(ENTRY(NUM-ENTRIES(radioButtons),radioButtons))
    .
  APPLY 'VALUE-CHANGED':U TO radioQty.
  RUN createTtblEstOp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadFilters C-Win 
PROCEDURE loadFilters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE jobNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE closeDate AS DATE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      jobStart:LIST-ITEMS = ?
      jobEnd:LIST-ITEMS = ?
      .
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ ipCompany
          AND job-hdr.est-no EQ ipEstNo
       ,FIRST job OF job-hdr NO-LOCK
          :
      IF closeDate EQ ? OR job.close-date LT closeDate THEN
      closeDate = job.close-date.
      jobNo = job-hdr.job-no + '-' + STRING(job-hdr.job-no2).
      jobStart:ADD-LAST(jobNo).
      jobEnd:ADD-LAST(jobNo).
      ASSIGN
        jobStart:INNER-LINES = jobStart:NUM-ITEMS
        jobStart:SCREEN-VALUE = jobStart:ENTRY(1)
        jobEnd:INNER-LINES = jobEnd:NUM-ITEMS
        jobEnd:SCREEN-VALUE = jobEnd:ENTRY(jobEnd:NUM-ITEMS)
        .
    END. /* each job-hdr */
    ASSIGN
      closeStart:SCREEN-VALUE = STRING(closeDate)
      closeEnd:SCREEN-VALUE = STRING(TODAY)
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMCodeCol C-Win 
PROCEDURE setMCodeCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    ttblEstOp.mcode = ttblEstOp.opSpeed AND ttblEstOp.mrHR AND ttblEstOp.opSpoil AND ttblEstOp.mrStd
    /*mCode:SCREEN-VALUE IN BROWSE ttblEstOp = STRING(ttblEstOp.mcode)*/
    mCodeCol = NO
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE submitJobData C-Win 
PROCEDURE submitJobData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ttblEstOp:
    IF ttblEstOp.opSpeed OR ttblEstOp.mrHR OR
       ttblEstOp.opSpoil OR ttblEstOp.mrStd THEN DO:

      FIND est-op EXCLUSIVE-LOCK WHERE ROWID(est-op) EQ ttblEstOp.estOpRowID NO-ERROR.
      IF NOT AVAILABLE est-op THEN NEXT.
      
      IF ttblEstOp.opSpeed THEN
      est-op.op-speed = ttblEstOp.op-speed.
      
      IF ttblEstOp.mrHR THEN
      est-op.op-mr = ttblEstOp.mr-hr.
      
      IF ttblEstOp.opSpoil THEN
      est-op.op-spoil = ttblEstOp.op-spoil.
      
      IF ttblEstOp.mrStd THEN
      est-op.op-waste = ttblEstOp.mr-waste.
      
      RELEASE est-op.
    END. /* if any toggles checked */
  END. /* each ttblest-op */
  {&OPEN-QUERY-estOpBrowse}

  IF VALID-HANDLE(ipQuery) THEN
  RUN dispatch IN ipQuery ('open-query').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize C-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  ASSIGN
    {&WINDOW-NAME}:WINDOW-STATE = 1
    {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - 30
    {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
    jobRect:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 53
    jobBrowse:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 68
    .
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN LockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

