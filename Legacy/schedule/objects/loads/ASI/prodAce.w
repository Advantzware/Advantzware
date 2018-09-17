&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : prodAce.w
    Purpose     : capture prodAce transactions into machtran

    Syntax      : run prodAce.w (input prodAce data file,
                                 input container handle,
                                 output continue)

    Description : ProdAce Interface

    Author(s)   : Ron Stark
    Created     :  7.14.2017 copied from vorne.w
    ReWritten   : 
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER lvProdAceDat AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iphContainerHandle AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER iplAutoMonitor AS LOGICAL NO-UNDO.

DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
 
{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/{&Board}/calcEnd.i}

DEFINE VARIABLE lvProdAceFile AS CHARACTER NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE lvProdAceData AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceBlank AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAcePass AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceOperator AS CHARACTER NO-UNDO EXTENT 10.
DEFINE VARIABLE lvAttrList AS CHARACTER NO-UNDO FORMAT 'x(4)'.
DEFINE VARIABLE lvFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvTemp AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProcessed AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvArchive AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvErrorFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE jobMchRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvShifts AS CHARACTER NO-UNDO INIT 'First,Second,Third,Fourth,Fifth,Sixth'.
DEFINE VARIABLE lvPostProdAce AS LOGICAL NO-UNDO.
DEFINE VARIABLE lvHoldFile AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttToggleBox NO-UNDO
    FIELD hToggleBox AS HANDLE
    FIELD rResource AS ROWID
    .
DEFINE TEMP-TABLE ttblProductID NO-UNDO
    FIELD productID AS CHARACTER 
    FIELD dmiID AS INTEGER 
    FIELD productDesc AS CHARACTER 
    FIELD standardCycle AS DECIMAL 
        INDEX ttblProductID IS PRIMARY 
              productID
              dmiID
              .
DEFINE TEMP-TABLE ttblStatus NO-UNDO 
    FIELD dmiID AS INTEGER
    FIELD job AS CHARACTER 
    FIELD blank-no AS INTEGER 
    FIELD pass AS INTEGER 
    FIELD productID AS CHARACTER 
    FIELD runID AS INTEGER 
    FIELD runComplete AS LOGICAL
        INDEX ttblStatus IS PRIMARY 
              runID
              .
DEFINE TEMP-TABLE ttblProdAce NO-UNDO
    FIELD prodAceResource AS CHARACTER 
    FIELD prodAceDMIID AS INTEGER 
    FIELD prodAceJob AS CHARACTER 
    FIELD prodAceItem AS CHARACTER 
    FIELD prodAceSeq AS INTEGER 
    FIELD prodAceShift AS CHARACTER  
    FIELD prodAceShiftDate AS DATE 
    FIELD prodAceStartDate AS DATE 
    FIELD prodAceStartTime AS INTEGER  
    FIELD prodAceEndDate AS DATE 
    FIELD prodAceEndTime AS INTEGER     
    FIELD prodAceDuration AS INTEGER 
    FIELD prodAceTranRunQty AS INTEGER 
    FIELD prodAceTranRejectQty AS INTEGER 
    FIELD prodAceQtyDue AS INTEGER 
    FIELD prodAceState AS CHARACTER 
    FIELD prodAceChargeCode AS CHARACTER 
    FIELD prodAceRunComplete AS LOGICAL 
    FIELD deleteFlag AS LOGICAL 
    FIELD prodAceOperator AS CHARACTER EXTENT 10
    FIELD prodAceSelected AS LOGICAL INITIAL YES
    FIELD tempSelected AS LOGICAL INITIAL YES
    FIELD prodAceData AS CHARACTER 
        INDEX ttblProdAceDetail IS PRIMARY
              prodAceResource
              prodAceJob
              prodAceItem
              prodAceSeq
              .
DEFINE BUFFER buffProdAce FOR ttblProdAce.

DEFINE STREAM sProdAce.
DEFINE STREAM sHold.
DEFINE STREAM sProcessed.
DEFINE STREAM sError.

{AOA/includes/dateOptionDef.i}

SESSION:SET-WAIT-STATE('').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnExportShifts btnReset btnSave btnImport ~
setAllResources selectedShift btnExportEmployees selectedStartDate ~
btnCalendar-1 selectedStartDateOption selectedEndDate btnCalendar-2 ~
btnExportJobs selectedEndDateOption selectedStartDueDate btnCalendar-3 ~
selectedStartDueDateOption selectedEndDueDate btnCalendar-4 ~
selectedEndDueDateOption svAllJobNo btnExportMachines svStartJobNo ~
svStartJobNo2 svEndJobNo svEndJobNo2 lvProdAceDir lvImportDir lvProdAceType ~
lvProdAceBlankEmployee lvResourceList 
&Scoped-Define DISPLAYED-OBJECTS setAllResources selectedShift ~
selectedStartDate selectedStartDateOption selectedEndDate ~
selectedEndDateOption selectedStartDueDate selectedStartDueDateOption ~
selectedEndDueDate selectedEndDueDateOption svAllJobNo svStartJobNo ~
svStartJobNo2 svEndJobNo svEndJobNo2 lvProdAceDir lvImportDir lvProdAceType ~
lvEmpLogin lvProdAceBlankEmployee lvResourceList 

/* Custom List Definitions                                              */
/* ProdAceDatValues,List-2,List-3,List-4,List-5,List-6                  */
&Scoped-define ProdAceDatValues lvProdAceDir lvImportDir lvProdAceType ~
lvEmpLogin lvProdAceBlankEmployee lvResourceList 
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
btnCalendar-4 btnTransSelection 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnExportEmployees 
     IMAGE-UP FILE "Graphics/32x32/users3.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 7.2 BY 1.71 TOOLTIP "Export Employees to Production ACE".

DEFINE BUTTON btnExportJobs 
     IMAGE-UP FILE "Graphics/32x32/tools.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 7.2 BY 1.71 TOOLTIP "Export Jobs to Production ACE".

DEFINE BUTTON btnExportMachines 
     IMAGE-UP FILE "Graphics/32x32/gearwheels.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 7.2 BY 1.71 TOOLTIP "Export Resources to Production ACE".

DEFINE BUTTON btnExportShifts 
     IMAGE-UP FILE "Graphics/32x32/calendar_clock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Export" 
     SIZE 7.2 BY 1.71 TOOLTIP "Export Shifts to Production ACE".

DEFINE BUTTON btnImport 
     IMAGE-UP FILE "Graphics/32x32/tools.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Import" 
     SIZE 7.2 BY 1.71 TOOLTIP "Import Job Transactions from Production ACE".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.2 BY 1.71 TOOLTIP "Reset"
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.2 BY 1.71 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE BUTTON btnTransSelection 
     IMAGE-UP FILE "AOA/images/aoashowbatch.jpg":U
     LABEL "Transaction Selection" 
     SIZE 6 BY 1.14 TOOLTIP "Transaction Selection".

DEFINE VARIABLE selectedEndDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE selectedEndDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE selectedStartDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE selectedStartDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE lvImportDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Import Path" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvProdAceBlankEmployee AS CHARACTER FORMAT "X(256)":U 
     LABEL "Blank Employee" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvProdAceDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Export Path" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lvResourceList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resources" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE selectedEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE selectedEndDueDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/49 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE selectedStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE selectedStartDueDate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/50 
     LABEL "Start Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svEndJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "End Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svStartJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "Start Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE lvEmpLogin AS CHARACTER INITIAL "ProdAce" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Prod Ace", "ProdAce",
"Touch Screen", "Touch Screen"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE lvProdAceType AS CHARACTER INITIAL "Summary" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Summary", "Summary",
"Detail", "Detail"
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE selectedShift AS CHARACTER INITIAL "All" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "First Shift", "First Shift",
"Second Shift", "Second Shift",
"Third Shift", "Third Shift",
"All", "All"
     SIZE 19 BY 3.81 TOOLTIP "Select Shift" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 7.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 6.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 6.19.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 78 BY 21.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 17 BY 2.14
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 33 BY 2.14
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 9 BY 2.14
     BGCOLOR 15 .

DEFINE VARIABLE setAllResources AS LOGICAL INITIAL yes 
     LABEL "Toggle (On/Off) Resources Selected" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE svAllJobNo AS LOGICAL INITIAL yes 
     LABEL "All Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnExportShifts AT ROW 12.19 COL 72 HELP
          "Export Shifts to Production ACE" WIDGET-ID 18
     btnReset AT ROW 20.52 COL 64 HELP
          "Click to Reset Values"
     btnSave AT ROW 20.52 COL 72 HELP
          "Click to Save"
     btnImport AT ROW 5.52 COL 72 HELP
          "Import from Production Ace" WIDGET-ID 6
     setAllResources AT ROW 2.19 COL 85 HELP
          "Select to Toggle All Resources (On/Off)" WIDGET-ID 30
     selectedShift AT ROW 2.91 COL 3 HELP
          "Select Shift" NO-LABEL
     btnExportEmployees AT ROW 12.19 COL 64 HELP
          "Export Employees to Production ACE" WIDGET-ID 14
     selectedStartDate AT ROW 2.91 COL 32 COLON-ALIGNED HELP
          "Enter Starting Date"
     btnCalendar-1 AT ROW 2.91 COL 50 WIDGET-ID 76
     selectedStartDateOption AT ROW 2.91 COL 53 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 74
     selectedEndDate AT ROW 4.1 COL 32 COLON-ALIGNED HELP
          "Enter Ending Date"
     btnCalendar-2 AT ROW 4.1 COL 50 WIDGET-ID 78
     btnExportJobs AT ROW 12.19 COL 48 HELP
          "Export Jobs to Production ACE" WIDGET-ID 26
     selectedEndDateOption AT ROW 4.1 COL 53 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 70
     selectedStartDueDate AT ROW 8.62 COL 25 COLON-ALIGNED HELP
          "Enter Starting Due Date" WIDGET-ID 82
     btnCalendar-3 AT ROW 8.62 COL 43 WIDGET-ID 86
     selectedStartDueDateOption AT ROW 8.62 COL 46 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 90
     selectedEndDueDate AT ROW 9.81 COL 25 COLON-ALIGNED HELP
          "Enter Ending Due Date" WIDGET-ID 84
     btnCalendar-4 AT ROW 9.81 COL 43 WIDGET-ID 88
     selectedEndDueDateOption AT ROW 9.81 COL 46 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 92
     svAllJobNo AT ROW 11 COL 27 HELP
          "All Jobs?" WIDGET-ID 174
     btnExportMachines AT ROW 12.19 COL 56 HELP
          "Export Machines to Production ACE" WIDGET-ID 4
     svStartJobNo AT ROW 11.95 COL 25 COLON-ALIGNED HELP
          "Enter Start Job" WIDGET-ID 178
     svStartJobNo2 AT ROW 11.95 COL 36 COLON-ALIGNED HELP
          "Enter Start Job Run" WIDGET-ID 180
     svEndJobNo AT ROW 13.14 COL 25 COLON-ALIGNED HELP
          "Enter End Job" WIDGET-ID 176
     svEndJobNo2 AT ROW 13.14 COL 36 COLON-ALIGNED HELP
          "Enter End Job Run" WIDGET-ID 182
     lvProdAceDir AT ROW 15.52 COL 21 COLON-ALIGNED
     lvImportDir AT ROW 16.71 COL 21 COLON-ALIGNED
     lvProdAceType AT ROW 17.91 COL 24 NO-LABEL
     btnTransSelection AT ROW 17.91 COL 50 HELP
          "Access Transaction Selection" WIDGET-ID 32
     lvEmpLogin AT ROW 19.1 COL 24 NO-LABEL
     lvProdAceBlankEmployee AT ROW 20.29 COL 21 COLON-ALIGNED
     lvResourceList AT ROW 21.48 COL 21 COLON-ALIGNED
     "Employee Login:" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 19.1 COL 6
     "Type:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 17.91 COL 16
     " Configuration" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 14.57 COL 4 WIDGET-ID 38
          FONT 6
     " Select to Set Current (1st) Job Per Resource" VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 1.24 COL 85 WIDGET-ID 190
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161 BY 22.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     " Export" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 7.91 COL 4 WIDGET-ID 40
          FONT 6
     "Select Shift to Post ... Enter Date Range" VIEW-AS TEXT
          SIZE 49 BY .62 AT ROW 2.19 COL 3
          FONT 6
     " Import" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.24 COL 4 WIDGET-ID 36
          FONT 6
     RECT-1 AT ROW 14.81 COL 2
     RECT-2 AT ROW 8.14 COL 2 WIDGET-ID 2
     RECT-3 AT ROW 1.48 COL 2 WIDGET-ID 34
     RECT-4 AT ROW 1.48 COL 83 WIDGET-ID 80
     RECT-5 AT ROW 20.29 COL 63 WIDGET-ID 184
     RECT-6 AT ROW 11.95 COL 47 WIDGET-ID 186
     RECT-7 AT ROW 5.29 COL 71 WIDGET-ID 188
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 161 BY 22.


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
         TITLE              = "Production ACE DMI"
         HEIGHT             = 22
         WIDTH              = 161
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 161
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 161
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR BUTTON btnTransSelection IN FRAME DEFAULT-FRAME
   NO-ENABLE 3                                                          */
ASSIGN 
       btnTransSelection:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET lvEmpLogin IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN lvImportDir IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvProdAceBlankEmployee IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvProdAceDir IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RADIO-SET lvProdAceType IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN lvResourceList IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Production ACE DMI */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Production ACE DMI */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME DEFAULT-FRAME
DO:
  {methods/btnCalendar.i selectedStartDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME DEFAULT-FRAME
DO:
  {methods/btnCalendar.i selectedEndDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 C-Win
ON CHOOSE OF btnCalendar-3 IN FRAME DEFAULT-FRAME
DO:
  {methods/btnCalendar.i selectedStartDueDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 C-Win
ON CHOOSE OF btnCalendar-4 IN FRAME DEFAULT-FRAME
DO:
  {methods/btnCalendar.i selectedEndDueDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportEmployees
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportEmployees C-Win
ON CHOOSE OF btnExportEmployees IN FRAME DEFAULT-FRAME /* Export */
DO:
    RUN pExport ('Employees').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportJobs C-Win
ON CHOOSE OF btnExportJobs IN FRAME DEFAULT-FRAME /* Export */
DO:
    RUN pExport ('Jobs').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportMachines
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportMachines C-Win
ON CHOOSE OF btnExportMachines IN FRAME DEFAULT-FRAME /* Export */
DO:
    RUN pExport ('Charge Codes,Machines,Machine Codes').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportShifts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportShifts C-Win
ON CHOOSE OF btnExportShifts IN FRAME DEFAULT-FRAME /* Export */
DO:
    RUN pExport ('Shifts').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImport C-Win
ON CHOOSE OF btnImport IN FRAME DEFAULT-FRAME /* Import */
DO:
  ASSIGN
    selectedShift
    selectedStartDate
    selectedEndDate
    opContinue = YES
    lvPostProdAce = YES
    .
  RUN postProdAce.
  RETURN NO-APPLY.
/*  RUN pImport.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME
DO:
  RUN getProdAceDatValues.
  DISPLAY {&prodAceDatValues} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME
DO:
  ASSIGN {&prodAceDatValues}.
  RUN saveProdAceDatValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTransSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTransSelection C-Win
ON CHOOSE OF btnTransSelection IN FRAME DEFAULT-FRAME /* Transaction Selection */
DO:
    RUN pTransSelection.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedEndDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedEndDate C-Win
ON HELP OF selectedEndDate IN FRAME DEFAULT-FRAME /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedEndDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedEndDateOption C-Win
ON VALUE-CHANGED OF selectedEndDateOption IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/tDateOption.i &dateObject=selectedEndDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedEndDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedEndDueDate C-Win
ON HELP OF selectedEndDueDate IN FRAME DEFAULT-FRAME /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedEndDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedEndDueDateOption C-Win
ON VALUE-CHANGED OF selectedEndDueDateOption IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/tDateOption.i &dateObject=selectedEndDueDate &btnCalendar=4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedShift C-Win
ON VALUE-CHANGED OF selectedShift IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
    {&SELF-NAME}
    selectedEndDate = TODAY
    .
  CASE {&SELF-NAME}:
    WHEN 'First' OR WHEN 'Second' THEN
    selectedStartDate = TODAY.
    WHEN 'Third' THEN
    selectedStartDate = TODAY - 1.
  END CASE.
  DISPLAY selectedStartDate selectedEndDate WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedStartDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedStartDate C-Win
ON HELP OF selectedStartDate IN FRAME DEFAULT-FRAME /* Start Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedStartDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedStartDateOption C-Win
ON VALUE-CHANGED OF selectedStartDateOption IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/tDateOption.i &dateObject=selectedStartDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedStartDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedStartDueDate C-Win
ON HELP OF selectedStartDueDate IN FRAME DEFAULT-FRAME /* Start Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME selectedStartDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL selectedStartDueDateOption C-Win
ON VALUE-CHANGED OF selectedStartDueDateOption IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/tDateOption.i &dateObject=selectedStartDueDate &btnCalendar=3}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setAllResources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setAllResources C-Win
ON VALUE-CHANGED OF setAllResources IN FRAME DEFAULT-FRAME /* Toggle (On/Off) Resources Selected */
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttToggleBox:
        ttToggleBox.hToggleBox:CHECKED = {&SELF-NAME}.
    END. /* each tttogglebox */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllJobNo C-Win
ON VALUE-CHANGED OF svAllJobNo IN FRAME DEFAULT-FRAME /* All Jobs */
DO:
    {AOA/includes/svAllValueChanged.i svStartJobNo svEndJobNo}
    {AOA/includes/svAllValueChanged.i svStartJobNo2 svEndJobNo2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
DISABLE TRIGGERS FOR LOAD OF machemp.
DISABLE TRIGGERS FOR LOAD OF machtran.
DISABLE TRIGGERS FOR LOAD OF emplogin.
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  
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
  RUN getProdAceDatValues.
  ASSIGN
    selectedStartDate = TODAY
    selectedEndDate = TODAY
    .
  RUN enable_UI.
  RUN pPopulateOptions (hContainer).
  RUN createResourceToggleBoxes.
  IF ID EQ "ASI/DMI" THEN DO:
      setAllResources:SCREEN-VALUE = "NO".
      APPLY "VALUE-CHANGED":U TO setAllResources.
  END. /* if dmi */
  IF iplAutoMonitor THEN DO:
    APPLY 'CHOOSE':U TO btnImport.
    RETURN.
  END. /* if auto monitor */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE completeMR C-Win 
PROCEDURE completeMR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND LAST machtran EXCLUSIVE-LOCK
       WHERE machtran.company EQ job-mch.company
         AND machtran.machine EQ ttblProdAce.prodAceResource
         AND machtran.job_number EQ job-mch.job-no
         AND machtran.job_sub EQ job-mch.job-no2
         AND machtran.form_number EQ job-mch.frm
         AND machtran.blank_number EQ job-mch.blank-no
         AND machtran.pass_sequence EQ job-mch.pass
         AND machtran.charge_code EQ 'MR'
       NO-ERROR.
  IF AVAILABLE machtran THEN
  machtran.completed = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createEmpLogin C-Win 
PROCEDURE createEmpLogin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER ttblProdAce FOR ttblProdAce.
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.

  DO idx = 1 TO EXTENT(lvProdAceOperator):
      IF ttblProdAce.prodAceOperator[idx] EQ '' THEN NEXT.
      FIND FIRST employee NO-LOCK
           WHERE employee.employee EQ STRING(ttblProdAce.prodAceOperator[idx])
           NO-ERROR.
      IF AVAILABLE employee THEN DO:
        IF CAN-FIND(FIRST emplogin
                    WHERE emplogin.company EQ employee.company
                      AND emplogin.employee EQ employee.employee
                      AND emplogin.start_date EQ machtran.start_date
                      AND emplogin.start_time EQ machtran.start_time
                      AND emplogin.machine EQ ttblProdAce.prodAceResource) THEN
        IF idx EQ 1 THEN
        ASSIGN
          machtran.start_time = machtran.start_time + 1
          machtran.total_time = machtran.total_time + 1
          .
        CREATE empLogin.
        ASSIGN
          emplogin.company = employee.company
          emplogin.employee = employee.employee
          emplogin.machine = ttblProdAce.prodAceResource
          emplogin.start_date = machtran.start_date
          emplogin.start_time = machtran.start_time
          emplogin.end_date = machtran.end_date
          emplogin.end_time = machtran.end_time
          emplogin.total_time = machtran.total_time
          emplogin.shift = ttblProdAce.prodAceShift
          .
        RUN createMachEmp (BUFFER machtran, INPUT ROWID(emplogin)).
      END. /* avail employee */
  END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createMachEmp C-Win 
PROCEDURE createMachEmp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  DEFINE INPUT PARAMETER ipEmpLoginRowID AS ROWID NO-UNDO.

  FIND FIRST emplogin NO-LOCK
       WHERE ROWID(emplogin) EQ ipEmpLoginRowID NO-ERROR.
  IF NOT AVAILABLE emplogin THEN RETURN.

  FIND FIRST employee NO-LOCK
       WHERE employee.company EQ emplogin.company
         AND employee.employee EQ emplogin.employee
       NO-ERROR.
  IF NOT AVAILABLE employee THEN RETURN.

  CREATE machemp.
  ASSIGN
    machemp.table_rec_key = machtran.rec_key
    machemp.employee = emplogin.employee
    machemp.start_date = IF machtran.start_date GT emplogin.start_date THEN machtran.start_date ELSE emplogin.start_date
    machemp.start_time = IF machtran.start_date GT emplogin.start_date THEN machtran.start_time ELSE emplogin.start_time
    machemp.shift = machtran.shift
    machemp.ratetype = 'Standard' 
    machemp.rate_usage = employee.rate_usage
    machemp.end_date = IF machtran.end_date LT emplogin.end_date OR emplogin.end_date EQ ? THEN machtran.end_date ELSE emplogin.end_date
    machemp.end_time = IF machtran.end_date LT emplogin.end_date OR emplogin.end_date EQ ? THEN machtran.end_time ELSE emplogin.end_time
    .
  RUN employeeRate(machtran.company,machemp.employee,machemp.shift,machtran.machine,
                   machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
  IF machemp.start_date EQ machemp.end_date THEN
  machemp.total_time = machemp.end_time - machemp.start_time.
  ELSE
  machemp.total_time = (86400 - machemp.start_time)
                     + (machemp.end_date - machemp.start_date - 1) * 86400
                     +  machemp.end_time.
  /*if end_date is blank, set total_time to 0*/
  IF machemp.total_time LT 0 OR machemp.total_time EQ ? THEN
     machemp.total_time = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createResourceToggleBoxes C-Win 
PROCEDURE createResourceToggleBoxes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DELETE WIDGET-POOL "resourceTogglePool" NO-ERROR.
    CREATE WIDGET-POOL "resourceTogglePool" PERSISTENT.

    DEFINE VARIABLE dCol    AS DECIMAL NO-UNDO INITIAL 89.
    DEFINE VARIABLE dRow    AS DECIMAL NO-UNDO INITIAL 3.35.
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    
    FOR EACH ttblResource
        WHERE ttblResource.dmiID GT 0
        :
        IF dRow + .95 GT FRAME {&FRAME-NAME}:HEIGHT THEN
        ASSIGN
            {&WINDOW-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT + .95
            {&WINDOW-NAME}:HEIGHT = {&WINDOW-NAME}:VIRTUAL-HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT
            .
        CREATE TOGGLE-BOX hWidget IN WIDGET-POOL "resourceTogglePool"
            ASSIGN
              FRAME = FRAME {&FRAME-NAME}:HANDLE
              FORMAT = "X(256)"
              ROW = dRow
              COL = dCol
              WIDTH = 70
              HEIGHT = .81
              SENSITIVE = YES
              HIDDEN = NO
              LABEL = ttblResource.resource + " - "
                    + ttblResource.resourceDescription
                    .
        /*
        TRIGGERS:
          ON VALUE-CHANGE
            PERSISTENT RUN pClick IN THIS-PROCEDURE (hWidget:HANDLE).
        END TRIGGERS.
        */
        CREATE ttToggleBox.
        ASSIGN
          ttToggleBox.hToggleBox = hWidget
          ttToggleBox.rResource = ROWID(ttblResource)
          dRow = dRow + .95
          hWidget:CHECKED = YES
          .
    END. /* each ttblresource */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTtblProdAce C-Win 
PROCEDURE createTtblProdAce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipProdAceFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE lvProdAceDMIID AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvState AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvChargeCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvTemp AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE ttblStatus.
  ASSIGN 
    lvFile = lvProdAceDir + '/wmsjobs.dat'
    lvTemp = REPLACE(lvFile,".dat",".tmp")
    .
  IF SEARCH(lvFile) NE ? THEN DO:
    OS-RENAME VALUE(lvFile) VALUE(lvTemp).
    INPUT STREAM sProdAce FROM VALUE(lvTemp).
    REPEAT:
      IMPORT STREAM sProdAce UNFORMATTED lvProdAceData.
      /* check if valid entry to process */
      IF INDEX(ENTRY(2,lvProdAceData),"Invalid WO") NE 0 THEN NEXT.
      /* check if valid entry to process */
      IF INDEX(ENTRY(2,lvProdAceData),"n/f") NE 0 THEN NEXT.
      /* make sure job is in long format */
      IF NUM-ENTRIES(ENTRY(2,lvProdAceData),' ') LT 2 THEN NEXT.
      CREATE ttblStatus.
      ASSIGN
        lvProdAceData = REPLACE(lvProdAceData,', ',',')
        lvProdAceData = REPLACE(lvProdAceData,'"','')
        ttblStatus.dmiID = INT(ENTRY(1,lvProdAceData))
        ttblStatus.job = ENTRY(2,lvProdAceData)
        ttblStatus.blank-no = INT(ENTRY(2,ttblStatus.job,' '))
        ttblStatus.pass = INT(ENTRY(3,ttblStatus.job,' '))
        ttblStatus.job = ENTRY(1,ttblStatus.job,' ')
        ttblStatus.productID = ENTRY(3,lvProdAceData)
        ttblStatus.runID = INT(ENTRY(6,lvProdAceData))
        ttblStatus.runComplete = ENTRY(10,lvProdAceData) EQ 'C'
        .
    END. /* repeat */
    OUTPUT STREAM sProdAce CLOSE.
  END. /* if search */

  EMPTY TEMP-TABLE ttblProdAce.
  INPUT STREAM sProdAce FROM VALUE(ipProdAceFile).
  IF lvPostProdAce THEN DO:
    OUTPUT STREAM sHold TO VALUE(lvHoldFile).
    OUTPUT STREAM sProcessed TO VALUE(lvProcessed).
  END. /* if lvpostprodAce */
  REPEAT:
    IMPORT STREAM sProdAce UNFORMATTED lvProdAceData.
    IF ENTRY(2,lvProdAceData) EQ 'n/f' THEN NEXT.
    lvProdAceDMIID = INTEGER(ENTRY(1,lvProdAceData)).
    FIND FIRST ttblResource
         WHERE ttblResource.dmiID EQ lvProdAceDMIID
         NO-ERROR.
    IF NOT AVAILABLE ttblResource THEN NEXT.
    IF lvResourceList NE '' AND NOT CAN-DO(lvResourceList,ttblResource.resource) THEN NEXT.
    ASSIGN
      lvDate = DATE(ENTRY(9,lvProdAceData))
      lvTime = INT(SUBSTR(ENTRY(10,lvProdAceData),1,2)) * 3600
             + INT(SUBSTR(ENTRY(10,lvProdAceData),4,2)) * 60
             + INT(SUBSTR(ENTRY(10,lvProdAceData),7,2))
             .
    IF lvPostProdAce AND
      ((selectedShift NE 'All' AND
        selectedShift NE ENTRY(5,lvProdAceData)) OR
        lvDate LT selectedStartDate OR
        lvDate GT selectedEndDate) THEN DO:
      PUT STREAM sHold UNFORMATTED lvProdAceData SKIP.
      NEXT.
    END. /* if prodAceshift ne */
    ASSIGN
      lvProdAceResource = ttblResource.resource
      lvProdAceJob      = ENTRY(2,lvProdAceData)
      lvProdAceBlank    = ENTRY(2,lvProdAceJob,' ')
      lvProdAcePass     = ENTRY(3,lvProdAceJob,' ')
      lvProdAceJob      = ENTRY(1,lvProdAceJob,' ')
      .
    IF NOT CAN-FIND(FIRST ttblJob
                    WHERE ttblJob.resource EQ lvProdAceResource
                      AND ttblJob.job EQ lvProdAceJob) AND
       NOT CAN-FIND(FIRST pendingJob
                    WHERE pendingJob.resource EQ lvProdAceResource
                      AND pendingJob.job EQ lvProdAceJob) THEN DO:
      PUT STREAM sError UNFORMATTED lvProdAceData SKIP.
      NEXT.
    END. /* cannot find job in SB */
    lvProdAceOperator = ''.
    DO idx = 21 TO NUM-ENTRIES(lvProdAceData):
      lvProdAceOperator[idx - 20] = IF ENTRY(idx,lvProdAceData) EQ '' THEN lvProdAceBlankEmployee
                                  ELSE ENTRY(idx,lvProdAceData).
      IF ENTRY(idx,lvProdAceData) EQ '' THEN LEAVE.
    END. /* do idx */
    ASSIGN
        lvState = SUBSTR(ENTRY(16,lvProdAceData),1,1)
        lvState = IF lvState EQ '1' THEN 'RUN'
             ELSE IF lvState EQ '4' THEN 'MR'
             ELSE 'DT'
        lvChargeCode = lvState
             . 
    /* get charge code for non run and mr */
    IF lvState EQ 'DT' AND INT(ENTRY(17,lvProdAceData)) NE 0 THEN DO: 
      FIND FIRST job-code NO-LOCK 
           WHERE job-code.dmiID EQ INT(ENTRY(17,lvProdAceData))
           NO-ERROR.
      IF AVAILABLE job-code THEN
      ASSIGN 
        lvState = job-code.cat
        lvChargeCode = job-code.code
        .
    END. /* if dt and dt reason given */
    CREATE ttblProdAce.
    ASSIGN
      ttblProdAce.prodAceResource = lvProdAceResource
      ttblProdAce.prodAceDMIID = lvProdAceDMIID
      ttblProdAce.prodAceJob = lvProdAceJob
      ttblProdAce.prodAceItem = ENTRY(3,lvProdAceData)
      ttblProdAce.prodAceSeq = INT(ENTRY(4,lvProdAceData))
      ttblProdAce.prodAceShift = ENTRY(5,lvProdAceData)
      ttblProdAce.prodAceShiftDate = DATE(ENTRY(6,lvProdAceData))
      ttblProdAce.prodAceStartDate = DATE(ENTRY(7,lvProdAceData))
      ttblProdAce.prodAceStartTime = lvTime
      ttblProdAce.prodAceTranRunQty = INT(ENTRY(11,lvProdAceData))
      ttblProdAce.prodAceTranRejectQty = INT(ENTRY(13,lvProdAceData))
      ttblProdAce.prodAceQtyDue = INT(ENTRY(15,lvProdAceData))
      ttblProdAce.prodAceState = lvState
      ttblProdAce.prodAceChargeCode = lvChargeCode
      ttblProdAce.prodAceOperator = lvProdAceOperator
      ttblProdAce.prodAceDuration = INT(ENTRY(18,lvProdAceData)) * 60
                                  + INT(ENTRY(20,lvProdAceData)) * 60
      ttblProdAce.prodAceRunComplete = CAN-FIND(FIRST ttblStatus
                                                WHERE ttblStatus.dmiID EQ ttblProdAce.prodAceDMIID
                                                  AND ttblStatus.job EQ ttblProdAce.prodAceJob
                                                  AND ttblStatus.productID EQ ttblProdAce.prodAceItem
                                                  AND ttblStatus.runID EQ ttblProdAce.prodAceSeq
                                                  AND ttblStatus.runComplete EQ YES)
                                                  AND ttblProdAce.prodAceState EQ 'RUN'
      ttblProdAce.prodAceData = lvProdAceData
      .
    RUN newEnd (ttblProdAce.prodAceDuration, ttblProdAce.prodAceStartDate, ttblProdAce.prodAceStartTime,
                OUTPUT ttblProdAce.prodAceEndDate, OUTPUT ttblProdAce.prodAceEndTime).
    IF lvPostProdAce THEN
    PUT STREAM sProcessed UNFORMATTED lvProdAceData SKIP.
  END. /* repeat */
  IF lvPostProdAce THEN DO:
    OUTPUT STREAM sHold CLOSE.
    OUTPUT STREAM sProcessed CLOSE.
  END. /* if lvpostprodAce */
  INPUT STREAM sProdAce CLOSE.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE employeeRate C-Win 
PROCEDURE employeeRate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipEmployee AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipShift AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRate_usage AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipRatetype AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opRate AS DECIMAL NO-UNDO.

  DEFINE BUFFER bRate FOR rate.

  IF ipRate_usage THEN
  ipMachine = ''.
  FIND bRate NO-LOCK
       WHERE bRate.company EQ ipCompany
         AND bRate.employee EQ ipEmployee
         AND bRate.shift EQ ipShift
         AND bRate.machine EQ ipMachine
         AND bRate.ratetype EQ 'Standard'
       NO-ERROR.
  IF NOT AVAILABLE bRate THEN
  RETURN.
  opRate = bRate.rate.
  FIND bRate NO-LOCK
       WHERE bRate.company = ipCompany
         AND bRate.employee = ipEmployee
         AND bRate.shift = ipShift
         AND bRate.machine = ipMachine
         AND bRate.ratetype = ipRatetype
       NO-ERROR.
  IF AVAILABLE bRate THEN
  CASE bRate.factortype:
    WHEN 'Straight' THEN
    opRate = bRate.rate.
    WHEN 'Additional' THEN
    opRate = opRate + bRate.rate.
    WHEN 'Multiply' THEN
    opRate = opRate * bRate.rate.
    OTHERWISE
    opRate = 0.
  END CASE.

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
  DISPLAY setAllResources selectedShift selectedStartDate 
          selectedStartDateOption selectedEndDate selectedEndDateOption 
          selectedStartDueDate selectedStartDueDateOption selectedEndDueDate 
          selectedEndDueDateOption svAllJobNo svStartJobNo svStartJobNo2 
          svEndJobNo svEndJobNo2 lvProdAceDir lvImportDir lvProdAceType 
          lvEmpLogin lvProdAceBlankEmployee lvResourceList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnExportShifts btnReset btnSave btnImport setAllResources 
         selectedShift btnExportEmployees selectedStartDate btnCalendar-1 
         selectedStartDateOption selectedEndDate btnCalendar-2 btnExportJobs 
         selectedEndDateOption selectedStartDueDate btnCalendar-3 
         selectedStartDueDateOption selectedEndDueDate btnCalendar-4 
         selectedEndDueDateOption svAllJobNo btnExportMachines svStartJobNo 
         svStartJobNo2 svEndJobNo svEndJobNo2 lvProdAceDir lvImportDir 
         lvProdAceType lvProdAceBlankEmployee lvResourceList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getProdAceDatValues C-Win 
PROCEDURE getProdAceDatValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  INPUT FROM VALUE(lvProdAceDat) NO-ECHO.
  IMPORT UNFORMATTED lvProdAceDir.           /* location of prodAce trans file   */
  IMPORT UNFORMATTED lvProdAceType.          /* Summary or Detail                */
  IMPORT UNFORMATTED lvEmpLogin.             /* ProdAce or TS                    */
  IMPORT UNFORMATTED lvProdAceBlankEmployee. /* default employee if blank        */
  IMPORT UNFORMATTED lvImportDir.            /* location of processed trans file */
  IMPORT UNFORMATTED lvResourceList.         /* comma delimited list, or blank   */
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newEnd C-Win 
PROCEDURE newEnd :
/*------------------------------------------------------------------------------
  Purpose:     calculate new ending date & time
  Parameters:  inputs timespan, start date & time, output new end date & time
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipTimeSpan AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER newStartDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER newStartTime AS INTEGER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER newEndDate AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER newEndTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE days AS INTEGER NO-UNDO.
  
  ASSIGN
    newEndTime = newStartTime + ipTimeSpan
    days = TRUNCATE(newEndTime / 86400,0)
    newEndDate = newStartDate + days
    newEndTime = newEndTime - days * 86400
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExport C-Win 
PROCEDURE pExport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE asiCompany     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dStandardCycle AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cProductID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdIDFile    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile1         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE firstJobsList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    
    IF ipcType EQ 'Jobs' THEN DO WITH FRAME {&FRAME-NAME}:
        EMPTY TEMP-TABLE ttblProductID.
        ASSIGN
            cFile = lvProdAceDir + '\mrp2jq.tmp'
            selectedStartDueDate
            selectedEndDueDate
            svAllJobNo
            svStartJobNo
            svStartJobNo2
            svEndJobNo
            svEndJobNo2
            .
        IF svAllJobNo THEN
        ASSIGN
            svStartJobNo = CHR(32)
            svEndJobNo   = CHR(254)
            .
        ELSE
        ASSIGN
            svStartJobNo = FILL(" ",6 - LENGTH(svStartJobNo))
                         + svStartJobNo + "-"
                         + STRING(svStartJobNo2)
            svEndJobNo   = FILL(" ",6 - LENGTH(svEndJobNo))
                         + svEndJobNo + "-"
                         + STRING(svEndJobNo2)
                         .
        OUTPUT TO VALUE(cFile).
        FOR EACH ttblResource
            WHERE ttblResource.dmiID GT 0,
            EACH ttblJob
            WHERE ttblJob.resource     EQ ttblResource.resource
              AND ttblJob.jobSort      GE svStartJobNo
              AND ttblJob.jobSort      LE svEndJobNo
              AND ttblJob.dueDate      GE selectedStartDueDate
              AND ttblJob.dueDate      LE selectedEndDueDate
              AND ttblJob.jobCompleted EQ NO
              AND ttblJob.liveUpdate   EQ YES
            BREAK BY ttblResource.dmiID
                  BY ttblJob.jobSequence
            :
            IF FIRST-OF(ttblResource.dmiID) THEN DO:
                idx = 0.
                FIND FIRST ttToggleBox
                     WHERE ttToggleBox.rResource EQ ROWID(ttblResource)
                     .
                IF ttToggleBox.hToggleBox:CHECKED THEN
                firstJobsList = firstJobsList
                              + STRING(ttblResource.dmiID,'999') + '|'
                              + ttblJob.job + ' '
                              + STRING(INT(ttblJob.userField19)) + ' '
                              + STRING(INT(ttblJob.userField20))
                              + ','
                              .
            END. /* if first-of */
            ASSIGN
                cProductID = REPLACE(ttblJob.userField08,'<Multi Item>',
                                     ttblJob.job + '.' + ttblResource.resource)
                dStandardCycle = 60 / (INT(ttblJob.userField88) / 60)
                .
            IF dStandardCycle EQ ? THEN 
            dStandardCycle = 1.
            idx = idx + 1.
            PUT UNFORMATTED 
                idx ','
                STRING(ttblResource.dmiID,'999') ',"'
                ttblJob.job ' '
                INT(ttblJob.userField19) ' '
                INT(ttblJob.userField20) '","'
                cProductID '",'
                REPLACE(ttblJob.userField15,',','') ',"",'
                YEAR(ttblJob.dueDate) STRING(MONTH(ttblJob.dueDate),'99') STRING(DAY(ttblJob.dueDate),'99') ',""'
                SKIP 
                .
            IF NOT CAN-FIND(FIRST ttblProductID
                            WHERE ttblProductID.productID EQ cProductID
                              AND ttblProductID.dmiID EQ ttblResource.dmiID) THEN DO:
                CREATE ttblProductID.
                ASSIGN 
                    ttblProductID.productID = cProductID
                    ttblProductID.dmiID = ttblResource.dmiID
                    ttblProductID.productDesc = ttblJob.userField09
                    ttblProductID.standardCycle = dStandardCycle
                    .
            END. /* if not can-find */
        END. /* each ttblresource */
        OUTPUT CLOSE.
        firstJobsList = TRIM(firstJobsList,',').
        DO idx = 1 TO NUM-ENTRIES(firstJobsList):
            cFile1 = lvProdAceDir + '\JC' + ENTRY(1,ENTRY(idx,firstJobsList),'|') + '.dat'.
            OUTPUT TO VALUE(cFile1).
            EXPORT ENTRY(2,ENTRY(idx,firstJobsList),'|').
            OUTPUT CLOSE.
        END. /* do idx */
        OS-COPY VALUE(SEARCH(cFile)) VALUE(REPLACE(SEARCH(cFile),'.tmp','.dat')).
        OS-DELETE VALUE(SEARCH(cFile)).
        cFile = lvProdAceDir + '\wmsprods.tmp'.
        OUTPUT TO VALUE(cFile).
        FOR EACH ttblProductID:
            PUT UNFORMATTED '"'
                ttblProductID.productID '",'
                STRING(ttblProductID.dmiID,'999') ',"","","'
                REPLACE(ttblProductID.productDesc,',','') '",'
                TRIM(STRING(ttblProductID.standardCycle,'>>>>9.9<<<')) ','
                0 ',' 1 ',' 0 ',' 0 ',' 0
                SKIP
                .
        END. /* each ttblproductid */
        OUTPUT CLOSE.
        OS-COPY VALUE(SEARCH(cFile)) VALUE(REPLACE(SEARCH(cFile),'.tmp','.dat')).
        OS-DELETE VALUE(SEARCH(cFile)).
    END. /* jobs */
    ELSE DO:
        RUN asiCommaList IN iphContainerHandle ('Company',OUTPUT asiCompany).
        RUN VALUE(findProgram('{&loads}/',ID,'/prodAceExport.p'))
            (asiCompany,ID,lvProdAceDir,ipcType).
    END. /* else */    
    MESSAGE
        'Export of' ipcType 'Complete'
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pImport C-Win 
PROCEDURE pImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    MESSAGE
        "Import Complete"
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postProdAce C-Win 
PROCEDURE postProdAce :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  lvProdAceFile = lvImportDir + '/adware.dat'.
  IF SEARCH(lvProdAceFile) EQ ? THEN DO:
      MESSAGE 
        'Production ACE File' lvProdAceFile 'does not exist'
      VIEW-AS ALERT-BOX.
      RETURN.
  END. /* if search */
  
  ASSIGN
    lvFile = lvProdAceFile
    lvTemp = REPLACE(lvFile,'.dat','.tmp')
    lvHoldFile = REPLACE(lvFile,'.dat','.hold')
    lvProcessed = lvImportDir
                + '/processed/adware.'
                + STRING(YEAR(TODAY),'9999')
                + STRING(MONTH(TODAY),'99')
                + STRING(DAY(TODAY),'99')
                + '.' + STRING(TIME,'99999')
                + '.dat'
    lvArchive = REPLACE(lvProcessed,'processed','archive')
    lvErrorFile = REPLACE(lvProcessed,'processed','errors')
    .
  
  OUTPUT STREAM sError TO VALUE(lvErrorFile).
  PUT STREAM sError UNFORMATTED
    'Parameters - Shift: ' selectedShift
    ' - Start Date: ' selectedStartDate
    ' - End Date: ' selectedEndDate
    SKIP
    .
  /* move transactions to tmp file */
  IF lvPostProdAce THEN DO:
    OS-COPY VALUE(lvFile) VALUE(lvArchive).
    OS-RENAME VALUE(lvFile) VALUE(lvTemp).
  END.
  ELSE OS-APPEND VALUE(lvFile) VALUE(lvTemp).

  /* append hold records */
  OS-APPEND VALUE(lvHoldFile) VALUE(lvTemp).
  
  /* create temp-table prodAce records */
  RUN createTtblProdAce (lvTemp).
  
  /* post to create machtran, or simply to reflect onto SB */
  IF lvPostProdAce THEN
  /* run Detail or Summary */
  RUN VALUE('prodAce' + lvProdAceType).
  /* not posting, simply reflect trans on SB jobs */
  ELSE RUN setSBJobs.

  /* remove tmp file */
  OS-DELETE VALUE(lvTemp).

  OUTPUT STREAM sError CLOSE.

  RELEASE machtran.

  IF iplAutoMonitor EQ NO THEN 
  OS-COMMAND NO-WAIT notepad.exe VALUE(lvErrorFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPopulateOptions C-Win 
PROCEDURE pPopulateOptions :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        hContainer = iphContainer.

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,selectedStartDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,selectedEndDateOption:HANDLE).

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,selectedStartDueDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,selectedEndDueDateOption:HANDLE).
        
        APPLY "VALUE-CHANGED":U TO svAllJobNo.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prodAceDetail C-Win 
PROCEDURE prodAceDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* split records that span midnight */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceSelected EQ YES
      :
    IF ttblProdAce.prodAceStartDate NE ttblProdAce.prodAceEndDate THEN DO:
      CREATE buffProdAce.
      BUFFER-COPY ttblProdAce TO buffProdAce.
      ASSIGN
        buffProdAce.prodAceStartDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceStartTime = 0
        buffProdAce.prodAceDuration = buffProdAce.prodAceEndTime - buffProdAce.prodAceStartTime
        ttblProdAce.prodAceEndDate = ttblProdAce.prodAceStartDate
        ttblProdAce.prodAceEndTime = 86340
        ttblProdAce.prodAceDuration = ttblProdAce.prodAceEndTime - ttblProdAce.prodAceStartTime
        ttblProdAce.prodAceTranRunQty = 0
        ttblProdAce.prodAceTranRejectQty = 0
        .
    END. /* spans midnight */
  END. /* each ttblprodace */
  
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceSelected EQ YES
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceSeq
      :
    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */
    CREATE machtran.
    ASSIGN
      machtran.company = job-mch.company
      machtran.machine = ttblProdAce.prodAceResource
      machtran.job_number = job-mch.job-no
      machtran.job_sub = job-mch.job-no2
      machtran.form_number = job-mch.frm
      machtran.blank_number = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code = ttblProdAce.prodAceState
      machtran.completed = ttblProdAce.prodAceRunComplete
      machtran.start_date = ttblProdAce.prodAceStartDate
      machtran.start_time = ttblProdAce.prodAceStartTime
      machtran.end_date = ttblProdAce.prodAceEndDate
      machtran.end_time = ttblProdAce.prodAceEndTime
      machtran.run_qty = ttblProdAce.prodAceTranRunQty
      machtran.waste_qty = ttblProdAce.prodAceTranRejectQty
      machtran.shift = ttblProdAce.prodAceShift
      machtran.total_time = machtran.end_time - machtran.start_time
      .
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.

    RUN setRecKey (BUFFER machtran).
    IF lvEmpLogin EQ 'ProdAce' THEN
    RUN createMachEmp (BUFFER machtran).
    IF ttblProdAce.prodAceRunComplete THEN
    RUN completeMR.
  END. /* each ttblProdAce */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prodAceSummary C-Win 
PROCEDURE prodAceSummary :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE empLoginRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE lvState AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvTime AS INTEGER NO-UNDO.
  
  /* Pass 1: consolidate prodAce transactions */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF FIRST-OF(ttblProdAce.prodAceShift) OR
       FIRST-OF(ttblProdAce.prodAceStartDate) OR
       lvState NE ttblProdAce.prodAceState THEN DO:
      FIND buffProdAce WHERE ROWID(buffProdAce) EQ ROWID(ttblProdAce).
      lvState = ttblProdAce.prodAceState.
    END. /* first-of */
    IF AVAILABLE buffProdAce AND (ROWID(buffProdAce) NE ROWID(ttblProdAce)) THEN DO:
      ASSIGN
        buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
        buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
        buffProdAce.prodAceDuration = buffProdAce.prodAceDuration + ttblProdAce.prodAceDuration
        buffProdAce.prodAceEndDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceEndTime = ttblProdAce.prodAceEndTime
        ttblProdAce.deleteFlag = YES
        .
    END. /* avail buffprodAce */
  END. /* each ttblProdAce */

  /* Pass 2: move non-RUN qty values to RUN transaction */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState NE 'RUN'
       AND  ttblProdAce.deleteFlag EQ NO
       AND (ttblProdAce.prodAceTranRunQty NE 0
        OR  ttblProdAce.prodAceTranRejectQty NE 0)
      :
    FIND FIRST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState EQ 'RUN'
           AND ttblProdAce.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    ASSIGN
      buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
      buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
      ttblProdAce.prodAceTranRunQty = 0
      ttblProdAce.prodAceTranRejectQty = 0
      .
  END. /* each ttblProdAce */

  /* Pass 3: flag records as run completed if necessary */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState EQ 'RUN'
        AND ttblProdAce.prodAceRunComplete EQ YES
        AND ttblProdAce.deleteFlag EQ YES
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    FIND LAST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState EQ 'RUN'
           AND ttblProdAce.prodAceRunComplete EQ NO
           AND buffProdAce.deleteFlag EQ NO
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    buffProdAce.prodAceRunComplete = YES.
  END. /* each ttblprodAce */

  /* Pass 4: remove deleted flag records */
  FOR EACH ttblProdAce:
    IF ttblProdAce.deleteFlag EQ YES OR
      (ttblProdAce.prodAceState EQ 'NC' AND
       ttblProdAce.prodAceDuration LE 60 AND
       ttblProdAce.prodAceTranRunQty EQ 0 AND
       ttblProdAce.prodAceTranRejectQty EQ 0 AND
       ttblProdAce.prodAceRunComplete EQ NO
       ) THEN
    DELETE ttblProdAce.
  END. /* each ttblprodAce */

  /* Pass 5: split records that span midnight */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF ttblProdAce.deleteFlag THEN NEXT.
    IF ttblProdAce.prodAceStartDate NE ttblProdAce.prodAceEndDate THEN DO:
      CREATE buffProdAce.
      BUFFER-COPY ttblProdAce TO buffProdAce.
      ASSIGN
        buffProdAce.prodAceStartDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceStartTime = 0
        buffProdAce.prodAceDuration = buffProdAce.prodAceEndTime - buffProdAce.prodAceStartTime
        ttblProdAce.prodAceEndDate = ttblProdAce.prodAceStartDate
        ttblProdAce.prodAceEndTime = 86340
        ttblProdAce.prodAceDuration = ttblProdAce.prodAceEndTime - ttblProdAce.prodAceStartTime
        ttblProdAce.prodAceTranRunQty = 0
        ttblProdAce.prodAceTranRejectQty = 0
        .
    END. /* spans midnight */
  END. /* each ttblProdAce */

  /* Pass 6: consolidate prodAce transactions again */
  RELEASE buffProdAce.
  lvState = ''.
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF FIRST-OF(ttblProdAce.prodAceShift) OR
       FIRST-OF(ttblProdAce.prodAceStartDate) OR
       lvState NE ttblProdAce.prodAceState THEN DO:
      FIND buffProdAce WHERE ROWID(buffProdAce) EQ ROWID(ttblProdAce).
      lvState = ttblProdAce.prodAceState.
    END. /* first-of */
    IF AVAILABLE buffProdAce AND (ROWID(buffProdAce) NE ROWID(ttblProdAce)) THEN DO:
      ASSIGN
        buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
        buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
        buffProdAce.prodAceDuration = buffProdAce.prodAceDuration + ttblProdAce.prodAceDuration
        buffProdAce.prodAceEndDate = ttblProdAce.prodAceEndDate
        buffProdAce.prodAceEndTime = ttblProdAce.prodAceEndTime
        buffProdAce.prodAceRunComplete = ttblProdAce.prodAceRunComplete
        ttblProdAce.deleteFlag = YES
        .
    END. /* avail buffprodAce */
  END. /* each ttblProdAce */

  /* Pass 7: move RUN qty values to last RUN transaction */
  FOR EACH ttblProdAce
      WHERE ttblProdAce.prodAceState EQ 'RUN'
       AND  ttblProdAce.deleteFlag EQ NO
       AND (ttblProdAce.prodAceTranRunQty NE 0
        OR  ttblProdAce.prodAceTranRejectQty NE 0)
      :
    FIND LAST buffProdAce
         WHERE buffProdAce.prodAceResource EQ ttblProdAce.prodAceResource
           AND buffProdAce.prodAceJob EQ ttblProdAce.prodAceJob
           AND buffProdAce.prodAceItem EQ ttblProdAce.prodAceItem
           AND buffProdAce.prodAceShift EQ ttblProdAce.prodAceShift
           AND buffProdAce.prodAceState EQ 'RUN'
           AND buffProdAce.deleteFlag EQ NO
           AND buffProdAce.prodAceSeq GT ttblProdAce.prodAceSeq
           AND ROWID(buffProdAce) NE ROWID(ttblProdAce)
         NO-ERROR.
    IF AVAILABLE buffProdAce THEN
    ASSIGN
      buffProdAce.prodAceTranRunQty = buffProdAce.prodAceTranRunQty + ttblProdAce.prodAceTranRunQty
      buffProdAce.prodAceTranRejectQty = buffProdAce.prodAceTranRejectQty + ttblProdAce.prodAceTranRejectQty
      ttblProdAce.prodAceTranRunQty = 0
      ttblProdAce.prodAceTranRejectQty = 0
      .
  END. /* each ttblProdAce */

  /* Pass 8: create machtran records */
  FOR EACH ttblProdAce
      BREAK BY ttblProdAce.prodAceResource
            BY ttblProdAce.prodAceJob
            BY ttblProdAce.prodAceItem
            BY ttblProdAce.prodAceShift
            BY ttblProdAce.prodAceStartDate
            BY ttblProdAce.prodAceSeq
            BY ttblProdAce.prodAceState
      :
    IF ttblProdAce.deleteFlag THEN NEXT.

    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:
      lvTime = ?.
      FIND FIRST ttblJob
           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource
             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
      IF AVAILABLE ttblJob THEN
      jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
      ELSE DO:
        FIND FIRST pendingJob
             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource
               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.
        IF NOT AVAILABLE pendingJob THEN NEXT.
        jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
      END. /* not avail ttbljob */
      FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
      IF NOT AVAILABLE job-mch THEN NEXT.
    END. /* first-of job */

    IF lvTime EQ ? THEN
    lvTime = ttblProdAce.prodAceStartTime.

    CREATE machtran.
    ASSIGN
      machtran.company = job-mch.company
      machtran.machine = ttblProdAce.prodAceResource
      machtran.job_number = job-mch.job-no
      machtran.job_sub = job-mch.job-no2
      machtran.form_number = job-mch.frm
      machtran.blank_number = job-mch.blank-no
      machtran.pass_sequence = job-mch.pass
      machtran.charge_code = ttblProdAce.prodAceState
      machtran.completed = ttblProdAce.prodAceRunComplete
      machtran.start_date = ttblProdAce.prodAceStartDate
      machtran.start_time = ttblProdAce.prodAceStartTime
      machtran.end_date = ttblProdAce.prodAceEndDate
      machtran.end_time = ttblProdAce.prodAceEndTime
      machtran.run_qty = ttblProdAce.prodAceTranRunQty
      machtran.waste_qty = ttblProdAce.prodAceTranRejectQty
      machtran.shift = ttblProdAce.prodAceShift
      machtran.total_time = machtran.end_time - machtran.start_time
      .
    IF machtran.total_time LT 0 OR machtran.total_time EQ ? THEN
    machtran.total_time = 0.

    RUN setRecKey (BUFFER machtran).
    IF lvEmpLogin EQ 'ProdAce' THEN
    RUN createEmpLogin (BUFFER ttblProdAce,BUFFER machtran).    
    IF ttblProdAce.prodAceRunComplete THEN
    RUN completeMR.
  END. /* each ttblProdAce */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTransSelection C-Win 
PROCEDURE pTransSelection :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveProdAceDatValues C-Win 
PROCEDURE saveProdAceDatValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  OUTPUT TO VALUE(lvProdAceDat).
  PUT UNFORMATTED lvProdAceDir SKIP.           /* location of prodAce trans file   */
  PUT UNFORMATTED lvProdAceType SKIP.          /* Summary or Detail                */
  PUT UNFORMATTED lvEmpLogin SKIP.             /* ProdAce or TS                    */
  PUT UNFORMATTED lvProdAceBlankEmployee SKIP. /* default employee if blank        */
  PUT UNFORMATTED lvImportDir SKIP.            /* location of import prod ace file */
  PUT UNFORMATTED lvResourceList SKIP.         /* comma delimited list, or blank   */
  OUTPUT CLOSE.

  /* create directories if they don't exist */
  OS-CREATE-DIR VALUE(lvImportDir).
  OS-CREATE-DIR VALUE(lvImportDir + '/processed').
  OS-CREATE-DIR VALUE(lvImportDir + '/archive').
  OS-CREATE-DIR VALUE(lvImportDir + '/errors').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRecKey C-Win 
PROCEDURE setRecKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER machtran FOR machtran.

  {custom/rec_key.i "machtran"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSBJobs C-Win 
PROCEDURE setSBJobs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  FOR EACH ttblProdAce                                                    */
/*      BREAK BY ttblProdAce.prodAceResource                                */
/*            BY ttblProdAce.prodAceJob                                     */
/*            BY ttblProdAce.prodAceItem                                    */
/*            BY ttblProdAce.prodAceSeq                                     */
/*      :                                                                   */
/*    IF FIRST-OF(ttblProdAce.prodAceJob) THEN DO:                          */
/*      FIND FIRST ttblJob                                                  */
/*           WHERE ttblJob.resource EQ ttblProdAce.prodAceResource          */
/*             AND ttblJob.job EQ ttblProdAce.prodAceJob NO-ERROR.          */
/*      IF AVAILABLE ttblJob THEN DO:                                       */
/*        IF ttblJob.jobLocked THEN NEXT.                                   */
/*      END. /* avail ttbljob */                                            */
/*      ELSE DO:                                                            */
/*        FIND FIRST pendingJob                                             */
/*             WHERE pendingJob.resource EQ ttblProdAce.prodAceResource     */
/*               AND pendingJob.job EQ ttblProdAce.prodAceJob NO-ERROR.     */
/*        IF NOT AVAILABLE pendingJob THEN NEXT.                            */
/*        CREATE ttblJob.                                                   */
/*        BUFFER-COPY pendingJob TO ttblJob.                                */
/*        DELETE pendingJob.                                                */
/*      END. /* not avail ttbljob */                                        */
/*      ASSIGN                                                              */
/*        ttblJob.startDate = ttblProdAce.prodAceStartDate                  */
/*        ttblJob.startTime = ttblProdAce.prodAceStartTime                  */
/*        .                                                                 */
/*      RUN calcEnd (ttblJob.startDate,ttblJob.startTime,0,ttblJob.timeSpan,*/
/*                   OUTPUT ttblJob.endDate,OUTPUT ttblJob.endTime).        */
/*    END. /* first-of job */                                               */
/*  END. /* each ttblprodAce */                                             */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

