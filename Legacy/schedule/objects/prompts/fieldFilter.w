&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fieldFilter.w

  Description: User Field Filter Popup Window

  Input Parameters: Calling program handle and usage type

  Output Parameters: <none>

  Author: Ron Stark

  Created: 6.27.2004

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
{{&print}/includes/printDefs.i} /* contains input param ipBoard & ipExcel */
{{&viewers}/includes/asiDept.i}

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipHandle AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER ipUsage AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE udfFieldLo AS WIDGET-HANDLE NO-UNDO EXTENT {&udfExtent}.
DEFINE VARIABLE udfFieldHi AS WIDGET-HANDLE NO-UNDO EXTENT {&udfExtent}.
DEFINE VARIABLE userFieldLo AS WIDGET-HANDLE NO-UNDO EXTENT {&userExtent}.
DEFINE VARIABLE userFieldHi AS WIDGET-HANDLE NO-UNDO EXTENT {&userExtent}.
DEFINE VARIABLE winTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE xCoord AS INTEGER NO-UNDO.
DEFINE VARIABLE hiCoord AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME filterFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fromDateObject btnFromDate toDateObject ~
btnToDate fromDueDateObject btnFromDueDate toDueDateObject btnToDueDate ~
jobLo jobHi btnBlank btnReset btnApply btnClose 
&Scoped-Define DISPLAYED-OBJECTS fromDateObject toDateObject ~
fromDueDateObject toDueDateObject jobLo jobHi resourceLo resourceHi 

/* Custom List Definitions                                              */
/* dateFields,jobFields,List-3,List-4,List-5,List-6                     */
&Scoped-define dateFields fromDateObject toDateObject fromDueDateObject ~
toDueDateObject 
&Scoped-define jobFields jobLo jobHi 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD comma C-Win 
FUNCTION comma RETURNS CHARACTER
  (ipValue AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnApply 
     LABEL "&Apply" 
     SIZE 13 BY 1
     FONT 6.

DEFINE BUTTON btnBlank 
     LABEL "&Blank" 
     SIZE 13 BY 1
     FONT 6.

DEFINE BUTTON btnClose 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "&Close" 
     SIZE 13 BY 1.43
     FONT 6.

DEFINE BUTTON btnFromDate 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY .81 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnFromDueDate 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY .81 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnReset 
     LABEL "&Reset" 
     SIZE 13 BY 1
     FONT 6.

DEFINE BUTTON btnToDate 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY .81 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnToDueDate 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY .81 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE fontValue AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "F&ont" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0",0
     DROP-DOWN-LIST
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE sortBy AS CHARACTER FORMAT "X(256)":U INITIAL "Job Sequence" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Job Sequence","Due Date","Job","Resource" 
     DROP-DOWN-LIST
     SIZE 29 BY 1.05 NO-UNDO.

DEFINE VARIABLE deptTitle AS CHARACTER FORMAT "X(256)":U INITIAL "Departments:" 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE fromDateObject AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE fromDueDateObject AS DATE FORMAT "99/99/9999":U 
     LABEL "Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE jobHi AS CHARACTER FORMAT "X(256)":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE jobLo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Job" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE linesPerPage AS INTEGER FORMAT ">>9":U INITIAL 66 
     LABEL "&Lines/Page" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE printTitle AS CHARACTER FORMAT "X(256)":U INITIAL "Print Program Selections:" 
      VIEW-AS TEXT 
     SIZE 50 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE resourceHi AS CHARACTER FORMAT "X(256)":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE resourceLo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resource" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE sortByLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By:" 
      VIEW-AS TEXT 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE toDateObject AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE toDueDateObject AS DATE FORMAT "99/99/9999":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE orientationValue AS CHARACTER INITIAL "Landscape" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrai&t", "Portrait",
"Lan&dscape", "Landscape"
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE outputTo AS CHARACTER INITIAL "Screen" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "&Print", "Print",
"e&Mail", "eMail",
"&Fax", "Fax",
"&Excel", "Excel",
"&Screen", "Screen"
     SIZE 13 BY 3.81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE printProgram AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Empty", "Empty"
     SIZE 50 BY .95
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE buttonRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 15 BY 5.48
     BGCOLOR 7 .

DEFINE RECTANGLE outputRect
     EDGE-PIXELS 1 GRAPHIC-EDGE  
     SIZE 15 BY 4.29
     BGCOLOR 15 .

DEFINE VARIABLE departments AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 17 BY 12.62 NO-UNDO.

DEFINE VARIABLE showParametersPage AS LOGICAL INITIAL no 
     LABEL "Show Parameters Page?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME filterFrame
     fromDateObject AT ROW 1.24 COL 17 COLON-ALIGNED HELP
          "Enter From Date"
     btnFromDate AT ROW 1.24 COL 36
     toDateObject AT ROW 1.24 COL 45 COLON-ALIGNED HELP
          "Enter To Date"
     btnToDate AT ROW 1.24 COL 65
     fontValue AT ROW 1.24 COL 85 COLON-ALIGNED
     departments AT ROW 1.71 COL 138 NO-LABEL
     fromDueDateObject AT ROW 2.19 COL 17 COLON-ALIGNED HELP
          "Enter From Due Date"
     btnFromDueDate AT ROW 2.19 COL 36
     toDueDateObject AT ROW 2.19 COL 45 COLON-ALIGNED HELP
          "Enter To Due Date"
     btnToDueDate AT ROW 2.19 COL 65
     linesPerPage AT ROW 2.43 COL 85 COLON-ALIGNED
     showParametersPage AT ROW 2.43 COL 95 HELP
          "Show Parameters Page"
     jobLo AT ROW 3.14 COL 17 COLON-ALIGNED
     jobHi AT ROW 3.14 COL 45 COLON-ALIGNED
     orientationValue AT ROW 3.62 COL 87 NO-LABEL
     outputTo AT ROW 3.86 COL 71 NO-LABEL
     resourceLo AT ROW 4.1 COL 17 COLON-ALIGNED
     resourceHi AT ROW 4.1 COL 45 COLON-ALIGNED
     printProgram AT ROW 5.76 COL 87 NO-LABEL
     sortBy AT ROW 6.95 COL 106 COLON-ALIGNED NO-LABEL
     btnBlank AT ROW 8.38 COL 71
     btnReset AT ROW 9.57 COL 71
     btnApply AT ROW 10.76 COL 71
     btnClose AT ROW 11.95 COL 71
     deptTitle AT ROW 1 COL 136 COLON-ALIGNED NO-LABEL
     printTitle AT ROW 4.57 COL 85 COLON-ALIGNED NO-LABEL
     sortByLabel AT ROW 6.95 COL 94 COLON-ALIGNED NO-LABEL
     outputRect AT ROW 3.62 COL 70
     buttonRect AT ROW 8.14 COL 70
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154 BY 13.33
         FONT 2.


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
         TITLE              = "Field Filter - Scheduler"
         HEIGHT             = 13.33
         WIDTH              = 154
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME filterFrame
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE buttonRect IN FRAME filterFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST departments IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       departments:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN deptTitle IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       deptTitle:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR COMBO-BOX fontValue IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fontValue:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN fromDateObject IN FRAME filterFrame
   1                                                                    */
/* SETTINGS FOR FILL-IN fromDueDateObject IN FRAME filterFrame
   1                                                                    */
/* SETTINGS FOR FILL-IN jobHi IN FRAME filterFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN jobLo IN FRAME filterFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN linesPerPage IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       linesPerPage:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR RADIO-SET orientationValue IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       orientationValue:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR RECTANGLE outputRect IN FRAME filterFrame
   NO-ENABLE                                                            */
ASSIGN 
       outputRect:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR RADIO-SET outputTo IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       outputTo:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR RADIO-SET printProgram IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       printProgram:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN printTitle IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       printTitle:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN resourceHi IN FRAME filterFrame
   NO-ENABLE                                                            */
ASSIGN 
       resourceHi:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN resourceLo IN FRAME filterFrame
   NO-ENABLE                                                            */
ASSIGN 
       resourceLo:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX showParametersPage IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       showParametersPage:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR COMBO-BOX sortBy IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sortBy:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN sortByLabel IN FRAME filterFrame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sortByLabel:HIDDEN IN FRAME filterFrame           = TRUE.

/* SETTINGS FOR FILL-IN toDateObject IN FRAME filterFrame
   1                                                                    */
/* SETTINGS FOR FILL-IN toDueDateObject IN FRAME filterFrame
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME filterFrame
/* Query rebuild information for FRAME filterFrame
     _Query            is NOT OPENED
*/  /* FRAME filterFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Field Filter - Scheduler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Field Filter - Scheduler */
DO:
  APPLY 'CHOOSE' TO btnBlank IN FRAME {&FRAME-NAME}.
  RUN applyFields (YES).
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply C-Win
ON CHOOSE OF btnApply IN FRAME filterFrame /* Apply */
DO:
  RUN applyFields (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME filterFrame /* Blank */
DO:
  RUN blankFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME filterFrame /* Close */
DO:
  APPLY 'CHOOSE' TO btnBlank.
  RUN applyFields (YES).
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFromDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFromDate C-Win
ON CHOOSE OF btnFromDate IN FRAME filterFrame
DO:
  APPLY 'HELP' TO fromDateObject.
  APPLY 'ENTRY' TO fromDateObject.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFromDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFromDueDate C-Win
ON CHOOSE OF btnFromDueDate IN FRAME filterFrame
DO:
  APPLY 'HELP' TO fromDueDateObject.
  APPLY 'ENTRY' TO fromDueDateObject.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME filterFrame /* Reset */
DO:
  RUN resetFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToDate C-Win
ON CHOOSE OF btnToDate IN FRAME filterFrame
DO:
  APPLY 'HELP' TO toDateObject.
  APPLY 'ENTRY' TO toDateObject.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnToDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnToDueDate C-Win
ON CHOOSE OF btnToDueDate IN FRAME filterFrame
DO:
  APPLY 'HELP' TO toDueDateObject.
  APPLY 'ENTRY' TO toDueDateObject.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME departments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL departments C-Win
ON RIGHT-MOUSE-CLICK OF departments IN FRAME filterFrame
DO:
  RUN VALUE(findProgram('{&viewers}/',ID,'/asiDept.r')).
  RUN asiDeptBuild.
  APPLY 'VALUE-CHANGED':U TO printProgram IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fontValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fontValue C-Win
ON VALUE-CHANGED OF fontValue IN FRAME filterFrame /* Font */
DO:
  ASSIGN {&SELF-NAME}
    SELF:FONT = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fromDateObject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fromDateObject C-Win
ON HELP OF fromDateObject IN FRAME filterFrame /* From Date */
DO:
  {{&includes}/calendar.i}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fromDueDateObject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fromDueDateObject C-Win
ON HELP OF fromDueDateObject IN FRAME filterFrame /* Due Date */
DO:
  {{&includes}/calendar.i}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME outputTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL outputTo C-Win
ON VALUE-CHANGED OF outputTo IN FRAME filterFrame
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} EQ 'Print' THEN
  ENABLE fontValue orientationValue WITH FRAME {&FRAME-NAME}.
  ELSE
  HIDE fontValue orientationValue NO-PAUSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME printProgram
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL printProgram C-Win
ON VALUE-CHANGED OF printProgram IN FRAME filterFrame
DO:
  ASSIGN {&SELF-NAME}
    departments:SCREEN-VALUE = ''.
  IF ID BEGINS 'ASI' THEN DO:
    ASSIGN
      sortByLabel:HIDDEN = NOT CAN-DO('jobByResource,pendingJobs,Status',ENTRY(1,{&SELF-NAME},':'))
      sortBy:HIDDEN = sortByLabel:HIDDEN
      sortBy:SENSITIVE = NOT sortByLabel:HIDDEN.
    IF NOT sortByLabel:HIDDEN THEN
    CASE ENTRY(1,{&SELF-NAME},':'):
      WHEN 'jobByResource' THEN
      sortBy:SCREEN-VALUE = 'Job Sequence'.
      WHEN 'pendingJobs' THEN
      sortBy:SCREEN-VALUE = 'Job'.
      WHEN 'Status' THEN
      sortBy:SCREEN-VALUE = 'Due Date'.
    END CASE.
    FIND FIRST asiDept NO-LOCK WHERE asiDept.rptName EQ ENTRY(1,{&SELF-NAME},':') NO-ERROR.
    IF NOT AVAILABLE asiDept THEN
    DISABLE departments WITH FRAME {&FRAME-NAME}.
    ELSE DO:
      ENABLE departments WITH FRAME {&FRAME-NAME}.
      DO idx = 1 TO NUM-ENTRIES(asiDept.asiDept):
        IF CAN-DO(departments:LIST-ITEMS,ENTRY(idx,asiDept.asiDept)) THEN
        departments:SCREEN-VALUE = ENTRY(idx,asiDept.asiDept).
      END. /* do idx */
    END. /* else */
  END. /* if id */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sortBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sortBy C-Win
ON VALUE-CHANGED OF sortBy IN FRAME filterFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME toDateObject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL toDateObject C-Win
ON HELP OF toDateObject IN FRAME filterFrame /* To */
DO:
  {{&includes}/calendar.i}
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME toDueDateObject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL toDueDateObject C-Win
ON HELP OF toDueDateObject IN FRAME filterFrame /* To */
DO:
  {{&includes}/calendar.i}
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

printPrgm = ''.
FOR EACH rptFormat NO-LOCK WHERE rptFormat.exclude EQ NO,
    FIRST rptNames NO-LOCK WHERE rptNames.rptName EQ rptFormat.rptName:
  IF CAN-DO('jobText,jobToolTip',rptFormat.rptName) THEN NEXT.
  printPrgm = printPrgm + comma(printPrgm) + 
             (IF rptFormat.rptAltName NE '' THEN rptFormat.rptAltName ELSE
              rptNames.rptTitle + ' ' + rptFormat.rptFormat) +
              ',' + rptNames.rptName + ':' + rptFormat.rptFormat.
END. /* each rptformat */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {&WINDOW-NAME}:WINDOW-STATE = 3.
  IF ipUsage NE 'fieldFilter':U THEN DO:
    ASSIGN
      btnApply:LABEL = 'R~&un'
      {&WINDOW-NAME}:TITLE = 'Reports - Scheduler'
      resourceLo:HIDDEN = NO
      resourceLo:SENSITIVE = YES
      resourceLo = resourceValueLo
      resourceHi:HIDDEN = NO
      resourceHi:SENSITIVE = YES
      resourceHi = resourceValueHi
      printTitle:HIDDEN = NO
      printTitle:SCREEN-VALUE = printTitle
      printProgram:HIDDEN = NO
      printProgram:SENSITIVE = YES
      printProgram:RADIO-BUTTONS = printPrgm
      orientationValue:SCREEN-VALUE = 'Landscape'.
    IF ID BEGINS 'ASI' THEN DO:
      RUN asiDeptBuild.
      ASSIGN
        deptTitle:HIDDEN = NO
        deptTitle:SCREEN-VALUE = deptTitle
        departments:HIDDEN = NO
        departments:SENSITIVE = YES
        departments:LIST-ITEMS = departmentList.
    END. /* if id */
    APPLY 'VALUE-CHANGED':U TO printProgram.
  END.
  ASSIGN
    jobLo:HIDDEN = NO
    jobLo:SENSITIVE = YES
    jobLo = jobValueLo
    jobHi:HIDDEN = NO
    jobHi:SENSITIVE = YES
    jobHi = jobValueHi
    fromDateObject = fromDate
    toDateObject = toDate
    fromDueDateObject = fromDueDate
    toDueDateObject = toDueDate.
  RUN createFields (IF ipUsage EQ 'fieldFilter':U THEN 65 ELSE 85).
  RUN enable_UI.
  IF ipUsage NE 'fieldFilter':U THEN DO:
    ASSIGN
      fontValue:SCREEN-VALUE = fontValue:ENTRY(1)
      linesPerPage:SCREEN-VALUE = STRING(linesPerPage).
    IF findProgram('{&data}/',ID,'/fontValue.dat') NE ? THEN DO:
      INPUT FROM VALUE(findProgram('{&data}/',ID,'/fontValue.dat')) NO-ECHO.
      IMPORT fontValue.
      INPUT CLOSE.
      fontValue:SCREEN-VALUE = STRING(fontValue).
    END. /* if findprogram */
    APPLY 'VALUE-CHANGED':U TO fontValue.
    APPLY 'VALUE-CHANGED':U TO printProgram.
  END.
  {{&viewers}/includes/winTitle.i}
  winTitle = {&WINDOW-NAME}:TITLE.
  APPLY 'ENTRY':U TO btnClose.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyFields C-Win 
PROCEDURE applyFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipClosing AS LOGICAL NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE runPrint AS CHARACTER NO-UNDO.
  DEFINE VARIABLE runFormat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE runProgram AS CHARACTER NO-UNDO.
  DEFINE VARIABLE runTitle AS CHARACTER NO-UNDO.

  DO i = 1 TO EXTENT(udfFieldLo):
    IF VALID-HANDLE(udfFieldLo[i]) THEN
    udfValueLo[i] = udfFieldLo[i]:SCREEN-VALUE.
    IF VALID-HANDLE(udfFieldHi[i]) THEN
    udfValueHi[i] = udfFieldHi[i]:SCREEN-VALUE.
    IF udfValueHi[i] EQ '' THEN
    udfValueHi[i] = FILL(CHR(122),20).
  END. /* do i */
  DO i = 1 TO EXTENT(userFieldLo):
    IF VALID-HANDLE(userFieldLo[i]) THEN
    userValueLo[i] = userFieldLo[i]:SCREEN-VALUE.
    IF VALID-HANDLE(userFieldHi[i]) THEN
    userValueHi[i] = userFieldHi[i]:SCREEN-VALUE.
    IF userValueHi[i] EQ '' THEN
    userValueHi[i] = FILL(CHR(122),20).
  END. /* do i */
  DO WITH FRAME {&FRAME-NAME}:
    IF ipUsage NE 'fieldFilter' THEN DO:
      ASSIGN
        resourceValueLo = resourceLo:SCREEN-VALUE
        resourceValueHi = resourceHi:SCREEN-VALUE.
      IF resourceValueHi EQ '' THEN
      resourceValueHi = FILL(CHR(122),20).
    END. /* if ipusage */
    ASSIGN {&dateFields} {&jobFields}
      jobValueLo = jobLo
      jobValueHi = jobHi
      fromDate = fromDateObject
      toDate = toDateObject
      fromDueDate = fromDueDateObject
      toDueDate = toDueDateObject
      filterSortBy = sortBy.
    IF jobValueHi EQ '' THEN
    jobValueHi = FILL(CHR(122),20).
    IF fromDate EQ ? THEN
    fromDate = {{&includes}/firstDate.i}.
    IF toDate EQ ? THEN
    toDate = {{&includes}/lastDate.i}.
    IF fromDueDate EQ ? THEN
    fromDueDate = {{&includes}/firstDate.i}.
    IF toDueDate EQ ? THEN
    toDueDate = {{&includes}/lastDate.i}.
  END. /* do with frame */
  IF ipUsage EQ 'fieldFilter' THEN DO:
    IF VALID-HANDLE(ipHandle) THEN
    RUN reopenBrowse IN ipHandle.
    ELSE
    MESSAGE 'Original Connection to Calling Program Lost!' VIEW-AS ALERT-BOX ERROR.
  END. /* if ipusage */
  ELSE /* called by print program */
  IF NOT ipClosing THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN printProgram linesPerPage outputTo fontValue orientationValue
      showParametersPage sortBy
      linesPerPageValue = linesPerPage
      outputToValue = outputTo
      sortByValue = sortBy
      runFormat = ENTRY(2,printProgram,':')
      runProgram = ENTRY(1,printProgram,':').
    OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/fontValue.dat')).
    EXPORT fontValue.
    OUTPUT CLOSE.
    IF ID BEGINS 'ASI' THEN
    departmentValue = departments:SCREEN-VALUE.
    runPrint = findProgram('{&print}/',ID,'/' + runProgram + '.r').
    IF SEARCH(runPrint) EQ ? THEN
    MESSAGE 'Program "' + runProgram + '.p" does not exist!' VIEW-AS ALERT-BOX ERROR.
    ELSE DO:
      runTitle = ''.
      FIND FIRST rptFormat NO-LOCK
           WHERE rptFormat.rptName EQ runProgram
             AND rptFormat.rptFormat EQ runFormat NO-ERROR.
      IF AVAILABLE rptFormat AND rptFormat.rptAltName NE '' THEN
      runTitle = rptFormat.rptAltName.
      ELSE DO:
        FIND FIRST rptNames NO-LOCK WHERE rptNames.rptName EQ runProgram NO-ERROR.
        IF AVAILABLE rptNames THEN
        runTitle = rptNames.rptTitle + ' ' + runFormat.
      END. /* else */
      RUN VALUE(runPrint) (ipBoard,runFormat,runTitle,outputTo EQ 'Excel',showParametersPage).
      runPrint = findProgram('{&print}/',ID,'/output.p').
      IF outputTo EQ 'Excel' THEN
      OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(printFile)).
      ELSE
      IF ID BEGINS 'ASI' AND (SEARCH(runPrint) NE ? OR SEARCH(REPLACE(runPrint,'.p','.r')) NE ?)THEN
      RUN VALUE(runPrint) (printFile,outputTo,runProgram,printPrgm,fontValue,orientationValue).
      ELSE
      IF ID BEGINS 'MXP' AND (SEARCH(runPrint) NE ? OR SEARCH(REPLACE(runPrint,'.p','.r')) NE ?)THEN
      RUN VALUE(runPrint) (printFile,outputTo,runProgram,printPrgm,fontValue,orientationValue).
      ELSE
      OS-COMMAND NO-WAIT notepad.exe VALUE(printFile).
    END. /* else */
  END. /* if not closing */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE blankFields C-Win 
PROCEDURE blankFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO EXTENT(udfFieldLo):
    IF VALID-HANDLE(udfFieldLo[i]) THEN
    udfFieldLo[i]:SCREEN-VALUE = ''.
    IF VALID-HANDLE(udfFieldHi[i]) THEN
    udfFieldHi[i]:SCREEN-VALUE = ''.
  END.
  DO i = 1 TO EXTENT(userFieldLo):
    IF VALID-HANDLE(userFieldLo[i]) THEN
    userFieldLo[i]:SCREEN-VALUE = ''.
    IF VALID-HANDLE(userFieldHi[i]) THEN
    userFieldHi[i]:SCREEN-VALUE = ''.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    IF ipUsage NE 'fieldFilter' THEN
    ASSIGN
      resourceLo:SCREEN-VALUE = ''
      resourceHi:SCREEN-VALUE = ''.
    ASSIGN
      jobLo = ''
      jobHi = ''
      fromDateObject = ?
      toDateObject = ?
      fromDueDateObject = ?
      toDueDateObject = ?.
    DISPLAY {&dateFields} {&jobFields}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createFields C-Win 
PROCEDURE createFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipYCoord AS INTEGER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  DEFINE VARIABLE iniValue AS CHARACTER NO-UNDO.

  DELETE WIDGET-POOL ipUsage NO-ERROR.
  CREATE WIDGET-POOL ipUsage PERSISTENT.
  
  DO i = 1 TO EXTENT(udfFieldLo) WITH FRAME {&FRAME-NAME}:
    IF udfLabel[i] EQ 'Unused' THEN NEXT.
    FIND FIRST rptFields NO-LOCK WHERE rptFields.fieldLabel EQ udfLabel[i]
                                   AND ID BEGINS rptFields.rptID NO-ERROR.
    IF AVAILABLE rptFields AND NOT rptFields.filterField THEN NEXT.
    RUN createObject (udfLabel[i],udfValueLo[i],udfValueHi[i],
                      INPUT-OUTPUT ipYCoord,OUTPUT udfFieldLo[i],OUTPUT udfFieldHi[i]).
    IF ipYCoord GT hiCoord THEN hiCoord = ipYCoord.
  END. /* do i */
  DO i = 1 TO EXTENT(userFieldLo) WITH FRAME {&FRAME-NAME}:
    IF userLabel[i] EQ 'Not Used' THEN NEXT.
    FIND FIRST rptFields NO-LOCK WHERE rptFields.fieldLabel EQ userLabel[i]
                                   AND ID BEGINS rptFields.rptID NO-ERROR.
    IF AVAILABLE rptFields AND NOT rptFields.filterField THEN NEXT.
    RUN createObject (userLabel[i],userValueLo[i],userValueHi[i],
                      INPUT-OUTPUT ipYCoord,OUTPUT userFieldLo[i],OUTPUT userFieldHi[i]).
    IF ipYCoord GT hiCoord THEN hiCoord = ipYCoord.
  END. /* do i */
  IF hiCoord LT printProgram:NUM-BUTTONS * 20 + printProgram:Y + sortBy:HEIGHT-PIXELS + 5 THEN
  hiCoord = printProgram:NUM-BUTTONS * 20 + printProgram:Y + sortBy:HEIGHT-PIXELS + 5.
  ASSIGN
    hiCoord = IF hiCoord LT 280 THEN 280 ELSE hiCoord
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = hiCoord
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = hiCoord
    {&WINDOW-NAME}:HEIGHT-PIXELS = hiCoord
    {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = hiCoord.
  IF ipUsage EQ 'fieldFilter' THEN
  ASSIGN
    i = 425 + (IF xCoord NE 0 THEN 260 ELSE 0)
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = i
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS = i
    {&WINDOW-NAME}:WIDTH-PIXELS = i
    {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS = i
    hiCoord = IF hiCoord LT 105 THEN 105 ELSE hiCoord.
  ELSE
  DO:
    fontValue:LIST-ITEM-PAIRS = ?.
    DO i = 0 TO FONT-TABLE:NUM-ENTRIES - 1:
      GET-KEY-VALUE SECTION 'FONTS'
        KEY 'font' + STRING(i)
        VALUE iniValue.
      ldummy = fontValue:ADD-LAST(STRING(i,'z9') + ' - ' + REPLACE(iniValue,',',';'),i).
    END.
    ASSIGN
      fontValue:HIDDEN = NO
      fontValue:SENSITIVE = YES
      fontValue:INNER-LINES = fontValue:NUM-ITEMS
      linesPerPage:HIDDEN = NO
      linesPerPage:SENSITIVE = YES
      orientationValue:HIDDEN = NO
      orientationValue:SENSITIVE = YES
      outputRect:HIDDEN = NO
      outputTo:HIDDEN = NO
      outputTo:SENSITIVE = YES
      showParametersPage:HIDDEN = NO
      showParametersPage:SENSITIVE = YES
      printProgram:HEIGHT-PIXELS = printProgram:NUM-BUTTONS * 20.
  END. /* else */
  IF ID BEGINS 'ASI' THEN
  ASSIGN
    departments:HEIGHT-PIXELS = hiCoord - departments:Y
    sortBy:SCREEN-VALUE = sortBy:ENTRY(1)
    sortBy:Y = printProgram:Y + printProgram:HEIGHT-PIXELS + 5
    sortByLabel:Y = sortBy:Y
    sortByLabel:SCREEN-VALUE = 'Sort By:'.
  ELSE
  ASSIGN
    departments:HIDDEN = YES
    sortByLabel:HIDDEN = YES
    sortBy:HIDDEN = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObject C-Win 
PROCEDURE createObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipValueLo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipValueHi AS CHARACTER NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER iopYCoord AS INTEGER NO-UNDO.

  DEFINE OUTPUT PARAMETER opFieldLo AS WIDGET NO-UNDO.
  DEFINE OUTPUT PARAMETER opFieldHi AS WIDGET NO-UNDO.

  DEFINE VARIABLE filterWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE labelWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

  CREATE TEXT labelWidget IN WIDGET-POOL ipUsage
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      AUTO-RESIZE = YES
      Y = iopYCoord
      FORMAT = 'X(' + (IF LENGTH(TRIM(ipLabel)) NE 0 THEN
               STRING(LENGTH(TRIM(ipLabel)) + 2) ELSE '2') + ')'
      SENSITIVE = YES
      SCREEN-VALUE = ipLabel + ': '.
  ASSIGN
    labelWidget:FONT = 2
    labelWidget:X = (IF 90 - labelWidget:WIDTH-PIXELS - 1 LT 0 THEN 0
                     ELSE 90 - labelWidget:WIDTH-PIXELS - 1) + xCoord
    labelWidget:HEIGHT-PIXELS = 17
    ldummy = labelWidget:MOVE-TO-TOP().
  CREATE FILL-IN filterWidget IN WIDGET-POOL ipUsage
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      X = 90 + xCoord
      Y = iopYCoord
      HEIGHT-PIXELS = 17
      WIDTH-PIXELS = 110
      DATA-TYPE = 'CHARACTER'
      FORMAT = 'X(256)'
      SENSITIVE = YES
      SIDE-LABEL-HANDLE = labelWidget.
  ASSIGN
    ldummy = filterWidget:MOVE-TO-TOP()
    filterWidget:SCREEN-VALUE = ipValueLo
    opFieldLo = filterWidget:HANDLE.
  CREATE TEXT labelWidget IN WIDGET-POOL ipUsage
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      AUTO-RESIZE = YES
      Y = iopYCoord
      FORMAT = 'X(4)'
      SENSITIVE = YES
      SCREEN-VALUE = 'To: '.
  ASSIGN
    labelWidget:FONT = 2
    labelWidget:X = (IF 230 - labelWidget:WIDTH-PIXELS - 1 LT 0 THEN 0
                     ELSE 230 - labelWidget:WIDTH-PIXELS - 1) + xCoord
    labelWidget:HEIGHT-PIXELS = 17
    ldummy = labelWidget:MOVE-TO-TOP().
  CREATE FILL-IN filterWidget IN WIDGET-POOL ipUsage
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      X = 230 + xCoord
      Y = iopYCoord
      HEIGHT-PIXELS = 17
      WIDTH-PIXELS = 110
      DATA-TYPE = 'CHARACTER'
      FORMAT = 'X(256)'
      SENSITIVE = YES
      SIDE-LABEL-HANDLE = labelWidget.
  ASSIGN
    ldummy = filterWidget:MOVE-TO-TOP()
    filterWidget:SCREEN-VALUE = ipValueHi
    opFieldHi = filterWidget:HANDLE
    iopYCoord = iopYCoord + 20.

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
  DISPLAY fromDateObject toDateObject fromDueDateObject toDueDateObject jobLo 
          jobHi resourceLo resourceHi 
      WITH FRAME filterFrame IN WINDOW C-Win.
  ENABLE fromDateObject btnFromDate toDateObject btnToDate fromDueDateObject 
         btnFromDueDate toDueDateObject btnToDueDate jobLo jobHi btnBlank 
         btnReset btnApply btnClose 
      WITH FRAME filterFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-filterFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetFields C-Win 
PROCEDURE resetFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO EXTENT(udfFieldLo):
    IF VALID-HANDLE(udfFieldLo[i]) THEN
    udfFieldLo[i]:SCREEN-VALUE = udfValueLo[i].
    IF VALID-HANDLE(udfFieldHi[i]) THEN
    udfFieldHi[i]:SCREEN-VALUE = udfValueHi[i].
  END.
  DO i = 1 TO EXTENT(userFieldLo):
    IF VALID-HANDLE(userFieldLo[i]) THEN
    userFieldLo[i]:SCREEN-VALUE = userValueLo[i].
    IF VALID-HANDLE(userFieldHi[i]) THEN
    userFieldHi[i]:SCREEN-VALUE = userValueHi[i].
  END.
  DO WITH FRAME {&FRAME-NAME}:
    IF ipUsage NE 'fieldFilter' THEN
    ASSIGN
      resourceLo:SCREEN-VALUE = resourceValueLo
      resourceHi:SCREEN-VALUE = resourceValueHi.
    ASSIGN
      jobLo = jobValueLo
      jobHi = jobValueHi
      fromDateObject = fromDate
      toDateObject = toDate
      fromDueDateObject = fromDueDate
      toDueDateObject = toDueDate.
    DISPLAY {&dateFields} {&jobFields}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNormal C-Win 
PROCEDURE setNormal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&WINDOW-NAME}:WINDOW-STATE = 3.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION comma C-Win 
FUNCTION comma RETURNS CHARACTER
  (ipValue AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF ipValue NE '' THEN ',' ELSE ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

