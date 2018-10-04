&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: config.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 4.23.2004

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
DEFINE VARIABLE colorWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE containerHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE dirList AS CHARACTER NO-UNDO.
DEFINE VARIABLE ID AS CHARACTER NO-UNDO {{&includes}/initID.i}.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE priorityWidget AS WIDGET NO-UNDO EXTENT 28.
DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE rectJob AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE scenario AS CHARACTER NO-UNDO.
DEFINE VARIABLE startDir AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE prioritySort NO-UNDO
  FIELD priority AS INTEGER
  FIELD prioritySet AS LOGICAL
  FIELD priorityIdx AS INTEGER
    INDEX priority IS PRIMARY priority prioritySet.

DEFINE WORKFILE userFlds NO-UNDO
  FIELD fieldID AS CHARACTER
  FIELD fieldLabel AS CHARACTER
  FIELD fieldName AS CHARACTER
  FIELD fieldUsed AS LOGICAL.

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

&SCOPED-DEFINE btnChangeColor btnChangeColor-16 btnChangeColor-17 ~
btnChangeColor-18 btnChangeColor-19 btnChangeColor-20 btnChangeColor-21 ~
btnChangeColor-22 btnChangeColor-23 btnChangeColor-24 btnChangeColor-25 ~
btnChangeColor-26 btnChangeColor-27 btnChangeColor-28 btnChangeColor-29

&SCOPED-DEFINE userFields useNotes useStatus ~
userField01 userField02 userField03 userField04 userField05 ~
userField06 userField07 userField08 userField09 userField10 ~
userField11 userField12 userField13 userField14 userField15 ~
userField16 userField17 userField18 userField19 userField20 ~
userField21 userField22 userField23 userField24 userField25 ~
userField26 userField27 userField28 userField29 userField30 ~
userField31 userField32 userField33 userField34 userField35 ~
userField36 userField37 userField38 userField39 userField40 ~
userField41 userField42 userField43 userField44 userField45 ~
userField46 userField47 userField48 userField49 userField50 ~
userField51 userField52 userField53 userField54 userField55 ~
userField56 userField57 userField58 userField59 userField60 ~
userField61 userField62 userField63 userField64 userField65 ~
userField66 userField67 userField68 userField69 userField70 ~
userField71 userField72 userField73 userField74 userField75 ~
userField76 userField77 userField78 userField79 userField80 ~
userField81 userField82 userField83 userField84 userField85 ~
userField86 userField87 userField88 userField89 userField90 ~
userField91 userField92 userField93 userField94 userField95 ~
userField96 userField97
    /*
userField98 
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME configurationFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS configFrameSelection btnChangeColor-16 ~
btnChangeColor-23 btnChangeColor-17 btnChangeColor-24 btnChangeColor-18 ~
btnChangeColor-25 btnChangeColor-19 btnChangeColor-26 btnChangeColor-20 ~
btnChangeColor-27 btnChangeColor-21 btnChangeColor-28 btnChangeColor-22 ~
btnChangeColor-29 btnSave btnRestore 
&Scoped-Define DISPLAYED-OBJECTS configFrameSelection 

/* Custom List Definitions                                              */
/* colorsFrameFields,defaultsFrameFields,defaultsFrameButtons,configButtons,jobLabelFields,customValueFields */
&Scoped-define colorsFrameFields jobLabel-1 customLabel-1 customValue-1 ~
colorPriorityValue-13 jobLabel-2 customLabel-2 customValue-2 ~
colorPriorityValue-14 jobLabel-3 colorPriorityValue-1 customLabel-3 ~
customValue-3 colorPriorityValue-15 jobLabel-4 colorPriorityValue-2 ~
customLabel-4 customValue-4 colorPriorityValue-16 jobLabel-5 ~
colorPriorityValue-3 customLabel-5 customValue-5 colorPriorityValue-17 ~
jobLabel-6 colorPriorityValue-4 customLabel-6 customValue-6 ~
colorPriorityValue-18 jobLabel-7 colorPriorityValue-5 customLabel-7 ~
customValue-7 colorPriorityValue-19 jobLabel-8 colorPriorityValue-6 ~
customLabel-8 customValue-8 colorPriorityValue-20 jobLabel-9 ~
colorPriorityValue-7 customLabel-9 customValue-9 colorPriorityValue-21 ~
jobLabel-10 colorPriorityValue-8 customLabel-10 customValue-10 ~
colorPriorityValue-22 jobLabel-11 colorPriorityValue-9 customLabel-11 ~
customValue-11 colorPriorityValue-23 jobLabel-12 colorPriorityValue-10 ~
customLabel-12 customValue-12 colorPriorityValue-24 jobLabel-13 ~
colorPriorityValue-11 customLabel-13 customValue-13 colorPriorityValue-25 ~
jobLabel-14 colorPriorityValue-12 customLabel-14 customValue-14 ~
colorPriorityValue-26 colorPriorityValue-27 colorPriorityValue-28 ~
reloadReportValue customCheckoffValue reloadStatusValue ~
completedCheckoffValue dueDateUsedValue 
&Scoped-define defaultsFrameFields intervals lockButtonValue threeDValue ~
timeValue autoSizeValue noteButtonValue threeDLeftValue threeDTopValue ~
hpixelsValue gapValue showStatusValue allResourcesValue threeDBottomValue ~
threeDRightValue downtimeTopValue alphaSortValue downtimeSizeValue ~
flashGridLineValue completedHideValue flashTimeLineValue fullBoardValue ~
saveIntervalValue priority1Value popupBottomValue priority2Value ~
viewRefreshValue priority3Value moveUndoRedoValue detailWindowValue ~
showDowntimeValue IDList jobBlockValue jobWarningValue jobPromptValue ~
flashLightValue boardDatePromptValue downtimeBlockValue ~
downtimeWarningValue downtimePromptValue packOptionValue pendingOverValue ~
packOptionPromptValue pendingLastDayValue pendingDaysValue datePromptValue ~
resourceJobDetailValue dontShowStartup boardSize resourceBrowseActionValue ~
loadCapacityValue endDateBufferValue 
&Scoped-define defaultsFrameButtons flashLightColorValue gridBGColorValue ~
gridLineColorValue lightBulbColorValue resourceBGColorValue ~
timeLineColorValue btnDetailBoard btnShowDowntime btnFlashLight ~
btnBoardDatePrompt btnDatePrompt btnDetailResource 
&Scoped-define configButtons colorChoice-0 colorChoice-1 colorChoice-10 ~
colorChoice-11 colorChoice-12 colorChoice-13 colorChoice-14 colorChoice-15 ~
colorChoice-30 colorChoice-2 colorChoice-3 colorChoice-4 colorChoice-5 ~
colorChoice-6 colorChoice-7 colorChoice-8 colorChoice-9 colorChoice-16 ~
colorChoice-17 colorChoice-18 colorChoice-19 colorChoice-20 colorChoice-21 ~
colorChoice-22 colorChoice-23 colorChoice-24 colorChoice-25 colorChoice-26 ~
colorChoice-27 colorChoice-28 colorChoice-29 
&Scoped-define jobLabelFields customBGColor-1 customBGColor-10 ~
customBGColor-11 customBGColor-12 customBGColor-13 customBGColor-14 ~
customBGColor-2 customBGColor-3 customBGColor-4 customBGColor-5 ~
customBGColor-6 customBGColor-7 customBGColor-8 customBGColor-9 ~
downtimeConflictBGColorValue jobBGColor-1 jobBGColor-10 jobBGColor-11 ~
jobBGColor-12 jobBGColor-13 jobBGColor-14 jobBGColor-2 jobBGColor-3 ~
jobBGColor-4 jobBGColor-5 jobBGColor-6 jobBGColor-7 jobBGColor-8 ~
jobBGColor-9 jobConflictBGColorValue jobFGColor-1 jobFGColor-2 jobFGColor-3 ~
jobFGColor-4 jobFGColor-5 jobFGColor-6 jobFGColor-7 jobFGColor-8 ~
jobFGColor-9 jobFGColor-10 jobFGColor-11 jobFGColor-12 jobFGColor-13 ~
jobFGColor-14 jobConflictFGColorValue downtimeConflictFGColorValue ~
customFGColor-1 customFGColor-2 customFGColor-3 customFGColor-4 ~
customFGColor-5 customFGColor-6 customFGColor-7 customFGColor-8 ~
customFGColor-9 customFGColor-10 customFGColor-11 customFGColor-12 ~
customFGColor-13 customFGColor-14 jobLabel-1 jobLabel-2 jobLabel-3 ~
colorPriorityValue-1 jobLabel-4 colorPriorityValue-2 jobLabel-5 ~
colorPriorityValue-3 jobLabel-6 colorPriorityValue-4 jobLabel-7 ~
colorPriorityValue-5 jobLabel-8 colorPriorityValue-6 jobLabel-9 ~
colorPriorityValue-7 jobLabel-10 colorPriorityValue-8 jobLabel-11 ~
colorPriorityValue-9 jobLabel-12 colorPriorityValue-10 jobLabel-13 ~
colorPriorityValue-11 jobLabel-14 colorPriorityValue-12 ~
colorPriorityValue-27 colorPriorityValue-28 
&Scoped-define customValueFields customLabel-1 customValue-1 ~
colorPriorityValue-13 customLabel-2 customValue-2 colorPriorityValue-14 ~
customLabel-3 customValue-3 colorPriorityValue-15 customLabel-4 ~
customValue-4 colorPriorityValue-16 customLabel-5 customValue-5 ~
colorPriorityValue-17 customLabel-6 customValue-6 colorPriorityValue-18 ~
customLabel-7 customValue-7 colorPriorityValue-19 customLabel-8 ~
customValue-8 colorPriorityValue-20 customLabel-9 customValue-9 ~
colorPriorityValue-21 customLabel-10 customValue-10 colorPriorityValue-22 ~
customLabel-11 customValue-11 colorPriorityValue-23 customLabel-12 ~
customValue-12 colorPriorityValue-24 customLabel-13 customValue-13 ~
colorPriorityValue-25 customLabel-14 customValue-14 colorPriorityValue-26 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCheckCustomValue sObject 
FUNCTION fCheckCustomValue RETURNS LOGICAL
  (iphCustomValue AS HANDLE, iphCheckValue AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE colorChoice-0
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 0 .

DEFINE RECTANGLE colorChoice-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 1 .

DEFINE RECTANGLE colorChoice-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY .95
     BGCOLOR 10 .

DEFINE RECTANGLE colorChoice-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 11 .

DEFINE RECTANGLE colorChoice-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 12 .

DEFINE RECTANGLE colorChoice-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 13 .

DEFINE RECTANGLE colorChoice-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 14 .

DEFINE RECTANGLE colorChoice-15
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE colorChoice-16
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 16 .

DEFINE RECTANGLE colorChoice-17
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 17 .

DEFINE RECTANGLE colorChoice-18
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 18 .

DEFINE RECTANGLE colorChoice-19
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 19 .

DEFINE RECTANGLE colorChoice-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 2 .

DEFINE RECTANGLE colorChoice-20
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 20 .

DEFINE RECTANGLE colorChoice-21
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 21 .

DEFINE RECTANGLE colorChoice-22
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 22 .

DEFINE RECTANGLE colorChoice-23
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 23 .

DEFINE RECTANGLE colorChoice-24
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 24 .

DEFINE RECTANGLE colorChoice-25
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 25 .

DEFINE RECTANGLE colorChoice-26
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 26 .

DEFINE RECTANGLE colorChoice-27
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 27 .

DEFINE RECTANGLE colorChoice-28
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 28 .

DEFINE RECTANGLE colorChoice-29
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 29 .

DEFINE RECTANGLE colorChoice-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 3 .

DEFINE RECTANGLE colorChoice-30
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 1.

DEFINE RECTANGLE colorChoice-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 4 .

DEFINE RECTANGLE colorChoice-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 5 .

DEFINE RECTANGLE colorChoice-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 6 .

DEFINE RECTANGLE colorChoice-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 7 .

DEFINE RECTANGLE colorChoice-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE colorChoice-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 7.2 BY 1
     BGCOLOR 9 .

DEFINE BUTTON btnHTMLPageLocation 
     LABEL "Set HTML Page Location" 
     SIZE 26 BY 1.14 TOOLTIP "HTML Page Location".

DEFINE VARIABLE colorPriorityValue-1 AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-10 AS INTEGER FORMAT ">9":U INITIAL 10 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-11 AS INTEGER FORMAT ">9":U INITIAL 11 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-12 AS INTEGER FORMAT ">9":U INITIAL 12 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-13 AS INTEGER FORMAT ">9":U INITIAL 13 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-14 AS INTEGER FORMAT ">9":U INITIAL 14 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-15 AS INTEGER FORMAT ">9":U INITIAL 15 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-16 AS INTEGER FORMAT ">9":U INITIAL 16 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-17 AS INTEGER FORMAT ">9":U INITIAL 17 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-18 AS INTEGER FORMAT ">9":U INITIAL 18 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-19 AS INTEGER FORMAT ">9":U INITIAL 19 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-2 AS INTEGER FORMAT ">9":U INITIAL 2 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-20 AS INTEGER FORMAT ">9":U INITIAL 20 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-21 AS INTEGER FORMAT ">9":U INITIAL 21 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-22 AS INTEGER FORMAT ">9":U INITIAL 22 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-23 AS INTEGER FORMAT ">9":U INITIAL 23 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-24 AS INTEGER FORMAT ">9":U INITIAL 24 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-25 AS INTEGER FORMAT ">9":U INITIAL 25 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-26 AS INTEGER FORMAT ">9":U INITIAL 26 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-27 AS INTEGER FORMAT ">9":U INITIAL 27 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-28 AS INTEGER FORMAT ">9":U INITIAL 28 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-3 AS INTEGER FORMAT ">9":U INITIAL 3 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-4 AS INTEGER FORMAT ">9":U INITIAL 4 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-5 AS INTEGER FORMAT ">9":U INITIAL 5 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-6 AS INTEGER FORMAT ">9":U INITIAL 6 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-7 AS INTEGER FORMAT ">9":U INITIAL 7 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-8 AS INTEGER FORMAT ">9":U INITIAL 8 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE colorPriorityValue-9 AS INTEGER FORMAT ">9":U INITIAL 9 
     VIEW-AS COMBO-BOX INNER-LINES 28
     LIST-ITEMS "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-13 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-14 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customValue-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE customLabel-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "01" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "10" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-11 AS CHARACTER FORMAT "X(256)":U 
     LABEL "11" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-12 AS CHARACTER FORMAT "X(256)":U 
     LABEL "12" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "13" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "14" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "02" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "03" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "04" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "05" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "06" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "07" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "08" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE customLabel-9 AS CHARACTER FORMAT "X(256)":U 
     LABEL "09" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "01" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "10" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-11 AS CHARACTER FORMAT "X(256)":U 
     LABEL "11" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-12 AS CHARACTER FORMAT "X(256)":U 
     LABEL "12" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "13" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "14" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "02" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "03" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "04" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "05" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "06" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "07" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "08" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE jobLabel-9 AS CHARACTER FORMAT "X(256)":U 
     LABEL "09" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dueDateUsedValue AS LOGICAL INITIAL yes 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Due Date", yes,
"Production Date", no,
"Mfg Date", ?
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE reloadReportValue AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Re-Load ALL Jobs", yes,
"Use Last Saved Jobs", no,
"Prompt", ?
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE reloadStatusValue AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Re-Load ALL Jobs", yes,
"Use Last Saved Jobs", no,
"Prompt", ?
     SIZE 59 BY 1 NO-UNDO.

DEFINE RECTANGLE customBGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customBGColor-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE customFGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE customFGColor-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2.2 BY 1.

DEFINE RECTANGLE downtimeConflictBGColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE downtimeConflictFGColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobBGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobBGColor-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobConflictBGColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE jobConflictFGColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-11
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-14
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-3
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-4
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-5
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-6
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE jobFGColor-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 2 BY 1.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.86.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 2.86.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 1.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 127 BY 1.43.

DEFINE VARIABLE completedCheckoffValue AS LOGICAL INITIAL no 
     LABEL "&Apply Completed Checkoff to Whole Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE customCheckoffValue AS LOGICAL INITIAL no 
     LABEL "&Apply Custom Value Checkoff to Whole Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY 1 NO-UNDO.

DEFINE BUTTON btnChangeColor-16 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-17 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-18 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-19 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-20 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-21 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-22 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-23 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-24 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-25 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-26 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-27 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-28 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnChangeColor-29 
     LABEL "" 
     SIZE 2 BY .48
     FONT 6.

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.24 TOOLTIP "Restore from Last Save"
     BGCOLOR 8 .

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.24 TOOLTIP "Save Selections".

DEFINE VARIABLE configFrameSelection AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Color Settings", "Color",
"Default Settings", "Default",
"Fields to Load", "Fields"
     SIZE 20 BY 2.62 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 1.67.

DEFINE BUTTON btnBoardDatePrompt 
     IMAGE-UP FILE "schedule/images/dateon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Turn Date Prompt Popup Off".

DEFINE BUTTON btnDatePrompt 
     IMAGE-UP FILE "schedule/images/dateon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.1 TOOLTIP "Turn Date Prompt Popup Off".

DEFINE BUTTON btnDetailBoard 
     IMAGE-UP FILE "schedule/images/detailwinon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Turn Detail Window Display Off".

DEFINE BUTTON btnDetailResource 
     IMAGE-UP FILE "schedule/images/detailwinon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Turn Detail Window Display Off".

DEFINE BUTTON btnFlashLight 
     IMAGE-UP FILE "schedule/images/lightbulbon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Turn Highlight On".

DEFINE BUTTON btnShowDowntime 
     IMAGE-UP FILE "schedule/images/downtimeon.bmp":U
     LABEL "" 
     SIZE 5.2 BY 1.05 TOOLTIP "Show Downtime".

DEFINE VARIABLE boardSize AS CHARACTER FORMAT "X(256)":U INITIAL "Maximize" 
     LABEL "Board Size" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "800x600","1024x768","1152x864","1280x960","1280x1024","Maximize" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE downtimeSizeValue AS INTEGER FORMAT "-9":U INITIAL 0 
     LABEL "&Downtime Size" 
     VIEW-AS COMBO-BOX INNER-LINES 9
     LIST-ITEM-PAIRS "+8",8,
                     "+6",6,
                     "+4",4,
                     "+2",2,
                     "0",0,
                     "-2",-2,
                     "-4",-4,
                     "-6",-6,
                     "-8",-8
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE gapValue AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "&Gap" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE hpixelsValue AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "H&eight" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE intervals AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Intervals" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE lockButtonValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Locks" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE noteButtonValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Notes" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 28 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE packOptionValue AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Pack Opt" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEM-PAIRS "1 First Job to VisibleEnd",1,
                     "2 Current to VisibleEnd",2,
                     "3 VisibleBegin to VisibleEnd",3,
                     "4 Visible Job to VisibleEnd",4,
                     "5 First Job to BoardEnd",5,
                     "6 Current to BoardEnd",6,
                     "7 VisibleBegin to BoardEnd",7,
                     "8 Visible Job to BoardEnd",8
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE timeValue AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "&Time" 
     VIEW-AS COMBO-BOX INNER-LINES 24
     LIST-ITEMS "0000","0100","0200","0300","0400","0500","0600","0700","0800","0900","1000","1100","1200","1300","1400","1500","1600","1700","1800","1900","2000","2100","2200","2300" 
     DROP-DOWN-LIST
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE rectJobBox AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 34 BY 1.14
     BGCOLOR 10 FONT 6 NO-UNDO.

DEFINE VARIABLE rectJobNoBox AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 33.6 BY .95
     BGCOLOR 10 FONT 6 NO-UNDO.

DEFINE VARIABLE endDateBufferValue AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Downtime End Date Buffer" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE pendingDaysValue AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Pending Days (End Date Prior to Due Date)" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE pendingLastDayValue AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Pending Last Day +" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE pendingOverValue AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Prompt Warning if Pending Job Start/End Date exceeds Current Date +" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE rectDTOver AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 15 BY 1.62
     BGCOLOR 0  NO-UNDO.

DEFINE VARIABLE resourceBrowseActionValue AS LOGICAL FORMAT "GoTo/Update":U INITIAL NO 
     LABEL "Browser Default Action (&U)pdate/(G)oTo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE saveIntervalValue AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 TOOLTIP "Auto Save Interval Minutes" NO-UNDO.

DEFINE VARIABLE viewRefreshValue AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 TOOLTIP "Auto View Refresh Interval Minutes" NO-UNDO.

DEFINE IMAGE lockImage
     FILENAME "schedule/images/locked.gif":U
     SIZE 2.4 BY .57.

DEFINE IMAGE noteImage-1
     FILENAME "schedule/images/notetack.bmp":U
     SIZE 2.4 BY .57.

DEFINE IMAGE noteImage-2
     FILENAME "schedule/images/notetackred.bmp":U
     SIZE 2.4 BY .57.

DEFINE VARIABLE priority1Value AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "1", 1,
"2", 2,
"3", 3
     SIZE 25 BY .62 NO-UNDO.

DEFINE VARIABLE priority2Value AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "1", 1,
"2", 2,
"3", 3
     SIZE 25 BY .62 NO-UNDO.

DEFINE VARIABLE priority3Value AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "1", 1,
"2", 2,
"3", 3
     SIZE 25 BY .62 NO-UNDO.

DEFINE VARIABLE threeDValue AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "3D", yes,
"Flat", no,
"Etched", ?
     SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE flashLightColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE gridBGColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE gridLineColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE lightBulbColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 17.38.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 3.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 4.29.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 4.05.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 2.86.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 9.05.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 2.62.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 3.1.

DEFINE RECTANGLE rectBottom
     EDGE-PIXELS 0    
     SIZE 34 BY .1
     BGCOLOR 0 .

DEFINE RECTANGLE rectDTUnder
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 15 BY 1.62
     BGCOLOR 0 FGCOLOR 8 .

DEFINE RECTANGLE rectGrid
     EDGE-PIXELS 0    
     SIZE 37 BY 1.62
     BGCOLOR 7 .

DEFINE RECTANGLE rectLeft
     EDGE-PIXELS 0    
     SIZE .4 BY 1.14
     BGCOLOR 15 .

DEFINE RECTANGLE rectRight
     EDGE-PIXELS 0    
     SIZE .4 BY 1.14
     BGCOLOR 0 .

DEFINE RECTANGLE rectTop
     EDGE-PIXELS 0    
     SIZE 34 BY .1
     BGCOLOR 15 .

DEFINE RECTANGLE resourceBGColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE RECTANGLE timeLineColorValue
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 6.2 BY 1.

DEFINE VARIABLE IDList AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 37 BY 3.1 NO-UNDO.

DEFINE VARIABLE allResourcesValue AS LOGICAL INITIAL no 
     LABEL "Sh&ow All Resources" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE alphaSortValue AS LOGICAL INITIAL no 
     LABEL "Alpha Sort &Resources" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE autoSizeValue AS LOGICAL INITIAL no 
     LABEL "&Auto Size" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE boardDatePromptValue AS LOGICAL INITIAL no 
     LABEL "Date && Time Prompt on Drop && Drag" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE completedHideValue AS LOGICAL INITIAL no 
     LABEL "Hide Job When Completed" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE datePromptValue AS LOGICAL INITIAL no 
     LABEL "Date && Time Prompt o&n Update" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE detailWindowValue AS LOGICAL INITIAL no 
     LABEL "Show &Job && Resource Popup" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE dontShowStartup AS LOGICAL INITIAL no 
     LABEL "&Hide Startup Configuration" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE downtimeBlockValue AS LOGICAL INITIAL no 
     LABEL "&Block" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE downtimePromptValue AS LOGICAL INITIAL no 
     LABEL "&Prompt" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE downtimeTopValue AS LOGICAL INITIAL no 
     LABEL "Show Downtime on Top of Job" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE downtimeWarningValue AS LOGICAL INITIAL no 
     LABEL "&Warning" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE flashGridLineValue AS LOGICAL INITIAL no 
     LABEL "Flashing Grid Line" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE flashLightValue AS LOGICAL INITIAL no 
     LABEL "&Flashlight Initial Setting" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE flashTimeLineValue AS LOGICAL INITIAL no 
     LABEL "Flashing Time Line" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE fullBoardValue AS LOGICAL INITIAL no 
     LABEL "Use Full &Board" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE jobBlockValue AS LOGICAL INITIAL no 
     LABEL "&Block" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE jobPromptValue AS LOGICAL INITIAL no 
     LABEL "&Prompt" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE jobWarningValue AS LOGICAL INITIAL no 
     LABEL "&Warning" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE loadCapacityValue AS LOGICAL INITIAL no 
     LABEL "Load &Capacity (Shifts/Downtime)" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE moveUndoRedoValue AS LOGICAL INITIAL no 
     LABEL "Reposition Board after Undo/Redo" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE packOptionPromptValue AS LOGICAL INITIAL no 
     LABEL "Pack Opt. Prompt" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .76 NO-UNDO.

DEFINE VARIABLE popupBottomValue AS LOGICAL INITIAL no 
     LABEL "&Position Job Popup at Screen Bottom" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE resourceJobDetailValue AS LOGICAL INITIAL no 
     LABEL "Sho&w Job Popup" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE showDowntimeValue AS LOGICAL INITIAL no 
     LABEL "&Show Downtime" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE showStatusValue AS LOGICAL INITIAL no 
     LABEL "Show Blue Screen Status &Messages" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE threeDBottomValue AS LOGICAL INITIAL no 
     LABEL "Bottom Edge" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE threeDLeftValue AS LOGICAL INITIAL no 
     LABEL "Left Edge" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE threeDRightValue AS LOGICAL INITIAL no 
     LABEL "Right Edge" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE threeDTopValue AS LOGICAL INITIAL no 
     LABEL "Top Edge" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE useNotes AS LOGICAL INITIAL no 
     LABEL "SB Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField01 AS LOGICAL INITIAL no 
     LABEL "User Field 01" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField02 AS LOGICAL INITIAL no 
     LABEL "User Field 02" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField03 AS LOGICAL INITIAL no 
     LABEL "User Field 03" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField04 AS LOGICAL INITIAL no 
     LABEL "User Field 04" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField05 AS LOGICAL INITIAL no 
     LABEL "User Field 05" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField06 AS LOGICAL INITIAL no 
     LABEL "User Field 06" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField07 AS LOGICAL INITIAL no 
     LABEL "User Field 07" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField08 AS LOGICAL INITIAL no 
     LABEL "User Field 08" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField09 AS LOGICAL INITIAL no 
     LABEL "User Field 09" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField10 AS LOGICAL INITIAL no 
     LABEL "User Field 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField11 AS LOGICAL INITIAL no 
     LABEL "User Field 11" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField12 AS LOGICAL INITIAL no 
     LABEL "User Field 12" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField13 AS LOGICAL INITIAL no 
     LABEL "User Field 13" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField14 AS LOGICAL INITIAL no 
     LABEL "User Field 14" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField15 AS LOGICAL INITIAL no 
     LABEL "User Field 15" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField16 AS LOGICAL INITIAL no 
     LABEL "User Field 16" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField17 AS LOGICAL INITIAL no 
     LABEL "User Field 17" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField18 AS LOGICAL INITIAL no 
     LABEL "User Field 18" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField19 AS LOGICAL INITIAL no 
     LABEL "User Field 19" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField20 AS LOGICAL INITIAL no 
     LABEL "User Field 20" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField21 AS LOGICAL INITIAL no 
     LABEL "User Field 21" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField22 AS LOGICAL INITIAL no 
     LABEL "User Field 22" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField23 AS LOGICAL INITIAL no 
     LABEL "User Field 23" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField24 AS LOGICAL INITIAL no 
     LABEL "User Field 24" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField25 AS LOGICAL INITIAL no 
     LABEL "User Field 25" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField26 AS LOGICAL INITIAL no 
     LABEL "User Field 26" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField27 AS LOGICAL INITIAL no 
     LABEL "User Field 27" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField28 AS LOGICAL INITIAL no 
     LABEL "User Field 28" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField29 AS LOGICAL INITIAL no 
     LABEL "User Field 29" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField30 AS LOGICAL INITIAL no 
     LABEL "User Field 30" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField31 AS LOGICAL INITIAL no 
     LABEL "User Field 31" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField32 AS LOGICAL INITIAL no 
     LABEL "User Field 32" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField33 AS LOGICAL INITIAL no 
     LABEL "User Field 33" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField34 AS LOGICAL INITIAL no 
     LABEL "User Field 34" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField35 AS LOGICAL INITIAL no 
     LABEL "User Field 35" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField36 AS LOGICAL INITIAL no 
     LABEL "User Field 36" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField37 AS LOGICAL INITIAL no 
     LABEL "User Field 37" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField38 AS LOGICAL INITIAL no 
     LABEL "User Field 38" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField39 AS LOGICAL INITIAL no 
     LABEL "User Field 39" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField40 AS LOGICAL INITIAL no 
     LABEL "User Field 40" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField41 AS LOGICAL INITIAL no 
     LABEL "User Field 41" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField42 AS LOGICAL INITIAL no 
     LABEL "User Field 42" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField43 AS LOGICAL INITIAL no 
     LABEL "User Field 43" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField44 AS LOGICAL INITIAL no 
     LABEL "User Field 44" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField45 AS LOGICAL INITIAL no 
     LABEL "User Field 45" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField46 AS LOGICAL INITIAL no 
     LABEL "User Field 46" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField47 AS LOGICAL INITIAL no 
     LABEL "User Field 47" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField48 AS LOGICAL INITIAL no 
     LABEL "User Field 48" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField49 AS LOGICAL INITIAL no 
     LABEL "User Field 49" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField50 AS LOGICAL INITIAL no 
     LABEL "User Field 50" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField51 AS LOGICAL INITIAL no 
     LABEL "User Field 51" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField52 AS LOGICAL INITIAL no 
     LABEL "User Field 52" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField53 AS LOGICAL INITIAL no 
     LABEL "User Field 53" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField54 AS LOGICAL INITIAL no 
     LABEL "User Field 54" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField55 AS LOGICAL INITIAL no 
     LABEL "User Field 55" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField56 AS LOGICAL INITIAL no 
     LABEL "User Field 56" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField57 AS LOGICAL INITIAL no 
     LABEL "User Field 57" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField58 AS LOGICAL INITIAL no 
     LABEL "User Field 58" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField59 AS LOGICAL INITIAL no 
     LABEL "User Field 59" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField60 AS LOGICAL INITIAL no 
     LABEL "User Field 60" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField61 AS LOGICAL INITIAL no 
     LABEL "User Field 61" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField62 AS LOGICAL INITIAL no 
     LABEL "User Field 62" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField63 AS LOGICAL INITIAL no 
     LABEL "User Field 63" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField64 AS LOGICAL INITIAL no 
     LABEL "User Field 64" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField65 AS LOGICAL INITIAL no 
     LABEL "User Field 65" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField66 AS LOGICAL INITIAL no 
     LABEL "User Field 66" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField67 AS LOGICAL INITIAL no 
     LABEL "User Field 67" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField68 AS LOGICAL INITIAL no 
     LABEL "User Field 68" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField69 AS LOGICAL INITIAL no 
     LABEL "User Field 69" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField70 AS LOGICAL INITIAL no 
     LABEL "User Field 70" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField71 AS LOGICAL INITIAL no 
     LABEL "User Field 71" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField72 AS LOGICAL INITIAL no 
     LABEL "User Field 72" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField73 AS LOGICAL INITIAL no 
     LABEL "User Field 73" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField74 AS LOGICAL INITIAL no 
     LABEL "User Field 74" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField75 AS LOGICAL INITIAL no 
     LABEL "User Field 75" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField76 AS LOGICAL INITIAL no 
     LABEL "User Field 76" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField77 AS LOGICAL INITIAL no 
     LABEL "User Field 77" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField78 AS LOGICAL INITIAL no 
     LABEL "User Field 78" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField79 AS LOGICAL INITIAL no 
     LABEL "User Field 79" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField80 AS LOGICAL INITIAL no 
     LABEL "User Field 80" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField81 AS LOGICAL INITIAL no 
     LABEL "User Field 81" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField82 AS LOGICAL INITIAL no 
     LABEL "User Field 82" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField83 AS LOGICAL INITIAL no 
     LABEL "User Field 83" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField84 AS LOGICAL INITIAL no 
     LABEL "User Field 84" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField85 AS LOGICAL INITIAL no 
     LABEL "User Field 85" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField86 AS LOGICAL INITIAL no 
     LABEL "User Field 86" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField87 AS LOGICAL INITIAL no 
     LABEL "User Field 87" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField88 AS LOGICAL INITIAL no 
     LABEL "User Field 88" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField89 AS LOGICAL INITIAL no 
     LABEL "User Field 89" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField90 AS LOGICAL INITIAL no 
     LABEL "User Field 90" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField91 AS LOGICAL INITIAL no 
     LABEL "User Field 91" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField92 AS LOGICAL INITIAL no 
     LABEL "User Field 92" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField93 AS LOGICAL INITIAL no 
     LABEL "User Field 93" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField94 AS LOGICAL INITIAL no 
     LABEL "User Field 94" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField95 AS LOGICAL INITIAL no 
     LABEL "User Field 95" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField96 AS LOGICAL INITIAL no 
     LABEL "User Field 96" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField97 AS LOGICAL INITIAL no 
     LABEL "User Field 97" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE userField98 AS LOGICAL INITIAL no 
     LABEL "User Field 98" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE useStatus AS LOGICAL INITIAL no 
     LABEL "Status Checkoffs" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME configurationFrame
     configFrameSelection AT ROW 1.24 COL 3 NO-LABEL
     btnChangeColor-16 AT ROW 15.29 COL 1
     btnChangeColor-23 AT ROW 15.29 COL 20
     btnChangeColor-17 AT ROW 16.48 COL 1
     btnChangeColor-24 AT ROW 16.48 COL 20
     btnChangeColor-18 AT ROW 17.67 COL 1
     btnChangeColor-25 AT ROW 17.67 COL 20
     btnChangeColor-19 AT ROW 18.86 COL 1
     btnChangeColor-26 AT ROW 18.86 COL 20
     btnChangeColor-20 AT ROW 20.05 COL 1
     btnChangeColor-27 AT ROW 20.05 COL 20
     btnChangeColor-21 AT ROW 21.24 COL 1
     btnChangeColor-28 AT ROW 21.24 COL 20
     btnChangeColor-22 AT ROW 22.43 COL 1
     btnChangeColor-29 AT ROW 22.43 COL 20
     btnSave AT ROW 24.81 COL 5 HELP
          "Click to Save Selections"
     btnRestore AT ROW 24.81 COL 13 HELP
          "Click to Restore from Last Save"
     RECT-6 AT ROW 24.57 COL 3
     SPACE(133.00) SKIP(0.24)
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         TITLE "Scheduler Configuration".

DEFINE FRAME colorsFrame
     jobLabel-1 AT ROW 1.95 COL 3 COLON-ALIGNED
     customLabel-1 AT ROW 1.95 COL 73 COLON-ALIGNED
     customValue-1 AT ROW 1.95 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-13 AT ROW 1.95 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-2 AT ROW 3.14 COL 3 COLON-ALIGNED
     customLabel-2 AT ROW 3.14 COL 73 COLON-ALIGNED
     customValue-2 AT ROW 3.14 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-14 AT ROW 3.14 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-3 AT ROW 4.33 COL 3 COLON-ALIGNED
     colorPriorityValue-1 AT ROW 4.33 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-3 AT ROW 4.33 COL 73 COLON-ALIGNED
     customValue-3 AT ROW 4.33 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-15 AT ROW 4.33 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-4 AT ROW 5.52 COL 3 COLON-ALIGNED
     colorPriorityValue-2 AT ROW 5.52 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-4 AT ROW 5.52 COL 73 COLON-ALIGNED
     customValue-4 AT ROW 5.52 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-16 AT ROW 5.52 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-5 AT ROW 6.71 COL 3 COLON-ALIGNED
     colorPriorityValue-3 AT ROW 6.71 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-5 AT ROW 6.71 COL 73 COLON-ALIGNED
     customValue-5 AT ROW 6.71 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-17 AT ROW 6.71 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-6 AT ROW 7.91 COL 3 COLON-ALIGNED
     colorPriorityValue-4 AT ROW 7.91 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-6 AT ROW 7.91 COL 73 COLON-ALIGNED
     customValue-6 AT ROW 7.91 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-18 AT ROW 7.91 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-7 AT ROW 9.1 COL 3 COLON-ALIGNED
     colorPriorityValue-5 AT ROW 9.1 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-7 AT ROW 9.1 COL 73 COLON-ALIGNED
     customValue-7 AT ROW 9.1 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-19 AT ROW 9.1 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-8 AT ROW 10.29 COL 3 COLON-ALIGNED
     colorPriorityValue-6 AT ROW 10.29 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-8 AT ROW 10.29 COL 73 COLON-ALIGNED
     customValue-8 AT ROW 10.29 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-20 AT ROW 10.29 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-9 AT ROW 11.48 COL 3 COLON-ALIGNED
     colorPriorityValue-7 AT ROW 11.48 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-9 AT ROW 11.48 COL 73 COLON-ALIGNED
     customValue-9 AT ROW 11.48 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-21 AT ROW 11.48 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-10 AT ROW 12.67 COL 3 COLON-ALIGNED
     colorPriorityValue-8 AT ROW 12.67 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-10 AT ROW 12.67 COL 73 COLON-ALIGNED
     customValue-10 AT ROW 12.67 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-22 AT ROW 12.67 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-11 AT ROW 13.86 COL 3 COLON-ALIGNED
     colorPriorityValue-9 AT ROW 13.86 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-11 AT ROW 13.86 COL 73 COLON-ALIGNED
     customValue-11 AT ROW 13.86 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-23 AT ROW 13.86 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-12 AT ROW 15.05 COL 3 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME colorsFrame
     colorPriorityValue-10 AT ROW 15.05 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-12 AT ROW 15.05 COL 73 COLON-ALIGNED
     customValue-12 AT ROW 15.05 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-24 AT ROW 15.05 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-13 AT ROW 16.24 COL 3 COLON-ALIGNED
     colorPriorityValue-11 AT ROW 16.24 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-13 AT ROW 16.24 COL 73 COLON-ALIGNED
     customValue-13 AT ROW 16.24 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-25 AT ROW 16.24 COL 118 COLON-ALIGNED NO-LABEL
     jobLabel-14 AT ROW 17.43 COL 3 COLON-ALIGNED
     colorPriorityValue-12 AT ROW 17.43 COL 33 COLON-ALIGNED NO-LABEL
     customLabel-14 AT ROW 17.43 COL 73 COLON-ALIGNED
     customValue-14 AT ROW 17.43 COL 93 COLON-ALIGNED NO-LABEL
     colorPriorityValue-26 AT ROW 17.43 COL 118 COLON-ALIGNED NO-LABEL
     colorPriorityValue-27 AT ROW 18.86 COL 33 COLON-ALIGNED NO-LABEL
     colorPriorityValue-28 AT ROW 18.86 COL 118 COLON-ALIGNED NO-LABEL
     reloadReportValue AT ROW 21 COL 21 NO-LABEL
     customCheckoffValue AT ROW 21 COL 83 HELP
          "Select to Apply Checkoff to Whole Job vs. Each Resource"
     reloadStatusValue AT ROW 22.19 COL 21 NO-LABEL
     completedCheckoffValue AT ROW 22.19 COL 83 HELP
          "Select to Apply Completed Checkoff to Whole Job vs. Each Resour"
     btnHTMLPageLocation AT ROW 23.62 COL 93 HELP
          "Click to Set HTML Page Location" WIDGET-ID 2
     dueDateUsedValue AT ROW 24.1 COL 32 HELP
          "Select Due Date/Production Date" NO-LABEL
     "Default ~"<Due>~" Value Used:" VIEW-AS TEXT
          SIZE 28 BY .81 AT ROW 24.1 COL 3
     "BG   FG" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 112
          FONT 1
     "<Job Completed>" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 3.14 COL 44
          FONT 1
     "<Due><Start--End><Now>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 15.05 COL 44
          FONT 1
     "Priority" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 121
     "Status Checkoffs:" VIEW-AS TEXT
          SIZE 17 BY .81 AT ROW 22.19 COL 3
     "[External Program Load Setting]" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 20.29 COL 3
          BGCOLOR 8 
     "Priority" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 36
     "<Due><Start--Now--End>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 16.24 COL 44
          FONT 1
     "Custom Color Label" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 1.24 COL 76
     "<Start--Now--End><Due>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 7.91 COL 44
          FONT 1
     "<Now><Start--Due--End>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 5.52 COL 44
          FONT 1
     "<Start--Due--End><Now>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 13.86 COL 44
          FONT 1
     "<Due><Now><Start--End>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 17.43 COL 44
          FONT 1
     "<Start--End><Due><Now>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 12.67 COL 44
          FONT 1
     "[Status Checkoff Default Settings]" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 20.29 COL 83
          BGCOLOR 8 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME colorsFrame
     "<Start--Due--Now--End>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 10.29 COL 44
          FONT 1
     "<Now><Start--End><Due>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 4.33 COL 44
          FONT 1
     "Job Conflict Color" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 19.1 COL 7
     "Job Color Label" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.24 COL 6
     "[Set Job Color based on <Due>]" VIEW-AS TEXT
          SIZE 31 BY .62 AT ROW 23.38 COL 3
          BGCOLOR 8 
     "<Start--Now--Due--End>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 9.1 COL 44
          FONT 1
     "<Start--End><Now><Due>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 11.48 COL 44
          FONT 1
     "Value" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1.24 COL 99
     "<Now><Due><Start--End>" VIEW-AS TEXT
          SIZE 25 BY 1 AT ROW 6.71 COL 44
          FONT 1
     "BG   FG" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 27
          FONT 1
     "Reports:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 21 COL 12
     "Downtime Conflict Color" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 19.1 COL 86
     "<Unavailable>" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 1.95 COL 44
          FONT 1
     RECT-7 AT ROW 18.62 COL 2
     customBGColor-1 AT ROW 1.95 COL 110
     customBGColor-10 AT ROW 12.67 COL 110
     customBGColor-11 AT ROW 13.86 COL 110
     customBGColor-12 AT ROW 15.05 COL 110
     customBGColor-13 AT ROW 16.24 COL 110
     customBGColor-14 AT ROW 17.43 COL 110
     customBGColor-2 AT ROW 3.14 COL 110
     customBGColor-3 AT ROW 4.33 COL 110
     customBGColor-4 AT ROW 5.52 COL 110
     customBGColor-5 AT ROW 6.71 COL 110
     customBGColor-6 AT ROW 7.91 COL 110
     customBGColor-7 AT ROW 9.1 COL 110
     customBGColor-8 AT ROW 10.29 COL 110
     customBGColor-9 AT ROW 11.48 COL 110
     downtimeConflictBGColorValue AT ROW 18.86 COL 110
     jobBGColor-1 AT ROW 1.95 COL 25
     jobBGColor-10 AT ROW 12.67 COL 25
     jobBGColor-11 AT ROW 13.86 COL 25
     jobBGColor-12 AT ROW 15.05 COL 25
     jobBGColor-13 AT ROW 16.24 COL 25
     jobBGColor-14 AT ROW 17.43 COL 25
     jobBGColor-2 AT ROW 3.14 COL 25
     jobBGColor-3 AT ROW 4.33 COL 25
     jobBGColor-4 AT ROW 5.52 COL 25
     jobBGColor-5 AT ROW 6.71 COL 25
     jobBGColor-6 AT ROW 7.91 COL 25
     jobBGColor-7 AT ROW 9.1 COL 25
     jobBGColor-8 AT ROW 10.29 COL 25
     jobBGColor-9 AT ROW 11.48 COL 25
     jobConflictBGColorValue AT ROW 18.86 COL 25
     jobFGColor-1 AT ROW 1.95 COL 32
     jobFGColor-2 AT ROW 3.14 COL 32
     jobFGColor-3 AT ROW 4.33 COL 32
     jobFGColor-4 AT ROW 5.52 COL 32
     jobFGColor-5 AT ROW 6.71 COL 32
     jobFGColor-6 AT ROW 7.91 COL 32
     jobFGColor-7 AT ROW 9.1 COL 32
     jobFGColor-8 AT ROW 10.29 COL 32
     jobFGColor-9 AT ROW 11.48 COL 32
     jobFGColor-10 AT ROW 12.67 COL 32
     jobFGColor-11 AT ROW 13.86 COL 32
     jobFGColor-12 AT ROW 15.05 COL 32
     jobFGColor-13 AT ROW 16.24 COL 32
     jobFGColor-14 AT ROW 17.43 COL 32
     jobConflictFGColorValue AT ROW 18.86 COL 32
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME colorsFrame
     downtimeConflictFGColorValue AT ROW 18.86 COL 117
     customFGColor-1 AT ROW 1.95 COL 117
     customFGColor-2 AT ROW 3.14 COL 117
     customFGColor-3 AT ROW 4.33 COL 117
     customFGColor-4 AT ROW 5.52 COL 117
     customFGColor-5 AT ROW 6.71 COL 117
     customFGColor-6 AT ROW 7.91 COL 117
     customFGColor-7 AT ROW 9.1 COL 117
     customFGColor-8 AT ROW 10.29 COL 117
     customFGColor-9 AT ROW 11.48 COL 117
     customFGColor-10 AT ROW 12.67 COL 117
     customFGColor-11 AT ROW 13.86 COL 117
     customFGColor-12 AT ROW 15.05 COL 117
     customFGColor-13 AT ROW 16.24 COL 117
     customFGColor-14 AT ROW 17.43 COL 117
     RECT-11 AT ROW 20.52 COL 82
     RECT-12 AT ROW 20.52 COL 2
     RECT-13 AT ROW 23.62 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24
         TITLE "Color, Label & Priority Settings".

DEFINE FRAME defaultsFrame
     intervals AT ROW 1.95 COL 10 COLON-ALIGNED HELP
          "Select Startup Interval Value"
     lockButtonValue AT ROW 1.95 COL 47 COLON-ALIGNED HELP
          "Select Startup Lock Button Images Value"
     threeDValue AT ROW 1.95 COL 91 NO-LABEL
     timeValue AT ROW 3.14 COL 10 COLON-ALIGNED HELP
          "Select Startup Time Value"
     autoSizeValue AT ROW 3.14 COL 26 HELP
          "Select to Force Scheduler to Optimize Job Bar Height and Gap"
     noteButtonValue AT ROW 3.14 COL 47 COLON-ALIGNED HELP
          "Select Startup Note Button Images Value"
     threeDLeftValue AT ROW 3.86 COL 91
     threeDTopValue AT ROW 3.86 COL 113
     hpixelsValue AT ROW 4.33 COL 47 COLON-ALIGNED HELP
          "Select Startup Job Bar Height Value"
     gapValue AT ROW 4.33 COL 67 COLON-ALIGNED HELP
          "Select Startup Gap  Between Job Bar Value"
     rectJobBox AT ROW 5.05 COL 92 NO-LABEL
     rectJobNoBox AT ROW 5.14 COL 92.4 NO-LABEL
     showStatusValue AT ROW 5.52 COL 49
     allResourcesValue AT ROW 6.48 COL 49 HELP
          "Click to Show All Resources"
     threeDBottomValue AT ROW 6.48 COL 91
     threeDRightValue AT ROW 6.48 COL 113
     downtimeTopValue AT ROW 7.43 COL 91
     alphaSortValue AT ROW 7.67 COL 49 HELP
          "Click to Alpha Sort Resources"
     downtimeSizeValue AT ROW 8.38 COL 111 COLON-ALIGNED HELP
          "Select Startup Gap  Between Job Bar Value"
     flashGridLineValue AT ROW 8.86 COL 49 HELP
          "Click to Show Grid Line Flashing"
     completedHideValue AT ROW 9.57 COL 91
     flashTimeLineValue AT ROW 10.05 COL 49 HELP
          "Click to Show Time Line Flashing"
     fullBoardValue AT ROW 11.24 COL 49
     saveIntervalValue AT ROW 11.48 COL 1 COLON-ALIGNED HELP
          "Enter Auto Save Interval Minutes" NO-LABEL
     priority1Value AT ROW 11.48 COL 103 NO-LABEL
     popupBottomValue AT ROW 12.43 COL 49
     priority2Value AT ROW 12.43 COL 103 NO-LABEL
     viewRefreshValue AT ROW 12.67 COL 1 COLON-ALIGNED HELP
          "Enter Auto View Refresh Interval Minutes" NO-LABEL
     priority3Value AT ROW 13.38 COL 103 NO-LABEL
     moveUndoRedoValue AT ROW 13.62 COL 49
     btnDetailBoard AT ROW 13.86 COL 3 HELP
          "Tune Detail Window Display On/Off"
     detailWindowValue AT ROW 13.86 COL 9
     btnShowDowntime AT ROW 15.05 COL 3
     showDowntimeValue AT ROW 15.05 COL 9
     IDList AT ROW 15.29 COL 50 NO-LABEL
     jobBlockValue AT ROW 15.76 COL 94 HELP
          "Select to Block Job Conflicts"
     jobWarningValue AT ROW 15.76 COL 104 HELP
          "Select to Issue Warning of Job Conflicts"
     jobPromptValue AT ROW 15.76 COL 117 HELP
          "Select to Block Job Conflicts"
     btnFlashLight AT ROW 16.24 COL 3 HELP
          "Turn Highlight On/Off"
     flashLightValue AT ROW 16.24 COL 9
     btnBoardDatePrompt AT ROW 17.43 COL 3 HELP
          "Turn Date Prompt Popup On/Off"
     boardDatePromptValue AT ROW 17.43 COL 9
     downtimeBlockValue AT ROW 17.67 COL 94 HELP
          "Select to Block Downtime Conflicts"
     downtimeWarningValue AT ROW 17.67 COL 104 HELP
          "Select to Issue Warning of Downtime Conflicts"
     downtimePromptValue AT ROW 17.67 COL 117 HELP
          "Select to Block Job Conflicts"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME defaultsFrame
     packOptionValue AT ROW 18.86 COL 10 COLON-ALIGNED
     pendingOverValue AT ROW 19.33 COL 120 COLON-ALIGNED HELP
          "Enter Number of Days to Set Job Ending Date Prior to Due Date"
     packOptionPromptValue AT ROW 20.05 COL 30 HELP
          "Select to Prompt Pack Options"
     pendingLastDayValue AT ROW 20.52 COL 71 COLON-ALIGNED HELP
          "Enter Number of Days from Current Date for Setting Ending Date"
     pendingDaysValue AT ROW 20.52 COL 120 COLON-ALIGNED HELP
          "Enter Number of Days to Set Job Ending Date Prior to Due Date"
     btnDatePrompt AT ROW 21.48 COL 3 HELP
          "Turn Date Prompt Popup On/Off"
     datePromptValue AT ROW 21.48 COL 9
     btnDetailResource AT ROW 22.67 COL 3 HELP
          "Tune Detail Window Display On/Off"
     resourceJobDetailValue AT ROW 22.67 COL 9
     dontShowStartup AT ROW 22.67 COL 54 HELP
          "Select to Not Show Startup Configurations"
     boardSize AT ROW 22.67 COL 109 COLON-ALIGNED HELP
          "Select Board Size Resolution"
     resourceBrowseActionValue AT ROW 23.86 COL 40 COLON-ALIGNED HELP
          "Set Behavior for Resource Browser Default Action"
     loadCapacityValue AT ROW 23.86 COL 54
     endDateBufferValue AT ROW 23.86 COL 120 COLON-ALIGNED HELP
          "Enter Number of Days Past Last Job Date to Buffer"
     rectDTOver AT ROW 4.81 COL 99 COLON-ALIGNED NO-LABEL
     "Auto Save Interval Minutes (Zero=Off)" VIEW-AS TEXT
          SIZE 38 BY .62 AT ROW 11.71 COL 10
     "[Board Settings]" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 3
          BGCOLOR 8 
     "[Pending Jobs]" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 18.86 COL 54
          BGCOLOR 8 
     "[Resource Popup Settings]" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 20.76 COL 3
          BGCOLOR 8 
     "Auto View Refresh Interval (Zero=Off)" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 12.91 COL 10
     "(if changed scheduler restart required)" VIEW-AS TEXT
          SIZE 36 BY .62 AT ROW 2.91 COL 91
          FGCOLOR 12 
     "Time Line Color" VIEW-AS TEXT
          SIZE 15.4 BY .62 AT ROW 8.14 COL 10
     "[Job Appearance]" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.24 COL 91
          BGCOLOR 8 
     "Resource Grid Background Color" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 5.76 COL 10
     "[Conflict Settings]" VIEW-AS TEXT
          SIZE 16.8 BY .62 AT ROW 14.33 COL 91
          BGCOLOR 8 
     "Job Selection Highlight Color" VIEW-AS TEXT
          SIZE 27.6 BY .62 AT ROW 10.52 COL 10
     "Job Conflict Settings" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 15.05 COL 91
     "[Valid Schedule Board ID's]" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 14.57 COL 50
          BGCOLOR 8 
     "[Scheduler Re-Start Required to take effect]" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 21.95 COL 54
          BGCOLOR 8 
     "Job Seq.:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 12.43 COL 92
     "Resource:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 11.48 COL 91
     "Grid Line Color" VIEW-AS TEXT
          SIZE 14.4 BY .62 AT ROW 6.95 COL 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME defaultsFrame
     "Downtime Conflict Settings" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 16.95 COL 91
     "Board Background Color" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 4.57 COL 10
     "[Priority Settings]" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 10.76 COL 91
          BGCOLOR 8 
     "Job on Resource Highlight Color" VIEW-AS TEXT
          SIZE 31 BY .62 AT ROW 9.33 COL 10
     "Res. Seq.:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 13.38 COL 91
     RECT-3 AT ROW 21 COL 2
     RECT-1 AT ROW 1.48 COL 2
     rectGrid AT ROW 4.81 COL 91
     RECT-5 AT ROW 1.48 COL 90
     RECT-2 AT ROW 14.57 COL 90
     rectTop AT ROW 5.05 COL 92
     RECT-4 AT ROW 22.19 COL 53
     rectRight AT ROW 5.05 COL 126
     rectDTUnder AT ROW 4.81 COL 101
     rectBottom AT ROW 6.1 COL 92.4
     rectLeft AT ROW 5.05 COL 92
     flashLightColorValue AT ROW 10.29 COL 3
     gridBGColorValue AT ROW 4.33 COL 3
     gridLineColorValue AT ROW 6.71 COL 3
     lightBulbColorValue AT ROW 9.1 COL 3
     lockImage AT ROW 2.19 COL 78
     resourceBGColorValue AT ROW 5.52 COL 3
     timeLineColorValue AT ROW 7.91 COL 3
     RECT-8 AT ROW 19.1 COL 53
     RECT-9 AT ROW 11 COL 90
     RECT-10 AT ROW 14.81 COL 49
     noteImage-1 AT ROW 3.38 COL 78
     noteImage-2 AT ROW 3.38 COL 81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 1.24
         SIZE 129 BY 25.24
         TITLE "Default Settings".

DEFINE FRAME colorDisplayFrame
     "No Color" VIEW-AS TEXT
          SIZE 9 BY .52 AT ROW 19.33 COL 5
     colorChoice-0 AT ROW 1.24 COL 2
     colorChoice-1 AT ROW 2.43 COL 2
     colorChoice-10 AT ROW 3.62 COL 10
     colorChoice-11 AT ROW 4.81 COL 10
     colorChoice-12 AT ROW 6 COL 10
     colorChoice-13 AT ROW 7.19 COL 10
     colorChoice-14 AT ROW 8.38 COL 10
     colorChoice-15 AT ROW 1.24 COL 10
     colorChoice-30 AT ROW 19.1 COL 2
     colorChoice-2 AT ROW 3.62 COL 2
     colorChoice-3 AT ROW 4.81 COL 2
     colorChoice-4 AT ROW 6 COL 2
     colorChoice-5 AT ROW 7.19 COL 2
     colorChoice-6 AT ROW 8.38 COL 2
     colorChoice-7 AT ROW 9.57 COL 2
     colorChoice-8 AT ROW 9.57 COL 10
     colorChoice-9 AT ROW 2.43 COL 10
     colorChoice-16 AT ROW 10.76 COL 2
     colorChoice-17 AT ROW 11.95 COL 2
     colorChoice-18 AT ROW 13.14 COL 2
     colorChoice-19 AT ROW 14.33 COL 2
     colorChoice-20 AT ROW 15.52 COL 2
     colorChoice-21 AT ROW 16.71 COL 2
     colorChoice-22 AT ROW 17.91 COL 2
     colorChoice-23 AT ROW 10.76 COL 10
     colorChoice-24 AT ROW 11.95 COL 10
     colorChoice-25 AT ROW 13.14 COL 10
     colorChoice-26 AT ROW 14.33 COL 10
     colorChoice-27 AT ROW 15.52 COL 10
     colorChoice-28 AT ROW 16.71 COL 10
     colorChoice-29 AT ROW 17.91 COL 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.1
         SIZE 17 BY 20.24
         TITLE "Colors".

DEFINE FRAME fieldsFrame
     userField71 AT ROW 1.24 COL 2
     userField72 AT ROW 2.19 COL 2
     userField73 AT ROW 3.14 COL 2
     userField74 AT ROW 4.1 COL 2
     userField75 AT ROW 5.05 COL 2
     userField76 AT ROW 6 COL 2
     userField77 AT ROW 6.95 COL 2
     userField24 AT ROW 7.91 COL 2
     userField19 AT ROW 8.86 COL 2
     userField03 AT ROW 9.81 COL 2
     userField26 AT ROW 10.76 COL 2
     userField04 AT ROW 11.71 COL 2
     userField27 AT ROW 12.67 COL 2
     userField34 AT ROW 13.62 COL 2
     userField22 AT ROW 14.57 COL 2
     userField79 AT ROW 15.52 COL 2
     userField21 AT ROW 16.48 COL 2
     userField25 AT ROW 17.43 COL 2
     userField14 AT ROW 18.38 COL 2
     userField28 AT ROW 19.33 COL 2
     userField89 AT ROW 20.29 COL 2
     userField69 AT ROW 21.24 COL 2
     userField64 AT ROW 22.19 COL 2
     userField63 AT ROW 23.14 COL 2
     userField01 AT ROW 24.1 COL 2
     userField02 AT ROW 1.24 COL 34
     userField90 AT ROW 2.19 COL 34
     userField12 AT ROW 3.14 COL 34
     userField05 AT ROW 4.1 COL 34
     userField80 AT ROW 5.05 COL 34
     userField85 AT ROW 6 COL 34
     userField92 AT ROW 6.95 COL 34
     userField66 AT ROW 7.91 COL 34
     userField68 AT ROW 8.86 COL 34
     userField08 AT ROW 9.81 COL 34
     userField81 AT ROW 10.76 COL 34
     userField91 AT ROW 11.71 COL 34
     userField18 AT ROW 12.67 COL 34
     userField58 AT ROW 13.62 COL 34
     userField59 AT ROW 14.57 COL 34
     userField41 AT ROW 15.52 COL 34
     userField42 AT ROW 16.48 COL 34
     userField43 AT ROW 17.43 COL 34
     userField44 AT ROW 18.38 COL 34
     userField45 AT ROW 19.33 COL 34
     userField46 AT ROW 20.29 COL 34
     userField47 AT ROW 21.24 COL 34
     userField48 AT ROW 22.19 COL 34
     userField49 AT ROW 23.14 COL 34
     userField50 AT ROW 24.1 COL 34
     userField65 AT ROW 1.24 COL 66
     userField67 AT ROW 2.19 COL 66
     userField09 AT ROW 3.14 COL 66
     userField10 AT ROW 4.1 COL 66
     userField32 AT ROW 5.05 COL 66
     userField82 AT ROW 6 COL 66
     userField30 AT ROW 6.95 COL 66
     userField61 AT ROW 7.91 COL 66
     userField93 AT ROW 8.86 COL 66
     userField62 AT ROW 9.81 COL 66
     userField94 AT ROW 10.76 COL 66
     userField96 AT ROW 11.71 COL 66
     userField78 AT ROW 12.67 COL 66
     userField87 AT ROW 13.62 COL 66
     userField60 AT ROW 14.57 COL 66
     userField56 AT ROW 15.52 COL 66
     userField55 AT ROW 16.48 COL 66
     userField51 AT ROW 17.43 COL 66
     userField20 AT ROW 18.38 COL 66
     userField06 AT ROW 19.33 COL 66
     userField07 AT ROW 20.29 COL 66
     userField16 AT ROW 21.24 COL 66
     userField35 AT ROW 22.19 COL 66
     userField57 AT ROW 23.14 COL 66
     userField52 AT ROW 24.1 COL 66
     userField38 AT ROW 1.24 COL 98
     userField37 AT ROW 2.19 COL 98
     userField95 AT ROW 3.14 COL 98
     userField54 AT ROW 4.1 COL 98
     userField15 AT ROW 5.05 COL 98
     userField97 AT ROW 6 COL 98
     userField36 AT ROW 6.95 COL 98
     useNotes AT ROW 7.91 COL 98
     userField23 AT ROW 8.86 COL 98
     userField39 AT ROW 9.81 COL 98
     userField40 AT ROW 10.76 COL 98
     userField83 AT ROW 11.71 COL 98
     userField88 AT ROW 12.67 COL 98
     useStatus AT ROW 13.62 COL 98
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 1.24
         SIZE 129 BY 25.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fieldsFrame
     userField13 AT ROW 14.57 COL 98
     userField53 AT ROW 15.52 COL 98
     userField29 AT ROW 16.48 COL 98
     userField33 AT ROW 17.43 COL 98
     userField86 AT ROW 18.38 COL 98
     userField84 AT ROW 19.33 COL 98
     userField31 AT ROW 20.29 COL 98
     userField70 AT ROW 21.24 COL 98
     userField17 AT ROW 22.19 COL 98
     userField11 AT ROW 23.14 COL 98
     userField98 AT ROW 24.1 COL 98
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 1.24
         SIZE 129 BY 25
         TITLE "Fields".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Add Fields to: Neither
   Design Page: 1
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
         HEIGHT             = 26.57
         WIDTH              = 152.8.
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
/* REPARENT FRAME */
ASSIGN FRAME colorDisplayFrame:FRAME = FRAME configurationFrame:HANDLE
       FRAME colorsFrame:FRAME = FRAME configurationFrame:HANDLE
       FRAME defaultsFrame:FRAME = FRAME configurationFrame:HANDLE
       FRAME fieldsFrame:FRAME = FRAME configurationFrame:HANDLE.

/* SETTINGS FOR FRAME colorDisplayFrame
                                                                        */
/* SETTINGS FOR RECTANGLE colorChoice-0 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-1 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-10 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-11 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-12 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-13 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-14 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-15 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-16 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-17 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-18 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-19 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-2 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-20 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-21 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-22 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-23 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-24 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-25 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-26 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-27 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-28 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-29 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-3 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-30 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-4 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-5 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-6 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-7 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-8 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR RECTANGLE colorChoice-9 IN FRAME colorDisplayFrame
   4                                                                    */
/* SETTINGS FOR FRAME colorsFrame
                                                                        */
/* SETTINGS FOR COMBO-BOX colorPriorityValue-1 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-1:PRIVATE-DATA IN FRAME colorsFrame     = 
                "1".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-10 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-10:PRIVATE-DATA IN FRAME colorsFrame     = 
                "10".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-11 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-11:PRIVATE-DATA IN FRAME colorsFrame     = 
                "11".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-12 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-12:PRIVATE-DATA IN FRAME colorsFrame     = 
                "12".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-13 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-13:PRIVATE-DATA IN FRAME colorsFrame     = 
                "13".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-14 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-14:PRIVATE-DATA IN FRAME colorsFrame     = 
                "14".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-15 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-15:PRIVATE-DATA IN FRAME colorsFrame     = 
                "15".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-16 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-16:PRIVATE-DATA IN FRAME colorsFrame     = 
                "16".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-17 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-17:PRIVATE-DATA IN FRAME colorsFrame     = 
                "17".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-18 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-18:PRIVATE-DATA IN FRAME colorsFrame     = 
                "18".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-19 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-19:PRIVATE-DATA IN FRAME colorsFrame     = 
                "19".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-2 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-2:PRIVATE-DATA IN FRAME colorsFrame     = 
                "2".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-20 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-20:PRIVATE-DATA IN FRAME colorsFrame     = 
                "20".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-21 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-21:PRIVATE-DATA IN FRAME colorsFrame     = 
                "21".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-22 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-22:PRIVATE-DATA IN FRAME colorsFrame     = 
                "22".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-23 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-23:PRIVATE-DATA IN FRAME colorsFrame     = 
                "23".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-24 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-24:PRIVATE-DATA IN FRAME colorsFrame     = 
                "24".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-25 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-25:PRIVATE-DATA IN FRAME colorsFrame     = 
                "25".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-26 IN FRAME colorsFrame
   1 6                                                                  */
ASSIGN 
       colorPriorityValue-26:PRIVATE-DATA IN FRAME colorsFrame     = 
                "26".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-27 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-27:PRIVATE-DATA IN FRAME colorsFrame     = 
                "27".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-28 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-28:PRIVATE-DATA IN FRAME colorsFrame     = 
                "28".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-3 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-3:PRIVATE-DATA IN FRAME colorsFrame     = 
                "3".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-4 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-4:PRIVATE-DATA IN FRAME colorsFrame     = 
                "4".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-5 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-5:PRIVATE-DATA IN FRAME colorsFrame     = 
                "5".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-6 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-6:PRIVATE-DATA IN FRAME colorsFrame     = 
                "6".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-7 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-7:PRIVATE-DATA IN FRAME colorsFrame     = 
                "7".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-8 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-8:PRIVATE-DATA IN FRAME colorsFrame     = 
                "8".

/* SETTINGS FOR COMBO-BOX colorPriorityValue-9 IN FRAME colorsFrame
   1 5                                                                  */
ASSIGN 
       colorPriorityValue-9:PRIVATE-DATA IN FRAME colorsFrame     = 
                "9".

/* SETTINGS FOR TOGGLE-BOX completedCheckoffValue IN FRAME colorsFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE customBGColor-1 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-1:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-10 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-10:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-11 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-11:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-12 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-12:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-13 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-13:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-14 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-14:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-2 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-2:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-3 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-3:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-4 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-4:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-5 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-5:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-6 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-6:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-7 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-7:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-8 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-8:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customBGColor-9 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customBGColor-9:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR TOGGLE-BOX customCheckoffValue IN FRAME colorsFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE customFGColor-1 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-1:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-10 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-10:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-11 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-11:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-12 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-12:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-13 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-13:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-14 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-14:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-2 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-2:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-3 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-3:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-4 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-4:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-5 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-5:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-6 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-6:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-7 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-7:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-8 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-8:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE customFGColor-9 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       customFGColor-9:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR FILL-IN customLabel-1 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-10 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-11 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-12 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-13 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-14 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-2 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-3 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-4 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-5 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-6 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-7 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-8 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR FILL-IN customLabel-9 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-1 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-10 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-11 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-12 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-13 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-14 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-2 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-3 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-4 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-5 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-6 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-7 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-8 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR COMBO-BOX customValue-9 IN FRAME colorsFrame
   1 6                                                                  */
/* SETTINGS FOR RECTANGLE downtimeConflictBGColorValue IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       downtimeConflictBGColorValue:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE downtimeConflictFGColorValue IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       downtimeConflictFGColorValue:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RADIO-SET dueDateUsedValue IN FRAME colorsFrame
   1                                                                    */
/* SETTINGS FOR RECTANGLE jobBGColor-1 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-1:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-10 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-10:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-11 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-11:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-12 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-12:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-13 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-13:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-14 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-14:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-2 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-2:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-3 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-3:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-4 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-4:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-5 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-5:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-6 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-6:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-7 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-7:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-8 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-8:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobBGColor-9 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobBGColor-9:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobConflictBGColorValue IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobConflictBGColorValue:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobConflictFGColorValue IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobConflictFGColorValue:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-1 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-1:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-10 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-10:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-11 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-11:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-12 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-12:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-13 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-13:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-14 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-14:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-2 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-2:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-3 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-3:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-4 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-4:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-5 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-5:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-6 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-6:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-7 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-7:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-8 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-8:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE jobFGColor-9 IN FRAME colorsFrame
   5                                                                    */
ASSIGN 
       jobFGColor-9:SELECTABLE IN FRAME colorsFrame       = TRUE.

/* SETTINGS FOR FILL-IN jobLabel-1 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-10 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-11 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-12 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-13 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-14 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-2 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-3 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-4 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-5 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-6 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-7 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-8 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR FILL-IN jobLabel-9 IN FRAME colorsFrame
   1 5                                                                  */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME colorsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-12 IN FRAME colorsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-13 IN FRAME colorsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME colorsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET reloadReportValue IN FRAME colorsFrame
   1                                                                    */
/* SETTINGS FOR RADIO-SET reloadStatusValue IN FRAME colorsFrame
   1                                                                    */
/* SETTINGS FOR FRAME configurationFrame
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME configurationFrame:SCROLLABLE       = FALSE
       FRAME configurationFrame:HIDDEN           = TRUE.

ASSIGN 
       btnChangeColor-16:PRIVATE-DATA IN FRAME configurationFrame     = 
                "16".

ASSIGN 
       btnChangeColor-17:PRIVATE-DATA IN FRAME configurationFrame     = 
                "17".

ASSIGN 
       btnChangeColor-18:PRIVATE-DATA IN FRAME configurationFrame     = 
                "18".

ASSIGN 
       btnChangeColor-19:PRIVATE-DATA IN FRAME configurationFrame     = 
                "19".

ASSIGN 
       btnChangeColor-20:PRIVATE-DATA IN FRAME configurationFrame     = 
                "20".

ASSIGN 
       btnChangeColor-21:PRIVATE-DATA IN FRAME configurationFrame     = 
                "21".

ASSIGN 
       btnChangeColor-22:PRIVATE-DATA IN FRAME configurationFrame     = 
                "22".

ASSIGN 
       btnChangeColor-23:PRIVATE-DATA IN FRAME configurationFrame     = 
                "23".

ASSIGN 
       btnChangeColor-24:PRIVATE-DATA IN FRAME configurationFrame     = 
                "24".

ASSIGN 
       btnChangeColor-25:PRIVATE-DATA IN FRAME configurationFrame     = 
                "25".

ASSIGN 
       btnChangeColor-26:PRIVATE-DATA IN FRAME configurationFrame     = 
                "26".

ASSIGN 
       btnChangeColor-27:PRIVATE-DATA IN FRAME configurationFrame     = 
                "27".

ASSIGN 
       btnChangeColor-28:PRIVATE-DATA IN FRAME configurationFrame     = 
                "28".

ASSIGN 
       btnChangeColor-29:PRIVATE-DATA IN FRAME configurationFrame     = 
                "29".

/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME configurationFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME defaultsFrame
                                                                        */
/* SETTINGS FOR TOGGLE-BOX allResourcesValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX alphaSortValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX autoSizeValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX boardDatePromptValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX boardSize IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR BUTTON btnBoardDatePrompt IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       btnBoardDatePrompt:PRIVATE-DATA IN FRAME defaultsFrame     = 
                "Turn Date Prompt Popup".

/* SETTINGS FOR BUTTON btnDatePrompt IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       btnDatePrompt:PRIVATE-DATA IN FRAME defaultsFrame     = 
                "Turn Date Prompt Popup".

/* SETTINGS FOR BUTTON btnDetailBoard IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       btnDetailBoard:PRIVATE-DATA IN FRAME defaultsFrame     = 
                "Turn Detail Window Display".

/* SETTINGS FOR BUTTON btnDetailResource IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       btnDetailResource:PRIVATE-DATA IN FRAME defaultsFrame     = 
                "Turn Detail Window Display".

/* SETTINGS FOR BUTTON btnFlashLight IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       btnFlashLight:PRIVATE-DATA IN FRAME defaultsFrame     = 
                "Turn Highlight".

/* SETTINGS FOR BUTTON btnShowDowntime IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       btnShowDowntime:PRIVATE-DATA IN FRAME defaultsFrame     = 
                " Downtime".

/* SETTINGS FOR TOGGLE-BOX completedHideValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX datePromptValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX detailWindowValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX dontShowStartup IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX downtimeBlockValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX downtimePromptValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX downtimeSizeValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX downtimeTopValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX downtimeWarningValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN endDateBufferValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX flashGridLineValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE flashLightColorValue IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       flashLightColorValue:SELECTABLE IN FRAME defaultsFrame       = TRUE.

/* SETTINGS FOR TOGGLE-BOX flashLightValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX flashTimeLineValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX fullBoardValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX gapValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE gridBGColorValue IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       gridBGColorValue:SELECTABLE IN FRAME defaultsFrame       = TRUE.

/* SETTINGS FOR RECTANGLE gridLineColorValue IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       gridLineColorValue:SELECTABLE IN FRAME defaultsFrame       = TRUE.

/* SETTINGS FOR COMBO-BOX hpixelsValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR SELECTION-LIST IDList IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX intervals IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX jobBlockValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX jobPromptValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX jobWarningValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE lightBulbColorValue IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       lightBulbColorValue:SELECTABLE IN FRAME defaultsFrame       = TRUE.

/* SETTINGS FOR TOGGLE-BOX loadCapacityValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX lockButtonValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX moveUndoRedoValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX noteButtonValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX packOptionPromptValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR COMBO-BOX packOptionValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN pendingDaysValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN pendingLastDayValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN pendingOverValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX popupBottomValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RADIO-SET priority1Value IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RADIO-SET priority2Value IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RADIO-SET priority3Value IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectBottom IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN rectDTOver IN FRAME defaultsFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR RECTANGLE rectDTUnder IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectGrid IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR rectJobBox IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR rectJobNoBox IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectLeft IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectRight IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rectTop IN FRAME defaultsFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE resourceBGColorValue IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       resourceBGColorValue:SELECTABLE IN FRAME defaultsFrame       = TRUE.

/* SETTINGS FOR FILL-IN resourceBrowseActionValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX resourceJobDetailValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN saveIntervalValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX showDowntimeValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX showStatusValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX threeDBottomValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX threeDLeftValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX threeDRightValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX threeDTopValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RADIO-SET threeDValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR RECTANGLE timeLineColorValue IN FRAME defaultsFrame
   3                                                                    */
ASSIGN 
       timeLineColorValue:SELECTABLE IN FRAME defaultsFrame       = TRUE.

/* SETTINGS FOR COMBO-BOX timeValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FILL-IN viewRefreshValue IN FRAME defaultsFrame
   2                                                                    */
/* SETTINGS FOR FRAME fieldsFrame
   L-To-R,COLUMNS                                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME colorDisplayFrame
/* Query rebuild information for FRAME colorDisplayFrame
     _Query            is NOT OPENED
*/  /* FRAME colorDisplayFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME colorsFrame
/* Query rebuild information for FRAME colorsFrame
     _Query            is NOT OPENED
*/  /* FRAME colorsFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME configurationFrame
/* Query rebuild information for FRAME configurationFrame
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME configurationFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME defaultsFrame
/* Query rebuild information for FRAME defaultsFrame
     _Query            is NOT OPENED
*/  /* FRAME defaultsFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fieldsFrame
/* Query rebuild information for FRAME fieldsFrame
     _Query            is NOT OPENED
*/  /* FRAME fieldsFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME boardDatePromptValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boardDatePromptValue sObject
ON VALUE-CHANGED OF boardDatePromptValue IN FRAME defaultsFrame /* Date  Time Prompt on Drop  Drag */
DO:
  APPLY 'CHOOSE' TO btnBoardDatePrompt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBoardDatePrompt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBoardDatePrompt sObject
ON CHOOSE OF btnBoardDatePrompt IN FRAME defaultsFrame
DO:
  ASSIGN
    boardDatePromptValue = NOT boardDatePromptValue
    boardDatePromptValue:SCREEN-VALUE = STRING(boardDatePromptValue)
    ldummy = SELF:LOAD-IMAGE('{&images}/date' + (IF boardDatePromptValue THEN 'On'
             ELSE 'Off') + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME configurationFrame
&Scoped-define SELF-NAME btnChangeColor-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChangeColor-16 sObject
ON CHOOSE OF btnChangeColor-16 IN FRAME configurationFrame
,btnChangeColor-17,btnChangeColor-18,btnChangeColor-19,btnChangeColor-20
,btnChangeColor-21,btnChangeColor-22,btnChangeColor-23,btnChangeColor-24
,btnChangeColor-25,btnChangeColor-26,btnChangeColor-27,btnChangeColor-28
,btnChangeColor-29
DO:
  DEFINE VARIABLE newColor AS INTEGER NO-UNDO.

  newColor = INTEGER(SELF:PRIVATE-DATA).
  SYSTEM-DIALOG COLOR newColor.
  SELF:BGCOLOR = INTEGER(SELF:PRIVATE-DATA).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME btnDatePrompt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDatePrompt sObject
ON CHOOSE OF btnDatePrompt IN FRAME defaultsFrame
DO:
  ASSIGN
    datePromptValue = NOT datePromptValue
    datePromptValue:SCREEN-VALUE = STRING(datePromptValue)
    ldummy = SELF:LOAD-IMAGE('{&images}/date' + (IF datePromptValue THEN 'On'
             ELSE 'Off') + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetailBoard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetailBoard sObject
ON CHOOSE OF btnDetailBoard IN FRAME defaultsFrame
DO:
  ASSIGN
    detailWindowValue = NOT detailWindowValue
    detailWindowValue:SCREEN-VALUE = STRING(detailWindowValue)
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT detailWindowValue,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/detailWin' +
                             TRIM(STRING(detailWindowValue,'On/Off')) + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDetailResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDetailResource sObject
ON CHOOSE OF btnDetailResource IN FRAME defaultsFrame
DO:
  ASSIGN
    resourceJobDetailValue = NOT resourceJobDetailValue
    resourceJobDetailValue:SCREEN-VALUE = STRING(resourceJobDetailValue)
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT resourceJobDetailValue,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/detailWin' +
                             TRIM(STRING(resourceJobDetailValue,'On/Off')) + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFlashLight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFlashLight sObject
ON CHOOSE OF btnFlashLight IN FRAME defaultsFrame
DO:
  ASSIGN
    flashLightValue = NOT flashLightValue
    flashLightValue:SCREEN-VALUE = STRING(flashLightValue)
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT flashLightValue,'On/Off')
    ldummy = SELF:LOAD-IMAGE('{&images}/lightBulb' +
                             TRIM(STRING(flashLightValue,'On/Off')) + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME btnHTMLPageLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTMLPageLocation sObject
ON CHOOSE OF btnHTMLPageLocation IN FRAME colorsFrame /* Set HTML Page Location */
DO:
    RUN {&prompts}\htmlPageLoc.w (INPUT-OUTPUT htmlPageLocation).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME configurationFrame
&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore sObject
ON CHOOSE OF btnRestore IN FRAME configurationFrame
DO:
  RUN getConfiguration.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave sObject
ON CHOOSE OF btnSave IN FRAME configurationFrame
DO:
  /* changes to config.dat, update getConfiguration in board.w,
     local-create-objects in detail.w & getConfigValues in this-procedure */
  IF boardType EQ 'View' THEN
  MESSAGE 'This Feature Not Available' SKIP
          'in Scheduler' boardType VIEW-AS ALERT-BOX.
  ELSE DO:
    DO WITH FRAME defaultsFrame:
      ASSIGN {&defaultsFrameFields}.
      ASSIGN
        allResources = allResourcesValue
        alphaSort = alphaSortValue
        autoSize = autoSizeValue
        boardDatePrompt = boardDatePromptValue
        completedHide = completedHideValue
        datePrompt = datePromptValue
        detailWindow = detailWindowValue
        downtimeBlock = downtimeBlockValue
        downtimePrompt = downtimePromptValue
        downtimeSize = downtimeSizeValue
        downtimeTop = downtimeTopValue
        downtimeWarning = downtimeWarningValue
        dueDateUsed = dueDateUsedValue
        endDateBuffer = endDateBufferValue
        flashGridLine = flashGridLineValue
        flashLightColor = flashLightColorValue:BGCOLOR
        flashLight = flashLightValue
        flashTimeLine = flashTimeLineValue
        fullBoard = fullBoardValue
        gap = gapValue
        gridBGColor = gridBGColorValue:BGCOLOR
        gridLineColor = gridLineColorValue:BGCOLOR
        hpixels = hpixelsValue
        intervalInit = intervals:LOOKUP(intervals:SCREEN-VALUE)
        jobBlock = jobBlockValue
        jobPrompt = jobPromptValue
        jobWarning = jobWarningValue
        lightBulbColor = lightBulbColorValue:BGCOLOR
        lockButtons = lockButtonValue:LOOKUP(lockButtonValue:SCREEN-VALUE)
        loadCapacity = loadCapacityValue
        moveUndoRedo = moveUndoRedoValue
        noteButtons = noteButtonValue:LOOKUP(noteButtonValue:SCREEN-VALUE)
        packOption = packOptionValue
        packOptionPrompt = packOptionPromptValue
        pendingDays = pendingDaysValue
        pendingLastDay = pendingLastDayValue
        pendingOver = pendingOverValue
        priority1 = priority1Value
        priority2 = priority2Value
        priority3 = priority3Value
        popupBottom = popupBottomValue
        reloadReport = reloadReportValue
        reloadStatus = reloadStatusValue
        resourceBGColor = resourceBGColorValue:BGCOLOR
        resourceBrowseAction = resourceBrowseActionValue
        resourceJobDetail = resourceJobDetailValue
        saveInterval = saveIntervalValue
        showDowntime = showDowntimeValue
        showStatus = showStatusValue
        threeD = threeDValue
        threeDBottom = threeDBottomValue
        threeDLeft = threeDLeftValue
        threeDRight = threeDRightValue
        threeDTop = threeDTopValue
        timeInit = timeValue
        timeLineColor = timeLineColorValue:BGCOLOR
        viewRefresh = viewRefreshValue.
      OUTPUT TO VALUE(clientDat + '{&data}/validID.dat').
      DO idx = 1 TO IDList:NUM-ITEMS:
        IF IDList:IS-SELECTED(idx) THEN
        EXPORT IDList:ENTRY(idx).
      END.
      OUTPUT CLOSE.
    END. /* defaultsframe */
    DO WITH FRAME colorsFrame:
      ASSIGN {&colorsFrameFields}.
      ASSIGN
        {{&includes}/putConfig.i 1 15}
        {{&includes}/putConfig.i 2 16}
        {{&includes}/putConfig.i 3 17}
        {{&includes}/putConfig.i 4 18}
        {{&includes}/putConfig.i 5 19}
        {{&includes}/putConfig.i 6 20}
        {{&includes}/putConfig.i 7 21}
        jobConflictBGColor = jobConflictBGColorValue:BGCOLOR
        jobConflictFGColor = jobConflictFGColorValue:BGCOLOR.
      ASSIGN
        {{&includes}/putConfig.i 8 22}
        {{&includes}/putConfig.i 9 23}
        {{&includes}/putConfig.i 10 24}
        {{&includes}/putConfig.i 11 25}
        {{&includes}/putConfig.i 12 26}
        {{&includes}/putConfig.i 13 27}
        {{&includes}/putConfig.i 14 28}
        completedCheckoff = completedCheckOffValue
        customCheckoff = customCheckOffValue
        downtimeConflictBGColor = downtimeConflictBGColorValue:BGCOLOR
        downtimeConflictFGColor = downtimeConflictFGColorValue:BGCOLOR.
    END. /* colorsframe */
    DO WITH FRAME fieldsFrame:
      ASSIGN {&userFields}.
      OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/userFields.dat')).
      ASSIGN
        pWidget = FRAME fieldsFrame:HANDLE
        pWidget = pWidget:FIRST-CHILD
        pWidget = pWidget:FIRST-CHILD.
      DO WHILE pWidget NE ?:
        IF pWidget:TYPE EQ 'TOGGLE-BOX' THEN DO:
          IF pWidget:SCREEN-VALUE EQ 'Yes' THEN
          EXPORT pWidget:NAME pWidget:LABEL.
        END. /* toggle-box */
        pWidget = pWidget:NEXT-SIBLING.
      END.
      OUTPUT CLOSE.
    END. /* fieldsFrame */
    RUN saveColors.
    RUN put{&version}.
    OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/startUp.dat')).
    EXPORT dontShowStartup boardSize.
    OUTPUT CLOSE.
    RUN loadConfiguration IN containerHandle.
  END. /* else */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME btnShowDowntime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowDowntime sObject
ON CHOOSE OF btnShowDowntime IN FRAME defaultsFrame
DO:
  ASSIGN
    showDowntimeValue = NOT showDowntimeValue
    showDowntimeValue:SCREEN-VALUE = STRING(showDowntimeValue)
    SELF:TOOLTIP = STRING(NOT showDowntimeValue,'Show/Hide') + SELF:PRIVATE-DATA
    ldummy = SELF:LOAD-IMAGE('{&images}/downtime' +
                             TRIM(STRING(showDowntimeValue,'On/Off')) + '.bmp').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorDisplayFrame
&Scoped-define SELF-NAME colorChoice-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorChoice-0 sObject
ON MOUSE-SELECT-CLICK OF colorChoice-0 IN FRAME colorDisplayFrame
,colorChoice-1,colorChoice-2,colorChoice-3,colorChoice-4,colorChoice-5
,colorChoice-6,colorChoice-7,colorChoice-8,colorChoice-9,colorChoice-10
,colorChoice-11,colorChoice-12,colorChoice-13,colorChoice-14,colorChoice-15
,colorChoice-16,colorChoice-17,colorChoice-18,colorChoice-19,colorChoice-20
,colorChoice-21,colorChoice-22,colorChoice-23,colorChoice-24,colorChoice-25
,colorChoice-26,colorChoice-27,colorChoice-28,colorChoice-29,colorChoice-30
DO:
  IF VALID-HANDLE(colorWidget) THEN
  ASSIGN
    colorWidget:BGCOLOR = SELF:BGCOLOR
    colorWidget:FILLED = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME colorPriorityValue-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL colorPriorityValue-1 sObject
ON VALUE-CHANGED OF colorPriorityValue-1 IN FRAME colorsFrame
,colorPriorityValue-2,colorPriorityValue-3,colorPriorityValue-4
,colorPriorityValue-5,colorPriorityValue-6,colorPriorityValue-7
,colorPriorityValue-8,colorPriorityValue-9,colorPriorityValue-10
,colorPriorityValue-11,colorPriorityValue-12,colorPriorityValue-13
,colorPriorityValue-14,colorPriorityValue-15,colorPriorityValue-16
,colorPriorityValue-17,colorPriorityValue-18,colorPriorityValue-19
,colorPriorityValue-20,colorPriorityValue-21,colorPriorityValue-22
,colorPriorityValue-23,colorPriorityValue-24,colorPriorityValue-25
,colorPriorityValue-26,colorPriorityValue-27,colorPriorityValue-28
DO:
  RUN setPriority (INTEGER(SELF:PRIVATE-DATA),INTEGER(SELF:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME completedCheckoffValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL completedCheckoffValue sObject
ON VALUE-CHANGED OF completedCheckoffValue IN FRAME colorsFrame /* Apply Completed Checkoff to Whole Job */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME configurationFrame
&Scoped-define SELF-NAME configFrameSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL configFrameSelection sObject
ON VALUE-CHANGED OF configFrameSelection IN FRAME configurationFrame
DO:
  ASSIGN {&SELF-NAME}
    colorWidget = ?.
  CASE {&SELF-NAME}:
    WHEN 'Color' THEN DO:
      HIDE FRAME defaultsFrame NO-PAUSE.
      HIDE FRAME fieldsFrame NO-PAUSE.
      VIEW {&btnChangeColor} IN FRAME {&FRAME-NAME}.
      VIEW FRAME colorDisplayFrame.
      VIEW FRAME colorsFrame.
    END.
    WHEN 'Default' THEN DO:
      HIDE FRAME colorsFrame NO-PAUSE.
      HIDE FRAME fieldsFrame NO-PAUSE.
      VIEW {&btnChangeColor} IN FRAME {&FRAME-NAME}.
      VIEW FRAME colorDisplayFrame.
      VIEW FRAME defaultsFrame.
    END.
    WHEN 'Fields' THEN DO:
      HIDE FRAME colorsFrame NO-PAUSE.
      HIDE FRAME colorDisplayFrame NO-PAUSE.
      HIDE FRAME defaultsFrame NO-PAUSE.
      HIDE {&btnChangeColor} IN FRAME {&FRAME-NAME}.
      VIEW FRAME fieldsFrame.
      ENABLE {&userFields} WITH FRAME fieldsFrame.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME customCheckoffValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customCheckoffValue sObject
ON VALUE-CHANGED OF customCheckoffValue IN FRAME colorsFrame /* Apply Custom Value Checkoff to Whole Job */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-1 sObject
ON ENTRY OF customValue-1 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-1 sObject
ON VALUE-CHANGED OF customValue-1 IN FRAME colorsFrame
,customValue-2,customValue-3,customValue-4,customValue-5
,customValue-6,customValue-7,customValue-8,customValue-9
,customValue-10,customValue-11,customValue-12
,customValue-13,customValue-14
DO:
  RUN checkCustomValue (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-10 sObject
ON ENTRY OF customValue-10 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-11 sObject
ON ENTRY OF customValue-11 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-12 sObject
ON ENTRY OF customValue-12 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-13 sObject
ON ENTRY OF customValue-13 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-14 sObject
ON ENTRY OF customValue-14 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-2 sObject
ON ENTRY OF customValue-2 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-3 sObject
ON ENTRY OF customValue-3 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-4 sObject
ON ENTRY OF customValue-4 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-5 sObject
ON ENTRY OF customValue-5 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-6 sObject
ON ENTRY OF customValue-6 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-7 sObject
ON ENTRY OF customValue-7 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-8 sObject
ON ENTRY OF customValue-8 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME customValue-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL customValue-9 sObject
ON ENTRY OF customValue-9 IN FRAME colorsFrame
DO:
    ASSIGN
        {&SELF-NAME}
        SELF:PRIVATE-DATA = {&SELF-NAME}
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME datePromptValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL datePromptValue sObject
ON VALUE-CHANGED OF datePromptValue IN FRAME defaultsFrame /* Date  Time Prompt on Update */
DO:
  APPLY 'CHOOSE' TO btnDatePrompt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME detailWindowValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL detailWindowValue sObject
ON VALUE-CHANGED OF detailWindowValue IN FRAME defaultsFrame /* Show Job  Resource Popup */
DO:
  APPLY 'CHOOSE' TO btnDetailBoard.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dontShowStartup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dontShowStartup sObject
ON VALUE-CHANGED OF dontShowStartup IN FRAME defaultsFrame /* Hide Startup Configuration */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME downtimeBlockValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL downtimeBlockValue sObject
ON VALUE-CHANGED OF downtimeBlockValue IN FRAME defaultsFrame /* Block */
DO:
  {{&includes}/{&Board}/conflict.i downtimeWarningValue downtimePromptValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME downtimePromptValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL downtimePromptValue sObject
ON VALUE-CHANGED OF downtimePromptValue IN FRAME defaultsFrame /* Prompt */
DO:
  {{&includes}/{&Board}/conflict.i downtimeBlockValue downtimeWarningValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME downtimeTopValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL downtimeTopValue sObject
ON VALUE-CHANGED OF downtimeTopValue IN FRAME defaultsFrame /* Show Downtime on Top of Job */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} THEN
  ASSIGN
    rectDTUnder:HIDDEN = YES
    rectDTOver:HIDDEN = NO
    ldummy = rectDTOver:MOVE-TO-TOP().
  ELSE
  ASSIGN
    rectDTUnder:HIDDEN = NO
    ldummy = rectJob:MOVE-TO-TOP()
    ldummy = rectBottom:MOVE-TO-TOP()
    ldummy = rectLeft:MOVE-TO-TOP()
    ldummy = rectRight:MOVE-TO-TOP()
    ldummy = rectTop:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME downtimeWarningValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL downtimeWarningValue sObject
ON VALUE-CHANGED OF downtimeWarningValue IN FRAME defaultsFrame /* Warning */
DO:
  {{&includes}/{&Board}/conflict.i downtimeBlockValue downtimePromptValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME dueDateUsedValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dueDateUsedValue sObject
ON VALUE-CHANGED OF dueDateUsedValue IN FRAME colorsFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME flashLightColorValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flashLightColorValue sObject
ON SELECTION OF flashLightColorValue IN FRAME defaultsFrame
,gridBGColorValue,lightBulbColorValue,resourceBGColorValue
,timeLineColorValue,gridLineColorValue
DO:
  colorWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME flashLightValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL flashLightValue sObject
ON VALUE-CHANGED OF flashLightValue IN FRAME defaultsFrame /* Flashlight Initial Setting */
DO:
  APPLY 'CHOOSE' TO btnFlashLight.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME jobBGColor-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobBGColor-1 sObject
ON MOUSE-SELECT-CLICK OF jobBGColor-1 IN FRAME colorsFrame
,jobBGColor-2,jobBGColor-3,jobBGColor-4,jobBGColor-5,jobBGColor-6
,jobBGColor-7,jobBGColor-8,jobBGColor-9,jobBGColor-10,jobBGColor-11
,jobBGColor-12,jobBGColor-13,jobBGColor-14
,customBGColor-1,customBGColor-2,customBGColor-3,customBGColor-4
,customBGColor-5,customBGColor-6,customBGColor-7,customBGColor-8
,customBGColor-9,customBGColor-10,customBGColor-11,customBGColor-12
,customBGColor-13,customBGColor-14
,downtimeConflictBGColorValue,jobConflictBGColorValue
DO:
  SELF:BGCOLOR = IF SELF:BGCOLOR EQ 29 THEN 0 ELSE SELF:BGCOLOR + 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobBGColor-1 sObject
ON SELECTION OF jobBGColor-1 IN FRAME colorsFrame
,jobBGColor-2,jobBGColor-3,jobBGColor-4,jobBGColor-5,jobBGColor-6
,jobBGColor-7,jobBGColor-8,jobBGColor-9,jobBGColor-10,jobBGColor-11
,jobBGColor-12,jobBGColor-13,jobBGColor-14
,customBGColor-1,customBGColor-2,customBGColor-3,customBGColor-4
,customBGColor-5,customBGColor-6,customBGColor-7,customBGColor-8
,customBGColor-9,customBGColor-10,customBGColor-11,customBGColor-12
,customBGColor-13,customBGColor-14
,downtimeConflictBGColorValue,jobConflictBGColorValue
DO:
  colorWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME jobBlockValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobBlockValue sObject
ON VALUE-CHANGED OF jobBlockValue IN FRAME defaultsFrame /* Block */
DO:
  {{&includes}/{&Board}/conflict.i jobWarningValue jobPromptValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME jobFGColor-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobFGColor-1 sObject
ON MOUSE-SELECT-CLICK OF jobFGColor-1 IN FRAME colorsFrame
,jobFGColor-2,jobFGColor-3,jobFGColor-4,jobFGColor-5,jobFGColor-6
,jobFGColor-7,jobFGColor-8,jobFGColor-9,jobFGColor-10,jobFGColor-11
,jobFGColor-12,jobFGColor-13,jobFGColor-14
,customFGColor-1,customFGColor-2,customFGColor-3,customFGColor-4
,customFGColor-5,customFGColor-6,customFGColor-7,customFGColor-8
,customFGColor-9,customFGColor-10,customFGColor-11,customFGColor-12
,customFGColor-13,customFGColor-14
,downtimeConflictFGColorValue,jobConflictFGColorValue
DO:
  SELF:BGCOLOR = IF SELF:BGCOLOR EQ 0 THEN 15 ELSE 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobFGColor-1 sObject
ON SELECTION OF jobFGColor-1 IN FRAME colorsFrame
,jobFGColor-2,jobFGColor-3,jobFGColor-4,jobFGColor-5,jobFGColor-6
,jobFGColor-7,jobFGColor-8,jobFGColor-9,jobFGColor-10,jobFGColor-11
,jobFGColor-12,jobFGColor-13,jobFGColor-14
,customFGColor-1,customFGColor-2,customFGColor-3,customFGColor-4
,customFGColor-5,customFGColor-6,customFGColor-7,customFGColor-8
,customFGColor-9,customFGColor-10,customFGColor-11,customFGColor-12
,customFGColor-13,customFGColor-14
,downtimeConflictFGColorValue,jobConflictFGColorValue
DO:
  colorWidget = SELF:HANDLE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME jobPromptValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobPromptValue sObject
ON VALUE-CHANGED OF jobPromptValue IN FRAME defaultsFrame /* Prompt */
DO:
  {{&includes}/{&Board}/conflict.i jobBlockValue jobWarningValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME jobWarningValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jobWarningValue sObject
ON VALUE-CHANGED OF jobWarningValue IN FRAME defaultsFrame /* Warning */
DO:
  {{&includes}/{&Board}/conflict.i jobBlockValue jobPromptValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME priority1Value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL priority1Value sObject
ON VALUE-CHANGED OF priority1Value IN FRAME defaultsFrame
DO:
  ASSIGN {&SELF-NAME}.
  RUN setPriorityValues (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME priority2Value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL priority2Value sObject
ON VALUE-CHANGED OF priority2Value IN FRAME defaultsFrame
DO:
  ASSIGN {&SELF-NAME}.
  RUN setPriorityValues (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME priority3Value
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL priority3Value sObject
ON VALUE-CHANGED OF priority3Value IN FRAME defaultsFrame
DO:
  ASSIGN {&SELF-NAME}.
  RUN setPriorityValues (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME colorsFrame
&Scoped-define SELF-NAME reloadReportValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reloadReportValue sObject
ON VALUE-CHANGED OF reloadReportValue IN FRAME colorsFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reloadStatusValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reloadStatusValue sObject
ON VALUE-CHANGED OF reloadStatusValue IN FRAME colorsFrame
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME defaultsFrame
&Scoped-define SELF-NAME resourceJobDetailValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resourceJobDetailValue sObject
ON VALUE-CHANGED OF resourceJobDetailValue IN FRAME defaultsFrame /* Show Job Popup */
DO:
  APPLY 'CHOOSE' TO btnDetailResource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME showDowntimeValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showDowntimeValue sObject
ON VALUE-CHANGED OF showDowntimeValue IN FRAME defaultsFrame /* Show Downtime */
DO:
  APPLY 'CHOOSE' TO btnShowDowntime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME threeDBottomValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL threeDBottomValue sObject
ON VALUE-CHANGED OF threeDBottomValue IN FRAME defaultsFrame /* Bottom Edge */
DO:
  ASSIGN
    {&SELF-NAME}
    rectBottom:HIDDEN = NOT {&SELF-NAME}
    ldummy = rectBottom:MOVE-TO-TOP().
  APPLY 'VALUE-CHANGED' TO downtimeTopValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME threeDLeftValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL threeDLeftValue sObject
ON VALUE-CHANGED OF threeDLeftValue IN FRAME defaultsFrame /* Left Edge */
DO:
  ASSIGN
    {&SELF-NAME}
    rectLeft:HIDDEN = NOT {&SELF-NAME}
    ldummy = rectLeft:MOVE-TO-TOP().
  APPLY 'VALUE-CHANGED' TO downtimeTopValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME threeDRightValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL threeDRightValue sObject
ON VALUE-CHANGED OF threeDRightValue IN FRAME defaultsFrame /* Right Edge */
DO:
  ASSIGN
    {&SELF-NAME}
    rectRight:HIDDEN = NOT {&SELF-NAME}
    ldummy = rectRight:MOVE-TO-TOP().
  APPLY 'VALUE-CHANGED' TO downtimeTopValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME threeDTopValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL threeDTopValue sObject
ON VALUE-CHANGED OF threeDTopValue IN FRAME defaultsFrame /* Top Edge */
DO:
  ASSIGN
    {&SELF-NAME}
    rectTop:HIDDEN = NOT {&SELF-NAME}
    ldummy = rectTop:MOVE-TO-TOP().
  APPLY 'VALUE-CHANGED' TO downtimeTopValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME threeDValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL threeDValue sObject
ON VALUE-CHANGED OF threeDValue IN FRAME defaultsFrame
DO:
  DEFINE VARIABLE lvSelection AS LOGICAL NO-UNDO.
  ASSIGN
    {&SELF-NAME}
    lvSelection = {&SELF-NAME} EQ YES
    threeDBottomValue = lvSelection
    threeDBottomValue:SCREEN-VALUE = STRING(lvSelection)
    threeDBottomValue:SENSITIVE = lvSelection
    threeDLeftValue = lvSelection
    threeDLeftValue:SCREEN-VALUE = STRING(lvSelection)
    threeDLeftValue:SENSITIVE = lvSelection
    threeDRightValue = lvSelection
    threeDRightValue:SCREEN-VALUE = STRING(lvSelection)
    threeDRightValue:SENSITIVE = lvSelection
    threeDTopValue = lvSelection
    threeDTopValue:SCREEN-VALUE = STRING(lvSelection)
    threeDTopValue:SENSITIVE = lvSelection
    rectJobBox:HIDDEN = {&SELF-NAME} NE ?
    rectJobNoBox:HIDDEN = NOT rectJobBox:HIDDEN
    rectJob = IF rectJobBox:HIDDEN THEN rectJobNoBox:HANDLE
              ELSE rectJobBox:HANDLE.
  APPLY 'VALUE-CHANGED' TO threeDBottomValue.
  APPLY 'VALUE-CHANGED' TO threeDLeftValue.
  APPLY 'VALUE-CHANGED' TO threeDRightValue.
  APPLY 'VALUE-CHANGED' TO threeDTopValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME configurationFrame
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkCustomValue sObject 
PROCEDURE checkCustomValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iphCustomValue AS HANDLE NO-UNDO.

  DO WITH FRAME colorsFrame:
      IF fCheckCustomValue(iphCustomValue,customValue-1:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-2:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-3:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-4:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-5:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-6:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-7:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-8:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-9:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-10:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-11:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-12:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-13:HANDLE) OR
         fCheckCustomValue(iphCustomValue,customValue-14:HANDLE) THEN DO:
          MESSAGE "Selected Value Already Used"
              VIEW-AS ALERT-BOX TITLE "Duplicate Value".
          iphCustomValue:SCREEN-VALUE = iphCustomValue:PRIVATE-DATA.
          APPLY "ENTRY":U TO iphCustomValue.
      END. /* if fcheckcustomvalue() */
  END. /* do  */

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
  IF boardType EQ 'View' THEN
  DO:
    DISABLE {&colorsFrameFields} WITH FRAME colorsFrame.
    DISABLE {&defaultsFrameFields} {&defaultsFrameButtons} WITH FRAME defaultsFrame.
    DISABLE {&configButtons} WITH FRAME colorDisplayFrame.
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
  HIDE FRAME colorDisplayFrame.
  HIDE FRAME colorsFrame.
  HIDE FRAME configurationFrame.
  HIDE FRAME defaultsFrame.
  HIDE FRAME fieldsFrame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getConfiguration sObject 
PROCEDURE getConfiguration :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE customValueList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE custVal AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE ulColor AS INTEGER NO-UNDO.
  
  INPUT FROM VALUE(SEARCH('{&data}/statusCheckOffs.dat')) NO-ECHO.
  REPEAT:
    IMPORT DELIMITER '~t' custVal.
    customValueList = customValueList + ',' + custVal.
  END. /* repeat */
  INPUT CLOSE.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/startUp.dat')) NO-ECHO.
  IMPORT dontShowStartup boardSize.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
  DO WITH FRAME defaultsFrame:
    ASSIGN
      gapValue:LIST-ITEMS = ''
      hpixelsValue:LIST-ITEMS = ''
      timeValue:LIST-ITEMS = ''.
    DO i = 0 TO 2300 BY 100:
      timeValue:ADD-LAST(STRING(i,'9999')).
    END.
    DO i = 14 TO 99:
      hpixelsValue:ADD-LAST(STRING(i)).
    END.
    DO i = 0 TO 9:
      gapValue:ADD-LAST(STRING(i)).
    END.
    ASSIGN
      allResourcesValue:SCREEN-VALUE = STRING(allResources)
      alphaSortValue:SCREEN-VALUE = STRING(alphaSort)
      autoSizeValue:SCREEN-VALUE = STRING(autoSize)
      boardDatePromptValue = NOT boardDatePrompt
      boardDatePromptValue:SCREEN-VALUE = STRING(boardDatePrompt)
      completedHideValue:SCREEN-VALUE = STRING(completedHide)
      datePromptValue = NOT datePrompt
      datePromptValue:SCREEN-VALUE = STRING(datePrompt)
      detailWindowValue = NOT detailWindow
      detailWindowValue:SCREEN-VALUE = STRING(detailWindow)
      downtimeBlockValue:SCREEN-VALUE = STRING(downtimeBlock)
      downtimePromptValue:SCREEN-VALUE = STRING(downtimePrompt)
      downtimeSizeValue:SCREEN-VALUE = STRING(downtimeSize)
      downtimeTopValue:SCREEN-VALUE = STRING(downtimeTop)
      downtimeWarningValue:SCREEN-VALUE = STRING(downtimeWarning)
      endDateBufferValue:SCREEN-VALUE = STRING(endDateBuffer)
      flashGridLineValue:SCREEN-VALUE = STRING(flashGridLine)
      flashLightColorValue:BGCOLOR = flashLightColor
      flashLightColorValue:FILLED = flashLightColor NE ?
      flashLightValue = NOT flashLight
      flashLightValue:SCREEN-VALUE = STRING(flashLight)
      flashTimeLineValue:SCREEN-VALUE = STRING(flashTimeLine)
      fullBoardValue:SCREEN-VALUE = STRING(fullBoard)
      gapValue:INNER-LINES = gapValue:NUM-ITEMS
      gapValue:SCREEN-VALUE = STRING(gap)
      gridBGColorValue:BGCOLOR = gridBGColor
      gridBGColorValue:FILLED = gridBGColor NE ?
      gridLineColorValue:BGCOLOR = gridLineColor
      hpixelsValue:INNER-LINES = hpixelsValue:NUM-ITEMS
      hpixelsValue:SCREEN-VALUE = STRING(hpixels)
      .
    ASSIGN
      intervals:LIST-ITEMS = '{{&includes}/intervals.i}'
      intervals:INNER-LINES = intervals:NUM-ITEMS
      intervals:SCREEN-VALUE = intervals:ENTRY(intervalInit)
      jobBlockValue:SCREEN-VALUE = STRING(jobBlock)
      jobPromptValue:SCREEN-VALUE = STRING(jobPrompt)
      jobWarningValue:SCREEN-VALUE = STRING(jobWarning)
      lightBulbColorValue:BGCOLOR = lightBulbColor
      lightBulbColorValue:FILLED = lightBulbColor NE ?
      lockButtonValue:LIST-ITEMS = '{{&includes}/intervals.i}'
      lockButtonValue:SCREEN-VALUE = lockButtonValue:ENTRY(lockButtons)
      loadCapacityValue:SCREEN-VALUE = STRING(loadCapacity)
      moveUndoRedoValue:SCREEN-VALUE = STRING(moveUndoRedo)
      noteButtonValue:LIST-ITEMS = '{{&includes}/intervals.i}'
      noteButtonValue:SCREEN-VALUE = noteButtonValue:ENTRY(noteButtons)
      packOptionValue:SCREEN-VALUE = STRING(packOption)
      packOptionPromptValue:SCREEN-VALUE = STRING(packOptionPrompt)
      pendingDaysValue:SCREEN-VALUE = STRING(pendingDays)
      pendingLastDayValue:SCREEN-VALUE = STRING(pendingLastDay)
      pendingOverValue:SCREEN-VALUE = STRING(pendingOver)
      popupBottomValue:SCREEN-VALUE = STRING(popupBottom)
      priority1Value:SCREEN-VALUE = STRING(priority1)
      priority2Value:SCREEN-VALUE = STRING(priority2)
      priority3Value:SCREEN-VALUE = STRING(priority3)
      resourceBGColorValue:BGCOLOR = resourceBGColor
      resourceBGColorValue:FILLED = resourceBGColor NE ?
      resourceBrowseActionValue:SCREEN-VALUE = STRING(resourceBrowseAction)
      resourceJobDetailValue = NOT resourceJobDetail
      resourceJobDetailValue:SCREEN-VALUE = STRING(resourceJobDetail)
      saveIntervalValue:SCREEN-VALUE = STRING(saveInterval)
      showDowntimeValue = NOT showDowntime
      showDowntimeValue:SCREEN-VALUE = STRING(showDowntime)
      showStatusValue:SCREEN-VALUE = STRING(showStatus)
      threeDValue:SCREEN-VALUE = STRING(threeD)
      threeDBottomValue:SCREEN-VALUE = STRING(threeDBottom)
      threeDLeftValue:SCREEN-VALUE = STRING(threeDLeft)
      threeDRightValue:SCREEN-VALUE = STRING(threeDRight)
      threeDTopValue:SCREEN-VALUE = STRING(threeDTop)
      timeLineColorValue:BGCOLOR = timeLineColor
      timeValue:INNER-LINES = timeValue:NUM-ITEMS
      timeValue:SCREEN-VALUE = STRING(timeInit)
      viewRefreshValue:SCREEN-VALUE = STRING(viewRefresh)
      .
    rectGrid:MOVE-TO-BOTTOM().
    DISPLAY dontShowStartup boardSize.
    ENABLE {&defaultsFrameFields} {&defaultsFrameButtons} dontShowStartup boardSize.
    IF jobBlock THEN APPLY 'VALUE-CHANGED' TO jobBlockValue.
    ELSE IF jobWarning THEN APPLY 'VALUE-CHANGED' TO jobWarningValue.
    ELSE IF jobPrompt THEN APPLY 'VALUE-CHANGED' TO jobPromptValue.
    IF downtimeBlock THEN APPLY 'VALUE-CHANGED' TO downtimeBlockValue.
    ELSE IF downtimeWarning THEN APPLY 'VALUE-CHANGED' TO downtimeWarningValue.
    ELSE IF downtimePrompt THEN APPLY 'VALUE-CHANGED' TO downtimePromptValue.
    APPLY 'VALUE-CHANGED' TO threeDValue.
    APPLY 'VALUE-CHANGED' TO threeDBottomValue.
    APPLY 'VALUE-CHANGED' TO threeDLeftValue.
    APPLY 'VALUE-CHANGED' TO threeDRightValue.
    APPLY 'VALUE-CHANGED' TO threeDTopValue.
    APPLY 'VALUE-CHANGED' TO downtimeTopValue.
    APPLY 'CHOOSE' TO btnBoardDatePrompt.
    APPLY 'CHOOSE' TO btnDetailBoard.
    APPLY 'CHOOSE' TO btnShowDowntime.
    APPLY 'CHOOSE' TO btnFlashLight.
    APPLY 'CHOOSE' TO btnDetailResource.
    APPLY 'CHOOSE' TO btnDatePrompt.
  END. /* defaultsframe */
  DO WITH FRAME colorsFrame:
    ASSIGN
      {{&includes}/getConfig.i 1 15}
      {{&includes}/getConfig.i 2 16}
      {{&includes}/getConfig.i 3 17}.
    ASSIGN
      {{&includes}/getConfig.i 4 18}
      {{&includes}/getConfig.i 5 19}
      {{&includes}/getConfig.i 6 20}.
    ASSIGN
      {{&includes}/getConfig.i 7 21}
      {{&includes}/getConfig.i 8 22}
      {{&includes}/getConfig.i 9 23}.
    ASSIGN
      {{&includes}/getConfig.i 10 24}
      {{&includes}/getConfig.i 11 25}
      {{&includes}/getConfig.i 12 26}.
    ASSIGN
      {{&includes}/getConfig.i 13 27}
      {{&includes}/getConfig.i 14 28}
      {{&includes}/getConfig.i 0 13}
      {{&includes}/getConfig.i 0 14}
      completedCheckOffValue:SCREEN-VALUE = STRING(completedCheckoff)
      customCheckOffValue:SCREEN-VALUE = STRING(customCheckoff)
      jobConflictBGColorValue:BGCOLOR = jobConflictBGColor
      jobConflictBGColorValue:FILLED = jobConflictBGColor NE ?
      jobConflictFGColorValue:BGCOLOR = IF jobConflictFGColor EQ ? THEN 0
                                        ELSE jobConflictFGColor
      downtimeConflictBGColorValue:BGCOLOR = downtimeConflictBGColor
      downtimeConflictBGColorValue:FILLED = downtimeConflictBGColor NE ?
      downtimeConflictFGColorValue:BGCOLOR = IF downtimeConflictFGColor EQ ? THEN 0
                                             ELSE downtimeConflictFGColor
      dueDateUsedValue:SCREEN-VALUE = STRING(dueDateUsed)
      reloadReportValue:SCREEN-VALUE = STRING(reloadReport)
      reloadStatusValue:SCREEN-VALUE = STRING(reloadStatus)
      .
    DISPLAY {&jobLabelFields} {&customValueFields}.
    ENABLE
      reloadReportValue reloadStatusValue dueDateUsedValue
      completedCheckOffValue customCheckOffValue {&jobLabelFields}
      btnHTMLPageLocation
      .
    IF customValueList EQ '' THEN
    DISABLE {&customValueFields}.
  END. /* colorsframe */
  RUN getUserFields.
  EMPTY TEMP-TABLE prioritySort.
  DO i = 1 TO EXTENT(colorPriority):
    CREATE prioritySort.
    ASSIGN
      prioritySort.priority = colorPriority[i]
      prioritySort.prioritySet = YES
      prioritySort.priorityIdx = i
      .
  END.
  ENABLE {&configButtons} WITH FRAME colorDisplayFrame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getID sObject 
PROCEDURE getID :
/*------------------------------------------------------------------------------
  Purpose:     get installation identifier dir names
  Parameters:  starting directory, return directory name
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/getID.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSize sObject 
PROCEDURE getSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opHeight AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opWidth AS INTEGER NO-UNDO.

  ASSIGN
    opHeight = FRAME {&FRAME-NAME}:HEIGHT-CHARS
    opWidth = FRAME {&FRAME-NAME}:WIDTH-CHARS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getUserFields sObject 
PROCEDURE getUserFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE fldID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fldLabel AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fldGroup AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fldName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE createFieldFile AS LOGICAL NO-UNDO.

  FOR EACH userFlds:
    DELETE userFlds.
  END.

  createFieldFile = SEARCH('{&data}/' + ID + '/userFields.dat') EQ ?.
  
  IF createFieldFile THEN
  OUTPUT TO VALUE('{&data}/' + ID + '/userFields.dat').

  INPUT FROM VALUE(SEARCH('{&data}/rptFields.dat')) NO-ECHO.
  REPEAT:
    fldGroup = ''.
    IMPORT fldID fldLabel fldName ^ ^ fldGroup.
    IF fldID NE ENTRY(1,ID,'~/') THEN NEXT.
    IF fldGroup NE '' THEN
    fldLabel = fldLabel + ' (' + fldGroup + ')'.
    CREATE userFlds.
    ASSIGN
      userFlds.fieldID = fldID
      userFlds.fieldLabel = fldLabel
      userFlds.fieldName = fldName.
    IF createFieldFile THEN EXPORT fldName fldLabel.
  END. /* repeat */
  INPUT CLOSE.

  DO WITH FRAME fieldsFrame:
    CREATE userFlds.
    ASSIGN
      userFlds.fieldLabel = useNotes:LABEL
      userFlds.fieldName = useNotes:NAME.
    CREATE userFlds.
    ASSIGN
      userFlds.fieldLabel = useStatus:LABEL
      userFlds.fieldName = useStatus:NAME.
  
    IF createFieldFile THEN DO:
      EXPORT useNotes:NAME useNotes:LABEL.
      EXPORT useStatus:NAME useStatus:LABEL.
      OUTPUT CLOSE.
    END. /* if createfieldfile */
  END. /* frame fieldsframe */

  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/userFields.dat')).
  REPEAT:
    IMPORT fldName fldLabel.
    FIND FIRST userFlds WHERE userFlds.fieldName EQ fldName NO-ERROR.
    IF AVAILABLE userFlds THEN
    userFlds.fieldUsed = YES.
  END. /* repeat */
  INPUT CLOSE.
  
  ASSIGN
    pWidget = FRAME fieldsFrame:HANDLE
    pWidget = pWidget:FIRST-CHILD
    pWidget = pWidget:FIRST-CHILD.
  DO WHILE pWidget NE ?:
    IF pWidget:TYPE EQ 'TOGGLE-BOX' THEN DO:
      FIND FIRST userFlds WHERE userFlds.fieldName EQ pWidget:NAME NO-ERROR.
      IF AVAILABLE userFlds THEN
      ASSIGN
        pWidget:LABEL = userFlds.fieldLabel
        pWidget:SCREEN-VALUE = STRING(userFlds.fieldUsed).
    END. /* toggle-box */
    pWidget = pWidget:NEXT-SIBLING.
  END. /* do while */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getValidID sObject 
PROCEDURE getValidID :
/*------------------------------------------------------------------------------
  Purpose:     highlight valid id's
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE validList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE validID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  IF SEARCH('{&data}/validID.dat') EQ ? THEN DO:
    OUTPUT TO VALUE(clientDat + '{&data}/validID.dat').
    DO i = 1 TO IDList:NUM-ITEMS IN FRAME defaultsFrame:
      EXPORT IDList:ENTRY(i).
    END.
    OUTPUT CLOSE.
  END.
  INPUT FROM VALUE(SEARCH(clientDat + '{&data}/validID.dat')) NO-ECHO.
  REPEAT:
    IMPORT validID.
    IF CAN-DO(IDList:LIST-ITEMS,validID) THEN
    validList = validList + (IF validList NE '' THEN ',' ELSE '') + validID.
  END.
  INPUT CLOSE.
  IDList:SCREEN-VALUE = validList.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize sObject 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE idValue AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN ID.
  startDir = clientDat + '{&data}'.
  RUN getID (startDir,OUTPUT dirList).
  DO i = 1 TO NUM-ENTRIES(dirList) WITH FRAME defaultsFrame:
    RUN getID (startDir + '/' + ENTRY(i,dirList),OUTPUT idValue).
    IF idValue EQ '' THEN
    IDList:ADD-LAST(ENTRY(i,dirList)).
    ELSE
    DO j = 1 TO NUM-ENTRIES(idValue):
      IDList:ADD-LAST(ENTRY(i,dirList) + '/' + ENTRY(j,idValue)).
    END.
  END.
  RUN getValidID.
  RUN getConfiguration.
  RUN setColorDynamic.
  RUN getUserFields.
  APPLY 'VALUE-CHANGED' TO configFrameSelection IN FRAME {&FRAME-NAME}.
  APPLY 'ENTRY' TO btnSave IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveColors sObject 
PROCEDURE saveColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE colorValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE redValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE greenValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE blueValue AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  OUTPUT TO VALUE(SEARCH('{&data}/sbColors.dat')).
  DO i = 16 TO 29:
    ASSIGN
      redValue = COLOR-TABLE:GET-RED-VALUE(i)
      greenValue = COLOR-TABLE:GET-GREEN-VALUE(i)
      blueValue = COLOR-TABLE:GET-BLUE-VALUE(i)
      colorValue = STRING(redValue) + ',' + STRING(greenValue) + ',' + STRING(blueValue).
    PUT UNFORMATTED colorValue SKIP.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setColorDynamic sObject 
PROCEDURE setColorDynamic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}\setColorDynamic.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPriority sObject 
PROCEDURE setPriority :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPriorityIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipNewPriority AS INTEGER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  FIND FIRST prioritySort
       WHERE prioritySort.priorityIdx EQ ipPriorityIdx.
  ASSIGN
    prioritySort.priority = ipNewPriority
    prioritySort.prioritySet = NO
    .
  FOR EACH prioritySort BY prioritySort.priority
      :
    ASSIGN
      i = i + 1
      priorityWidget[prioritySort.priorityIdx]:SCREEN-VALUE = STRING(i)
      .
  END.
  DO i = 1 TO EXTENT(priorityWidget):
    FIND prioritySort EXCLUSIVE-LOCK WHERE prioritySort.priorityIdx EQ i.
    ASSIGN
      prioritySort.priority = INTEGER(priorityWidget[i]:SCREEN-VALUE)
      prioritySort.prioritySet = YES
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPriorityValues sObject 
PROCEDURE setPriorityValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipPriority AS INTEGER NO-UNDO.

  CASE ipPriority:
    WHEN 1 THEN
      IF priority1Value EQ priority2Value THEN
      priority2Value = 6 - priority1Value - priority3Value.
      ELSE
      priority3Value = 6 - priority1Value - priority2Value.
    WHEN 2 THEN
      IF priority2Value EQ priority3Value THEN
      priority3Value = 6 - priority1Value - priority2Value.
      ELSE
      priority1Value = 6 - priority2Value - priority3Value.
    WHEN 3 THEN
      IF priority3Value EQ priority1Value THEN
      priority1Value = 6 - priority2Value - priority3Value.
      ELSE
      priority2Value = 6 - priority1Value - priority3Value.
  END CASE.
  DISPLAY priority1Value priority2Value priority3Value
    WITH FRAME defaultsFrame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCheckCustomValue sObject 
FUNCTION fCheckCustomValue RETURNS LOGICAL
  (iphCustomValue AS HANDLE, iphCheckValue AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN iphCustomValue NE iphCheckValue AND
         iphCustomValue:SCREEN-VALUE EQ iphCheckValue:SCREEN-VALUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

