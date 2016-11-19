&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-commcr.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 3.19.2016

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

&SCOPED-DEFINE useCustList
{aoa/includes/aoaParamVars.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svCompany svLocation svAllDept svStartDept ~
svEndDept svAllMachine svStartMachine svEndMachine svStartOpDate ~
btnCalendar-1 svStartOpDateOption svEndOpDate btnCalendar-2 ~
svEndOpDateOption svAllShift svStartShift svEndShift svCustList btnCustList ~
svAllCustNo svStartCustNo svEndCustNo svPrintByScheduledMachine ~
svRoundDecimals svSort 
&Scoped-Define DISPLAYED-OBJECTS svCompany svLocation svAllDept svStartDept ~
startDeptName svEndDept endDeptName svAllMachine svStartMachine ~
startMachineDescription svEndMachine endMachineDescription svStartOpDate ~
svStartOpDateOption svEndOpDate svEndOpDateOption svAllShift svStartShift ~
startShiftName svEndShift endShiftName svCustList svAllCustNo svStartCustNo ~
startCustName svEndCustNo endCustName svPrintByScheduledMachine ~
svRoundDecimals svSort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .95.

DEFINE VARIABLE svEndOpDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartOpDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE endDeptName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE endMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.

DEFINE VARIABLE endShiftName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE startCustName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE startDeptName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1.

DEFINE VARIABLE startMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1.

DEFINE VARIABLE startShiftName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "End Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndDept AS CHARACTER FORMAT "X(2)" 
     LABEL "End Department" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svEndMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "End Machine" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svEndOpDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndShift AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "End Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svLocation AS CHARACTER FORMAT "X(5)" 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE svStartCustNo AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Customer" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartDept AS CHARACTER FORMAT "X(2)" 
     LABEL "Start Department" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE svStartMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Machine" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svStartOpDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartShift AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Start Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svSort AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alphabetically", "Alphabetically",
"By Start Time", "By Start Time"
     SIZE 37.4 BY 1 NO-UNDO.

DEFINE VARIABLE svAllCustNo AS LOGICAL INITIAL yes 
     LABEL "All Customers" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE svAllDept AS LOGICAL INITIAL yes 
     LABEL "All Departments" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svAllMachine AS LOGICAL INITIAL yes 
     LABEL "All Machines" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svAllShift AS LOGICAL INITIAL yes 
     LABEL "All Shifts" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE svCustList AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .95 NO-UNDO.

DEFINE VARIABLE svPrintByScheduledMachine AS LOGICAL INITIAL no 
     LABEL "Print by Scheduled Machine?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE svRoundDecimals AS LOGICAL INITIAL no 
     LABEL "Round Decimals" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 23 COLON-ALIGNED WIDGET-ID 60
     svLocation AT ROW 1.24 COL 39 COLON-ALIGNED WIDGET-ID 130
     svAllDept AT ROW 2.91 COL 25 HELP
          "All Departments?" WIDGET-ID 108
     svStartDept AT ROW 4.1 COL 23 COLON-ALIGNED HELP
          "Enter Start Department" WIDGET-ID 112
     startDeptName AT ROW 4.1 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 106
     svEndDept AT ROW 5.29 COL 23 COLON-ALIGNED HELP
          "Enter End Department" WIDGET-ID 110
     endDeptName AT ROW 5.29 COL 32 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 104
     svAllMachine AT ROW 6.95 COL 25 HELP
          "All Macines?" WIDGET-ID 58
     svStartMachine AT ROW 8.14 COL 23 COLON-ALIGNED HELP
          "Enter Start Machine" WIDGET-ID 22
     startMachineDescription AT ROW 8.14 COL 37 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 18
     svEndMachine AT ROW 9.33 COL 23 COLON-ALIGNED HELP
          "Enter End Machine" WIDGET-ID 20
     endMachineDescription AT ROW 9.33 COL 37 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 16
     svStartOpDate AT ROW 11 COL 23 COLON-ALIGNED HELP
          "Enter Start Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 11 COL 41 WIDGET-ID 76
     svStartOpDateOption AT ROW 11 COL 44 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svEndOpDate AT ROW 12.19 COL 23 COLON-ALIGNED HELP
          "Enter End Date" WIDGET-ID 68
     btnCalendar-2 AT ROW 12.19 COL 41 WIDGET-ID 78
     svEndOpDateOption AT ROW 12.19 COL 44 COLON-ALIGNED HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svAllShift AT ROW 13.86 COL 25 HELP
          "All Shifts?" WIDGET-ID 118
     svStartShift AT ROW 15.05 COL 23 COLON-ALIGNED HELP
          "Enter Start Shift" WIDGET-ID 122
     startShiftName AT ROW 15.05 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 116
     svEndShift AT ROW 16.24 COL 23 COLON-ALIGNED HELP
          "Enter End Shift" WIDGET-ID 120
     endShiftName AT ROW 16.24 COL 28 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 114
     svCustList AT ROW 17.91 COL 25 WIDGET-ID 48
     btnCustList AT ROW 17.91 COL 55 WIDGET-ID 46
     svAllCustNo AT ROW 19.1 COL 25 HELP
          "All Customers?" WIDGET-ID 56
     svStartCustNo AT ROW 20.29 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 2
     startCustName AT ROW 20.29 COL 39 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 4
     svEndCustNo AT ROW 21.48 COL 23 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 6
     endCustName AT ROW 21.48 COL 39 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 8
     svPrintByScheduledMachine AT ROW 23.14 COL 25 WIDGET-ID 124
     svRoundDecimals AT ROW 24.33 COL 25 WIDGET-ID 126
     svSort AT ROW 25.76 COL 24.6 HELP
          "Select Sort Option" NO-LABEL WIDGET-ID 84
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 25.76 COL 15 WIDGET-ID 90
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.8 BY 27.1
         TITLE "Report Parameters".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
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
         HEIGHT             = 27.1
         WIDTH              = 85.8.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN endCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endDeptName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endShiftName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startCustName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startDeptName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startShiftName IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       svCompany:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       svLocation:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartOpDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndOpDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList sObject
ON CHOOSE OF btnCustList IN FRAME F-Main /* Preview */
DO:
    RUN sys/ref/CustListManager.w (svCompany, "AR15").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllCustNo sObject
ON VALUE-CHANGED OF svAllCustNo IN FRAME F-Main /* All Customers */
DO:
    {aoa/includes/svAllValueChanged.i svStartCustNo svEndCustNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllDept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllDept sObject
ON VALUE-CHANGED OF svAllDept IN FRAME F-Main /* All Departments */
DO:
    {aoa/includes/svAllValueChanged.i svStartDept svEndDept}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllMachine sObject
ON VALUE-CHANGED OF svAllMachine IN FRAME F-Main /* All Machines */
DO:
    {aoa/includes/svAllValueChanged.i svStartMachine svEndMachine}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllShift sObject
ON VALUE-CHANGED OF svAllShift IN FRAME F-Main /* All Shifts */
DO:
    {aoa/includes/svAllValueChanged.i svStartShift svEndShift}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllDept.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCustList sObject
ON VALUE-CHANGED OF svCustList IN FRAME F-Main /* Use Defined Customer List */
DO:
  ASSIGN {&SELF-NAME}
      svStartCustNo:READ-ONLY = {&SELF-NAME}
      svEndCustNo:READ-ONLY   = {&SELF-NAME}
      btnCustList:SENSITIVE   = {&SELF-NAME}
      .
  IF {&SELF-NAME} THEN
  ASSIGN svAllCustNo:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndCustNo sObject
ON LEAVE OF svEndCustNo IN FRAME F-Main /* End Customer */
DO:
    endCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDept sObject
ON LEAVE OF svEndDept IN FRAME F-Main /* End Department */
DO:
    endDeptName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndMachine sObject
ON LEAVE OF svEndMachine IN FRAME F-Main /* End Machine */
DO:
    endMachineDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOpDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOpDate sObject
ON HELP OF svEndOpDate IN FRAME F-Main /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndOpDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndOpDateOption sObject
ON VALUE-CHANGED OF svEndOpDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndOpDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndShift sObject
ON LEAVE OF svEndShift IN FRAME F-Main /* End Shift */
DO:
    endShiftName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLocation sObject
ON ENTRY OF svLocation IN FRAME F-Main /* Location */
DO:
  APPLY "ENTRY":U TO svAllDept.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svPrintByScheduledMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svPrintByScheduledMachine sObject
ON VALUE-CHANGED OF svPrintByScheduledMachine IN FRAME F-Main /* Print by Scheduled Machine? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svRoundDecimals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svRoundDecimals sObject
ON VALUE-CHANGED OF svRoundDecimals IN FRAME F-Main /* Round Decimals */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartCustNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartCustNo sObject
ON LEAVE OF svStartCustNo IN FRAME F-Main /* Start Customer */
DO:
    startCustName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDept sObject
ON LEAVE OF svStartDept IN FRAME F-Main /* Start Department */
DO:
    startDeptName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartMachine sObject
ON LEAVE OF svStartMachine IN FRAME F-Main /* Start Machine */
DO:
    startMachineDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOpDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOpDate sObject
ON HELP OF svStartOpDate IN FRAME F-Main /* Start Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartOpDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartOpDateOption sObject
ON VALUE-CHANGED OF svStartOpDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartOpDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartShift sObject
ON LEAVE OF svStartShift IN FRAME F-Main /* Start Shift */
DO:
    startShiftName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitialize sObject 
PROCEDURE pInitialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            hContainer = iphContainer
            svCompany:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetCompany' IN hContainer)
            svCompany
            svLocation:SCREEN-VALUE = DYNAMIC-FUNCTION('fGetLocation' IN hContainer)
            svLocation
            .

        APPLY "VALUE-CHANGED":U TO svAllDept.
        APPLY "LEAVE":U TO svStartDept.
        APPLY "LEAVE":U TO svEndDept.
        
        APPLY "VALUE-CHANGED":U TO svAllMachine.
        APPLY "LEAVE":U TO svStartMachine.
        APPLY "LEAVE":U TO svEndMachine.
        
        APPLY "VALUE-CHANGED":U TO svStartOpDateOption.
        APPLY "VALUE-CHANGED":U TO svEndOpDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllShift.
        APPLY "LEAVE":U TO svStartShift.
        APPLY "LEAVE":U TO svEndShift.
        
        APPLY "VALUE-CHANGED":U TO svAllCustNo.
        APPLY "LEAVE":U TO svStartCustNo.
        APPLY "LEAVE":U TO svEndCustNo.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPopulateOptions sObject 
PROCEDURE pPopulateOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphContainer AS HANDLE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        hContainer = iphContainer.
        
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartOpDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndOpDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

