&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-mchtrn.w

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
&Scoped-Define ENABLED-OBJECTS btnAddEmail svCompany svStartMachTranDate ~
btnCalendar-1 svStartMachTranDateOption svEndMachTranDate btnCalendar-2 ~
svEndMachTranDateOption svAllMachine svStartMachine svEndMachine svAllShift ~
svStartShift svEndShift svUseTime svStartTime svStartAMPM svEndTime ~
svEndAMPM svSort svSubRpt_EmployeeTransactions svRecipients 
&Scoped-Define DISPLAYED-OBJECTS svCompany svStartMachTranDate ~
svStartMachTranDateOption svEndMachTranDate svEndMachTranDateOption ~
svAllMachine svStartMachine startMachineDescription svEndMachine ~
endMachineDescription svAllShift svStartShift startShiftDescription ~
svEndShift endShiftDescription svUseTime svStartTime svStartAMPM svEndTime ~
svEndAMPM svSort svSubRpt_EmployeeTransactions svRecipients 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE svEndAMPM AS CHARACTER FORMAT "X(2)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "am","pm" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 TOOLTIP "Select AM/PM"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE svEndMachTranDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartAMPM AS CHARACTER FORMAT "X(2)":U INITIAL "AM" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "am","pm" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 TOOLTIP "Select AM/PM"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE svStartMachTranDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 2.86
     BGCOLOR 15 .

DEFINE VARIABLE endMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE endShiftDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE startMachineDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE startShiftDescription AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svEndMachTranDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndShift AS INTEGER FORMAT ">9" INITIAL 3 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svEndTime AS CHARACTER FORMAT "99:99":U INITIAL "1200" 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE svStartMachine AS CHARACTER FORMAT "X(8)" 
     LABEL "Start Machine" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE svStartMachTranDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartShift AS INTEGER FORMAT ">9" INITIAL 1 
     LABEL "Start Shift" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svStartTime AS CHARACTER FORMAT "99:99":U INITIAL "1200" 
     LABEL "Start Time" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE svSort AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Start Date / Time", "Start Date / Time",
"Start Date / Job#", "Start Date / Job#",
"Machine / Date / Time", "Machine / Date / Time"
     SIZE 25 BY 2.62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.1.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.1.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.33.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 3.33.

DEFINE VARIABLE svAllMachine AS LOGICAL INITIAL yes 
     LABEL "All Machines" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE svAllShift AS LOGICAL INITIAL yes 
     LABEL "All Shifts" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE svSubRpt_EmployeeTransactions AS LOGICAL INITIAL no 
     LABEL "Show Employee Transactions" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE svUseTime AS LOGICAL INITIAL no 
     LABEL "Use Start/End Times (not Shift Tables)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnAddEmail AT ROW 20.05 COL 40 HELP
          "Add Recipents" WIDGET-ID 636
     svCompany AT ROW 1.24 COL 142 COLON-ALIGNED WIDGET-ID 60
     svStartMachTranDate AT ROW 1.71 COL 66 COLON-ALIGNED HELP
          "Enter Start Transaction Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 1.71 COL 84 WIDGET-ID 76
     svStartMachTranDateOption AT ROW 1.71 COL 87 COLON-ALIGNED HELP
          "Select Start Receipt Date Option" NO-LABEL WIDGET-ID 74
     svEndMachTranDate AT ROW 2.91 COL 66 COLON-ALIGNED HELP
          "Enter End Transaction Date" WIDGET-ID 68
     btnCalendar-2 AT ROW 2.91 COL 84 WIDGET-ID 78
     svEndMachTranDateOption AT ROW 2.91 COL 87 COLON-ALIGNED HELP
          "Select End Receipt Date Option" NO-LABEL WIDGET-ID 70
     svAllMachine AT ROW 5.05 COL 37 HELP
          "All Macines?" WIDGET-ID 58
     svStartMachine AT ROW 5.05 COL 66 COLON-ALIGNED HELP
          "Enter Start Machine" WIDGET-ID 22
     startMachineDescription AT ROW 5.05 COL 80 COLON-ALIGNED HELP
          "Enter Beginning Customer Name" NO-LABEL WIDGET-ID 18
     svEndMachine AT ROW 6.24 COL 66 COLON-ALIGNED HELP
          "Enter End Machine" WIDGET-ID 20
     endMachineDescription AT ROW 6.24 COL 80 COLON-ALIGNED HELP
          "Enter Ending Customer Name" NO-LABEL WIDGET-ID 16
     svAllShift AT ROW 8.38 COL 37 HELP
          "All Shifts?" WIDGET-ID 216
     svStartShift AT ROW 8.38 COL 66 COLON-ALIGNED HELP
          "Enter Start Shift" WIDGET-ID 220
     startShiftDescription AT ROW 8.38 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 214
     svEndShift AT ROW 9.57 COL 66 COLON-ALIGNED HELP
          "Enter End Shift" WIDGET-ID 218
     endShiftDescription AT ROW 9.57 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 212
     svUseTime AT ROW 11.71 COL 37 HELP
          "Select to Use Time vs Shift Table" WIDGET-ID 248
     svStartTime AT ROW 11.71 COL 97 COLON-ALIGNED HELP
          "Enter Start Time" WIDGET-ID 222
     svStartAMPM AT ROW 11.71 COL 105 COLON-ALIGNED HELP
          "Select AM/PM" NO-LABEL WIDGET-ID 244
     svEndTime AT ROW 12.91 COL 97 COLON-ALIGNED HELP
          "Enter End Time" WIDGET-ID 228
     svEndAMPM AT ROW 12.91 COL 105 COLON-ALIGNED HELP
          "Select AM/PM" NO-LABEL WIDGET-ID 246
     svSort AT ROW 15.05 COL 91 NO-LABEL WIDGET-ID 84
     svSubRpt_EmployeeTransactions AT ROW 15.81 COL 37 HELP
          "Select to Show Employee Transactions" WIDGET-ID 88
     svRecipients AT ROW 18.38 COL 46 NO-LABEL WIDGET-ID 600
     "Recipients:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 19.1 COL 35 WIDGET-ID 602
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 14.81 COL 82 WIDGET-ID 90
     "Email" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 18.38 COL 40 WIDGET-ID 640
     RECT-1 AT ROW 1.24 COL 34 WIDGET-ID 250
     RECT-2 AT ROW 4.57 COL 34 WIDGET-ID 252
     RECT-3 AT ROW 7.91 COL 34 WIDGET-ID 254
     RECT-4 AT ROW 11.24 COL 34 WIDGET-ID 256
     RECT-5 AT ROW 14.57 COL 34 WIDGET-ID 258
     RECT-6 AT ROW 18.14 COL 34 WIDGET-ID 638
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.2 BY 21.67
         FGCOLOR 1 
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
         HEIGHT             = 21.67
         WIDTH              = 149.2.
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
/* SETTINGS FOR FILL-IN endMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN endShiftDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startMachineDescription IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startShiftDescription IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       svCompany:READ-ONLY IN FRAME F-Main        = TRUE.

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

&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail sObject
ON CHOOSE OF btnAddEmail IN FRAME F-Main /* Email */
DO:
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = svRecipients:SCREEN-VALUE.
    RUN AOA/Recipients.w (INPUT-OUTPUT cRecipients).
    svRecipients:SCREEN-VALUE = cRecipients.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartMachTranDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndMachTranDate}
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
  APPLY "ENTRY":U TO svStartMachTranDate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndMachine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndMachine sObject
ON LEAVE OF svEndMachine IN FRAME F-Main /* To */
DO:
    endMachineDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndMachTranDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndMachTranDate sObject
ON HELP OF svEndMachTranDate IN FRAME F-Main /* To */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndMachTranDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndMachTranDateOption sObject
ON VALUE-CHANGED OF svEndMachTranDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndMachTranDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndShift sObject
ON LEAVE OF svEndShift IN FRAME F-Main /* To */
DO:
    endShiftDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndTime sObject
ON LEAVE OF svEndTime IN FRAME F-Main /* To */
DO:
    {AOA/includes/svTime.i}
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


&Scoped-define SELF-NAME svStartMachTranDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartMachTranDate sObject
ON HELP OF svStartMachTranDate IN FRAME F-Main /* Start Transaction Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartMachTranDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartMachTranDateOption sObject
ON VALUE-CHANGED OF svStartMachTranDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartMachTranDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartShift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartShift sObject
ON LEAVE OF svStartShift IN FRAME F-Main /* Start Shift */
DO:
    startShiftDescription:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartTime sObject
ON LEAVE OF svStartTime IN FRAME F-Main /* Start Time */
DO:
    {AOA/includes/svTime.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUseTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUseTime sObject
ON VALUE-CHANGED OF svUseTime IN FRAME F-Main /* Use Start/End Times (not Shift Tables) */
DO:
    {AOA/includes/svTimeInit.i svStartTime svStartAMPM svEndTime svEndAMPM}
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
            .

        APPLY "VALUE-CHANGED":U TO svStartMachTranDateOption.
        APPLY "VALUE-CHANGED":U TO svEndMachTranDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllMachine.
        APPLY "LEAVE":U TO svStartMachine.
        APPLY "LEAVE":U TO svEndMachine.

        APPLY "VALUE-CHANGED":U TO svAllShift.
        APPLY "LEAVE":U TO svStartShift.
        APPLY "LEAVE":U TO svEndShift.
        
        APPLY "VALUE-CHANGED":U TO svUseTime.
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

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartMachTranDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndMachTranDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

