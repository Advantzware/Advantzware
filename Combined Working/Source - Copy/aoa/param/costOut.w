&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: costOut.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 10.26.2017

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
&Scoped-Define ENABLED-OBJECTS svCompany svLocation svAllJobNo svStartJobNo ~
svStartJobNo2 svEndJobNo svEndJobNo2 svStartDate btnCalendar-5 ~
svStartDateOption svEndDate btnCalendar-6 svEndDateOption svOpened svSort 
&Scoped-Define DISPLAYED-OBJECTS svCompany svLocation svAllJobNo ~
svStartJobNo svStartJobNo2 svEndJobNo svEndJobNo2 svStartDate ~
svStartDateOption svEndDate svEndDateOption svOpened svSort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-5 btnCalendar-6 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-5 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-6 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE svEndDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "End Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svEndJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svLocation AS CHARACTER FORMAT "X(5)" 
     LABEL "Location" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE svStartDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartJobNo AS CHARACTER FORMAT "X(6)" 
     LABEL "Start Job" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartJobNo2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE svOpened AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Yes",
"Closed", "No",
"Both", "Both"
     SIZE 37 BY 1 TOOLTIP "Job Status" NO-UNDO.

DEFINE VARIABLE svSort AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Job", "Job",
"Die No", "Die No",
"Sales Rep", "Sales Rep",
"Customer", "Customer",
"Item No", "Item No"
     SIZE 16 BY 5 NO-UNDO.

DEFINE VARIABLE svAllJobNo AS LOGICAL INITIAL yes 
     LABEL "All Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 23 COLON-ALIGNED WIDGET-ID 60
     svLocation AT ROW 1.24 COL 39 COLON-ALIGNED WIDGET-ID 232
     svAllJobNo AT ROW 3.14 COL 25 HELP
          "All Jobs?" WIDGET-ID 174
     svStartJobNo AT ROW 4.33 COL 23 COLON-ALIGNED HELP
          "Enter Start Job" WIDGET-ID 178
     svStartJobNo2 AT ROW 4.33 COL 34 COLON-ALIGNED HELP
          "Enter Start Job Run" WIDGET-ID 180
     svEndJobNo AT ROW 5.52 COL 23 COLON-ALIGNED HELP
          "Enter End Job" WIDGET-ID 176
     svEndJobNo2 AT ROW 5.52 COL 34 COLON-ALIGNED HELP
          "Enter End Job Run" WIDGET-ID 182
     svStartDate AT ROW 7.43 COL 23 COLON-ALIGNED HELP
          "Enter Start Date" WIDGET-ID 26
     btnCalendar-5 AT ROW 7.43 COL 41 WIDGET-ID 80
     svStartDateOption AT ROW 7.43 COL 44 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 64
     svEndDate AT ROW 8.62 COL 23 COLON-ALIGNED HELP
          "Enter End Date" WIDGET-ID 24
     btnCalendar-6 AT ROW 8.62 COL 41 WIDGET-ID 82
     svEndDateOption AT ROW 8.62 COL 44 COLON-ALIGNED HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 66
     svOpened AT ROW 10.52 COL 25 HELP
          "Select Job Status" NO-LABEL WIDGET-ID 234
     svSort AT ROW 12.19 COL 25 NO-LABEL WIDGET-ID 240
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 12.19 COL 16 WIDGET-ID 246
     "Job Status:" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 10.52 COL 13 WIDGET-ID 238
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 85.4 BY 17.38
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
         HEIGHT             = 17.38
         WIDTH              = 85.4.
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

/* SETTINGS FOR BUTTON btnCalendar-5 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-6 IN FRAME F-Main
   3                                                                    */
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

&Scoped-define SELF-NAME btnCalendar-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-5 sObject
ON CHOOSE OF btnCalendar-5 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-6 sObject
ON CHOOSE OF btnCalendar-6 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllJobNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllJobNo sObject
ON VALUE-CHANGED OF svAllJobNo IN FRAME F-Main /* All Jobs */
DO:
    {aoa/includes/svAllValueChanged.i svStartJobNo svEndJobNo}
    {aoa/includes/svAllValueChanged.i svStartJobNo2 svEndJobNo2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllJobNo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDate sObject
ON HELP OF svEndDate IN FRAME F-Main /* End Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDateOption sObject
ON VALUE-CHANGED OF svEndDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndDate &btnCalendar=6}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svLocation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svLocation sObject
ON ENTRY OF svLocation IN FRAME F-Main /* Location */
DO:
  APPLY "ENTRY":U TO svAllJobNo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDate sObject
ON HELP OF svStartDate IN FRAME F-Main /* Start Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDateOption sObject
ON VALUE-CHANGED OF svStartDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartDate &btnCalendar=5}
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

        APPLY "VALUE-CHANGED":U TO svStartDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllJobNo.
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

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

