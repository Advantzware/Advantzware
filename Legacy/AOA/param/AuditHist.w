&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: AuditHist.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 10.10.2017

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

DEFINE VARIABLE dtStartDateTime AS DATETIME  NO-UNDO.
DEFINE VARIABLE dtEndDateTime   AS DATETIME  NO-UNDO.

{AOA/tempTable/ttAudit.i}

{AOA/includes/aoaParamVars.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS svCompany svType svStartDate btnCalendar-1 ~
svStartDateOption svEndDate btnCalendar-2 svEndDateOption svUser svDB ~
svTable svField svBeforeValueFilter svAfterValueFilter svPurge 
&Scoped-Define DISPLAYED-OBJECTS svCompany svType svStartDate ~
svStartDateOption svEndDate svEndDateOption svUser svDB svTable svField ~
svBeforeValueFilter svAfterValueFilter svPurge 

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

DEFINE VARIABLE svDB AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "DB" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 26 BY 1 TOOLTIP "Select Audit DB Filter" NO-UNDO.

DEFINE VARIABLE svEndDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svField AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Field" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 26 BY 1 TOOLTIP "Select Audit Field Filter" NO-UNDO.

DEFINE VARIABLE svStartDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svTable AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 26 BY 1 TOOLTIP "Select Audit Table Filter" NO-UNDO.

DEFINE VARIABLE svType AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "All","CREATE","DELETE","UPDATE","RESTORED","TRACK","LOG" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Select Audit Type Filter" NO-UNDO.

DEFINE VARIABLE svUser AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "User ID" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Select User Filter" NO-UNDO.

DEFINE VARIABLE svAfterValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "After Value" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE svBeforeValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Before Value" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svPurge AS LOGICAL INITIAL no 
     LABEL "Purge Audit History" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 14 COLON-ALIGNED WIDGET-ID 60
     svType AT ROW 3.14 COL 14 COLON-ALIGNED HELP
          "Select Audit Type Filter" WIDGET-ID 6
     svStartDate AT ROW 5.05 COL 14 COLON-ALIGNED HELP
          "Enter Start Date" WIDGET-ID 72
     btnCalendar-1 AT ROW 5.05 COL 32 WIDGET-ID 76
     svStartDateOption AT ROW 5.05 COL 35 COLON-ALIGNED HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svEndDate AT ROW 6.24 COL 14 COLON-ALIGNED HELP
          "Enter End Date" WIDGET-ID 68
     btnCalendar-2 AT ROW 6.24 COL 32 WIDGET-ID 78
     svEndDateOption AT ROW 6.24 COL 35 COLON-ALIGNED HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svUser AT ROW 8.14 COL 14 COLON-ALIGNED HELP
          "Select User Filter" WIDGET-ID 12
     svDB AT ROW 10.05 COL 14 COLON-ALIGNED HELP
          "Select Audit DB Filter" WIDGET-ID 14
     svTable AT ROW 11.24 COL 14 COLON-ALIGNED HELP
          "Select Audit Table Filter" WIDGET-ID 16
     svField AT ROW 12.43 COL 14 COLON-ALIGNED HELP
          "Select Audit Field Filter" WIDGET-ID 18
     svBeforeValueFilter AT ROW 14.33 COL 14 COLON-ALIGNED HELP
          "Enter Before Value to Filter" WIDGET-ID 36
     svAfterValueFilter AT ROW 15.52 COL 14 COLON-ALIGNED HELP
          "Enter After Value to Filter" WIDGET-ID 38
     svPurge AT ROW 17.43 COL 16 HELP
          "Select to Purge Audit History" WIDGET-ID 80
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74 BY 18.38
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
         HEIGHT             = 18.38
         WIDTH              = 74.
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

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svStartDate.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svDB sObject
ON VALUE-CHANGED OF svDB IN FRAME F-Main /* DB */
DO:
  ASSIGN
    {&SELF-NAME}
    svTable:SCREEN-VALUE = "All"
    svTable
    svField:SCREEN-VALUE = "All"
    svField
    .
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
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
    {aoa/includes/tDateOption.i &dateObject=svEndDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svField sObject
ON VALUE-CHANGED OF svField IN FRAME F-Main /* Field */
DO:
  ASSIGN {&SELF-NAME}.
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
    {aoa/includes/tDateOption.i &dateObject=svStartDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svTable sObject
ON VALUE-CHANGED OF svTable IN FRAME F-Main /* Table */
DO:
  ASSIGN
    {&SELF-NAME}
    svField:SCREEN-VALUE = "All"
    svField
    .
    RUN pGetFilterValues ("FIELD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svType sObject
ON VALUE-CHANGED OF svType IN FRAME F-Main /* Type */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUser sObject
ON VALUE-CHANGED OF svUser IN FRAME F-Main /* User ID */
DO:
  ASSIGN {&SELF-NAME}.
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

{AOA/includes/pGetAuditQueryFilters.i}

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
        APPLY "VALUE-CHANGED":U TO svStartDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDateOption.                
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParamValuesOverride sObject 
PROCEDURE pParamValuesOverride :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    svPurge:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no".
    
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

        svType:SCREEN-VALUE = "All".
        
        RUN pGetFilterValues ("INIT").
        RUN pGetFilterValues ("ALL").
        RUN pGetFilterValues ("TABLE").
        RUN pGetFilterValues ("FIELD").        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

