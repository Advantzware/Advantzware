&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File: r-mchord.p.w

  Description: from SMART.W - Template for basic ADM2 SmartObject

  Author: Ron Stark
  Created: 1.29.2019

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
&Scoped-Define ENABLED-OBJECTS svCompany svAllOrderNo svStartOrderNo ~
svEndOrderNo svAllItemNo svStartItemNo svEndItemNo svStartDueDate ~
btnCalendar-1 svStartDueDateOption svEndDueDate btnCalendar-2 ~
svEndDueDateOption 
&Scoped-Define DISPLAYED-OBJECTS svCompany svAllOrderNo svStartOrderNo ~
svEndOrderNo svAllItemNo svStartItemNo startItemName svEndItemNo ~
endItemName svStartDueDate svStartDueDateOption svEndDueDate ~
svEndDueDateOption 

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

DEFINE VARIABLE svEndDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDueDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE endItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE startItemName AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE svCompany AS CHARACTER FORMAT "X(3)" 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE svEndDueDate AS DATE FORMAT "99/99/9999" INITIAL 12/31/49 
     LABEL "End Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svEndItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "End Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svEndOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "End Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE svStartDueDate AS DATE FORMAT "99/99/9999" INITIAL 01/01/50 
     LABEL "Start Due Date" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE svStartItemNo AS CHARACTER FORMAT "X(15)" 
     LABEL "Start Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE svStartOrderNo AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Start Order" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 3.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 3.81.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 2.62.

DEFINE VARIABLE svAllItemNo AS LOGICAL INITIAL yes 
     LABEL "All Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE svAllOrderNo AS LOGICAL INITIAL yes 
     LABEL "All Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     svCompany AT ROW 1.24 COL 153 COLON-ALIGNED WIDGET-ID 60
     svAllOrderNo AT ROW 3.38 COL 59 HELP
          "All Orders?" WIDGET-ID 196
     svStartOrderNo AT ROW 4.57 COL 57 COLON-ALIGNED HELP
          "Enter Start Order" WIDGET-ID 200
     svEndOrderNo AT ROW 5.76 COL 57 COLON-ALIGNED HELP
          "Enter End Order" WIDGET-ID 198
     svAllItemNo AT ROW 7.91 COL 59 HELP
          "All Items?" WIDGET-ID 164
     svStartItemNo AT ROW 9.1 COL 57 COLON-ALIGNED HELP
          "Enter Start Item" WIDGET-ID 168
     startItemName AT ROW 9.1 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     svEndItemNo AT ROW 10.29 COL 57 COLON-ALIGNED HELP
          "Enter End Item" WIDGET-ID 166
     endItemName AT ROW 10.29 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     svStartDueDate AT ROW 12.43 COL 57 COLON-ALIGNED HELP
          "Enter Start Due Date" WIDGET-ID 134
     btnCalendar-1 AT ROW 12.43 COL 75 WIDGET-ID 126
     svStartDueDateOption AT ROW 12.43 COL 78 COLON-ALIGNED HELP
          "Select Start Due Date Option" NO-LABEL WIDGET-ID 136
     svEndDueDate AT ROW 13.62 COL 57 COLON-ALIGNED HELP
          "Enter End Due Date" WIDGET-ID 130
     btnCalendar-2 AT ROW 13.62 COL 75 WIDGET-ID 128
     svEndDueDateOption AT ROW 13.62 COL 78 COLON-ALIGNED HELP
          "Select End Due Date Option" NO-LABEL WIDGET-ID 132
     RECT-1 AT ROW 3.14 COL 42 WIDGET-ID 250
     RECT-2 AT ROW 7.67 COL 42 WIDGET-ID 252
     RECT-3 AT ROW 12.19 COL 42 WIDGET-ID 254
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 17
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Report Parameters".


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
         HEIGHT             = 17
         WIDTH              = 160.
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
/* SETTINGS FOR FILL-IN endItemName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN startItemName IN FRAME F-Main
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

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 sObject
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svStartDueDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 sObject
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i svEndDueDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllItemNo sObject
ON VALUE-CHANGED OF svAllItemNo IN FRAME F-Main /* All Items */
DO:
    {aoa/includes/svAllValueChanged.i svStartItemNo svEndItemNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAllOrderNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAllOrderNo sObject
ON VALUE-CHANGED OF svAllOrderNo IN FRAME F-Main /* All Orders */
DO:
    {aoa/includes/svAllValueChanged.i svStartOrderNo svEndOrderNo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svCompany sObject
ON ENTRY OF svCompany IN FRAME F-Main /* Company */
DO:
  APPLY "ENTRY":U TO svAllOrderNo.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDueDate sObject
ON HELP OF svEndDueDate IN FRAME F-Main /* End Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDueDateOption sObject
ON VALUE-CHANGED OF svEndDueDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndDueDate &btnCalendar=2}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndItemNo sObject
ON LEAVE OF svEndItemNo IN FRAME F-Main /* End Item */
DO:
    endItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDueDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDueDate sObject
ON HELP OF svStartDueDate IN FRAME F-Main /* Start Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDueDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDueDateOption sObject
ON VALUE-CHANGED OF svStartDueDateOption IN FRAME F-Main
DO:
    {aoa/includes/tDateOption.i &dateObject=svStartDueDate &btnCalendar=1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartItemNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartItemNo sObject
ON LEAVE OF svStartItemNo IN FRAME F-Main /* Start Item */
DO:
    startItemName:SCREEN-VALUE = {aoa/includes/fSetDescription.i}
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

        APPLY "VALUE-CHANGED":U TO svStartDueDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDueDateOption.
        
        APPLY "VALUE-CHANGED":U TO svAllItemNo.
        APPLY "LEAVE":U TO svStartItemNo.
        APPLY "LEAVE":U TO svEndItemNo.
        
        APPLY "VALUE-CHANGED":U TO svAllOrderNo.
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

        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svStartDueDateOption:HANDLE).
        DYNAMIC-FUNCTION('fDateOptions' IN hContainer,svEndDueDateOption:HANDLE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

