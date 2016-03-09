&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: smartobj/options.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 02/08/98

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndlset.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Select_appl Select_help Select_List ~
Select_spec Select_Search Select_Notes Select_Misc_Fields Select_Browser ~
Select_Viewer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Select_appl 
     IMAGE-UP FILE "images/appl.bmp":U NO-FOCUS
     LABEL "Util_appl" 
     SIZE 7.8 BY 1.81 TOOLTIP "Utility Application".

DEFINE BUTTON Select_Browser 
     IMAGE-UP FILE "images\b-browse":U NO-FOCUS
     LABEL "Browser" 
     SIZE 7.8 BY 1.81 TOOLTIP "Browser".

DEFINE BUTTON Select_help 
     IMAGE-UP FILE "images/help.ico":U NO-FOCUS
     LABEL "Help" 
     SIZE 7.8 BY 1.81 TOOLTIP "Help".

DEFINE BUTTON Select_List 
     IMAGE-UP FILE "images/print-u.bmp":U
     IMAGE-INSENSITIVE FILE "images/badsmo":U NO-FOCUS
     LABEL "List" 
     SIZE 7.8 BY 1.81 TOOLTIP "List".

DEFINE BUTTON Select_Misc_Fields 
     IMAGE-UP FILE "images/flds-u":U
     IMAGE-INSENSITIVE FILE "images/badsmo":U NO-FOCUS
     LABEL "Misc_Fields" 
     SIZE 7.8 BY 1.81 TOOLTIP "Misc. Fields".

DEFINE BUTTON Select_Notes 
     IMAGE-UP FILE "images/edit":U
     IMAGE-INSENSITIVE FILE "images/badsmo":U NO-FOCUS
     LABEL "Notes" 
     SIZE 7.8 BY 1.81 TOOLTIP "Notes".

DEFINE BUTTON Select_Search 
     IMAGE-UP FILE "images/prospy":U
     IMAGE-INSENSITIVE FILE "images/badsmo":U NO-FOCUS
     LABEL "Search" 
     SIZE 7.8 BY 1.81 TOOLTIP "Search".

DEFINE BUTTON Select_spec 
     IMAGE-UP FILE "images\dict":U NO-FOCUS
     LABEL "spec_note" 
     SIZE 7.8 BY 1.81 TOOLTIP "Spec Notes".

DEFINE BUTTON Select_Viewer 
     IMAGE-UP FILE "images\b-view":U NO-FOCUS
     LABEL "Viewer" 
     SIZE 7.8 BY 1.81 TOOLTIP "Viewer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Select_appl AT ROW 1 COL 57
     Select_help AT ROW 1 COL 65
     Select_List AT ROW 1 COL 9
     Select_spec AT ROW 1 COL 49
     Select_Search AT ROW 1 COL 1
     Select_Notes AT ROW 1 COL 17
     Select_Misc_Fields AT ROW 1 COL 25
     Select_Browser AT ROW 1 COL 33
     Select_Viewer AT ROW 1 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 1.81
         WIDTH              = 72.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

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

&Scoped-define SELF-NAME Select_appl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_appl s-object
ON CHOOSE OF Select_appl IN FRAME F-Main /* Util_appl */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Browser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Browser s-object
ON CHOOSE OF Select_Browser IN FRAME F-Main /* Browser */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_help s-object
ON CHOOSE OF Select_help IN FRAME F-Main /* Help */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_List s-object
ON CHOOSE OF Select_List IN FRAME F-Main /* List */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Misc_Fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Misc_Fields s-object
ON CHOOSE OF Select_Misc_Fields IN FRAME F-Main /* Misc_Fields */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Notes s-object
ON CHOOSE OF Select_Notes IN FRAME F-Main /* Notes */
DO:
   {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Search s-object
ON CHOOSE OF Select_Search IN FRAME F-Main /* Search */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_spec s-object
ON CHOOSE OF Select_spec IN FRAME F-Main /* spec_note */
DO:
    {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Viewer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Viewer s-object
ON CHOOSE OF Select_Viewer IN FRAME F-Main /* Viewer */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

RUN Tool_Tips IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Buttons s-object 
PROCEDURE Init-Buttons :
/*------------------------------------------------------------------------------
  Purpose:     Find out from container-source program which buttons are not needed
  Parameters:  OUTPUT search-button, list-button, misc_fields-button
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE search-button AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-button AS LOGICAL NO-UNDO.
  DEFINE VARIABLE notes-button AS LOGICAL NO-UNDO.
  DEFINE VARIABLE misc_fields-button AS LOGICAL NO-UNDO.

  {methods/run_link.i "CONTAINER-SOURCE" "Init-Options-Panel"
    "(OUTPUT search-button,OUTPUT list-button,OUTPUT notes-button,OUTPUT misc_fields-button)"}
  IF NOT search-button THEN
  DISABLE select_search WITH FRAME {&FRAME-NAME}.
  IF NOT list-button THEN
  DISABLE select_list WITH FRAME {&FRAME-NAME}.
  IF NOT notes-button THEN
  DISABLE select_notes WITH FRAME {&FRAME-NAME}.
  IF NOT misc_fields-button THEN
  DISABLE select_misc_fields WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Init-Buttons.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

