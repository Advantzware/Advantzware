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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Select_dept Select_list Select_spec UDF ~
Select_add Select_frac Select_appl Select_help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Select_add 
     IMAGE-UP FILE "Graphics/32x32/add.png":U
     IMAGE-DOWN FILE "Graphics/32x32/add_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/add_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 7.8 BY 1.81 TOOLTIP "Add".

DEFINE BUTTON Select_appl 
     IMAGE-UP FILE "Graphics/32x32/window_gear.png":U
     IMAGE-DOWN FILE "Graphics/32x32/window_gear_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/window_gear_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Run App" 
     SIZE 6.4 BY 1.52 TOOLTIP "Utility Application".

DEFINE BUTTON Select_dept 
     IMAGE-UP FILE "Graphics/32x32/edit.png":U
     IMAGE-DOWN FILE "Graphics/32x32/edit_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/edit_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Notes" 
     SIZE 6.4 BY 1.52 TOOLTIP "Notes".

DEFINE BUTTON Select_frac 
     IMAGE-UP FILE "Graphics/32x32/calculator.png":U
     IMAGE-DOWN FILE "Graphics/32x32/calculator_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/calculator_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 6.4 BY 1.52 TOOLTIP "Conversions".

DEFINE BUTTON Select_help 
     IMAGE-UP FILE "Graphics/32x32/question.png":U
     IMAGE-DOWN FILE "Graphics/32x32/question_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/question_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Help" 
     SIZE 6.4 BY 1.52 TOOLTIP "Help".

DEFINE BUTTON Select_list 
     IMAGE-UP FILE "Graphics/32x32/printer.png":U
     IMAGE-DOWN FILE "Graphics/32x32/printer_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/printer_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "List" 
     SIZE 6.4 BY 1.52 TOOLTIP "List".

DEFINE BUTTON Select_spec 
     IMAGE-UP FILE "Graphics/32x32/book_open.png":U
     IMAGE-DOWN FILE "Graphics/32x32/book_open_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/book_open_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Spec Note" 
     SIZE 6.4 BY 1.52 TOOLTIP "Spec Notes".

DEFINE BUTTON UDF 
     IMAGE-UP FILE "Graphics/32x32/window_dialog.png":U
     IMAGE-DOWN FILE "Graphics/32x32/window_dialog_hover.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/window_dialog_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "UDF" 
     SIZE 6.4 BY 1.52 TOOLTIP "UDF Viewer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Select_dept AT ROW 1 COL 17
     Select_list AT ROW 1 COL 9
     Select_spec AT ROW 1 COL 25
     UDF AT ROW 1 COL 49 HELP
          "Access UDF Viewer"
     Select_add AT ROW 1 COL 1
     Select_frac AT ROW 1 COL 57
     Select_appl AT ROW 1 COL 33
     Select_help AT ROW 1 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 21 .


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
         WIDTH              = 63.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
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

&Scoped-define SELF-NAME Select_add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_add s-object
ON CHOOSE OF Select_add IN FRAME F-Main /* Add */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_appl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_appl s-object
ON CHOOSE OF Select_appl IN FRAME F-Main /* Run App */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_dept s-object
ON CHOOSE OF Select_dept IN FRAME F-Main /* Notes */
DO:
  
   {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_frac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_frac s-object
ON CHOOSE OF Select_frac IN FRAME F-Main
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


&Scoped-define SELF-NAME Select_list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_list s-object
ON CHOOSE OF Select_list IN FRAME F-Main /* List */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}

  /* gdm - 11060802 - refreshes the viewer */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"oeinvopt-source",OUTPUT char-hdl).

  IF char-hdl NE "" THEN
     RUN refreshViewer IN WIDGET-HANDLE(char-hdl).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_spec s-object
ON CHOOSE OF Select_spec IN FRAME F-Main /* Spec Note */
DO:
   {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}

   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"spec-source",OUTPUT char-hdl).

   IF char-hdl NE "" THEN
      RUN value-changed-proc IN WIDGET-HANDLE(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME UDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL UDF s-object
ON CHOOSE OF UDF IN FRAME F-Main /* UDF */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-pen-image s-object 
PROCEDURE dept-pen-image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-log AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      IF NOT ip-log THEN
         Select_dept:LOAD-IMAGE("Graphics/32x32/edit.png").
      ELSE
         Select_dept:LOAD-IMAGE("Graphics/32x32/edit_star.png").
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DEFINE VARIABLE spec-note AS LOGICAL NO-UNDO.
  {methods/calcBtnImage.i}
  
  {methods/run_link.i "CONTAINER-SOURCE" "Init-Options-Panel"
    "(OUTPUT search-button,OUTPUT list-button,OUTPUT notes-button,OUTPUT misc_fields-button, output spec-note)"}
/*  IF NOT search-button THEN
  DISABLE select_search WITH FRAME {&FRAME-NAME}. */
  IF NOT list-button THEN
  DISABLE select_list WITH FRAME {&FRAME-NAME}.
  IF NOT notes-button THEN
  DISABLE select_dept WITH FRAME {&FRAME-NAME}.
  IF NOT misc_fields-button THEN
  DISABLE UDF WITH FRAME {&FRAME-NAME}.
  IF NOT spec-note THEN
  DISABLE select_spec WITH FRAME {&FRAME-NAME}.

  {methods/run_link.i "note-link-target" "disable-note" "(output notes-button)"}
  IF NOT notes-button THEN
  DISABLE select_dept WITH FRAME {&FRAME-NAME}.

  IF INDEX(program-name(5),"w-ordinq") > 0 OR
     INDEX(program-name(5),"w-oecred") > 0 OR
     INDEX(program-name(5),"w-oeweb") > 0 THEN
     DISABLE select_add WITH FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Spec-Book-Image s-object 
PROCEDURE Spec-Book-Image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-log AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      IF NOT ip-log THEN
         SELECT_spec:LOAD-IMAGE("Graphics/32x32/book_open.png").
      ELSE
         SELECT_spec:LOAD-IMAGE("Graphics/32x32/book_open_star.png").
   END.
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

