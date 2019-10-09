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
&Scoped-Define ENABLED-OBJECTS Select_Add Select_dept Select_att ~
Select_spec Select_frac Select_appl Select_help UDF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Select_Add 
     IMAGE-UP FILE "Graphics/32x32/plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 7.8 BY 1.81 TOOLTIP "Add".

DEFINE BUTTON Select_appl 
     IMAGE-UP FILE "Graphics/32x32/window_gear.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Run App" 
     SIZE 7.8 BY 1.81 TOOLTIP "Utility Application".

DEFINE BUTTON Select_att 
     IMAGE-UP FILE "Graphics/32x32/paperclip.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Attachment" 
     SIZE 7.8 BY 1.81 TOOLTIP "Attachments".

DEFINE BUTTON Select_dept 
     IMAGE-UP FILE "Graphics/32x32/edit.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Notes" 
     SIZE 7.8 BY 1.81 TOOLTIP "Notes".

DEFINE BUTTON Select_frac 
     IMAGE-UP FILE "Graphics/32x32/calculator.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.8 BY 1.81 TOOLTIP "Conversions".

DEFINE BUTTON Select_help 
     IMAGE-UP FILE "Graphics/32x32/question.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Help" 
     SIZE 7.8 BY 1.81 TOOLTIP "Help".

DEFINE BUTTON UDF 
     IMAGE-UP FILE "Graphics/32x32/window_dialog.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "UDF" 
     SIZE 7.8 BY 1.81 TOOLTIP "UDF Viewer".

DEFINE BUTTON Select_spec 
     IMAGE-UP FILE "Graphics/32x32/book_open.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Spec Note" 
     SIZE 7.8 BY 1.81 TOOLTIP "Spec Notes".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Select_Add AT ROW 1 COL 1
     Select_dept AT ROW 1 COL 16.8
     Select_att AT ROW 1 COL 8.8
     Select_spec AT ROW 1 COL 25
     Select_frac AT ROW 1 COL 57
     Select_appl AT ROW 1 COL 33
     Select_help AT ROW 1 COL 41
     UDF AT ROW 1 COL 49 HELP
          "Access UDF Viewer"
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
         HEIGHT             = 1.91
         WIDTH              = 75.4.
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

&Scoped-define SELF-NAME Select_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Add s-object
ON CHOOSE OF Select_Add IN FRAME F-Main /* Add */
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


&Scoped-define SELF-NAME Select_att
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_att s-object
ON CHOOSE OF Select_att IN FRAME F-Main /* Attachment */
DO:
   DEF VAR char-hdl AS CHAR NO-UNDO.

   {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
    
   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"attach-source",OUTPUT char-hdl).

   IF char-hdl NE "" THEN
      RUN value-changed-proc IN WIDGET-HANDLE(char-hdl).
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


&Scoped-define SELF-NAME UDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL UDF s-object
ON CHOOSE OF UDF IN FRAME F-Main /* Home */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_spec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_spec s-object
ON CHOOSE OF Select_spec IN FRAME F-Main /* Spec Note */
DO:
   {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}

   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"attach-source",OUTPUT char-hdl).

   IF char-hdl NE "" THEN
      RUN value-changed-proc IN WIDGET-HANDLE(char-hdl).
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
         Select_dept:LOAD-IMAGE("Graphics/32x32/edit.ico").
      ELSE
         Select_dept:LOAD-IMAGE("Graphics/32x32/edit_star.ico").
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
/*   IF NOT list-button THEN                        */
/*   DISABLE select_list WITH FRAME {&FRAME-NAME}.  */
  IF NOT notes-button THEN
  DISABLE select_dept WITH FRAME {&FRAME-NAME}.
/*  IF NOT misc_fields-button THEN
  DISABLE select_misc_fields WITH FRAME {&FRAME-NAME}.  */
  IF NOT spec-note THEN
  DISABLE select_spec WITH FRAME {&FRAME-NAME}.

  {methods/run_link.i "note-link-target" "disable-note" "(output notes-button)"}
  IF NOT notes-button THEN
  DISABLE select_dept WITH FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-insensitive s-object 
PROCEDURE make-insensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
  Select_Add:SENSITIVE IN FRAME F-Main = FALSE
  Select_appl:SENSITIVE IN FRAME F-Main = FALSE
  Select_att:SENSITIVE IN FRAME F-Main = FALSE
  Select_dept:SENSITIVE IN FRAME F-Main = FALSE
  Select_frac:SENSITIVE IN FRAME F-Main = FALSE
  Select_help:SENSITIVE IN FRAME F-Main = FALSE
  UDF:SENSITIVE IN FRAME F-Main = FALSE
  Select_spec:SENSITIVE IN FRAME F-Main = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-sensitive s-object 
PROCEDURE make-sensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN
  Select_Add:SENSITIVE IN FRAME F-Main = TRUE
  Select_appl:SENSITIVE IN FRAME F-Main = TRUE
  Select_att:SENSITIVE IN FRAME F-Main = TRUE
  Select_dept:SENSITIVE IN FRAME F-Main = TRUE
  Select_frac:SENSITIVE IN FRAME F-Main = TRUE
  Select_help:SENSITIVE IN FRAME F-Main = TRUE
  UDF:SENSITIVE IN FRAME F-Main = TRUE
  Select_spec:SENSITIVE IN FRAME F-Main = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Paper-Clip-Image s-object 
PROCEDURE Paper-Clip-Image :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-attach AS LOG NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      IF NOT ip-attach THEN
         SELECT_att:LOAD-IMAGE("Graphics/32x32/paperclip.ico").
      ELSE
         SELECT_att:LOAD-IMAGE("Graphics/32x32/paperclip_star.ico").
   END.
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
         SELECT_spec:LOAD-IMAGE("Graphics/32x32/book_open.ico").
      ELSE
         SELECT_spec:LOAD-IMAGE("Graphics/32x32/book_open_star.ico").
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

