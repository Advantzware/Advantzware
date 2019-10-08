&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: smartobj/prgrms.w

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
&Scoped-Define ENABLED-OBJECTS Select_Font_Color Select_Menu_Bar ~
Select_MF_Design Select_Security 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Select_Font_Color 
     IMAGE-UP FILE "Graphics/32x32/window_font.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Font_Color" 
     SIZE 7.8 BY 1.81 TOOLTIP "Set Font and Colors".

DEFINE BUTTON Select_Menu_Bar 
     IMAGE-UP FILE "Graphics/32x32/drop_down_list.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Menu_Bar" 
     SIZE 7.8 BY 1.81 TOOLTIP "Menu Bar".

DEFINE BUTTON Select_MF_Design 
     IMAGE-UP FILE "Graphics/32x32/compasses.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "MF_Design" 
     SIZE 7.8 BY 1.81 TOOLTIP "Misc Fields Design Screen".

DEFINE BUTTON Select_Security 
     IMAGE-UP FILE "Graphics/32x32/spy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Security" 
     SIZE 7.8 BY 1.81 TOOLTIP "Security".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Select_Font_Color AT ROW 1 COL 9
     Select_Menu_Bar AT ROW 1 COL 17
     Select_MF_Design AT ROW 1 COL 25
     Select_Security AT ROW 1 COL 1
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
         WIDTH              = 42.4.
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

&Scoped-define SELF-NAME Select_Font_Color
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Font_Color s-object
ON CHOOSE OF Select_Font_Color IN FRAME F-Main /* Font_Color */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Menu_Bar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Menu_Bar s-object
ON CHOOSE OF Select_Menu_Bar IN FRAME F-Main /* Menu_Bar */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_MF_Design
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_MF_Design s-object
ON CHOOSE OF Select_MF_Design IN FRAME F-Main /* MF_Design */
DO:
  {methods/run_link.i "CONTAINER-SOURCE" "{&SELF-NAME}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Select_Security
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Select_Security s-object
ON CHOOSE OF Select_Security IN FRAME F-Main /* Security */
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

