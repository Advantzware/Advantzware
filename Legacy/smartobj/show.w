&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: show.w

  Description: from SMART.W - Template for basic SmartObject

  Author:
  Created:

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
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-1 show-notes ~
show-selection-parameters show-misc-field-values show-addresses show-phones 
&Scoped-Define DISPLAYED-OBJECTS show-notes note-options ~
show-selection-parameters show-misc-field-values show-addresses show-phones 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 show-notes note-options show-selection-parameters ~
show-misc-field-values show-addresses show-phones 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE IMAGE IMAGE-1
     FILENAME "Graphics/32x32/binocular2.ico":U CONVERT-3D-COLORS
     STRETCH-TO-FIT
     SIZE 7 BY 1.67.

DEFINE VARIABLE note-options AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "&All Notes", 1,
"&Viewed Only", 2,
"Not Viewed &Only", 3
     SIZE 61 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 8.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7.8 BY 1.81.

DEFINE VARIABLE show-addresses AS LOGICAL INITIAL no 
     LABEL "Show Addresses" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE show-misc-field-values AS LOGICAL INITIAL no 
     LABEL "Show Misc. Field Values" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE show-notes AS LOGICAL INITIAL no 
     LABEL "Sho&w Notes" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE show-phones AS LOGICAL INITIAL no 
     LABEL "Show Phones" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE show-selection-parameters AS LOGICAL INITIAL yes 
     LABEL "Show &Selection Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     show-notes AT ROW 1.95 COL 15 HELP
          "Show Notes Indicator"
     note-options AT ROW 1.95 COL 36 HELP
          "Select Type of Notes to Show" NO-LABEL
     show-selection-parameters AT ROW 3.62 COL 15 HELP
          "Show Selection Parameters Indicator"
     show-misc-field-values AT ROW 5.05 COL 15 HELP
          "Show Misc. Field Values Indicator"
     show-addresses AT ROW 6.71 COL 15 HELP
          "Show Addresses Indicator"
     show-phones AT ROW 8.38 COL 15 HELP
          "Show Phones Indicator"
     "Show Options" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 1 COL 12
     IMAGE-1 AT ROW 1.24 COL 1
     RECT-2 AT ROW 1.24 COL 1
     RECT-1 AT ROW 1.24 COL 11
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
         HEIGHT             = 8.81
         WIDTH              = 97.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{methods/enhance.i}

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

/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET note-options IN FRAME F-Main
   NO-ENABLE 1                                                          */
ASSIGN 
       note-options:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR TOGGLE-BOX show-addresses IN FRAME F-Main
   1                                                                    */
ASSIGN 
       show-addresses:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR TOGGLE-BOX show-misc-field-values IN FRAME F-Main
   1                                                                    */
ASSIGN 
       show-misc-field-values:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR TOGGLE-BOX show-notes IN FRAME F-Main
   1                                                                    */
ASSIGN 
       show-notes:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR TOGGLE-BOX show-phones IN FRAME F-Main
   1                                                                    */
ASSIGN 
       show-phones:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

/* SETTINGS FOR TOGGLE-BOX show-selection-parameters IN FRAME F-Main
   1                                                                    */
ASSIGN 
       show-selection-parameters:PRIVATE-DATA IN FRAME F-Main     = 
                "export".

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

&Scoped-define SELF-NAME show-notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL show-notes s-object
ON VALUE-CHANGED OF show-notes IN FRAME F-Main /* Show Notes */
DO:
  ASSIGN
    {&SELF-NAME}
    note-options:SENSITIVE = {&SELF-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Frame-Handle s-object 
PROCEDURE Frame-Handle :
/* ---------------------------------------------------------------------------
  Purpose:     Called externally to get THIS-PROCEDURE's Frame handle
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER fhandle AS WIDGET-HANDLE NO-UNDO.

  fhandle = FRAME {&FRAME-NAME}:HANDLE.

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
  {methods/smartobj/showinit.i}

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

