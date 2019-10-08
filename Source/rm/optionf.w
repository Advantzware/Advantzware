&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File:

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


{methods/defines/hndldefs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_fm-1 Btn_fm-2 Btn_fm-4 Btn-fm-3 Btn_fm-5 ~
Btn_fm-6 Btn_Close 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-fm-3 
     LABEL "Raw Material Bin File" 
     SIZE 40 BY 2.33
     FONT 6.

DEFINE BUTTON Btn_Close 
     LABEL "Close" 
     SIZE 40 BY 2.38
     FONT 6.

DEFINE BUTTON Btn_fm-1 
     LABEL "Materials Cost Type" 
     SIZE 40 BY 2.38
     FONT 6.

DEFINE BUTTON Btn_fm-2 
     LABEL "Raw Material Categories" 
     SIZE 40 BY 2.38
     FONT 6.

DEFINE BUTTON Btn_fm-4 
     LABEL "Raw Material Control File" 
     SIZE 40 BY 2.33
     FONT 6.

DEFINE BUTTON Btn_fm-5 
     LABEL "Raw Material Item File - Corrug." 
     SIZE 40 BY 2.33
     FONT 6.

DEFINE BUTTON Btn_fm-6 
     LABEL "Raw Material Item File - Fold" 
     SIZE 40 BY 2.38
     FONT 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_fm-1 AT ROW 1.24 COL 2
     Btn_fm-2 AT ROW 1.24 COL 45
     Btn_fm-4 AT ROW 4.1 COL 2
     Btn-fm-3 AT ROW 4.1 COL 45
     Btn_fm-5 AT ROW 6.95 COL 2
     Btn_fm-6 AT ROW 6.95 COL 45
     Btn_Close AT ROW 10.05 COL 25
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.76
         WIDTH              = 85.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* Close */
DO:
  {methods/run_link.i "CONTAINER" "Close_RM_Whse"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_fm-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_fm-1 s-object
ON CHOOSE OF Btn_fm-1 IN FRAME F-Main /* Materials Cost Type */
DO:
    run rm/shfm1.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_fm-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_fm-2 s-object
ON CHOOSE OF Btn_fm-2 IN FRAME F-Main /* Raw Material Categories */
DO:
  run rm/shfm2.w.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object _DEFAULT-DISABLE
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


