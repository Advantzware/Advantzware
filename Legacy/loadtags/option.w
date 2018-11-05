&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_rmpo btn_fgord btn_tag RECT-1 RECT-2 ~
RECT-9 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-fgjob 
     LABEL "FG Load Tag From Job" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn-fgpo 
     LABEL "FG Load Tag From PO" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON Btn_Close 
     LABEL "Close" 
     SIZE 32 BY .57
     FONT 6.

DEFINE BUTTON btn_fginv 
     LABEL "FG Load Tag From Stock" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_fgord 
     LABEL "FG Load Tag" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_rminv 
     LABEL "RM Load Tag From Stock" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_rmpo 
     LABEL "RM Load Tag From PO" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_tag 
     LABEL "Loadtag Maintenance" 
     SIZE 32 BY 2
     FONT 6.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 4.76.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 8.81.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 16.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn_rmpo AT ROW 1.48 COL 3
     btn_rminv AT ROW 3.62 COL 3
     btn_fgord AT ROW 6.48 COL 3
     btn-fgjob AT ROW 8.62 COL 3
     btn_fginv AT ROW 10.76 COL 3
     btn-fgpo AT ROW 12.91 COL 3
     btn_tag AT ROW 15.29 COL 3
     Btn_Close AT ROW 17.19 COL 3
     RECT-1 AT ROW 1.24 COL 2
     RECT-2 AT ROW 6.24 COL 2
     RECT-9 AT ROW 1 COL 1
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
         HEIGHT             = 19.57
         WIDTH              = 70.2.
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

/* SETTINGS FOR BUTTON btn-fgjob IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-fgjob:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btn-fgpo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-fgpo:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Close IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Close:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btn_fginv IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn_fginv:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON btn_rminv IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn_rminv:HIDDEN IN FRAME F-Main           = TRUE.

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

&Scoped-define SELF-NAME btn-fgjob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-fgjob s-object
ON CHOOSE OF btn-fgjob IN FRAME F-Main /* FG Load Tag From Job */
DO:
     /*run loadtags/loadjobs.w. */
     /*run /*loadtags/fgloado.w.*/ oerep/r-loadtg.w. */
     RUN addon/loadtags/r-loadtg.w.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-fgpo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-fgpo s-object
ON CHOOSE OF btn-fgpo IN FRAME F-Main /* FG Load Tag From PO */
DO:
     run loadtags/fgloadpo.w.
/*     apply "window-close" to current-window. */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* Close */
DO:
  {methods/run_link.i "CONTAINER" "Close_RM_Whse"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_fginv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_fginv s-object
ON CHOOSE OF btn_fginv IN FRAME F-Main /* FG Load Tag From Stock */
DO:
  /*  run loadtags/fgloadi.w. */
     run /*loadtags/fgloado.w.*/ addon/loadtags/r-ldtagi.w. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_fgord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_fgord s-object
ON CHOOSE OF btn_fgord IN FRAME F-Main /* FG Load Tag */
DO:
    run /*loadtags/fgloado.w.*/ addon/loadtags/r-loadtg.w. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_rminv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_rminv s-object
ON CHOOSE OF btn_rminv IN FRAME F-Main /* RM Load Tag From Stock */
DO:
    run loadtags/loadtagi.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_rmpo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_rmpo s-object
ON CHOOSE OF btn_rmpo IN FRAME F-Main /* RM Load Tag From PO */
DO:
    run loadtags/loadtags.w.
    /*run rm/w-rmtrs.w.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_tag s-object
ON CHOOSE OF btn_tag IN FRAME F-Main /* Loadtag Maintenance */
DO:
    run loadtags/w-ldtag.w.
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

