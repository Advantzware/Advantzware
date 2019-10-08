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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_fgord RECT-9 btn_rmpo btn_scan btn_wip ~
btn_tag Btn_Close 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "Close" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_fgord 
     LABEL "Make FG Load Tags" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_rmpo 
     LABEL "Make Raw Mat'l Tags" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_scan 
     LABEL "Scan Case Label" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_tag 
     LABEL "File Maintenance" 
     SIZE 32 BY 2
     FONT 6.

DEFINE BUTTON btn_wip 
     LABEL "Make WIP Tags" 
     SIZE 32 BY 2
     FONT 6.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 14.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn_fgord AT ROW 1.24 COL 4
     btn_rmpo AT ROW 3.38 COL 4
     btn_scan AT ROW 5.52 COL 4
     btn_wip AT ROW 7.52 COL 4 WIDGET-ID 2
     btn_tag AT ROW 9.67 COL 4
     Btn_Close AT ROW 12.29 COL 4
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
         HEIGHT             = 14.38
         WIDTH              = 38.
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

&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* Close */
DO:
  {methods/run_link.i "CONTAINER" "Close_RM_Whse"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_fgord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_fgord s-object
ON CHOOSE OF btn_fgord IN FRAME F-Main /* Make FG Load Tags */
DO:
    run  /* for addon loadtags/r-ldtaga.w. */
        oerep/r-loadtg.w. /* same as gui */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_rmpo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_rmpo s-object
ON CHOOSE OF btn_rmpo IN FRAME F-Main /* Make Raw Mat'l Tags */
DO:
    RUN rmrep/rmloadtg.w.
    /*run loadtags/loadtags.w.*/
    /*run rm/w-rmtrs.w.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_scan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_scan s-object
ON CHOOSE OF btn_scan IN FRAME F-Main /* Scan Case Label */
DO:
   /* RUN loadtags/d-ldfgit.w (OUTPUT v-fg-loadtag).

    IF v-fg-loadtag THEN run loadtags/w-ldtag.w.
    ELSE RUN loadtags/w-ldtagi.w.
   */
    RUN loadtags/scanlbl.w.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_tag s-object
ON CHOOSE OF btn_tag IN FRAME F-Main /* File Maintenance */
DO:
    DEF VAR v-fg-loadtag AS cha NO-UNDO.

    RUN loadtags/d-ldfgit.w (OUTPUT v-fg-loadtag).

    IF v-fg-loadtag = "F" THEN run loadtags/w-ldtag.w.
    ELSE IF v-fg-loadtag = "R" THEN RUN loadtags/w-ldtagi.w.
    ELSE IF v-fg-loadtag = "C" THEN RUN loadtags/w-ldtagc.w.
    ELSE IF v-fg-loadtag = "W" THEN RUN jcrep/w-wipmt.w.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_wip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_wip s-object
ON CHOOSE OF btn_wip IN FRAME F-Main /* Make WIP Tags */
DO:
   DEFINE VARIABLE vlc-output AS CHAR INIT "" NO-UNDO.

   RUN jcrep/wipldtg.w (INPUT "",
                        INPUT "",
                        INPUT 0,
                        INPUT "",
                        INPUT 0,
                        INPUT 0,
                        INPUT 0,
                        INPUT "",
                        OUTPUT vlc-output).
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

