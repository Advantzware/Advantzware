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
DEFINE VARIABLE hProgram AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Rcpt Btn_Issues Btn_jobreturns ~
Btn_Transfers Btn_Move Btn_Adjust-3 Btn_Delete Btn_Post Btn_scan-vend ~
Btn_Close 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Adjust-3 
     LABEL "&Count Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Close 
     LABEL "Close (e&Xit)" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Issues 
     LABEL "&Issue Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_jobreturns 
     LABEL "&Job Returns" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Move 
     LABEL "&Move Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Post 
     LABEL "&Post Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Rcpt 
     LABEL "&Receive Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_scan-vend 
     LABEL "Scan Vendor Tag" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Transfers 
     LABEL "&Transfer Materials" 
     SIZE 40 BY 1.52
     FONT 6.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 16.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Rcpt AT ROW 1.24 COL 3
     Btn_Issues AT ROW 2.91 COL 3
     Btn_jobreturns AT ROW 4.57 COL 3
     Btn_Transfers AT ROW 6.24 COL 3
     Btn_Move AT ROW 7.91 COL 3
     Btn_Adjust-3 AT ROW 9.57 COL 3
     Btn_Delete AT ROW 11.24 COL 3
     Btn_Post AT ROW 12.91 COL 3
     Btn_scan-vend AT ROW 14.57 COL 3 WIDGET-ID 2
     Btn_Close AT ROW 16.24 COL 3
     RECT-1 AT ROW 1 COL 1
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
         HEIGHT             = 17.1
         WIDTH              = 44.
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

&Scoped-define SELF-NAME Btn_Adjust-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Adjust-3 s-object
ON CHOOSE OF Btn_Adjust-3 IN FRAME F-Main /* Count Materials */
DO:
  RUN addon/rm/w-phycnt.w PERSISTENT SET hProgram.
  RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close s-object
ON CHOOSE OF Btn_Close IN FRAME F-Main /* Close (eXit) */
DO:
  {methods/run_link.i "CONTAINER" "Close_RM_Whse"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete s-object
ON CHOOSE OF Btn_Delete IN FRAME F-Main /* Delete Materials */
DO:
  RUN addon/rm/w-rcpt.w PERSISTENT SET hProgram ('Delete').
  RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Issues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Issues s-object
ON CHOOSE OF Btn_Issues IN FRAME F-Main /* Issue Materials */
DO:
  RUN addon/rm/w-issue.w PERSISTENT SET hProgram.
  RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_jobreturns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_jobreturns s-object
ON CHOOSE OF Btn_jobreturns IN FRAME F-Main /* Job Returns */
DO:
  RUN addon/rm/w-jobret.w PERSISTENT SET hProgram.
  RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Move
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Move s-object
ON CHOOSE OF Btn_Move IN FRAME F-Main /* Move Materials */
DO:
  RUN addon/rm/rm-ucpt.w PERSISTENT SET hProgram.
  RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Post
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Post s-object
ON CHOOSE OF Btn_Post IN FRAME F-Main /* Post Materials */
DO:
    RUN rm/r-rmte&p.w PERSISTENT SET hProgram.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rcpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rcpt s-object
ON CHOOSE OF Btn_Rcpt IN FRAME F-Main /* Receive Materials */
DO:
  RUN addon/rm/w-rcpt.w PERSISTENT SET hProgram ('Receipt').
  RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_scan-vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_scan-vend s-object
ON CHOOSE OF Btn_scan-vend IN FRAME F-Main /* Scan Vendor Tag */
DO:
    RUN addon/rm/w-recven.w PERSISTENT SET hProgram.
    RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Transfers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Transfers s-object
ON CHOOSE OF Btn_Transfers IN FRAME F-Main /* Transfer Materials */
DO:
  RUN addon/rm/w-trans.w PERSISTENT SET hProgram.
  RUN dispatch IN hProgram ("initialize").
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

