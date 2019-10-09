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

DEFINE NEW SHARED VARIABLE choice         AS LOG NO-UNDO. /* for post fg */
DEFINE     SHARED VARIABLE g-sharpshooter AS LOG NO-UNDO.
DEFINE            VARIABLE hProgram       AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS Btn_Rcpt RECT-6 Btn_update Btn_Transfers ~
Btn_Adjust-3 Btn_delete Btn_return Btn_Post Btn_Consol Btn_Close 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Adjust-3 
     LABEL "Count Goods" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Close 
     LABEL "Close" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Consol 
     LABEL "Consolidate Tags" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_delete 
     LABEL "Delete Goods" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Post 
     LABEL "Post Goods" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Rcpt 
     LABEL "Receive Goods" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_return 
     LABEL "Return Goods" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_Transfers 
     LABEL "Transfer Goods" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE BUTTON Btn_update 
     LABEL "Move Receipts" 
     SIZE 35 BY 1.52
     FONT 6.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 16.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Rcpt AT ROW 1.24 COL 2
     Btn_update AT ROW 2.91 COL 2
     Btn_Transfers AT ROW 4.57 COL 2
     Btn_Adjust-3 AT ROW 6.24 COL 2
     Btn_delete AT ROW 7.91 COL 2
     Btn_return AT ROW 9.57 COL 2
     Btn_Post AT ROW 11.24 COL 2
     Btn_Consol AT ROW 12.91 COL 2 WIDGET-ID 2
     Btn_Close AT ROW 15.29 COL 2
     RECT-6 AT ROW 1 COL 1
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
         HEIGHT             = 16.62
         WIDTH              = 37.4.
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
ON CHOOSE OF Btn_Adjust-3 IN FRAME F-Main /* Count Goods */
DO:
    IF g-sharpshooter THEN RUN addon/fg/fg-physs.w PERSISTENT SET hProgram.
    ELSE RUN addon/fg/fg-phys.w PERSISTENT SET hProgram.
   RUN dispatch IN hProgram ("initialize").
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


&Scoped-define SELF-NAME Btn_Consol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consol s-object
ON CHOOSE OF Btn_Consol IN FRAME F-Main /* Consolidate Tags */
DO:
   RUN addon/fg/fg-cons.w PERSISTENT SET hProgram.
   RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_delete s-object
ON CHOOSE OF Btn_delete IN FRAME F-Main /* Delete Goods */
DO:
   IF g-sharpshooter THEN RUN addon/fg/fg-rcpts.w PERSISTENT SET hProgram ("Delete").
   ELSE RUN addon/fg/fg-rcpt.w PERSISTENT SET hProgram ("Delete").
   RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Post
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Post s-object
ON CHOOSE OF Btn_Post IN FRAME F-Main /* Post Goods */
DO:
DEF VAR v-post-sec AS LOG NO-UNDO.
DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.

/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "fgpstall.",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT v-post-sec, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */
    IF v-post-sec THEN
      RUN fg/fgpstall.w PERSISTENT (?, "").
    ELSE
      MESSAGE "Authorization required to post." VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rcpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rcpt s-object
ON CHOOSE OF Btn_Rcpt IN FRAME F-Main /* Receive Goods */
DO:
   IF g-sharpshooter THEN RUN addon/fg/fg-rcpts.w PERSISTENT SET hProgram ("Receipt").
   ELSE RUN addon/fg/fg-rcpt.w PERSISTENT SET hProgram ("Receipt").
   RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_return
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_return s-object
ON CHOOSE OF Btn_return IN FRAME F-Main /* Return Goods */
DO:
   RUN addon/sshoot/sssetups.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Transfers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Transfers s-object
ON CHOOSE OF Btn_Transfers IN FRAME F-Main /* Transfer Goods */
DO:
   IF g-sharpshooter THEN RUN addon/fg/fg-trnss.w PERSISTENT SET hProgram.
   ELSE RUN addon/fg/fg-trans.w PERSISTENT SET hProgram.
   RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_update s-object
ON CHOOSE OF Btn_update IN FRAME F-Main /* Move Receipts */
DO:
   IF g-sharpshooter THEN RUN addon/fg/fg-ucpts.w PERSISTENT SET hProgram.
   ELSE RUN addon/fg/fg-ucpt.w PERSISTENT SET hProgram.
   RUN dispatch IN hProgram ("initialize").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

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
  
  /*IF g-sharpshooter THEN DISABLE btn_post WITH FRAME {&FRAME-NAME}.*/


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

