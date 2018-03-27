&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: oe\d-oexfer.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF OUTPUT PARAMETER op-custom AS LOG NO-UNDO.


/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
/*
DEF TEMP-TABLE tt-bud NO-UNDO
    FIELD sman AS CHAR
    FIELD cust-no AS CHAR
    FIELD procat AS CHAR
    FIELD amt AS DEC EXTENT 13
    FIELD msf AS INT EXTENT 13
    FIELD tons AS INT EXTENT 13
    INDEX tt-bud-idx cust-no procat. */
DEF VAR ll-secure AS LOGICAL NO-UNDO.
ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-41 tb_delall btn-import btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_delall 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-GO 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-import 
     LABEL "&OK" 
     SIZE 18 BY 1.14.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 5.86.

DEFINE VARIABLE tb_delall AS LOGICAL INITIAL no 
     LABEL "Delete All Preps for this Estimate?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1.43 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tb_delall AT ROW 3.29 COL 8.4 WIDGET-ID 32
     btn-import AT ROW 5.29 COL 6.8
     btn-cancel AT ROW 5.29 COL 40.8
     "Are you sure you want to delete the selected Prep?" VIEW-AS TEXT
          SIZE 54.4 BY 1.24 AT ROW 1.67 COL 7.6 WIDGET-ID 28
     RECT-41 AT ROW 1.1 COL 2
     SPACE(0.59) SKIP(0.17)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Delete Preparation".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Choose custom or standard report */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    op-custom = ?.
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-import
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-import Dialog-Frame
ON CHOOSE OF btn-import IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN tb_delall.
      IF tb_delall THEN
          op-custom = YES.
      ELSE
          op-custom = NO.
        
       apply "window-close" to frame {&frame-name}. 

    /*  &IF DEFINED (adm-panel) <> 0 &THEN 
          RUN dispatch IN THIS-PROCEDURE ('exit').
      &ELSE                                         */
         /* APPLY "window-close" TO FRAME Dialog-Frame. */
    /*  &ENDIF*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  op-custom = NO.
  RUN enable_UI.
  op-custom = ?.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY tb_delall 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-41 tb_delall btn-import btn-cancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

