&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: messageDialog.w

  Description: Message Dialog with larger text/buttons

  Input Parameters: Message Text, Yes Button, No Button, Cancel Button

  Output Parameters: Choice

  Author: Ron Stark

  Created: 9.17.2021
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER ipcMessageText  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplYesButton    AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iplNoButton     AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iplCancelButton AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER oplChoice       AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE dWidth AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPos   AS DECIMAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS messageText btnYes btnNo btnCancel 
&Scoped-Define DISPLAYED-OBJECTS messageText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "CANCEL" 
     SIZE 15 BY 2.1
     BGCOLOR 8 .

DEFINE BUTTON btnNo AUTO-GO 
     LABEL "NO" 
     SIZE 15 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON btnYes AUTO-GO 
     LABEL "YES" 
     SIZE 15 BY 2.14
     BGCOLOR 8 .

DEFINE VARIABLE messageText AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 101 BY 6.67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     messageText AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 4
     btnYes AT ROW 8.14 COL 56
     btnNo AT ROW 8.14 COL 72 WIDGET-ID 2
     btnCancel AT ROW 8.14 COL 88
     SPACE(0.39) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 38
         TITLE BGCOLOR 21 FGCOLOR 15 "Message Dialog"
         DEFAULT-BUTTON btnYes CANCEL-BUTTON btnCancel WIDGET-ID 100.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       messageText:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Message Dialog */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame /* CANCEL */
DO:
    oplChoice = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNo Dialog-Frame
ON CHOOSE OF btnNo IN FRAME Dialog-Frame /* NO */
DO:
    oplChoice = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnYes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnYes Dialog-Frame
ON CHOOSE OF btnYes IN FRAME Dialog-Frame /* YES */
DO:
    oplChoice = YES.
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
  RUN enable_UI.
  
  dWidth = FRAME {&FRAME-NAME}:WIDTH.
  
  ASSIGN
      messageText:SCREEN-VALUE = ipcMessageText
      btnYes:HIDDEN            = NOT iplYesButton
      btnNo:HIDDEN             = NOT iplNoButton
      btnCancel:HIDDEN         = NOT iplCancelButton
      .
  
  dPos = 1 / (INT(iplYesButton) + INT(iplNoButton) + INT(iplCancelButton) + 1).

  ASSIGN
      btnYes:COL    = 1 * dPos * dWidth - (btnYes:WIDTH * 0.5)
      btnNo:COL     = (2 - INT(NOT iplYesButton)) * dPos * dWidth - (btnNo:WIDTH * 0.5)
      btnCancel:COL = (3 - INT(NOT iplYesButton) - INT(NOT iplNoButton)) * dPos * dWidth - (btnCancel:WIDTH * 0.5)
      .
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
  DISPLAY messageText 
      WITH FRAME Dialog-Frame.
  ENABLE messageText btnYes btnNo btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

