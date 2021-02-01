&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: system/d-QueryLimitAlert.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Rahul Rawat

  Created: 23rd December,2020
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
    DEFINE INPUT  PARAMETER ipcMessage       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTitle         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplEnableShowAll AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcResponse      AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-2 edText btYes btNo 
&Scoped-Define DISPLAYED-OBJECTS edText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btNo AUTO-GO 
     LABEL "No" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btShowAll AUTO-GO 
     LABEL "Show All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btYes AUTO-GO 
     LABEL "Yes" 
     SIZE 15 BY 1.14
     FONT 5.

DEFINE VARIABLE edText AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 87 BY 2.62 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "Graphics/32x32/question.ico":U
     STRETCH-TO-FIT
     SIZE 8.8 BY 2.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     edText AT ROW 1.95 COL 12 NO-LABEL WIDGET-ID 10
     btShowAll AT ROW 4.86 COL 21.2 WIDGET-ID 2
     btYes AT ROW 4.86 COL 42.4 WIDGET-ID 4
     btNo AT ROW 4.86 COL 64.4 WIDGET-ID 6
     IMAGE-2 AT ROW 1.91 COL 2 WIDGET-ID 14
     SPACE(90.99) SKIP(2.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 5
         TITLE "" WIDGET-ID 100.


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

/* SETTINGS FOR BUTTON btShowAll IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       edText:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame
DO:
    APPLY "CHOOSE" TO btNo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame
DO:
    APPLY "CHOOSE" TO btNo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNo Dialog-Frame
ON CHOOSE OF btNo IN FRAME Dialog-Frame /* No */
DO:
    opcResponse = "NO". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btShowAll Dialog-Frame
ON CHOOSE OF btShowAll IN FRAME Dialog-Frame /* Show All */
DO:
    opcResponse = "ShowAll".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btYes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btYes Dialog-Frame
ON CHOOSE OF btYes IN FRAME Dialog-Frame /* Yes */
DO:
    opcResponse = "YES".  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN pSetTitleAndText.
  IF iplEnableShowAll THEN 
    btShowAll:SENSITIVE = iplEnableShowAll.
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
  DISPLAY edText 
      WITH FRAME Dialog-Frame.
  ENABLE IMAGE-2 edText btYes btNo 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetTitleAndText Dialog-Frame 
PROCEDURE pSetTitleAndText PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            edText:SCREEN-VALUE       = ipcMessage
            FRAME {&FRAME-NAME}:TITLE = ipcTitle
            .
    END.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

