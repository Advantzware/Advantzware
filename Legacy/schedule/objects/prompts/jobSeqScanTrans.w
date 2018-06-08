&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: JobSeqScanTrans.w

  Description: Job Seq

  Input Parameters: List of Resources, comma delimited

  Output Parameters: Selected Resource

  Author: Ron Stark

  Created: 6/7/2018
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER ipcResourceList AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcResource     AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnCancel btnOK svResource svResourceList 
&Scoped-Define DISPLAYED-OBJECTS svResource svResourceList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnCancel btnOK 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "Cancel".

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 9.6 BY 2.29 TOOLTIP "OK".

DEFINE VARIABLE svResource AS CHARACTER FORMAT "X(256)":U 
     LABEL "Resource" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE svResourceList AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Empty", "Empty"
     SIZE 51 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCancel AT ROW 1.24 COL 55 WIDGET-ID 16
     btnOK AT ROW 1.24 COL 45 WIDGET-ID 36
     svResource AT ROW 1.71 COL 18 COLON-ALIGNED HELP
          "Scan/Enter Resource" WIDGET-ID 4
     svResourceList AT ROW 3.62 COL 13 HELP
          "Select Resource" NO-LABEL WIDGET-ID 6
     SPACE(1.13) SKIP(7.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Transfer Job From..." WIDGET-ID 100.


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

/* SETTINGS FOR BUTTON btnCancel IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR BUTTON btnOK IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Transfer Job From... */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel Dialog-Frame
ON CHOOSE OF btnCancel IN FRAME Dialog-Frame
DO:
    opcResource = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame
DO:
    opcResource = svResource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svResource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svResource Dialog-Frame
ON LEAVE OF svResource IN FRAME Dialog-Frame /* Resource */
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svResource Dialog-Frame
ON RETURN OF svResource IN FRAME Dialog-Frame /* Resource */
DO:
    APPLY "LEAVE":U TO SELF.
    APPLY "CHOOSE":U TO btnOK.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svResourceList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svResourceList Dialog-Frame
ON VALUE-CHANGED OF svResourceList IN FRAME Dialog-Frame
DO:
    ASSIGN
        svResourceList
        svResource:SCREEN-VALUE = svResourceList
        svResource
        .
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
  ASSIGN
      svResourceList:RADIO-BUTTONS = ipcResourceList
      svResourceList:HEIGHT        = svResourceList:NUM-BUTTONS * 1.5
      svResource:SCREEN-VALUE      = ENTRY(2,ipcResourceList)
      svResource
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
  DISPLAY svResource svResourceList 
      WITH FRAME Dialog-Frame.
  ENABLE btnCancel btnOK svResource svResourceList 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

