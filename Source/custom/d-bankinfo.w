&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ipType AS CHARACTER NO-UNDO.
DEF INPUT-OUTPUT PARAM iopAccount AS CHARACTER NO-UNDO.
DEF INPUT-OUTPUT PARAM iopSwift AS CHARACTER NO-UNDO.
DEF INPUT-OUTPUT PARAM iopRouting AS INTEGER NO-UNDO.


/* Local Variable Definitions ---                                       */


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR i AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 bank_account bank_swift bank_routing  ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS bank_account bank_swift bank_routing Btn_OK Btn_Cancel

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */


DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .
  
DEFINE VARIABLE bank_account AS CHARACTER FORMAT "X(18)" 
     LABEL "Bank Account" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE bank_swift AS CHARACTER FORMAT "X(11)":U 
     LABEL "Swift Code" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE bank_routing AS INTEGER FORMAT ">>>>>>>>9":U 
     LABEL "Routing" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90.2 BY 6.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     bank_account AT ROW 1.95 COL 30 COLON-ALIGNED 
     bank_swift AT ROW 3.50 COL 30 COLON-ALIGNED  
     bank_routing AT ROW 5.00 COL 30 COLON-ALIGNED 
     Btn_OK AT ROW 8.29 COL 18.60
     Btn_Cancel AT ROW 8.29 COL 57.40
     
     RECT-30 AT ROW 1.2 COL 1.2 WIDGET-ID 56
     SPACE(2.39) SKIP(2.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Bank Info"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Report Columns Selection */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            iopAccount =  bank_account:SCREEN-VALUE
            iopSwift   =  bank_swift:SCREEN-VALUE
            iopRouting =  integer(bank_routing:SCREEN-VALUE) .
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

   bank_account:SCREEN-VALUE IN FRAME {&FRAME-NAME} = iopAccount .
   bank_swift:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = iopSwift   .
   bank_routing:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(iopRouting) .
   bank_account = iopAccount .
   bank_swift = iopSwift   .
   bank_routing = INTEGER(iopRouting) .

  RUN enable_UI.
  IF ipType EQ "View" THEN
      ASSIGN
      bank_account:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
      bank_swift:SENSITIVE IN FRAME {&FRAME-NAME}   = NO   
      bank_routing:SENSITIVE IN FRAME {&FRAME-NAME} = NO .



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
  DISPLAY bank_account bank_swift bank_routing
      WITH FRAME Dialog-Frame.
  ENABLE RECT-30 bank_account bank_swift bank_routing Btn_OK 
         Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


