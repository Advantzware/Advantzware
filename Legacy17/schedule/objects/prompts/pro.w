&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: pro.w

  Description: Get/Set Scheduler Pro Options

  Input Parameters: <none>

  Output Parameters: pro options value

  Author: Ron Stark

  Created: 5.3.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE OUTPUT PARAMETER opProOptions AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipInteractive AS LOGICAL NO-UNDO.
&ELSE
DEFINE VARIABLE opProOptions AS INTEGER NO-UNDO.
DEFINE VARIABLE ipInteractive AS LOGICAL NO-UNDO INITIAL YES.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE proOptions AS CHARACTER NO-UNDO.
DEFINE VARIABLE proOpts AS LOGICAL NO-UNDO EXTENT 10.

RUN getProOptions.
RUN setOptions.

opProOptions = INTEGER(proOptions).

IF NOT ipInteractive THEN
RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS passWord btnCancel 
&Scoped-Define DISPLAYED-OBJECTS passWord dropDrag senerios downTime ~
detailWindow 

/* Custom List Definitions                                              */
/* options,List-2,List-3,List-4,List-5,List-6                           */
&Scoped-define options dropDrag btnOK senerios downTime detailWindow 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "schedule/images/exit1.bmp":U
     LABEL "" 
     SIZE 7 BY 1.67
     BGCOLOR 8 .

DEFINE BUTTON btnOK AUTO-GO 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 7 BY 1.67
     BGCOLOR 8 .

DEFINE VARIABLE passWord AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE detailWindow AS LOGICAL INITIAL no 
     LABEL "&4 Detail Window Enabled" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 TOOLTIP "8" NO-UNDO.

DEFINE VARIABLE downTime AS LOGICAL INITIAL no 
     LABEL "&3 Downtime Enabled" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 TOOLTIP "4" NO-UNDO.

DEFINE VARIABLE dropDrag AS LOGICAL INITIAL no 
     LABEL "&1 Drop and Drag Enabled" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 TOOLTIP "1" NO-UNDO.

DEFINE VARIABLE senerios AS LOGICAL INITIAL no 
     LABEL "&2 Senerios Enabled" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 TOOLTIP "2" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     passWord AT ROW 1.24 COL 11 COLON-ALIGNED BLANK 
     dropDrag AT ROW 2.67 COL 13
     btnOK AT ROW 3.62 COL 44
     senerios AT ROW 3.86 COL 13
     downTime AT ROW 5.05 COL 13
     btnCancel AT ROW 5.29 COL 44
     detailWindow AT ROW 6.24 COL 13
     SPACE(11.39) SKIP(0.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Scheduler Pro Options"
         CANCEL-BUTTON btnCancel.


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnOK IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX detailWindow IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX downTime IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX dropDrag IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX senerios IN FRAME Dialog-Frame
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Scheduler Pro Options */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame
DO:
  RUN putProOptions.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME detailWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL detailWindow Dialog-Frame
ON VALUE-CHANGED OF detailWindow IN FRAME Dialog-Frame /* 4 Detail Window Enabled */
DO:
  ASSIGN {&SELF-NAME}.
  proOpts[4] = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME downTime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL downTime Dialog-Frame
ON VALUE-CHANGED OF downTime IN FRAME Dialog-Frame /* 3 Downtime Enabled */
DO:
  ASSIGN {&SELF-NAME}.
  proOpts[3] = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dropDrag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dropDrag Dialog-Frame
ON VALUE-CHANGED OF dropDrag IN FRAME Dialog-Frame /* 1 Drop and Drag Enabled */
DO:
  ASSIGN {&SELF-NAME}.
  proOpts[1] = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME passWord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL passWord Dialog-Frame
ON LEAVE OF passWord IN FRAME Dialog-Frame /* Password */
DO:
  ASSIGN {&SELF-NAME}.
  
  IF {&SELF-NAME} EQ 'Pro' + proOptions THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&OPTIONS}.
    DISABLE {&SELF-NAME}.
  END.
  ELSE
  MESSAGE 'Invalid Password' VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL passWord Dialog-Frame
ON RETURN OF passWord IN FRAME Dialog-Frame /* Password */
DO:
  APPLY 'LEAVE' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME senerios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL senerios Dialog-Frame
ON VALUE-CHANGED OF senerios IN FRAME Dialog-Frame /* 2 Senerios Enabled */
DO:
  ASSIGN {&SELF-NAME}.
  proOpts[2] = {&SELF-NAME}.
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
  DISPLAY passWord dropDrag senerios downTime detailWindow 
      WITH FRAME Dialog-Frame.
  ENABLE passWord btnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getProOptions Dialog-Frame 
PROCEDURE getProOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  LOAD 'SOFTWARE' BASE-KEY 'HKEY_CURRENT_USER'.
  USE 'SOFTWARE'.
  GET-KEY-VALUE SECTION 'TheStarkGroup'
                KEY 'Scheduler'
                VALUE proOptions.
  UNLOAD 'SOFTWARE'.
  IF proOptions EQ ? THEN
  proOptions = '0'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putProOptions Dialog-Frame 
PROCEDURE putProOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  DO i = 1 TO EXTENT(proOpts):
    IF proOpts[i] THEN
    j = j + EXP(2,i - 1).
  END.
  ASSIGN
    proOptions = STRING(j)
    opProOptions = j.
  LOAD 'SOFTWARE' BASE-KEY 'HKEY_CURRENT_USER'.
  USE 'SOFTWARE'.
  PUT-KEY-VALUE SECTION 'TheStarkGroup'
                KEY 'Scheduler'
                VALUE proOptions.
  UNLOAD 'SOFTWARE'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOptions Dialog-Frame 
PROCEDURE setOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.

  j = INTEGER(proOptions).
  DO i = EXTENT(proOpts) TO 1 BY -1:
    proOpts[i] = NO.
    IF j GE EXP(2,i - 1) THEN
    ASSIGN
      j = j - EXP(2,i - 1)
      proOpts[i] = YES.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      dropDrag = proOpts[1]
      senerios = proOpts[2]
      downTime = proOpts[3]
      detailWindow = proOpts[4].
    IF ipInteractive THEN
    DISPLAY {&OPTIONS}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

