&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: touch/numeric.w

  Description: Numeric Keypad

  Input Parameters: Window Handle of Calling Procedure.

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 4.6.2000

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

&IF DEFINED(UIB_is_Running) NE 0 &THEN
DEFINE VARIABLE h_calling_window AS HANDLE NO-UNDO.
DEFINE VARIABLE h_touchfrm AS HANDLE NO-UNDO.
&ELSE
DEFINE INPUT PARAMETER h_calling_window AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER h_touchfrm AS HANDLE NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
DO TRANSACTION:
   {sys/inc/tskey.i}
END.

IF NOT tskey-log THEN
   RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Period Btn_Backspace Btn_Clear Btn_Eight ~
Btn_Five Btn_Four Btn_Minus Btn_Nine Btn_One Btn_Seven Btn_Six Btn_Three ~
Btn_Two Btn_Zero 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Backspace 
     LABEL "BACKSPACE" 
     SIZE 22 BY 2.38 TOOLTIP "BACKSPACE"
     FONT 6.

DEFINE BUTTON Btn_Clear 
     LABEL "CLEAR" 
     SIZE 11 BY 2.38 TOOLTIP "CLEAR"
     FONT 6.

DEFINE BUTTON Btn_Eight 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_8.png":U NO-FOCUS FLAT-BUTTON
     LABEL "8" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Five 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_5.png":U NO-FOCUS FLAT-BUTTON
     LABEL "5" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Four 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_4.png":U NO-FOCUS FLAT-BUTTON
     LABEL "4" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Minus 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_minus.png":U NO-FOCUS FLAT-BUTTON
     LABEL "-" 
     SIZE 11 BY 2.38 TOOLTIP "Minus".

DEFINE BUTTON Btn_Nine 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_9.png":U NO-FOCUS FLAT-BUTTON
     LABEL "9" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_One 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_1.png":U NO-FOCUS FLAT-BUTTON
     LABEL "1" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Period 
     IMAGE-UP FILE "Graphics/Keyboard/symbol_key_period.png":U NO-FOCUS FLAT-BUTTON
     LABEL "." 
     SIZE 11 BY 2.38 TOOLTIP "Period".

DEFINE BUTTON Btn_Seven 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_7.png":U NO-FOCUS FLAT-BUTTON
     LABEL "7" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Six 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_6.png":U NO-FOCUS FLAT-BUTTON
     LABEL "6" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Three 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_3.png":U NO-FOCUS FLAT-BUTTON
     LABEL "3" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Two 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_2.png":U NO-FOCUS FLAT-BUTTON
     LABEL "2" 
     SIZE 11 BY 2.38.

DEFINE BUTTON Btn_Zero 
     IMAGE-UP FILE "Graphics/Keyboard/keyboard_key_0.png":U NO-FOCUS FLAT-BUTTON
     LABEL "0" 
     SIZE 11 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Period AT ROW 10.52 COL 1
     Btn_Backspace AT ROW 1 COL 1
     Btn_Clear AT ROW 1 COL 23
     Btn_Eight AT ROW 8.14 COL 12
     Btn_Five AT ROW 5.76 COL 12
     Btn_Four AT ROW 5.76 COL 1
     Btn_Minus AT ROW 10.52 COL 23
     Btn_Nine AT ROW 8.14 COL 23
     Btn_One AT ROW 3.38 COL 1
     Btn_Seven AT ROW 8.14 COL 1
     Btn_Six AT ROW 5.76 COL 23
     Btn_Three AT ROW 3.38 COL 23
     Btn_Two AT ROW 3.38 COL 12
     Btn_Zero AT ROW 10.52 COL 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 33.2 BY 11.95
         BGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Numeric Pad"
         COLUMN             = 125
         ROW                = 1.52
         HEIGHT             = 11.95
         WIDTH              = 33.2
         MAX-HEIGHT         = 11.95
         MAX-WIDTH          = 33.2
         VIRTUAL-HEIGHT     = 11.95
         VIRTUAL-WIDTH      = 33.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       Btn_Backspace:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "BACKSPACE".

ASSIGN 
       Btn_Clear:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "CLEAR".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Numeric Pad */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Numeric Pad */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Backspace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Backspace C-Win
ON CHOOSE OF Btn_Backspace IN FRAME DEFAULT-FRAME /* BACKSPACE */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear C-Win
ON CHOOSE OF Btn_Clear IN FRAME DEFAULT-FRAME /* CLEAR */
DO:
  RUN Apply_Key (SELF:TOOLTIP).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Eight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Eight C-Win
ON CHOOSE OF Btn_Eight IN FRAME DEFAULT-FRAME /* 8 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Five
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Five C-Win
ON CHOOSE OF Btn_Five IN FRAME DEFAULT-FRAME /* 5 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Four
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Four C-Win
ON CHOOSE OF Btn_Four IN FRAME DEFAULT-FRAME /* 4 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Minus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Minus C-Win
ON CHOOSE OF Btn_Minus IN FRAME DEFAULT-FRAME /* - */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Nine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nine C-Win
ON CHOOSE OF Btn_Nine IN FRAME DEFAULT-FRAME /* 9 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_One
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_One C-Win
ON CHOOSE OF Btn_One IN FRAME DEFAULT-FRAME /* 1 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Period C-Win
ON CHOOSE OF Btn_Period IN FRAME DEFAULT-FRAME /* . */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Seven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Seven C-Win
ON CHOOSE OF Btn_Seven IN FRAME DEFAULT-FRAME /* 7 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Six
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Six C-Win
ON CHOOSE OF Btn_Six IN FRAME DEFAULT-FRAME /* 6 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Three
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Three C-Win
ON CHOOSE OF Btn_Three IN FRAME DEFAULT-FRAME /* 3 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Two
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Two C-Win
ON CHOOSE OF Btn_Two IN FRAME DEFAULT-FRAME /* 2 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Zero C-Win
ON CHOOSE OF Btn_Zero IN FRAME DEFAULT-FRAME /* 0 */
DO:
  RUN Apply_Key (SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {touch/kbLanguage.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Apply_Key C-Win 
PROCEDURE Apply_Key :
/*------------------------------------------------------------------------------
  Purpose:     Accept keystroke and send to calling procedure.
  Parameters:  Input Keystroke Value
  Notes:       
------------------------------------------------------------------------------*/
  
  
  {touch/applykey.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE Btn_Period Btn_Backspace Btn_Clear Btn_Eight Btn_Five Btn_Four 
         Btn_Minus Btn_Nine Btn_One Btn_Seven Btn_Six Btn_Three Btn_Two 
         Btn_Zero 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

