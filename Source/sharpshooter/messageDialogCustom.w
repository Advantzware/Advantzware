&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: sharpshooter/messageDialogCustom.w

  Description: Message Dialog with larger text/buttons and custom button text

  Input Parameters: Message Text, Button Options (Max 4)

  Output Parameters: Choice

  Author: DEVA$!

  Created: 03.04.2022
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT  PARAMETER ipcMessageText  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcOptions      AS CHARACTER NO-UNDO. /* Comma Separated List of Button Options. Maximum 4 buttons */
DEFINE OUTPUT PARAMETER opcChoice       AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE dWidth        AS DECIMAL NO-UNDO.
DEFINE VARIABLE dPos          AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTextWidth    AS DECIMAL NO-UNDO.
DEFINE VARIABLE dMaxBtnWidth  AS DECIMAL NO-UNDO.
DEFINE VARIABLE dMinBtnWidth  AS DECIMAL NO-UNDO INITIAL 30.
DEFINE VARIABLE dButtonsWidth AS DECIMAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS messageText btn1 btn2 btn3 btn4 
&Scoped-Define DISPLAYED-OBJECTS messageText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn1 
     LABEL "Button1" 
     SIZE 15 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON btn2 
     LABEL "Button2" 
     SIZE 15 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON btn3 
     LABEL "Button3" 
     SIZE 15 BY 2.1
     BGCOLOR 8 .

DEFINE BUTTON btn4 
     LABEL "Button4" 
     SIZE 15 BY 2.14
     BGCOLOR 8 .

DEFINE VARIABLE messageText AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 155 BY 9.76 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     messageText AT ROW 1.48 COL 5.4 NO-LABEL WIDGET-ID 4
     btn1 AT ROW 11.81 COL 46
     btn2 AT ROW 11.81 COL 62 WIDGET-ID 2
     btn3 AT ROW 11.81 COL 77.8
     btn4 AT ROW 11.81 COL 93 WIDGET-ID 6
     SPACE(55.39) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 21 FGCOLOR 15 FONT 38
         TITLE BGCOLOR 21 FGCOLOR 15 "Message Dialog"
         DEFAULT-BUTTON btn1 WIDGET-ID 100.


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


&Scoped-define SELF-NAME btn1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn1 Dialog-Frame
ON CHOOSE OF btn1 IN FRAME Dialog-Frame /* Button1 */
DO:
    opcChoice = SELF:LABEL.
    APPLY "GO":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn2 Dialog-Frame
ON CHOOSE OF btn2 IN FRAME Dialog-Frame /* Button2 */
DO:
    opcChoice = SELF:LABEL.
    APPLY "GO":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn3 Dialog-Frame
ON CHOOSE OF btn3 IN FRAME Dialog-Frame /* Button3 */
DO:
    opcChoice = SELF:LABEL.
    APPLY "GO":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn4 Dialog-Frame
ON CHOOSE OF btn4 IN FRAME Dialog-Frame /* Button4 */
DO:
    opcChoice = SELF:LABEL.
    APPLY "GO":U TO FRAME {&FRAME-NAME}.
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
      btn1:VISIBLE             = NUM-ENTRIES(ipcOptions) GE 1
      btn2:VISIBLE             = NUM-ENTRIES(ipcOptions) GE 2
      btn3:VISIBLE             = NUM-ENTRIES(ipcOptions) GE 3
      btn4:VISIBLE             = NUM-ENTRIES(ipcOptions) GE 4
      btn1:LABEL               = CAPS(ENTRY(1, ipcOptions))
      btn2:LABEL               = CAPS(ENTRY(2, ipcOptions))
      btn3:LABEL               = CAPS(ENTRY(3, ipcOptions))
      btn4:LABEL               = CAPS(ENTRY(4, ipcOptions))      
      NO-ERROR.
  
  /* Fetch Text width for font and adjust button width*/
  ASSIGN
      dMaxBtnWidth  = (dWidth) * 0.25
      dMaxBtnWidth  = dMaxBtnWidth 
                    + INTEGER(btn1:HIDDEN) * (dMaxBtnWidth * 0.25) 
                    + INTEGER(btn2:HIDDEN) * (dMaxBtnWidth * 0.25) 
                    + INTEGER(btn3:HIDDEN) * (dMaxBtnWidth * 0.25) 
                    + INTEGER(btn4:HIDDEN) * (dMaxBtnWidth * 0.25)
      dTextWidth    = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ENTRY(1, ipcOptions), 38) + 2
      btn1:WIDTH    = IF dTextWidth GT dMaxBtnWidth THEN dMaxBtnWidth ELSE IF dTextWidth GT dMinBtnWidth THEN dTextWidth ELSE dMinBtnWidth
      dTextWidth    = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ENTRY(2, ipcOptions), 38) + 2 
      btn2:WIDTH    = IF dTextWidth GT dMaxBtnWidth THEN dMaxBtnWidth ELSE IF dTextWidth GT dMinBtnWidth THEN dTextWidth ELSE dMinBtnWidth
      dTextWidth    = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ENTRY(3, ipcOptions), 38) + 2
      btn3:WIDTH    = IF dTextWidth GT dMaxBtnWidth THEN dMaxBtnWidth ELSE IF dTextWidth GT dMinBtnWidth THEN dTextWidth ELSE dMinBtnWidth
      dTextWidth    = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ENTRY(4, ipcOptions), 38) + 2 
      btn4:WIDTH    = IF dTextWidth GT dMaxBtnWidth THEN dMaxBtnWidth ELSE IF dTextWidth GT dMinBtnWidth THEN dTextWidth ELSE dMinBtnWidth
      dButtonsWidth = INTEGER(btn1:VISIBLE) * btn1:WIDTH + INTEGER(btn2:VISIBLE) * btn2:WIDTH + INTEGER(btn3:VISIBLE) * btn3:WIDTH + INTEGER(btn4:VISIBLE) * btn4:WIDTH 
      NO-ERROR.

  dPos = (dWidth - dButtonsWidth) / (MIN(NUM-ENTRIES(ipcOptions), 4) + 1).

  ASSIGN
      btn1:COL = 1 * dPos
      btn2:COL = 2 * dPos + btn1:WIDTH
      btn3:COL = 3 * dPos + btn1:WIDTH + btn2:WIDTH 
      btn4:COL = 4 * dPos + btn1:WIDTH + btn2:WIDTH + btn3:WIDTH
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
  ENABLE messageText btn1 btn2 btn3 btn4 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

