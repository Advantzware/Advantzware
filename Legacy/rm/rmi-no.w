&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: addon\rm\rmi-no.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF INPUT PARAMETER ip-company AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-i-no AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-40 scr-rm-i-no btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS scr-rm-i-no scr-rm-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE scr-rm-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "RM Item #" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE scr-rm-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "RM Item Name" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 37.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     scr-rm-i-no AT ROW 1.95 COL 15.6 COLON-ALIGNED
     scr-rm-name AT ROW 3.1 COL 15.6 COLON-ALIGNED
     btn-ok AT ROW 4.48 COL 17.6
     btn-cancel AT ROW 4.48 COL 33.2
     RECT-40 AT ROW 1 COL 1
     SPACE(3.19) SKIP(0.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select RM Item".


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

/* SETTINGS FOR FILL-IN scr-rm-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select RM Item */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   APPLY "WINDOW-CLOSE" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok Dialog-Frame
ON CHOOSE OF btn-ok IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:

     IF NOT CAN-FIND(FIRST ITEM WHERE
        ITEM.company EQ ip-company AND
        ITEM.i-no EQ scr-rm-i-no:SCREEN-VALUE) THEN
        DO:
           MESSAGE "Invalid RM Item #."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO scr-rm-i-no IN FRAME {&FRAME-NAME}.
           RETURN NO-APPLY.
        END.

     op-i-no = scr-rm-i-no:SCREEN-VALUE.

     APPLY "WINDOW-CLOSE" TO FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-rm-i-no Dialog-Frame
ON HELP OF scr-rm-i-no IN FRAME Dialog-Frame /* RM Item # */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR look-recid AS RECID NO-UNDO.

   RUN windows/l-itmall.w (ip-company, "","", scr-rm-i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
   IF char-val NE "" AND ENTRY(1,char-val) NE scr-rm-i-no:SCREEN-VALUE THEN
      ASSIGN scr-rm-i-no:SCREEN-VALUE = ENTRY(1,char-val)
             scr-rm-name:SCREEN-VALUE = ENTRY(2,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-rm-i-no Dialog-Frame
ON LEAVE OF scr-rm-i-no IN FRAME Dialog-Frame /* RM Item # */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      FIND FIRST ITEM WHERE
           ITEM.company EQ ip-company AND
           ITEM.i-no EQ scr-rm-i-no:SCREEN-VALUE
           NO-LOCK NO-ERROR.

      IF AVAIL ITEM THEN
         scr-rm-name:SCREEN-VALUE = ITEM.i-name.
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
  DISPLAY scr-rm-i-no scr-rm-name 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-40 scr-rm-i-no btn-ok btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

