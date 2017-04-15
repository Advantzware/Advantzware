&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\d-smtrx2.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-smtrx-recid AS RECID NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

DEF BUFFER bf-smtrx FOR smanmtrx.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cat end_cat btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cat end_cat v-status 

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

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Copy From Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Copy To Category" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE v-status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     begin_cat AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter From Category"
     end_cat AT ROW 3.86 COL 67 COLON-ALIGNED HELP
          "Enter To Category"
     v-status AT ROW 7.67 COL 2 NO-LABEL
     btn-process AT ROW 9.33 COL 21
     btn-cancel AT ROW 9.33 COL 60
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.71 COL 6
     RECT-17 AT ROW 1.24 COL 2
     SPACE(2.39) SKIP(3.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Copy Sales Rep Matrix".


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

/* SETTINGS FOR FILL-IN v-status IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Copy Sales Rep Matrix */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "begin_cat" OR WHEN "end_cat" THEN DO:
      /*RUN lookups/procat.p.
      IF g_lookup-var NE '' THEN
      ASSIGN
        smanmtrx.procat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = g_lookup-var
        procatDscr:SCREEN-VALUE = procatDscr(smanmtrx.company,smanmtrx.procat:SCREEN-VALUE).
      APPLY 'ENTRY':U TO smanmtrx.procat IN BROWSE {&BROWSE-NAME}.
    END.                     */
      run windows/l-fgcat.w (g_company,focus:SCREEN-VALUE, output char-val).
      if char-val <> "" then 
         assign focus:SCREEN-VALUE = entry(1,char-val).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Copy Sales Rep Matrix */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel Dialog-Frame
ON CHOOSE OF btn-cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process Dialog-Frame
ON CHOOSE OF btn-process IN FRAME Dialog-Frame /* Start Process */
DO:
  DEF VAR v-process AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN validate-cat NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.

  MESSAGE "Are you sure you want to copy category entered? " VIEW-AS ALERT-BOX QUESTION
      BUTTON YES-NO UPDATE v-process.

  IF v-process THEN RUN run-process.
  ELSE APPLY "choose" TO btn-cancel.

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
  
  FIND FIRST smanmtrx WHERE RECID(smanmtrx) = ip-smtrx-recid NO-LOCK.
  ASSIGN begin_cat = smanmtrx.procat.

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
  DISPLAY begin_cat end_cat v-status 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-17 begin_cat end_cat btn-process btn-cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process Dialog-Frame 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b2-smtrx FOR smanmtrx.

SESSION:SET-WAIT-STATE("general").

FOR EACH bf-smtrx NO-LOCK WHERE bf-smtrx.company = g_company
                            AND bf-smtrx.sman = smanmtrx.sman
                            AND bf-smtrx.custype = smanmtrx.custype
                            AND bf-smtrx.procat = begin_cat :
    CREATE b2-smtrx.
    BUFFER-COPY bf-smtrx EXCEPT bf-smtrx.procat bf-smtrx.rec_key TO b2-smtrx.
    ASSIGN b2-smtrx.procat = END_cat.
    v-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Coping category from " + bf-smtrx.procat +
                            " to " + END_cat + ", commission% " + string(b2-smtrx.comm) .
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE("").

MESSAGE " Process Is Completed." VIEW-AS ALERT-BOX.

APPLY "go" TO FRAME {&frame-name}.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-cat Dialog-Frame 
PROCEDURE validate-cat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF /*begin_cat:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" AND*/
     NOT CAN-FIND(FIRST fgcat WHERE fgcat.company EQ g_company
                          AND fgcat.procat EQ end_cat:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
     MESSAGE "Invalid To Category. Try help." VIEW-AS ALERT-BOX ERROR.
     APPLY 'entry' TO END_cat.
     RETURN ERROR.
  END.

  IF NOT CAN-FIND(FIRST bf-smtrx WHERE bf-smtrx.company = g_company 
                                  AND bf-smtrx.sman = smanmtrx.sman
                                  AND bf-smtrx.custype = smanmtrx.custype
                                  AND bf-smtrx.procat = begin_cat:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid category for the customer type to copy. Try help." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

  IF CAN-FIND(FIRST bf-smtrx WHERE bf-smtrx.company = g_company 
                               AND bf-smtrx.sman = smanmtrx.sman
                               AND bf-smtrx.custype = smanmtrx.custype
                               AND bf-smtrx.procat = END_cat:SCREEN-VALUE) THEN DO:
      MESSAGE "Already category exists. Try help." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

