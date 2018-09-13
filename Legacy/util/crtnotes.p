&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tg_notes Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tg_notes v-f-cust v-t-cust tb_override 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tg_notes v-f-cust v-t-cust tb_override 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "Start Process" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE v-f-cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer Range Begin" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE VARIABLE v-t-cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "End" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 10.48.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 6.24.

DEFINE VARIABLE tb_override AS LOGICAL INITIAL no 
     LABEL "Override Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_notes AS LOGICAL INITIAL no 
     LABEL "Copy Department Notes From Customer to Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 65 BY 1.19
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tg_notes AT ROW 2.48 COL 15
     v-f-cust AT ROW 4.38 COL 35 COLON-ALIGNED
     v-t-cust AT ROW 4.38 COL 62 COLON-ALIGNED
     tb_override AT ROW 6.05 COL 33
     Btn_OK AT ROW 9.29 COL 19
     Btn_Cancel AT ROW 9.29 COL 61
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 1.91 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 10.67.


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
         TITLE              = "Copy Customer Notes"
         HEIGHT             = 10.67
         WIDTH              = 90
         MAX-HEIGHT         = 10.67
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 10.67
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_override IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg_notes IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN v-f-cust IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v-t-cust IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Copy Customer Notes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Copy Customer Notes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Start Process */
DO:
   DEF VAR ll-ans AS LOG NO-UNDO.
   DEF VAR lc-msg AS CHAR NO-UNDO.

   ASSIGN {&list-1} {&list-2} {&list-3}.

   IF tg_notes THEN lc-msg = lc-msg + CHR(10) + tg_notes:LABEL.  
   

   IF lc-msg NE '' THEN DO:
     MESSAGE 'Are you sure you want to:' SKIP lc-msg VIEW-AS ALERT-BOX
       QUESTION BUTTONS YES-NO UPDATE ll-ans.
     IF ll-ans THEN DO:
       SESSION:SET-WAIT-STATE("general").
       IF tg_notes THEN RUN trans-notes.
      
       SESSION:SET-WAIT-STATE("").
       MESSAGE "Completed." VIEW-AS ALERT-BOX.
     END.
   END.
   /*
   IF tg_notes AND tg_title THEN
      MESSAGE "Are you sure you want to copy Dept notes and ERelease Note?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
   ELSE IF tg_notes THEN
      MESSAGE "Are you sure you want to copy Dept notes?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
   ELSE IF tg_title THEN
      MESSAGE "Are you sure you want to copy ERelease Note?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_notes C-Win
ON VALUE-CHANGED OF tg_notes IN FRAME DEFAULT-FRAME /* Copy Department Notes From Customer to Estimates? */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN tg_notes.

    IF tg_notes THEN DO:
      ASSIGN
       v-f-cust:SENSITIVE    = YES
       v-t-cust:SENSITIVE    = YES
       tb_override:SENSITIVE = YES
       v-t-cust:SCREEN-VALUE = "zzzzzzzz".

      APPLY "entry" TO v-f-cust.
    END.

    ELSE
      ASSIGN
       v-f-cust:SENSITIVE       = NO
       v-t-cust:SENSITIVE       = NO
       tb_override:SENSITIVE    = NO
       v-f-cust:SCREEN-VALUE    = ""
       v-t-cust:SCREEN-VALUE    = ""
       tb_override:SCREEN-VALUE = "no".
  END.
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
     /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.
  RUN enable_UI.
  {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    ASSIGN tg_notes.
    ASSIGN 
        tg_notes = YES 
        tg_notes:SCREEN-VALUE = "Yes" .
    IF tg_notes THEN DO:
      ASSIGN
       v-f-cust:SENSITIVE    = YES
       v-t-cust:SENSITIVE    = YES
       tb_override:SENSITIVE = YES
       v-t-cust:SCREEN-VALUE = "zzzzzzzz".

      APPLY "entry" TO v-f-cust.
    END.
   END.

  APPLY "entry" TO FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY tg_notes v-f-cust v-t-cust tb_override 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tg_notes Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trans-notes C-Win 
PROCEDURE trans-notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bf-notes FOR notes.
 
 FOR EACH cust WHERE cust.company = g_company 
                 AND cust.cust-no >= v-f-cust 
                 AND cust.cust-no <= v-t-cust NO-LOCK,
     each eb WHERE eb.company = cust.company
                 AND eb.cust-no = cust.cust-no NO-LOCK,
     FIRST est WHERE est.company = cust.company
                 AND est.est-no = eb.est-no NO-LOCK:

  STATUS DEFAULT "Updating Notes for " + cust.cust-no + ", Est#:" + eb.est-no.

         for each notes where notes.rec_key   eq cust.rec_key
                          and notes.note_type eq "D"
                          and notes.note_code ne "" no-lock
             TRANSACTION:

             find first bf-notes where bf-notes.rec_key   eq est.rec_key
                                   and bf-notes.note_type eq notes.note_type
                                   and bf-notes.note_code eq notes.note_code 
                                   no-error.
             if not avail bf-notes then create bf-notes.

             IF NEW bf-notes OR tb_override THEN DO:
                   buffer-copy notes except notes.note_form_no to bf-notes
                   ASSIGN bf-notes.rec_key   = est.rec_key
                          bf-notes.note_date = today
                          bf-notes.note_time = time.
                   /*DISP eb.est-no WITH DOWN.
                     DOWN.
                     PAUSE 0.                      */
             END.
         END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

