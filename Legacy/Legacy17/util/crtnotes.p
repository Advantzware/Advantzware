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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tg_notes tg_title tg_eservice Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tg_notes v-f-cust v-t-cust tb_override ~
tg_title v-erel-fcust tg_eservice 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tg_notes v-f-cust v-t-cust tb_override 
&Scoped-define List-2 tg_title v-erel-fcust 
&Scoped-define List-3 tg_eservice 

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

DEFINE VARIABLE v-erel-fcust AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Customer" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

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
     SIZE 88 BY 3.81.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 2.62.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.43.

DEFINE VARIABLE tb_override AS LOGICAL INITIAL no 
     LABEL "Override Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_eservice AS LOGICAL INITIAL no 
     LABEL "Copy the Customer Type Eservice to ALL Customers?" 
     VIEW-AS TOGGLE-BOX
     SIZE 65 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tg_notes AS LOGICAL INITIAL no 
     LABEL "Copy Department Notes From Customer to Estimates?" 
     VIEW-AS TOGGLE-BOX
     SIZE 65 BY 1.19
     FONT 6 NO-UNDO.

DEFINE VARIABLE tg_title AS LOGICAL INITIAL no 
     LABEL "Copy ERelease Note to All Customer?" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tg_notes AT ROW 1.48 COL 15
     v-f-cust AT ROW 2.91 COL 35 COLON-ALIGNED
     v-t-cust AT ROW 2.91 COL 62 COLON-ALIGNED
     tb_override AT ROW 4.1 COL 33
     tg_title AT ROW 5.52 COL 15
     v-erel-fcust AT ROW 6.71 COL 38 COLON-ALIGNED
     tg_eservice AT ROW 8.38 COL 15
     Btn_OK AT ROW 10.05 COL 19
     Btn_Cancel AT ROW 10.05 COL 61
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 1.24 COL 2
     RECT-3 AT ROW 5.29 COL 2
     RECT-4 AT ROW 8.14 COL 2
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_override IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg_eservice IN FRAME DEFAULT-FRAME
   3                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg_notes IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg_title IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR FILL-IN v-erel-fcust IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
   IF tg_title THEN lc-msg = lc-msg + CHR(10) + tg_title:LABEL.
   IF tg_eservice THEN lc-msg = lc-msg + CHR(10) + tg_eservice:LABEL.

   IF lc-msg NE '' THEN DO:
     MESSAGE 'Are you sure you want to:' SKIP lc-msg VIEW-AS ALERT-BOX
       QUESTION BUTTONS YES-NO UPDATE ll-ans.
     IF ll-ans THEN DO:
       SESSION:SET-WAIT-STATE("general").
       IF tg_notes THEN RUN trans-notes.
       IF tg_title THEN RUN trans-phone.
       IF tg_eservice THEN RUN trans-eservice.
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg_eservice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_eservice C-Win
ON VALUE-CHANGED OF tg_eservice IN FRAME DEFAULT-FRAME /* Copy the Customer Type Eservice to ALL Customers? */
DO:
  ASSIGN tg_title.
  IF tg_title THEN do:
     v-erel-fcust:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "entry" TO v-erel-fcust.
  END.
  ELSE DO:
     ASSIGN v-erel-fcust:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            v-erel-fcust:SCREEN-VALUE = "".
  END.

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


&Scoped-define SELF-NAME tg_title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_title C-Win
ON VALUE-CHANGED OF tg_title IN FRAME DEFAULT-FRAME /* Copy ERelease Note to All Customer? */
DO:
  ASSIGN tg_title.
  IF tg_title THEN do:
     v-erel-fcust:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY "entry" TO v-erel-fcust.
  END.
  ELSE DO:
     ASSIGN v-erel-fcust:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            v-erel-fcust:SCREEN-VALUE = "".
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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
  APPLY "entry" TO FRAME {&FRAME-NAME}.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY tg_notes v-f-cust v-t-cust tb_override tg_title v-erel-fcust 
          tg_eservice 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tg_notes tg_title tg_eservice Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trans-eservice C-Win 
PROCEDURE trans-eservice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH cust WHERE cust.company EQ g_company:
    IF CAN-DO('E,I,X',cust.active) THEN NEXT.
    cust.active = 'E'.
  END. /* each cust */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE trans-phone C-Win 
PROCEDURE trans-phone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-cust FOR cust.
  DEF BUFFER bf-phone FOR phone.
  DEF BUFFER bx-phone FOR phone.

  FIND FIRST bf-cust WHERE bf-cust.company = g_company
                       AND bf-cust.cust-no = v-erel-fcust NO-LOCK NO-ERROR.

  FOR EACH cust WHERE cust.company = g_company NO-LOCK:
      IF cust.cust-no <> v-erel-fcust THEN DO:
         STATUS DEFAULT "Updating ERelease for " + cust.cust-no .

         FOR EACH phone WHERE phone.table_rec_key = bf-cust.rec_key 
                          AND phone.titlcode = "ERELEASE" NO-LOCK :
             FIND FIRST bf-phone WHERE bf-phone.TABLE_rec_key = cust.rec_key
                                   AND bf-phone.titlcode = phone.titlcode 
                                   AND bf-phone.attention = phone.attention NO-ERROR.
             IF NOT AVAIL bf-phone THEN DO:                
                FIND FIRST bx-phone WHERE bx-phone.TABLE_rec_key = cust.rec_key
                                      AND bx-phone.attention = phone.attention NO-ERROR.                
                CREATE bf-phone.
                BUFFER-COPY phone EXCEPT phone.rec_key phone.attention TO bf-phone.
                ASSIGN bf-phone.TABLE_rec_key = cust.rec_key
                       bf-phone.attention = IF AVAIL bx-phone THEN phone.titlcode + " " + phone.attention
                                            ELSE phone.attention.                  

             END.
         END.

      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

