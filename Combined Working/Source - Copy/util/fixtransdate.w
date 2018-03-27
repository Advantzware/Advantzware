&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*----------------------------------------------------------------------*/
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

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 t-post-date t-trans-date begin_i-no ~
end_i-no t-receipt t-adj t-ship t-ret t-trans t-count begin_date end_date ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS t-post-date t-trans-date begin_i-no ~
end_i-no t-receipt t-adj t-ship t-ret t-trans t-count begin_date end_date 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Replace Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U 
     LABEL "with" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 7.62.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-count AS LOGICAL INITIAL no 
     LABEL "Cycle Counts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-post-date AS LOGICAL INITIAL no 
     LABEL "Posted Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.8 BY .81 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-ret AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-ship AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE t-trans-date AS LOGICAL INITIAL yes 
     LABEL "Trans Date" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     t-post-date AT ROW 5.24 COL 36.2 WIDGET-ID 2
     t-trans-date AT ROW 5.24 COL 62.6 WIDGET-ID 4
     begin_i-no AT ROW 6.71 COL 23 COLON-ALIGNED HELP
          "Enter the Beginning FG Item Number"
     end_i-no AT ROW 6.71 COL 64 COLON-ALIGNED HELP
          "Enter the Ending FG Item Number"
     t-receipt AT ROW 8.14 COL 20
     t-adj AT ROW 8.14 COL 51
     t-ship AT ROW 8.86 COL 20
     t-ret AT ROW 8.86 COL 51
     t-trans AT ROW 9.57 COL 20
     t-count AT ROW 9.57 COL 51
     begin_date AT ROW 10.76 COL 30 COLON-ALIGNED
     end_date AT ROW 10.76 COL 52 COLON-ALIGNED
     btn-process AT ROW 12.91 COL 22
     btn-cancel AT ROW 12.91 COL 53
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 13.67.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fix FG History Trans Date"
         HEIGHT             = 13.76
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix FG History Trans Date */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix FG History Trans Date */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO: 
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  IF begin_date EQ end_date THEN DO:
    MESSAGE "Old and New Dates are the same, cannot process..."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY "entry" TO end_date.
    RETURN NO-APPLY.
  END.

  MESSAGE "Are you sure you want change date for all FG Transactions from"
          begin_date "to" end_date
          "for the selected parameters" + "?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll.

  IF ll THEN RUN run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
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

  ASSIGN
   begin_date = TODAY - 1
   end_date   = TODAY.

  RUN enable_UI.

  APPLY "entry" TO begin_i-no.

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
  DISPLAY t-post-date t-trans-date begin_i-no end_i-no t-receipt t-adj t-ship 
          t-ret t-trans t-count begin_date end_date 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 t-post-date t-trans-date begin_i-no end_i-no t-receipt t-adj 
         t-ship t-ret t-trans t-count begin_date end_date btn-process 
         btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
DEF VAR lv-codes AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.


SESSION:SET-WAIT-STATE("general").

lv-codes = (IF t-receipt THEN "R," ELSE "") +
           (IF t-ship THEN "S," ELSE "") +
           (IF t-trans THEN "T," ELSE "") +
           (IF t-adj THEN "A," ELSE "") +
           (IF t-ret THEN "E," ELSE "") +
           (IF t-count THEN "C," ELSE "").

IF LENGTH(lv-codes) GT 0 AND
   SUBSTR(lv-codes,LENGTH(lv-codes),1) EQ "," THEN
   SUBSTR(lv-codes,LENGTH(lv-codes),1) = "".

IF t-trans-date THEN
FOR EACH fg-rcpth
    WHERE fg-rcpth.company    EQ cocode
      AND fg-rcpth.trans-date EQ begin_date
      AND fg-rcpth.i-no       GE begin_i-no
      AND fg-rcpth.i-no       LE end_i-no
      AND CAN-DO(lv-codes,fg-rcpth.rita-code)
    USE-INDEX trans-date:

  fg-rcpth.trans-date = end_date.

  FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ fg-rcpth.r-no NO-ERROR.
  IF AVAIL fg-rctd THEN 
     ASSIGN
      fg-rctd.rct-date = fg-rcpth.trans-date
      fg-rctd.trans-time = TIME.
END.

IF t-post-date THEN
FOR EACH fg-rcpth
    WHERE fg-rcpth.company    EQ cocode
      AND fg-rcpth.post-date EQ begin_date
      AND fg-rcpth.i-no       GE begin_i-no
      AND fg-rcpth.i-no       LE end_i-no
      AND CAN-DO(lv-codes,fg-rcpth.rita-code)
    USE-INDEX post-date:

  fg-rcpth.post-date = end_date.

END.

SESSION:SET-WAIT-STATE("").

MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.

APPLY "close" TO THIS-PROCEDURE.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

