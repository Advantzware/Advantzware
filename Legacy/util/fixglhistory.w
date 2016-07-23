&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
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
{methods/defines/hndldefs.i}
{custom/globdefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

cocode = g_company.

DEFINE TEMP-TABLE tt-gltrans NO-UNDO LIKE gltrans
   FIELD gltrans-flag AS LOG
   FIELD acct-dscr LIKE account.dscr
   FIELD tt-rowid AS ROWID
   INDEX tt-gltrans gltrans-flag DESC actnum ASC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-gltrans

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-gltrans.actnum tt-gltrans.acct-dscr tt-gltrans.jrnl tt-gltrans.tr-dscr tt-gltrans.tr-date tt-gltrans.tr-amt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 tt-gltrans.tr-amt   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 tt-gltrans
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 tt-gltrans
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-gltrans
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH tt-gltrans.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-gltrans
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-gltrans


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_run-no BUTTON-1 BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS begin_run-no 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Populate Browser" 
     SIZE 22 BY 1.14.

DEFINE VARIABLE begin_run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Run#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      tt-gltrans SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      tt-gltrans.actnum FORMAT "X(14)" COLUMN-LABEL "Account Number"
 tt-gltrans.acct-dscr FORMAT "X(28)" COLUMN-LABEL "Account Description"
 tt-gltrans.jrnl COLUMN-LABEL "Journal"
 tt-gltrans.tr-dscr COLUMN-LABEL "Reference" WIDTH 50
 tt-gltrans.tr-date COLUMN-LABEL "Date"
 tt-gltrans.tr-amt COLUMN-LABEL "Balance" FORMAT "->,>>>,>>9.99"

 ENABLE tt-gltrans.tr-amt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 12.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_run-no AT ROW 1.48 COL 8 COLON-ALIGNED HELP
          "Enter Beginning Run Number"
     BUTTON-1 AT ROW 1.48 COL 29
     BROWSE-1 AT ROW 3.14 COL 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130 BY 15.38.


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
         TITLE              = "Fix G/L History"
         HEIGHT             = 15.38
         WIDTH              = 130
         MAX-HEIGHT         = 15.38
         MAX-WIDTH          = 130
         VIRTUAL-HEIGHT     = 15.38
         VIRTUAL-WIDTH      = 130
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
/* BROWSE-TAB BROWSE-1 BUTTON-1 FRAME-A */
ASSIGN 
       begin_run-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-gltrans.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix G/L History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix G/L History */
DO:
  /* This event will close the window and terminate the procedure.  */
  DEF VAR confirm AS LOG NO-UNDO.

  MESSAGE "Do you wish to update G/L Account Balances?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm.

  IF confirm THEN
     RUN util\fxacctg3.p.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_run-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run-no C-Win
ON HELP OF begin_run-no IN FRAME FRAME-A /* Run# */
DO:
  DEF VAR lv AS CHAR NO-UNDO.
  lv = {&self-name}:SCREEN-VALUE.
  RUN run-no-help (INPUT-OUTPUT lv).
  {&self-name}:SCREEN-VALUE = lv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run-no C-Win
ON LEAVE OF begin_run-no IN FRAME FRAME-A /* Run# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME FRAME-A /* Populate Browser */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN begin_run-no.

      EMPTY TEMP-TABLE tt-gltrans.

      FOR EACH gltrans WHERE
          gltrans.company EQ cocode AND
          gltrans.trnum   EQ begin_run-no
          NO-LOCK,
          FIRST account fields(dscr) WHERE
                account.company EQ cocode AND
                account.actnum  EQ gltrans.actnum
                NO-LOCK:

          CREATE tt-gltrans.
          BUFFER-COPY gltrans TO tt-gltrans
             ASSIGN tt-gltrans.gltrans-flag = YES
                    tt-gltrans.acct-dscr = account.dscr
                    tt-gltrans.tt-rowid = ROWID(gltrans).
          RELEASE tt-gltrans.
      END.

      FOR EACH glhist WHERE
          glhist.company EQ cocode AND
          glhist.tr-num   EQ begin_run-no
          NO-LOCK:

          CREATE tt-gltrans.
          BUFFER-COPY glhist TO tt-gltrans
             ASSIGN tt-gltrans.gltrans-flag = NO
                    tt-gltrans.tt-rowid = ROWID(glhist).
          RELEASE tt-gltrans.
      END.

      OPEN QUERY browse-1 FOR EACH tt-gltrans.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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

SESSION:DATA-ENTRY-RETURN = YES.

ON 'leave':U OF tt-gltrans.tr-amt DO:
  IF LASTKEY NE -1 THEN DO:
    IF AVAIL tt-gltrans AND
       DEC(tt-gltrans.tr-amt:SCREEN-VALUE IN BROWSE {&browse-name}) NE
       tt-gltrans.tr-amt THEN DO:
        
       IF tt-gltrans.gltrans-flag THEN
       DO:
          FIND FIRST gltrans WHERE
               ROWID(gltrans) EQ tt-gltrans.tt-rowid
               EXCLUSIVE-LOCK.

          gltrans.tr-amt = DEC(tt-gltrans.tr-amt:SCREEN-VALUE IN BROWSE {&browse-name}).
               
          FIND CURRENT gltrans NO-LOCK.
       END.
       ELSE
       DO:
          FIND FIRST glhist WHERE
               ROWID(glhist) EQ tt-gltrans.tt-rowid
               EXCLUSIVE-LOCK.

          glhist.tr-amt = DEC(tt-gltrans.tr-amt:SCREEN-VALUE IN BROWSE {&browse-name}).
               
          FIND CURRENT glhist NO-LOCK.
       END.
    END.
    
  END.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

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
  DISPLAY begin_run-no 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_run-no BUTTON-1 BROWSE-1 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

