&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: dataCollection.w

  Description: Touch Screen Data Collection

  Input Parameters: ttblJob rowids

  Output Parameters: <none>

  Author: Ron Stark

  Created: 3.9.2005

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

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipRowIDs AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipRowIDs AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lvCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lvMCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvEndTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES machtran

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 machtran.jobseq ~
machtran.charge_code machtran.machine machtran.form_number ~
machtran.blank_number machtran.pass_sequence machtran.start_date ~
STRING(machtran.start_time,'HH:MM am') @ lvStartTime machtran.end_date ~
STRING(machtran.end_time,'HH:MM am') @ lvEndTime machtran.shift ~
machtran.completed machtran.posted machtran.run_qty machtran.waste_qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH machtran ~
      WHERE machtran.company EQ lvCompany AND ~
(machtran.machine EQ resources OR ~
resources EQ '<Select ...>') AND ~
machtran.job_number EQ lvJobNo AND ~
machtran.job_sub EQ lvJobNo2 ~
USE-INDEX pi-machtran NO-LOCK ~
    BY machtran.machine ~
       BY machtran.start_date ~
        BY machtran.start_time ~
         BY machtran.jobseq INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH machtran ~
      WHERE machtran.company EQ lvCompany AND ~
(machtran.machine EQ resources OR ~
resources EQ '<Select ...>') AND ~
machtran.job_number EQ lvJobNo AND ~
machtran.job_sub EQ lvJobNo2 ~
USE-INDEX pi-machtran NO-LOCK ~
    BY machtran.machine ~
       BY machtran.start_date ~
        BY machtran.start_time ~
         BY machtran.jobseq INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 machtran
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 machtran


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS resources BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS resources 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE resources AS CHARACTER FORMAT "X(256)":U INITIAL "<Select ...>" 
     LABEL "&Resource" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEMS "<Select ...>" 
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      machtran SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      machtran.jobseq FORMAT ">9":U
      machtran.charge_code FORMAT "X(5)":U
      machtran.machine COLUMN-LABEL "Resource" FORMAT "x(6)":U
      machtran.form_number FORMAT ">>9":U
      machtran.blank_number FORMAT ">9":U
      machtran.pass_sequence FORMAT ">>9":U
      machtran.start_date FORMAT "99/99/9999":U
      STRING(machtran.start_time,'HH:MM am') @ lvStartTime COLUMN-LABEL "Start" FORMAT "X(8)":U
            WIDTH 10
      machtran.end_date FORMAT "99/99/9999":U
      STRING(machtran.end_time,'HH:MM am') @ lvEndTime COLUMN-LABEL "End" FORMAT "X(8)":U
            WIDTH 10
      machtran.shift FORMAT "X":U
      machtran.completed FORMAT "yes/no":U
      machtran.posted FORMAT "yes/no":U
      machtran.run_qty FORMAT "->>,>>>,>>9":U
      machtran.waste_qty FORMAT "->>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 134 BY 21.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     resources AT ROW 1 COL 92.2 HELP
          "Select Resource"
     BROWSE-1 AT ROW 2 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 134 BY 22.52.


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
         TITLE              = "Data Collection"
         HEIGHT             = 22.52
         WIDTH              = 134
         MAX-HEIGHT         = 22.52
         MAX-WIDTH          = 134
         VIRTUAL-HEIGHT     = 22.52
         VIRTUAL-WIDTH      = 134
         MAX-BUTTON         = no
         TOP-ONLY           = yes
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
IF NOT C-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 resources DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX resources IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "machtran"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "machtran.machine|yes,machtran.start_date|yes,machtran.start_time|yes,machtran.jobseq|yes"
     _Where[1]         = "machtran.company EQ lvCompany AND
(machtran.machine EQ resources OR
resources EQ '<Select ...>') AND
machtran.job_number EQ lvJobNo AND
machtran.job_sub EQ lvJobNo2
USE-INDEX pi-machtran"
     _FldNameList[1]   = machtran.jobseq
     _FldNameList[2]   = machtran.charge_code
     _FldNameList[3]   > machtran.machine
"machtran.machine" "Resource" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = machtran.form_number
     _FldNameList[5]   = machtran.blank_number
     _FldNameList[6]   = machtran.pass_sequence
     _FldNameList[7]   = machtran.start_date
     _FldNameList[8]   > "_<CALC>"
"STRING(machtran.start_time,'HH:MM am') @ lvStartTime" "Start" "X(8)" ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[9]   = machtran.end_date
     _FldNameList[10]   > "_<CALC>"
"STRING(machtran.end_time,'HH:MM am') @ lvEndTime" "End" "X(8)" ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[11]   = machtran.shift
     _FldNameList[12]   = machtran.completed
     _FldNameList[13]   = machtran.posted
     _FldNameList[14]   = machtran.run_qty
     _FldNameList[15]   = machtran.waste_qty
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Data Collection */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Data Collection */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME resources
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL resources C-Win
ON VALUE-CHANGED OF resources IN FRAME DEFAULT-FRAME /* Resource */
DO:
  ASSIGN {&SELF-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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
  FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ TO-ROWID(ENTRY(2,ipRowIDs)) NO-ERROR.
  IF AVAILABLE job-mch THEN
  ASSIGN
    lvCompany = job-mch.company
    lvJobNo = job-mch.job-no
    lvJobNo2 = job-mch.job-no2
    lvMCode = job-mch.m-code
    {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + ' - Job: ' +
                           LEFT-TRIM(lvJobNo) + '-' + STRING(lvJobNo2) + '.' +
                           STRING(job-mch.frm).
  RUN getResources.
  RUN enable_UI.
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
  DISPLAY resources 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE resources BROWSE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getResources C-Win 
PROCEDURE getResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {{&includes}/getResources.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

