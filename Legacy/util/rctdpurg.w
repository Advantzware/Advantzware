&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util/rctd-purge.w

  Description: Delete "C" status receipts

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: S Brooks

  Created: 12/28/11

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.
DEFINE VARIABLE cPostLst       AS CHARACTER     NO-UNDO.
DEFINE VARIABLE ll-secure AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_seq end_seq dt-from dt-to ~
t-receipt rd_posting t-ship t-trans t-adj t-ret tgIssue tbphycount ~
btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_seq end_seq dt-from dt-to t-receipt ~
rd_posting t-ship t-trans t-adj t-ret tgIssue tbphycount lv-file-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valid-seq C-Win 
FUNCTION valid-seq RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE begin_seq AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Seq#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE dt-from AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE dt-to AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_seq AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Ending Seq#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-file-name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 59.2 BY 1 NO-UNDO.

DEFINE VARIABLE rd_posting AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Posted", "P",
"Unposted", "U"
     SIZE 28.8 BY .91 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 13.1.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ret AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-ship AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE tbphycount AS LOGICAL INITIAL no 
     LABEL "Physical count" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.

DEFINE VARIABLE tgIssue AS LOGICAL INITIAL no 
     LABEL "Issue Farm Outs" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_seq AT ROW 6.33 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_seq AT ROW 6.33 COL 63 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     dt-from AT ROW 7.43 COL 27 COLON-ALIGNED HELP
          "Enter the From Date" WIDGET-ID 2
     dt-to AT ROW 7.43 COL 63 COLON-ALIGNED HELP
          "Enter the To Date" WIDGET-ID 4
     t-receipt AT ROW 9.86 COL 28.4 WIDGET-ID 38
     rd_posting AT ROW 10 COL 55.2 NO-LABEL WIDGET-ID 46
     t-ship AT ROW 10.71 COL 28.4 WIDGET-ID 42
     t-trans AT ROW 11.57 COL 28.4 WIDGET-ID 44
     t-adj AT ROW 12.48 COL 28.4 WIDGET-ID 36
     t-ret AT ROW 13.38 COL 28.4 WIDGET-ID 40
     tgIssue AT ROW 14.24 COL 28.4 WIDGET-ID 34
     tbphycount AT ROW 15.1 COL 28.4 WIDGET-ID 50
     lv-file-name AT ROW 16.43 COL 25.8 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     btn-process AT ROW 18.19 COL 21
     btn-cancel AT ROW 18.19 COL 53
     "Dump File:" VIEW-AS TEXT
          SIZE 11.6 BY .62 AT ROW 16.62 COL 15 WIDGET-ID 54
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     "Transaction Types :" VIEW-AS TEXT
          SIZE 20.8 BY .95 AT ROW 8.81 COL 28.6 WIDGET-ID 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 18.91.

DEFINE FRAME FRAME-B
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
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
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Purge Transactions"
         HEIGHT             = 18.91
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN 
       dt-from:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       dt-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-file-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       t-adj:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-receipt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ret:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       t-trans:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tbphycount:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgIssue:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Purge Counts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Purge Counts */
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
    run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-ship
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-ship C-Win
ON VALUE-CHANGED OF t-ship IN FRAME FRAME-A /* Shipments */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
DO:
      ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbphycount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbphycount C-Win
ON VALUE-CHANGED OF tbphycount IN FRAME FRAME-A /* Physical count */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgIssue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgIssue C-Win
ON VALUE-CHANGED OF tgIssue IN FRAME FRAME-A /* Issue Farm Outs */
DO:
  ASSIGN {&self-name}.
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
    /* check security */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  FIND ap-ctrl WHERE ap-ctrl.company = gcompany NO-LOCK NO-ERROR.
  
  
  RUN enable_UI.

   DO WITH FRAME {&FRAME-NAME}:
   rd_posting:SCREEN-VALUE = "U" .
   END.
  
  {methods/nowait.i}
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
  DISPLAY begin_seq end_seq dt-from dt-to t-receipt rd_posting t-ship t-trans 
          t-adj t-ret tgIssue tbphycount lv-file-name 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_seq end_seq dt-from dt-to t-receipt rd_posting t-ship 
         t-trans t-adj t-ret tgIssue tbphycount btn-process btn-cancel 
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
/*****************************************************   */
/****************************************************************************/

/* def var v-post-date as date init today no-undo.  */
DEF var v-first-seq like fg-rctd.r-no no-undo.
DEF var v-last-seq like fg-rctd.r-no no-undo.
DEFINE VARIABLE cFileName AS CHARACTER FORMAT "x(200)" NO-UNDO .
DEFINE VARIABLE ctmp-dir AS CHARACTER NO-UNDO .

DEF BUFFER b-fg-rctd FOR fg-rctd.

 FIND FIRST users WHERE
        users.user_id EQ USERID(ldbname(1))
        NO-LOCK NO-ERROR.

   IF AVAIL users AND users.user_program[2] NE "" THEN
      ctmp-dir = users.user_program[2].
   ELSE
      ctmp-dir = "c:\tmp".

IF NOT valid-seq() THEN RETURN.

 ASSIGN cFileName = ctmp-dir + "\RctdPurge" + STRING(TIME) + ".d" . 

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   begin_seq
   end_seq.
END.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.


ASSIGN
 v-first-seq = begin_seq
 v-last-seq  = end_seq
 v-process   = no.

cPostLst   =  (IF t-receipt THEN "R," ELSE "") +
               (IF t-ship THEN "S," ELSE "") +
               (IF t-trans THEN "T," ELSE "") +
               (IF t-adj THEN "A," ELSE "") +
               (IF t-ret THEN "E," ELSE "") +
               (IF tgIssue THEN "F," ELSE "") +
               (IF tbphycount THEN "C" ELSE "") .

IF LENGTH(cPostLst) GT 0 AND
    SUBSTR(cPostLst,LENGTH(cPostLst),1) EQ "," THEN
    SUBSTR(cPostLst,LENGTH(cPostLst),1) = "".

MESSAGE "Are you sure you want to delete within the " +
        "selection parameters?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

IF NOT v-process THEN RETURN.

IF dt-from LE TODAY - 183 THEN do: 
IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
END.
ELSE ll-secure = YES .
 
  IF ll-secure THEN do:

      SESSION:set-wait-state("General").

      OUTPUT TO value(cFileName)  .

      FOR EACH fg-rctd
          WHERE fg-rctd.company  EQ cocode
            AND fg-rctd.r-no   ge v-first-seq
            AND fg-rctd.r-no   le v-last-seq 
            AND fg-rctd.rct-date  GE dt-from
            AND fg-rctd.rct-date  LE dt-to
            AND fg-rctd.rita-code NE "" 
            AND ( (LOOKUP(fg-rctd.rita-code,cPostLst) GT 0 AND rd_posting EQ "U" ) OR
                 (fg-rctd.rita-code EQ "P" AND rd_posting EQ "P")) EXCLUSIVE TRANSACTION:

            EXPORT fg-rctd .
    
          DELETE fg-rctd.
      END. /* for each fg-rctd */
      
      OUTPUT CLOSE.
      lv-file-name:SCREEN-VALUE = cFileName .
      SESSION:set-wait-state("").

      MESSAGE TRIM(c-win:TITLE) + " Process Is Completed." VIEW-AS ALERT-BOX.
      APPLY "close" TO THIS-PROCEDURE.

      RETURN.
  END.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valid-seq C-Win 
FUNCTION valid-seq RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

      IF int(begin_seq:SCREEN-VALUE) = ? THEN DO:
          MESSAGE "Invalid beginning sequence."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN FALSE.
      END.

      IF int(end_seq:SCREEN-VALUE) = ? THEN DO:
          MESSAGE "Invalid ending sequence."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN FALSE.
      END.

      IF int(begin_seq:SCREEN-VALUE) > int(end_seq:SCREEN-VALUE) THEN DO:
          MESSAGE "Beginning sequence cannot exceed ending sequence."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN FALSE.
      END.
  END.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

