&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-inve&p.w

  Description: Invoice Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/07/02

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
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def new shared var v-post as log init NO NO-UNDO.
def new shared var v-trnum as INT NO-UNDO.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---".
def var time_stamp as ch.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
def var xap-acct as CHAR NO-UNDO.
def workfile work-gl
  field actnum  like account.actnum
  field debits  as dec
  field credits as dec.
DEF VAR lv-frt-total AS DEC NO-UNDO.  /* accum total */
def var v-postable as log init NO NO-UNDO.
DEF VAR v-fgpostgl AS LOG NO-UNDO.
{sa/sa-sls01.i}

do for ap-ctrl transaction:
  find first ap-ctrl where ap-ctrl.company eq cocode no-lock no-error.
  if not avail ap-ctrl then return.
  assign xap-acct = ap-ctrl.payables
         v-frt-acct = ap-ctrl.freight.
  release ap-ctrl.
end.

DO TRANSACTION:
  {sys/inc/fgpostgl.i}
  v-fgpostgl = fgpostgl NE "None".
END.




    DEF VAR hcol AS HANDLE NO-UNDO.
    DEF VAR winstate AS INT NO-UNDO.
    DEF TEMP-TABLE tt_size NO-UNDO FIELD wg_name AS cha
                                   FIELD wg_width AS DEC
                                   FIELD wg_height AS DEC
                                   FIELD wg_xpos AS DEC
                                   FIELD wg_ypos AS DEC
                                  INDEX wg_name IS PRIMARY wg_name.
    DEF BUFFER bf_size FOR tt_size.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_vend end_vend ~
begin_date end_date tb_sort rd-dest lines-per-page td-show-parm btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_vend end_vend ~
begin_date end_date lbl_sort tb_sort rd-dest lines-per-page td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "x(8)" 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Print G/L Acount Description?" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 5.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.24.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Print G/L Account Description?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.43 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.62 COL 44 COLON-ALIGNED
     begin_vend AT ROW 5.29 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 5.29 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_date AT ROW 6.24 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Date"
     end_date AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Invoice Date"
     lbl_sort AT ROW 8.62 COL 30 COLON-ALIGNED NO-LABEL
     tb_sort AT ROW 8.62 COL 62
     rd-dest AT ROW 12.67 COL 11 NO-LABEL
     lines-per-page AT ROW 13.14 COL 72 COLON-ALIGNED
     td-show-parm AT ROW 14.81 COL 58
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.71 COL 6
     RECT-6 AT ROW 11.48 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 19.71.


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
         TITLE              = "Vendor Invoices Edit/Post Register"
         HEIGHT             = 20.19
         WIDTH              = 95.4
         MAX-HEIGHT         = 20.19
         MAX-WIDTH          = 95.8
         VIRTUAL-HEIGHT     = 20.19
         VIRTUAL-WIDTH      = 95.8
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
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vendor Invoices Edit/Post Register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vendor Invoices Edit/Post Register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MAXIMIZED OF C-Win /* Vendor Invoices Edit/Post Register */
DO:

  ASSIGN winstate = 1 NO-ERROR.
  ASSIGN hcol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.

  FIND FIRST bf_size WHERE bf_size.wg_name = STRING(CURRENT-WINDOW) NO-ERROR.
  IF AVAIL bf_size THEN DO:           
      ASSIGN hcol:HEIGHT-PIXELS = (hcol:HEIGHT-PIXELS * CURRENT-WINDOW:HEIGHT-PIXELS) 
                             / bf_size.wg_height
              hcol:width-PIXELS = (hcol:width-PIXELS * CURRENT-WINDOW:WIDTH-PIXELS) 
                             / bf_size.wg_width .


       FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
       IF NOT AVAIL tt_size THEN DO:
           CREATE tt_size.
           assign tt_size.wg_name = STRING(hcol)
                  tt_size.wg_width = hcol:WIDTH-PIXELS
                  tt_size.wg_height = hcol:height-PIXELS
                  tt_size.wg_xpos = hcol:X
                  tt_size.wg_ypos = hcol:Y NO-ERROR.
       END.
       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       assign hcol = hcol:FIRST-CHILD NO-ERROR. 
       DO WHILE VALID-HANDLE(hcol):
          FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
          IF NOT AVAIL tt_size THEN DO:
             CREATE tt_size.
             assign tt_size.wg_name = STRING(hcol)
                  tt_size.wg_width = hcol:WIDTH-PIXELS
                  tt_size.wg_height = hcol:height-PIXELS
                  tt_size.wg_xpos = hcol:X
                  tt_size.wg_ypos = hcol:Y NO-ERROR.
          END.
          ASSIGN hcol:HEIGHT-PIXELS = (hcol:HEIGHT-PIXELS * CURRENT-WINDOW:HEIGHT-PIXELS) 
                             / bf_size.wg_height
                 hcol:width-PIXELS = (hcol:width-PIXELS * CURRENT-WINDOW:WIDTH-PIXELS) 
                             / bf_size.wg_width 
                 hcol:X = (hcol:X * CURRENT-WINDOW:WIDTH-PIXELS) / bf_size.wg_width
                 hcol:Y = (hcol:Y * CURRENT-WINDOW:height-PIXELS) / bf_size.wg_height NO-ERROR.
          ASSIGN hcol = hcol:NEXT-SIBLING NO-ERROR.
       END. /* do while */

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-MINIMIZED OF C-Win /* Vendor Invoices Edit/Post Register */
DO:
    IF winstate <> 1 THEN DO:
        FIND FIRST tt_size WHERE tt_size.wg_name = STRING(CURRENT-WINDOW) NO-ERROR.
        IF AVAIL tt_size THEN
            ASSIGN CURRENT-WINDOW:HEIGHT-PIXELS = tt_size.wg_height
                   CURRENT-WINDOW:WIDTH-PIXELS = tt_size.wg_width NO-ERROR.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESTORED OF C-Win /* Vendor Invoices Edit/Post Register */
DO:
   IF winstate = 3 THEN ASSIGN winstate = 0 NO-ERROR.
   ELSE DO:
       IF winstate = 1 THEN DO:
           ASSIGN winstate = 0 NO-ERROR.
       END.
       ASSIGN hcol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.
       FIND FIRST tt_size WHERE wg_name = STRING(hcol) NO-ERROR.
       IF AVAIL tt_size THEN DO:
           ASSIGN hcol:HEIGHT-PIXELS = tt_size.wg_height
                  hcol:WIDTH-PIXELS = tt_size.wg_width
                  hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos NO-ERROR.
       END.
       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       DO WHILE VALID-HANDLE(hcol):
          FIND FIRST tt_size WHERE wg_name = STRING(hcol) NO-ERROR.
          IF AVAIL tt_size THEN DO:
             ASSIGN hcol:HEIGHT-PIXELS = tt_size.wg_height
                  hcol:WIDTH-PIXELS = tt_size.wg_width
                  hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos NO-ERROR.
          END.
          ASSIGN hcol = hcol:NEXT-SIBLING NO-ERROR.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  assign rd-dest.

  run check-date.
  if v-invalid then return no-apply.
  ASSIGN tran-period.

  DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN v-trnum       = gl-ctrl.trnum + 1
               gl-ctrl.trnum = v-trnum.
        FIND CURRENT gl-ctrl NO-LOCK.
         LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.

  run run-report.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF v-postable THEN DO:
    lv-post = NO.

    MESSAGE "Post Invoices?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:
      RUN post-gl.

      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
  END.

  ELSE MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.

  IF NOT v-postable OR NOT lv-post THEN DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        IF gl-ctrl.trnum EQ v-trnum THEN gl-ctrl.trnum = v-trnum - 1.
        FIND CURRENT gl-ctrl NO-LOCK.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Print G/L Account Description? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  assign {&self-name}.
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

  ASSIGN CURRENT-WINDOW:MAX-WIDTH = SESSION:WIDTH-CHARS
         CURRENT-WINDOW:MAX-height = SESSION:HEIGHT-CHARS.
  ASSIGN hcol = CURRENT-WINDOW NO-ERROR.
  CREATE tt_size.
  ASSIGN tt_size.wg_name = STRING(hcol)
         tt_size.wg_width = hcol:WIDTH-PIXELS
         tt_size.wg_height = hcol:HEIGHT-PIXELS
         tt_size.wg_xpos = hcol:X
         tt_size.wg_ypos = hcol:Y NO-ERROR.

  tran-date = TODAY.

  RUN enable_UI.

  RUN check-date.

  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-date C-Win 
PROCEDURE check-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO with frame {&frame-name}:
    v-invalid = no.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then tran-period:SCREEN-VALUE = string(period.pnum).

    ELSE DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      v-invalid = yes.
    end.
  END.
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
  DISPLAY tran-date tran-period begin_vend end_vend begin_date end_date lbl_sort 
          tb_sort rd-dest lines-per-page td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_vend end_vend begin_date end_date 
         tb_sort rd-dest lines-per-page td-show-parm btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-file C-Win 
PROCEDURE output-to-file :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE list-name
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME

         UPDATE OKpressed.

     IF NOT OKpressed THEN  RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-printer C-Win 
PROCEDURE output-to-printer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
    IF NOT RESULT THEN v-postable = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-screen C-Win 
PROCEDURE output-to-screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-gl C-Win 
PROCEDURE post-gl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR g2 AS INT NO-UNDO.
 DEF VAR t1 AS INT NO-UNDO.
 DEF VAR v-upd AS LOG NO-UNDO.
 DEF var v-po-no like fg-rcpth.po-no NO-UNDO.
 def var total-msf like ap-invl.amt-msf NO-UNDO.
 def var v-qty like ap-invl.qty.
def var v-qty1 like v-qty.
def var v-qty2 like v-qty.
def var v-qty3 like v-qty.
def var v-cost like fg-rdtlh.cost.

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
  postit:
do transaction on error undo postit:
  g2 = 0.

  for each report
      where report.term-id eq v-term
        and can-find(first ap-inv where recid(ap-inv) eq report.rec-id
                                    and ap-inv.posted eq no):

    find first ap-inv
        where recid(ap-inv) eq report.rec-id
        exclusive-lock no-error no-wait.

    if not avail ap-inv then do:
      message "Unable to Post due to Invoice Record being Locked.  " +
              "Please Try again Later".
      pause.
      hide message no-pause.
      undo postit, leave postit.
    end.

    ap-inv.period = tran-period.
    v-upd = yes.

    for each ap-invl where ap-invl.i-no eq ap-inv.i-no no-lock,

        first po-ordl
        where po-ordl.company   eq cocode
          and po-ordl.po-no     eq (if ap-invl.po-no eq 0 then ap-inv.po-no
                                                          else ap-invl.po-no)
          and po-ordl.line      eq {ap/invlline.i -1}
          and po-ordl.item-type eq no
        use-index po-no no-lock:

      v-po-no = trim(string(po-ordl.po-no,">>>>>>>>>>")).

      find first fg-rcpth
          where fg-rcpth.company   eq cocode
            and fg-rcpth.i-no      eq po-ordl.i-no
            and fg-rcpth.po-no     eq v-po-no
            and fg-rcpth.rita-code eq "R"
          use-index item-po no-lock no-error.
      if not avail fg-rcpth then do:
        v-upd = no.
        leave.
      end.  

      find first fg-rcpts
          where fg-rcpts.company   eq cocode
            and fg-rcpts.i-no      eq po-ordl.i-no
            and fg-rcpts.po-no     eq v-po-no
            and fg-rcpts.rita-code eq "R"
          use-index i-no no-lock no-error.
      if avail fg-rcpts then do:
        v-upd = no.
        leave.
      end.
    end.  

    if not v-upd then next.

    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend no-lock.

    for each ap-invl where ap-invl.i-no eq ap-inv.i-no:
      create gltrans.
      assign
       t1 = t1 + ap-invl.amt
       g2 = g2 + ap-invl.amt
       total-msf = total-msf + ap-invl.amt-msf

       gltrans.company = cocode
       gltrans.actnum  = ap-invl.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.tr-dscr = vend.name  + "  " + string(ap-inv.inv-date)
       gltrans.tr-date = tran-date
       gltrans.tr-amt  = ap-invl.amt
       gltrans.trnum   = v-trnum
       gltrans.period  = tran-period
       ap-invl.posted  = yes.

      find first po-ordl
          where po-ordl.company eq cocode
            and po-ordl.po-no   eq (if ap-invl.po-no eq 0 then ap-inv.po-no
                                                          else ap-invl.po-no)
            and po-ordl.line    eq {ap/invlline.i -1}
          use-index po-no no-error.
      if avail po-ordl then do:
        find first reftable
            {ap/apreftbw.i po-ordl.po-no}
              and reftable.code2 eq string(ap-invl.i-no,"9999999999")
            no-lock no-error.
        if not avail reftable then do:
          {ap/addreftb.i po-ordl.po-no}
        end.

        po-ordl.t-inv-qty = po-ordl.t-inv-qty + ap-invl.qty.

        /* Ensure receipts = payables */
        if not po-ordl.item-type and v-fgpostgl then do:
          release prod.
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
              no-error.

          if avail itemfg then
          find first prodl
              where prodl.company eq cocode
                and prodl.procat  eq itemfg.procat
                and can-find(first prod
                             where prod.company eq cocode
                               and prod.prolin  eq prodl.prolin)
              no-lock no-error.

          if avail prodl then
          find first prod
              where prod.company eq cocode
                and prod.prolin  eq prodl.prolin
              no-lock no-error.

          if avail itemfg then do:
            run sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                    ap-invl.qty, output v-qty1).

            assign
             v-po-no = trim(string(po-ordl.po-no,">>>>>>>>>>"))
             v-qty   = 0
             v-cost  = ap-invl.amt / (v-qty1 / 1000).

            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.i-no      eq po-ordl.i-no
                  and fg-rcpth.po-no     eq v-po-no
                  and fg-rcpth.rita-code eq "R"
                  and ((fg-rcpth.b-no    eq ap-invl.i-no and v-fgpostgl) or
                       (fg-rcpth.b-no    eq 0        and not v-fgpostgl))
                use-index item-po,

                each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no

                break by fg-rcpth.trans-date
                      BY fg-rdtlh.trans-time
                      by fg-rcpth.r-no
                      by recid(fg-rdtlh):

              /* Remove the accrued AP & FG assets created for FG receipt */
              if fg-rdtlh.cost ne 0 then do:
                if avail prod         and
                   prod.fg-mat ne ""  and
                   prod.wip-mat ne "" and
                   fg-rdtlh.cost ne ? then do:

                  /* Debit FG Material */
                  find first work-gl where work-gl.actnum eq prod.fg-mat
                      no-lock no-error.

                  if not avail work-gl then do:
                    create work-gl.
                    work-gl.actnum = prod.fg-mat.
                  end.

                  work-gl.debits = work-gl.debits -
                                   (fg-rdtlh.qty / 1000 * fg-rdtlh.cost ).

                  /* Credit WIP Material */
                  find first work-gl where work-gl.actnum eq prod.wip-mat
                      no-lock no-error.

                  if not avail work-gl then do:
                    create work-gl.
                    work-gl.actnum = prod.wip-mat.
                  end.

                  work-gl.credits = work-gl.credits -
                                    (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).
                end.
              end.  
              /* Balance GL */

              assign
               v-qty         = v-qty + fg-rdtlh.qty
               fg-rdtlh.cost = v-cost
               fg-rcpth.b-no = ap-invl.i-no.

              if last(fg-rcpth.trans-date) and
                 v-qty ne v-qty1           then do:

                find first fg-bin
                    where fg-bin.company eq cocode
                      and fg-bin.i-no    eq fg-rcpth.i-no
                      and fg-bin.loc     eq fg-rdtlh.loc
                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                      and fg-bin.tag     eq fg-rdtlh.tag
                      and fg-bin.job-no  eq fg-rcpth.job-no
                      and fg-bin.job-no2 eq fg-rcpth.job-no2
                    no-error.  

                if not avail fg-bin then do:
                  create fg-bin.
                  assign
                   fg-bin.company      = fg-rdtlh.company
                   fg-bin.job-no       = fg-rcpth.job-no
                   fg-bin.job-no2      = fg-rcpth.job-no2
                   fg-bin.loc          = fg-rdtlh.loc
                   fg-bin.loc-bin      = fg-rdtlh.loc-bin
                   fg-bin.tag          = fg-rdtlh.tag
                   fg-bin.i-no         = fg-rcpth.i-no
                   fg-bin.case-count   = itemfg.case-count
                   fg-bin.cases-unit   = 1
                   fg-bin.aging-date   = fg-rcpth.trans-date
                   fg-bin.pur-uom      = "M"
                   fg-bin.std-tot-cost = fg-rdtlh.cost
                   fg-bin.std-mat-cost = fg-bin.std-tot-cost
                   fg-bin.std-lab-cost = 0
                   fg-bin.std-var-cost = 0
                   fg-bin.std-fix-cost = 0.
                end.

                assign
                 v-qty1         = v-qty1 - v-qty
                 fg-rdtlh.qty   = fg-rdtlh.qty + v-qty1
                 fg-rdtlh.cases = trunc(fg-rdtlh.qty / fg-rdtlh.qty-case,0)
                 fg-bin.qty     = fg-bin.qty + v-qty1
                 itemfg.q-onh   = itemfg.q-onh + v-qty1.
                 run fg/chkfgloc.p (input fg-rcpth.i-no, input fg-rdtlh.loc).
                 FIND FIRST itemfg-loc 
                    WHERE itemfg-loc.company EQ fg-rdtlh.company
                      AND itemfg-loc.i-no    EQ fg-rcpth.i-no
                      AND itemfg-loc.loc     EQ fg-rdtlh.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                 IF AVAIL itemfg-loc THEN
                   itemfg-loc.q-onh = itemfg-loc.q-onh + v-qty1.
                 FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.

              end.
            end.

            for each fg-rcpth
                where fg-rcpth.company   eq cocode
                  and fg-rcpth.i-no      eq po-ordl.i-no
                  and fg-rcpth.po-no     eq v-po-no
                  and fg-rcpth.rita-code eq "R"
                use-index item-po no-lock,

                each fg-rdtlh where fg-rdtlh.r-no eq fg-rcpth.r-no

                break by fg-rcpth.job-no
                      by fg-rcpth.job-no2
                      by fg-rdtlh.loc
                      by fg-rdtlh.loc-bin
                      by fg-rdtlh.tag:

              if first-of(fg-rdtlh.tag) then
                assign
                 v-qty  = 0
                 v-cost = 0.

              assign
               v-qty  = v-qty + fg-rdtlh.qty
               v-cost = v-cost + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).

              if last-of(fg-rdtlh.tag) then do:
                find first fg-bin
                    where fg-bin.company eq cocode
                      and fg-bin.i-no    eq fg-rcpth.i-no
                      and fg-bin.loc     eq fg-rdtlh.loc
                      and fg-bin.loc-bin eq fg-rdtlh.loc-bin
                      and fg-bin.tag     eq fg-rdtlh.tag
                      and fg-bin.job-no  eq fg-rcpth.job-no
                      and fg-bin.job-no2 eq fg-rcpth.job-no2
                    no-error.  

                if not avail fg-bin then do:
                  create fg-bin.
                  assign
                   fg-bin.company      = fg-rdtlh.company
                   fg-bin.job-no       = fg-rcpth.job-no
                   fg-bin.job-no2      = fg-rcpth.job-no2
                   fg-bin.loc          = fg-rdtlh.loc
                   fg-bin.loc-bin      = fg-rdtlh.loc-bin
                   fg-bin.tag          = fg-rdtlh.tag
                   fg-bin.i-no         = fg-rcpth.i-no
                   fg-bin.case-count   = itemfg.case-count
                   fg-bin.cases-unit   = 1
                   fg-bin.aging-date   = fg-rcpth.trans-date
                   fg-bin.pur-uom      = "M"
                   fg-bin.std-tot-cost = fg-rdtlh.cost
                   fg-bin.std-mat-cost = fg-bin.std-tot-cost
                   fg-bin.std-lab-cost = 0
                   fg-bin.std-var-cost = 0
                   fg-bin.std-fix-cost = 0.
                end.

                v-cost = v-cost / (v-qty / 1000).

                if fg-bin.pur-uom eq "M" then
                  fg-bin.std-tot-cost = v-cost.
                else
                  run sys/ref/convcuom.p ("M", fg-bin.pur-uom, 0, 0, 0, 0,
                                          v-cost, output fg-bin.std-tot-cost).

                assign
                 fg-bin.std-mat-cost = fg-bin.std-tot-cost
                 fg-bin.std-lab-cost = 0
                 fg-bin.std-var-cost = 0
                 fg-bin.std-fix-cost = 0.
              end.
            end.
          end.

          run fg/updfgcst.p (po-ordl.i-no).
        end.
      end.
    end.  /* each line */

    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend exclusive-lock.

    assign
     vend.purch[tran-period]   = vend.purch[tran-period] + t1
     vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
     vend.purch[13]        = vend.purch[13] + t1
     vend.n-purch[13]      = vend.n-purch[13] + 1
     vend.ptd-msf[tran-period] = vend.ptd-msf[tran-period] + total-msf
     vend.ytd-msf          = vend.ytd-msf + total-msf
     vend.acc-bal          = vend.acc-bal + t1.

    if vend.acc-bal ge vend.hibal then
      assign
       vend.hibal      = vend.acc-bal
       vend.hibal-date = ap-inv.inv-date.

    create ap-ledger.
    assign
     ap-ledger.company  = cocode
     ap-ledger.vend-no  = ap-inv.vend-no
     ap-ledger.amt      = ap-inv.net
     ap-ledger.refnum   = "INV# " + ap-inv.inv-no
     ap-ledger.ref-date = ap-inv.inv-date
     ap-ledger.trnum    = v-trnum
     ap-ledger.period   = tran-period
     ap-ledger.tr-date  = tran-date.

    assign
     t1            = 0
     ap-inv.posted = yes.
  end. /* for each ap-inv */

  if lv-frt-total ne 0 then do:
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-frt-acct
     gltrans.jrnl    = "ACPAY"
     gltrans.tr-dscr = "ACCOUNTS PAYABLE FREIGHT"
     gltrans.tr-date = tran-date
     gltrans.tr-amt  = lv-frt-total
     gltrans.period  = tran-period
     gltrans.trnum   = v-trnum.

    g2 = g2 + lv-frt-total.
  end.

  create gltrans.
  assign
   gltrans.company = cocode
   gltrans.actnum  = xap-acct
   gltrans.jrnl    = "ACPAY"
   gltrans.tr-dscr = "ACCOUNTS PAYABLE INVOICE"
   gltrans.tr-date = tran-date
   gltrans.tr-amt  = - g2
   gltrans.period  = tran-period
   gltrans.trnum   = v-trnum.

  for each work-gl break by work-gl.actnum:
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "ACPAY"
       gltrans.period  = tran-period
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = tran-date
       gltrans.tr-dscr = "AP for FG Receipts from PO"
       gltrans.trnum   = v-trnum.

      assign
       debits  = 0
       credits = 0.
    end.
  end.
end. /* postit: transaction */

  for each report where report.term-id eq v-term:
    delete report.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-inreg.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */
def var g1 as dec format "->>,>>>,>>9.99" NO-UNDO.
def var g2 like g1 NO-UNDO.
def var t1 like g1 NO-UNDO.
def var t2 like g1 NO-UNDO.
def var t3 like g1 NO-UNDO.
def var v1 like g1 NO-UNDO.
def var v2 like g1 NO-UNDO.

def var total-msf like ap-invl.amt-msf NO-UNDO.
def var v-s-date like inv-head.inv-date format "99/99/9999" init 01/01/0001 NO-UNDO.
def var v-e-date like v-s-date init today NO-UNDO.
def var v-prt-dscr as log init no no-undo.
def var v-s-vend like vend.vend-no initial "First" no-undo.
def var v-e-vend like vend.vend-no initial "Last" no-undo.
DEF BUFFER xap-inv FOR ap-inv.
def var v-loop as int init 1 no-undo.

{sys/form/r-top3w.f}
time_stamp = string(time,"hh:mmam").

form header
     "VENDOR#  Name                              INVOICE #       INV.DATE    DUE DATE         AMOUNT " 
     "    G/L DISTRIBUTION" skip fill("_",130) format "x(130)"
    with no-labels no-box no-underline frame f-top page-top width 132 STREAM-IO.

tmpstore = fill("_",125).

ASSIGN v-s-vend = begin_vend
       v-e-vend = END_vend
       v-s-date = begin_date
       v-e-date = END_date
       v-prt-dscr = tb_sort
       .

 {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

assign
 g1 = 0
 g2 = 0
 t1 = 0
 t2 = 0
 t3 = 0
 v1 = 0
 v2 = 0
 total-msf = 0.

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "VENDOR INVOICES  -  EDIT REGISTER " + string(v-trnum)
 str-tit3 = "Period " + string(tran-period,"99") +
            " - transaction Date Entered: " + string(tran-date)
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (114 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

display "" with frame r-top.
display "" with frame f-top.

for each report where report.term-id eq v-term:
  delete report.
end.

for each xap-inv
    where xap-inv.company  eq cocode
      and xap-inv.posted   eq no
      and xap-inv.inv-date ge v-s-date
      and xap-inv.inv-date le v-e-date 
      and xap-inv.vend-no  ge v-s-vend
      and xap-inv.vend-no  le v-e-vend
    use-index posted no-lock
    transaction:

  find first ap-inv where recid(ap-inv) eq recid(xap-inv)
     exclusive-lock no-wait no-error.

  if avail ap-inv then do:
    create report.
    assign
     report.term-id = v-term
     report.rec-id  = recid(ap-inv).
  end.
end.

for each report where report.term-id eq v-term no-lock,
    first ap-inv where recid(ap-inv) eq report.rec-id NO-LOCK
    break by ap-inv.vend-no
          by ap-inv.inv-no          
    with frame a1 STREAM-IO:
  v-postable = YES.  
  if first-of(ap-inv.vend-no) then do:
    find first vend
        where vend.company eq cocode
          and vend.vend-no eq ap-inv.vend-no
        use-index vend no-lock no-error.

    put vend.vend-no space(1)
        vend.name.
  end.

  else
  if first-of(ap-inv.inv-no) then put skip(1).

  put ap-inv.inv-no   to 55
      ap-inv.inv-date at 60
      ap-inv.due-date at 71                        
      ap-inv.net + ap-inv.freight format "->,>>>,>>9.99" to 94.

  assign
   v2 = v2 + ap-inv.net
   v1 = v1 + ap-inv.disc-taken.

  for each ap-invl where ap-invl.i-no eq ap-inv.i-no no-lock use-index i-no
      with frame a2 no-box no-labels width 132:

    put ap-invl.actnum at 96 format "x(19)" space(1)            
        ap-invl.amt skip.

    if v-prt-dscr then do:
      find first account
          where account.company eq cocode
            and account.actnum  eq ap-invl.actnum
          no-lock no-error.
      if avail account then put account.dscr at 90 format "x(40)" skip.
    end.   
  end. /* each ap-invl */

  if last-of(ap-inv.vend-no) then do:
    put v-frt-acct at 96 format "x(19)" space(1)            
        ap-inv.freight to 127 skip.

    if v-prt-dscr then do:
      find first account
          where account.company eq cocode
            and account.actnum  eq v-frt-acct
          no-lock no-error.
      if avail account then put account.dscr at 90 format "x(40)" skip.
    end.   
    v2 = v2 + ap-inv.freight.

    display  "*  VENDOR TOTALS" at 90 v2 to 127 "*" skip(1)
        with frame vtot no-box no-labels width 132 STREAM-IO.

    assign    
     g1 = g1 + v1
     g2 = g2 + v2
     v1 = 0
     v2 = 0.
  end.
end. /* each invoice */

display  "** GRAND TOTAL  "  at 90  g2 to 127 "**"
    with no-labels no-underline width 132 frame GT STREAM-IO.

hide frame f-top.

str-tit3 = "Period " + string(tran-period,"99") + " - " + "Summary by Account".
x = (132 - length(str-tit3)) / 2.
str-tit3 = fill(" ",x) + str-tit3.

page.

form header
     "ACCOUNT                                  DATE   VENDOR#  INVOICE#    "
     "LINE DESCRIPTION              QTY   UNIT PRICE      AMOUNT" skip
     fill("_",132) format "x(130)"

    with no-labels no-box no-underline frame f-top2 page-top width 132 STREAM-IO .

display "" with frame f-top2.

v-loop = 1.

for each report where report.term-id eq v-term no-lock,

    first ap-inv where recid(ap-inv) eq report.rec-id no-lock,

    first vend
    where vend.company eq cocode
      and vend.vend-no eq ap-inv.vend-no
    use-index vend no-lock

    break by ap-inv.vend-no:

  if ap-inv.freight ne 0 then do:
    if v-loop eq 1 then do:
      v-loop = 2.
      find first account
          where account.company eq cocode
            and account.actnum  eq v-frt-acct
          no-lock no-error.
      put v-frt-acct + " - " + account.dscr format "x(40)".
    end.

    put ap-inv.inv-date at 41       space(1)
        ap-inv.vend-no              space(1)
        ap-inv.inv-no               space(6)
        "Freight"    format "x(18)" space(7)
        1.0          format "9.9"   space(1)
        ap-inv.freight              to 118
        ap-inv.freight              to 131
        skip.
  end.

  accumulate ap-inv.freight (total).

  if last(ap-inv.vend-no) then
    put "** TOTAL " to 114
        accum total ap-inv.freight format "->>,>>>,>>9.99" to 128
        " *" skip(1).
end.

for each report where report.term-id eq v-term,

    first ap-inv where recid(ap-inv) eq report.rec-id no-lock,

    each ap-invl
    where ap-invl.i-no    eq ap-inv.i-no
      and ap-invl.posted  eq no
    use-index i-no no-lock

    break by ap-invl.actnum
          by ap-invl.inv-no
          by ap-invl.line

    with width 132 no-labels:

  find first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-inv.vend-no
      use-index vend no-lock no-error.

  if first-of(ap-invl.actnum) then do:
    find first account
        where account.company eq cocode
          and account.actnum  eq ap-invl.actnum
        no-lock no-error.

    put ap-invl.actnum + " - " + account.dscr format "x(40)".
  end.

  put ap-inv.inv-date       at 41
      space(1)
      ap-inv.vend-no
      space(1)
      ap-inv.inv-no
      space(1)
      {ap/invlline.i -1}    format ">>>9"
      space(1)
      ap-invl.dscr          format "x(18)"
      space(1)
      ap-invl.qty           format "->>,>>9.9<<"
      space(1)
      ap-invl.unit-pr
      space(1)
      ap-invl.amt
      space(1)
      skip.

  accumulate ap-invl.amt (total by ap-invl.actnum).
  accumulate ap-invl.amt (total).

  if last-of(ap-invl.actnum) then
    put "** TOTAL " to 114
        accum total BY ap-invl.actnum ap-invl.amt format "->>,>>>,>>9.99" to 128
        " *" skip(1).
end.

put "***** TOTAL for ALL ACCOUNTS " to 116
    ((accum total ap-invl.amt) + (accum total ap-inv.freight)) format "->>,>>>,~>>9.99" to 130 skip(2).

lv-frt-total = (accum total ap-inv.freight).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-param C-Win 
PROCEDURE show-param :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

