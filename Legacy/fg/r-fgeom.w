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

DEF NEW SHARED VAR choice AS LOG NO-UNDO .
DEF VAR v-invalid AS LOG NO-UNDO.
def var v-postable as log init NO NO-UNDO.

DEF VAR v-peryr AS INT NO-UNDO.
DEF VAR v-lastper AS LOG NO-UNDO.
DEF VAR v-newbal AS LOG NO-UNDO.
DEF VAR v-begdate LIKE period.pst NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tran-date tb_close begin_i-no end_i-no ~
btn-ok btn-cancel RECT-10 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period tb_close begin_i-no ~
end_i-no v-note-1 v-note-2 v-note-3 

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

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE v-note-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE v-note-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE v-note-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 99 BY 4.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 99 BY 6.91.

DEFINE VARIABLE tb_close AS LOGICAL INITIAL no 
     LABEL "Close Period" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 2.19 COL 44 COLON-ALIGNED
     tran-period AT ROW 3.38 COL 44 COLON-ALIGNED
     tb_close AT ROW 4.81 COL 46
     begin_i-no AT ROW 6.24 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 6.24 COL 70 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     v-note-1 AT ROW 8.62 COL 2 COLON-ALIGNED NO-LABEL
     v-note-2 AT ROW 9.57 COL 2 COLON-ALIGNED NO-LABEL
     v-note-3 AT ROW 10.52 COL 2 COLON-ALIGNED NO-LABEL
     btn-ok AT ROW 12.91 COL 25
     btn-cancel AT ROW 12.91 COL 59
     RECT-10 AT ROW 7.91 COL 1
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 100.2 BY 14.19.


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
         TITLE              = "FG End of Period Processing"
         HEIGHT             = 14.43
         WIDTH              = 102
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
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
                                                                        */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-note-1 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-note-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-note-3 IN FRAME FRAME-A
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
ON END-ERROR OF C-Win /* FG End of Period Processing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FG End of Period Processing */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning FG Item# */
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
  DEF VAR v-sort AS LOGICAL INIT YES FORMAT "Y/N" NO-UNDO.


  RUN check-date.
  IF v-invalid THEN RETURN NO-APPLY.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  choice = YES.

  v-postable = NO.

  RUN run-report.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending FG Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
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

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  ASSIGN
   tran-date = TODAY
   v-note-1  = "  This Procedure Will Clear All Monthly/Period-To-Date Quantities. "
   v-note-2  = "  The quantities are captured in the Year-To-Date Totals. " 
   v-note-3  = "  This should be run on the last day of the period. ".

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO tran-date.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tots C-Win 
PROCEDURE calc-tots :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-frst-date AS DATE NO-UNDO.
DEF INPUT PARAM ip-last-date AS DATE NO-UNDO.
DEF INPUT PARAM ip-period-yr LIKE period.Y NO-UNDO.

DEF VAR lv-qty LIKE oe-ordl.qty NO-UNDO.


FOR EACH ar-invl
    WHERE ar-invl.company EQ itemfg.company
      AND ar-invl.i-no    EQ itemfg.i-no
    USE-INDEX i-no NO-LOCK,
    FIRST ar-inv
    WHERE ar-inv.x-no     EQ ar-invl.x-no
      AND ar-inv.inv-date GE ip-frst-date
      AND ar-inv.inv-date LE ip-last-date
      AND ar-inv.posted   EQ YES
    USE-INDEX x-no NO-LOCK,
    FIRST period
    WHERE period.company EQ ar-inv.company
      AND period.yr      EQ ip-period-yr
      AND period.pst     LE ar-inv.inv-date
      AND period.pend    GE ar-inv.inv-date
    NO-LOCK:

  IF (period.yr    EQ v-peryr AND
      (period.pnum EQ tran-period AND NOT tb_close) OR
      period.pnum  GT tran-period) OR
      period.yr GT v-peryr         THEN
    ASSIGN
     itemfg.q-inv-ptd            = itemfg.q-inv-ptd + ar-invl.inv-qty
     itemfg.ptd-msf[period.pnum] = itemfg.ptd-msf[period.pnum] +
                                   (IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                    ELSE itemfg.t-sqft * ar-invl.ship-qty / 1000).

  ASSIGN
   itemfg.q-inv-ytd = itemfg.q-inv-ytd + ar-invl.inv-qty
   itemfg.ytd-msf   = itemfg.ytd-msf   + (IF ar-invl.amt-msf NE 0 THEN ar-invl.amt-msf
                                          ELSE itemfg.t-sqft * ar-invl.ship-qty / 1000).

  RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT ar-inv.loc).
  FIND FIRST itemfg-loc 
    WHERE itemfg-loc.company EQ itemfg.company
      AND itemfg-loc.i-no    EQ itemfg.i-no
      AND itemfg-loc.loc     EQ ar-inv.loc
    EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL itemfg-loc THEN DO:
      IF (period.yr    EQ v-peryr AND
          (period.pnum EQ tran-period AND NOT tb_close) OR
          period.pnum  GT tran-period) OR
          period.yr GT v-peryr         THEN
        ASSIGN
         itemfg-loc.q-inv-ptd            = itemfg-loc.q-inv-ptd + ar-invl.inv-qty.

      ASSIGN
       itemfg-loc.q-inv-ytd = itemfg-loc.q-inv-ytd + ar-invl.inv-qty.
  END.
END.

FOR EACH fg-rcpth
    WHERE fg-rcpth.company    EQ itemfg.company
      AND fg-rcpth.i-no       EQ itemfg.i-no
      AND fg-rcpth.trans-date GE ip-frst-date
      AND fg-rcpth.trans-date LE ip-last-date
      AND fg-rcpth.rita-code  GE "R"
      AND fg-rcpth.rita-code  LE "S"
    USE-INDEX tran NO-LOCK,
    EACH fg-rdtlh
    WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
    USE-INDEX rm-rdtl NO-LOCK,
    FIRST period
    WHERE period.company EQ fg-rcpth.company
      AND period.yr      EQ ip-period-yr
      AND period.pst     LE fg-rcpth.trans-date
      AND period.pend    GE fg-rcpth.trans-date
    NO-LOCK:

  IF (period.yr    EQ v-peryr AND
      (period.pnum EQ tran-period AND NOT tb_close) OR
      period.pnum  GT tran-period) OR
      period.yr GT v-peryr THEN DO:

    IF fg-rcpth.rita-code EQ "R" THEN
      itemfg.q-prod-ptd = itemfg.q-prod-ptd + fg-rdtlh.qty.
    ELSE
    IF fg-rcpth.rita-code EQ "S" THEN
      itemfg.q-ship-ptd = itemfg.q-ship-ptd + fg-rdtlh.qty.


  END.

  FIND FIRST itemfg-loc 
    WHERE itemfg-loc.company EQ itemfg.company
      AND itemfg-loc.i-no    EQ itemfg.i-no
      AND itemfg-loc.loc     EQ fg-rcpth.loc
    EXCLUSIVE-LOCK NO-ERROR.

  IF AVAIL itemfg-loc THEN DO:

      IF fg-rcpth.rita-code EQ "R" THEN
        itemfg.q-prod-ytd = itemfg.q-prod-ytd + fg-rdtlh.qty.
      ELSE
      IF fg-rcpth.rita-code EQ "S" THEN
        itemfg.q-ship-ytd = itemfg.q-ship-ytd + fg-rdtlh.qty.

      IF (period.yr    EQ v-peryr AND
          (period.pnum EQ tran-period AND NOT tb_close) OR
          period.pnum  GT tran-period) OR
          period.yr GT v-peryr THEN DO:

        IF fg-rcpth.rita-code EQ "R" THEN
          itemfg-loc.q-prod-ptd = itemfg-loc.q-prod-ptd + fg-rdtlh.qty.
        ELSE
        IF fg-rcpth.rita-code EQ "S" THEN
          itemfg-loc.q-ship-ptd = itemfg-loc.q-ship-ptd + fg-rdtlh.qty.

      END.

      IF fg-rcpth.rita-code EQ "R" THEN
        itemfg-loc.q-prod-ytd = itemfg-loc.q-prod-ytd + fg-rdtlh.qty.
      ELSE
      IF fg-rcpth.rita-code EQ "S" THEN
        itemfg-loc.q-ship-ytd = itemfg-loc.q-ship-ytd + fg-rdtlh.qty.
      FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  END.
END.

FOR EACH oe-ordl
    WHERE oe-ordl.company EQ itemfg.company
      AND oe-ordl.i-no    EQ itemfg.i-no
    USE-INDEX item NO-LOCK,
    FIRST oe-ord
    WHERE oe-ord.company  EQ oe-ordl.company
      AND oe-ord.ord-no   EQ oe-ordl.ord-no
      AND oe-ord.ord-date GE ip-frst-date
      AND oe-ord.ord-date LE ip-last-date  
    USE-INDEX ord-no NO-LOCK,
    FIRST period
    WHERE period.company EQ oe-ord.company
      AND period.yr      EQ ip-period-yr
      AND period.pst     LE oe-ord.ord-date
      AND period.pend    GE oe-ord.ord-date
    NO-LOCK:

  lv-qty = IF oe-ordl.opened THEN oe-ordl.qty ELSE oe-ordl.t-ship-qty.

  IF (period.yr    EQ v-peryr AND
      (period.pnum EQ tran-period AND NOT tb_close) OR
      period.pnum  GT tran-period) OR
      period.yr GT v-peryr THEN itemfg.q-ptd = itemfg.q-ptd + lv-qty.

  itemfg.q-ord-ytd = itemfg.q-ord-ytd + lv-qty.

  FIND FIRST itemfg-loc 
    WHERE itemfg-loc.company EQ itemfg.company
      AND itemfg-loc.i-no    EQ itemfg.i-no
      AND itemfg-loc.loc     EQ oe-ord.loc
    EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL itemfg-loc THEN
    itemfg-loc.q-ord-ytd = itemfg-loc.q-ord-ytd  + lv-qty.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
          AND period.pst     LE DATE(tran-date:SCREEN-VALUE)
          and period.pend    GE DATE(tran-date:SCREEN-VALUE)
        no-lock no-error.
    if avail period AND
       (period.pend EQ DATE(tran-date:SCREEN-VALUE) OR
        tb_close:SCREEN-VALUE EQ "no")              THEN DO:
        ASSIGN  tran-period:SCREEN-VALUE = string(period.pnum)
                v-peryr = period.yr.

        find next period where period.company eq cocode
                       and period.pst     gt tran-date no-lock no-error.
        IF AVAIL period THEN v-begdate = period.pst.

    END.
    ELSE DO:
         IF AVAIL period THEN
           message "Period ending date does not equal" DATE(tran-date:SCREEN-VALUE)
               view-as alert-box error.
         ELSE
           message "No Defined Period Exists for" DATE(tran-date:SCREEN-VALUE)
               view-as alert-box error.
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
  DISPLAY tran-date tran-period tb_close begin_i-no end_i-no v-note-1 v-note-2 
          v-note-3 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE tran-date tb_close begin_i-no end_i-no btn-ok btn-cancel RECT-10 
         RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc C-Win 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
  */

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
/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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


  RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
*/
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
 /* run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */  
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- oe/invpost.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */
DEF VAR lv-frst-date AS DATE NO-UNDO.
DEF VAR lv-last-date AS DATE NO-UNDO.


 find first fg-rctd
     where fg-rctd.company   eq cocode
       and fg-rctd.rita-code ne "P"
       and fg-rctd.rita-code ne ""
     no-lock no-error.
 if avail fg-rctd then do:
      MESSAGE  "  NOT ALL TRANSACTIONS HAVE BEEN POSTED.  "
               "  PERIOD END PROCESSING CANNOT CONTINUE.  " skip(1)
               "  Would you like to post these transactions? "
               VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
               UPDATE v-post AS LOG .

      if v-post then do:
        RUN fg/fgpstall.w PERSISTENT (tran-date,"").
        if not choice then return.
      end.

      else RETURN.
 end.

 release period.
 find last period where period.company  eq cocode
          and period.yr       eq v-peryr
        no-lock no-error.
 if avail period and period.pnum eq tran-period AND tb_close then do:

       MESSAGE " This is the Last Period.  The Year-To-Date Totals will be Moved to"
               " Last-Year Totals and the Qty-On-Hand will Update the Beginning Balance."
               " Running Physical Count Processing is recommended to update your actual"
               " On Hand Balances counted via a Physical Inventory Count prior to Updating!"
               VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE v-lastper .

       if not v-lastper then return.
 end.

 ELSE MESSAGE " This will move the Qty-On-Hand to the Beginning Balance! "
                 " Running Physical Count Processing is recommended to update your actual "
                 " On Hand Balances counted via a Physical Count prior to Updating! "
                 " Update Beginning Balance? " 
                 VIEW-AS ALERT-BOX WARNING BUTTON YES-NO update v-newbal .

 SESSION:SET-WAIT-STATE("general").

 FOR EACH itemfg
     WHERE itemfg.company EQ cocode
       AND itemfg.i-no    GE begin_i-no
       AND itemfg.i-no    LE end_i-no
       AND itemfg.i-no    GT ""
     TRANSACTION:

     STATUS DEFAULT "Processing FG#/Customer#: " +
                    TRIM(itemfg.i-no) + "/" + TRIM(itemfg.cust-no).

     IF v-newbal OR (v-lastper AND tb_close) THEN
       ASSIGN
        itemfg.beg-bal  = itemfg.q-onh
        itemfg.beg-date = v-begdate.

     ASSIGN
      itemfg.q-ptd      = 0
      itemfg.q-prod-ptd = 0
      itemfg.q-ship-ptd = 0
      itemfg.q-inv-ptd  = 0
      itemfg.ptd-msf    = 0
      itemfg.q-ord-ytd  = 0
      itemfg.q-prod-ytd = 0
      itemfg.q-ship-ytd = 0
      itemfg.q-inv-ytd  = 0
      itemfg.ytd-msf    = 0.

     ASSIGN
      lv-frst-date = 12/31/9999
      lv-last-date = 01/01/0001.

     FOR EACH period
         WHERE period.company EQ cocode
           AND period.yr      EQ v-peryr 
         NO-LOCK
         BY period.pnum:

       IF period.pst  LT lv-frst-date THEN lv-frst-date = period.pst.
       IF period.pend GT lv-last-date THEN lv-last-date = period.pend.
     END.

     RUN calc-tots (lv-frst-date, lv-last-date, v-peryr).

     IF v-lastper AND tb_close THEN DO:                /** MOVE YTD INFO. TO LAST YR **/
       ASSIGN
        itemfg.u-ord      = itemfg.q-ord-ytd
        itemfg.u-prod     = itemfg.q-prod-ytd
        itemfg.u-ship     = itemfg.q-ship-ytd
        itemfg.u-inv      = itemfg.q-inv-ytd
        itemfg.lyytd-msf  = itemfg.ytd-msf
        itemfg.q-ord-ytd  = 0
        itemfg.q-prod-ytd = 0
        itemfg.q-ship-ytd = 0
        itemfg.q-inv-ytd  = 0
        itemfg.ytd-msf    = 0
        itemfg.q-ptd      = 0
        itemfg.q-prod-ptd = 0
        itemfg.q-ship-ptd = 0
        itemfg.q-inv-ptd  = 0
        itemfg.ptd-msf    = 0.

       FOR EACH period
           WHERE period.company EQ cocode
             AND period.yr      EQ v-peryr + 1
             AND (period.pend   LT TODAY OR
                  (period.pst   LE TODAY AND
                   period.pend  GE TODAY))
           NO-LOCK
           BY period.pnum:

         tran-period = period.pnum.
         RUN calc-tots (period.pst, period.pend, period.yr).
         LEAVE.
       END.
     END.
 end.

STATUS DEFAULT "".

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

MESSAGE "FG End of Period Processing completed..." VIEW-AS ALERT-BOX INFORMATION.

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

