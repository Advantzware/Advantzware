&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-rmte&p.w

  Description: RM Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 05/23/2002

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
&IF DEFINED(UIB_IS_RUNNING) NE 0 &THEN
DEF VAR ip-post AS LOG INIT NO NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

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
 {oe/invwork.i NEW}
def var v-s-ra-no like oe-reth.ra-no no-undo.
def var v-e-ra-no like oe-reth.ra-no no-undo.
def var v-notes as log init yes no-undo.

def var v-tot-qty-return as integer format "->>>,>>>,>>9" no-undo.
def var v-qty-return-inv as integer format "->>>,>>>,>>9" no-undo.
def var v-g-tot-qty-return as integer format "->>>,>>>,>>9" no-undo.
def var v-g-qty-return-inv as integer format "->>>,>>>,>>9" no-undo.
def var time_stamp as character no-undo.
def var save_id as recid no-undo.
def var v-noted as log no-undo.
def var ll-valid as log no-undo.
def var v-postable as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-F

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-18 RECT-6 tran-date begin_ra-no ~
end_ra-no tb_prt-notes lv-ornt lines-per-page rd-dest lv-font-no ~
td-show-parm Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date begin_ra-no end_ra-no ~
tb_prt-notes lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBolCaseCnt C-Win 
FUNCTION getBolCaseCnt RETURNS DECIMAL
  ( ipInvRow AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE begin_ra-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning RA#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_ra-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending RA#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 16 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.57.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.1.

DEFINE VARIABLE tb_prt-notes AS LOGICAL INITIAL yes 
     LABEL "Print Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1.05 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-F
     tran-date AT ROW 2.91 COL 24 COLON-ALIGNED
     tran-period AT ROW 2.91 COL 62 COLON-ALIGNED
     begin_ra-no AT ROW 4.33 COL 24 COLON-ALIGNED HELP
          "Enter the beginning BOL number"
     end_ra-no AT ROW 4.33 COL 62 COLON-ALIGNED HELP
          "Enter the ending BOL number"
     tb_prt-notes AT ROW 6 COL 25
     lv-ornt AT ROW 11 COL 33 NO-LABEL
     lines-per-page AT ROW 11 COL 84 COLON-ALIGNED
     rd-dest AT ROW 11.95 COL 11 NO-LABEL
     lv-font-no AT ROW 14.33 COL 33.2 COLON-ALIGNED
     lv-font-name AT ROW 15.29 COL 33 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.24 COL 9
     Btn_OK AT ROW 18.86 COL 19
     Btn_Cancel AT ROW 18.86 COL 52
     "Selection Parameters" VIEW-AS TEXT
          SIZE 22 BY .95 AT ROW 1.48 COL 6
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 10.52 COL 5
     RECT-18 AT ROW 1 COL 1
     RECT-6 AT ROW 9.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93 BY 19.76.


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
         TITLE              = "Returns Edit & Post"
         HEIGHT             = 19.76
         WIDTH              = 93
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
/* SETTINGS FOR FRAME FRAME-F
   FRAME-NAME                                                           */
ASSIGN
       Btn_Cancel:PRIVATE-DATA IN FRAME FRAME-F     = 
                "ribbon-button".


ASSIGN
       Btn_OK:PRIVATE-DATA IN FRAME FRAME-F     = 
                "ribbon-button".


ASSIGN 
       begin_ra-no:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       end_ra-no:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-F
   NO-ENABLE                                                            */
ASSIGN 
       tb_prt-notes:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

ASSIGN 
       tran-date:PRIVATE-DATA IN FRAME FRAME-F     = 
                "parm".

/* SETTINGS FOR FILL-IN tran-period IN FRAME FRAME-F
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tran-period:HIDDEN IN FRAME FRAME-F           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Returns Edit  Post */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Returns Edit  Post */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ra-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ra-no C-Win
ON LEAVE OF begin_ra-no IN FRAME FRAME-F /* Beginning RA# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME FRAME-F /* Cancel */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME FRAME-F /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  run check-date.
  if not ll-valid then return no-apply.

  ASSIGN
    tran-date 
    tran-period
    v-s-ra-no   = begin_ra-no
    v-e-ra-no   = END_ra-no
    v-notes     = tb_prt-notes
    v-postable  = NO.

  run run-report. 


  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF v-postable AND ip-post THEN do:
    lv-post = NO.

    MESSAGE "Post Returns?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:
      RUN post-returns.

      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
  END.

  ELSE IF ip-post THEN MESSAGE "No Returns available for posting..." VIEW-AS ALERT-BOX ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ra-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ra-no C-Win
ON LEAVE OF end_ra-no IN FRAME FRAME-F /* Ending RA# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-F /* Lines Per Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-F /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-F /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-F
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-F
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-F
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-notes C-Win
ON VALUE-CHANGED OF tb_prt-notes IN FRAME FRAME-F /* Print Notes? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-F /* Show Parameters? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-F /* Transaction Date */
DO:
  assign {&self-name}.

  if lastkey ne -1 then do:
    run check-date.
    if not ll-valid then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */    
{sys/inc/f3helpw.i}
DEF VAR choice AS LOG NO-UNDO.

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
     RETURN .
  END.
ASSIGN
  c-win:TITLE = IF ip-post THEN "Return Posting/Post Returns"
                            ELSE "Return Edit List".
  tran-date = TODAY.
 RUN enable_UI.

  /*RUN check-date.*/

  IF NOT ip-post THEN DISABLE tran-date WITH FRAME {&FRAME-NAME}.

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
    ll-valid = YES.

    find first period                   
        where period.company eq cocode
          and period.pst     le tran-date
          and period.pend    ge tran-date
        no-lock no-error.
    if avail period then tran-period:SCREEN-VALUE = string(period.pnum).

    else
    IF ip-post THEN DO:
      message "No Defined Period Exists for" tran-date view-as alert-box error.
      ll-valid = NO.
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
  DISPLAY tran-date begin_ra-no end_ra-no tb_prt-notes lv-ornt lines-per-page 
          rd-dest lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE RECT-18 RECT-6 tran-date begin_ra-no end_ra-no tb_prt-notes lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm Btn_OK Btn_Cancel 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
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
  /*   RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
*/
     RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
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
 run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-returns C-Win 
PROCEDURE post-returns :
/* --------------------------------------------------- oe/oe-retpo.p  8/94 RM */
/* Order Processing Return Post Program - O/E Module                          */
/* -------------------------------------------------------------------------- */


DEFINE VARIABLE v-memo-no    AS INTEGER            FORMAT ">>>>>>>>>>" NO-UNDO.
DEFINE VARIABLE v-c-no       LIKE ar-cash.c-no       NO-UNDO.
DEFINE VARIABLE v-l-no       LIKE ar-cash.c-no       NO-UNDO.
DEFINE VARIABLE v-cas-cnt    LIKE itemfg.case-count.
DEFINE VARIABLE v-fac        AS INTEGER.

DEFINE VARIABLE t1           AS DECIMAL            FORMAT "->,>>>,>>9.99".
DEFINE VARIABLE g2           AS DECIMAL            FORMAT "->,>>>,>>9.99".
DEFINE VARIABLE v-on-act-amt AS DECIMAL.
DEFINE VARIABLE xtrnum       AS INTEGER.
DEFINE VARIABLE xar-acct     AS CHARACTER.
DEFINE VARIABLE xsl-acct     AS CHARACTER.
DEFINE VARIABLE xfr-acct     AS CHARACTER.
DEFINE VARIABLE xtx-acct     AS CHARACTER.
DEFINE VARIABLE v-disc       LIKE ar-invl.disc       NO-UNDO.
DEF VAR ldAmt-Disc AS DEC NO-UNDO.


FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK.
ASSIGN
xtrnum        = gl-ctrl.trnum + 1
gl-ctrl.trnum = xtrnum.
RELEASE gl-ctrl.

FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.
ASSIGN
xar-acct = ar-ctrl.receivables
xsl-acct = ar-ctrl.sales
xfr-acct = ar-ctrl.freight
xtx-acct = ar-ctrl.stax.
RELEASE ar-ctrl.

FIND LAST ar-cash USE-INDEX c-no NO-LOCK NO-ERROR.
IF avail ar-cash THEN v-c-no = ar-cash.c-no.

FOR EACH ar-cash FIELDS(check-no)
  WHERE ar-cash.company EQ cocode
  AND ar-cash.memo    EQ YES
  NO-LOCK
  BREAK BY ar-cash.check-no DESCENDING:
  v-memo-no = ar-cash.check-no.
  LEAVE.
END.
IF v-memo-no LT 90000000 THEN v-memo-no = 90000000.

g2 = 0.
FOR EACH work-job:
  DELETE work-job.
END.

FOR EACH oe-reth
  WHERE oe-reth.company EQ cocode
  AND oe-reth.posted  EQ NO
  AND oe-reth.applied EQ YES
  AND oe-reth.ra-no   GE v-s-ra-no
  AND oe-reth.ra-no   LE v-e-ra-no
  USE-INDEX ra-no
  TRANSACTION:

  ASSIGN
  oe-reth.posted = YES
  v-c-no         = v-c-no + 1
  v-memo-no      = v-memo-no + 1
  ldAmt-Disc     = 0.

  /* create CREDIT MEMO RECORD FOR RETURN */
  CREATE ar-cash.
  ASSIGN
  ar-cash.memo       = YES
  ar-cash.c-no       = v-c-no
  ar-cash.company    = oe-reth.company
  ar-cash.check-date = TODAY
  ar-cash.check-amt  = oe-reth.tot-return-amt
  ar-cash.cust-no    = oe-reth.cust-no
  ar-cash.check-no   = v-memo-no.

  /* UPDATE FINISHED GOODS INVENTORY & GL WORK FILE */
  FOR EACH oe-retl
    WHERE oe-retl.company EQ oe-reth.company
    AND oe-retl.r-no    EQ oe-reth.r-no
    EXCLUSIVE-LOCK
    BREAK BY oe-retl.i-no:

    FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-retl.i-no
      NO-LOCK.

    RELEASE fgcat.
    IF AVAIL itemfg THEN
      FIND FIRST fgcat
        WHERE fgcat.company EQ itemfg.company
          AND fgcat.procat  EQ itemfg.procat
        NO-LOCK NO-ERROR.

    v-fac = IF oe-retl.uom       EQ "CS" AND
              avail itemfg              AND
              itemfg.case-count NE 0    THEN itemfg.case-count
              ELSE IF oe-retl.uom EQ "C"   THEN 100
              ELSE IF oe-retl.uom EQ "M"   THEN 1000 ELSE 1.

    /* Accumulate disc */
    FIND ar-inv
        WHERE ar-inv.company = oe-reth.company
          AND ar-inv.cust-no = oe-reth.cust-no
          AND ar-inv.inv-no = oe-reth.inv-no
        NO-LOCK NO-ERROR.

    v-disc = 0.
    ldAmt-Disc = 0 .
    IF AVAILABLE ar-inv THEN
    DO:
      FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no
          AND ar-invl.i-no = oe-retl.i-no
          NO-LOCK NO-ERROR.
        IF AVAILABLE ar-invl THEN v-disc = ar-invl.disc.
    END.

    ldAmt-Disc = ldAmt-Disc + 
        (ROUND((IF avail ar-invl THEN ar-invl.unit-pr ELSE oe-retl.unit-pr) * oe-retl.tot-qty-return / v-fac
        * (100 - v-disc) / 100, 2)).

    STATUS DEFAULT "Processing... ".      

    IF last-of(oe-retl.i-no) THEN DO:

        STATUS DEFAULT "Processing Summary...".      
     /* Summarize multiple records for an item and write to ar-cashl */
      FIND ar-inv
          WHERE ar-inv.company = oe-reth.company
            AND ar-inv.cust-no = oe-reth.cust-no
            AND ar-inv.inv-no = oe-reth.inv-no
          NO-LOCK NO-ERROR.
      v-disc = 0.
      IF AVAILABLE ar-inv THEN
      DO:
        FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no
        AND ar-invl.i-no = oe-retl.i-no
        NO-LOCK NO-ERROR.
        IF AVAILABLE ar-invl THEN v-disc = ar-invl.disc.
      END.

      CREATE ar-cashl.
      ASSIGN
      ar-cashl.memo     = YES
      ar-cashl.c-no     = v-c-no
      ar-cashl.LINE     = oe-retl.LINE
      ar-cashl.actnum   = IF AVAIL fgcat THEN fgcat.glacc ELSE xsl-acct
      ar-cashl.company  = oe-reth.company
      ar-cashl.inv-no   = oe-reth.inv-no
      ar-cashl.cust-no  = oe-reth.cust-no
      ar-cashl.check-no = STRING(v-memo-no)
      ar-cashl.amt-disc = ldAmt-Disc      
      ar-cashl.dscr     = "CREDIT MEMO CREATED FROM OE RETURN - ITEMS"
      ar-cashl.dscr     = TRIM(ar-cashl.dscr) +
      FILL(" ",50 - LENGTH(TRIM(ar-cashl.dscr))) +
      STRING(oe-reth.r-no,"999999999999")
      ar-cashl.returnNote     = TRIM(SUBSTR(ar-cashl.dscr,
                                     INDEX(ar-cashl.dscr,"OE RETURN") + 12,10))
      ar-cashl.returnRno   = oe-reth.r-no.

          END. /* Create of ar-cashl */
    IF AVAIL(ar-invl) THEN
    v-cas-cnt = getBolCaseCnt(ROWID(ar-invl)).
    X         = 0.

    FIND FIRST oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.ord-no  EQ oe-retl.ord-no
        AND oe-ordl.i-no    EQ oe-retl.i-no
      NO-ERROR.

    IF avail oe-ordl THEN
    DO:

      ASSIGN
      oe-ordl.ship-qty = oe-ordl.ship-qty -
      MIN(oe-ordl.ship-qty,oe-retl.qty-return-inv).
      IF v-cas-cnt EQ 0 THEN
        v-cas-cnt        = oe-ordl.cas-cnt.

      FIND CURRENT oe-ordl NO-LOCK.
    END.

    IF v-cas-cnt EQ 0 THEN
      v-cas-cnt = itemfg.case-count.

    FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
      IF AVAIL fg-rctd AND fg-rctd.r-no GT X THEN X = fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
      IF AVAIL fg-rcpth AND fg-rcpth.r-no GT X THEN X = fg-rcpth.r-no.
/*     FOR EACH fg-rctd WHERE fg-rctd.company EQ oe-retl.company NO-LOCK */
/*       BY fg-rctd.r-no DESC:                                           */
/*       X = fg-rctd.r-no.                                               */
/*       LEAVE.                                                          */
/*     END.                                                              */
/*                                                                       */
/*     FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.               */
/*     IF AVAIL fg-rcpth AND fg-rcpth.r-no GT X THEN X = fg-rcpth.r-no.  */
/*                                                                       */
    CREATE fg-rctd.
    ASSIGN
    fg-rctd.r-no      = X + 1
    fg-rctd.rct-date  = TODAY
    fg-rctd.trans-time = TIME
    fg-rctd.company   = cocode
    fg-rctd.loc       = oe-retl.loc
    fg-rctd.loc-bin   = oe-retl.loc-bin
    fg-rctd.tag       = oe-retl.tag
    fg-rctd.rita-code = "E"
    fg-rctd.pur-uom   = itemfg.prod-uom
    fg-rctd.t-qty     = oe-retl.qty-return-inv
    fg-rctd.std-cost  = oe-retl.cost
    fg-rctd.ext-cost  = fg-rctd.std-cost * (fg-rctd.t-qty / 1000)
    fg-rctd.qty-case  = v-cas-cnt
    fg-rctd.i-no      = oe-retl.i-no
    fg-rctd.i-name    = oe-retl.i-name
    fg-rctd.job-no    = oe-retl.job-no
    fg-rctd.job-no2   = oe-retl.job-no2.

    RUN sys/ref/convcuom.p("M", fg-rctd.pur-uom, 0, 0, 0, 0,
    fg-rctd.std-cost, OUTPUT fg-rctd.std-cost).

    IF v-cas-cnt GT 0 THEN
      ASSIGN
      fg-rctd.partial = fg-rctd.t-qty MODULO v-cas-cnt
      fg-rctd.cases   = trunc(fg-rctd.t-qty / v-cas-cnt,0).
    ELSE
      ASSIGN
      fg-rctd.partial = fg-rctd.t-qty
      fg-rctd.cases   = 0.

    IF fg-rctd.t-qty LE 0 OR
    fg-rctd.cases EQ ? THEN fg-rctd.cases = 0.

    FIND FIRST fg-bin
      WHERE fg-bin.company EQ fg-rctd.company
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.loc     EQ fg-rctd.loc
        AND fg-bin.loc-bin EQ fg-rctd.loc-bin
        AND fg-bin.tag     EQ fg-rctd.tag
      NO-ERROR.
    IF NOT avail fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
      fg-bin.company    = fg-rctd.company
      fg-bin.loc        = fg-rctd.loc
      fg-bin.loc-bin    = fg-rctd.loc-bin
      fg-bin.i-no       = fg-rctd.i-no
      fg-bin.job-no     = fg-rctd.job-no
      fg-bin.job-no2    = fg-rctd.job-no2
      fg-bin.tag        = fg-rctd.tag
      fg-bin.aging-date = fg-rctd.rct-date
      fg-bin.pur-uom    = fg-rctd.pur-uom.
    END.

    IF fg-bin.case-count EQ 0 OR
    fg-bin.case-count EQ ? THEN fg-bin.case-count = v-cas-cnt.

    FIND FIRST job-hdr
      WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no  EQ fg-bin.job-no
        AND job-hdr.job-no2 EQ fg-bin.job-no2
        AND job-hdr.i-no    EQ fg-bin.i-no
      USE-INDEX job-no NO-LOCK NO-ERROR.

    ASSIGN
    fg-bin.std-mat-cost = IF avail job-hdr THEN job-hdr.std-mat-cost
    ELSE itemfg.std-mat-cost
    fg-bin.std-lab-cost = IF avail job-hdr THEN job-hdr.std-lab-cost
    ELSE itemfg.std-lab-cost
    fg-bin.std-fix-cost = IF avail job-hdr THEN job-hdr.std-fix-cost
    ELSE itemfg.std-fix-cost
    fg-bin.std-var-cost = IF avail job-hdr THEN job-hdr.std-var-cost
    ELSE itemfg.std-var-cost
    fg-bin.std-tot-cost = IF avail job-hdr THEN job-hdr.std-tot-cost
    ELSE itemfg.std-tot-cost
    fg-bin.last-cost    = IF avail job-hdr THEN job-hdr.std-tot-cost
    ELSE itemfg.std-tot-cost.

    FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    IF avail itemfg AND avail jc-ctrl THEN DO:
      FOR EACH prodl
        WHERE prodl.company EQ cocode
        AND prodl.procat  EQ itemfg.procat
        NO-LOCK,
        FIRST prod
        WHERE prod.company EQ cocode
        AND prod.prolin  EQ prodl.prolin
        NO-LOCK:

        RUN oe/invpostx.p(INPUT prod.cgs-dl,  fg-bin.std-lab-cost,
        oe-retl.qty-return-inv, NO,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.fg-lab,  fg-bin.std-lab-cost,
        oe-retl.qty-return-inv, YES,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.cgs-fo,  fg-bin.std-fix-cost,
        oe-retl.qty-return-inv, NO,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.fg-fo ,  fg-bin.std-fix-cost,
        oe-retl.qty-return-inv, YES,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.cgs-vo,  fg-bin.std-var-cost,
        oe-retl.qty-return-inv, NO,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.fg-vo ,  fg-bin.std-var-cost,
        oe-retl.qty-return-inv, YES,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.cgs-mat, fg-bin.std-mat-cost,
        oe-retl.qty-return-inv, NO,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        RUN oe/invpostx.p(INPUT prod.fg-mat,  fg-bin.std-mat-cost,
        oe-retl.qty-return-inv, YES,itemfg.i-no,
        oe-reth.inv-no, itemfg.prod-uom).

        LEAVE.
      END.

      v-l-no = oe-retl.LINE.
    END.
  END. /* for each oe-retl record */

  FIND LAST oe-retl USE-INDEX r-no
  WHERE oe-retl.company = oe-reth.company
  AND oe-retl.r-no = oe-reth.r-no NO-LOCK NO-ERROR.
  IF AVAILABLE oe-retl THEN v-l-no = oe-retl.LINE.

  IF oe-reth.tot-freight NE 0 THEN DO:
    CREATE ar-cashl.
    ASSIGN
    ar-cashl.memo     = YES
    ar-cashl.c-no     = v-c-no
    v-l-no            = v-l-no + 1
    ar-cashl.LINE     = v-l-no
    ar-cashl.actnum   = xfr-acct
    ar-cashl.company  = oe-reth.company
    ar-cashl.inv-no   = oe-reth.inv-no
    ar-cashl.cust-no  = oe-reth.cust-no
    ar-cashl.check-no = STRING(v-memo-no)
    ar-cashl.amt-disc = oe-reth.tot-freight
    ar-cashl.dscr     = "CREDIT MEMO CREATED FROM OE RETURN - FREIGHT"
    ar-cashl.dscr     = TRIM(ar-cashl.dscr) +
    FILL(" ",50 - LENGTH(TRIM(ar-cashl.dscr))) +
    STRING(oe-reth.r-no,"999999999999")
    ar-cashl.returnNote     = TRIM(SUBSTR(ar-cashl.dscr,
                                     INDEX(ar-cashl.dscr,"OE RETURN") + 12,10))
    ar-cashl.returnRno   = oe-reth.r-no.

      END.

  IF oe-reth.tot-tax NE 0 THEN DO:
    CREATE ar-cashl.
    ASSIGN
    ar-cashl.memo     = YES
    ar-cashl.c-no     = v-c-no
    v-l-no            = v-l-no + 1
    ar-cashl.LINE     = v-l-no
    ar-cashl.actnum   = xtx-acct
    ar-cashl.company  = oe-reth.company
    ar-cashl.inv-no   = oe-reth.inv-no
    ar-cashl.cust-no  = oe-reth.cust-no
    ar-cashl.check-no = STRING(v-memo-no)
    ar-cashl.amt-disc = oe-reth.tot-tax
    ar-cashl.dscr     = "CREDIT MEMO CREATED FROM OE RETURN - TAX"
    ar-cashl.dscr     = TRIM(ar-cashl.dscr) +
    FILL(" ",50 - LENGTH(TRIM(ar-cashl.dscr))) +
    STRING(oe-reth.r-no,"999999999999")
    ar-cashl.returnNote     = TRIM(SUBSTR(ar-cashl.dscr,
                                     INDEX(ar-cashl.dscr,"OE RETURN") + 12,10))
    ar-cashl.returnRno   = oe-reth.r-no.

  END.
END. /* for each oe-reth record */

RELEASE ar-cash.
RELEASE ar-cashl.
RELEASE fg-rctd.
FIND CURRENT fg-bin NO-LOCK NO-ERROR.

CREATE gltrans.
ASSIGN
gltrans.company = cocode
gltrans.actnum  = xar-acct
gltrans.jrnl    = "DBMEM"
gltrans.tr-dscr = "CREDIT/DEBIT MEMO"
gltrans.tr-date = tran-date
gltrans.tr-amt  = + g2
gltrans.period  = tran-period
gltrans.trnum   = xtrnum.
IF gltrans.tr-amt LT 0 THEN gltrans.jrnl = "CRMEM".

RELEASE gltrans.

FOR EACH work-job BREAK BY work-job.actnum:
  CREATE gltrans.
  ASSIGN
  gltrans.company = cocode
  gltrans.actnum  = work-job.actnum
  gltrans.jrnl    = "OEINV"
  gltrans.tr-date = tran-date
  gltrans.period  = tran-period
  gltrans.trnum   = xtrnum.

  IF work-job.fg THEN
    ASSIGN
    gltrans.tr-amt  = work-job.amt
    gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
  ELSE
    ASSIGN
    gltrans.tr-amt  = - work-job.amt
    gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".

  RELEASE gltrans.
END. /* each work-job */
STATUS DEFAULT "Posting Complete.".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- oe/oe-retel.p  9/94 RM */
/* Order Processing Return Edit List Program - O/E Module                     */
/* -------------------------------------------------------------------------- */
{sys/form/r-topw.f}

def var v-s-ra-no like oe-reth.ra-no no-undo.
def var v-e-ra-no like oe-reth.ra-no no-undo.
def var v-notes as log init yes no-undo.

def var v-tot-qty-return as integer format "->>>,>>>,>>9" no-undo.
def var v-qty-return-inv as integer format "->>>,>>>,>>9" no-undo.
def var v-g-tot-qty-return as integer format "->>>,>>>,>>9" no-undo.
def var v-g-qty-return-inv as integer format "->>>,>>>,>>9" no-undo.
def var time_stamp as character no-undo.
def var save_id as recid no-undo.
def var v-noted as log no-undo.
DISPLAY WITH frame ra-no.

form header
"RA #     Customer                         Invoice #   Order #   PO #             Item              Qty Returned   Qty Returned Inv." at 1
   fill("_",132) format "x(132)"
   with frame f-top WIDTH 132 STREAM-IO no-box page-top.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}
       v-s-ra-no   = begin_ra-no
       v-e-ra-no   = END_ra-no
       v-notes     = tb_prt-notes.



{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.
display with frame f-top. 

for each oe-reth where oe-reth.company = cocode and
                          oe-reth.posted  = no and
                          oe-reth.applied = yes and
                          oe-reth.ra-no   >= v-s-ra-no and
                          oe-reth.ra-no   <= v-e-ra-no
                          use-index ra-no no-lock:

       assign v-tot-qty-return = 0
              v-qty-return-inv = 0.

       find first cust where cust.company = oe-reth.company and
                             cust.cust-no = oe-reth.cust-no
                             use-index cust no-lock no-error.

       put oe-reth.ra-no at 1
           oe-reth.cust-no at 10
           cust.name at 20 format "x(20)"
           oe-reth.inv-no at 43.

       v-noted = no.

       if v-notes then do i = 1 to 4:
         if oe-reth.notes[i] ne "" then do:
           if not v-noted then put skip(1) "Notes:" at 10.
           put trim(oe-reth.notes[i]) format "x(60)" at 17 skip.
           v-noted = yes.
         end.
       end.

       if v-noted then put skip(1).

       for each oe-retl where oe-retl.company = oe-reth.company and
                              oe-retl.r-no = oe-reth.r-no
                              use-index r-no no-lock:

           put oe-retl.ord-no at 55
               oe-retl.po-no at 65
               oe-retl.i-no at 82
               oe-retl.tot-qty-return at 100 format "->,>>>,>>9"
               oe-retl.qty-return-inv at 115 format "->,>>>,>>9".

           assign v-tot-qty-return = v-tot-qty-return + oe-retl.tot-qty-return
                  v-qty-return-inv = v-qty-return-inv + oe-retl.qty-return-inv.


         v-noted = no.

         if v-notes then do i = 1 to 4:
           if oe-retl.notes[i] ne "" then do:
             if not v-noted then put skip(1) "Notes:" at 65.
             put trim(oe-retl.notes[i]) format "x(60)" at 72 skip.
             v-noted = yes.
           end.
         end.

         if v-noted then put skip(1).

         v-postable = YES.
     end. /* for each oe-retl record */

     put skip(1)
         "***** Return Totals:" at 77
         v-tot-qty-return at 98
         v-qty-return-inv at 113
         skip(1).

     assign v-g-tot-qty-return = v-g-tot-qty-return + v-tot-qty-return
            v-g-qty-return-inv = v-g-qty-return-inv + v-qty-return-inv.

 end. /* for each oe-reth record */

 put skip(1)
     "***** Grand Totals:" at 78
     v-g-tot-qty-return at 98
     v-g-qty-return-inv at 113.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBolCaseCnt C-Win 
FUNCTION getBolCaseCnt RETURNS DECIMAL
  ( ipInvRow AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER bf-ar-invl FOR ar-invl.
  DEF BUFFER bf-oe-boll FOR oe-boll.
  FIND FIRST bf-ar-invl 
    WHERE rowid(bf-ar-invl) EQ ipInvRow NO-LOCK NO-ERROR.
  IF AVAIL bf-ar-invl THEN
  FIND FIRST bf-oe-boll 
    WHERE bf-oe-boll.company EQ bf-ar-invl.company 
      AND bf-oe-boll.b-no EQ bf-ar-invl.b-no 
      AND bf-oe-boll.i-no EQ bf-ar-invl.i-no
      AND (IF oe-retl.tag GT "" THEN bf-oe-boll.tag  EQ oe-retl.tag ELSE TRUE)
    NO-LOCK NO-ERROR.
  IF AVAIL bf-oe-boll THEN
    RETURN bf-oe-boll.qty-case.   /* or .cases */
  ELSE
    RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

