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
DEF VAR list-name AS cha NO-UNDO.
DEF VAR init-dir AS CHA NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

/*
def new shared var v-ar-acct like ar-ctrl.receivables.
def new shared var v-ar-freight like ar-ctrl.freight.
def new shared var v-ar-stax like ar-ctrl.stax.
def new shared var v-ar-sales like ar-ctrl.sales.
def new shared var v-ar-disc like ar-ctrl.discount.
def new shared var v-return as log init no.
def new shared var v-start2-compress as char.
def new shared var v-end2-compress as char.
def new shared var v-pr-port like printer.pr-port.
def new shared var v-back like itemfg.q-back.
def new shared var v-balance as dec format ">>>,>>>,>>9.99cr".
def new shared var v-reduce-ord-bal like cust.ord-bal no-undo.
def new shared var v-invline as recid.
def new shared var v-invhead as recid.
def new shared var v-detail   as   log format "Detail/Summary" init no no-undo.
def new shared var v-gldetail as   log format "Detail/Summary" init no no-undo.
*/
DEF NEW SHARED VAR v-post AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR v-trnum AS INT NO-UNDO.

DEF VAR v-unline AS CHAR FORMAT "x(80)" INIT
  "--------------- ------------------------- ------- ----------- ---".
DEF VAR time_stamp AS ch.
/*
def var v-tot-qty as int.
def var v-fr-tax as log init no.
def var v-line-price like oe-ordl.price.
def var v-t-comm like inv-head.t-comm.
def var v-xno like ar-inv.x-no. /* Unique Internial # for header */
def var v-xline as int.     /* Unique Internail # for lines */
def var v-bol-bal like oe-boll.qty.
def var v-inv-qty like oe-ordl.inv-qty.
def var v-ord-no like inv-line.ord-no.
def var v-ord-date as date.
def var v-inv-disc as dec format "->>,>>9.99".
def var v-invl-pric as dec.
def var save_id as recid.
def var v-tax-rate as dec extent 4.
def var v-qty-fact as dec no-undo.
def var v-uninv-ordl-amt like oe-ordl.t-price no-undo init 0.
def var v-u-inv like oe-ctrl.u-inv init false.
def var v-tmp-tax-rate as dec format "->>>.99".
def var v-frt-tax-rate as dec format "->>>.99".
def var v-bol-qty like oe-boll.qty.
def var v-rcpth-no as int.
def var v-frst as log init no.
def var v-line-tot like inv-line.t-price.
def var v-misc-tot like inv-misc.amt.
def var v-post-zero-cgs as log no-undo.
def var v-export as log no-undo.
def var v-rec-written as int no-undo.
def var t-rec-written as int no-undo.
def var v-s-inv-no like inv-head.inv-no init 0 no-undo.
def var v-e-inv-no like v-s-inv-no init 999999.
def var v-s-date   like inv-head.inv-date format "99/99/9999"
                                          init 01/01/0001 no-undo.
def var v-e-date   like v-s-date init today.
def var v-file-name AS CHAR.
def var v-cost as dec extent 4.
def var v-cas-cnt like itemfg.case-count.

def var v-close-qty like oe-ordl.qty.
def var v-temp-tax-rate     as   dec format "->>>.99".
def var v-temp-frt-tax-rate as   dec format "->>>.99".
def var v-dcr-val      like oe-ordl.cost init 0.
def var v-uom-rate     as   int.
def var v-sum-rel-qty as dec no-undo.
def var v-tax as dec.
*/
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR v-frt-acct LIKE ap-ctrl.freight NO-UNDO.
DEF VAR xap-acct AS CHAR NO-UNDO.
DEF TEMP-TABLE work-gl NO-UNDO
  FIELD actnum  LIKE account.actnum
  FIELD debits  AS DEC
  FIELD credits AS DEC
  INDEX actnum actnum.

DEF VAR lv-frt-total AS DEC NO-UNDO.  /* accum total */
DEF VAR v-postable AS LOG INIT NO NO-UNDO.
{sa/sa-sls01.i}

DO FOR ap-ctrl TRANSACTION:
  FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
  IF NOT AVAIL ap-ctrl THEN RETURN.
  ASSIGN xap-acct = ap-ctrl.payables
         v-frt-acct = ap-ctrl.freight.
  RELEASE ap-ctrl.
END.
DEF VAR v-fgpostgl AS LOG NO-UNDO.
DO TRANSACTION :
  {sys/inc/fgpostgl.i}
  v-fgpostgl = fgpostgl NE "None".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date begin_vend end_vend ~
begin_date end_date tb_sort rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period begin_vend end_vend ~
begin_date end_date lbl_sort tb_sort rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Transaction Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tran-period AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Period" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

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
     SIZE 23 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.43.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.52.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL NO 
     LABEL "Print G/L Account Description?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
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
     rd-dest AT ROW 12.43 COL 8 NO-LABEL
     lv-ornt AT ROW 12.67 COL 29 NO-LABEL
     lines-per-page AT ROW 12.67 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 13.86 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 15.05 COL 26 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.48 COL 8
     btn-ok AT ROW 18.38 COL 23
     btn-cancel AT ROW 18.38 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.48 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 11 COL 1
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
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
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
THEN C-Win:HIDDEN = NO.

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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Invoice Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  ASSIGN rd-dest.

  RUN check-date.
  IF v-invalid THEN RETURN NO-APPLY.
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

  RUN run-report.

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
  END CASE.

  IF v-postable THEN DO:

    lv-post = NO.

    MESSAGE "Post Invoices?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN DO:
      RUN post-gl.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
    ELSE RUN undo-trnum.
  END.

  ELSE DO:
      MESSAGE "No Invoices available for posting..." VIEW-AS ALERT-BOX ERROR.
      RUN undo-trnum.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Invoice Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
   ASSIGN lv-font-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
DO:
  ASSIGN lv-ornt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON VALUE-CHANGED OF lv-ornt IN FRAME FRAME-A
DO:
  {custom/chgfont.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Print G/L Account Description? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME td-show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-show-parm C-Win
ON VALUE-CHANGED OF td-show-parm IN FRAME FRAME-A /* Show Parameters? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-date C-Win
ON LEAVE OF tran-date IN FRAME FRAME-A /* Transaction Date */
DO:
  ASSIGN {&self-name}.

  IF LASTKEY NE -1 THEN DO:
    RUN check-date.
    IF v-invalid THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tran-period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tran-period C-Win
ON LEAVE OF tran-period IN FRAME FRAME-A /* Period */
DO:
  ASSIGN {&self-name}.
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


/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.


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
  DO WITH FRAME {&frame-name}:
    v-invalid = NO.

    FIND FIRST period                   
        WHERE period.company EQ cocode
          AND period.pst     LE tran-date
          AND period.pend    GE tran-date
        NO-LOCK NO-ERROR.
    IF AVAIL period THEN tran-period:SCREEN-VALUE = STRING(period.pnum).

    ELSE DO:
      MESSAGE "No Defined Period Exists for" tran-date VIEW-AS ALERT-BOX ERROR.
      v-invalid = YES.
    END.
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
          tb_sort rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date begin_vend end_vend begin_date end_date 
         tb_sort rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel 
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

     IF init-dir = "" THEN init-dir = "c:\temp" .
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

   RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
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
 DEF VAR v-po-no LIKE fg-rcpth.po-no NO-UNDO.
 DEF VAR total-msf LIKE ap-invl.amt-msf NO-UNDO.
 DEF VAR v-qty LIKE ap-invl.qty.
DEF VAR v-qty1 LIKE v-qty.
DEF VAR v-qty2 LIKE v-qty.
DEF VAR v-qty3 LIKE v-qty.
DEF VAR v-cost LIKE fg-rdtlh.cost.

  /** POST TO GENERAL LEDGER ACCOUNTS TRANSACTION FILE **/
  postit:
DO TRANSACTION ON ERROR UNDO postit:
  g2 = 0.

  FOR EACH report
      WHERE report.term-id EQ v-term
        AND CAN-FIND(FIRST ap-inv WHERE RECID(ap-inv) EQ report.rec-id
                                    AND ap-inv.posted EQ NO):

    FIND FIRST ap-inv
        WHERE RECID(ap-inv) EQ report.rec-id
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

    IF NOT AVAIL ap-inv THEN DO:
      MESSAGE "Unable to Post due to Invoice Record being Locked.  " +
              "Please Try again Later".
      PAUSE.
      HIDE MESSAGE NO-PAUSE.
      UNDO postit, LEAVE postit.
    END.
ASSIGN
    ap-inv.period = tran-period
    ap-inv.postedDate = tran-date
    ap-inv.runNumber  = v-trnum
    ap-inv.glYear     = YEAR(tran-date)
    v-upd = YES.

    FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK,

        FIRST po-ordl
        WHERE po-ordl.company   EQ cocode
          AND po-ordl.po-no     EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                                                          ELSE ap-invl.po-no)
          AND po-ordl.line      EQ {ap/invlline.i -1}
          AND po-ordl.item-type EQ NO
        USE-INDEX po-no NO-LOCK:

      v-po-no = TRIM(STRING(po-ordl.po-no,">>>>>>>>>>")).

      FIND FIRST fg-rcpth
          WHERE fg-rcpth.company   EQ cocode
            AND fg-rcpth.i-no      EQ po-ordl.i-no
            AND fg-rcpth.po-no     EQ v-po-no
            AND fg-rcpth.rita-code EQ "R"
          USE-INDEX item-po NO-LOCK NO-ERROR.
      IF NOT AVAIL fg-rcpth THEN DO:
        v-upd = NO.
        LEAVE.
      END.  

      FIND FIRST fg-rcpts
          WHERE fg-rcpts.company   EQ cocode
            AND fg-rcpts.i-no      EQ po-ordl.i-no
            AND fg-rcpts.po-no     EQ v-po-no
            AND fg-rcpts.rita-code EQ "R"
          USE-INDEX i-no NO-LOCK NO-ERROR.
      IF AVAIL fg-rcpts THEN DO:
        v-upd = NO.
        LEAVE.
      END.
    END.  

    IF NOT v-upd THEN NEXT.

    FIND FIRST vend
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ ap-inv.vend-no
        USE-INDEX vend NO-LOCK.

    FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no:
     RUN spCreateGLHist(cocode,
                        ap-invl.actnum,
                        "ACPAY",
                        vend.name  + "  " + string(ap-inv.inv-date),
                        tran-date,
                        ap-invl.amt,
                        v-trnum,
                        tran-period,
                        "A",
                        tran-date,
                        string(ap-inv.inv-no),
                        "AP").
      ASSIGN
       t1 = t1 + ap-invl.amt
       g2 = g2 + ap-invl.amt
       total-msf = total-msf + ap-invl.amt-msf
       ap-invl.posted  = YES.

      FIND FIRST po-ordl
          WHERE po-ordl.company EQ cocode
            AND po-ordl.po-no   EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                                                          ELSE ap-invl.po-no)
            AND po-ordl.line    EQ {ap/invlline.i -1}
          USE-INDEX po-no NO-ERROR.
      IF AVAIL po-ordl THEN DO:
        FIND FIRST reftable
            {ap/apreftbw.i po-ordl.po-no}
              AND reftable.code2 EQ string(ap-invl.i-no,"9999999999")
            NO-LOCK NO-ERROR.
        IF NOT AVAIL reftable THEN DO:
          {ap/addreftb.i po-ordl.po-no}
        END.

        po-ordl.t-inv-qty = po-ordl.t-inv-qty + ap-invl.qty.

        /* Ensure receipts = payables */
        IF NOT po-ordl.item-type AND v-fgpostgl THEN DO:
          RELEASE prod.
          FIND FIRST itemfg
              WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ po-ordl.i-no
              NO-ERROR.

          IF AVAIL itemfg THEN
          FIND FIRST prodl
              WHERE prodl.company EQ cocode
                AND prodl.procat  EQ itemfg.procat
                AND CAN-FIND(FIRST prod
                             WHERE prod.company EQ cocode
                               AND prod.prolin  EQ prodl.prolin)
              NO-LOCK NO-ERROR.

          IF AVAIL prodl THEN
          FIND FIRST prod
              WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin
              NO-LOCK NO-ERROR.

          IF AVAIL itemfg THEN DO:
            RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, "EA", 0, 0, 0, 0,
                                    ap-invl.qty, OUTPUT v-qty1).

            ASSIGN
             v-po-no = TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
             v-qty   = 0
             v-cost  = ap-invl.amt / (v-qty1 / 1000).

            FOR EACH fg-rcpth
                WHERE fg-rcpth.company   EQ cocode
                  AND fg-rcpth.i-no      EQ po-ordl.i-no
                  AND fg-rcpth.po-no     EQ v-po-no
                  AND fg-rcpth.rita-code EQ "R"
                USE-INDEX item-po,

                EACH fg-rdtlh
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                  AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                  AND substr(fg-rdtlh.receiver-no,1,10) EQ
                                             string(ap-inv.i-no,"9999999999")

                BREAK BY fg-rcpth.trans-date
                      BY fg-rcpth.r-no
                      BY RECID(fg-rdtlh):

              /* Remove the accrued AP & FG assets created for FG receipt */
              IF fg-rdtlh.cost NE 0 THEN DO:
                IF AVAIL prod         AND
                   prod.fg-mat NE ""  AND
                   prod.wip-mat NE "" AND
                   fg-rdtlh.cost NE ? THEN DO:

                  /* Debit FG Material */
                  FIND FIRST work-gl WHERE work-gl.actnum EQ prod.fg-mat
                      NO-LOCK NO-ERROR.

                  IF NOT AVAIL work-gl THEN DO:
                    CREATE work-gl.
                    work-gl.actnum = prod.fg-mat.
                  END.

                  work-gl.debits = work-gl.debits -
                                   (fg-rdtlh.qty / 1000 * fg-rdtlh.cost ).

                  /* Credit WIP Material */
                  FIND FIRST work-gl WHERE work-gl.actnum EQ prod.wip-mat
                      NO-LOCK NO-ERROR.

                  IF NOT AVAIL work-gl THEN DO:
                    CREATE work-gl.
                    work-gl.actnum = prod.wip-mat.
                  END.

                  work-gl.credits = work-gl.credits -
                                    (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).
                END.
              END.  
              /* Balance GL */

              ASSIGN
               v-qty         = v-qty + fg-rdtlh.qty
               fg-rdtlh.cost = v-cost
               fg-rcpth.b-no = ap-invl.i-no.

              IF LAST(fg-rcpth.trans-date) AND
                 v-qty NE v-qty1           THEN DO:

                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.i-no    EQ fg-rcpth.i-no
                      AND fg-bin.loc     EQ fg-rdtlh.loc
                      AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                      AND fg-bin.tag     EQ fg-rdtlh.tag
                      AND fg-bin.job-no  EQ fg-rcpth.job-no
                      AND fg-bin.job-no2 EQ fg-rcpth.job-no2
                    NO-ERROR.  

                IF NOT AVAIL fg-bin THEN DO:
                  CREATE fg-bin.
                  ASSIGN
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
                END.

                ASSIGN
                 v-qty1         = v-qty1 - v-qty
                 fg-rdtlh.qty   = fg-rdtlh.qty + v-qty1
                 fg-rdtlh.cases = trunc(fg-rdtlh.qty / fg-rdtlh.qty-case,0)
                 fg-bin.qty     = fg-bin.qty + v-qty1
                 itemfg.q-onh   = itemfg.q-onh + v-qty1.
                 RUN fg/chkfgloc.p (INPUT fg-rcpth.i-no, INPUT fg-rdtlh.loc).
                 FIND FIRST itemfg-loc 
                    WHERE itemfg-loc.company EQ fg-rdtlh.company
                      AND itemfg-loc.i-no    EQ fg-rcpth.i-no
                      AND itemfg-loc.loc     EQ fg-rdtlh.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                 IF AVAIL itemfg-loc THEN
                   itemfg-loc.q-onh = itemfg-loc.q-onh + v-qty1.
                 FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
              END.
            END.

            FOR EACH fg-rcpth
                WHERE fg-rcpth.company   EQ cocode
                  AND fg-rcpth.i-no      EQ po-ordl.i-no
                  AND fg-rcpth.po-no     EQ v-po-no
                  AND fg-rcpth.rita-code EQ "R"
                USE-INDEX item-po NO-LOCK,

                EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no

                BREAK BY fg-rcpth.job-no
                      BY fg-rcpth.job-no2
                      BY fg-rdtlh.loc
                      BY fg-rdtlh.loc-bin
                      BY fg-rdtlh.tag:

              IF FIRST-OF(fg-rdtlh.tag) THEN
                ASSIGN
                 v-qty  = 0
                 v-cost = 0.

              ASSIGN
               v-qty  = v-qty + fg-rdtlh.qty
               v-cost = v-cost + (fg-rdtlh.qty / 1000 * fg-rdtlh.cost).

              IF LAST-OF(fg-rdtlh.tag) THEN DO:
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.i-no    EQ fg-rcpth.i-no
                      AND fg-bin.loc     EQ fg-rdtlh.loc
                      AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                      AND fg-bin.tag     EQ fg-rdtlh.tag
                      AND fg-bin.job-no  EQ fg-rcpth.job-no
                      AND fg-bin.job-no2 EQ fg-rcpth.job-no2
                    NO-ERROR.  

                IF NOT AVAIL fg-bin THEN DO:
                  CREATE fg-bin.
                  ASSIGN
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
                END.

                v-cost = v-cost / (v-qty / 1000).

                IF fg-bin.pur-uom EQ "M" THEN
                  fg-bin.std-tot-cost = v-cost.
                ELSE
                  RUN sys/ref/convcuom.p ("M", fg-bin.pur-uom, 0, 0, 0, 0,
                                          v-cost, OUTPUT fg-bin.std-tot-cost).

                ASSIGN
                 fg-bin.std-mat-cost = fg-bin.std-tot-cost
                 fg-bin.std-lab-cost = 0
                 fg-bin.std-var-cost = 0
                 fg-bin.std-fix-cost = 0.
              END.
            END.
          END.

          RUN fg/updfgcst.p (po-ordl.i-no, NO).
        END.
      END.
    END.  /* each line */

    FIND FIRST vend
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ ap-inv.vend-no
        USE-INDEX vend EXCLUSIVE-LOCK.

    ASSIGN
     vend.purch[tran-period]   = vend.purch[tran-period] + t1
     vend.n-purch[tran-period] = vend.n-purch[tran-period] + 1
     vend.purch[13]        = vend.purch[13] + t1
     vend.n-purch[13]      = vend.n-purch[13] + 1
     vend.ptd-msf[tran-period] = vend.ptd-msf[tran-period] + total-msf
     vend.ytd-msf          = vend.ytd-msf + total-msf
     vend.acc-bal          = vend.acc-bal + t1 + ar-inv.tax-amt.

    IF vend.acc-bal GE vend.hibal THEN
      ASSIGN
       vend.hibal      = vend.acc-bal
       vend.hibal-date = ap-inv.inv-date.

    CREATE ap-ledger.
    ASSIGN
     ap-ledger.company  = cocode
     ap-ledger.vend-no  = ap-inv.vend-no
     ap-ledger.amt      = ap-inv.net
     ap-ledger.refnum   = "INV# " + ap-inv.inv-no
     ap-ledger.ref-date = ap-inv.inv-date
     ap-ledger.trnum    = v-trnum
     ap-ledger.period   = tran-period
     ap-ledger.tr-date  = tran-date.

    ASSIGN
     t1            = 0
     ap-inv.posted = YES.
  END. /* for each ap-inv */

  IF lv-frt-total NE 0 THEN DO:
     RUN spCreateGLHist(cocode,
                        v-frt-acct,
                        "ACPAY",
                        "ACCOUNTS PAYABLE FREIGHT",
                        tran-date,
                        lv-frt-total,
                        v-trnum,
                        tran-period,
                        "A",
                        tran-date,
                        string(ap-inv.inv-no),
                        "AP").
    
    ASSIGN
      g2 = g2 + lv-frt-total.
  END.

     RUN spCreateGLHist(cocode,
                        xap-acct,
                        "ACPAY",
                        "ACCOUNTS PAYABLE INVOICE",
                        tran-date,
                        - g2,
                        v-trnum,
                        tran-period,
                        "A",
                        tran-date,
                        string(ap-inv.inv-no),
                        "AP").
  
  FOR EACH work-gl BREAK BY work-gl.actnum:
    ASSIGN
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    IF LAST-OF(work-gl.actnum) THEN DO:
     RUN spCreateGLHist(cocode,
                        work-gl.actnum,
                        "ACPAY",
                        "AP for FG Receipts from PO",
                        tran-date,
                        (debits - credits),
                        v-trnum,
                        tran-period,
                        "A",
                        tran-date,
                        string(ap-inv.inv-no),
                        "AP").
      
      ASSIGN
       debits  = 0
       credits = 0.
    END.
  END.
END. /* postit: transaction */

  FOR EACH report WHERE report.term-id EQ v-term:
    DELETE report.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-inreg.p 10/94 gb */
/* Invoicing  - Edit Register & Post Invoicing Transactions                   */
/* -------------------------------------------------------------------------- */
DEF VAR g1 AS DEC FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF VAR g2 LIKE g1 NO-UNDO.
DEF VAR t1 LIKE g1 NO-UNDO.
DEF VAR t2 LIKE g1 NO-UNDO.
DEF VAR t3 LIKE g1 NO-UNDO.
DEF VAR v1 LIKE g1 NO-UNDO.
DEF VAR v2 LIKE g1 NO-UNDO.

DEF VAR total-msf LIKE ap-invl.amt-msf NO-UNDO.
DEF VAR v-s-date LIKE inv-head.inv-date FORMAT "99/99/9999" INIT 01/01/0001 NO-UNDO.
DEF VAR v-e-date LIKE v-s-date INIT TODAY NO-UNDO.
DEF VAR v-prt-dscr AS LOG INIT NO NO-UNDO.
DEF VAR v-s-vend LIKE vend.vend-no INITIAL "First" NO-UNDO.
DEF VAR v-e-vend LIKE vend.vend-no INITIAL "Last" NO-UNDO.
DEF BUFFER xap-inv FOR ap-inv.
DEF VAR v-loop AS INT INIT 1 NO-UNDO.

{sys/form/r-top3w.f}
time_stamp = STRING(TIME,"hh:mmam").

FORM HEADER
     "VENDOR#  Name                              INVOICE #       INV.DATE    DUE DATE         AMOUNT " 
     "    G/L DISTRIBUTION" SKIP FILL("_",130) FORMAT "x(130)"
    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top PAGE-TOP WIDTH 132 STREAM-IO.

tmpstore = FILL("_",125).

ASSIGN v-s-vend = begin_vend
       v-e-vend = END_vend
       v-s-date = begin_date
       v-e-date = END_date
       v-prt-dscr = tb_sort
       .


 {sys/inc/print1.i}

  {sys/inc/outprint.i VALUE(lines-per-page)}

  IF td-show-parm THEN RUN show-param.

ASSIGN
 g1 = 0
 g2 = 0
 t1 = 0
 t2 = 0
 t3 = 0
 v1 = 0
 v2 = 0
 total-msf = 0.

ASSIGN
 str-tit  = coname + " - " + loname
 str-tit2 = "VENDOR INVOICES  -  EDIT REGISTER " + string(v-trnum)
 str-tit3 = "Period " + string(tran-period,"99") +
            " - transaction Date Entered: " + string(tran-date)
 x = (112 - length(str-tit)) / 2
 str-tit  = FILL(" ",x) + str-tit
 x = (114 - length(str-tit2)) / 2
 str-tit2 = FILL(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = FILL(" ",x) + str-tit3.

DISPLAY "" WITH FRAME r-top.
DISPLAY "" WITH FRAME f-top.

FOR EACH report WHERE report.term-id EQ v-term:
  DELETE report.
END.

FOR EACH xap-inv
    WHERE xap-inv.company  EQ cocode
      AND xap-inv.posted   EQ NO
      AND xap-inv.inv-date GE v-s-date
      AND xap-inv.inv-date LE v-e-date 
      AND xap-inv.vend-no  GE v-s-vend
      AND xap-inv.vend-no  LE v-e-vend
    USE-INDEX posted NO-LOCK
    TRANSACTION:

  FIND FIRST ap-inv WHERE RECID(ap-inv) EQ recid(xap-inv)
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

  IF AVAIL ap-inv THEN DO:
    CREATE report.
    ASSIGN
     report.term-id = v-term
     report.rec-id  = RECID(ap-inv).
  END.
END.

FOR EACH report WHERE report.term-id EQ v-term NO-LOCK,
    FIRST ap-inv WHERE RECID(ap-inv) EQ report.rec-id NO-LOCK
    BREAK BY ap-inv.vend-no
          BY ap-inv.inv-no          
    WITH FRAME a1 STREAM-IO:
  v-postable = YES.  
  IF FIRST-OF(ap-inv.vend-no) THEN DO:
    FIND FIRST vend
        WHERE vend.company EQ cocode
          AND vend.vend-no EQ ap-inv.vend-no
        USE-INDEX vend NO-LOCK NO-ERROR.

    PUT vend.vend-no SPACE(1)
        vend.name.
  END.

  ELSE
  IF FIRST-OF(ap-inv.inv-no) THEN PUT SKIP(1).

  PUT ap-inv.inv-no   TO 55
      ap-inv.inv-date AT 60
      ap-inv.due-date AT 71                        
      ap-inv.net + ap-inv.freight FORMAT "->,>>>,>>9.99" TO 94.

  ASSIGN
   v2 = v2 + ap-inv.net
   v1 = v1 + ap-inv.disc-taken.

  FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK USE-INDEX i-no
      WITH FRAME a2 NO-BOX NO-LABELS WIDTH 132:

    PUT ap-invl.actnum AT 96 FORMAT "x(19)" SPACE(1)            
        ap-invl.amt SKIP.

    IF v-prt-dscr THEN DO:
      FIND FIRST account
          WHERE account.company EQ cocode
            AND account.actnum  EQ ap-invl.actnum
          NO-LOCK NO-ERROR.
      IF AVAIL account THEN PUT account.dscr AT 90 FORMAT "x(40)" SKIP.
    END.   
  END. /* each ap-invl */

  IF LAST-OF(ap-inv.vend-no) THEN DO:
    PUT v-frt-acct AT 96 FORMAT "x(19)" SPACE(1)            
        ap-inv.freight TO 127 SKIP.

    IF v-prt-dscr THEN DO:
      FIND FIRST account
          WHERE account.company EQ cocode
            AND account.actnum  EQ v-frt-acct
          NO-LOCK NO-ERROR.
      IF AVAIL account THEN PUT account.dscr AT 90 FORMAT "x(40)" SKIP.
    END.   
    v2 = v2 + ap-inv.freight.

    DISPLAY  "*  VENDOR TOTALS" AT 90 v2 TO 127 "*" SKIP(1)
        WITH FRAME vtot NO-BOX NO-LABELS WIDTH 132 STREAM-IO.

    ASSIGN    
     g1 = g1 + v1
     g2 = g2 + v2
     v1 = 0
     v2 = 0.
  END.
END. /* each invoice */

DISPLAY  "** GRAND TOTAL  "  AT 90  g2 TO 127 "**"
    WITH NO-LABELS NO-UNDERLINE WIDTH 132 FRAME GT STREAM-IO.

HIDE FRAME f-top.

str-tit3 = "Period " + string(tran-period,"99") + " - " + "Summary by Account".
x = (132 - length(str-tit3)) / 2.
str-tit3 = FILL(" ",x) + str-tit3.

PAGE.

FORM HEADER
     "ACCOUNT                             PO#   DATE   VENDOR#  INVOICE#    "
     "LINE DESCRIPTION              QTY    UNIT PRICE       AMOUNT" SKIP
     FILL("_",132) FORMAT "x(132)"

    WITH NO-LABELS NO-BOX NO-UNDERLINE FRAME f-top2 PAGE-TOP WIDTH 132 STREAM-IO .

DISPLAY "" WITH FRAME f-top2.

v-loop = 1.

FOR EACH report WHERE report.term-id EQ v-term NO-LOCK,

    FIRST ap-inv WHERE RECID(ap-inv) EQ report.rec-id NO-LOCK,

    FIRST vend
    WHERE vend.company EQ cocode
      AND vend.vend-no EQ ap-inv.vend-no
    USE-INDEX vend NO-LOCK

    BREAK BY ap-inv.vend-no:

  IF ap-inv.freight NE 0 THEN DO:
    IF v-loop EQ 1 THEN DO:
      v-loop = 2.
      FIND FIRST account
          WHERE account.company EQ cocode
            AND account.actnum  EQ v-frt-acct
          NO-LOCK NO-ERROR.
      PUT v-frt-acct + " - " + account.dscr FORMAT "x(40)".
    END.

    PUT ap-inv.inv-date AT 41       SPACE(1)
        ap-inv.vend-no              SPACE(1)
        ap-inv.inv-no               SPACE(6)
        "Freight"    FORMAT "x(18)" SPACE(7)
        1.0          FORMAT "9.9"   SPACE(1)
        ap-inv.freight              TO 118
        ap-inv.freight              TO 131
        SKIP.
  END.

  ACCUMULATE ap-inv.freight (TOTAL).

  IF LAST(ap-inv.vend-no) THEN
    PUT "** TOTAL " TO 114
        ACCUM TOTAL ap-inv.freight FORMAT "->>,>>>,>>9.99" TO 128
        " *" SKIP(1).
END.

FOR EACH report WHERE report.term-id EQ v-term,

    FIRST ap-inv WHERE RECID(ap-inv) EQ report.rec-id NO-LOCK,

    EACH ap-invl
    WHERE ap-invl.i-no    EQ ap-inv.i-no
      AND ap-invl.posted  EQ NO
    USE-INDEX i-no NO-LOCK

    BREAK BY ap-invl.actnum
          BY ap-invl.inv-no
          BY ap-invl.line

    WITH WIDTH 132 NO-LABELS:

  FIND FIRST vend
      WHERE vend.company EQ cocode
        AND vend.vend-no EQ ap-inv.vend-no
      USE-INDEX vend NO-LOCK NO-ERROR.

  IF FIRST-OF(ap-invl.actnum) THEN DO:
    FIND FIRST account
        WHERE account.company EQ cocode
          AND account.actnum  EQ ap-invl.actnum
        NO-LOCK NO-ERROR.

    PUT ap-invl.actnum + " - " + account.dscr FORMAT "x(40)" SKIP.
  END.

  PUT ap-invl.po-no         AT 34
      SPACE(1)
      ap-inv.inv-date
      SPACE(1)
      ap-inv.vend-no
      SPACE(1)
      ap-inv.inv-no
      SPACE(1)
      {ap/invlline.i -1}    FORMAT ">>>9"
      SPACE(1)
      ap-invl.dscr          FORMAT "x(18)"
      SPACE(1)
      ap-invl.qty           FORMAT "->>,>>9.9<<"
      SPACE(1)
      ap-invl.unit-pr
      SPACE(1)
      ap-invl.amt
      SPACE(1)
      SKIP.

  ACCUMULATE ap-invl.amt (TOTAL BY ap-invl.actnum).
  ACCUMULATE ap-invl.amt (TOTAL).

  IF LAST-OF(ap-invl.actnum) THEN
    PUT "** TOTAL " TO 114
        ACCUM TOTAL BY ap-invl.actnum ap-invl.amt FORMAT "->>,>>>,>>9.99" TO 128
        " *" SKIP(1).
END.

PUT "***** TOTAL for ALL ACCOUNTS " TO 116
    ((ACCUM TOTAL ap-invl.amt) + (ACCUM TOTAL ap-inv.freight)) FORMAT "->>,>>>,~>>9.99" TO 130 SKIP(2).

lv-frt-total = (ACCUM TOTAL ap-inv.freight).

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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".
                  END.
                  lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                 
              END.       
           END.                 
        END.            
     lv-field-hdl = lv-field-hdl:NEXT-SIBLING.   
  END.

  PUT SPACE(28)
      "< Selection Parameters >"
      SKIP(1).

  DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):
    IF ENTRY(i,parm-fld-list) NE "" OR
       entry(i,parm-lbl-list) NE "" THEN DO:

      lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +
                 trim(ENTRY(i,parm-lbl-list)) + ":".

      PUT lv-label FORMAT "x(35)" AT 5
          SPACE(1)
          TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"
          SKIP.              
    END.
  END.

  PUT FILL("-",80) FORMAT "x(80)" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undo-trnum C-Win 
PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /** GET next G/L TRANS. POSTING # **/
/* gdm - 11050906 */
REPEAT:
 FIND FIRST gl-ctrl EXCLUSIVE-LOCK
   WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
 IF AVAIL gl-ctrl THEN DO:
   ASSIGN v-trnum       = gl-ctrl.trnum + 1
          gl-ctrl.trnum = v-trnum.
   LEAVE.
 END. /* IF AVAIL gl-ctrl */
END. /* REPEAT */
/* gdm - 11050906 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

