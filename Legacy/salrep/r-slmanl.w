&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-slmanl.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def buffer xoe-ord for oe-ord.

{oe/rep/backlog1.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS inv-date begin_slmn end_slmn tb_fin-chg ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel ~
RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS inv-date begin_slmn end_slmn tb_fin-chg ~
rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_slmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE end_slmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE inv-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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
     SIZE 92 BY 7.38.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.1.

DEFINE VARIABLE tb_fin-chg AS LOGICAL INITIAL no 
     LABEL "Include Finance Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     inv-date AT ROW 3.14 COL 32 COLON-ALIGNED HELP
          "Enter Invoice Date"
     begin_slmn AT ROW 4.81 COL 32 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slmn AT ROW 4.81 COL 67 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tb_fin-chg AT ROW 6.71 COL 34
     rd-dest AT ROW 11 COL 6 NO-LABEL
     lv-ornt AT ROW 11.24 COL 31 NO-LABEL
     lines-per-page AT ROW 11.24 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.14 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 14.1 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 15.05 COL 6
     btn-ok AT ROW 19.57 COL 19
     btn-cancel AT ROW 19.57 COL 57
     RECT-6 AT ROW 9.33 COL 2
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.05 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.57.


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
         TITLE              = "Sales Analysis - Sales Journal"
         HEIGHT             = 21.81
         WIDTH              = 95.8
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
       begin_slmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       inv-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sales Analysis - Sales Journal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales Journal */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slmn C-Win
ON LEAVE OF begin_slmn IN FRAME FRAME-A /* Beginning SalesRep# */
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
  assign rd-dest.
       
  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slmn C-Win
ON LEAVE OF end_slmn IN FRAME FRAME-A /* Ending SalesRep# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-date C-Win
ON LEAVE OF inv-date IN FRAME FRAME-A /* Invoice Date */
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
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
     RETURN .
  END.
   
  assign
   inv-date = date(1,1,year(today)).
   

  RUN enable_UI.
  
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
  DISPLAY inv-date begin_slmn end_slmn tb_fin-chg rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE inv-date begin_slmn end_slmn tb_fin-chg rd-dest lv-ornt lines-per-page 
         lv-font-no td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
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
  run scr-rpt.w (list-name,c-win:title). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/backlog.p 10/94 gb */
/* Order Backlog Summary / Detail Report                                      */
/* -------------------------------------------------------------------------- */
 /*
{sys/form/r-topw.f}

def var v-fcust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-fslsm like oe-ord.sman extent 2 init ["","zzz"].
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-fdate as date format "99/99/9999" extent 2 init [01/01/01,today].
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ponum as log init yes.
def var v-sort as char format "!" init "C".
def var v-sumdet as log format "Summary/Detail" init yes.
def var v-sub-item as log.
def var v-price as log format "Price/Status" init no.
def var v-jobs as log init yes.
def var v-all as log init no.
def var v-fg-qty as log init yes format "FG/Job".

def var v-profit as log init yes.
def var v-password as char no-undo.
def var security-flag as log no-undo.

def var v-qty as int extent 2.
def var v-cost as dec.
def var v-tot-qty as int format "->>>,>>>,>>9" extent 2.
def var v-tot-cost as dec format  "->>>,>>>,>>9.99" extent 2.
def var v-tot-sales as dec format "->>>,>>>,>>9.99" extent 2.
def var v-tot-pct as dec format "->>,>>9.99".
def var v-head as char format "x(132)" extent 3.
def var v-gpdollar as dec format  "->>>,>>>,>>9.99".
def var v-gp as dec.
def var v-uom as char format "x(4)".
def var v-qty-pal as int.
def var fstat like oe-ord.stat.
def var tstat like fstat init "".

{sa/sa-sls01.i}

format header
  v-head[1] skip
  v-head[2]
  fill("_",132) format "x(132)"
  with no-labels no-box no-underline stream-io width 132 frame f-top page-top.

format header
  fill("_",127) format "x(127)"
  with no-labels no-box no-underline stream-io width 128 frame f-topd page-top.

format
  w-ord.due-date
  w-ord.ord-date
  w-ord.ord-no
  w-ord.po-num
  w-ord.i-name  format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  w-ord.price
  space(0)
  v-uom
  w-ord.t-price skip
  with frame ordhead-po no-labels no-box no-underline stream-io width 150.

format
  w-ord.due-date
  w-ord.ord-date
  w-ord.ord-no
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  w-ord.price
  space(0)
  v-uom
  w-ord.t-price skip
  with frame ordhead no-labels no-box no-underline stream-io width 132.
  
format
  w-ord.due-date
  w-ord.ord-date
  w-ord.ord-no
  w-ord.po-num
  w-ord.i-name       format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  space(3)
  w-ord.stat  skip
  with frame ordhead-po-s no-labels no-box no-underline stream-io width 150.

format
  skip(1)
  w-ord.ord-no label "Order No." colon 10 space(1)
  w-ord.due-date label "Date" space(1)
  w-ord.cust-no label "Customer" "-"
  w-ord.cust-name no-labels format "x(25)" space(1)
  w-ord.sman label "Salesman" space(1)
  w-ord.po-num label "PO #" skip(1)
  with frame detailhead-po side-labels no-box no-underline stream-io width 127.

format
  w-ord.i-name label "Description" format "x(25)"
  w-ord.est-no label "Estimate#"   format "x(8)"
  w-ord.qty-due label "Qty Due"
  w-ord.stat label "Status"
  w-ord.cost label "Std. Cost"
  w-ord.t-price label "Sales"
  v-gpdollar label "GP Dollars"
  v-gp label "GP%"
  with frame ordline no-box no-underline down stream-io width 125.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BACKLOG"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BACKLOG"
   sys-ctrl.descrip = "Print Cost & Profit on Order Backlog Report?".
  message sys-ctrl.descrip update sys-ctrl.log-fld.
end.
v-profit = sys-ctrl.log-fld.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-fcust[1]   = begin_cust-no
 v-fcust[2]   = end_cust-no
 /*v-fslsm[1]   = begin_slsmn
 v-fslsm[2]   = end_slsmn
 v-ford-no[1] = begin_ord-no
 v-ford-no[2] = end_ord-no
 v-fitem[1]   = begin_i-no
 v-fitem[2]   = end_i-no*/
 v-fdate[1]   = begin_inv-date
 v-fdate[2]   = end_inv-date
 v-ponum      = tb_po-no
 v-sort       = if rd_sort eq "Customer#" then "C" else
                if rd_sort eq "Due Date"  then "D" else "S"
 v-sumdet     = not tb_detailed
 v-sub-item   = tb_subt
 v-price      = rd_show begins "Pri"
 v-jobs       = tb_jobs
 v-all        = tb_qohgt0
 v-fg-qty     = rd_qoh eq "FG".

if v-price then do:
  find first sys-ctrl
     where sys-ctrl.company eq cocode
       and sys-ctrl.name    eq "SECURITY"
  no-lock no-error.

  if not available sys-ctrl then
  do transaction:
    create sys-ctrl.
    assign
      sys-ctrl.company = cocode
      sys-ctrl.name = "SECURITY"
      sys-ctrl.descrip = "Password?  Prompt for Password on Show Prices/Sales"
      sys-ctrl.log-fld = yes
      sys-ctrl.char-fld = "yorkie".
    security-flag = no.
  end.
    
  if sys-ctrl.log-fld then do:
    update v-password label "Password" blank
        with centered side-labels overlay frame password-frame
             title "Pricing Security" row 10.
    if v-password <> sys-ctrl.char-fld then do:
      message "* Password Incorrect - Report pricing disabled. *".
      bell.
      pause.
      security-flag = no.
    end.
      
    else do:
      hide message no-pause.
      security-flag = yes.
    end.
    hide frame password-frame.
  end.
    
  else security-flag = v-price.
end.
  
v-head[3] = if v-price then "     Price          Sales $" else "  Status".

if v-sumdet and v-ponum then
  assign v-head[1] =
    "Due      Order    Order     PO                                      Item  "
    + "                 Qty          Qty                           "
           v-head[2] =
    "Date     Date     Number    Number          Description             Number"
    + "             On-hand          Due" + v-head[3].

else
if v-sumdet and not v-ponum then
  assign v-head[1] =
    "Due      Order    Order                                  Item   "
    + "                  Qty          Qty                         "
           v-head[2] =
    "Date     Date     Number       Description               Number "
    + "              On-hand          Due" + v-head[3].

else
  assign 
   v-head[1] = ""
   v-head[2] = "".

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.

if v-sumdet then display "" with frame f-top.

else display "" with frame f-topd.

    do i = 1 to 3:
      assign
       fstat = tstat
       tstat = if i eq 1 then "C" else
               if i eq 2 then "D" else "Z".

      for each xoe-ord
          where xoe-ord.company  eq cocode
            and xoe-ord.stat     gt fstat
            and xoe-ord.stat     lt tstat
            and xoe-ord.ord-no   ge v-ford-no[1]
            and xoe-ord.ord-no   le v-ford-no[2]
            and xoe-ord.cust-no  ge v-fcust[1]
            and xoe-ord.cust-no  le v-fcust[2]
            and xoe-ord.sman[1]  ge v-fslsm[1]
            and xoe-ord.sman[1]  le v-fslsm[2]
          use-index stat no-lock,

          each oe-ordl
          where oe-ordl.company    eq cocode
            and oe-ordl.ord-no     eq xoe-ord.ord-no
            and oe-ordl.i-no       ge v-fitem[1]
            and oe-ordl.i-no       le v-fitem[2]
            and oe-ordl.prom-date  ge v-fdate[1]
            and oe-ordl.prom-date  le v-fdate[2]
            and (if oe-ctrl.u-inv then
                   oe-ordl.ship-qty lt oe-ordl.qty
                 else
                   oe-ordl.t-ship-qty lt oe-ordl.qty)
          no-lock:

        create report.
        assign
         report.term-id = v-term
         report.key-01  = string(if v-sort eq "D" then
                                   string(year(oe-ordl.prom-date),"9999") +
                                   string(month(oe-ordl.prom-date),"99")  +
                                   string(day(oe-ordl.prom-date),"99")
                                 else "xxxxxxxx") +
                          string(if v-sort eq "S" then
                                   xoe-ord.sman[1]
                                 else "","xxx") +
                          xoe-ord.cust-no
         report.key-02  = if v-sumdet then oe-ordl.i-no
                          else string(xoe-ord.ord-no,"9999999999")
         report.key-03  = oe-ordl.i-no
         report.rec-id  = recid(oe-ordl).
      end.
    end.
    
    if v-jobs then
    for each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.i-no    ge v-fitem[1]
          and job-hdr.i-no    le v-fitem[2]
          and job-hdr.cust-no ge v-fcust[1]
          and job-hdr.cust-no le v-fcust[2]
          and job-hdr.ord-no  eq 0
        no-lock,
        
        first job
        where job.company    eq cocode
          and job.job        eq job-hdr.job
          and job.job-no     eq job-hdr.job-no
          and job.job-no2    eq job-hdr.job-no2
          and job.start-date ge v-fdate[1]
          and job.start-date le v-fdate[2]
          and job.opened
        no-lock,  
        
        first cust
        where cust.company eq cocode
          and cust.cust-no eq job-hdr.cust-no
          and cust.sman    ge v-fslsm[1]
          and cust.sman    le v-fslsm[2]
        no-lock,
        
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq job-hdr.i-no
        no-lock  
        
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              by job-hdr.i-no:

      v-qty[1] = v-qty[1] + job-hdr.qty.
      
      if last-of(job-hdr.i-no) then do:
        for each fg-act
            where fg-act.company eq cocode
              and fg-act.job     eq job-hdr.job
              and fg-act.job-no  eq job-hdr.job-no
              and fg-act.job-no2 eq job-hdr.job-no2
              and fg-act.i-no    eq job-hdr.i-no
            use-index job-idx no-lock:
            
          v-qty[2] = v-qty[2] + fg-act.qty.  
        end.
        
        if v-qty[2] lt v-qty[1] then do:
          create report.
          assign
           report.term-id = v-term
           report.key-01  = string(if v-sort eq "D" then
                                     string(year(job.start-date),"9999") +
                                     string(month(job.start-date),"99")  +
                                     string(day(job.start-date),"99")
                                   else "xxxxxxxx") +
                            string(if v-sort eq "S" then cust.sman
                                   else "","xxx") +
                            cust.cust-no
           report.key-02  = if v-sumdet then job-hdr.i-no
                            else (trim(job.job-no) + "-" +
                                  string(job.job-no2,"99"))
           report.key-03  = job-hdr.i-no
           report.key-04  = string(v-qty[1] - v-qty[2],"9999999999")
           report.key-05  = job-hdr.cust-no
           report.rec-id  = recid(job).
        end.
        
        v-qty = 0.
      end.      
    end.
    
    if v-all and today ge v-fdate[1] and today le v-fdate[2] then
    for each itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    ge v-fitem[1]
          and itemfg.i-no    le v-fitem[2]
          and itemfg.cust-no ge v-fcust[1]
          and itemfg.cust-no le v-fcust[2]
          and (itemfg.q-onh  gt 0 or itemfg.q-ono gt 0)
          and not can-find(first report where report.term-id eq v-term
                                          and report.key-03  eq itemfg.i-no)
        no-lock,
        
        first cust
        where cust.company eq cocode
          and cust.cust-no eq itemfg.cust-no
          and cust.sman    ge v-fslsm[1]
          and cust.sman    le v-fslsm[2]
        no-lock:
        
      create report.
      assign
       report.term-id = v-term
       report.key-01  = string(if v-sort eq "D" then
                                 string(year(today),"9999") +
                                 string(month(today),"99")  +
                                 string(day(today),"99")
                               else "xxxxxxxx") +
                        string(if v-sort eq "S" then cust.sman
                               else "","xxx") +
                        cust.cust-no
       report.key-02  = if v-sumdet then itemfg.i-no else ""
       report.rec-id  = recid(itemfg).
    end.

    for each report where report.term-id eq v-term
        break by report.key-01 by report.key-02:

      {oe/rep/backlog.i}

      v-uom = "/" + if w-ord.uom ne "" then caps(w-ord.uom) else "EA".

      if v-sumdet then do:
        if first-of(report.key-01) then do:
          if v-sort eq "S" then put "Salesman: " w-ord.sman space(4).
          put "Customer: " w-ord.cust-no " - " w-ord.cust-name skip.
        end.

        if v-ponum then do:
          if v-price then do:
            display w-ord.due-date
                    w-ord.ord-date
                    w-ord.ord-no
                    w-ord.po-num
                    w-ord.i-name
                    w-ord.i-no
                    w-ord.qty-onh
                    w-ord.qty-due
                    w-ord.price    when security-flag
                    v-uom          when security-flag
                    w-ord.t-price  when security-flag
                with frame ordhead-po.
            down with frame ordhead-po.
          end.
          
          else do:
            display w-ord.due-date
                    w-ord.ord-date
                    w-ord.ord-no
                    w-ord.po-num
                    w-ord.i-name
                    w-ord.i-no
                    w-ord.qty-onh
                    w-ord.qty-due
                    w-ord.stat
                with frame ordhead-po-s.
            down with frame ordhead-po-s.
          end.
        end.

        else do:
          if v-price then do:
            display w-ord.due-date
                    w-ord.ord-date
                    w-ord.ord-no
                    w-ord.i-name
                    w-ord.i-no
                    w-ord.qty-onh
                    w-ord.qty-due
                    w-ord.price   when security-flag
                    v-uom
                    w-ord.t-price when security-flag
                with frame ordhead.
            down with frame ordhead.
          end.
          
          else do:
            display w-ord.due-date
                    w-ord.ord-date
                    w-ord.ord-no
                    w-ord.i-name
                    w-ord.i-no
                    w-ord.qty-onh
                    w-ord.qty-due
                    w-ord.stat
                with frame ordhead-s.
            down with frame ordhead-s.
          end.
        end.
      end.

      else do:
        if first-of(report.key-02) then do:
          if v-ponum then
            display skip(1)
                    w-ord.ord-no
                    w-ord.due-date
                    w-ord.cust-no
                    w-ord.cust-name
                    w-ord.sman
                    w-ord.po-num
                with frame detailhead-po.

          else
            display skip(1)
                    w-ord.ord-no
                    w-ord.due-date
                    w-ord.cust-no
                    w-ord.cust-name
                    w-ord.sman
                with frame detailhead.
        end.

        assign
         v-gpdollar = (w-ord.t-price - w-ord.cost)
         v-gp       = round(v-gpdollar / w-ord.t-price * 100,2).

        display w-ord.i-name
                w-ord.est-no
                w-ord.qty-due
                w-ord.stat
                w-ord.cost when v-profit and security-flag
                w-ord.t-price when security-flag
                v-gpdollar when v-profit and security-flag
                v-gp       when v-profit and security-flag
            with frame ordline.
        down with frame ordline.
      end.

      assign
       v-tot-qty[1]   = v-tot-qty[1] + w-ord.qty
       v-tot-cost[1]  = v-tot-cost[1] + w-ord.cost
       v-tot-sales[1] = v-tot-sales[1] + w-ord.t-price.

      if last-of(report.key-02) then do:
        if v-sumdet and v-sub-item and v-sort ne "D" then do:
          if line-counter + 4 gt page-size then page.

          if v-tot-cost[1] eq ? then v-tot-cost[1] = 0.
          v-tot-pct = if v-tot-sales[1] ne 0 then
                        (v-tot-sales[1] - v-tot-cost[1]) /
                        v-tot-sales[1] * 100 else 0.

          put skip(1)
              "Order Qty"    to 42
              "Cost"         to 58
              "Sales"        to 74
              "GP%"          to 84 skip
              "ITEM TOTALS:" to 21
              v-tot-qty[1]   to 42.

          if v-profit then put v-tot-cost[1] to 58.

          put v-tot-sales[1] to 74.

          if v-profit then put v-tot-pct to 84.

          put skip(1).
        end.

        assign
         v-tot-qty[2]   = v-tot-qty[2] + v-tot-qty[1]
         v-tot-cost[2]  = v-tot-cost[2] + v-tot-cost[1]
         v-tot-sales[2] = v-tot-sales[2] + v-tot-sales[1]
         v-tot-qty[1]   = 0
         v-tot-cost[1]  = 0
         v-tot-sales[1] = 0.
      end.

      if last-of(report.key-01) and
         (v-sumdet or last(report.key-01)) then put skip(1).

      if last(report.key-01) then do:
        if line-counter + 4 gt page-size then page.

        if v-tot-cost[2] eq ? then v-tot-cost[2] = 0.
        v-tot-pct = if v-tot-sales[2] ne 0 then
                      (v-tot-sales[2] - v-tot-cost[2]) /
                       v-tot-sales[2] * 100 else 0.

        put skip(1)
            "Order Qty"     to 42
            "Cost"          to 58
            "Sales"         to 74
            "GP%"           to 84 skip
            "GRAND TOTALS:" to 21
            v-tot-qty[2]    to 42.

        if v-profit then put v-tot-cost[2] to 58.

        put v-tot-sales[2] to 74.

        if v-profit then put v-tot-pct to 84.

        put skip(1).
      end.

      delete w-ord.
      delete report.
    end. /* for each */

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
*/
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

