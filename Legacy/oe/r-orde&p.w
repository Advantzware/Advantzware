&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-orde&p.w

  Description: Order Edit List & Posting

  Input Parameters: ip-post

  Output Parameters:
      <none>

  Author: JLF

  Created: 03/05/2002

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
DEF VAR ip-post AS LOG NO-UNDO.
&ELSE
DEF INPUT PARAMETER ip-post AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR exl-due-date AS CHAR NO-UNDO.
DEF VAR itemname AS CHAR NO-UNDO.
DEF VAR ino AS CHAR NO-UNDO.
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

def new shared buffer xoe-ord for oe-ord.
def new shared buffer xest    for est.
def new shared buffer xef     for ef.
def new shared buffer xeb     for eb.

def new shared variable v-misc as log init no no-undo.
def new shared variable v-fr-tax like oe-ctrl.f-tax.

def var v-detail as log init no.
def var v-postable as log init no.
def var save_id as recid.
def var time_stamp as ch.

def var v-invalid as log init no no-undo.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

define TEMP-TABLE w-ord-line NO-UNDO
  field ord-no like oe-ordl.ord-no
  field i-no like oe-ordl.i-no
  field i-name like oe-ordl.i-name
  field qty-lft like oe-ordl.qty format "->>,>>>,>>9"
  field price like oe-ordl.price format "->>,>>>,>>9.99"
  field uom like oe-ordl.pr-uom
  field t-price like oe-ordl.t-price format "->>,>>>,>>9.99".

define TEMP-TABLE w-ord-misc NO-UNDO
  field ord-no like oe-ordm.ord-no
  field charge like oe-ordm.charge
  field dscr like oe-ordm.dscr
  field amt like oe-ordm.amt format "->>,>>>,>>9.99"
  field tax like oe-ordm.tax
  field bill like oe-ordm.bill.

{sys/form/r-top3w.f}

form HEADER
     SKIP(1)
     "Order#  Est#      Job#     Date     Bill To#   " +
     "Name                           Sold  To#     Order Total"
     fill("=",128) format "x(128)"

    WITH frame r-top.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
v-fr-tax = oe-ctrl.f-tax.

{sa/sa-sls01.i}


DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR v-last-shipid AS CHAR NO-UNDO.

DEFINE STREAM excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tran-date tb_detailed lv-ornt ~
rd-dest lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tran-date tran-period tb_detailed lv-ornt ~
rd-dest lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file 

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

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-backl.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 55 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE tran-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.1.

DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no 
     LABEL "Detailed?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tran-date AT ROW 3.14 COL 43 COLON-ALIGNED
     tran-period AT ROW 4.33 COL 43 COLON-ALIGNED
     tb_detailed AT ROW 5.76 COL 45
     lv-ornt AT ROW 10.38 COL 31.6 NO-LABEL
     rd-dest AT ROW 11 COL 7 NO-LABEL
     lines-per-page AT ROW 11.48 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 11.52 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 12.62 COL 29.6 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 14.19 COL 31.8
     tb_excel AT ROW 15.57 COL 48.8
     tb_runExcel AT ROW 15.57 COL 70.8
     fi_file AT ROW 16.57 COL 46.6 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 18.86 COL 23
     btn-cancel AT ROW 18.86 COL 58
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 9.81 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 9.33 COL 1
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
         TITLE              = "Order Edit List & Posting"
         HEIGHT             = 20.19
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_detailed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Order Edit List  Posting */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Order Edit List  Posting */
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


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.


  run check-date.
  if v-invalid then return no-apply.

  assign
   rd-dest
   tran-period.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
          {custom/asifax.i &type= "Customer"
                            &begin_cust= tran-date
                            &END_cust= tran-date 
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }   
       END. 
       when 5 then do:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = " "
                             &begin_cust= ''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = " "
                                  &begin_cust= ''
                                  &END_cust='' 
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
     END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.
  end case.

  IF ip-post THEN DO:
    IF v-postable THEN DO:
      lv-post = NO.

      MESSAGE "Post Orders?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE lv-post.

      IF lv-post THEN do:
        RUN list-post ("post").

        MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
      END.
    END.

    ELSE MESSAGE "No orders available for posting..." VIEW-AS ALERT-BOX ERROR.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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


&Scoped-define SELF-NAME tb_detailed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detailed C-Win
ON VALUE-CHANGED OF tb_detailed IN FRAME FRAME-A /* Detailed? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
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
ON LEAVE OF tran-date IN FRAME FRAME-A /* Post Date */
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  assign
   tran-date   = today
   c-win:TITLE = IF ip-post THEN "Order Posting/Purge Deleted"
                            ELSE "Order Edit List".

  RUN enable_UI.

  RUN check-date.

  IF NOT ip-post THEN DO WITH FRAME {&FRAME-NAME}:  
    DISABLE tran-date tran-period.
  END.

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

    else
    IF ip-post THEN DO:
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
  DISPLAY tran-date tran-period tb_detailed lv-ornt rd-dest lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tran-date tb_detailed lv-ornt rd-dest lines-per-page 
         lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE list-post C-Win 
PROCEDURE list-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-list-post AS CHAR NO-UNDO.

def var v-tax-rate as dec format ">,>>9.99<<<".
DEF VAR v-tax-amt AS DEC NO-UNDO INIT 0.
def var v-inv as log init no.
def var v-ship as log init no.
def var v-tot-tax like oe-ord.tax.
def var v-tot-freight like oe-ord.t-freight.
def var v-qty-lft like oe-ordl.qty.
def var v-ext-price like oe-ordl.t-price.
def var v-tot-ord as dec format "->>,>>>,>>9.99".
def var v-tot-qty as int format "->>,>>>,>>9".
def var v-g-tot-ord as dec format "->>,>>>,>>9.99".
def var v-g-tot-qty as int format "->>,>>>,>>9".
def var v-g-del-tot-ord as dec format "->>,>>>,>>9.99".
def var v-g-del-tot-qty as int format "->>,>>>,>>9".
DEF VAR v-freight-rate  AS DECIMAL NO-UNDO.  /* this variable is set and ignored, not used in calcs */
DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.
form
  xoe-ord.ord-no xoe-ord.est-no xoe-ord.job-no space(0) "-" space(0)
  xoe-ord.job-no2 format "99"
  space(2) xoe-ord.due-date
  space(2) xoe-ord.cust-no
  space(3) xoe-ord.cust-name xoe-ord.sold-id space(3) v-tot-ord
with no-labels no-box  no-underline STREAM-IO width 132 frame ord.

form
      w-ord-line.i-no at 10 label "Item"
      w-ord-line.i-name format "x(25)" label "Description"
      w-ord-line.uom label "UOM"
      w-ord-line.qty-lft format "->>,>>>,>>9" label "Quantity"
      w-ord-line.price format "->>,>>>,>>9.99" label "Price" 
      w-ord-line.t-price label "Ext. Price" skip
with down no-box STREAM-IO width 132 frame ordl.

form
      w-ord-misc.charge at 10 column-label "Miscellaneous!Charge"
      w-ord-misc.dscr label "Description"
      w-ord-misc.amt format "->>>,>>9.99" to 80 label "Price"
      skip
with down no-box STREAM-IO width 132 frame ordm.


  SESSION:SET-WAIT-STATE("general").

  IF ip-list-post EQ "list" THEN

  for each tt-report where tt-report.term-id eq v-term no-lock,

      first xoe-ord where recid(xoe-ord) eq tt-report.rec-id NO-LOCK,

      first cust {sys/ref/custW.i} and cust.cust-no eq xoe-ord.cust-no NO-LOCK

      by tt-report.key-01:

      RUN ar/cctaxrt.p (cocode,xoe-ord.tax-gr, OUTPUT v-tax-rate, OUTPUT v-freight-rate /* ignored */).

      /******* CALCULATE TOTALS FOR ORDER ***************/
      assign
       v-postable = yes
       v-tot-ord = 0
       v-tot-qty = 0.

      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ xoe-ord.company
            AND oe-ordl.ord-no  EQ xoe-ord.ord-no
          no-lock break by oe-ordl.ord-no:

        if first-of(oe-ordl.ord-no) and v-detail  then
          put skip.

        assign
         v-qty-lft = oe-ordl.qty - oe-ordl.inv-qty.
         v-ext-price = 0.

        if oe-ordl.pr-uom Begins "L" AND oe-ordl.pr-uom NE "LB" then
          assign
           v-ext-price = oe-ordl.price -
                         round( (oe-ordl.price * oe-ordl.disc) / 100, 2).

        else
        if oe-ordl.pr-uom EQ "CS" THEN DO:
          FIND FIRST itemfg
              {sys/look/itemfgrlW.i}
                and itemfg.i-no EQ oe-ordl.i-no
              no-lock no-error.
          if avail itemfg and itemfg.case-count ne 0 then
            assign
             v-ext-price = ((v-qty-lft / itemfg.case-count) * oe-ordl.price) -
                           round((((v-qty-lft / itemfg.case-count) *
                                   oe-ordl.price) * oe-ordl.disc) / 100, 2).
          else
            assign
             v-ext-price = (v-qty-lft * oe-ordl.price) -
                           round(((v-qty-lft * oe-ordl.price) * oe-ordl.disc) / 100, 2).
        end.

        ELSE
        if oe-ordl.pr-uom EQ "C" then
          assign
           v-ext-price = ((v-qty-lft / 100) *
                          oe-ordl.price) - round((((v-qty-lft / 100) *
                          oe-ordl.price) * oe-ordl.disc) / 100, 2).
        else
        if oe-ordl.pr-uom EQ "EA" then
          v-ext-price = (v-qty-lft * oe-ordl.price) -
                        round(( (v-qty-lft * oe-ordl.price) * oe-ordl.disc) / 100, 2).

        ELSE
          assign
           v-ext-price = ((v-qty-lft / 1000) *
                          oe-ordl.price) - round(( ((v-qty-lft / 1000) *
                          oe-ordl.price) * oe-ordl.disc) / 100, 2).

                                           /** CALCULATE FREIGHT CHARGES **/
        v-tot-freight = v-tot-freight +
                        (round(oe-ordl.t-freight / oe-ordl.qty, 2) * v-qty-lft).

                                           /** CALCULATE TAX CHARGES **/
        if oe-ordl.tax and v-tax-rate > 0 then assign
          v-tot-tax = v-tot-tax +
                                round((v-ext-price * v-tax-rate) / 100,2).

        create w-ord-line.
        assign
         w-ord-line.ord-no = oe-ordl.ord-no
         w-ord-line.i-no = oe-ordl.i-no
         w-ord-line.i-name = oe-ordl.i-name
         w-ord-line.qty-lft = v-qty-lft
         w-ord-line.price = oe-ordl.price
         w-ord-line.uom = oe-ordl.pr-uom
         w-ord-line.t-price = v-ext-price
         v-tot-ord = v-tot-ord +
                     if xoe-ord.stat ne "D" then v-ext-price
                                           else - ( v-ext-price )
         v-tot-qty = v-tot-qty +
                     if xoe-ord.stat ne "D" then v-qty-lft
                                           else - ( v-qty-lft ).  
      end. /* each oe-ordl */

      for each oe-ordm
          where oe-ordm.company EQ xoe-ord.company
            AND oe-ordm.ord-no  EQ xoe-ord.ord-no
          no-lock

          break by oe-ordm.ord-no:

        if oe-ordm.bill EQ "Y" then do:
          v-tot-ord = v-tot-ord + oe-ordm.amt.

          if oe-ordm.tax and v-tax-rate > 0 THEN
            v-tot-tax = v-tot-tax + round((oe-ordm.amt * v-tax-rate) / 100,2).
        end. /* = "Y" */

        create w-ord-misc.
        assign
         w-ord-misc.ord-no = oe-ordm.ord-no
         w-ord-misc.charge = oe-ordm.charge
         w-ord-misc.dscr = oe-ordm.dscr
         w-ord-misc.amt = oe-ordm.amt
         w-ord-misc.tax = oe-ordm.tax
         w-ord-misc.bill = oe-ordm.bill.
      end. /* each oe-ordm */

      DISPLAY xoe-ord.ord-no
              xoe-ord.est-no
              xoe-ord.job-no
              xoe-ord.job-no2
              xoe-ord.due-date 
              xoe-ord.cust-no
              xoe-ord.cust-name
              xoe-ord.sold-id
              v-tot-ord

          with frame ord.

      ASSIGN exl-due-date = STRING(xoe-ord.due-date) .

      IF tb_excel AND NOT v-detail THEN DO:                                     /*Task# 12121301*/
              PUT STREAM excel UNFORMATTED
                  '"'  xoe-ord.ord-no       '",'
                  '"'  xoe-ord.est-no       '",'
                  '"'  xoe-ord.job-no       '",'
                  '"'  xoe-ord.job-no2      '",'
                  '"'  IF exl-due-date EQ "?" THEN "" ELSE  exl-due-date     '",'
                  '"'  xoe-ord.cust-no      '",'
                  '"'  xoe-ord.cust-name    '",'
                  '"'  xoe-ord.sold-id      '",'
                  '"'  v-tot-ord            '",'        
             SKIP.
      END. 

      for each w-ord-line break by w-ord-line.ord-no:
        if v-detail THEN do:
          if first(w-ord-line.ord-no) then put skip(1).

          DISPLAY w-ord-line.i-no
                  w-ord-line.i-name
                  w-ord-line.qty-lft
                  w-ord-line.price
                  w-ord-line.uom
                  w-ord-line.t-price

              with frame ordl.
          down with frame ordl.

          ASSIGN itemname = w-ord-line.i-name 
                 ino      = w-ord-line.i-no . 

          if index(itemname,'"',1) > 0 then assign
              itemname = replace(itemname,'"'," "). 
          if index(itemname,',',1) > 0 then assign
              itemname = replace(itemname,','," "). 
          if index(ino,'"',1) > 0 then assign
              ino = replace(ino,'"'," "). 
          if index(ino,',',1) > 0 then assign
              ino = replace(ino,','," "). 


         IF tb_excel THEN DO:                                       /*Task# 12121301*/
              PUT STREAM excel UNFORMATTED
                  '"'  xoe-ord.ord-no       '",'
                  '"'  xoe-ord.est-no       '",'
                  '"'  xoe-ord.job-no       '",'
                  '"'  xoe-ord.job-no2      '",'
                  '"'  IF exl-due-date EQ "?" THEN "" ELSE  exl-due-date    '",'
                  '"'  xoe-ord.cust-no      '",'
                  '"'  xoe-ord.cust-name    '",'
                  '"'  xoe-ord.sold-id      '",'
                  '"'  v-tot-ord            '",' 
                  '"'  ino                  '",'
                  '"'  itemname              '",'
                  '"'  w-ord-line.uom        '",'
                  '"'  w-ord-line.qty-lft    '",'
                  '"'  w-ord-line.price      '",'
                  '"'  w-ord-line.t-price    '",'      SKIP .
          END. 


          if LAST(w-ord-line.ord-no) THEN do:                       /*Task# 12121301*/
              for each w-ord-misc break by w-ord-misc.ord-no:
                  IF tb_excel THEN DO:
                      PUT STREAM excel UNFORMATTED
                          '"'  xoe-ord.ord-no       '",'
                          '"'  xoe-ord.est-no       '",'
                          '"'  xoe-ord.job-no       '",'
                          '"'  xoe-ord.job-no2      '",'
                          '"'  IF exl-due-date EQ "?" THEN "" ELSE  exl-due-date    '",'
                          '"'  xoe-ord.cust-no      '",'
                          '"'  xoe-ord.cust-name    '",'
                          '"'  xoe-ord.sold-id      '",'
                          '"'  v-tot-ord            '",' 
                          '"'  w-ord-misc.charge    '",'
                          '"'  w-ord-misc.dscr      '",'
                          '"'  ""    '",'
                          '"'  ""    '",'
                          '"'  w-ord-misc.amt       '",'  
                          SKIP.
                  END.
              end. /* each w-ord-misc */
          END. 
        END.
        delete w-ord-line.
      end. /* each w-ord-line */

      for each w-ord-misc break by w-ord-misc.ord-no:
        if v-detail THEN do:
          if first(w-ord-misc.ord-no) then put skip(1).

          DISPLAY w-ord-misc.charge
                  w-ord-misc.dscr w-ord-misc.amt

              with frame ordm.

          if w-ord-misc.bill = "N" then
            DISPLAY "       N/C" @ w-ord-misc.amt with frame ordm.

          down with frame ordm.
        end. /* v-detail */

        delete w-ord-misc.
      end. /* each w-ord-misc */

      if xoe-ord.stat EQ "H" then
        put "** THIS ORDER IS ON CREDIT HOLD **" to 39.
      else
      if xoe-ord.stat EQ "D" then
        put "** THIS ORDER IS DELETED **" to 39.
      else
      if xoe-ord.stat EQ "C" OR xoe-ord.stat EQ "Z" then
        put "** THIS ORDER IS CLOSED **" to 39.

      if xoe-ord.stat ne "D" then
        assign
         v-g-tot-ord = v-g-tot-ord + v-tot-ord
         v-g-tot-qty = v-g-tot-qty + v-tot-qty.
      else
        assign
         v-g-del-tot-ord = v-g-del-tot-ord + v-tot-ord
         v-g-del-tot-qty = v-g-del-tot-qty + v-tot-qty.

      put skip(1).
  END. /*ip-list-post eq "list"*/

  ELSE IF ip-list-post EQ "post" THEN
  DO:
     for each tt-report where tt-report.term-id eq v-term no-lock,

     first xoe-ord where recid(xoe-ord) eq tt-report.rec-id,

     first cust {sys/ref/custW.i} and cust.cust-no eq xoe-ord.cust-no

     by tt-report.key-01:

     RUN ar/cctaxrt.p (cocode,xoe-ord.tax-gr, OUTPUT v-tax-rate, OUTPUT v-freight-rate /* ignored */).

     DO TRANSACTION:
        assign
         xoe-ord.t-comm    = 0
         xoe-ord.t-cost    = 0
         xoe-ord.t-revenue = 0
         xoe-ord.tax       = 0
         v-inv             = NO
         v-ship            = NO.

        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ xoe-ord.company
              AND oe-ordl.ord-no  EQ xoe-ord.ord-no:

          if oe-ordl.stat eq "I" or oe-ordl.stat eq "B" then v-inv = yes.
          else v-ship = yes.

          assign
           xoe-ord.t-revenue = xoe-ord.t-revenue + oe-ordl.t-price
           xoe-ord.t-cost    = xoe-ord.t-cost    + oe-ordl.t-cost.

          if oe-ordl.tax and v-tax-rate gt 0 THEN DO:

              RUN ar/calctax2.p (xoe-ord.tax-gr,
                                 NO,
                                 oe-ordl.t-price,
                                 xoe-ord.company,
                                 oe-ordl.i-no,
                                 OUTPUT v-tax-amt).

              ASSIGN xoe-ord.tax = xoe-ord.tax + v-tax-amt.
          END.

          if xoe-ord.stat eq "D" then delete oe-ordl.
        END.

        for each oe-ordm
            where oe-ordm.company eq xoe-ord.company
              and oe-ordm.ord-no  eq xoe-ord.ord-no:

          IF oe-ordm.bill EQ "Y" THEN do:
            xoe-ord.t-revenue = xoe-ord.t-revenue + oe-ordm.amt.

            if oe-ordm.tax and v-tax-rate gt 0 THEN DO:

                RUN ar/calctax2.p (xoe-ord.tax-gr,
                                   NO,
                                 oe-ordm.amt,
                                 xoe-ord.company,
                                 oe-ordm.ord-i-no,
                                 OUTPUT v-tax-amt).

                ASSIGN xoe-ord.tax = xoe-ord.tax + v-tax-amt.
            END.
          END.

          if xoe-ord.stat eq "D" then delete oe-ordm.
        END.

        run oe/oe-comm.p.  /* Calculate Commissions */

        if xoe-ord.f-bill THEN
          xoe-ord.t-revenue = xoe-ord.t-revenue + xoe-ord.t-freight.

        if v-fr-tax THEN
          xoe-ord.tax = xoe-ord.tax +
                       round((xoe-ord.t-freight * v-tax-rate) / 100,2).

        IF xoe-ord.stat eq "H" then xoe-ord.posted = yes.

        ELSE
        IF xoe-ord.stat eq "D" then do:
          FOR EACH oe-rel
              WHERE oe-rel.company EQ xoe-ord.company
                AND oe-rel.ord-no  EQ xoe-ord.ord-no:

               FIND FIRST itemfg-loc
                   WHERE itemfg-loc.company EQ oe-rel.company
                     AND itemfg-loc.i-no    EQ oe-rel.i-no
                     AND itemfg-loc.loc     EQ oe-rel.spare-char-1
                   EXCLUSIVE-LOCK NO-ERROR.
               FIND itemfg WHERE itemfg.company EQ oe-rel.company
                    AND itemfg.i-no EQ oe-rel.i-no
                   NO-LOCK NO-ERROR.
               IF AVAIL(itemfg) AND AVAIL(itemfg-loc) THEN
                 RUN fg/calcqabl.p (ROWID(itemfg), oe-rel.spare-char-1, 
                                    OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
               FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
            DELETE oe-rel.
          END.

          for each oe-relh
              where oe-relh.company eq xoe-ord.company
                and oe-relh.ord-no  eq xoe-ord.ord-no
                and oe-relh.posted  eq no
              use-index relh:

            for each oe-rell where oe-rell.r-no eq oe-relh.r-no:
              delete oe-rell.
            end.

            delete oe-relh.
          end.
/*        06211305 - Replace n-ord with sequence */        
/*           find first oe-ctrl WHERE oe-ctrl.company EQ xoe-ord.company */
/*               exclusive-lock no-error.                                */
/*           if xoe-ord.ord-no eq oe-ctrl.n-ord - 1 then                 */
/*              oe-ctrl.n-ord = xoe-ord.ord-no.                          */

          delete xoe-ord.
        end.

        else
        if xoe-ord.stat eq "C" then xoe-ord.stat = "Z".

        ELSE DO:   
          if xoe-ord.stat eq "U" THEN
            xoe-ord.stat = IF v-inv THEN "I" ELSE
                          IF v-ship THEN "S" ELSE "R".

          else
            assign
             cust.n-sales[13]          = cust.n-sales[13] + 1
             cust.n-sales[tran-period] = cust.n-sales[tran-period] + 1
             xoe-ord.stat              = "R".

          xoe-ord.tot-ord = xoe-ord.t-revenue.
        end.
     END.
     END.
   FIND CURRENT xoe-ord NO-LOCK NO-ERROR.
   FIND CURRENT cust NO-LOCK NO-ERROR.
  END.

  IF ip-list-post EQ "list" THEN DO:
    if v-detail then put  "---------" at 56.

    put "------------" at 89 skip.

    if v-detail THEN do:
      put "Total New Orders:" to 53
          v-g-tot-qty         to 64
          v-g-tot-ord         to 100.

      put "  Deleted Orders:" to 53
          v-g-del-tot-qty     to 64
          v-g-del-tot-ord     to 100.

      put "    Grand Totals:"                                     to 53
          (v-g-tot-qty + v-g-del-tot-qty) format "->>,>>>,>>9"    to 64
          (v-g-tot-ord + v-g-del-tot-ord) format "->>,>>>,>>9.99" to 100.
      IF tb_excel THEN DO:                              /*Task# 12121301*/
          PUT STREAM excel UNFORMATTED
              '"'  "Total New Orders:"  '",'
              '"'  v-g-tot-qty          '",'
              '"'  v-g-tot-ord          '",' SKIP
              '"'  " Deleted Orders:"   '",'
              '"'  v-g-del-tot-qty     '",'
              '"'  v-g-del-tot-ord     '",'  SKIP
              '"'  "Grand Totals:"    '",'
              '"'  (v-g-tot-qty + v-g-del-tot-qty)      '",'
              '"'  (v-g-tot-ord + v-g-del-tot-ord)      '",' SKIP .
      END.
    end.

    ELSE do:
      put "Total New Orders:" to 68
          v-g-tot-ord         to 100.
      put "  Deleted Orders:" to 68
          v-g-del-tot-ord     to 100.
      put "    Grand Totals:"                                     to 68
          (v-g-tot-ord + v-g-del-tot-ord) format "->>,>>>,>>9.99" to 100.
      IF tb_excel THEN DO:                      /*Task# 12121301*/
          PUT STREAM excel UNFORMATTED
              '"'  "Total New Orders:"  '",'
              '"'  v-g-tot-ord          '",' SKIP
              '"'  " Deleted Orders:"   '",'
              '"'  v-g-del-tot-ord     '",'  SKIP
              '"'  "Grand Totals:"    '",'
              '"'  (v-g-tot-ord + v-g-del-tot-ord)      '",' SKIP .
      END.
    END.
  END.

  SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-port C-Win 
PROCEDURE output-to-port :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN custom/d-print.w (list-name).

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

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------- oe/rep/neword.p 8/93 rd  */
/* Order Entry - Edit Register                                                */
/*  FOR: Order Status of - (N)ew, (A)pproved Credit, (U)pdated, (D)eleted,    */
/*                          & (C)losed                                        */
/* -------------------------------------------------------------------------- */

  DEF VAR excelheader AS CHAR NO-UNDO.

  FIND first period                   
      where period.company eq gcompany
        and period.pst     le tran-date
        and period.pend    ge tran-date
      no-lock no-error.

  assign
   str-tit2 = c-win:title 
   {sys/inc/ctrtext.i str-tit2 112}

   str-tit3 = "Period " + string(tran-period,"99") + " - " +
              IF AVAIL period THEN
                (string(period.pst) + " to " + string(period.pend)) ELSE ""
   {sys/inc/ctrtext.i str-tit3 132}

   v-detail   = tb_detailed
   v-postable = NO.

   EMPTY TEMP-TABLE tt-report.

   for each oe-ord
      where oe-ord.company eq cocode
        and oe-ord.posted  eq no
        and (oe-ord.stat   eq "N" or
             oe-ord.stat   eq "A" or
             oe-ord.stat   eq "U" or
             oe-ord.stat   eq "H" or 
             oe-ord.stat   eq "D" or
             oe-ord.stat   eq "C")
      use-index posted no-lock,

      first cust
      {sys/ref/custW.i}
        and cust.cust-no eq oe-ord.cust-no
      NO-LOCK

      transaction:

    find xoe-ord where recid(xoe-ord) eq recid(oe-ord)
        exclusive-lock no-wait no-error.

    if avail xoe-ord then do:
      create tt-report.
      assign
       tt-report.term-id = v-term
       tt-report.key-01  = string(xoe-ord.ord-no,"9999999999")
       tt-report.rec-id  = recid(xoe-ord).

      FIND CURRENT xoe-ord NO-LOCK.
    end.
  end.

 /*
  for each oe-ord no-lock
      where oe-ord.company eq cocode
        and oe-ord.posted  eq no
        and can-do("C,D",oe-ord.stat)
      use-index posted,

      first cust no-lock
      {sys/ref/custW.i}
        and cust.cust-no eq oe-ord.cust-no:

    find xoe-ord where recid(xoe-ord) eq recid(oe-ord)
        exclusive-lock no-wait no-error.

    if avail xoe-ord then do:
      create tt-report.
      assign
       tt-report.term-id = v-term
       tt-report.key-01  = string(xoe-ord.ord-no,"9999999999")
       tt-report.rec-id  = recid(xoe-ord).
    end.
  end.
  */

  {sys/inc/print1.i}

  {sys/inc/outprint.i value(lines-per-page)}

  if td-show-parm then run show-param.

  display with frame r-top.

  IF tb_excel THEN do:                                  /*Task# 12121301*/
      OUTPUT STREAM excel TO VALUE(fi_file).
      IF v-detail THEN
          excelheader = excelheader + "Order #,Est #,Job#,Job#2,Date,Bill To#,Name,"
                                    + "Sold To#,Order Total,Item/Misc Chg,Description,UOM,Quantity,"
                                    + "Price,Ext Price".
      ELSE
          excelheader = excelheader + "Order #,Est #,Job#,Job#2,Date,Bill To#,Name,"
                                    + "Sold To#,Order Total".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.


  RUN list-post ("list").

  IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.

    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
  END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

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
  def var lv-label as cha NO-UNDO.

  ASSIGN
  lv-frame-hdl = frame {&frame-name}:HANDLE
  lv-group-hdl = lv-frame-hdl:first-child
  lv-field-hdl = lv-group-hdl:first-child.

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
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

