&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-schrpt2.w

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

   
def new shared var v-s-vend like vend.vend-no init "".
def new shared var v-e-vend like vend.vend-no init "zzzzzzzz".
def new shared var v-s-date like po-ord.po-date init today format "99/99/9999".
def new shared var v-e-date like v-s-date init 12/31/9999.
def new shared var v-po-stat like po-ord.stat init "O".

def TEMP-TABLE w-sched NO-UNDO
    field job-no like po-ordl.job-no
    field job-no2 like po-ordl.job-no2
    field i-no like po-ordl.i-no
    field i-name like po-ordl.i-name
    field vend-no like po-ord.vend-no
    field po-no like po-ordl.po-no
    field po-date like po-ord.po-date
    field cons-uom like po-ordl.cons-uom
    field cons-qty like po-ordl.cons-qty
    field t-rec-qty like po-ordl.t-rec-qty
    field due-date like po-ordl.due-date
    field amt-msf like ap-invl.amt-msf
    field vend-name like vend.name
    FIELD carrier LIKE vend.carrier.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date rd_show rd_print rd-dest lv-ornt lines-per-page lv-font-no ~
td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date lbl_show rd_show lbl_print rd_print rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_print AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_show AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Job" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job", "Job",
"Item", "Item",
"Vendor", "Vendor"
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Open" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All PO's", "All PO's"
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.81.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_vend-no AT ROW 3.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend-no AT ROW 3.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor number"
     begin_due-date AT ROW 4.81 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Due Date"
     end_due-date AT ROW 4.81 COL 69 COLON-ALIGNED HELP
          "Enter ending Due Date"
     lbl_show AT ROW 6.48 COL 39 COLON-ALIGNED NO-LABEL
     rd_show AT ROW 6.48 COL 48 NO-LABEL
     lbl_print AT ROW 7.67 COL 39 COLON-ALIGNED NO-LABEL
     rd_print AT ROW 7.67 COL 48 NO-LABEL
     rd-dest AT ROW 11.71 COL 5 NO-LABEL
     lv-ornt AT ROW 11.95 COL 31 NO-LABEL
     lines-per-page AT ROW 11.95 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 13.86 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 14.81 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 16.48 COL 31
     btn-ok AT ROW 19.1 COL 18
     btn-cancel AT ROW 19.1 COL 57
     RECT-6 AT ROW 10.05 COL 1
     RECT-7 AT ROW 1 COL 1
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 10.76 COL 3
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
         TITLE              = "Scheduled Receipts"
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
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_print IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_print".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Scheduled Receipts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Scheduled Receipts */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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

run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=END_vend-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend-no
                             &END_cust=end_vend-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
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
     begin_due-date =  TODAY
     end_due-date   =  date(12,31,9999).

  RUN enable_UI.

  {methods/nowait.i}

APPLY "entry"  TO begin_vend-no IN FRAME {&FRAME-NAME}.

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
  DISPLAY begin_vend-no end_vend-no begin_due-date end_due-date lbl_show rd_show 
          lbl_print rd_print rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_vend-no end_vend-no begin_due-date end_due-date rd_show rd_print 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
         btn-cancel RECT-6 RECT-7 
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
/*     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
          
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
         
     IF NOT OKpressed THEN  RETURN NO-APPLY. */
     
{custom/out2file.i}

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ---------------------------------------------- po/rep/sch-rcts.p 8/96 fwk  */
/* Scheduled Receipts Report                                                  */
/* -------------------------------------------------------------------------- */
{sys/form/r-topw.f}

def var v-sort as char format "x" init "J" no-undo.
def var code-text as char no-undo.
def var stat-list as char.
def var v-len like po-ordl.s-len.
def var v-wid like po-ordl.s-wid.
def var v-bwt like item.basis-w.
def var v-qty as dec.
def var v-cost as dec.
def var v-s-num like po-ordl.s-num init 1 no-undo.
def var v-tot as dec.

FORM header
        "JOB NO" at 1
        "ITEM NO" at 12
        "ITEM NAME" at 29
        "VEND NO" at 57
        "P/O" to 72
        "P/O DATE" at 75
        "UOM" at 85
        "QTY ORDER" to 104
        "QTY RECEIVED" to 121
        "REQ DATE"   FORMAT "99/99/99" AT 124
        "CARRIER" AT 130
        fill("-",132) format "x(132)"
        with STREAM-IO width 132 no-labels no-box no-underline page-top
        frame sch-head-job.

form header
        "ITEM NO" at 1
        "ITEM NAME" at 18
        "VEND NO" at 46
        "JOB NO" at 57
        "P/O" to 72
        "P/O DATE" at 75
        "UOM" at 85
        "QTY ORDER" to 104
        "QTY RECEIVED" to 121
        "REQ DATE"     FORMAT "99/99/99"  AT 124
        "CARRIER" AT 130
        fill("-",132) format "x(132)"
        with STREAM-IO width 132 no-labels no-box no-underline page-top
        frame sch-head-item.

form header
        "VEND NO" at 1
        "ITEM NO" at 28
        "JOB NO" at 45
        "P/O" to 61
        "P/O DATE" at 64
        "QTY ORDER" to 91
        "QTY RECEIVED" to 108
        "REQ DATE"     FORMAT "99/99/99"  AT 111
        "MSF" to 131 skip
        "VENDOR NAME" at 3
        "ITEM NAME" at 30
        "Carrier"   AT 130
        fill("-",132) format "x(132)"
        with STREAM-IO width 132 no-labels no-box no-underline page-top
        frame sch-head-vend.

  form w-sched.job-no at 1 space(0) "-" space(0) w-sched.job-no2
       w-sched.i-no at 12
       w-sched.i-name at 29 format "x(25)"
       w-sched.vend-no at 57
       w-sched.po-no at 67
       w-sched.po-date at 75
       w-sched.cons-uom at 85
       w-sched.cons-qty to 104 format "->>>,>>>,>>9.99"
       w-sched.t-rec-qty to 121 format "->>>,>>>,>>9.99"
       w-sched.due-date FORMAT "99/99/99" at 122
       w-sched.carrier AT 130
       with down STREAM-IO width 132 no-labels no-box no-underline frame sch-rcts-job.

  form w-sched.i-no at 1
       w-sched.i-name at 18 format "x(25)"
       w-sched.vend-no at 46
       w-sched.job-no at 57 space(0) "-" space(0) w-sched.job-no2
       w-sched.po-no at 67
       w-sched.po-date at 75
       w-sched.cons-uom at 85
       w-sched.cons-qty to 104 format "->>>,>>>,>>9.99"
       w-sched.t-rec-qty to 121 format "->>>,>>>,>>9.99"
       w-sched.due-date FORMAT "99/99/99" AT 122
       with down STREAM-IO width 132 no-labels no-box no-underline frame sch-rcts-item.
            
  form w-sched.vend-no at 1
       w-sched.i-no at 28
       w-sched.job-no at 45 space(0) "-" space(0) w-sched.job-no2
       w-sched.po-no AT 61
       w-sched.po-date  AT 64
       w-sched.cons-qty to 91 format "->>>,>>>,>>9.99"
       w-sched.t-rec-qty to 108 format "->>>,>>>,>>9.99"
       w-sched.due-date FORMAT "99/99/99" AT 111
       w-sched.amt-msf to 131 skip
       w-sched.vend-name at 3 format "x(25)"
       w-sched.i-name at 30 format "x(20)"
       with down STREAM-IO width 132 no-labels no-box no-underline frame sch-rcts-vend.
      
  {ce/msfcalc.i}

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-s-vend   = begin_vend-no
 v-e-vend   = end_vend-no
 v-s-date   = begin_due-date
 v-e-date   = end_due-date
 v-po-stat  = SUBSTR (rd_show,1,1)
 v-sort     = SUBSTR (rd_print,1,1).

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.
 
   if v-sort eq "J" then
    DISPLAY WITH frame sch-head-job.
  else if v-sort eq "I" then
    DISPLAY WITH frame sch-head-item.
  else
    DISPLAY WITH frame sch-head-vend.

  EMPTY TEMP-TABLE w-sched.
  
  stat-list = if v-po-stat eq "A" then ""             else
              if v-po-stat eq "O" then "O,U,P,A,N,H"  else "C,X,F".

  for each po-ord
      where po-ord.company eq cocode
        and (lookup(po-ord.stat,stat-list) gt 0 or v-po-stat eq "A")
        and po-ord.vend-no ge v-s-vend
        and po-ord.vend-no le v-e-vend
      no-lock,

      each po-ordl WHERE 
           po-ordl.company EQ po-ord.company
        AND po-ordl.po-no EQ po-ord.po-no
        and (lookup(po-ordl.stat,stat-list) gt 0 or v-po-stat eq "A")
        and po-ordl.due-date ge v-s-date
        and po-ordl.due-date le v-e-date
      no-lock:
    
    find first item
        where item.company eq cocode
          and item.i-no    eq po-ordl.i-no
          and po-ordl.item-type
        no-lock no-error.
      
    assign
     v-qty  = po-ordl.cons-qty
     v-cost = po-ordl.cons-cost.  
      
    if not avail item and po-ordl.cons-uom ne "EA" then do:
      run sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                             po-ordl.cons-qty, output v-qty).
                             
      run sys/ref/convcuom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                             po-ordl.cons-cost, output v-cost).
    end.
      
    if v-qty - po-ordl.t-rec-qty gt 0 then do:
      v-tot = v-tot + ((v-qty - po-ordl.t-rec-qty) * v-cost).
      
      create w-sched.
      assign
       w-sched.job-no    = po-ordl.job-no
       w-sched.job-no2   = po-ordl.job-no2
       w-sched.i-no      = po-ordl.i-no
       w-sched.i-name    = po-ordl.i-name
       w-sched.vend-no   = po-ord.vend-no
       w-sched.po-no     = po-ordl.po-no
       w-sched.po-date   = po-ord.po-date
       w-sched.cons-uom  = po-ordl.cons-uom
       w-sched.cons-qty  = v-qty
       w-sched.t-rec-qty = po-ordl.t-rec-qty
       w-sched.due-date  = po-ordl.due-date
       w-sched.carrier   = po-ord.carrier.
      if v-sort eq "V" then do:
        find first vend
            where vend.company eq cocode
              and vend.vend-no eq po-ord.vend-no
            no-lock no-error.

        assign
         w-sched.vend-name = if avail vend then vend.name else ""

         v-len = po-ordl.s-len
         v-wid = po-ordl.s-wid
         v-bwt = 0
         v-s-num = po-ordl.s-num.

        if po-ordl.item-type then do:
          find first item
              where item.company eq cocode
                and item.i-no    eq po-ordl.i-no
              no-lock no-error.
          if avail item and item.mat-type eq "B" then do:
            {po/pol-dims.i}
          end.
        end.

        else do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq po-ordl.i-no
              no-lock no-error.
          if avail itemfg then do:
            if v-len eq 0 then v-len = itemfg.t-len.
            if v-wid eq 0 then v-wid = itemfg.t-wid.
          end.
        end.

        w-sched.amt-msf = v-qty - po-ordl.t-rec-qty.
        
        if not avail item then
          run sys/ref/convquom.p("EA", "MSF", 0,
                                 po-ordl.s-len, po-ordl.s-wid, 0,
                                 w-sched.amt-msf, output w-sched.amt-msf).
        else
        if po-ordl.cons-uom ne "MSF" then
          run sys/ref/convquom.p(po-ordl.cons-uom, "MSF", item.basis-w,
                                 po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                 w-sched.amt-msf, output w-sched.amt-msf).
      end.
    end.
  end.

  if v-sort eq "J" then
  for each w-sched break by w-sched.job-no by w-sched.job-no2:
    display w-sched.job-no w-sched.job-no2 w-sched.i-no w-sched.i-name
            w-sched.vend-no w-sched.po-no w-sched.po-date w-sched.cons-uom
            w-sched.cons-qty w-sched.t-rec-qty w-sched.due-date
            with frame sch-rcts-job.
    down with frame sch-rcts-job.
  end.

  else
  if v-sort eq "I" then
  for each w-sched break by w-sched.i-no:
    display w-sched.job-no w-sched.job-no2 w-sched.i-no w-sched.i-name
            w-sched.vend-no w-sched.po-no w-sched.po-date w-sched.cons-uom
            w-sched.cons-qty w-sched.t-rec-qty w-sched.due-date
            with frame sch-rcts-item.
    down with frame sch-rcts-item.
  end.

  else
  for each w-sched break by w-sched.vend-no:
    if first-of(w-sched.vend-no) then do:
      put skip(1).
      display w-sched.vend-no w-sched.vend-name with frame sch-rcts-vend.
    end.

    display w-sched.job-no w-sched.job-no2 w-sched.i-no w-sched.i-name
            w-sched.po-no w-sched.po-date w-sched.cons-qty
            w-sched.t-rec-qty w-sched.due-date w-sched.amt-msf
            with frame sch-rcts-vend.
    down with frame sch-rcts-vend.
  end.  
  find first w-sched no-error.
  if avail w-sched then
    put skip(1) "Total Value:" at 100 v-tot format ">>,>>>,>>9.99" skip(1).
      
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

