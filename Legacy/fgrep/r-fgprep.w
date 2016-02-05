&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-fgprep.w

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

def new shared var v-types as char format "x(10)".

def new shared var b-post-date as date init today no-undo.
def new shared var e-post-date as date init today no-undo.
def new shared var v-pr-tots      as log   format "Y/N"  init false no-undo.
def var v-post-date as date no-undo init today.     
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 post-date ldt-from ldt-to ~
t-receipt t-issue t-trans t-adj t-phy tb_sho-msf rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS post-date ldt-from ldt-to t-receipt ~
t-issue t-trans t-adj t-phy lbl_sho-msf tb_sho-msf rd-dest lv-ornt ~
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

DEFINE VARIABLE lbl_sho-msf AS CHARACTER FORMAT "X(256)":U INITIAL "Show MSF Totals?" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE ldt-from AS DATE FORMAT "99/99/9999":U 
     LABEL "Date range to select" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE ldt-to AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE post-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 
     LABEL "Posting Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

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
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.95.

DEFINE VARIABLE t-adj AS LOGICAL INITIAL no 
     LABEL "Adjustments" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .71
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-issue AS LOGICAL INITIAL no 
     LABEL "Shipments" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-phy AS LOGICAL INITIAL no 
     LABEL "Credit Returns" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-receipt AS LOGICAL INITIAL no 
     LABEL "Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE t-trans AS LOGICAL INITIAL no 
     LABEL "Transfers" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_sho-msf AS LOGICAL INITIAL no 
     LABEL "Show MSF Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     post-date AT ROW 2.43 COL 26 COLON-ALIGNED
     ldt-from AT ROW 3.86 COL 26 COLON-ALIGNED
     ldt-to AT ROW 3.86 COL 46 COLON-ALIGNED NO-LABEL
     t-receipt AT ROW 6.71 COL 32
     t-issue AT ROW 7.43 COL 32
     t-trans AT ROW 8.14 COL 32
     t-adj AT ROW 8.86 COL 32
     t-phy AT ROW 9.57 COL 32
     lbl_sho-msf AT ROW 10.52 COL 55 COLON-ALIGNED NO-LABEL
     tb_sho-msf AT ROW 10.52 COL 78
     rd-dest AT ROW 13.38 COL 5 NO-LABEL
     lv-ornt AT ROW 14.1 COL 31 NO-LABEL
     lines-per-page AT ROW 14.1 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 15.52 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 16.48 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.62 COL 30
     btn-ok AT ROW 21.24 COL 19
     btn-cancel AT ROW 21.24 COL 57
     "Transaction Types" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 5.76 COL 19
          FONT 6
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.67 COL 3
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
     RECT-6 AT ROW 12.43 COL 1
     RECT-7 AT ROW 1.24 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 22.62.


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
         TITLE              = "Aged Inventory Report"
         HEIGHT             = 22.86
         WIDTH              = 96
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
IF NOT C-Win:LOAD-ICON("images\progress":U) THEN
    MESSAGE "Unable to load icon: images\progress"
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
/* SETTINGS FOR FILL-IN lbl_sho-msf IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       post-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sho-msf:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Aged Inventory Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Aged Inventory Report */
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

  run run-report. 

  SESSION:SET-WAIT-STATE("general").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=post-date
                            &END_cust=post-date
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
   SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL post-date C-Win
ON LEAVE OF post-date IN FRAME FRAME-A /* Posting Date */
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


&Scoped-define SELF-NAME t-adj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-adj C-Win
ON VALUE-CHANGED OF t-adj IN FRAME FRAME-A /* Adjustments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-issue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-issue C-Win
ON VALUE-CHANGED OF t-issue IN FRAME FRAME-A /* Shipments */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-receipt C-Win
ON VALUE-CHANGED OF t-receipt IN FRAME FRAME-A /* Receipts */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-trans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-trans C-Win
ON VALUE-CHANGED OF t-trans IN FRAME FRAME-A /* Transfers */
DO:
      assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sho-msf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sho-msf C-Win
ON VALUE-CHANGED OF tb_sho-msf IN FRAME FRAME-A /* Show MSF Totals? */
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
      post-date = TODAY.

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
  DISPLAY post-date ldt-from ldt-to t-receipt t-issue t-trans t-adj t-phy 
          lbl_sho-msf tb_sho-msf rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 post-date ldt-from ldt-to t-receipt t-issue t-trans 
         t-adj t-phy tb_sho-msf rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm btn-ok btn-cancel 
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
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.  */
     
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
/* ---------------------------------------------- fg/rep/fg-aging.p 12/96 JLF */
/* finished goods aged inventory report                                       */
/* -------------------------------------------------------------------------- */
{sys/form/r-topw.f}



/* def shared var v-types as char format "x(10)".
def shared var b-post-date as date no-undo.
def shared var e-post-date as date no-undo.
def shared var v-pr-tots as log format "Y/N" init false no-undo.    

def var save_id as recid.
def var time_stamp as ch.
time_stamp = string(time, "HH:MMam").     

def shared var file_stamp as ch format "x(12)".*/
def var type as ch format "X" initial "R".
def var type-prt as ch format "X(11)" initial "           ".
def var v-fg-qty like fg-rdtlh.qty.
def var v-fg-cost as dec format "->>>,>>9.99<<".
def var v-fg-value as dec format "->>,>>>,>>9.99".
def var v-msf as dec format ">,>>9.999" extent 6.
def var v-tot-qty as int format "->>>,>>>,>>9".
def var v-tot-cost as dec format "->>>,>>9.99<<".
def var v-grd-tot-cost as dec format "->>,>>>,>>9.99<<".                     
def var v-tot-value as dec format "->>,>>>,>>9.99".
def var v-cum-tot as de.                                   
def var v-tran-type as char format "x(1)".      
def var v-entrytype as char initial "REC ,TRAN,ADJ ,SHIP,RET ".
def var v-on like eb.num-up.
def var v-qty-pallet as decimal format "->>,>>>,>>9" no-undo.
def var v-whse like fg-rdtlh.loc.   
def var v-cases like fg-rdtlh.cases no-undo.
def var v-qty-case like fg-rdtlh.qty-case no-undo.
def var v-i-no like fg-rcpth.i-no no-undo.
                      
def buffer b-fgrdtlh for fg-rdtlh.


 form fg-rcpth.trans-date            label "DATE"
     fg-rcpth.i-no                  label "ITEM"
     fg-rcpth.i-name format "x(14)" label "DESCRIPTION"
     fg-rcpth.po-no                 label "P.O. #"             
     po-ord.vend-no                 label "VENDOR"
     v-tran-type                    label "T"
     fg-rdtlh.tag                   label "TAG #"
     v-cases                        label "UNITS"   format "->>,>>9"
     v-qty-case                     label "COUNT"   format "->>>,>>9"
     fg-rdtlh.qty-case              label "COUNT"   format "->>>,>>9"
     fg-rdtlh.loc-bin               label "BIN"
     fg-rcpth.pur-uom               label "UOM"
     v-fg-cost                      column-label "TOTAL!COST"    

    with frame itemx no-box down STREAM-IO width 132.

{ce/msfcalc.i}

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-post-date  = post-date
 b-post-date  = ldt-from
 e-post-date  = ldt-to
 /* v-entrytype
    v-types      =  */
 v-pr-tots    =  tb_sho-msf.


  
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

display "" with frame r-top.
   
 {sa/sa-sls01.i}
  
  v-i-no = "".
  
  find first fg-rcpth
      where fg-rcpth.company eq cocode
        and fg-rcpth.i-no    ge v-i-no
      no-lock no-error.
  
  do while avail fg-rcpth:
    v-i-no = fg-rcpth.i-no.
  /* Create Report file for History Records */
    do i = 1 to length(trim(v-types)):
      if index("RSTAE",substr(v-types,i,1)) gt 0 then
      for each fg-rcpth 
          where fg-rcpth.company                  eq cocode
            and fg-rcpth.i-no                     eq v-i-no
            and fg-rcpth.rita-code                eq substr(v-types,i,1)
            and ((fg-rcpth.post-date              ge b-post-date and
                  fg-rcpth.post-date              le e-post-date and
                  fg-rcpth.post-date              ne ?) or
                 (fg-rcpth.trans-date             ge b-post-date and
                  fg-rcpth.trans-date             le e-post-date and
                  fg-rcpth.trans-date             eq ?))
          use-index i-no no-lock,
       
          each fg-rdtlh
          where fg-rdtlh.r-no      eq fg-rcpth.r-no
            and fg-rdtlh.rita-code eq fg-rcpth.rita-code
          no-lock:
      
        create report.
        assign
         report.term-id = v-term
         report.key-01  = fg-rdtlh.loc
         report.key-02  = fg-rcpth.i-no
         report.key-03  = fg-rdtlh.loc-bin
         report.key-04  = fg-rdtlh.tag
         report.rec-id  = recid(fg-rdtlh).
      end.
    end.
    
    find first fg-rcpth
        where fg-rcpth.company eq cocode
          and fg-rcpth.i-no    gt v-i-no
        no-lock no-error.
  end.
  
  for each report where report.term-id eq v-term no-lock,
  
      first fg-rdtlh where recid(fg-rdtlh) eq report.rec-id no-lock,
      
      first fg-rcpth
      where fg-rdtlh.r-no      eq fg-rcpth.r-no
        and fg-rdtlh.rita-code eq fg-rcpth.rita-code
      no-lock
      
      break by report.key-01
            by report.key-02
            by report.key-03
            by report.key-04
      
      with frame itemx:

    if first-of(report.key-01) then do:             
      v-whse = fg-rdtlh.loc.
      
      if first(report.key-01) then do:
        hide frame r-top.
        view frame r-top.
        page.
      end.
        
      else put skip(3) "WHSE: " v-whse skip(1).
    end.                                                 

    assign
     v-fg-qty   = fg-rdtlh.qty
     v-fg-cost  = fg-rdtlh.cost * (v-fg-qty / 1000)
     v-fg-value = 0
     v-msf[1]   = 0
     v-msf[2]   = 0.

    release job-mat.

    if fg-rcpth.rita-code eq "R" then do:
      v-on = 1.

      find first job-hdr
           where job-hdr.company eq cocode
             and job-hdr.job-no  eq fg-rcpth.job-no
             and job-hdr.job-no2 eq fg-rcpth.job-no2
             and job-hdr.i-no    eq fg-rcpth.i-no
           use-index job-no no-lock no-error.

      /* For calculating the quantity per pallet. */
      if available job-hdr then do:
        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.job-no  eq job-hdr.job-no
              and fg-bin.job-no2 eq job-hdr.job-no2
              and fg-bin.i-no    eq job-hdr.i-no
              and fg-bin.loc-bin eq fg-rdtlh.loc-bin
              and fg-bin.tag     eq fg-rdtlh.tag
            no-lock no-error.

        v-qty-pallet = fg-rdtlh.cases * if avail fg-bin then
                                          fg-bin.cases-unit else 1.
      end.
      
      if avail job-hdr and job-hdr.e-num ne 0 then do:
        release ef.

        run sys/inc/numup.p (job-hdr.e-num, job-hdr.frm, output v-on).

        find first ef
            where ef.e-num   eq job-hdr.e-num
              and ef.form-no eq job-hdr.frm
            no-lock no-error.

        IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).

        if last-of(report.key-02) then           
        for each mch-act
            where mch-act.company  eq cocode
              and mch-act.job      eq job-hdr.job
              and mch-act.job-no   eq job-hdr.job-no
              and mch-act.job-no2  eq job-hdr.job-no2
              and mch-act.frm      eq job-hdr.frm
            use-index job no-lock:
          v-msf[2] = v-msf[2] + (mch-act.waste * job-hdr.sq-in / 100).
        end.

        for each job-mat
            where job-mat.company eq cocode
              and job-mat.job     eq job-hdr.job
              and job-mat.job-no  eq job-hdr.job-no
              and job-mat.job-no2 eq job-hdr.job-no2
              and job-mat.frm     eq job-hdr.frm
            no-lock,
            
            first item
            where item.company    eq cocode
              and item.i-no       eq job-mat.i-no
              and item.mat-type   eq "B"
            no-lock:
          leave.
        end.

        if avail job-mat then do:
          assign
           v-msf[1] = fg-rdtlh.qty / v-on * (job-mat.len * job-mat.wid)

           v-msf[2] = v-msf[2]      / v-on * (job-mat.len * job-mat.wid).

          if v-corr then
            assign
             v-msf[1] = v-msf[1] * .007
             v-msf[2] = v-msf[2] * .007.
          else
            assign
             v-msf[1] = v-msf[1] / 144
             v-msf[2] = v-msf[2] / 144.
        end.
      end.
    end.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq fg-rcpth.i-no
        use-index i-no no-lock no-error.
    if avail itemfg then do:
      find first uom
          where uom.uom  eq itemfg.sell-uom
            and uom.mult ne 0
          no-lock no-error.
      if avail uom then
        v-fg-value = itemfg.sell-price * (fg-rdtlh.qty / uom.mult).

      else
      if itemfg.sell-uom = "L" then
        v-fg-value = itemfg.sell-price * fg-rdtlh.qty.

      else
      if itemfg.sell-uom = "CS" then
        v-fg-value = itemfg.sell-price * (fg-rdtlh.qty / fg-rdtlh.qty-case).

      else
        v-fg-value = itemfg.sell-price * (fg-rdtlh.qty / 1000).

      if fg-rcpth.rita-code eq "R" then do:
        if v-msf[1] gt fg-rdtlh.qty * itemfg.t-sqft then
          v-msf[2] = v-msf[2] +
                     (v-msf[1] - (fg-rdtlh.qty * itemfg.t-sqft)).

        v-msf[1] = fg-rdtlh.qty * itemfg.t-sqft.
      end.
    end. /* avail itemfg */

    assign
     v-msf[1] = v-msf[1] / 1000
     v-msf[2] = v-msf[2] / 1000.

    if index("RTASE", fg-rcpth.rita-code) ne 0 then
      v-tran-type = entry(index("RTASE", fg-rcpth.rita-code),v-entrytype).
    else v-tran-type = "".

    if line-counter eq 56 then page.

    if fg-rcpth.po-no ne " " then
    find first po-ord
        where po-ord.company eq cocode
          and po-ord.po-no   eq int(fg-rcpth.po-no)
        no-lock no-error.                
 
    if fg-rdtlh.qty-case eq 0 then do:
      find first fg-bin
          where fg-bin.company eq cocode
            and fg-bin.job-no  eq fg-rcpth.job-no
            and fg-bin.job-no2 eq fg-rcpth.job-no2
            and fg-bin.i-no    eq fg-rcpth.i-no
            and fg-bin.loc     eq fg-rdtlh.loc
            and fg-bin.loc-bin eq fg-rdtlh.loc-bin
            and fg-bin.tag     eq fg-rdtlh.tag
          use-index job no-lock no-error.

      if avail fg-bin then
        assign
         v-cases    = trunc((v-fg-qty / fg-bin.case-count),0)
         v-qty-case = fg-bin.case-count.
         
      else do:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq fg-rcpth.i-no
            no-lock no-error.
        if avail itemfg then
          assign
           v-cases    = trunc((v-fg-qty / itemfg.case-count),0)
           v-qty-case = itemfg.case-count.
      end.
    end.
    
    else
      assign
       v-cases    = fg-rdtlh.cases
       v-qty-case = fg-rdtlh.qty-case.             

    display fg-rcpth.trans-date       when first-of(report.key-02)
            fg-rcpth.i-no             when first-of(report.key-02)
            fg-rcpth.i-name
            fg-rcpth.po-no                                                                   po-ord.vend-no            when avail po-ord
            v-tran-type
            fg-rdtlh.tag
            v-cases
            v-qty-case
            v-fg-qty            /* (sub-total by fg-rcpth.i-no) */
            fg-rdtlh.loc-bin
            fg-rcpth.pur-uom       
            v-fg-cost           /* (sub-total by fg-rcpth.i-no) */
                                                
        with frame itemx.
    down with frame itemx.
    
    if fg-rdtlh.rita-code eq "T" then
      put "To: " to 94 fg-rdtlh.loc2 fg-rdtlh.loc-bin2 skip.

    if v-pr-tots then do:                
      assign
       v-tot-qty = v-tot-qty + v-fg-qty
       v-tot-cost = v-tot-cost + v-fg-cost
       v-grd-tot-cost = v-grd-tot-cost + v-tot-cost       
       v-tot-value = v-tot-value + round(v-fg-value,2)
       v-msf[3] = v-msf[3] + v-msf[1]
       v-msf[4] = v-msf[4] + v-msf[2].

      if fg-rdtlh.rita-code eq "R" or
         fg-rdtlh.rita-code eq "A" or
         fg-rdtlh.rita-code eq "E" then
        v-cum-tot  = v-cum-tot + v-fg-cost.
      else
      if fg-rdtlh.rita-code eq "S" then
        v-cum-tot  = v-cum-tot - v-fg-cost.
    end.  /*   if v-pr-tots   */                                                                 
    if v-pr-tots then do:                                                              if last-of(report.key-02) then do:
        put "-----------" to 98 "----------" to 124 skip.                

        if fg-rcpth.rita-code eq "R" then
          put "MSF->  FG: " + trim(string(v-msf[3],">>,>>9.9<<")) +
              "  Wst: " + trim(string(v-msf[4],">>,>>9.9<<"))    +
              "  Tot: " + trim(string(v-msf[3] + v-msf[4],">>,>>9.9<<"))
                             format "x(63)" at 15.

        put v-tot-qty to 98 v-tot-cost to 124 skip(1).

        assign
         v-msf[5]   = v-msf[5] + v-msf[3]
         v-msf[6]   = v-msf[6] + v-msf[4]
         v-tot-qty  = 0
         v-tot-cost = 0
         v-msf[3]   = 0
         v-msf[4]   = 0.
      end.  /* if last-of(fg-rcpth.i-no) */        
    end. /* if v-pr-tots */
  end.

  if v-pr-tots then
    put " " to 124 skip       
        "MSF->  FG: " + trim(string(v-msf[5],">>,>>9.9<<")) +
        "  Wst: " + trim(string(v-msf[6],">>,>>9.9<<"))    +
        "  Tot: " + trim(string(v-msf[5] + v-msf[6],">>,>>9.9<<"))
                             format "x(63)" at 15
        "GRAND TOTAL COST:" to 105 v-grd-tot-cost to 124 skip .     
  

   

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

