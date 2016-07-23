&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Print Invoices

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

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

{ar/rep/invoice.i "new"}

DEF VAR v-program AS CHAR NO-UNDO.
{custom/xprint.i}
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-multi-faxout AS LOG NO-UNDO.
DEF VAR lv-fax-image AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_inv end_inv begin_cust end_cust ~
tb_reprint tb_posted rd-dest td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_inv end_inv begin_cust end_cust ~
tb_reprint tb_posted rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm 

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

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
     SIZE 94 BY 8.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.24.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Reprint Posted Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Invoices?" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_inv AT ROW 3.86 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Invoice Number"
     end_inv AT ROW 3.86 COL 70 COLON-ALIGNED HELP
          "Enter Ending Invoice Number"
     begin_cust AT ROW 4.81 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 4.81 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     tb_reprint AT ROW 6.71 COL 36
     tb_posted AT ROW 7.91 COL 66 RIGHT-ALIGNED
     rd-dest AT ROW 12.91 COL 7 NO-LABEL
     lv-ornt AT ROW 13.14 COL 31 NO-LABEL
     lines-per-page AT ROW 13.14 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.57 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 15.52 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 17.43 COL 30
     btn-ok AT ROW 20.76 COL 24
     btn-cancel AT ROW 20.76 COL 57
     RECT-6 AT ROW 11.48 COL 1
     RECT-7 AT ROW 1 COL 1
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 11.95 COL 5
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
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
         TITLE              = "Invoicing"
         HEIGHT             = 21.76
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_posted IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_posted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Invoicing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Invoicing */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv C-Win
ON LEAVE OF begin_inv IN FRAME FRAME-A /* Beginning Invoice# */
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
  DEF VAR lv-fax-type AS cha NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  DEF VAR ll-ans AS LOG INIT NO NO-UNDO.

  ASSIGN {&DISPLAYED-OBJECTS}.

  IF rd-dest EQ 1 AND v-print-fmt EQ "Harwell" THEN
    MESSAGE "Is this a laser printer?:" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-ans.
  IF ll-ans THEN lines-per-page = lines-per-page - 4.

  SESSION:SET-WAIT-STATE("general").

  lv-multi-faxout = IF rd-dest = 4 AND begin_cust <> END_cust THEN YES 
                   ELSE NO.
  IF is-xprint-form AND rd-dest = 4 THEN lv-multi-faxout = YES.

  lv-fax-type = IF lv-multi-faxout THEN "MULTI" ELSE "VENDOR".
  init-dir = "C:\tmp".
  lv-pdf-file = init-dir + (IF v-print-fmt EQ "Centbox" THEN "\CBXAR" ELSE "\AR")
               + string(begin_inv).

  run run-report. 

   
  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           IF lv-fax-type = "MULTI" /*AND is-xprint-form*/ THEN DO:
              RUN output-to-fax-prt. /* create tif file */              
              {custom/asifaxm.i &TYPE="MULTI"
                            &begin_cust=begin_cust
                            &END_cust=END_cust
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=lv-fax-image }      
           END.
           ELSE DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust 
                            &END_cust= begin_cust 
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
            END.
       END. 
       when 5 then do:
          IF is-xprint-form THEN  DO:
            
             RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

             {custom/asimail.i &TYPE="CUSTOMER"
                               &begin_cust=begin_cust
                               &END_cust=END_cust
                               &mail-subject= "Invoice"
                               &mail-body="Invoice"
                               &mail-file=list-name }
          END.
          ELSE DO:
              {custom/asimailR.i &TYPE="CUSTOMER"
                               &begin_cust=begin_cust
                               &END_cust=END_cust
                               &mail-subject="Invoice"
                               &mail-body="Invoice"
                               &mail-file=list-name }
          END.
       END.
       WHEN 6 THEN RUN output-to-port.
  end case. 
SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv C-Win
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
DO:
   assign {&self-name}.
   IF begin_inv = END_inv THEN DO:
     FIND FIRST ar-inv WHERE ar-inv.company = g_company
                         AND ar-inv.inv-no = begin_inv NO-LOCK NO-ERROR.
     IF AVAIL ar-inv THEN ASSIGN begin_cust:SCREEN-VALUE = ar-inv.cust-no
                                 end_cust:SCREEN-VALUE = ar-inv.cust-no.
   END. 
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


&Scoped-define SELF-NAME tb_posted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_posted C-Win
ON VALUE-CHANGED OF tb_posted IN FRAME FRAME-A /* Reprint Posted Invoices? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Invoices? */
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

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
  if not avail sys-ctrl then
  do transaction:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "INVPRINT"
     sys-ctrl.descrip = "Print Invoice Headers on Invoice Form".
    message sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  ASSIGN
   v-print-head = sys-ctrl.log-fld
   v-print-fmt  = sys-ctrl.char-fld.

  IF v-print-fmt EQ "Clev 1/2" THEN
    ASSIGN
     v-program      = "ar/rep/invhalfp.p"
     lines-per-page = 42.

  ELSE
  IF v-print-fmt EQ "TriState" THEN
    ASSIGN
     v-program      = "ar/rep/invhalfp.p"
     lines-per-page = 41.

  ELSE
  IF v-print-fmt EQ "1/2 Page" THEN
    ASSIGN
     v-program      = "ar/rep/invhalfp.p"
     lines-per-page = 44.

  ELSE
  IF v-print-fmt EQ "Livngstn" THEN
    ASSIGN
     v-program      = "ar/rep/invhalfp.p"
     lines-per-page = 66.

  ELSE
  IF v-print-fmt eq "PAC 1/2" THEN
    ASSIGN
     v-program      = "ar/rep/invpack.p"
     lines-per-page = 44.

  ELSE
  IF v-print-fmt EQ "Color" THEN
    ASSIGN
     v-program      = "ar/rep/color.p"
     lines-per-page = 60.

  ELSE
  IF v-print-fmt EQ "Phoenix" THEN
    ASSIGN
     v-program      = "ar/rep/invphx.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "Rudd" THEN
    ASSIGN
     v-program      = "ar/rep/invrudd.p"
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Premier" THEN
    ASSIGN
     v-program      = "ar/rep/invprem.p"
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Triad" THEN
    ASSIGN
     v-program      = "ar/rep/invtriad.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "Brick" THEN
    ASSIGN
     v-program      = "ar/rep/invbrick.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "Danbury" THEN
    ASSIGN
     v-program      = "ar/rep/invdnbry.p"
     lines-per-page = 41.

  ELSE
  IF v-print-fmt EQ "Sonoco" THEN
    ASSIGN
     v-program      = "ar/rep/invsono.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "Empire" THEN
    ASSIGN
     v-program      = "ar/rep/invempir.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "HOP" THEN
    ASSIGN
     v-program      = "ar/rep/invhop.p"
     lines-per-page = 42.

  ELSE
  IF v-print-fmt EQ "Allpkg" THEN
    ASSIGN
     v-program      = "ar/rep/invallpk.p"
     lines-per-page = 62.

  ELSE
  IF v-print-fmt EQ "MaxPak" THEN
    ASSIGN
     v-program      = "ar/rep/invmaxpk.p"
     lines-per-page = 42. 

  ELSE
  IF v-print-fmt EQ "Fibre" THEN
    ASSIGN
     v-program      = "ar/rep/invfibre.p"
     lines-per-page = 50.  

  ELSE
  IF v-print-fmt EQ "Abox" THEN
    ASSIGN
     v-program      = "ar/rep/invabox.p"
     lines-per-page = 60.

  ELSE
  IF v-print-fmt EQ "Harwell" THEN
    ASSIGN
     v-program      = "ar/rep/invharwl.p"
     lines-per-page = 63.

  ELSE
  IF v-print-fmt EQ "Chillic" THEN
    ASSIGN
     v-program      = "ar/rep/invchill.p"
     lines-per-page = 45.

  ELSE IF v-print-fmt EQ "Pacific" THEN
    ASSIGN
     v-program      = "ar/rep/invpacif.p"
     lines-per-page = 66
     is-xprint-form = YES.

  ELSE IF v-print-fmt EQ "Oracle" THEN
  ASSIGN
   v-program      = "ar/rep/invoracl.p"
   lines-per-page = 66
   is-xprint-form = YES.

  ELSE IF v-print-fmt EQ "Xprint" OR v-print-fmt EQ "ASIXprnt" THEN
    ASSIGN
     v-program      = "ar/rep/invxprnt.p"
     lines-per-page = 66
     is-xprint-form = YES.
  
  ELSE IF v-print-fmt EQ "midwest" THEN
    ASSIGN
     v-program      = "ar/rep/invmidws.p"
     lines-per-page = 66
     is-xprint-form = YES.

  ELSE IF v-print-fmt EQ "mwbox" THEN
    ASSIGN
     v-program      = "ar/rep/invmwbox.p"
     lines-per-page = 66
     is-xprint-form = YES.

  ELSE IF v-print-fmt EQ "Southpak" THEN
    ASSIGN
     v-program      = "ar/rep/invsthpk.p"
     lines-per-page = 66
     is-xprint-form = YES.

  ELSE IF v-print-fmt EQ "Hughes" THEN
    ASSIGN
     v-program      = "ar/rep/invhughs.p"
     lines-per-page = 66
     is-xprint-form = YES.

  ELSE IF v-print-fmt EQ "Concepts" THEN
      ASSIGN
       v-program      = "ar/rep/invxcorc.p"  /*Xprint format*/
       lines-per-page = 66
       is-xprint-form = YES.

  ELSE IF v-print-fmt = "Imperial" THEN
    ASSIGN
     v-program      = "ar/rep/invimper.p"
     lines-per-page = 62.

  ELSE IF v-print-fmt = "Herman" THEN
    ASSIGN
     v-program      = "ar/rep/invhermn.p"
     lines-per-page = 62.
      
  ELSE
    ASSIGN
     v-program      = "ar/rep/invasi.p"
     lines-per-page = 66.

  
  IF v-print-fmt = "BOXTECH" THEN lv-prt-bypass = YES.

  RUN enable_UI.
 /*
  DO WITH FRAME {&frame-name}:
    DISABLE lines-per-page.
  END.
   */
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    APPLY "entry" TO begin_inv.
    DISABLE lines-per-page.
  END.

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
  DISPLAY begin_inv end_inv begin_cust end_cust tb_reprint tb_posted rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_inv end_inv begin_cust end_cust tb_reprint tb_posted rd-dest 
         td-show-parm btn-ok btn-cancel RECT-6 RECT-7 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax-prt C-Win 
PROCEDURE output-to-fax-prt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv-file-name AS cha FORM "x(60)" NO-UNDO.
  DEF VAR lv-xpr-file AS cha FORM "x(60)" NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
     REPEAT:
        SET lv-file-name.
        IF lv-file-name <> "." AND lv-file-name <> ".."  AND lv-file-name MATCHES "*xpr*" 
        THEN DO:
             lv-xpr-file = "c:\temp\fax\" + lv-file-name.             
             RUN printfile (lv-xpr-file).
        END.
        lv-file-name = "".   
     END.
     
  END.
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
     /*
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
     DEF VAR lv-save-to AS cha NO-UNDO.

     if init-dir = "" then init-dir = "c:\temp" .
     lv-save-to = list-name.
     SYSTEM-DIALOG GET-FILE lv-save-to
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
     OS-COPY VALUE(list-name) VALUE(lv-save-to).
     */
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
  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN DO:
       RUN custom/d-print.w (list-name).
  END.
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt). 
  

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
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
 END.
 ELSE /*run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ */
    run custom/scr-rpt2.w (list-name,c-win:title,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ ar/rep/invoice.p  9/94 RM */
/* PRINT INVOICE - A/R MODULE                                                 */
/* -------------------------------------------------------------------------- */
DEF VAR lv-copy# AS INT NO-UNDO.

{sys/form/r-top.i}

ASSIGN
 finv       = begin_inv
 tinv       = end_inv
 fcust      = begin_cust
 tcust      = end_cust
 v-print    = tb_reprint
 v-posted   = tb_posted.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

{sa/sa-sls01.i}

v-term-id = v-term.

 SESSION:SET-WAIT-STATE ("general").

FOR EACH ar-inv
    WHERE ar-inv.company                EQ cocode

      AND ar-inv.inv-no                 GE finv
      AND ar-inv.inv-no                 LE tinv

      AND ar-inv.cust-no                GE fcust
      AND ar-inv.cust-no                LE tcust

      AND (ar-inv.posted                EQ NO OR
           ar-inv.posted                EQ v-posted)

      AND ar-inv.printed                EQ v-print
      AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no /*AND ar-invl.amt <> 0*/ )
    USE-INDEX inv-no NO-LOCK:
      
  CREATE report.
  ASSIGN
   report.term-id = v-term-id
   report.key-01  = STRING(ar-inv.inv-no,"9999999999")
   report.rec-id  = RECID(ar-inv).
END.

v-lines-per-page = lines-per-page.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVCOPYS" NO-LOCK NO-ERROR.
lv-copy# = IF AVAIL sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.

IF is-xprint-form THEN DO:
   CASE rd-dest :
        WHEN 1 THEN PUT "<COPIES=" + string(lv-copy#) + "><PRINTER?>" FORM "x(30)".
        WHEN 2 THEN PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
        WHEN 5 THEN PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(60)".  
   END CASE.
END.

RUN VALUE(v-program).

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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

  PAGE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

