&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date ~
begin_accnt end_accnt tb_cashr tb_apckr tb_general tb_arinv tb_mcshrec ~
tb_cdisb tb_apmem tb_crmem tb_acpay tb_apvoidck tb_ap-purch tb_oeinv ~
rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_accnt end_accnt ~
lbl_cashr tb_cashr lbl_apckr tb_apckr lbl_general tb_general lbl_arinv ~
tb_arinv lbl_mcshrec tb_mcshrec lbl_cdisb tb_cdisb lbl_apmem tb_apmem ~
lbl_crmem tb_crmem lbl_acpay tb_acpay lbl_apvoidck tb_apvoidck lbl_ap-purch ~
tb_ap-purch lbl_oeinv tb_oeinv rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm 

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

DEFINE VARIABLE begin_accnt AS CHARACTER FORMAT "X(25)":U 
     LABEL "Beginning Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_accnt AS CHARACTER FORMAT "X(25)":U INITIAL "zzzzzzzzzzzzzzzzzzzzzzzzz" 
     LABEL "Ending Acct#" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_acpay AS CHARACTER FORMAT "X(256)":U INITIAL "Accounts Payable" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_ap-purch AS CHARACTER FORMAT "X(256)":U INITIAL "Accounts Payable Purchases" 
     VIEW-AS FILL-IN 
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_apckr AS CHARACTER FORMAT "X(256)":U INITIAL "Accounts Payable Check Register" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_apmem AS CHARACTER FORMAT "X(256)":U INITIAL "Accounts Payable Memo" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_apvoidck AS CHARACTER FORMAT "X(256)":U INITIAL "Accounts Payable Void Check" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_arinv AS CHARACTER FORMAT "X(256)":U INITIAL "Accounts Receivable Invoice" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_cashr AS CHARACTER FORMAT "X(256)":U INITIAL "Cash Receipts" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_cdisb AS CHARACTER FORMAT "X(256)":U INITIAL "Cash Disbursements" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_crmem AS CHARACTER FORMAT "X(256)":U INITIAL "Credit Memo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_general AS CHARACTER FORMAT "X(256)":U INITIAL "General" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_mcshrec AS CHARACTER FORMAT "X(256)":U INITIAL "Misc Cash Receipts" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_oeinv AS CHARACTER FORMAT "X(256)":U INITIAL "Order Entry Invoice" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .95 NO-UNDO.

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 92 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 10.71.

DEFINE VARIABLE tb_acpay AS LOGICAL INITIAL no 
     LABEL "Accounts Payable" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_ap-purch AS LOGICAL INITIAL no 
     LABEL "Accounts Payable Purchases" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_apckr AS LOGICAL INITIAL no 
     LABEL "Accounts Payable Check Register" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_apmem AS LOGICAL INITIAL no 
     LABEL "Accounts Payable Memo" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_apvoidck AS LOGICAL INITIAL no 
     LABEL "Accounts Payable Void Check" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_arinv AS LOGICAL INITIAL no 
     LABEL "Accounts Receivable Invoice" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cashr AS LOGICAL INITIAL no 
     LABEL "Cash Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cdisb AS LOGICAL INITIAL no 
     LABEL "Cash Disbursements" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_crmem AS LOGICAL INITIAL no 
     LABEL "Credit Memo" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_general AS LOGICAL INITIAL no 
     LABEL "General" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY 1.05 NO-UNDO.

DEFINE VARIABLE tb_mcshrec AS LOGICAL INITIAL no 
     LABEL "Misc Cash Receipts" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .95 NO-UNDO.

DEFINE VARIABLE tb_oeinv AS LOGICAL INITIAL no 
     LABEL "Order Entry Invoice" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.8 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_date AT ROW 2.43 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 2.43 COL 64 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_accnt AT ROW 3.38 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Account Number"
     end_accnt AT ROW 3.38 COL 64 COLON-ALIGNED HELP
          "Enter Ending Account Number"
     lbl_cashr AT ROW 5.05 COL 20 COLON-ALIGNED NO-LABEL
     tb_cashr AT ROW 5.05 COL 38
     lbl_apckr AT ROW 5.05 COL 47 COLON-ALIGNED NO-LABEL
     tb_apckr AT ROW 5.05 COL 84
     lbl_general AT ROW 6 COL 26 COLON-ALIGNED NO-LABEL
     tb_general AT ROW 6 COL 38
     lbl_arinv AT ROW 6 COL 51 COLON-ALIGNED NO-LABEL
     tb_arinv AT ROW 6 COL 84
     lbl_mcshrec AT ROW 6.95 COL 15 COLON-ALIGNED NO-LABEL
     tb_mcshrec AT ROW 6.95 COL 38
     lbl_cdisb AT ROW 6.95 COL 60 COLON-ALIGNED NO-LABEL
     tb_cdisb AT ROW 6.95 COL 84
     lbl_apmem AT ROW 7.91 COL 10 COLON-ALIGNED NO-LABEL
     tb_apmem AT ROW 7.91 COL 38
     lbl_crmem AT ROW 7.91 COL 67 COLON-ALIGNED NO-LABEL
     tb_crmem AT ROW 7.91 COL 84
     lbl_acpay AT ROW 8.86 COL 16 COLON-ALIGNED NO-LABEL
     tb_acpay AT ROW 8.86 COL 38
     lbl_apvoidck AT ROW 8.86 COL 51 COLON-ALIGNED NO-LABEL
     tb_apvoidck AT ROW 8.86 COL 84
     lbl_ap-purch AT ROW 9.81 COL 6 COLON-ALIGNED NO-LABEL
     tb_ap-purch AT ROW 9.81 COL 38
     lbl_oeinv AT ROW 9.81 COL 61 COLON-ALIGNED NO-LABEL
     tb_oeinv AT ROW 9.81 COL 84
     rd-dest AT ROW 12.91 COL 8 NO-LABEL
     lv-ornt AT ROW 12.91 COL 31 NO-LABEL
     lines-per-page AT ROW 12.91 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 14.81 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 15.76 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 18.38 COL 31
     btn-ok AT ROW 20.05 COL 20
     btn-cancel AT ROW 20.05 COL 59
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.19 COL 6
     RECT-6 AT ROW 11.95 COL 3
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 21.52.


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
         TITLE              = "Journals By GL Account Number"
         HEIGHT             = 21.81
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       begin_accnt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_accnt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_acpay IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_ap-purch IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_apckr IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_apmem IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_apvoidck IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_arinv IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_cashr IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_cdisb IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_crmem IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_general IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_general:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lbl_mcshrec IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_oeinv IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       tb_acpay:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ap-purch:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_apckr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_apmem:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_apvoidck:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_arinv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cashr:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cdisb:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_crmem:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_general:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_mcshrec:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_oeinv:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Journals By GL Account Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Journals By GL Account Number */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_accnt C-Win
ON LEAVE OF begin_accnt IN FRAME FRAME-A /* Beginning Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  run run-report. 

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_date
                            &END_cust=begin_date
                            &fax-subject="Journals By GL Account Number"
                            &fax-body="Journals By GL Account Number"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject="Journals By GL Account Number"
                             &mail-body="Journals By GL Account Number"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject="Journals By GL Account Number"
                                  &mail-body="Journals By GL Account Number"
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_accnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_accnt C-Win
ON LEAVE OF end_accnt IN FRAME FRAME-A /* Ending Acct# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME tb_acpay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_acpay C-Win
ON VALUE-CHANGED OF tb_acpay IN FRAME FRAME-A /* Accounts Payable */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ap-purch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ap-purch C-Win
ON VALUE-CHANGED OF tb_ap-purch IN FRAME FRAME-A /* Accounts Payable Purchases */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_apckr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_apckr C-Win
ON VALUE-CHANGED OF tb_apckr IN FRAME FRAME-A /* Accounts Payable Check Register */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_apmem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_apmem C-Win
ON VALUE-CHANGED OF tb_apmem IN FRAME FRAME-A /* Accounts Payable Memo */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_apvoidck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_apvoidck C-Win
ON VALUE-CHANGED OF tb_apvoidck IN FRAME FRAME-A /* Accounts Payable Void Check */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_arinv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_arinv C-Win
ON VALUE-CHANGED OF tb_arinv IN FRAME FRAME-A /* Accounts Receivable Invoice */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cashr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cashr C-Win
ON VALUE-CHANGED OF tb_cashr IN FRAME FRAME-A /* Cash Receipts */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_cdisb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cdisb C-Win
ON VALUE-CHANGED OF tb_cdisb IN FRAME FRAME-A /* Cash Disbursements */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_crmem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_crmem C-Win
ON VALUE-CHANGED OF tb_crmem IN FRAME FRAME-A /* Credit Memo */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_general
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_general C-Win
ON VALUE-CHANGED OF tb_general IN FRAME FRAME-A /* General */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_mcshrec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_mcshrec C-Win
ON VALUE-CHANGED OF tb_mcshrec IN FRAME FRAME-A /* Misc Cash Receipts */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_oeinv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_oeinv C-Win
ON VALUE-CHANGED OF tb_oeinv IN FRAME FRAME-A /* Order Entry Invoice */
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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
   begin_date = date(month(today),1,year(today))
   end_date   = today.

  RUN enable_UI.

  {methods/nowait.i}

   DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    APPLY "entry" TO begin_accnt.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY begin_date end_date begin_accnt end_accnt lbl_cashr tb_cashr lbl_apckr 
          tb_apckr lbl_general tb_general lbl_arinv tb_arinv lbl_mcshrec 
          tb_mcshrec lbl_cdisb tb_cdisb lbl_apmem tb_apmem lbl_crmem tb_crmem 
          lbl_acpay tb_acpay lbl_apvoidck tb_apvoidck lbl_ap-purch tb_ap-purch 
          lbl_oeinv tb_oeinv rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_date end_date begin_accnt end_accnt tb_cashr 
         tb_apckr tb_general tb_arinv tb_mcshrec tb_cdisb tb_apmem tb_crmem 
         tb_acpay tb_apvoidck tb_ap-purch tb_oeinv rd-dest lv-ornt 
         lines-per-page lv-font-no td-show-parm btn-ok btn-cancel 
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
RUN custom/dprint.w (list-name).

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
/***************************************************************************\
*****************************************************************************
**  Program: ap/rep/pjgl.p
**       By: Chris Heins
** Descript: CASH DISBURSEMENT / VOUCHER REGISTER BY GL ACCT
**
*****************************************************************************
\***************************************************************************/

{sys/form/r-topw.f}

DEFINE VARIABLE lo_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "From Date".
DEFINE VARIABLE hi_trandate AS DATE FORMAT "99/99/9999" NO-UNDO LABEL "Thru Date".
DEFINE VARIABLE DEBUG AS LOGICAL NO-UNDO INITIAL TRUE.
DEFINE VARIABLE ws_disc LIKE ap-payl.amt-disc COLUMN-LABEL "Discount" NO-UNDO.
DEFINE VARIABLE ws_check-no LIKE ap-chk.check-no NO-UNDO format ">>>>>>>"
    column-label "Check#".
DEFINE VARIABLE ws_order-no LIKE oe-ord.ord-no NO-UNDO
    format ">>>>>>".
def var ws_jrnl like gltrans.jrnl column-label "Journal" no-undo.
DEF VAR GL_JRNL_LIST AS CHAR NO-UNDO.

def var lo_actnum like account.actnum label "From GL Acct#" no-undo.
def var hi_actnum like account.actnum label "Thru GL Acct#" no-undo.

  DEFINE VARIABLE t-amt AS DECIMAL NO-UNDO.
  DEFINE VARIABLE t-disc AS DECIMAL NO-UNDO.
  DEF VAR t-qty as decimal no-undo.
  def var t-msf as decimal no-undo.
    DEFINE VARIABLE hdg_printed AS LOGICAL NO-UNDO.

form
        ws_jrnl
        ap-inv.vend-no    COLUMN-LABEL "Vendor"
        vend.name
        ap-inv.inv-date COLUMN-LABEL "Date"
        ap-inv.inv-no COLUMN-LABEL "Invoice#"
        ws_check-no
        ws_order-no
        ap-invl.qty
        ap-invl.amt-msf
        ws_disc
        ap-invl.amt
        WITH FRAME f-det width 144 DOWN STREAM-IO.


assign
 str-tit2 = c-win:title 
 {sys/inc/ctrtext.i str-tit2 112}

 lo_actnum    = begin_accnt
 hi_actnum    = end_accnt
 lo_trandate  = begin_date
 hi_trandate  = end_date
 gl_jrnl_list = (if tb_cashr    then "CASHR,"    else "") +
                (if tb_general  then "GENERAL,"  else "") +
                (if tb_mcshrec  then "MCSHREC,"  else "") +
                (if tb_apmem    then "APMEM,"    else "") +
                (if tb_acpay    then "ACPAY,"    else "") +
                (if tb_ap-purch then "AP-PURCH," else "") +
                (if tb_apckr    then "APCHR,"    else "") +
                (if tb_arinv    then "ARINV,"    else "") +
                (if tb_cdisb    then "CDISB,"    else "") +
                (if tb_crmem    then "CRMEM,"    else "") +
                (if tb_apvoidck then "APVOIDCK," else "") +
                (if tb_oeinv    then "OEINV,"    else "").


{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

DISPLAY "" WITH FRAME r-top.

SESSION:SET-WAIT-STATE ("general").

  FOR EACH account NO-LOCK
      WHERE account.company = cocode
      and account.actnum >= lo_actnum
      and account.actnum <= hi_actnum:

    if line-counter >= (page-size - 2) then do:
        page.
        view frame f-det.
        down 0 with frame f-det.
    end.

    ASSIGN
      hdg_printed = FALSE
      t-amt = 0
      t-disc = 0
      t-msf = 0
      t-qty = 0
      ws_disc = 0
      ws_jrnl = ''
      ws_check-no = 0
      ws_order-no = 0.

    VIEW FRAME F-DET.
    DOWN 0 WITH FRAME F-DET.

    FOR EACH gltrans NO-LOCK
        WHERE gltrans.company = cocode
        AND gltrans.actnum = account.actnum
        AND gltrans.tr-date >= lo_trandate
        AND gltrans.tr-date <= hi_trandate
        AND CAN-DO(GL_JRNL_LIST, gltrans.jrnl)
        BY gltrans.tr-date:
      IF NOT hdg_printed THEN
      DO:
        PUT SKIP account.actnum ' - '
          account.dscr
          SKIP.
        hdg_printed = TRUE.
      END.
      DISPLAY
        gltrans.jrnl @ ws_jrnl
        gltrans.tr-dscr @ vend.name
        gltrans.tr-date @ ap-inv.inv-date
        gltrans.tr-amt  @ ap-invl.amt
      with frame f-det.
      down 1 with frame f-det.
      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + gltrans.tr-amt.
    END.

    FOR EACH glhist NO-LOCK
        WHERE glhist.company = cocode
        AND glhist.actnum = account.actnum
        AND glhist.tr-date >= lo_trandate
        AND glhist.tr-date <= hi_trandate
        AND CAN-DO(GL_JRNL_LIST, glhist.jrnl)
        BY glhist.tr-date:
      IF NOT hdg_printed THEN
      DO:
        PUT SKIP account.actnum ' - '
          account.dscr
          SKIP.
        hdg_printed = TRUE.
      END.
      DISPLAY
        glhist.jrnl @ ws_jrnl
        glhist.tr-dscr @ vend.name
        glhist.tr-date @ ap-inv.inv-date
        glhist.tr-amt  @ ap-invl.amt
      with frame f-det.
      down 1 with frame f-det.
      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + glhist.tr-amt.
    END.

/*
      ws_jrnl = "AP-DIS".
*/
/* Commented out for duplicatation of AP-PURCH and ACPAY and non posted CDISB

      ws_jrnl = "CDISB".
    FOR EACH ap-disl NO-LOCK
        WHERE ap-disl.company = cocode
        AND ap-disl.actnum = account.actnum,
        EACH ap-dis NO-LOCK
        WHERE ap-dis.d-no = ap-disl.d-no
        AND ap-dis.check-date >= lo_trandate
        AND ap-dis.check-date <= hi_trandate
        BY ap-dis.check-date BY ap-dis.check-no:
      IF NOT hdg_printed THEN
      DO:
        PUT SKIP account.actnum ' - '
          account.dscr
          SKIP.
        hdg_printed = TRUE.
      END.

      FIND vend OF ap-dis NO-LOCK NO-ERROR.
      ws_disc = 0.
      ws_check-no = ap-dis.check-no.
      DISPLAY
        ws_jrnl
        ap-dis.vend-no    @ ap-inv.vend-no
        vend.name WHEN AVAILABLE vend
        ap-dis.check-date @ ap-inv.inv-date
        ws_check-no
        ws_order-no
        ap-disl.qty WHEN ap-disl.qty <> 0 @ ap-invl.qty
        ap-disl.amt @ ap-invl.amt
        WITH FRAME f-det.
      down 1 with frame f-det.
      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + ap-disl.amt
        t-qty = t-qty + ap-disl.qty.
    END.    /* ap-disl */

*/


if can-do(GL_JRNL_LIST, "AP-PURCH") then
do:
    ws_jrnl = "AP-PURCH".
    ws_check-no = 0.
    FOR EACH ap-invl NO-LOCK
        WHERE ap-invl.company = cocode
        AND ap-invl.actnum = account.actnum,
        EACH ap-inv NO-LOCK
        WHERE ap-inv.i-no = ap-invl.i-no
        AND ap-inv.inv-date >= lo_trandate
        AND ap-inv.inv-date <= hi_trandate
        BY ap-inv.inv-date BY ap-inv.inv-no:
      IF NOT hdg_printed THEN
      DO:
        PUT SKIP account.actnum ' - '
          account.dscr
          SKIP.
        hdg_printed = TRUE.
      END.

      FIND vend OF ap-inv NO-LOCK NO-ERROR.
      ws_disc = ap-invl.amt * (ap-inv.disc-% / 100).
      ws_order-no = ap-inv.po-no.
      DISPLAY
        ws_jrnl
        ap-inv.vend-no
        vend.name WHEN AVAILABLE vend
        ap-inv.inv-date
        ap-inv.inv-no
        ws_check-no
        ws_order-no
        ap-invl.qty WHEN ap-invl.qty <> 0
        ap-invl.amt-msf WHEN ap-invl.amt-msf <> 0
        ws_disc WHEN ws_disc <> 0
        ap-invl.amt
        WITH FRAME f-det.
      down 1 with frame f-det.
      ASSIGN t-disc = t-disc + ws_disc
        t-amt = t-amt + ap-invl.amt
        t-qty = t-qty + ap-invl.qty
        t-msf = t-msf + ap-invl.amt-msf.
    END.    /* ap-invl */
end.

    IF NOT hdg_printed THEN
    NEXT.   /* inactive account */

    do with frame f-det:
    UNDERLINE
      ap-invl.qty
      ap-invl.amt-msf
      ap-invl.amt
      ws_disc.
    DOWN 1.
    DISPLAY
      /* "* ACCOUNT TOTAL *" @ vend.name */
      t-disc @ ws_disc
      t-amt  @ ap-invl.amt
      t-qty  @ ap-invl.qty
      t-msf  @ ap-invl.amt-msf.
     /* down 1. */
    end.
  END.  /* for each account */

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE ("").

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

