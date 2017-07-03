&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Cost Estimating Control File

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

DEF INPUT PARAM ip-job-no AS cha NO-UNDO.
DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.

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
 cocode = g_company
 locode = g_loc.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLJobTicket &Company=cocode} /* rstark 05181205 */

DEF NEW SHARED VAR v-dept-log AS LOG NO-UNDO.
DEF NEW SHARED VAR v-dept-codes AS CHAR NO-UNDO.
DEF NEW SHARED VAR lv-qty AS int NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR lv-format-c AS CHAR NO-UNDO.
DEF VAR lv-default-f AS CHAR NO-UNDO.
DEF VAR lv-default-c AS CHAR NO-UNDO.
DEF VAR lv-int-f AS INT NO-UNDO.
DEF VAR lv-int-c AS INT NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

{jcrep/r-ticket.i "new shared"}
{custom/xprint.i}

DEF NEW SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-revno AS LOG NO-UNDO.
DEF NEW SHARED VAR revision-no AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF NEW SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF NEW SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF NEW SHARED VAR s-committed-board-only AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF NEW SHARED VAR s-sample-required AS LOG NO-UNDO.
DEF NEW SHARED VAR s-show-release AS LOG NO-UNDO.


DEF VAR lv-save-spec AS CHAR NO-UNDO.
DEF VAR v-freezenotes-log AS LOG NO-UNDO.

{cerep/jc-keyst.i "NEW"}
{cerep/jc-keys2.i "NEW"}
{cecrep/jc-prem.i "NEW"}
{cecrep/jc-fibre.i "NEW"}
{cecrep/jc-pallet.i "NEW"}
{cecrep/jc-soule.i "NEW"}
/*{cecrep/tt-artios.i "NEW"}*/
{cerep/tt-samp-ctn.i "NEW"}

 DEF TEMP-TABLE t-ef-form
    FIELD form-no LIKE ef.form-no.

DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF NEW SHARED VAR s-prt-ship-split AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-label AS LOG NO-UNDO.

{ cerep/tt-wrk-ink.i "NEW SHARED" }

/* gdm - 10010805 */
DEF TEMP-TABLE tt-specCd NO-UNDO
    FIELD tt-char-val AS CHAR
    INDEX chr-1 tt-char-val.
/* gdm - 11030807 */
DEF NEW SHARED VAR v-newdie   AS LOG NO-UNDO INIT FALSE.
DEF NEW SHARED VAR v-newfilm  AS LOG NO-UNDO INIT FALSE.
DEF NEW SHARED VAR v-newcombo AS LOG NO-UNDO INIT FALSE.
DEF BUFFER b-reftable-freeze FOR reftable.
DEF BUFFER b-reftable-split FOR reftable.
DEF VAR li AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job1 begin_job2 end_job1 ~
end_job2 tb_fold tb_show-rel tb_RS tb_corr tb_PR tb_reprint tb_DC tb_box ~
tb_GL tb_SW tb_approve spec_codes revsn_no tb_prt-label tb_committed ~
tb_prt-set-header tb_prompt-ship TB_sample_req tb_dept-note dept_codes ~
tb_freeze-note rd-dest lines-per-page lv-ornt lv-font-no td-show-parm ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job1 begin_job2 end_job1 end_job2 ~
tb_fold tb_show-rel tb_RS tb_corr tb_PR tb_reprint tb_DC tb_box tb_GL ~
tb_fgimage tb_SW spec_codes tb_prt-rev revsn_no tb_prt-mch rd_print-speed ~
tb_prt-shipto tb_prt-sellprc tb_prt-label tb_committed tb_prt-set-header ~
tb_prompt-ship TB_sample_req tb_dept-note dept_codes tb_freeze-note rd-dest ~
lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_job1 begin_job2 end_job1 end_job2 tb_reprint ~
tb_box tb_fgimage tb_approve tb_tray-2 tb_make_hold tb_draft ~
tb_app-unprinted tb_prt-rev tb_prt-mch tb_prt-shipto tb_prt-sellprc ~
tb_prt-label td-show-parm 

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

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "x(6)" 
     LABEL "Beginning  Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE dept_codes AS CHARACTER FORMAT "X(256)":U INITIAL "QA" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "x(6)" INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">9" INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

DEFINE VARIABLE fl-jobord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 35.8 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE revsn_no AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE spec_codes AS CHARACTER FORMAT "X(256)":U INITIAL "QA" 
     LABEL "Spec Codes" 
     VIEW-AS FILL-IN 
     SIZE 57 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 24.6 BY 1.48 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 5.95 NO-UNDO.

DEFINE VARIABLE rd_print-speed AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Speed", "S",
"Run Hour", "H"
     SIZE 24.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 7.62.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 18.33.

DEFINE VARIABLE tb_app-unprinted AS LOGICAL INITIAL no 
     LABEL "Print All Unprinted App. Tickets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81.

DEFINE VARIABLE tb_approve AS LOGICAL INITIAL no 
     LABEL "Approve Job(s)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_box AS LOGICAL INITIAL yes 
     LABEL "Print Box Design?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tb_committed AS LOGICAL INITIAL no 
     LABEL "Print Only Committed Board?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no 
     LABEL "Corrugated" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_DC AS LOGICAL INITIAL no 
     LABEL "Print &Die Cutter Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_dept-note AS LOGICAL INITIAL no 
     LABEL "Departments?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_draft AS LOGICAL INITIAL no 
     LABEL "Mark as Draft?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81.

DEFINE VARIABLE tb_fgimage AS LOGICAL INITIAL no 
     LABEL "Print FG Item Image?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fold AS LOGICAL INITIAL no 
     LABEL "Folding Carton" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tb_freeze-note AS LOGICAL INITIAL no 
     LABEL "Freeze Job Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tb_GL AS LOGICAL INITIAL no 
     LABEL "Print &Gluer /  Window Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_make_hold AS LOGICAL INITIAL no 
     LABEL "Print Make & Hold?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE tb_PR AS LOGICAL INITIAL no 
     LABEL "Print &Printer Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prompt-ship AS LOGICAL INITIAL no 
     LABEL "Prompt Split Shipment or Split Order?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-label AS LOGICAL INITIAL no 
     LABEL "Print Label Info?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-mch AS LOGICAL INITIAL no 
     LABEL "Print Machine Standard?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-rev AS LOGICAL INITIAL no 
     LABEL "Print Revision Number?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-sellprc AS LOGICAL INITIAL no 
     LABEL "Print Sell Price in place of UPC#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-set-header AS LOGICAL INITIAL no 
     LABEL "Print Set Unitization Page?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-shipto AS LOGICAL INITIAL no 
     LABEL "Print Shipto?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no 
     LABEL "Reprint Tickets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE tb_RS AS LOGICAL INITIAL no 
     LABEL "Print &Sheeter Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE TB_sample_req AS LOGICAL INITIAL no 
     LABEL "Sample(s) Required?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_show-rel AS LOGICAL INITIAL no 
     LABEL "Print by Release Lines" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_SW AS LOGICAL INITIAL no 
     LABEL "Print Shrink &Wrap Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_tray-2 AS LOGICAL INITIAL no 
     LABEL "Copy 2 and 3 in Tray 2 (Artios)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_job1 AT ROW 3.14 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job2 AT ROW 3.14 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Run#"
     end_job1 AT ROW 3.14 COL 55.2 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     end_job2 AT ROW 3.14 COL 71.2 COLON-ALIGNED HELP
          "Enter Ending Run#"
     tb_fold AT ROW 4.33 COL 21
     tb_show-rel AT ROW 4.33 COL 57.2 WIDGET-ID 16
     tb_RS AT ROW 4.33 COL 57.2
     tb_corr AT ROW 5.33 COL 21
     tb_PR AT ROW 5.33 COL 57.2
     tb_reprint AT ROW 6.33 COL 21
     tb_DC AT ROW 6.33 COL 57.2
     tb_box AT ROW 7.33 COL 42 RIGHT-ALIGNED
     tb_GL AT ROW 7.33 COL 57.2
     tb_fgimage AT ROW 8.33 COL 47 RIGHT-ALIGNED
     tb_SW AT ROW 8.33 COL 57.2
     tb_approve AT ROW 9.33 COL 47 RIGHT-ALIGNED
     tb_tray-2 AT ROW 9.33 COL 91 RIGHT-ALIGNED WIDGET-ID 6
     tb_make_hold AT ROW 10.29 COL 91 RIGHT-ALIGNED WIDGET-ID 12
     tb_draft AT ROW 10.33 COL 56 RIGHT-ALIGNED WIDGET-ID 18
     tb_app-unprinted AT ROW 10.33 COL 56 RIGHT-ALIGNED WIDGET-ID 10
     spec_codes AT ROW 11.24 COL 18.2 COLON-ALIGNED
     tb_prt-rev AT ROW 12.29 COL 20
     revsn_no AT ROW 12.29 COL 49 COLON-ALIGNED
     tb_prt-mch AT ROW 13.19 COL 20
     fl-jobord AT ROW 13.19 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rd_print-speed AT ROW 14.1 COL 56.8 NO-LABEL
     tb_prt-shipto AT ROW 15.24 COL 20
     tb_prt-sellprc AT ROW 15.24 COL 56
     tb_prt-label AT ROW 16.19 COL 20
     tb_committed AT ROW 16.19 COL 56
     tb_prt-set-header AT ROW 17.14 COL 20
     tb_prompt-ship AT ROW 17.14 COL 56
     TB_sample_req AT ROW 17.95 COL 56 WIDGET-ID 2
     tb_dept-note AT ROW 18 COL 56
     dept_codes AT ROW 18 COL 79 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     tb_freeze-note AT ROW 18.1 COL 20
     rd-dest AT ROW 20.62 COL 20.6 NO-LABEL
     lines-per-page AT ROW 20.62 COL 54.6 COLON-ALIGNED
     lv-ornt AT ROW 21.76 COL 56.6 NO-LABEL
     lv-font-no AT ROW 24.1 COL 54.6 COLON-ALIGNED
     lv-font-name AT ROW 24.1 COL 58.6 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 25.76 COL 56.6
     btn-ok AT ROW 27.38 COL 26
     btn-cancel AT ROW 27.38 COL 57
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.48 COL 3
          BGCOLOR 2 
     "Print Machine's Speed or Run Hour ?" VIEW-AS TEXT
          SIZE 36.6 BY .62 AT ROW 14.24 COL 20.4
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 19.67 COL 4
     RECT-6 AT ROW 19.57 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.4 BY 27.95.


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
         TITLE              = "Job Ticket"
         HEIGHT             = 27.95
         WIDTH              = 97.2
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FILL-IN begin_job1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_job1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN begin_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       begin_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job1 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_job1:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN end_job2 IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       end_job2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN fl-jobord IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fl-jobord:HIDDEN IN FRAME FRAME-A           = TRUE
       fl-jobord:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lv-font-no:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR RADIO-SET rd_print-speed IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       revsn_no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_app-unprinted IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_app-unprinted:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_app-unprinted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_approve IN FRAME FRAME-A
   NO-DISPLAY ALIGN-R 1                                                 */
ASSIGN 
       tb_approve:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_approve:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_box IN FRAME FRAME-A
   ALIGN-R 1                                                            */
ASSIGN 
       tb_box:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_committed:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_DC:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_draft IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_draft:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_draft:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_fgimage IN FRAME FRAME-A
   NO-ENABLE ALIGN-R 1                                                  */
ASSIGN 
       tb_fgimage:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_GL:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_make_hold IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_make_hold:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_make_hold:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_PR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_prompt-ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-label IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       tb_prt-label:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-mch IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-mch:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-rev IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-rev:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-sellprc IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-sellprc:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-set-header:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-shipto IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-shipto:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_reprint IN FRAME FRAME-A
   1                                                                    */
ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_RS:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_show-rel:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tb_SW:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_tray-2 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_tray-2:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_tray-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Ticket */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Ticket */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF lv-format-f = 'Indiana-XL' AND tb_fold:CHECKED IN FRAME {&FRAME-NAME} THEN
     run CleanUp. 
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON RETURN OF FRAME FRAME-A
ANYWHERE
DO:

   IF SELF:TYPE <> "Button" THEN  do:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
   ELSE do:
       APPLY "choose" TO self.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 C-Win
ON LEAVE OF begin_job1 IN FRAME FRAME-A /* Beginning  Job# */
DO:
  IF {&self-name}:MODIFIED THEN RUN new-job-no.
  ASSIGN {&self-name}.

  IF lv-format-f = "FibreFC" AND lv-format-c = "Artios" THEN
     RUN split-ship-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A /* - */
DO:
  IF {&self-name}:MODIFIED THEN RUN new-job-no.
  ASSIGN {&self-name}.

  IF lv-format-f = "FibreFC" AND lv-format-c = "Artios" THEN
     RUN split-ship-proc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
  IF lv-format-f = 'Indiana-XL' AND tb_fold:CHECKED IN FRAME {&FRAME-NAME} THEN
     run CleanUp. 
  apply "close" to this-procedure.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR hold-title AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.

    IF tb_tray-2:HIDDEN = NO THEN
       ASSIGN tb_tray-2.
  END.  

  ASSIGN lv-pdf-file = INIT-dir +  "\Job" + STRING(begin_job1)
         s-prt-mstandard = tb_prt-mch
         s-prt-shipto  = tb_prt-shipto
         s-prt-sellprc = tb_prt-sellprc
         s-run-speed = rd_print-speed = "S"
         s-sample-required = TB_sample_req .


  hold-title = c-win:TITLE.

  /* gdm - 11030807 */  
     ASSIGN   v-newdie   = NO
              v-newfilm  = NO
              v-newcombo = NO.

  IF tb_fold THEN DO:
    /*lines-per-page = IF lv-format-f EQ "HOP" THEN 64 ELSE 58. */

    RUN run-report ("Fold").

    c-win:TITLE = "Foldware " + TRIM(c-win:TITLE).

    case rd-dest:
       when 1 then run output-to-printer.
       when 2 then do:
           RUN output-to-screen.
       END.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_job1
                            &END_cust=END_job1
                            &fax-subject=c-win:title
                            &fax-body="c-win title"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = c-win:title
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job1
                                  &END_cust=end_job1
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 

    c-win:TITLE = hold-title.
  END.

  IF tb_corr THEN DO:
    /*lines-per-page = 0. ??? */

    RUN run-report ("Corr").

    c-win:TITLE = "Corrware " + TRIM(c-win:TITLE).

    case rd-dest:
       when 1 then run output-to-printer.
       when 2 then do:
           RUN output-to-screen.      
       END.
       when 3 then run output-to-file.
       when 4 then run output-to-fax.
       when 5 THEN RUN OUTPUT-to-mail.
       WHEN 6 THEN run output-to-port.
  end case. 
      c-win:TITLE = hold-title.      
  END.

  IF rd-dest = 2  THEN DO:
      /* not working well. keep running background
       APPLY "close":U TO this-procedure.
       RETURN NO-APPLY. 
      */  
      APPLY "entry" TO btn-cancel.
   END.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dept_codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dept_codes C-Win
ON HELP OF dept_codes IN FRAME FRAME-A
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR ip-char-val AS CHAR NO-UNDO.
   DEF VAR i-cnt AS INT NO-UNDO.

   ASSIGN spec_codes
          ip-char-val = "".

   EMPTY TEMP-TABLE tt-specCd.
   DO i-cnt = 1 TO NUM-ENTRIES(spec_codes):

       IF TRIM(ENTRY(i-cnt,spec_codes))  EQ "" THEN NEXT.

       CREATE tt-specCd.
       ASSIGN tt-char-val = TRIM(ENTRY(i-cnt,spec_codes)).
   END.

   FOR FIRST  tt-specCd NO-LOCK 
       BY tt-specCd.tt-char-val:

       ASSIGN ip-char-val = TRIM(tt-specCd.tt-char-val).
   END.

   RUN cec/l-itspec.w (g_company, ip-char-val, OUTPUT char-val).
   IF char-val NE "" AND {&self-name}:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
      ASSIGN
      {&self-name}:SCREEN-VALUE = {&self-name}:SCREEN-VALUE + "," + ENTRY(1,char-val)
      {&self-name}:SCREEN-VALUE = LEFT-TRIM({&self-name}:SCREEN-VALUE,",").
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dept_codes C-Win
ON LEAVE OF dept_codes IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 C-Win
ON LEAVE OF end_job1 IN FRAME FRAME-A /* Ending Job# */
DO:
  IF {&self-name}:MODIFIED THEN RUN new-job-no.
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A /* - */
DO:
  IF {&self-name}:MODIFIED THEN RUN new-job-no.
  ASSIGN {&self-name}.
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
  lines-per-page = IF SELF:SCREEN-VALUE = "L" THEN 48 ELSE 99.
  DISP lines-per-page WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME revsn_no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL revsn_no C-Win
ON LEAVE OF revsn_no IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_codes C-Win
ON HELP OF spec_codes IN FRAME FRAME-A /* Spec Codes */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR ip-char-val AS CHAR NO-UNDO.
   DEF VAR i-cnt AS INT NO-UNDO.

   ASSIGN spec_codes
          ip-char-val = "".

   EMPTY TEMP-TABLE tt-specCd.
   DO i-cnt = 1 TO NUM-ENTRIES(spec_codes):

       IF TRIM(ENTRY(i-cnt,spec_codes))  EQ "" THEN NEXT.

       CREATE tt-specCd.
       ASSIGN tt-char-val = TRIM(ENTRY(i-cnt,spec_codes)).
   END.

   FOR FIRST  tt-specCd NO-LOCK 
       BY tt-specCd.tt-char-val:

       ASSIGN ip-char-val = TRIM(tt-specCd.tt-char-val).
   END.

   RUN cec/l-itspec.w (g_company, ip-char-val, OUTPUT char-val).
   IF char-val NE "" AND {&self-name}:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
      ASSIGN
      {&self-name}:SCREEN-VALUE = {&self-name}:SCREEN-VALUE + "," + ENTRY(1,char-val)
      {&self-name}:SCREEN-VALUE = LEFT-TRIM({&self-name}:SCREEN-VALUE,",").
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_codes C-Win
ON LEAVE OF spec_codes IN FRAME FRAME-A /* Spec Codes */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_app-unprinted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_app-unprinted C-Win
ON VALUE-CHANGED OF tb_app-unprinted IN FRAME FRAME-A /* Print All Unprinted App. Tickets? */
DO:
  DEFINE BUFFER b-job-hdr FOR job-hdr.
  DEFINE BUFFER b-job FOR job.

  DEF VAR v-min-job-no AS CHAR INIT "zzzzzz00" NO-UNDO.
  DEF VAR v-max-job-no AS CHAR NO-UNDO.

  assign {&self-name}.

  IF tb_app-unprinted EQ YES THEN
  DO:
     SESSION:SET-WAIT-STATE("GENERAL").

     FOR EACH b-job-hdr FIELDS(company job job-no job-no2) WHERE
         b-job-hdr.company EQ cocode AND
         b-job-hdr.opened EQ YES
         NO-LOCK,
         FIRST b-job WHERE
               b-job.company  EQ b-job-hdr.company AND
               b-job.job      EQ b-job-hdr.job AND
               b-job.job-no   EQ b-job-hdr.job-no AND
               b-job.job-no2  EQ b-job-hdr.job-no2 AND
               b-job.pr-printed = NO AND
               b-job.opened   EQ YES AND
               b-job.cs-to-pr EQ YES
               NO-LOCK:

         IF b-job-hdr.job-no + STRING(b-job-hdr.job-no2,"99") LT v-min-job-no THEN
            v-min-job-no = b-job-hdr.job-no + STRING(b-job-hdr.job-no2,"99").

         IF b-job-hdr.job-no + STRING(b-job-hdr.job-no2,"99") GT v-max-job-no THEN
            v-max-job-no = b-job-hdr.job-no + STRING(b-job-hdr.job-no2,"99").
     END.

     ASSIGN begin_job1:SCREEN-VALUE = SUBSTRING(v-min-job-no,1,6)
            begin_job2:SCREEN-VALUE = SUBSTRING(v-min-job-no,7)
            end_job1:SCREEN-VALUE = SUBSTRING(v-max-job-no,1,6)
            end_job2:SCREEN-VALUE = SUBSTRING(v-max-job-no,7)
            tb_reprint:SCREEN-VALUE = "NO".

     APPLY "LEAVE" TO end_job2 IN FRAME {&FRAME-NAME}.

     SESSION:SET-WAIT-STATE("").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_approve
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_approve C-Win
ON VALUE-CHANGED OF tb_approve IN FRAME FRAME-A /* Approve Job(s)? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_box C-Win
ON VALUE-CHANGED OF tb_box IN FRAME FRAME-A /* Print Box Design? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_DC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_DC C-Win
ON VALUE-CHANGED OF tb_DC IN FRAME FRAME-A /* Print Die Cutter Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_dept-note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_dept-note C-Win
ON VALUE-CHANGED OF tb_dept-note IN FRAME FRAME-A /* Departments? */
DO:
  assign {&self-name}.
  IF tb_dept-note = YES THEN
      dept_codes:HIDDEN IN FRAME FRAME-A = NO.
  ELSE
      dept_codes:HIDDEN IN FRAME FRAME-A = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_draft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_draft C-Win
ON VALUE-CHANGED OF tb_draft IN FRAME FRAME-A /* Mark as Draft? */
DO:
    assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fgimage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fgimage C-Win
ON VALUE-CHANGED OF tb_fgimage IN FRAME FRAME-A /* Print FG Item Image? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding Carton */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_GL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_GL C-Win
ON VALUE-CHANGED OF tb_GL IN FRAME FRAME-A /* Print Gluer /  Window Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_make_hold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_make_hold C-Win
ON VALUE-CHANGED OF tb_make_hold IN FRAME FRAME-A /* Print Make  Hold? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_PR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_PR C-Win
ON VALUE-CHANGED OF tb_PR IN FRAME FRAME-A /* Print Printer Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-label C-Win
ON VALUE-CHANGED OF tb_prt-label IN FRAME FRAME-A /* Print Label Info? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-mch C-Win
ON VALUE-CHANGED OF tb_prt-mch IN FRAME FRAME-A /* Print Machine Standard? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-rev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-rev C-Win
ON VALUE-CHANGED OF tb_prt-rev IN FRAME FRAME-A /* Print Revision Number? */
DO:
  assign {&self-name}.

  IF NOT tb_prt-rev THEN
      revsn_no:HIDDEN IN FRAME FRAME-A = YES .
  ELSE
      revsn_no:HIDDEN IN FRAME FRAME-A = NO .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-sellprc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-sellprc C-Win
ON VALUE-CHANGED OF tb_prt-sellprc IN FRAME FRAME-A /* Print Sell Price in place of UPC#? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-shipto C-Win
ON VALUE-CHANGED OF tb_prt-shipto IN FRAME FRAME-A /* Print Shipto? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Tickets? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_RS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_RS C-Win
ON VALUE-CHANGED OF tb_RS IN FRAME FRAME-A /* Print Sheeter Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TB_sample_req
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TB_sample_req C-Win
ON VALUE-CHANGED OF TB_sample_req IN FRAME FRAME-A /* Sample(s) Required? */
DO:
  ASSIGN TB_sample_req.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-rel C-Win
ON VALUE-CHANGED OF tb_show-rel IN FRAME FRAME-A /* Print by Release Lines */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_SW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_SW C-Win
ON VALUE-CHANGED OF tb_SW IN FRAME FRAME-A /* Print Shrink Wrap Card */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tray-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tray-2 C-Win
ON VALUE-CHANGED OF tb_tray-2 IN FRAME FRAME-A /* Copy 2 and 3 in Tray 2 (Artios)? */
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

    DEF VAR plev AS INT NO-UNDO.

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

    production = NO.

    FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTES"
      NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.NAME     = "FREEZENOTES"
     sys-ctrl.module   = "OU5"
     sys-ctrl.descrip = "Default Toggle to User's Last Action?".
  end.

  v-freezenotes-log = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_fold = NOT AVAIL sys-ctrl           OR
             sys-ctrl.char-fld EQ "Both"  OR
             sys-ctrl.char-fld EQ "Foldware"
   tb_corr = AVAIL sys-ctrl AND
             (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").

 /* IF tb_fold THEN DO:
    {sys/inc/jobcard.i "F"}
    lv-format-f = sys-ctrl.char-fld.
    IF lookup(lv-format-f,"Interpac,Dayton,FibreFC,Livngstn,CentBox,Frankstn,Colonial,Unipak,Ottpkg,Shelby,CCC,Accord,Metro") > 0 THEN lines-per-page = 55.
  END.  

  IF tb_corr THEN DO:
    {sys/inc/jobcard.i "C"}
    lv-format-c = sys-ctrl.char-fld.
  END.*/

  {sys/inc/jobcard.i "F"}
   ASSIGN
    lv-format-f = sys-ctrl.char-fld
    lv-default-f = lv-format-f
    lv-int-f    = sys-ctrl.int-fld.
   IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0*/
       lookup(lv-format-f,"Interpac,FibreFC,HPB,metro,Dayton,Livngstn,CentBox,Frankstn,Colonial,Unipak,Ottpkg,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,Dee,Carded") > 0 THEN lines-per-page = 55.

   {sys/inc/jobcard.i "C"}
   ASSIGN
    lv-format-c = sys-ctrl.char-fld
    lv-default-c = lv-format-c
    lv-int-c    = sys-ctrl.int-fld.

    ASSIGN begin_job1 = ip-job-no
           begin_job2 = ip-job-no2
           end_job1 = ip-job-no
           end_job2 = ip-job-no2.

    IF TRIM(begin_job1) NE ""                          AND
       TRIM(begin_job1) EQ TRIM(end_job1)  THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1)) +
                                   TRIM(begin_job1)
            AND job-hdr.job-no2 EQ INT(begin_job2)
          NO-ERROR.

      IF AVAIL job-hdr THEN DO:
             FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = "JOBCARDC" AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                           /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-c = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-c = lv-default-c .

              FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "JOBCARDF" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                    /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.
                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-f = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-f = lv-default-f .
      END.
    END.

   /* gdm - 11120805*/
  ASSIGN 
      tb_fgimage:SENSITIVE = NO
      revsn_no:SENSITIVE = NO
      tb_prt-rev:SENSITIVE = NO.

  IF tb_dept-note = YES THEN
      dept_codes:HIDDEN IN FRAME FRAME-A = NO.
  ELSE
      dept_codes:HIDDEN IN FRAME FRAME-A = YES.

  IF LOOKUP(lv-format-c,"Artios,Protagon,CapCity,Trilakes2,Suthrlnd,RFC2,Peachtree") > 0 THEN
     ASSIGN tb_fgimage:SENSITIVE = YES.

  IF LOOKUP(lv-format-c,"Protagon") > 0 THEN
     ASSIGN tb_prt-rev:SENSITIVE = YES
            revsn_no:HIDDEN IN FRAME FRAME-A = NO .

  IF lv-format-c = "Artios" THEN
     ASSIGN
        tb_tray-2:HIDDEN = NO
        tb_tray-2:SENSITIVE = YES.

  FIND FIRST users WHERE
       users.user_id EQ USERID("NOSWEAT")
       NO-LOCK NO-ERROR.

  IF AVAIL users AND users.user_program[2] NE "" THEN
     init-dir = users.user_program[2].
  ELSE
     init-dir = "c:\tmp".

  RUN enable_UI.

  IF lv-format-c = "PEACHTREE" THEN
      ASSIGN
         tb_tray-2:HIDDEN = YES
         tb_tray-2:SENSITIVE = NO
         tb_app-unprinted:HIDDEN = YES
         tb_app-unprinted:SENSITIVE = NO.

  {methods/nowait.i}

  FIND FIRST job WHERE job.company = g_company
                   AND job.job-no = ip-job-no
                   AND job.job-no2 = ip-job-no2 NO-LOCK NO-ERROR.
  IF AVAIL job THEN FIND FIRST est WHERE est.company = g_company
                                     AND est.est-no = job.est-no NO-LOCK NO-ERROR.
  IF AVAIL est THEN DO:
     IF est.est-type < 5 THEN ASSIGN tb_fold = YES
                                     tb_corr = NO.
     ELSE ASSIGN tb_fold = NO
                 tb_corr = YES.
  END.
  IF AVAIL job THEN
    FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2
     NO-ERROR.
  IF AVAIL job-hdr THEN DO:
    tb_reprint = job-hdr.ftick-prnt.
  END.

  RUN enable_UI.

  DO WITH FRAME {&frame-name}:

    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
 /*   {custom/usrprint.i} */
      IF lv-format-c = "ColonialPL" OR lv-format-f = "Colonial" THEN
      ASSIGN
        tb_draft:HIDDEN = NO
        tb_draft:SENSITIVE = YES
        tb_draft:SCREEN-VALUE = "NO".

     revsn_no:HIDDEN IN FRAME FRAME-A           = TRUE.
     IF LOOKUP(lv-format-c,"Protagon") > 0 THEN
     ASSIGN tb_prt-rev:SENSITIVE = YES
             revsn_no:HIDDEN IN FRAME FRAME-A    = NO.

     IF NOT tb_prt-rev THEN
         revsn_no:HIDDEN IN FRAME FRAME-A = YES .
     ELSE
         revsn_no:HIDDEN IN FRAME FRAME-A = NO .

      IF INDEX(PROGRAM-NAME(4),"mainmenu") GT 0 
      THEN ASSIGN fl-jobord = 0
                  fl-jobord:SCREEN-VALUE = "0".

    /* gdm - 07130906 */
    IF lv-format-f = "FibreFC" 
      THEN ASSIGN fl-jobord = INT(fl-jobord:SCREEN-VALUE).
      ELSE ASSIGN fl-jobord = 0.

      ASSIGN
       tb_approve:SCREEN-VALUE = "no"
       TB_sample_req:HIDDEN = TRUE
       tb_app-unprinted:SCREEN-VALUE = "no"
       tb_app-unprinted:HIDDEN = TRUE
       tb_prompt-ship:SCREEN-VALUE = "no".

    IF v-freezenotes-log = NO THEN
       tb_freeze-note:SCREEN-VALUE = "no".

    IF lv-format-c = "pacific" OR lv-format-c = "Allwest" THEN
       ASSIGN
          lv-ornt:SCREEN-VALUE = "L"
          lv-font-no:SCREEN-VALUE = "13"
          lv-font-name:SCREEN-VALUE = "Courier New Size=9 (13CPI)"
          lines-per-page:SCREEN-VALUE = "48"
          lines-per-page = 48.

    /*IF lv-format-c = "pacific" THEN DO:
      lv-ornt = "L".
      APPLY "value-changed" TO lv-ornt .
    END.
    IF NOT tb_fold OR lv-format-f EQ "HOP" THEN DISABLE spec_codes.*/

     IF (tb_fold AND (lv-format-f = "Interpac"  OR lv-format-f = "Dayton" 
                 OR lv-format-f = "Livngstn"  OR lv-format-f = "FibreFC"  OR lv-format-f = "HPB"
                 OR lv-format-f = "metro"     or lv-format-f = "Indiana-XL" OR lv-format-f = "MidYork"
                 OR lv-format-f = "CentBox"   OR lv-format-f = "Keystone" OR lv-format-f = "Frankstn" 
                 OR lv-format-f = "Colonial"  OR lv-format-f = "Unipak"   OR lv-format-f = "Ottpkg"
                 OR lv-format-f = "MWFIbre"   OR lv-format-f = "Shelby"   OR lv-format-f = "CCC"
                 OR lv-format-f = "PPI"       OR lv-format-f = "Accord"   OR lv-format-f = "Knight"  
                 OR lv-format-f = "PackRite"  OR lv-format-f = "Knight***"
                 OR lv-format-f = "Dee"       OR lv-format-f = "Rosmar" OR lv-format-f = "Carded" OR lv-format-f = "Carded2" )) OR
        (tb_corr AND (lv-format-c = "Trilakes" OR lv-format-c = "Axis" OR lv-format-c = "Trilakes2" OR lv-format-c = "Hughes")) THEN
      assign 
        tb_prt-mch:SENSITIVE      = YES
        tb_prt-shipto:SENSITIVE   = YES
        tb_prt-sellprc:SENSITIVE  = YES
        rd_print-speed:SENSITIVE  = YES .            

    ELSE do:
        ASSIGN tb_prt-mch = NO
               tb_prt-shipto = NO
               tb_prt-sellprc = NO.
        ASSIGN tb_prt-mch:SCREEN-VALUE = "no"
               tb_prt-shipto:SCREEN-VALUE = "no"
               tb_prt-sellprc:SCREEN-VALUE = "no" 
               rd_print-speed:SCREEN-VALUE = "S".
    END.

    IF lv-format-c = "PEACHTREE" THEN
      ASSIGN
         tb_make_hold:HIDDEN = NO
         tb_make_hold:SENSITIVE = YES.
    ELSE
      ASSIGN
         tb_make_hold:HIDDEN = YES
         tb_make_hold:SENSITIVE = NO.

     /*IF lv-format-c EQ "Artios" AND lv-format-f EQ "FibreFC" THEN DO:*/
      plev = 1.
      REPEAT WHILE PROGRAM-NAME(plev) NE ?:
        IF PROGRAM-NAME(plev) MATCHES "*w-jobapp*" THEN DO:
          production = YES.
          LEAVE.
        END.
        plev = plev + 1.
      END.
      IF NOT production THEN
         ASSIGN
            tb_approve:HIDDEN    = NO
            tb_approve:SENSITIVE = YES.

      ELSE IF lv-format-c EQ "Artios" AND lv-format-f EQ "FibreFC" THEN
         ASSIGN
            tb_app-unprinted:HIDDEN    = NO
            tb_app-unprinted:SENSITIVE = YES.

    IF lv-format-c EQ "Artios" AND lv-format-f EQ "FibreFC" THEN
       RUN split-ship-proc. /*only for Fibre*/

     IF tb_corr = TRUE AND (lv-format-c = "Protagon" OR lv-format-c = "Hughes" OR lv-format-c = "Allwest") THEN
       TB_sample_req:HIDDEN = FALSE.
    ELSE
       TB_sample_req:HIDDEN = TRUE.

    IF TRIM(begin_job1:SCREEN-VALUE) NE ""                          AND
       TRIM(begin_job1:SCREEN-VALUE) EQ TRIM(end_job1:SCREEN-VALUE) AND
       INT(begin_job2:SCREEN-VALUE)  EQ INT(end_job2:SCREEN-VALUE)  THEN DO:
       FIND FIRST job-hdr NO-LOCK
           WHERE job-hdr.company EQ cocode
             AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1:SCREEN-VALUE)) +
                                    TRIM(begin_job1:SCREEN-VALUE)
             AND job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
           NO-ERROR.
       IF AVAIL job-hdr THEN DO:
          FIND FIRST job NO-LOCK
               WHERE job.company EQ job-hdr.company
                 AND job.job     EQ job-hdr.job
                 AND job.job-no  EQ job-hdr.job-no
                 AND job.job-no2 EQ job-hdr.job-no2
               NO-ERROR.
           IF AVAIL job THEN  do:
               IF job.cs-to-pr = TRUE THEN 
                assign
                          tb_approve:SCREEN-VALUE = "Yes" 
                          tb_approve:HIDDEN    = NO
                          tb_approve:SENSITIVE = NO .
               ELSE
                       assign
                           tb_approve:SCREEN-VALUE = "no"   
                           tb_approve:HIDDEN    =  NO
                           tb_approve:SENSITIVE =  YES .

           END.

           IF can-find(FIRST b-reftable-freeze WHERE
              b-reftable-freeze.reftable EQ "FREEZENOTE" AND
              b-reftable-freeze.company  EQ cocode AND
              b-reftable-freeze.loc      EQ job-hdr.job-no AND
              b-reftable-freeze.CODE     EQ STRING(job-hdr.job-no2,"99")) THEN
              tb_freeze-note:SCREEN-VALUE = "YES".

           IF CAN-FIND(FIRST b-reftable-split WHERE
              b-reftable-split.reftable EQ "splitshp" AND
              b-reftable-split.company  EQ cocode AND
              b-reftable-split.loc      EQ TRIM(job-hdr.job-no) AND
              b-reftable-split.code     EQ STRING(job-hdr.job-no2,"9999999999")) THEN
              tb_prompt-ship:SCREEN-VALUE = "YES".

       END.  /* avail job-hdr */
    END.

    IF lv-format-f EQ "Accord" THEN
       ASSIGN tb_freeze-note:SCREEN-VALUE = "NO"
              tb_freeze-note:SENSITIVE = NO.

    IF lv-format-f EQ "Prystup" THEN
        tb_show-rel:HIDDEN IN FRAME FRAME-A = NO.
    ELSE
        tb_show-rel:HIDDEN IN FRAME FRAME-A = YES.
    /* Task #: 02160708 - dgd 04/04/2007 - START */
    if can-do ("Indiana-XL", lv-format-f) 
      then run HideDeptBoxes (no).
      else run HideDeptBoxes (yes).
    /* Task #: 02160708 - dgd 04/04/2007 - END */

    tb_prt-set-header:SENSITIVE = CAN-DO("Artios,Premier,Xprint,Suthrlnd,United,MulticellGA,oklahoma,Hughes,Protagon,Spectrum,CapCity,Allwest,LoyLang,PQP,RFC2,PEACHTREE",lv-format-c).
    IF NOT tb_prt-set-header:SENSITIVE THEN
      tb_prt-set-header:SCREEN-VALUE = "no".

    IF TRIM(begin_job1:SCREEN-VALUE) NE ""                          AND
       TRIM(begin_job1:SCREEN-VALUE) EQ TRIM(end_job1:SCREEN-VALUE) AND
       INT(begin_job2:SCREEN-VALUE)  EQ INT(end_job2:SCREEN-VALUE)  THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1:SCREEN-VALUE)) +
                                   TRIM(begin_job1:SCREEN-VALUE)
            AND job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
          NO-ERROR.
      IF AVAIL job-hdr THEN DO:
        tb_reprint:SCREEN-VALUE = STRING(job-hdr.ftick-prnt).
        IF production THEN DO:
          FIND FIRST job NO-LOCK
              WHERE job.company EQ job-hdr.company
                AND job.job     EQ job-hdr.job
                AND job.job-no  EQ job-hdr.job-no
                AND job.job-no2 EQ job-hdr.job-no2
              NO-ERROR.
          IF AVAIL job THEN tb_reprint:SCREEN-VALUE = STRING(job.pr-printed). 

        END.
      END.
    END.    

    /*IF lookup(lv-format-c,"Artios,Premier,Xprint,Suthrlnd,United,Hughes,Spectrum,CapCity,RFC2") <= 0 THEN 
    do:
       ASSIGN tb_prt-set-header:SCREEN-VALUE = "No"
              tb_prt-set-header:SENSITIVE = NO  .
    END.
    ELSE ASSIGN tb_prt-set-header:SCREEN-VALUE = "No"
              tb_prt-set-header:SENSITIVE = YES  .

    /*IF lv-format-f = "CentBox" THEN*/ ASSIGN tb_reprint = YES
                                               tb_reprint:SCREEN-VALUE = "Yes"  .*/
  END.
  {methods/nowait.i}
  /*APPLY "entry" TO begin_job1 IN FRAME {&FRAME-NAME}. */
    RUN new-job-no.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp C-Win 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
  Purpose:    Clean up routine.
  Parameters: <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Excel Handle */
  def var chExcelApplication  as com-handle no-undo.
  def var chWorkbook          as com-handle no-undo.
  def var chWorkSheet         as com-handle no-undo.
  def var chHyper             as com-handle no-undo.

  /* Connect to the running Excel session. */
  CREATE "Excel.Application" chExcelApplication connect no-error.

  if valid-handle (chExcelApplication) then
  do:
    chWorkBook:close()                no-error.
    chExcelApplication:Quit()         no-error.
    chExcelApplication:Quit(0)        no-error.

    /* RELEASE OBJECTS */
    RELEASE OBJECT chWorkbook         NO-ERROR.
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chHyper            NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.
  end.

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
  DISPLAY begin_job1 begin_job2 end_job1 end_job2 tb_fold tb_show-rel tb_RS 
          tb_corr tb_PR tb_reprint tb_DC tb_box tb_GL tb_fgimage tb_SW 
          spec_codes tb_prt-rev revsn_no tb_prt-mch rd_print-speed tb_prt-shipto 
          tb_prt-sellprc tb_prt-label tb_committed tb_prt-set-header 
          tb_prompt-ship TB_sample_req tb_dept-note dept_codes tb_freeze-note 
          rd-dest lines-per-page lv-ornt lv-font-no lv-font-name td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job1 begin_job2 end_job1 end_job2 tb_fold 
         tb_show-rel tb_RS tb_corr tb_PR tb_reprint tb_DC tb_box tb_GL tb_SW 
         tb_approve spec_codes revsn_no tb_prt-label tb_committed 
         tb_prt-set-header tb_prompt-ship TB_sample_req tb_dept-note dept_codes 
         tb_freeze-note rd-dest lines-per-page lv-ornt lv-font-no td-show-parm 
         btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideDeptBoxes C-Win 
PROCEDURE HideDeptBoxes :
/*------------------------------------------------------------------------------
  Purpose     : To Enable or Disable Dept Toggle Boxes.
  Parameters  : ilHide - yes/no
  Notes       : Task#: 02160708 - dgd 04/04/2007
------------------------------------------------------------------------------*/

  /* Parameters */
  def input param ilHide  as log no-undo.

  /* Enable / Disable Dept Toggle Boxes. */
  do with frame {&frame-name}:
    assign
      tb_RS:hidden  = ilHide
      tb_PR:hidden  = ilHide
      tb_DC:hidden  = ilHide
      tb_GL:hidden  = ilHide
      tb_SW:hidden  = ilHide no-error.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no C-Win 
PROCEDURE new-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-fold AS LOG NO-UNDO.
  DEF VAR ll-corr AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN set-job-vars.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.

    FIND FIRST job NO-LOCK
         WHERE job.company EQ cocode
         AND job.job-no EQ SUBSTR(fjob-no,1,6) NO-ERROR.
/*          IF AVAIL job AND job.opened = YES  THEN DO : */
/*                                                       */
/*             if job.cs-to-pr = YES THEN                */
/*               ASSIGN                                  */
/*                 tb_approve:SCREEN-VALUE = "yes"       */
/*                tb_approve:HIDDEN    = NO              */
/*                tb_approve:SENSITIVE = NO .            */
/*             ELSE                                      */
/*               ASSIGN                                  */
/*                tb_approve:SCREEN-VALUE = "no"         */
/*                tb_approve:HIDDEN    = NO              */
/*                tb_approve:SENSITIVE = YES .           */
/*          END.                                         */

    ASSIGN
     ll-fold = NOT AVAIL sys-ctrl           OR
               sys-ctrl.char-fld EQ "Both"  OR
               sys-ctrl.char-fld EQ "Foldware"
     ll-corr = AVAIL sys-ctrl AND
               (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").

    IF ll-fold AND ll-corr THEN
    FOR EACH job-hdr
        WHERE job-hdr.company               EQ cocode

          AND job-hdr.job-no                GE SUBSTR(fjob-no,1,6)
          AND job-hdr.job-no                LE SUBSTR(tjob-no,1,6)

          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  GE fjob-no

          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  LE tjob-no
        NO-LOCK,

        FIRST job
        WHERE job.company                   EQ cocode
          AND job.job                       EQ job-hdr.job
          AND job.job-no                    EQ job-hdr.job-no
          AND job.job-no2                   EQ job-hdr.job-no2
          AND job.stat                      NE "H"
        NO-LOCK,

        FIRST est
        WHERE est.company = job.company
          AND est.est-no                    EQ job.est-no
        NO-LOCK

        BREAK BY job-hdr.company:

      IF FIRST(job-hdr.company) THEN
        ASSIGN
         ll-fold = NO
         ll-corr = NO.

      FIND FIRST sys-ctrl-shipto WHERE
             sys-ctrl-shipto.company = cocode AND
             sys-ctrl-shipto.NAME = "JOBCARDC" AND
             sys-ctrl-shipto.cust-vend = YES AND
             sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
             /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
             sys-ctrl-shipto.char-fld > ''
             NO-LOCK NO-ERROR.

         IF AVAIL sys-ctrl-shipto AND substr(fjob-no,1,6) EQ substr(tjob-no,1,6) THEN do:
             lv-format-c = sys-ctrl-shipto.char-fld .
          END.
          ELSE DO:
               lv-format-c = lv-default-c .
          END.

          IF lv-format-c = "PEACHTREE" THEN
                 ASSIGN
                 tb_tray-2:HIDDEN IN FRAME FRAME-A = YES
                 tb_tray-2:SENSITIVE = NO
                 tb_app-unprinted:HIDDEN IN FRAME FRAME-A = YES
                 tb_app-unprinted:SENSITIVE = NO
                 tb_make_hold:HIDDEN IN FRAME FRAME-A = NO
                 tb_make_hold:SENSITIVE = YES     .
             ELSE
                 ASSIGN
                     tb_make_hold:HIDDEN IN FRAME FRAME-A = YES
                     tb_make_hold:SENSITIVE = NO.

            IF lv-format-c = "MulticellGA" THEN
                ASSIGN
                tb_dept-note:HIDDEN IN FRAME FRAME-A = NO
                tb_dept-note:SENSITIVE = YES
                dept_codes:HIDDEN IN FRAME FRAME-A = NO .
             ELSE
                 ASSIGN
                    tb_dept-note:HIDDEN IN FRAME FRAME-A = YES
                    tb_dept-note:SENSITIVE = NO .

          FIND FIRST sys-ctrl-shipto WHERE
              sys-ctrl-shipto.company = cocode AND
              sys-ctrl-shipto.NAME = "JOBCARDF" AND
              sys-ctrl-shipto.cust-vend = YES AND
              sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
              /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
              sys-ctrl-shipto.char-fld > ''
                NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl-shipto AND substr(fjob-no,1,6) EQ substr(tjob-no,1,6) THEN 
              lv-format-f = sys-ctrl-shipto.char-fld .
          ELSE 
              lv-format-f = lv-default-f .

      IF est.est-type LE 4 THEN ll-fold = YES.
                           ELSE ll-corr = YES.

      IF ll-fold AND ll-corr THEN LEAVE.
    END.

    ASSIGN
     tb_fold:SCREEN-VALUE = STRING(ll-fold)
     tb_corr:SCREEN-VALUE = STRING(ll-corr).

    IF v-freezenotes-log EQ NO THEN
    DO:
       IF lv-format-f NE "Accord" AND AVAIL job-hdr AND
          can-find(FIRST b-reftable-freeze WHERE
          b-reftable-freeze.reftable EQ "FREEZENOTE" AND
          b-reftable-freeze.company  EQ cocode AND
          b-reftable-freeze.loc      EQ job-hdr.job-no AND
          b-reftable-freeze.CODE     EQ STRING(job-hdr.job-no2,"99")) THEN DO:
          tb_freeze-note:SCREEN-VALUE = "YES".
       END.
       ELSE 
           tb_freeze-note:SCREEN-VALUE = "NO".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-fax C-Win 
PROCEDURE output-to-fax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*run output-to-fax.*/
   DO WITH FRAME {&FRAME-NAME}:


           {custom/asifax.i &begin_cust=begin_job1
                            &END_cust=END_job1
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
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

   {custom/out2file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE output-to-mail C-Win 
PROCEDURE output-to-mail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
  IF is-xprint-form THEN DO:
     RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
     {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject="Factory Ticket"
                             &mail-body="Factory Ticket"
                             &mail-file=lv-pdf-file + ".pdf" }  

  END.
  ELSE DO:
      {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job1
                                  &END_cust=end_job1
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

  END.

 END.
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
   DEFINE VARIABLE result AS LOGICAL NO-UNDO.

/*     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.

/*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
     /*RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 11, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
*/
*/
   IF tb_corr THEN DO:
       FILE-INFO:FILE-NAME = list-name.
       RUN printfile (FILE-INFO:FILE-NAME).
   END.
   ELSE DO:
      IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0 */
        lookup(lv-format-f, 
          "Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OttPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Dee,Badger,Carded") > 0 THEN
     DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
      END.
      ELSE RUN custom/prntproc.p (list-name, lv-font-no, lv-ornt).
   END.

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
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR lv-file AS cha NO-UNDO.
   DEF VAR lv-xpfile AS cha NO-UNDO.

/*
  RUN scr-rpt.w (list-name,c-win:TITLE). /* open file-name, title */  
*/
/* for xprint view    not working, print automatically
   FILE-INFO:FILE-NAME = "custom\vpxprint.exe".
   lv-cmd = FILE-INFO:FILE-NAME.
   FILE-INFO:FILE-NAME = list-name.
   lv-file = FILE-INFO:FILE-NAME.
   lv-xpfile = lv-file + ".xpr".


   OS-COPY VALUE(lv-file) VALUE(lv-xpfile).
   OS-COMMAND VALUE(lv-cmd + " " + lv-xpfile) .
*/ 
 IF tb_corr THEN DO:
    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).   
 END.

 ELSE DO:

     do with frame {&frame-name}:
        if lv-format-f = 'Indiana-XL'          and 
           (logical (tb_RS:screen-value) = true or
            logical (tb_PR:screen-value) = true or
            logical (tb_DC:screen-value) = true or
            logical (tb_GL:screen-value) = true or
            logical (tb_SW:screen-value) = true) then
           return.
     end.

     IF  /*index("Interpac,FibreFC,Dayton,Livngstn",lv-format-f) > 0 */
        lookup(lv-format-f, "Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OTTPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,MidYork,Knight,Dee,Badger,Carded,Carded2,Knight***") > 0 THEN
     DO:
         FILE-INFO:FILE-NAME = list-name.
         RUN printfile (FILE-INFO:FILE-NAME).   
     END.
     ELSE 
       RUN scr-rpt.w (list-name,c-win:TITLE,lv-font-no,lv-ornt). /* open file-name, title */
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-industry AS CHAR NO-UNDO.
DEF BUFFER b-reftable FOR reftable.
DEF BUFFER b-eb FOR eb.    

IF cocode = ""  THEN cocode = "001".
IF locode = "" THEN locode = "MAIN".

{sys/form/r-top.i}

RUN set-job-vars.

  ASSIGN 
    print-box               = tb_box
    reprint                 = tb_reprint
    s-prt-fgimage           = tb_fgimage
    s-prt-label             = tb_prt-label
    s-committed-board-only  = tb_committed
    s-prt-set-header        = tb_prt-set-header
    spec-list               = spec_codes
    s-prt-ship-split        = tb_prompt-ship
    s-show-release          = tb_show-rel
    approve                 = tb_approve
    s-prt-revno             = tb_prt-rev 
    v-dept-log              = tb_dept-note
    v-dept-codes            = dept_codes
    lDraft                  = tb_draft.

  IF s-prt-revno THEN
      ASSIGN revision-no             = string(revsn_no).
  ELSE
      ASSIGN revision-no             = "".

  FOR EACH wrk-ink:
      DELETE wrk-ink.
  END.
  {jcrep/tickrrpt.i}

/*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE). */

SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-job-vars C-Win 
PROCEDURE set-job-vars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(begin_job1:SCREEN-VALUE))) +
                 TRIM(begin_job1:SCREEN-VALUE)
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(end_job1:SCREEN-VALUE))) +
                 trim(end_job1:SCREEN-VALUE)
     fjob-no2  = INT(begin_job2:SCREEN-VALUE)
     tjob-no2  = INT(end_job2:SCREEN-VALUE)
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99").
  END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE split-ship-proc C-Win 
PROCEDURE split-ship-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:

      FIND FIRST reftable WHERE
           reftable.reftable EQ "SPLITSHIP" AND
           reftable.company  EQ cocode AND
           reftable.loc      EQ FILL(" ",6 - LENGTH(begin_job1:SCREEN-VALUE))
                                + begin_job1:SCREEN-VALUE AND
           reftable.CODE     EQ STRING(begin_job2:SCREEN-VALUE,"99")
           NO-LOCK NO-ERROR.

      IF AVAIL reftable THEN
      DO:
         tb_prompt-ship:CHECKED = YES.
         RELEASE reftable.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

