&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : jcrep/r-ticket.w

  History           : dgd 04/04/2007  - TN 02160708 Dept Toggle Boxes
                      gdm 07130906 - ADDED HIDDEN FILL IN fl-jobord.
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

DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTitle    AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
{api/ttAPIOutboundEvent.i}

ASSIGN
    cocode = gcompany
    locode = gloc.
{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLJobTicket &Company=cocode} /* rstark 05181205 */

DEFINE NEW SHARED VARIABLE v-dept-codes      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE lv-qty            AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE qty               AS INTEGER   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel      AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-format-f       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-format-c       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-default-f      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-default-c      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-int-f          AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-int-c          AS INTEGER   NO-UNDO.
DEFINE            VARIABLE dDecimalFoldValue AS DECIMAL   NO-UNDO .

{jcrep/r-ticket.i "new shared"}

DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cHoldMessage   AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

{custom/xprint.i}

DEFINE NEW SHARED VARIABLE s-prt-fgimage          AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-revno            AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE revision-no            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form         AS LOG       NO-UNDO.
DEFINE            VARIABLE ls-fax-file            AS cha       NO-UNDO.

DEFINE NEW SHARED VARIABLE s-prt-mstandard        AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-shipto           AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-sellprc          AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-run-speed            AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-committed-board-only AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-set-header       AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-sample-required      AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE s-show-release         AS LOG       NO-UNDO.

DEFINE            VARIABLE lv-save-spec           AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-freezenotes-log      AS LOG       NO-UNDO.
DEFINE            VARIABLE v-freezenote-log       AS LOG       NO-UNDO.
DEFINE            VARIABLE v-freezenotes-pass     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lFreezeNoteVal         AS LOG       NO-UNDO.
DEFINE            VARIABLE v-oe-ctrl              AS LOG       INIT YES.
DEFINE            VARIABLE lExportXML             AS LOGICAL   INIT NO NO-UNDO.
DEFINE            VARIABLE cReturnChar            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE XMLJobTicket-log       AS LOGICAL   NO-UNDO.

{cerep/jc-keyst.i "NEW"}
{cerep/jc-keys2.i "NEW"}
{cecrep/jc-prem.i "NEW"}
{cecrep/jc-fibre.i "NEW"}
{cecrep/jc-pallet.i "NEW"}
{cecrep/jc-soule.i "NEW"}
/*{cecrep/tt-artios.i "NEW"}*/
{cerep/tt-samp-ctn.i "NEW"}

DEFINE TEMP-TABLE t-ef-form
    FIELD form-no LIKE ef.form-no.

DEFINE            VARIABLE lv-pdf-file      AS cha NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-ship-split AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE s-prt-label      AS LOG NO-UNDO.

{ cerep/tt-wrk-ink.i "NEW SHARED" }

/* gdm - 10010805 */
DEFINE TEMP-TABLE tt-specCd NO-UNDO
    FIELD tt-char-val AS CHARACTER
    INDEX chr-1 tt-char-val.
/* gdm - 11030807 */
DEFINE NEW SHARED VARIABLE v-newdie         AS LOG       NO-UNDO INIT FALSE.
DEFINE NEW SHARED VARIABLE v-newfilm        AS LOG       NO-UNDO INIT FALSE.
DEFINE NEW SHARED VARIABLE v-newcombo       AS LOG       NO-UNDO INIT FALSE.
DEFINE NEW SHARED VARIABLE lIncludeLastPage AS LOGICAL   NO-UNDO .
DEFINE NEW SHARED VARIABLE cRdOptionMclean  AS CHARACTER INITIAL "M" NO-UNDO .
DEFINE NEW SHARED VARIABLE cJobType         AS CHARACTER NO-UNDO .
DEFINE NEW SHARED VARIABLE lFSC             AS LOGICAL   NO-UNDO .
DEFINE BUFFER b-reftable-freeze FOR reftable.
DEFINE BUFFER b-reftable-split  FOR reftable.


/* Excel Vars */
DEFINE VARIABLE chExcel         AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet     AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet2    AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook      AS COMPONENT-HANDLE NO-UNDO.

DEFINE VARIABLE cExcelOutput    AS cha              NO-UNDO.

DEFINE VARIABLE lAsiUser        AS LOGICAL          NO-UNDO .
DEFINE VARIABLE hPgmSecurity    AS HANDLE           NO-UNDO.
DEFINE VARIABLE lResult         AS LOGICAL          NO-UNDO.

DEFINE VARIABLE hdOutboundProcs AS HANDLE           NO-UNDO.
DEFINE VARIABLE lPrinterAllow   AS LOGICAL          NO-UNDO.

/* Procedure to prepare and execute API calls */
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
RUN epCanAccess IN hPgmSecurity ("oerep/r-bolprt.w","", OUTPUT lResult).
RUN epCanAccess IN hPgmSecurity ("jcrep/r-ticket.w", "", OUTPUT lPrinterAllow).
DELETE OBJECT hPgmSecurity.
IF lResult THEN ASSIGN lAsiUser = YES .

RUN sys/ref/nk1look.p (INPUT cocode, "XMLJobTicket", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cReturnChar, OUTPUT lRecfound).
XMLJobTicket-log = LOGICAL(cReturnChar) NO-ERROR.

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
tb_GL tb_SW tb_approve tb_spanish tb_print-metric tbPageBreakByForm ~
spec_codes revsn_no rd_print-Sheet tb_prt-label tb_committed ~
tb_prt-set-header tb_prompt-ship dept_codes TB_sample_req tb_freeze-note ~
rd-dest run_format tb_ExportXML tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job1 begin_job2 end_job1 end_job2 ~
tb_fold tb_show-rel tb_RS tb_corr tb_PR tb_reprint tb_DC tb_box tb_GL ~
tb_fgimage tb_SW tb_print-metric tbPageBreakByForm spec_codes tb_prt-rev ~
revsn_no tb_prt-dmi rd_print-Sheet tb_prt-mch rd_print-speed tb_prt-shipto ~
tb_prt-sellprc tb_prt-label tb_committed tb_prt-set-header tb_prompt-ship ~
dept_codes TB_sample_req tb_freeze-note rd-dest run_format tb_ExportXML ~
tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 begin_job1 begin_job2 end_job1 end_job2 tb_reprint ~
tb_box tb_fgimage tb_approve tb_tray-2 tb_spanish tb_make_hold tb_draft ~
tb_print-metric tb_app-unprinted tb_attched tb_prt-rev tb_prt-dmi ~
tb_prt-mch tb_prt-shipto tb_prt-sellprc tb_prt-label td-show-parm 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setBold C-Win 
FUNCTION setBold RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRange C-Win 
FUNCTION setRange RETURNS CHARACTER
    ( pcSCol AS CHARACTER,
    pciCol AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSize C-Win 
FUNCTION setSize RETURNS LOGICAL
    (  iN# AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setValue C-Win 
FUNCTION setValue RETURNS LOGICAL
    ( cValue AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "&Cancel" 
     SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 16 BY 1.29.

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "x(9)" 
     LABEL "Beginning  Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE begin_job2 AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1.

DEFINE VARIABLE dept_codes AS CHARACTER FORMAT "X(256)":U INITIAL "QA" 
     LABEL "Departments Code" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "x(9)" INITIAL "zzzzzz" 
     LABEL "Ending Job#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 3 FGCOLOR 15 .

DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">>9" INITIAL 99 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 5.4 BY 1.

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

DEFINE VARIABLE revsn_no AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE run_format AS CHARACTER FORMAT "X(30)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

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
"To Email", 5
     SIZE 16.8 BY 4.29 NO-UNDO.

DEFINE VARIABLE rd_print-Sheet AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Job Ticket", "J",
"Sheeting Ticket", "S",
"Both", "B"
     SIZE 39.6 BY .95 NO-UNDO.

DEFINE VARIABLE rd_print-speed AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Speed", "S",
"Run Hour", "H"
     SIZE 24.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 4.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 17.62.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tbPageBreakByForm AS LOGICAL INITIAL no 
     LABEL "Page Break By Form" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE tb_app-unprinted AS LOGICAL INITIAL no 
     LABEL "Print All Unprinted App. Tickets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81.

DEFINE VARIABLE tb_approve AS LOGICAL INITIAL no 
     LABEL "Approve Job(s)?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE tb_attched AS LOGICAL INITIAL no 
     LABEL "Print Attachment Images" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

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

DEFINE VARIABLE tb_draft AS LOGICAL INITIAL no 
     LABEL "Mark as Draft?" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81.

DEFINE VARIABLE tb_ExportXML AS LOGICAL INITIAL no 
     LABEL "Export XML" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.2 BY .81 NO-UNDO.

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
     LABEL "Make & Hold?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_PR AS LOGICAL INITIAL no 
     LABEL "Print &Printer Card" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE tb_print-metric AS LOGICAL INITIAL no 
     LABEL "Print Metric Size" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81.

DEFINE VARIABLE tb_prompt-ship AS LOGICAL INITIAL no 
     LABEL "Prompt Split Shipment or Split Order?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-dmi AS LOGICAL INITIAL no 
     LABEL "Print DMI Barcode page?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY 1 NO-UNDO.

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

DEFINE VARIABLE tb_spanish AS LOGICAL INITIAL no 
     LABEL "Print Spanish" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

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
     begin_job1 AT ROW 2.14 COL 19 COLON-ALIGNED HELP
          "Enter Beginning Job Number"
     begin_job2 AT ROW 2.14 COL 35 COLON-ALIGNED HELP
          "Enter Beginning Run#"
     end_job1 AT ROW 2.14 COL 55.2 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     end_job2 AT ROW 2.14 COL 71.2 COLON-ALIGNED HELP
          "Enter Ending Run#"
     tb_fold AT ROW 3.33 COL 21
     tb_show-rel AT ROW 3.33 COL 57 WIDGET-ID 16
     tb_RS AT ROW 3.33 COL 57.2
     tb_corr AT ROW 4.33 COL 21
     tb_PR AT ROW 4.33 COL 57
     tb_reprint AT ROW 5.33 COL 21
     tb_DC AT ROW 5.33 COL 57
     tb_box AT ROW 6.33 COL 42 RIGHT-ALIGNED
     tb_GL AT ROW 6.33 COL 57
     tb_fgimage AT ROW 7.33 COL 47 RIGHT-ALIGNED
     tb_SW AT ROW 7.33 COL 57
     tb_approve AT ROW 8.33 COL 47 RIGHT-ALIGNED
     tb_tray-2 AT ROW 8.33 COL 91 RIGHT-ALIGNED WIDGET-ID 6
     tb_spanish AT ROW 9.19 COL 47 RIGHT-ALIGNED
     tb_make_hold AT ROW 9.33 COL 91 RIGHT-ALIGNED WIDGET-ID 12
     tb_draft AT ROW 9.38 COL 56 RIGHT-ALIGNED WIDGET-ID 18
     tb_print-metric AT ROW 9.38 COL 56 RIGHT-ALIGNED WIDGET-ID 22
     tb_app-unprinted AT ROW 9.38 COL 56 RIGHT-ALIGNED WIDGET-ID 10
     tbPageBreakByForm AT ROW 10.29 COL 21 WIDGET-ID 66
     tb_attched AT ROW 10.29 COL 91 RIGHT-ALIGNED WIDGET-ID 68
     spec_codes AT ROW 11.24 COL 18.8 COLON-ALIGNED
     tb_prt-rev AT ROW 12.29 COL 21
     revsn_no AT ROW 12.29 COL 49 COLON-ALIGNED
     tb_prt-dmi AT ROW 12.29 COL 60.8 WIDGET-ID 24
     rd_print-Sheet AT ROW 13.14 COL 57.2 NO-LABEL WIDGET-ID 26
     tb_prt-mch AT ROW 13.19 COL 21
     fl-jobord AT ROW 13.19 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rd_print-speed AT ROW 14.1 COL 57.4 NO-LABEL
     tb_prt-shipto AT ROW 15 COL 21
     tb_prt-sellprc AT ROW 15 COL 56
     tb_prt-label AT ROW 16 COL 21
     tb_committed AT ROW 16 COL 56
     tb_prt-set-header AT ROW 17 COL 21
     tb_prompt-ship AT ROW 17 COL 56
     dept_codes AT ROW 17.91 COL 69 COLON-ALIGNED WIDGET-ID 14
     TB_sample_req AT ROW 17.95 COL 56 WIDGET-ID 2
     tb_freeze-note AT ROW 18 COL 21
     lv-ornt AT ROW 19.62 COL 53 NO-LABEL
     lines-per-page AT ROW 19.62 COL 91 COLON-ALIGNED
     rd-dest AT ROW 19.86 COL 5 NO-LABEL
     lv-font-no AT ROW 21.33 COL 48 COLON-ALIGNED
     lv-font-name AT ROW 21.33 COL 58 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.33 COL 31.4
     run_format AT ROW 22.95 COL 69 COLON-ALIGNED WIDGET-ID 12
     tb_ExportXML AT ROW 23.05 COL 31.4 WIDGET-ID 20
     tbAutoClose AT ROW 24.48 COL 31.4 WIDGET-ID 64
     btn-ok AT ROW 25.33 COL 31.2
     btn-cancel AT ROW 25.33 COL 55.2
     "Print Machine's Speed or Run Hour ?" VIEW-AS TEXT
          SIZE 36.6 BY .62 AT ROW 14.24 COL 21
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 19.1 COL 4
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.05 COL 4
     RECT-6 AT ROW 19.48 COL 3
     RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.8 BY 26.05
         BGCOLOR 15 .


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
         HEIGHT             = 26.05
         WIDTH              = 98.8
         MAX-HEIGHT         = 32.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 32.29
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

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

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

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE
       lv-font-no:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

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

/* SETTINGS FOR TOGGLE-BOX tb_attched IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_attched:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_attched:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_print-metric IN FRAME FRAME-A
   ALIGN-R 1                                                            */
ASSIGN 
       tb_print-metric:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prompt-ship:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_prt-dmi IN FRAME FRAME-A
   NO-ENABLE 1                                                          */
ASSIGN 
       tb_prt-dmi:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_spanish IN FRAME FRAME-A
   NO-DISPLAY ALIGN-R 1                                                 */
ASSIGN 
       tb_spanish:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_spanish:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_SW:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_tray-2 IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R 1                                       */
ASSIGN 
       tb_tray-2:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_tray-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE 1                                               */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Ticket */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
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
            RUN CleanUp. 

        IF VALID-HANDLE (hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
                
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

        IF SELF:TYPE <> "Button" THEN  
        DO:
            APPLY "tab" TO SELF.
            RETURN NO-APPLY.
        END.
        ELSE 
        DO:
            APPLY "choose" TO SELF.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job1 C-Win
ON LEAVE OF begin_job1 IN FRAME FRAME-A /* Beginning  Job# */
DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no (INPUT "begin-only").
        ASSIGN 
            {&self-name}:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', {&self-name}:SCREEN-VALUE)).

        IF lv-format-f = "FibreFC" AND lv-format-c = "Artios" THEN
            RUN split-ship-proc.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job2 C-Win
ON LEAVE OF begin_job2 IN FRAME FRAME-A /* - */
DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no (INPUT "begin-only").
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
            RUN CleanUp.

        IF VALID-HANDLE (hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
                     
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
        DEFINE VARIABLE hold-title AS CHARACTER NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
            lv-ornt = lv-ornt:SCREEN-VALUE. 
            IF NOT lPrinterAllow AND (lv-format-f EQ "Colonial" OR lv-format-f EQ "CCC" ) AND rd-dest EQ 1 THEN
            DO:
                MESSAGE "Only Administrators can print a job card." VIEW-AS ALERT-BOX INFORMATION .
                RETURN.
            END.
       
    
            /* Task 10181302  */
            IF STRING(begin_job1:SCREEN-VALUE) <> "" THEN
                ASSIGN begin_job1:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE)).

            IF STRING(end_job1:SCREEN-VALUE) <> "" THEN
                ASSIGN end_job1:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job1:SCREEN-VALUE)).

            IF tb_tray-2:HIDDEN = NO THEN
                ASSIGN tb_tray-2.

            IF STRING(begin_job1:SCREEN-VALUE) = "" AND string(end_job1:SCREEN-VALUE) = "" THEN 
            DO:
                MESSAGE "Job Number can't be blank. Please enter a valid Job.." VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
            IF begin_job1:SCREEN-VALUE <> "" AND begin_job1:SCREEN-VALUE EQ end_job1:SCREEN-VALUE THEN 
            DO:

                FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                    AND job-hdr.job-no EQ STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE)) NO-LOCK NO-ERROR.

                IF NOT AVAILABLE job-hdr THEN 
                DO:
                    MESSAGE "Enter a Valid Job No.." VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                END.
            END.

            FOR EACH job-hdr 
                WHERE job-hdr.company         EQ cocode
                AND job-hdr.job-no            GE STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE))
                AND job-hdr.job-no            LE STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job1:SCREEN-VALUE))
                NO-LOCK :
                /*  FIND FIRST cust WHERE cust.company EQ cocode 
                      AND cust.cust-no EQ job-hdr.cust-no NO-LOCK NO-ERROR.
                  IF AVAIL cust THEN DO:
                      IF cust.cr-hold THEN */
                FIND FIRST oe-ord WHERE oe-ord.company EQ cocode 
                    AND oe-ord.ord-no EQ job-hdr.ord-no
                    /*AND oe-ord.job-no EQ job-hdr.job-no 
                    AND oe-ord.job-no2 EQ job-hdr.job-no2*/ NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN 
                DO:
                    IF oe-ord.stat EQ "H" OR oe-ord.priceHold THEN
                        IF NOT v-oe-ctrl THEN 
                        DO:
                            IF oe-ord.stat EQ "H" THEN cHoldMessage = "Hold".
                            ELSE cHoldMessage = "Price Hold". 
                            MESSAGE "Order " + string(job-hdr.ord-no) + " is on " + cHoldMessage + ".  Can not Print Job Card." VIEW-AS ALERT-BOX ERROR.
                            RETURN NO-APPLY.  /* task 03201401 */
                        END.
                END.
            END.

        END.  

        ASSIGN 
            lv-pdf-file       = INIT-dir +  "\Job" + TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', begin_job1, begin_job2)))
            s-prt-mstandard   = tb_prt-mch
            s-prt-shipto      = tb_prt-shipto
            s-prt-sellprc     = tb_prt-sellprc
            s-run-speed       = rd_print-speed = "S"
            s-sample-required = TB_sample_req
            hold-title        = c-win:TITLE

            /* gdm - 11030807 */  
            v-newdie          = NO
            v-newfilm         = NO
            v-newcombo        = NO
            lDraft            = LOGICAL(tb_draft:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

        IF tb_fold THEN 
        DO:
            /*lines-per-page = IF lv-format-f EQ "HOP" THEN 64 ELSE 58. */

            RUN run-report ("Fold").

            c-win:TITLE = "Folding Carton " + TRIM(c-win:TITLE).

            CASE rd-dest:
                WHEN 1 THEN 
                    DO:
                        IF lv-format-f = "prystup"  THEN .
                        ELSE RUN output-to-printer.
                    END.
                WHEN 2 THEN RUN output-to-screen.
                WHEN 3 THEN RUN output-to-file.
                WHEN 4 THEN 
                    DO:
                        /*run output-to-fax.*/
                        {custom/asifax.i &begin_cust=begin_job1
                            &END_cust=END_job1
                            &fax-subject=c-win:title
                            &fax-body="c-win title"
                            &fax-file=list-name }
                    END.
                WHEN 5 THEN 
                    DO:           
                        IF is-xprint-form THEN 
                        DO:
                            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                            {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject="Factory Ticket"
                             &mail-body="Factory Ticket"
                             &mail-file=lv-pdf-file + ".pdf" }             
                        END.
                        ELSE IF lv-format-f = "prystup"  THEN RUN ExcelEmail.

                            ELSE 
                            DO:
                                {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job1
                                  &END_cust=end_job1
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                            END.

                    END. 
                WHEN 6 THEN RUN output-to-port.
            END CASE. 

            c-win:TITLE = hold-title.     
        END.

        else IF tb_corr THEN 
        DO:
            /*lines-per-page = 0. ??? */

            RUN run-report ("Corr").

            c-win:TITLE = "Corrugated " + TRIM(c-win:TITLE).

            CASE rd-dest:
                WHEN 1 THEN RUN output-to-printer.
                WHEN 2 THEN RUN output-to-screen.
                WHEN 3 THEN RUN output-to-file.
                WHEN 4 THEN RUN output-to-fax.
                WHEN 5 THEN RUN output-to-mail.
                WHEN 6 THEN RUN output-to-port.
            END CASE. 
            c-win:TITLE = hold-title.      
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dept_codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dept_codes C-Win
ON HELP OF dept_codes IN FRAME FRAME-A /* Departments Code */
DO:
        DEFINE VARIABLE char-val    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ip-char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i-cnt       AS INTEGER   NO-UNDO.

        ASSIGN spec_codes
            ip-char-val = "".

        EMPTY TEMP-TABLE tt-specCd.
        DO i-cnt = 1 TO NUM-ENTRIES(spec_codes):

            IF TRIM(ENTRY(i-cnt,spec_codes))  EQ "" THEN NEXT.

            CREATE tt-specCd.
            ASSIGN 
                tt-char-val = TRIM(ENTRY(i-cnt,spec_codes)).
        END.

        FOR FIRST  tt-specCd NO-LOCK 
            BY tt-specCd.tt-char-val:

            ASSIGN 
                ip-char-val = TRIM(tt-specCd.tt-char-val).
        END.

        RUN cec/l-itspec.w (g_company, ip-char-val, OUTPUT char-val).
        IF char-val NE "" AND {&self-name}:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
        DO:
            ASSIGN
                {&self-name}:SCREEN-VALUE = {&self-name}:SCREEN-VALUE + "," + ENTRY(1,char-val)
                {&self-name}:SCREEN-VALUE = LEFT-TRIM({&self-name}:SCREEN-VALUE,",").
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dept_codes C-Win
ON LEAVE OF dept_codes IN FRAME FRAME-A /* Departments Code */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job1 C-Win
ON LEAVE OF end_job1 IN FRAME FRAME-A /* Ending Job# */
DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no (INPUT "All").
        ASSIGN 
            {&self-name}:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', {&self-name}:SCREEN-VALUE)).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job2 C-Win
ON LEAVE OF end_job2 IN FRAME FRAME-A /* - */
DO:
        IF {&self-name}:MODIFIED THEN RUN new-job-no (INPUT "All").
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
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
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
        DISPLAY lines-per-page WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON HELP OF run_format IN FRAME FRAME-A /* Format */
DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO .

        IF tb_fold:SCREEN-VALUE EQ "Yes" THEN 
        DO:
            RUN windows/l-syschrL.w (gcompany,"JOBCARDF",run_format:SCREEN-VALUE,OUTPUT char-val).
            IF char-val NE '' THEN
                run_format:SCREEN-VALUE = ENTRY(1,char-val).
            IF lv-format-f NE run_format:SCREEN-VALUE THEN 
            DO:
                ASSIGN 
                    lv-format-f = run_format:SCREEN-VALUE.
                RUN  pRunFormatValueChanged .
            END.
        END.
        ELSE  
        DO:
            RUN windows/l-syschrL.w (gcompany,"JOBCARDC",run_format:SCREEN-VALUE,OUTPUT char-val).
            IF char-val NE '' THEN
                run_format:SCREEN-VALUE = ENTRY(1,char-val).
            IF lv-format-c NE run_format:SCREEN-VALUE THEN 
            DO:
                ASSIGN 
                    lv-format-c = run_format:SCREEN-VALUE.
                RUN  pRunFormatValueChanged .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON LEAVE OF run_format IN FRAME FRAME-A /* Format */
DO:
        ASSIGN run_format.
   
        IF tb_fold:SCREEN-VALUE EQ "Yes" THEN 
        DO:
            IF lv-format-f NE run_format THEN 
            DO:
                ASSIGN 
                    lv-format-f = run_format.
                RUN  pRunFormatValueChanged .
            END.
        END.
        ELSE IF tb_corr:SCREEN-VALUE EQ "Yes" THEN 
            DO:
                IF lv-format-c NE run_format THEN 
                DO:
                    ASSIGN 
                        lv-format-c = run_format.
                    RUN  pRunFormatValueChanged .
                END.
            END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_codes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_codes C-Win
ON HELP OF spec_codes IN FRAME FRAME-A /* Spec Codes */
DO:
        DEFINE VARIABLE char-val    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ip-char-val AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i-cnt       AS INTEGER   NO-UNDO.

        ASSIGN spec_codes
            ip-char-val = "".

        EMPTY TEMP-TABLE tt-specCd.
        DO i-cnt = 1 TO NUM-ENTRIES(spec_codes):

            IF TRIM(ENTRY(i-cnt,spec_codes))  EQ "" THEN NEXT.

            CREATE tt-specCd.
            ASSIGN 
                tt-char-val = TRIM(ENTRY(i-cnt,spec_codes)).
        END.

        FOR FIRST  tt-specCd NO-LOCK 
            BY tt-specCd.tt-char-val:

            ASSIGN 
                ip-char-val = TRIM(tt-specCd.tt-char-val).
        END.

        RUN cec/l-itspec.w (g_company, ip-char-val, OUTPUT char-val).
        IF char-val NE "" AND {&self-name}:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
        DO:
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_app-unprinted
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_app-unprinted C-Win
ON VALUE-CHANGED OF tb_app-unprinted IN FRAME FRAME-A /* Print All Unprinted App. Tickets? */
DO:
        DEFINE BUFFER b-job-hdr FOR job-hdr.
        DEFINE BUFFER b-job     FOR job.

        DEFINE VARIABLE v-min-job-no AS CHARACTER INIT "zzzzzzzzz000" NO-UNDO.
        DEFINE VARIABLE v-max-job-no AS CHARACTER NO-UNDO.

        ASSIGN {&self-name}.

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

                IF STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', b-job-hdr.job-no, b-job-hdr.job-no2)) LT v-min-job-no THEN
                    v-min-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', b-job-hdr.job-no, b-job-hdr.job-no2)).

                IF STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', b-job-hdr.job-no, b-job-hdr.job-no2)) GT v-max-job-no THEN
                    v-max-job-no = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', b-job-hdr.job-no, b-job-hdr.job-no2)).
            END.

            ASSIGN 
                begin_job1:SCREEN-VALUE = SUBSTRING(v-min-job-no,1,9)
                begin_job2:SCREEN-VALUE = SUBSTRING(v-min-job-no,10)
                end_job1:SCREEN-VALUE   = SUBSTRING(v-max-job-no,1,9)
                end_job2:SCREEN-VALUE   = SUBSTRING(v-max-job-no,10)
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_attched
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_attched C-Win
ON VALUE-CHANGED OF tb_attched IN FRAME FRAME-A /* Print Attachment Images */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_box
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_box C-Win
ON VALUE-CHANGED OF tb_box IN FRAME FRAME-A /* Print Box Design? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_corr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_corr C-Win
ON VALUE-CHANGED OF tb_corr IN FRAME FRAME-A /* Corrugated */
DO:

        ASSIGN {&self-name}.

        IF tb_corr:SCREEN-VALUE EQ "Yes" THEN
            ASSIGN run_format:SCREEN-VALUE = lv-format-c .
        ELSE IF tb_fold:SCREEN-VALUE EQ "Yes" THEN
                ASSIGN run_format:SCREEN-VALUE = lv-format-f .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_DC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_DC C-Win
ON VALUE-CHANGED OF tb_DC IN FRAME FRAME-A /* Print Die Cutter Card */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_draft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_draft C-Win
ON VALUE-CHANGED OF tb_draft IN FRAME FRAME-A /* Mark as Draft? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fgimage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fgimage C-Win
ON VALUE-CHANGED OF tb_fgimage IN FRAME FRAME-A /* Print FG Item Image? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fold C-Win
ON VALUE-CHANGED OF tb_fold IN FRAME FRAME-A /* Folding Carton */
DO:
        ASSIGN {&self-name}.
        IF tb_fold:SCREEN-VALUE EQ "Yes" THEN
            ASSIGN run_format:SCREEN-VALUE = lv-format-f .
        ELSE IF tb_corr:SCREEN-VALUE EQ "Yes" THEN
                ASSIGN run_format:SCREEN-VALUE = lv-format-c .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_freeze-note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_freeze-note C-Win
ON VALUE-CHANGED OF tb_freeze-note IN FRAME FRAME-A /* Freeze Job Notes? */
DO:
        DEFINE VARIABLE v-password AS CHARACTER.
  
        /* Is a password required to change, and is the screen value now different from the "stored" value? */
        IF v-freezenote-log 
            AND lFreezeNoteVal NE tb_freeze-note:CHECKED THEN 
        DO:
            RUN custom/d-passwd.w (OUTPUT v-password).
            /* If the password matches the value in NK1, allow the screen value to change, and set the "stored" value */
            IF v-password EQ v-freezenotes-pass THEN ASSIGN 
                    tb_freeze-note:CHECKED = NOT lFreezeNoteVal
                    lFreezeNoteVal         = tb_freeze-note:CHECKED.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_GL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_GL C-Win
ON VALUE-CHANGED OF tb_GL IN FRAME FRAME-A /* Print Gluer /  Window Card */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_make_hold
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_make_hold C-Win
ON VALUE-CHANGED OF tb_make_hold IN FRAME FRAME-A /* Make  Hold? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_PR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_PR C-Win
ON VALUE-CHANGED OF tb_PR IN FRAME FRAME-A /* Print Printer Card */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-metric
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-metric C-Win
ON VALUE-CHANGED OF tb_print-metric IN FRAME FRAME-A /* Print Metric Size */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-dmi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-dmi C-Win
ON VALUE-CHANGED OF tb_prt-dmi IN FRAME FRAME-A /* Print DMI Barcode page? */
DO:
        ASSIGN {&self-name}.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-label
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-label C-Win
ON VALUE-CHANGED OF tb_prt-label IN FRAME FRAME-A /* Print Label Info? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-mch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-mch C-Win
ON VALUE-CHANGED OF tb_prt-mch IN FRAME FRAME-A /* Print Machine Standard? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-rev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-rev C-Win
ON VALUE-CHANGED OF tb_prt-rev IN FRAME FRAME-A /* Print Revision Number? */
DO:
        ASSIGN {&self-name}.

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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-shipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-shipto C-Win
ON VALUE-CHANGED OF tb_prt-shipto IN FRAME FRAME-A /* Print Shipto? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Reprint Tickets? */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_RS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_RS C-Win
ON VALUE-CHANGED OF tb_RS IN FRAME FRAME-A /* Print Sheeter Card */
DO:
        ASSIGN {&self-name}.
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_spanish
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_spanish C-Win
ON VALUE-CHANGED OF tb_spanish IN FRAME FRAME-A /* Print Spanish */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_SW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_SW C-Win
ON VALUE-CHANGED OF tb_SW IN FRAME FRAME-A /* Print Shrink Wrap Card */
DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tray-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tray-2 C-Win
ON VALUE-CHANGED OF tb_tray-2 IN FRAME FRAME-A /* Copy 2 and 3 in Tray 2 (Artios)? */
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

    DEFINE VARIABLE plev AS INTEGER NO-UNDO.



    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    production = NO.

    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode
        NO-LOCK NO-ERROR.
    IF AVAILABLE oe-ctrl THEN
        ASSIGN v-oe-ctrl = oe-ctrl.p-fact .

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTES"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.NAME    = "FREEZENOTES"
            sys-ctrl.module  = "OU5"
            sys-ctrl.descrip = "Default Toggle to User's Last Action?".
    END.

    v-freezenotes-log = sys-ctrl.log-fld.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTE"
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.NAME    = "FREEZENOTE"
            sys-ctrl.module  = "OU5"
            sys-ctrl.descrip = "Prompt for password on Freeze Notes?".
    END.

    v-freezenote-log = sys-ctrl.log-fld.

    IF v-freezenote-log THEN
        v-freezenotes-pass = sys-ctrl.char-fld.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
        NO-LOCK NO-ERROR.
    ASSIGN
        tb_fold = NOT AVAILABLE sys-ctrl           OR
             sys-ctrl.char-fld EQ "Both"  OR
             sys-ctrl.char-fld EQ "Foldware"
        tb_corr = AVAILABLE sys-ctrl AND
             (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").

    {sys/inc/jobcard.i "F"}
    ASSIGN
        lv-format-f       = sys-ctrl.char-fld
        lv-int-f          = sys-ctrl.int-fld
        lv-default-f      = sys-ctrl.char-fld
        dDecimalFoldValue = sys-ctrl.dec-fld .
    IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0*/
        LOOKUP(lv-format-f,"Interpac,FibreFC,HPB,metro,Dayton,Livngstn,CentBox,Wingate,Frankstn,Preferred,Colonial,xml,Unipak,Ottpkg,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Dee,Carded,burt,McLean,Carded2,GPI-STN,Coburn,Knight***") > 0 THEN lines-per-page = 55.

    {sys/inc/jobcard.i "C"}
    ASSIGN
        lv-format-c  = sys-ctrl.char-fld
        lv-int-c     = sys-ctrl.int-fld
        lv-default-c = sys-ctrl.char-fld.  
 
    
    IF TRIM(begin_job1:SCREEN-VALUE) NE ""                          AND
        TRIM(begin_job1:SCREEN-VALUE) EQ TRIM(end_job1:SCREEN-VALUE) AND
        INT(begin_job2:SCREEN-VALUE)  EQ INT(end_job2:SCREEN-VALUE)  THEN 
    DO:
        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no EQ STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE))
            AND job-hdr.job-no2 EQ INT(begin_job2:SCREEN-VALUE)
            NO-ERROR.

        IF AVAILABLE job-hdr THEN 
        DO:
            FIND FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "JOBCARDC" AND
                sys-ctrl-shipto.cust-vend = YES AND
                sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                sys-ctrl-shipto.char-fld > ''
                NO-LOCK NO-ERROR.

            IF AVAILABLE sys-ctrl-shipto THEN 
                lv-format-f = sys-ctrl-shipto.char-fld .
            ELSE 
                lv-format-f = lv-default-f .

            FIND FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "JOBCARDF" AND
                sys-ctrl-shipto.cust-vend = YES AND
                sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                sys-ctrl-shipto.char-fld > ''
                NO-LOCK NO-ERROR.
            IF AVAILABLE sys-ctrl-shipto THEN 
                lv-format-f = sys-ctrl-shipto.char-fld .
            ELSE 
                lv-format-f = lv-default-f .
        END.
    END.


    /* gdm - 11120805*/
    ASSIGN 
        tb_fgimage:SENSITIVE             = NO
        revsn_no:HIDDEN IN FRAME FRAME-A = YES      
        tb_prt-rev:SENSITIVE             = NO.
         
    IF  XMLJobTicket-log  THEN 
        tb_ExportXML:HIDDEN  IN FRAME FRAME-A = NO.
    ELSE 
        tb_ExportXML:HIDDEN  IN FRAME FRAME-A = YES.
  
    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OU5" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .
    {methods/nowait.i}
    DO WITH FRAME {&frame-name}:
      
        {custom/usrprint.i}
   &IF DEFINED(TSJobCard) NE 0 &THEN
        ASSIGN
            begin_job1:SCREEN-VALUE = ip-job-no
            begin_job2:SCREEN-VALUE = STRING(ip-job-no2)
            end_job1:SCREEN-VALUE   = ip-job-no
            end_job2:SCREEN-VALUE   = STRING(ip-job-no2)
            .
   &ENDIF
  

        revsn_no:HIDDEN IN FRAME FRAME-A           = TRUE.
   
        IF INDEX(PROGRAM-NAME(4),"mainmenu") GT 0 
            THEN ASSIGN fl-jobord              = 0
                fl-jobord:SCREEN-VALUE = "0".

        ASSIGN
            tb_approve:SCREEN-VALUE       = "no"
            TB_sample_req:HIDDEN          = TRUE
            tb_app-unprinted:SCREEN-VALUE = "no"
            tb_app-unprinted:HIDDEN       = TRUE
            tb_prompt-ship:SCREEN-VALUE   = "no".

        IF v-freezenotes-log = NO THEN ASSIGN
                tb_freeze-note:SENSITIVE = FALSE
                tb_freeze-note:CHECKED   = FALSE
                lFreezeNoteVal           = FALSE.
    
        plev = 1.
        REPEAT WHILE PROGRAM-NAME(plev) NE ?:
            IF PROGRAM-NAME(plev) MATCHES "*w-jobapp*" THEN 
            DO:
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

        IF TRIM(begin_job1:SCREEN-VALUE) NE ""                          AND
            TRIM(begin_job1:SCREEN-VALUE) EQ TRIM(end_job1:SCREEN-VALUE) AND
            INT(begin_job2:SCREEN-VALUE)  EQ INT(end_job2:SCREEN-VALUE)  THEN 
        DO:
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company       EQ cocode
                AND job-hdr.job-no          EQ STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE))
                AND job-hdr.job-no2         EQ INT(begin_job2:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE job-hdr THEN 
            DO:
                FIND FIRST job NO-LOCK
                    WHERE job.company EQ job-hdr.company
                    AND job.job     EQ job-hdr.job
                    AND job.job-no  EQ job-hdr.job-no
                    AND job.job-no2 EQ job-hdr.job-no2
                    NO-ERROR.
                IF AVAILABLE job THEN  
                DO:
                    IF job.cs-to-pr = TRUE THEN 
                        ASSIGN
                            tb_approve:SCREEN-VALUE = "Yes" 
                            tb_approve:HIDDEN       = NO
                            tb_approve:SENSITIVE    = NO .
                    ELSE
                        ASSIGN
                            tb_approve:SCREEN-VALUE = "no"   
                            tb_approve:HIDDEN       = NO
                            tb_approve:SENSITIVE    = YES .

                END.
           
                IF job-hdr.freezeNote EQ YES THEN ASSIGN
                        tb_freeze-note:CHECKED = TRUE
                        lFreezeNoteVal         = TRUE.
                IF job-hdr.splitShip EQ YES THEN              
                    tb_prompt-ship:CHECKED = TRUE.

            END.  /* avail job-hdr */
        END.
  
        IF NOT tb_prt-rev THEN
            revsn_no:HIDDEN IN FRAME FRAME-A = YES .
        ELSE
            revsn_no:HIDDEN IN FRAME FRAME-A = NO .
   
        IF TRIM(begin_job1:SCREEN-VALUE) NE ""                          AND
            TRIM(begin_job1:SCREEN-VALUE) EQ TRIM(end_job1:SCREEN-VALUE) AND
            INT(begin_job2:SCREEN-VALUE)  EQ INT(end_job2:SCREEN-VALUE)  THEN 
        DO:
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company       EQ cocode
                AND job-hdr.job-no          EQ STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE))
                AND job-hdr.job-no2         EQ INT(begin_job2:SCREEN-VALUE)
                NO-ERROR.
            IF AVAILABLE job-hdr THEN 
            DO:
                tb_reprint:SCREEN-VALUE = STRING(job-hdr.ftick-prnt).
                IF production THEN 
                DO:
                    FIND FIRST job NO-LOCK
                        WHERE job.company EQ job-hdr.company
                        AND job.job     EQ job-hdr.job
                        AND job.job-no  EQ job-hdr.job-no
                        AND job.job-no2 EQ job-hdr.job-no2
                        NO-ERROR.
                    IF AVAILABLE job THEN tb_reprint:SCREEN-VALUE = STRING(job.pr-printed). 

                END.
            END.
        END.    

        APPLY "entry" TO begin_job1.    
    END.  

    RUN new-job-no (INPUT "All").
  
    IF NOT lAsiUser THEN 
    DO:
        RUN_format:HIDDEN IN FRAME FRAME-A = YES .
    END.
    ELSE 
    DO:
        IF tb_fold:SCREEN-VALUE IN FRAME FRAME-A EQ "Yes" THEN
            RUN_format:SCREEN-VALUE IN FRAME FRAME-A = lv-format-f .
        ELSE IF tb_corr:SCREEN-VALUE IN FRAME FRAME-A EQ "Yes" THEN
                RUN_format:SCREEN-VALUE IN FRAME FRAME-A = lv-format-c .
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddWorkSheet C-Win 
PROCEDURE AddWorkSheet :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRange   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE iRow     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iCol     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iRows    AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iCols    AS INTEGER          NO-UNDO.
    DEFINE VARIABLE chQuery  AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE my-range AS cha              NO-UNDO.

    ASSIGN 
        iCols = 8.

  chExcel:workbooks:ITEM(1):worksheets:ADD(,chWorkSheet). 
    IF VALID-HANDLE (chworksheet) 
        THEN  RELEASE OBJECT chworksheet.
    /* Select 2nd sheet */
    chWorksheet = chExcel:sheets:ITEM(2).
    chWorksheet:NAME = "Job Card 2nd Page".
    chworksheet:SELECT. 

    chWorkSheet:PageSetup:PaperSize = 5 /*1 - xlPaperLetter, 5 - xlPaperLegal */ .
    chWorkSheet:PageSetup:Orientation = 2 /*xlLandscape*/ .
    /*chWorkSheet:PageSetup:PrintArea = "A1:O28". */
    chWorkSheet:PageSetup:Zoom = FALSE.
    chWorkSheet:PageSetup:FitToPagesWide = 1.
    chWorkSheet:PageSetup:FitToPagesTall = 1.

    /* Open 2nd file into 2nd sheet */
    chQuery = chWorkSheet:QueryTables:Add("TEXT;" + list-name + "-2",chWorkSheet:cells(1,1)).
    /*             h-excel:Workbooks:OPEN(F-OutFile). */
    chQuery:REFRESH().

    cRange = setRange("A1",16) + "1".
    chWorkSheet:Range(cRange):SELECT().   
    chExcel:Selection:Font:Name = "Arial".
    chExcel:SELECTION():FONT:SIZE = 18.

    /*chExcel:SELECTION():HorizontalAlignment = -4108 /* center */ */.

    /*ASSIGN my-range = "A1:O28"*/
    /*chWorksheet:Range(my-range):COLUMNS:SELECT()*/
    /*chExcel:SELECTION():COLUMNS:autofit()
    chWorksheet2:COLUMNS:autofit() */
    /*
       chExcel:SELECTION():ShrinkToFit = True
       chExcel:Selection:Font:Bold = TRUE.

       ShrinkToFit = False
       MergeCells = False
      */ 


    /* 2nd page BreakOut */
    cRange = setRange("A2",16) + "28".
    chWorkSheet:Range(cRange):SELECT().   
    chExcel:SELECTION():FONT:SIZE = 10.
    chExcel:Selection:Font:Bold = FALSE.
    chExcel:SELECTION():HorizontalAlignment = -4108.  /*center*/
    chExcel:SELECTION():WrapText = TRUE.


    chWorkSheet:Cells(1,1):SELECT().     
    chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
    chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */
    chExcel:SELECTION():RowHeight = 40.00.
    chWorkSheet:Cells(1,9):SELECT().     
    chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
    chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
    chWorkSheet:Cells(1,16):SELECT().     
    chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
    chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */


    cRange = setRange("A1",16) + "1".
    chWorkSheet:Range(cRange):SELECT().     
    chExcel:SELECTION():Borders(9):linestyle = 1. /*Border top*/
    chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */


    DO iRows = 2 TO 40:

        DO iCol = 1 TO 16:  /* 2nd BreakOut Page */  

            chWorkSheet:Cells(iRows,iCol):SELECT().     
            chExcel:SELECTION():Borders(9):linestyle = 1. /*Border top*/
            chExcel:SELECTION():Borders(9):weight = IF iRows = 2 THEN 4 ELSE 2.  /* xLThick = 4, XLThin = 2 */
            /*chExcel:SELECTION():Borders(9):linestyle = 1. /*Border bottom*/
            chExcel:SELECTION():Borders(9):weight = IF iRows = 2 OR iRows = 40 THEN 4 ELSE 2.  /* xLThick = 4, XLThin = 2 */
            */
            /*chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
            chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */
            */
            chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
            chExcel:SELECTION():Borders(10):weight = IF icol = 9 OR iCol = 16 THEN 4 ELSE 2.  /* xLThick = 4, XLThin = 2 */
            IF iCol = 1 THEN 
            DO:
                chWorkSheet:Cells(iRows,iCol):SELECT().     
                chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
                chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */
            END.


            IF iRows = 2 THEN
                CASE iCol:
                    WHEN 1 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 19.43.
                    WHEN 2 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 30.57.
                    WHEN 3 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 6.86 .
                    WHEN 4 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 12.43.
                    WHEN 5 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 5.14.
                    WHEN 6 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 16.14.
                    WHEN 7 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 13.14.
                    WHEN 8 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 11.71.
                    WHEN 9 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 10.43. /* del date*/
                    WHEN 10 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 14.
                    WHEN 12 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 15.
                    WHEN 13 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 15.
                    WHEN 14 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 15.
                    WHEN 15 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 15.
                    WHEN 16 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 15.
                    WHEN 16 THEN 
                        ASSIGN 
                            chExcel:SELECTION():ColumnWidth = 15.
                END CASE.     

            IF iRows = 40 THEN 
            DO:
                chWorkSheet:Cells(iRows,iCol):SELECT().     
                chExcel:SELECTION():Borders(9):linestyle = 1. /*Border left*/
                chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */

            END.
        END.

    END.  /*iRows*/
/*
DO iRow = 2 TO 28:
   chWorkSheet:Cells(iRow,1):SELECT().     
   chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
   chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     

   chWorkSheet:Cells(iRow,icols + 7):SELECT().     
   chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
   chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */

END.
*/

/* chworksheet:CLOSE()  .  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CleanUp C-Win 
PROCEDURE CleanUp :
/*------------------------------------------------------------------------------
      Purpose:    Clean up routine.
      Parameters: <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /* Excel Handle */
    DEFINE VARIABLE chExcelApplication AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkbook         AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkSheet        AS COMPONENT-HANDLE NO-UNDO.
    DEFINE VARIABLE chHyper            AS COMPONENT-HANDLE NO-UNDO.

    /* Connect to the running Excel session. */
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.

    IF VALID-HANDLE (chExcelApplication) THEN
    DO:
        chWorkBook:close()                NO-ERROR.
        chExcelApplication:Quit()         NO-ERROR.
        chExcelApplication:Quit(0)        NO-ERROR.

        /* RELEASE OBJECTS */
        RELEASE OBJECT chWorkbook         NO-ERROR.
        RELEASE OBJECT chWorkSheet        NO-ERROR.
        RELEASE OBJECT chHyper            NO-ERROR.
        RELEASE OBJECT chExcelApplication NO-ERROR.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateExcel C-Win 
PROCEDURE CreateExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
  DEF VAR cRange       AS CHAR NO-UNDO.
  DEF VAR iRow         AS INT  NO-UNDO.
  DEF VAR iCol         AS INT  NO-UNDO.
  DEF VAR iRows        AS INT  NO-UNDO.
  DEF VAR iCols        AS INT  NO-UNDO.
  DEF VAR cImageFile AS cha NO-UNDO.

  ASSIGN iCols = 10.  /*J*/

  DEFINE VARIABLE    cFileName   AS CHAR       NO-UNDO.
  cFileName = "c:\tmp\prystjob" + STRING(TIME) + ".xls" .
  OS-COPY VALUE(list-name) VALUE(cFileName).

  /* Create excel automation*/
  CREATE "excel.application" chExcel.
  /* Open an Excel    */
  chExcel:Workbooks:OPEN(cFileName).

  /* open  sheet*/
  chWorkbook  = chExcel:ActiveWorkbook.
  chWorkSheet = chExcel:Sheets:Item(1).
  chWorkSheet:PageSetup:Orientation = 2 /*xlLandscape*/ .
  /* PaperSize = 1 - xlPaperLetter, 5 - xlPaperLegal */
  chWorksheet:NAME = "Job Card FrontPage".
  /*chWorkSheet:PageSetup:PrintArea = "A1:J28". */
  chWorkSheet:PageSetup:Zoom = FALSE.
  chWorkSheet:PageSetup:FitToPagesWide = 1.
  chWorkSheet:PageSetup:FitToPagesTall = 1.
  /*
  chWorkSheet:PageSetup:FitToPagesWide = 1.
                       FitToPagesTall = 1 
  */
  chExcel:ScreenUpdating = no.

  cRange = setRange("A1",iCols) + "5".
  chWorkSheet:Range(cRange):SELECT().   
  chExcel:Selection:Font:Name = "Calibri".
  chExcel:SELECTION():HorizontalAlignment = -4108. /*xlCenter    */
  /*chExcel:SELECTION():FONT:SIZE = 8.*/

  cRange = setRange("A1",10) + "1".
  chWorkSheet:Range(cRange):SELECT().   
  chExcel:SELECTION():FONT:SIZE = 20.
  setBold().

  cRange = setRange("A4",10) + "6".
  chWorkSheet:Range(cRange):SELECT().   
  chExcel:SELECTION():FONT:SIZE = 14.
  /*setBold().*/

  cRange = setRange("A6",10) + "9".
  chWorkSheet:Range(cRange):SELECT().   
  chExcel:SELECTION():HorizontalAlignment = -4131. /*xlleft    */

  cRange = setRange("A10",iCols) + "18".
  chWorkSheet:Range(cRange):SELECT().   
  chExcel:Selection:Font:Name = "Calibri".
  chExcel:SELECTION():HorizontalAlignment = -4108. /*xlCenter    */

  file-info:file-name = "images\prystup.bmp".
  cImageFile = FILE-INFO:FULL-PATHNAME.
  chWorkSheet:Shapes:AddPicture (cImageFile,TRUE,FALSE,1,1,105,45) NO-ERROR.

  /*chWorkSheet:Cells(10,1):SELECT().
  chExcel:SELECTION():ColumnWidth = 10.68.
  chWorkSheet:Cells(10,2):SELECT().
  chExcel:SELECTION():ColumnWidth = 9.89.
  cRange = setRange("C10",iCols + 2) + "10".
  chWorkSheet:Range(cRange):SELECT().
  chExcel:SELECTION():ColumnWidth = 10.00.
  */

  /*Mach Op desc and speed - not bold */
  /* error when run
  chWorkSheet:row(18):SELECT().
  chExcel:Selection:Font:Bold = False.
  chWorkSheet:row(20):SELECT().
  chExcel:Selection:Font:Bold = FALSE.
  */
  /* job# box bold */
   /* xlEdgeTop = 8, xlEdgeBottom = 9, xlEdgeLeft = 7,xlEdgeRight = 10*/
  chWorkSheet:Cells(4,1):SELECT().
  chExcel:SELECTION():Borders(7):linestyle = 1. /*Border Left*/
  chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */

  chWorkSheet:Cells(4,iCols):SELECT().
  chExcel:SELECTION():Borders(7):linestyle = 1. /*Border Left*/
  chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */
  chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
  chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */

  cRange = setRange("A10",10) + "12".
  chWorkSheet:Range(cRange):SELECT().   
  setBold().

  DO iCol = 1 TO iCols: /* one row, multi celss*/
     /*chWorkSheet:row(5):SELECT().
       chExcel:SELECTION():RowHeight = 25.00.*/
     chWorkSheet:Cells(1,iCol):SELECT().
     chExcel:SELECTION():RowHeight = 26.25.
     /*chExcel:Selection:Font:size = 20.*/

     chWorkSheet:Cells(4,iCol):SELECT().
     chExcel:SELECTION():RowHeight = 25.25.
     chExcel:SELECTION():Borders(8):linestyle = 1. /*Border top*/
     chExcel:SELECTION():Borders(8):weight = 4.  /* xLThick = 4, XLThin = 2 */
     chExcel:SELECTION():Borders(9):linestyle = 1.  /*Border bottom */
     chExcel:SELECTION():Borders(9):weight = 4.

     /*chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
     chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
     chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
     chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
     */
     chWorkSheet:Cells(9,iCol):SELECT().
     chExcel:SELECTION():Borders(9):linestyle = 1.  /*Border bottom */
     chExcel:SELECTION():Borders(9):weight = 4.

     IF iCol <= 9  THEN DO:
         chWorkSheet:Cells(10,iCol):SELECT().     
         /*chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */
         */
         chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
         chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */

         /*chWorkSheet:Cells(11,iCol):SELECT().     
         chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
         chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
         */
         chWorkSheet:Cells(11,iCol):SELECT().     
         /*chExcel:SELECTION():Borders(8):linestyle = 1. /*Border top*/
         chExcel:SELECTION():Borders(8):weight = 4.  /* xLThick = 4, XLThin = 2 */
         */
         chExcel:SELECTION():Borders(9):linestyle = 1. /*Border bottom*/
         chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */
         chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
         chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */

         chWorkSheet:Cells(12,iCol):SELECT().     
         chExcel:SELECTION():Borders(9):linestyle = 1. /*Border bottom*/
         chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */
         chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
         chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
         /*chExcel:SELECTION():NumberFormat = "0" */

         chWorkSheet:Cells(13,iCol):SELECT().     
         chExcel:SELECTION():Borders(9):linestyle = 1. /*Border bottom*/
         chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */
        /* chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
         chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
        */
     END.

     chWorkSheet:Cells(14,iCol):SELECT().     
     chExcel:SELECTION():Borders(8):linestyle = 1. /*Border top*/
     chExcel:SELECTION():Borders(8):weight = 4.  /* xLThick = 4, XLThin = 2 */
     chExcel:Selection:Font:Bold = true.

     chWorkSheet:Cells(16,iCol):SELECT().
     chExcel:Selection:Font:Bold = true.

     chWorkSheet:Cells(17,iCol):SELECT().     
     chExcel:SELECTION():Borders(9):linestyle = 1. /*Border bottom*/
     chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */
     chExcel:Selection:Font:Bold = False.

     chWorkSheet:Cells(19,iCol):SELECT().     
     chExcel:SELECTION():Borders(8):linestyle = 1. /*Border top*/
     chExcel:SELECTION():Borders(8):weight = 4.  /* xLThick = 4, XLThin = 2 */
     chExcel:Selection:Font:Bold = true.

     /* bottom line line 33 */
     chWorkSheet:Cells(32,iCol):SELECT().     
     chExcel:SELECTION():Borders(9):linestyle = 1. /*Border bottom*/
     chExcel:SELECTION():Borders(9):weight = 4.  /* xLThick = 4, XLThin = 2 */

      CASE iCol:
         WHEN 1 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 13.57.
         WHEN 2 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 14.14.
         WHEN 3 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 8.43.  /*5.43*/ 
         WHEN 4 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 13.43. /*8.43*/
         WHEN 5 THEN ASSIGN chExcel:SELECTION():ColumnWidth =  8.71.
         WHEN 6 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 17.57.
         WHEN 7 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 13.71.
         WHEN 8 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 12.29 /* 9.29*/ . /* H*/
         WHEN 9 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 13.71 /*18.71*/ . /*I*/
         WHEN 10 THEN ASSIGN chExcel:SELECTION():ColumnWidth = 13.14. /*J*/

     END CASE.


  END.
  /* notes - not bold */
  chWorkSheet:Cells(22,1):SELECT().
  chExcel:Selection:Font:Bold = False.
  chWorkSheet:Cells(23,1):SELECT().
  chExcel:Selection:Font:Bold = False.
  chWorkSheet:Cells(24,1):SELECT().
  chExcel:Selection:Font:Bold = False.
  chWorkSheet:Cells(25,1):SELECT().
  chExcel:Selection:Font:Bold = False.
  chWorkSheet:Cells(26,1):SELECT().
  chExcel:Selection:Font:Bold = False.
  chWorkSheet:Cells(27,1):SELECT().
  chExcel:Selection:Font:Bold = False.


  DO iRow = 5 TO 16:
     IF iRow = 13 OR iRow = 18 THEN NEXT.

     chWorkSheet:Cells(iRow,1):SELECT().     
     chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
     chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
     IF iRow <= 9 THEN DO:
       chWorkSheet:Cells(iRow,10):SELECT().     
       chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
       chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
     END.
  END.

  DO iRow = 14 TO 18:
     IF iRow < 18 THEN DO:     
         chWorkSheet:Cells(iRow,1):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
         /*chExcel:SELECTION():ColumnWidth = 32.*/

         /*chWorkSheet:Cells(iRow,2):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
         chExcel:SELECTION():ColumnWidth = 32.
         */
         chWorkSheet:Cells(iRow,iCols):SELECT().     
         chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
         chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */


         chWorkSheet:Cells(iRow,3):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
         /*chExcel:SELECTION():ColumnWidth = 32.*/

         /*chWorkSheet:Cells(iRow,4):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
         chExcel:SELECTION():ColumnWidth = 32.
         */
         chWorkSheet:Cells(iRow,5):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
         /*chExcel:SELECTION():ColumnWidth = 32.*/

         chWorkSheet:Cells(iRow,7):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     

         chWorkSheet:Cells(iRow,9):SELECT().     
         chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
         chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */     
     END.

  END.

  cRange = setRange("A21",iCols) + "33".
  chWorkSheet:Range(cRange):SELECT().   
  chExcel:SELECTION():HorizontalAlignment = -4131. /* xlLeft */

  DO iRow = 19 TO 32:
     chWorkSheet:Cells(iRow,1):SELECT().         
     chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
     chExcel:SELECTION():Borders(7):weight = 4.  /* xLThick = 4, XLThin = 2 */ 
     chWorkSheet:Cells(iRow,iCols):SELECT().     
     chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
     chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
     chWorkSheet:Cells(iRow,6):SELECT().     
     chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
     chExcel:SELECTION():Borders(10):weight = 4.  /* xLThick = 4, XLThin = 2 */
  END.

  /* merge cell */
  chexcel:range("D4:E4"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.

  chexcel:range("A14:B14"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("C14:D14"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("E14:F14"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("G14:H14"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("I14:J14"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.

  chexcel:range("A15:B15"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("C15:D15"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("E15:F15"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("G15:H15"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("I15:J15"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.

  chexcel:range("A16:B16"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("C16:D16"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("E16:F16"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("G16:H16"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("I16:J16"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.

  chexcel:range("A17:B17"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("C17:D17"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("E17:F17"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("G17:H17"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.
  chexcel:range("I17:J17"):SELECT().
  chexcel:SELECTION():MergeCells = TRUE.
  chexcel:SELECTION():WrapText = TRUE.


  /*
  chWorkSheet:Cells(14 + iRows,2):SELECT().
  /*chExcel:SELECTION():FONT:Italic = TRUE.*/
  /* xlEdgeTop = 8, xlEdgeBottom = 9, xlEdgeLeft = 7,xlEdgeRight = 10*/
  chExcel:SELECTION():Borders(8):linestyle = 1. /*Border top*/
  chExcel:SELECTION():Borders(8):weight = 4.  /* xLThick = 4, XLThin = 2 */
  chExcel:SELECTION():Borders(9):linestyle = 1.  /*Border bottom */
  chExcel:SELECTION():Borders(9):weight = 4.
  chExcel:SELECTION():Borders(7):linestyle = 1. /*Border left*/
  chExcel:SELECTION():Borders(7):weight = 2.  /* xLThick = 4, XLThin = 2 */
  chExcel:SELECTION():Borders(10):linestyle = 1. /*Border right*/
  chExcel:SELECTION():Borders(10):weight = 2.  /* xLThick = 4, XLThin = 2 */
  chExcel:SELECTION():WrapText = TRUE.
  /*setValue("Stations in Italics are Agent Locations").  */
  */


  RUN AddWorkSheet.

  chExcel:ScreenUpdating = YES. /* no - to fast generation */

  /*chWorkSheet:Sheets("Job Card FrontPage"):Select(). */
  cExcelOutput = cFileName.
  chexcel:worksheets:ITEM(1):SELECT.
  chexcel:CELLS(1,1):SELECT.

  chExcel:VISIBLE = TRUE.
  /*
  IF rd-dest = 2 /* to screen */ THEN chExcel:VISIBLE = TRUE.
  ELSE IF rd-dest = 1 /* to Print */ THEN do:
      chExcel:VISIBLE = FALSE.
      chExcel:Dialogs(8):Show.
  END.
  ELSE chExcel:VISIBLE = FALSE.
  */

  RELEASE OBJECT chWorksheet.
  RELEASE OBJECT chWorkbook.
  RELEASE OBJECT    chExcel .
  /*============*/
*/

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
          tb_print-metric tbPageBreakByForm spec_codes tb_prt-rev revsn_no 
          tb_prt-dmi rd_print-Sheet tb_prt-mch rd_print-speed tb_prt-shipto 
          tb_prt-sellprc tb_prt-label tb_committed tb_prt-set-header 
          tb_prompt-ship dept_codes TB_sample_req tb_freeze-note rd-dest 
          run_format tb_ExportXML tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_job1 begin_job2 end_job1 end_job2 tb_fold 
         tb_show-rel tb_RS tb_corr tb_PR tb_reprint tb_DC tb_box tb_GL tb_SW 
         tb_approve tb_spanish tb_print-metric tbPageBreakByForm spec_codes 
         revsn_no rd_print-Sheet tb_prt-label tb_committed tb_prt-set-header 
         tb_prompt-ship dept_codes TB_sample_req tb_freeze-note rd-dest 
         run_format tb_ExportXML tbAutoClose btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelEmail C-Win 
PROCEDURE ExcelEmail :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    ASSIGN 
        cTitle = "Factory Ticket".
    {custom/asimailx.i &TYPE = "Excel"
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject=cTitle
                             &mail-body=cTitle
                             &mail-file=cExcelOutput } 

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
    DEFINE INPUT PARAMETER ilHide  AS LOG NO-UNDO.

    /* Enable / Disable Dept Toggle Boxes. */
    DO WITH FRAME {&frame-name}:
        ASSIGN
            tb_RS:hidden = ilHide
            tb_PR:hidden = ilHide
            tb_DC:hidden = ilHide
            tb_GL:hidden = ilHide
            tb_SW:hidden = ilHide no-error.
    END.

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
    DEFINE INPUT  PARAMETER ipcRunType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-fold AS LOG NO-UNDO.
    DEFINE VARIABLE ll-corr AS LOG NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RUN set-job-vars.

        FIND FIRST sys-ctrl
            WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "CEMENU"
            NO-LOCK NO-ERROR.

        FIND FIRST job NO-LOCK
            WHERE job.company EQ cocode
            AND job.job-no    EQ SUBSTR(fjob-no,1,iJobLen) NO-ERROR.
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
            ll-fold = NOT AVAILABLE sys-ctrl           OR
               sys-ctrl.char-fld EQ "Both"  OR
               sys-ctrl.char-fld EQ "Foldware"
            ll-corr = AVAILABLE sys-ctrl AND
               (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").

        IF ll-fold AND ll-corr THEN
            FOR EACH job-hdr
                WHERE job-hdr.company               EQ cocode

                AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
                TRIM(job-hdr.job-no) +
                STRING(job-hdr.job-no2,"999")  GE fjob-no

                AND FILL(" ", iJobLen - LENGTH(TRIM(job-hdr.job-no))) +
                TRIM(job-hdr.job-no) +
                STRING(job-hdr.job-no2,"999")  LE (IF ipcRunType EQ "All" THEN tjob-no ELSE fJob-no)
                AND job-hdr.job-no2 GE fjob-no2
                AND job-hdr.job-no2 LE tjob-no2
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

                IF AVAILABLE sys-ctrl-shipto AND substr(fjob-no,1,9) EQ substr(tjob-no,1,9) THEN
                DO:
                    lv-format-c = sys-ctrl-shipto.char-fld .
                END.
                ELSE 
                DO:
                    lv-format-c = lv-default-c .
                END.

                IF lv-format-c = "PEACHTREE" THEN
                    ASSIGN
                        tb_tray-2:HIDDEN IN FRAME FRAME-A        = YES
                        tb_tray-2:SENSITIVE                      = NO
                        tb_app-unprinted:HIDDEN IN FRAME FRAME-A = YES
                        tb_app-unprinted:SENSITIVE               = NO
                        tb_make_hold:HIDDEN IN FRAME FRAME-A     = NO
                        tb_make_hold:SENSITIVE                   = YES     .
                ELSE
                    ASSIGN
                        tb_make_hold:HIDDEN IN FRAME FRAME-A = YES
                        tb_make_hold:SENSITIVE               = NO.
                                
                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "JOBCARDF" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                    /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.
                IF AVAILABLE sys-ctrl-shipto AND substr(fjob-no,1,9) EQ substr(tjob-no,1,9) THEN 
                    lv-format-f = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-f = lv-default-f .

                IF fjob-no EQ tjob-no THEN
                    tb_reprint:SCREEN-VALUE = STRING(job-hdr.ftick-prnt).

                IF est.est-type LE 4 THEN ll-fold = YES.
                ELSE ll-corr = YES.

                IF ll-fold AND ll-corr THEN LEAVE.
            END.

        ASSIGN
            tb_fold:SCREEN-VALUE = STRING(ll-fold)
            tb_corr:SCREEN-VALUE = STRING(ll-corr)
            tb_fold              = ll-fold
            tb_corr              = ll-corr   .

        IF v-freezenotes-log EQ NO THEN 
        DO:
            IF (lv-format-f NE "Accord" OR 
                lv-format-f NE "Carded2" OR
                lv-format-f NE "GPI-STN" OR 
                lv-format-f NE "Coburn" OR 
                lv-format-f NE "Knight***") AND AVAILABLE job-hdr AND
                job-hdr.freezeNote EQ TRUE THEN ASSIGN
                    tb_freeze-note:CHECKED = TRUE
                    lFreezeNoteVal         = TRUE.
            ELSE ASSIGN
                    tb_freeze-note:CHECKED = FALSE
                    lFreezeNoteVal         = FALSE.
        END.
    END.

    IF tb_corr:SCREEN-VALUE EQ "Yes" THEN
        ASSIGN run_format:SCREEN-VALUE = lv-format-c .
    ELSE IF tb_fold:SCREEN-VALUE EQ "Yes" THEN
            ASSIGN run_format:SCREEN-VALUE = lv-format-f .

    RUN  pRunFormatValueChanged .

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
        IF is-xprint-form THEN 
        DO:
            RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
            {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_job1
                             &END_cust=end_job1
                             &mail-subject="Factory Ticket"
                             &mail-body="Factory Ticket"
                             &mail-file=lv-pdf-file + ".pdf" }  

        END.
        ELSE 
        DO:
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
    IF tb_corr THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE 
    DO:
        /*      MESSAGE "lv-format-f  " lv-format-f VIEW-AS ALERT-BOX ERROR.*/
        IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0 */
            LOOKUP(lv-format-f, 
            "Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Wingate,Keystone,Ruffino,Frankstn,Preferred,Colonial,Unipak,OttPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Dee,Badger,Carded,Burt,McLean,Carded2,GPI-STN,Coburn,Knight***,jobcardf 1,jobcardf 2,Henry") > 0 THEN
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
    DEFINE VARIABLE lv-cmd    AS cha NO-UNDO.
    DEFINE VARIABLE lv-file   AS cha NO-UNDO.
    DEFINE VARIABLE lv-xpfile AS cha NO-UNDO.

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
    IF ( tb_fold AND lv-format-f EQ "xml")   THEN
        RETURN.
    IF tb_corr THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).   
    END.

    ELSE 
    DO:

        DO WITH FRAME {&frame-name}:
            IF lv-format-f = 'Indiana-XL'          AND 
                (LOGICAL (tb_RS:screen-value) = TRUE OR
                logical (tb_PR:screen-value) = TRUE OR
                logical (tb_DC:screen-value) = TRUE OR
                logical (tb_GL:screen-value) = TRUE OR
                logical (tb_SW:screen-value) = TRUE) THEN
                RETURN.
        END.

        IF  /*index("Interpac,FibreFC,Dayton,Livngstn",lv-format-f) > 0 */
            LOOKUP(lv-format-f, "Interpac,FibreFC,HPB,Metro,Dayton,Livngstn,CentBox,Wingate,Keystone,Ruffino,Frankstn,Preferred,Colonial,Unipak,OTTPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,MidYork,Knight,Dee,Badger,Carded,burt,McLean,Carded2,GPI-STN,Coburn,Knight***,jobcardf 1,jobcardf 2,Henry") > 0 THEN
        DO:
            FILE-INFO:FILE-NAME = list-name.
            RUN printfile (FILE-INFO:FILE-NAME).

        END.
        ELSE IF lv-format-f = "prystup" THEN . 
            ELSE 
                RUN scr-rpt.w (list-name,c-win:TITLE,lv-font-no,lv-ornt). /* open file-name, title */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallOutboundAPI C-Win 
PROCEDURE pCallOutboundAPI PRIVATE :
/*------------------------------------------------------------------------------
     Purpose: To call outbound api 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job FOR job.
    DEFINE INPUT PARAMETER iplReprint AS LOGICAL NO-UNDO.  

    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.
    
    IF AVAILABLE ipbf-job THEN 
    DO:
        IF iplReprint THEN 
            cTriggerID = "RePrintJob".
        ELSE 
            cTriggerID = "PrintJob".
            
        ASSIGN  
            cAPIId       = "SendJob"
            cPrimaryID   = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', ipbf-job.job-no, ipbf-job.job-no2)))
            cDescription = cAPIID + " triggered by " + cTriggerID + " from r-ticket.w for Job: " + cPrimaryID
            .
        
        IF lExportXML THEN
            RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                INPUT  ipbf-job.company,           /* Company Code (Mandatory) */
                INPUT  ipbf-job.loc,               /* Location Code (Mandatory) */
                INPUT  cAPIID,                     /* API ID (Mandatory) */
                INPUT  "",                         /* Client ID (Optional) - Pass empty in case to make request for all clients */
                INPUT  cTriggerID,                 /* Trigger ID (Mandatory) */
                INPUT  "job",                      /* Comma separated list of table names for which data being sent (Mandatory) */
                INPUT  STRING(ROWID(ipbf-Job)),    /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                INPUT  cPrimaryID,                 /* Primary ID for which API is called for (Mandatory) */   
                INPUT  cDescription,               /* Event's description (Optional) */
                OUTPUT lSuccess,                   /* Success/Failure flag */
                OUTPUT cMessage                    /* Status message */
                ).
        
        cAPIId = "SendJobAMS".
        
        RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
            INPUT  ipbf-job.company,           /* Company Code (Mandatory) */
            INPUT  ipbf-job.loc,               /* Location Code (Mandatory) */
            INPUT  cAPIID,                     /* API ID (Mandatory) */
            INPUT  "",                         /* Client ID (Optional) - Pass empty in case to make request for all clients */
            INPUT  cTriggerID,                 /* Trigger ID (Mandatory) */
            INPUT  "job",                      /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  STRING(ROWID(ipbf-Job)),    /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  cPrimaryID,                 /* Primary ID for which API is called for (Mandatory) */   
            INPUT  cDescription,               /* Event's description (Optional) */
            OUTPUT lSuccess,                   /* Success/Failure flag */
            OUTPUT cMessage                    /* Status message */
            ).            

        IF lExportXML THEN 
        DO:
            RUN Outbound_GetEvents IN hdOutboundProcs (
                OUTPUT TABLE ttAPIOutboundEvent BY-REFERENCE
                ).
            FIND FIRST ttAPIOutboundEvent NO-ERROR.            
            IF AVAILABLE ttAPIOutboundEvent THEN 
            DO:
                FIND FIRST bf-APIOutboundEvent NO-LOCK
                    WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
                    NO-ERROR.
                IF AVAILABLE bf-APIOutboundEvent AND bf-APIOutboundEvent.apiID EQ "SendJob" THEN 
                    STATUS DEFAULT "XML Export complete" IN WINDOW THIS-PROCEDURE:CURRENT-WINDOW.
            END.
        END.
                        
        /* Reset context at the end of API calls to clear temp-table 
           data inside OutboundProcs */
        RUN Outbound_ResetContext IN hdOutboundProcs. 
    END.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunFormatValueChanged C-Win 
PROCEDURE pRunFormatValueChanged :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        
        IF LOOKUP(lv-format-c,"Artios,Protagon,VINELAND,CapCity,Trilakes2,Suthrlnd,RFC2,Peachtree,jobcardc 1,jobcardc 2,xprint,Valley,Fluted,jobcardf 1,jobcardf 2,Henry,Soule") > 0 THEN
            ASSIGN tb_fgimage:SENSITIVE = YES.
        ELSE  ASSIGN tb_fgimage:SENSITIVE = NO.
        IF LOOKUP(lv-format-f,"jobcardf 1,jobcardf 2,Henry") > 0 THEN
            ASSIGN tb_fgimage:SENSITIVE = YES.
        ELSE ASSIGN tb_fgimage:SENSITIVE = NO.
        
        IF LOOKUP(lv-format-c,"Protagon") > 0 THEN
            ASSIGN 
                tb_prt-rev:SENSITIVE             = YES
                revsn_no:HIDDEN IN FRAME FRAME-A = NO .
        ELSE
            ASSIGN tb_prt-rev:SENSITIVE             = NO
                revsn_no:HIDDEN IN FRAME FRAME-A = YES .
        
        IF lv-format-c = "Artios" THEN
            ASSIGN
                tb_tray-2:HIDDEN    = NO
                tb_tray-2:SENSITIVE = YES.
        ELSE
            ASSIGN
                tb_tray-2:HIDDEN    = YES
                tb_tray-2:SENSITIVE = NO.


        IF lv-format-c = "PEACHTREE" THEN
            ASSIGN
                tb_tray-2:HIDDEN           = YES
                tb_tray-2:SENSITIVE        = NO
                tb_app-unprinted:HIDDEN    = YES
                tb_app-unprinted:SENSITIVE = NO
                tb_make_hold:HIDDEN        = NO
                tb_make_hold:SENSITIVE     = YES     .
        ELSE
            ASSIGN
                tb_make_hold:HIDDEN    = YES
                tb_make_hold:SENSITIVE = NO.

        IF lv-format-c = "ColonialPL" OR lv-format-f = "Colonial" OR lv-format-f = "xml" THEN
            ASSIGN
                tb_draft:HIDDEN    = NO
                tb_draft:SENSITIVE = YES.
        ELSE ASSIGN tb_draft:HIDDEN    = YES
                tb_draft:SENSITIVE = NO.
            
        IF LOOKUP(lv-format-c,"Protagon") > 0 THEN
            ASSIGN tb_prt-rev:SENSITIVE             = YES
                revsn_no:HIDDEN IN FRAME FRAME-A = NO.
        ELSE ASSIGN
                tb_prt-rev:SENSITIVE             = NO
                revsn_no:HIDDEN IN FRAME FRAME-A = YES.

        IF lv-format-f = "FibreFC" 
            THEN ASSIGN fl-jobord = INT(fl-jobord:SCREEN-VALUE).
        ELSE ASSIGN fl-jobord = 0.

        IF lv-format-c = "pacific" OR lv-format-c = "Allwest" THEN
            ASSIGN
                lv-ornt:SCREEN-VALUE        = "L"
                lv-font-no:SCREEN-VALUE     = "13"
                lv-font-name:SCREEN-VALUE   = "Courier New Size=9 (13CPI)"
                lines-per-page:SCREEN-VALUE = "48"
                lines-per-page              = 48.

        IF (tb_fold AND (lv-format-f = "Interpac"  OR lv-format-f = "Dayton" 
            OR lv-format-f = "Livngstn"  OR lv-format-f = "FibreFC"  OR lv-format-f = "HPB"
            OR lv-format-f = "metro"     OR lv-format-f = "Indiana-XL" OR lv-format-f = "MidYork"
            OR lv-format-f = "CentBox"   OR lv-format-f = "Keystone" OR lv-format-f = "Frankstn" OR lv-format-f = "Preferred" OR lv-format-f = "Ruffino" 
            OR lv-format-f = "Colonial"  OR lv-format-f = "xml"  OR lv-format-f = "Unipak"   OR lv-format-f = "Ottpkg"
            OR lv-format-f = "MWFIbre"   OR lv-format-f = "Shelby"   OR lv-format-f = "CCC"
            OR lv-format-f = "PPI"       OR lv-format-f = "Accord"   OR lv-format-f = "Knight" 
            OR lv-format-f = "PackRite"  OR lv-format-f = "Knight***" OR lv-format-f = "Wingate"
            OR lv-format-f = "Dee"       OR lv-format-f = "Rosmar" OR lv-format-f = "Carded" OR lv-format-f = "McLean" OR lv-format-f = "Carded2" OR lv-format-f = "GPI-STN" OR lv-format-f = "Coburn")) OR
            (tb_corr AND (lv-format-c = "Trilakes" OR lv-format-c = "Axis" OR lv-format-c = "Trilakes2" OR lv-format-c = "Hughes" OR lv-format-c = "colonialPL" OR lv-format-c = "JobCardc 20" OR lv-format-c = "AtlanticBox"
            OR lv-format-c = "PkgAtlanta" OR lv-format-c = "Onducorr" OR lv-format-c = "HoneyCell" OR lv-format-c = "AmCarton" OR lv-format-c = "PreCorr" OR lv-format-c = "Valley20" OR lv-format-c = "PExpress")) THEN
            ASSIGN 
                tb_prt-mch:SENSITIVE     = YES
                tb_prt-shipto:SENSITIVE  = YES
                tb_prt-sellprc:SENSITIVE = YES
                rd_print-speed:SENSITIVE = YES .            
        ELSE 
        DO:
            ASSIGN 
                tb_prt-mch     = NO
                tb_prt-shipto  = NO
                tb_prt-sellprc = NO.
            ASSIGN 
                tb_prt-mch:SCREEN-VALUE     = "no"
                tb_prt-shipto:SCREEN-VALUE  = "no"
                tb_prt-sellprc:SCREEN-VALUE = "no" 
                rd_print-speed:SCREEN-VALUE = "S".
        END.

        IF lv-format-c EQ "Artios" AND lv-format-f EQ "FibreFC" THEN
            RUN split-ship-proc. /*only for Fibre*/
            
        IF tb_corr = TRUE AND (lv-format-c = "Protagon" OR lv-format-c = "Hughes" OR lv-format-c = "Allwest") THEN 
        DO:
            TB_sample_req:HIDDEN = NO.
            dept_codes:HIDDEN = YES.
        END.
        ELSE 
        DO:
            TB_sample_req:HIDDEN = YES.
            dept_codes:HIDDEN = NO.
        END.
        
        IF lv-format-f EQ "Accord" 
            OR lv-format-f EQ "Knight***" 
            OR lv-format-f EQ "Carded" 
            OR lv-format-f EQ "Carded2"  
            OR lv-format-f EQ "GPI-STN"
            OR lv-format-f EQ "Coburn" THEN ASSIGN 
                tb_freeze-note:SCREEN-VALUE = "NO"
                lFreezeNoteVal              = FALSE
                tb_freeze-note:SENSITIVE    = NO.
        ELSE ASSIGN tb_freeze-note:SENSITIVE = YES.


        IF lv-format-f EQ "Prystup" THEN
            tb_show-rel:HIDDEN IN FRAME FRAME-A = NO.
        ELSE
            tb_show-rel:HIDDEN IN FRAME FRAME-A = YES.
                    
        IF CAN-DO ("Indiana-XL", lv-format-f) 
            THEN RUN HideDeptBoxes (NO).
        ELSE RUN HideDeptBoxes (YES).
                    
        IF NOT tb_prt-rev THEN
            revsn_no:HIDDEN IN FRAME FRAME-A = YES .
        ELSE
            revsn_no:HIDDEN IN FRAME FRAME-A = NO .

        tb_prt-set-header:SENSITIVE = CAN-DO("jobcardc 20,Artios,Premier,Xprint,Valley,Fluted,jobcardc 1,jobcardc 2,Printers,Lakeside,VINELAND,Suthrlnd,United,MulticellGA,MCPartitions,oklahoma,Hughes,Protagon,Spectrum,CapCity,Allwest,LoyLang,PQP,RFC2,PEACHTREE,Soule,BELL",lv-format-c).
        IF NOT tb_prt-set-header:SENSITIVE THEN
            tb_prt-set-header:SCREEN-VALUE = "no".
                   
        IF tb_corr EQ YES AND LOOKUP(lv-format-c,"jobcardc 20,PreCorr,AmCarton,PkgAtlanta,AtlanticBox,Valley20,Delta10,HoneyCell,PExpress,Onducorr") > 0 THEN
            ASSIGN tb_fgimage:SENSITIVE      = NO
                tb_fgimage:SCREEN-VALUE   = "yes" 
                tb_box:SENSITIVE          = NO
                tb_box:SCREEN-VALUE       = "yes" 
                tb_prt-shipto:HIDDEN      = YES 
                tb_prt-label:HIDDEN       = YES 
                tb_freeze-note:HIDDEN     = YES 
                tb_prt-sellprc:HIDDEN     = YES 
                tb_committed:SCREEN-VALUE = "No"
                tb_committed:HIDDEN       = YES 
                tb_prompt-ship:HIDDEN     = YES 
                tb_prt-set-header:HIDDEN  = YES .
        ELSE
            ASSIGN tb_fgimage:SENSITIVE     = YES
                tb_box:SENSITIVE         = YES
                tb_prt-shipto:HIDDEN     = NO 
                tb_prt-label:HIDDEN      = NO 
                tb_freeze-note:HIDDEN    = NO 
                tb_prt-sellprc:HIDDEN    = NO 
                tb_committed:HIDDEN      = NO 
                tb_prompt-ship:HIDDEN    = NO 
                tb_prt-set-header:HIDDEN = NO .
                
        IF tb_corr EQ YES AND lv-format-c EQ "jobcardc 20" THEN
        ASSIGN
           tb_prt-set-header:HIDDEN  = NO
           tb_prt-set-header:SENSITIVE  = YES.

        IF lv-format-f EQ "McLean" AND tb_fold:SCREEN-VALUE EQ "Yes" THEN
            ASSIGN tb_ExportXML:HIDDEN       = YES
                tb_ExportXML:SCREEN-VALUE = "No"
                .
        ELSE ASSIGN tb_ExportXML:HIDDEN = NO.                       

        IF tb_corr AND lv-format-c EQ "Premier" THEN
            tb_print-metric:HIDDEN = NO .
        ELSE
            tb_print-metric:HIDDEN = YES .
        IF tb_corr AND lv-format-c EQ "Peachtree" THEN
            ASSIGN
                tb_prt-dmi:HIDDEN    = NO 
                tb_prt-dmi:SENSITIVE = YES .
        ELSE
            tb_prt-dmi:HIDDEN = YES .
        IF lv-format-f EQ "CentBox" AND /* lv-int-f EQ 0 AND */ tb_fold:SCREEN-VALUE EQ "Yes" THEN
        DO:
            rd_print-Sheet:HIDDEN =  NO.  
        END.
        ELSE rd_print-Sheet:HIDDEN =  YES. 
           
        IF lv-format-f EQ "Henry" AND tb_fold:SCREEN-VALUE EQ "Yes" THEN
            tb_spanish:HIDDEN = NO.
        ELSE tb_spanish:HIDDEN = YES.
            
        IF lv-format-c EQ "jobcardc 1" OR lv-format-c EQ "jobcardc 2" THEN
        DO: 
           ASSIGN
            lv-ornt:HIDDEN = NO
            lv-ornt:SENSITIVE = YES.
        END.
        ELSE ASSIGN 
            lv-ornt:HIDDEN = YES
            lv-ornt:SENSITIVE = NO
            lv-font-name:HIDDEN = YES
            lv-font-no:HIDDEN = YES
            lines-per-page:HIDDEN = YES.
        
        IF lv-format-c EQ "jobcardc 20"  THEN
        DO: 
           ASSIGN
            tb_attched:HIDDEN = NO
            tb_attched:SENSITIVE = YES.
        END.
        ELSE ASSIGN 
            tb_attched:HIDDEN = YES
            tb_attched:SENSITIVE = NO 
            tb_attched:SCREEN-VALUE = "NO" 
            tb_attched = NO .
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

    /* Parameters */
    DEFINE INPUT PARAMETER ip-industry AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-reftable FOR reftable.
    DEFINE BUFFER b-eb       FOR eb.
    
    DEFINE VARIABLE lcRequestData AS LONGCHAR NO-UNDO.

    {sys/form/r-top.i}

    RUN set-job-vars.

    ASSIGN 
        print-box              = tb_box
        reprint                = tb_reprint
        s-prt-fgimage          = tb_fgimage
        s-prt-label            = tb_prt-label
        s-committed-board-only = tb_committed
        s-prt-set-header       = tb_prt-set-header
        s-show-release         = tb_show-rel
        spec-list              = spec_codes
        s-prt-ship-split       = tb_prompt-ship
        approve                = tb_approve    
        s-prt-revno            = tb_prt-rev          
        v-dept-codes           = dept_codes
        lDraft                 = LOGICAL(tb_draft:SCREEN-VALUE IN FRAME {&FRAME-NAME})
        lExportXML             = tb_ExportXML
        lPrintMetric           = tb_print-metric
        lPrintDMIPage          = tb_prt-dmi 
        cPrintSheetTicket      = rd_print-Sheet
        lSpanish               = tb_spanish
        . 
        
    IF s-prt-revno THEN
        ASSIGN revision-no = STRING(revsn_no).
    ELSE
        ASSIGN revision-no = "".

    FOR EACH wrk-ink:
        DELETE wrk-ink.
    END.

    system.SharedConfig:Instance:SetValue("JobTicket_SpecList", spec-list).
    system.SharedConfig:Instance:SetValue("JobTicket_PrintBoxDesign", STRING(tb_box)).
    system.SharedConfig:Instance:SetValue("JobTicket_PrintFGItemImage", STRING(tb_fgimage)).
    system.SharedConfig:Instance:SetValue("JobTicket_PageBreakByForm", STRING(tbPageBreakByForm)).
    system.SharedConfig:Instance:SetValue("JobTicket_PrintAttachedImage", STRING(tbPageBreakByForm)).
    
    {jcrep/tickrrpt.i}

    system.SharedConfig:Instance:DeleteValue("JobTicket_SpecList").
    system.SharedConfig:Instance:DeleteValue("JobTicket_PrintBoxDesign").
    system.SharedConfig:Instance:DeleteValue("JobTicket_PrintFGItemImage").
    system.SharedConfig:Instance:DeleteValue("JobTicket_PageBreakByForm").
    system.SharedConfig:Instance:DeleteValue("JobTicket_PrintAttachedImage").
    
    OUTPUT CLOSE.
    
    IF lcRequestData NE "" THEN
        COPY-LOB FROM lcRequestData TO FILE list-name.
        
    IF tb_fold AND lv-format-f = "Prystup" THEN 
    DO:      
    /*RUN createExcel. */
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    /*share settings between the different ways this program is called*/
    IF INDEX(PROGRAM-NAME(1),"job_") NE 0 THEN
        RUN custom/usrprint.p ("r-ticket.", FRAME {&FRAME-NAME}:HANDLE).
    ELSE IF INDEX(PROGRAM-NAME(1),"r-ticket") NE 0 THEN
            RUN custom/usrprint.p ("job_.", FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

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
            fjob-no  = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE))
            tjob-no  = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', end_job1:SCREEN-VALUE))
            fjob-no2 = INT(begin_job2:SCREEN-VALUE)
            tjob-no2 = INT(end_job2:SCREEN-VALUE)
            fjob-no  = fjob-no + string(fjob-no2,"999")
            tjob-no  = tjob-no + string(tjob-no2,"999")
            .
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
    /* no parm display on Excel output */
    IF tb_fold AND lv-format-f = "prystup" THEN RETURN.

    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:handle.
    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
    lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    .
                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN 
                    DO:
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
            entry(i,parm-lbl-list) NE "" THEN 
        DO:

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE split-ship-proc C-Win 
PROCEDURE split-ship-proc :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no    EQ STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job1:SCREEN-VALUE))
            AND job-hdr.job-no2   EQ INT(begin_job2:SCREEN-VALUE)
            NO-ERROR.
        IF AVAILABLE job-hdr AND  job-hdr.splitShip EQ YES THEN
            tb_prompt-ship:CHECKED = YES.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setBold C-Win 
FUNCTION setBold RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    chExcel:Selection:Font:Bold = TRUE.
    RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRange C-Win 
FUNCTION setRange RETURNS CHARACTER
    ( pcSCol AS CHARACTER,
    pciCol AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cVar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE k    AS INTEGER   NO-UNDO.    
    DEFINE VARIABLE j    AS INTEGER   NO-UNDO.

    i =  TRUNCATE(pciCol / 26,0).
    j = pciCol MOD 26.

    IF i GT 0  AND j <> 0 THEN cVar = CHR(64 + i).
    IF j = 0   THEN 
    DO:
        k = i - 1.
        IF k GT 0 THEN cVar = CHR(64 + k).
        cVar = cVar + CHR( 64  + 26). 
    END.
    IF j GT 0 THEN cVar = cVar + CHR(64 + j).

    RETURN pcSCol + ":" + cVar.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSize C-Win 
FUNCTION setSize RETURNS LOGICAL
    (  iN# AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    chExcel:Selection:Font:Size = iN#.
    RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setValue C-Win 
FUNCTION setValue RETURNS LOGICAL
    ( cValue AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    chExcel:Selection:Value = cValue.
    RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

