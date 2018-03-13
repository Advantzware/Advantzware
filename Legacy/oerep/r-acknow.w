&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-acknow.w

  Description: Order Acknowledgement

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
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
 cocode = gcompany
 locode = gloc.

{XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXMLAck &Company=cocode &c=c} /* bpv 05291402 */

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR lv-fax-image AS cha NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEF VAR vmDefaultForm AS CHAR NO-UNDO.
DEF VAR viDefaultLinesPerPage AS INT NO-UNDO.
DEF VAR lv-termPath AS CHAR NO-UNDO.
DEF VAR v-fmt-int AS INT INIT 0 NO-UNDO.
DEF VAR v-ack-master AS LOG INIT NO NO-UNDO .
DEF VAR lv-attach-push AS cha NO-UNDO.
DEFINE VARIABLE lAsiUser AS LOGICAL NO-UNDO .

{oe/rep/acknowl.i new}
{custom/xprint.i}

/* for BOM print */
    DEF TEMP-TABLE tt-specCd NO-UNDO
    FIELD tt-char-val AS CHAR
    INDEX chr-1 tt-char-val.
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

DEF NEW SHARED VAR s-committed-board-only AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-revised AS LOG NO-UNDO.
DEF NEW SHARED VAR LvOutputSelection AS CHAR NO-UNDO.
DEF NEW SHARED VAR v-rs-whs-mths AS CHAR NO-UNDO.
DEF NEW SHARED VAR v-tg-whs-mths AS LOG NO-UNDO.
DEF NEW SHARED VAR v-dept-codes AS CHAR NO-UNDO. 
DEF NEW SHARED VAR v-print-components AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-po AS LOG NO-UNDO.
DEF NEW SHARED VAR v-UntCnt AS LOG NO-UNDO.
DEF NEW SHARED VAR v-Shpnot AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-due AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-tot AS LOG NO-UNDO.

DEF BUFFER b-oe-ord FOR oe-ord.
DEF BUFFER b1-oe-ord FOR oe-ord.
DEF BUFFER b2-oe-ord FOR oe-ord.

{jcrep/r-ticket.i "new shared"}

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.
IF AVAIL users AND users.USER_id EQ "ASI" THEN
    ASSIGN lAsiUser = YES .

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

DO TRANSACTION:
  {sys/inc/ackmst.i}
  {sys/inc/acksps.i}  /* SPS ACK xml file generation logic */
END.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

PROCEDURE mail EXTERNAL 'xpMail.dll' :

  DEFINE INPUT  PARAMETER mailTo      AS CHAR.
  DEFINE INPUT  PARAMETER mailsubject AS CHAR.
  DEFINE INPUT  PARAMETER mailText    AS CHAR.
  DEFINE INPUT  PARAMETER mailFiles   AS CHAR.
  DEFINE INPUT  PARAMETER mailDialog  AS LONG.
  DEFINE OUTPUT PARAMETER retCode     AS LONG.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-8 RECT-9 rd_ack-ordmst ~
begin_ord-no end_ord-no begin_cust-no end_cust-no begin_due-date ~
end_due-date begin_relnum end_relnum tb_reprint TG_cons_form TG_whs-mths ~
RS_whs-mths tb_sch-rel tb_inst tb_ship-to tb_act-rel tb_prt-revise ~
tb_prt-bom tb_billnotes tb_terms tb_itempo TG_print-pen-notes tb_untcnt ~
tb_print-component TG_print-due-cd tb_hide_sell tb_itm-tot tb_shpnot ~
spec-code lv-termFile dept-code rd-dest lv-ornt lines-per-page lv-font-no ~
sel-attch TG_eml-push-att TG_preview td-show-parm run_format btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS rd_ack-ordmst begin_ord-no end_ord-no ~
begin_cust-no end_cust-no begin_due-date end_due-date begin_relnum ~
end_relnum tb_reprint TG_cons_form TG_whs-mths RS_whs-mths tb_sch-rel ~
tb_inst tb_ship-to tb_act-rel tb_prt-revise tb_prt-bom tb_billnotes ~
tb_terms tb_itempo TG_print-pen-notes tb_untcnt tb_print-component ~
TG_print-due-cd tb_hide_sell tb_itm-tot tb_shpnot spec-code lv-termFile ~
dept-code rd-dest lv-ornt lines-per-page lv-font-no lv-font-name sel-attch ~
TG_eml-push-att TG_preview td-show-parm run_format

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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_relnum AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Beginning Release#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE dept-code AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dept Code" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_relnum AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 
     LABEL "Ending Release#" 
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

DEFINE VARIABLE lv-termFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Terms File" 
     VIEW-AS FILL-IN 
     SIZE 49.4 BY 1 NO-UNDO.

DEFINE VARIABLE spec-code AS CHARACTER FORMAT "X(256)":U 
     LABEL "Spec Code" 
     VIEW-AS FILL-IN 
     SIZE 26.6 BY 1 NO-UNDO.

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
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_ack-ordmst AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Acknowledgement", "A",
"Order Master", "M"
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE RS_whs-mths AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "3 Months", "3",
"6 Months", "6"
     SIZE 28 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93.6 BY 8.1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93.6 BY 3.24.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93.6 BY 6.1.

DEFINE VARIABLE sel-attch AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 31.6 BY 3.1 NO-UNDO.

DEFINE VARIABLE tb_act-rel AS LOGICAL INITIAL NO 
     LABEL "Print Actual Releases?" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_billnotes AS LOGICAL INITIAL YES 
     LABEL "Print Bill Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_hide_sell AS LOGICAL INITIAL NO 
     LABEL "Hide Sell Price?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_inst AS LOGICAL INITIAL YES 
     LABEL "Print Spec Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_itempo AS LOGICAL INITIAL NO 
     LABEL "Print Item PO#s?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tb_itm-tot AS LOGICAL INITIAL NO 
     LABEL "Print Item Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-component AS LOGICAL INITIAL NO 
     LABEL "Print Components" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-bom AS LOGICAL INITIAL NO 
     LABEL "Print Bill of Materials?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-revise AS LOGICAL INITIAL NO 
     LABEL "Print REVISED?" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL NO 
     LABEL "Do you want to reprint Acknowledgements?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .81 NO-UNDO.

DEFINE VARIABLE tb_sch-rel AS LOGICAL INITIAL YES 
     LABEL "Print Scheduled Releases?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .81 NO-UNDO.

DEFINE VARIABLE tb_ship-to AS LOGICAL INITIAL YES 
     LABEL "Print ShipTo Address" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_shpnot AS LOGICAL INITIAL NO 
     LABEL "Print Ship Notes per Release?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tb_terms AS LOGICAL INITIAL NO 
     LABEL "Print Terms?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tb_untcnt AS LOGICAL INITIAL NO 
     LABEL "Print Unit Count?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG_cons_form AS LOGICAL INITIAL NO 
     LABEL "Consolidate Forms ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE TG_eml-push-att AS LOGICAL INITIAL NO 
     LABEL "Email Push Pin Attachment ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG_preview AS LOGICAL INITIAL NO 
     LABEL "Show Preview Before Email?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG_print-due-cd AS LOGICAL INITIAL NO 
     LABEL "Due Date Code?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE TG_print-pen-notes AS LOGICAL INITIAL NO 
     LABEL "Print Pen Notes?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE TG_whs-mths AS LOGICAL INITIAL NO 
     LABEL "Warehouse Months" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE run_format AS CHARACTER FORMAT "X(30)":U 
     LABEL "Format" 
     VIEW-AS FILL-IN /*COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST*/
     SIZE 25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     rd_ack-ordmst AT ROW 1.24 COL 34.4 NO-LABEL WIDGET-ID 38
     begin_ord-no AT ROW 2.24 COL 26.6 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 2.24 COL 69.6 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_cust-no AT ROW 3.19 COL 26.6 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.19 COL 69.6 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_due-date AT ROW 4.14 COL 26.6 COLON-ALIGNED
     end_due-date AT ROW 4.14 COL 69.6 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     begin_relnum AT ROW 5.1 COL 26.6 COLON-ALIGNED HELP
          "Enter Beginning Release Number"
     end_relnum AT ROW 5.1 COL 69.6 COLON-ALIGNED HELP
          "Enter Ending Release Number"
     tb_reprint AT ROW 7.52 COL 3.8
     TG_cons_form AT ROW 7.52 COL 60 WIDGET-ID 16
     TG_whs-mths AT ROW 8.38 COL 3.8 WIDGET-ID 6
     RS_whs-mths AT ROW 8.38 COL 31 NO-LABEL WIDGET-ID 2
     tb_sch-rel AT ROW 10.52 COL 3.8
     tb_inst AT ROW 10.52 COL 37
     tb_ship-to AT ROW 10.52 COL 66
     tb_act-rel AT ROW 11.43 COL 3.8
     tb_prt-revise AT ROW 11.43 COL 37
     tb_prt-bom AT ROW 11.43 COL 66
     tb_billnotes AT ROW 11.48 COL 37 WIDGET-ID 46
     tb_terms AT ROW 12.29 COL 3.8 WIDGET-ID 20
     tb_itempo AT ROW 12.33 COL 3.8 WIDGET-ID 30
     TG_print-pen-notes AT ROW 12.33 COL 37 WIDGET-ID 22
     tb_untcnt AT ROW 12.33 COL 37 WIDGET-ID 30
     tb_print-component AT ROW 12.33 COL 66 WIDGET-ID 28
     TG_print-due-cd AT ROW 12.43 COL 3.8 WIDGET-ID 34
     tb_hide_sell AT ROW 13.24 COL 3.8 WIDGET-ID 32
     tb_itm-tot AT ROW 13.24 COL 37 WIDGET-ID 44
     tb_shpnot AT ROW 14.14 COL 3.8 WIDGET-ID 42
     spec-code AT ROW 14.29 COL 13 COLON-ALIGNED WIDGET-ID 24
     lv-termFile AT ROW 14.29 COL 41.6 COLON-ALIGNED WIDGET-ID 18
     dept-code AT ROW 14.33 COL 60 COLON-ALIGNED WIDGET-ID 26
     rd-dest AT ROW 16.91 COL 3.4 NO-LABEL
     lv-ornt AT ROW 17.19 COL 30 NO-LABEL
     lines-per-page AT ROW 17.19 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 18.38 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 19.33 COL 26 COLON-ALIGNED NO-LABEL
     sel-attch AT ROW 20.52 COL 61.4 NO-LABEL WIDGET-ID 14
     TG_eml-push-att AT ROW 20.62 COL 28.6 WIDGET-ID 48
     TG_preview AT ROW 21.62 COL 28.6 WIDGET-ID 36
     td-show-parm AT ROW 22.67 COL 28.6
     run_format AT ROW 22.67 COL 65 COLON-ALIGNED WIDGET-ID 12
     btn-ok AT ROW 24.1 COL 20
     btn-cancel AT ROW 24.1 COL 60
     "Print Options" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 9.71 COL 2 WIDGET-ID 12
     "Report Type" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 6.57 COL 2 WIDGET-ID 14
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.52 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.33 COL 2.4
          BGCOLOR 2 
     RECT-6 AT ROW 15.76 COL 1
     RECT-8 AT ROW 6.33 COL 1 WIDGET-ID 8
     RECT-9 AT ROW 9.43 COL 1 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.4 BY 24.71.


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
         TITLE              = "Print Order Acknowledgements"
         HEIGHT             = 24.71
         WIDTH              = 94.6
         MAX-HEIGHT         = 46.57
         MAX-WIDTH          = 280
         VIRTUAL-HEIGHT     = 46.57
         VIRTUAL-WIDTH      = 280
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
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_relnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_relnum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       sel-attch:SELECTED IN FRAME FRAME-A       = TRUE.

ASSIGN 
       tb_act-rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_billnotes:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_billnotes:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_hide_sell:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inst:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_itempo:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_itempo:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_itm-tot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prt-bom:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_reprint:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sch-rel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_ship-to:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_shpnot:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_terms:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_untcnt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Print Order Acknowledgements */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Print Order Acknowledgements */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_relnum C-Win
ON VALUE-CHANGED OF begin_relnum IN FRAME FRAME-A /* Beginning Release# */
DO:
  RUN new-relnum.
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
  ASSIGN rd-dest tb_inst .
  ASSIGN {&DISPLAYED-OBJECTS}.
  RUN GetSelected.

  CASE rd-dest:
     WHEN 1 THEN LvOutputSelection = "Printer".
     WHEN 2 THEN LvOutputSelection = "Screen".
     WHEN 3 THEN LvOutputSelection = "File".
     WHEN 4 THEN LvOutputSelection = "Fax". 
     WHEN 5 THEN LvOutputSelection = "Email".
     WHEN 6 THEN LvOutputSelection = "Port".
  END CASE.

  ASSIGN
     ford-no   = begin_ord-no
     tord-no   = end_ord-no
     fdate     = begin_due-date
     tdate     = END_due-date
     v-schrel  = tb_sch-rel
     v-actrel  = tb_act-rel
     v-shipto  = tb_ship-to
     v-reprint = tb_reprint
     v-prntinst = tb_inst
     v-terms    = tb_terms
     v-termfile = lv-termFile
     v-hide-price = tb_hide_sell
     v-shpnot   = tb_shpnot.

  IF AckMst-log = YES THEN
      v-ack-master = IF rd_ack-ordmst = "M" THEN YES ELSE NO .
      ELSE
       v-ack-master = NO .

  IF LvOutputSelection = "Email"THEN DO:
     RUN BatchMail. 
  END. 
  ELSE DO:  /* not email */
    IF v-ack-master = YES THEN DO:

      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "ACKMASTER") THEN
     DO:
        IF CAN-FIND(FIRST b-oe-ord
           WHERE b-oe-ord.company  EQ cocode
             AND b-oe-ord.ord-no   GE ford-no
             AND b-oe-ord.ord-no   LE tord-no
             AND b-oe-ord.cust-no  GE begin_cust-no
             AND b-oe-ord.cust-no  LE end_cust-no
             AND b-oe-ord.ord-date GE fdate
             AND b-oe-ord.ord-date LE tdate
             AND b-oe-ord.ack-prnt EQ v-reprint) THEN
           FOR EACH b-oe-ord FIELDS(company cust-no ord-no) WHERE
               b-oe-ord.company  EQ cocode AND
               b-oe-ord.ord-no   GE ford-no AND
               b-oe-ord.ord-no   LE tord-no AND
               b-oe-ord.cust-no  GE begin_cust-no AND
               b-oe-ord.cust-no  LE end_cust-no AND
               b-oe-ord.ord-date GE fdate AND
               b-oe-ord.ord-date LE tdate AND
               b-oe-ord.ack-prnt EQ v-reprint
               NO-LOCK
               BREAK BY b-oe-ord.company
                     BY b-oe-ord.cust-no:

               IF FIRST-OF(b-oe-ord.cust-no) THEN DO:

                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "ACKMASTER" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-ord.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     RUN SetAckMstForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetAckMstForm (vmDefaultForm).
                     v-print-fmt = vmDefaultForm.
                  END.

                  RUN SetGlobalVariables(b-oe-ord.ord-no).
                  RUN run-report(b-oe-ord.cust-no, TRUE).
                  RUN GenerateReport(b-oe-ord.cust-no,b-oe-ord.cust-no).
               END.
           END. /* FOR EACH*/
        ELSE
           MESSAGE "No Order Acknowledgements Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /*if can-find sys-ctrl-shipto*/
  ELSE
     DO:

        v-print-fmt = vmDefaultForm.
        RUN SetAckMstForm (v-print-fmt).
        RUN SetGlobalVariables(INPUT begin_ord-no).
        RUN run-report("", FALSE).
        RUN GenerateReport(begin_cust-no, end_cust-no).
     END.

    END.  /* end of v-ack-master log = yes */


    ELSE DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "ACKHEAD") THEN
     DO:
        IF CAN-FIND(FIRST b-oe-ord
           WHERE b-oe-ord.company  EQ cocode
             AND b-oe-ord.ord-no   GE ford-no
             AND b-oe-ord.ord-no   LE tord-no
             AND b-oe-ord.cust-no  GE begin_cust-no
             AND b-oe-ord.cust-no  LE end_cust-no
             AND b-oe-ord.ord-date GE fdate
             AND b-oe-ord.ord-date LE tdate
             AND b-oe-ord.ack-prnt EQ v-reprint) THEN
           FOR EACH b-oe-ord FIELDS(company cust-no ord-no) WHERE
               b-oe-ord.company  EQ cocode AND
               b-oe-ord.ord-no   GE ford-no AND
               b-oe-ord.ord-no   LE tord-no AND
               b-oe-ord.cust-no  GE begin_cust-no AND
               b-oe-ord.cust-no  LE end_cust-no AND
               b-oe-ord.ord-date GE fdate AND
               b-oe-ord.ord-date LE tdate AND
               b-oe-ord.ack-prnt EQ v-reprint
               NO-LOCK
               BREAK BY b-oe-ord.company
                     BY b-oe-ord.cust-no:

               IF FIRST-OF(b-oe-ord.cust-no) THEN DO:

                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "ACKHEAD" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-ord.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     RUN SetOEAckForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetOEAckForm (vcDefaultForm).
                     v-print-fmt = vcDefaultForm.
                  END.

                  RUN SetGlobalVariables(b-oe-ord.ord-no).
                  RUN run-report(b-oe-ord.cust-no, TRUE).
                  RUN GenerateReport(b-oe-ord.cust-no,b-oe-ord.cust-no).
               END.
           END. /* FOR EACH*/
        ELSE
           MESSAGE "No Order Acknowledgements Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /*if can-find sys-ctrl-shipto*/
     ELSE DO:
        v-print-fmt = vcDefaultForm.
        RUN SetOEAckForm (v-print-fmt).

        RUN SetGlobalVariables(INPUT begin_ord-no).
        RUN run-report("", FALSE).
        RUN GenerateReport(begin_cust-no, end_cust-no).
     END.

    END. /* else of v-ack-master  */
  END.  /* NOT output to email */

END.  /* end of btn-ok */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dept-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dept-code C-Win
ON HELP OF dept-code IN FRAME FRAME-A /* Dept Code */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR ip-char-val AS CHAR NO-UNDO.
   DEF VAR i-cnt AS INT NO-UNDO.

   ASSIGN spec-code
          ip-char-val = "".

   EMPTY TEMP-TABLE tt-specCd.
   DO i-cnt = 1 TO NUM-ENTRIES(spec-code):

       IF TRIM(ENTRY(i-cnt,spec-code))  EQ "" THEN NEXT.

       CREATE tt-specCd.
       ASSIGN tt-char-val = TRIM(ENTRY(i-cnt,spec-code)).
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dept-code C-Win
ON LEAVE OF dept-code IN FRAME FRAME-A /* Dept Code */
DO:
    ASSIGN {&self-name}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.

     RUN get-cust-attch .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Order Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  ASSIGN {&self-name}.
  IF begin_ord-no = END_ord-no THEN DO:
     FIND FIRST oe-ord WHERE oe-ord.company = g_company
                         AND oe-ord.ord-no = begin_ord-no NO-LOCK NO-ERROR.
     IF AVAIL oe-ord THEN ASSIGN begin_cust-no:SCREEN-VALUE = oe-ord.cust-no
                                 end_cust-no:SCREEN-VALUE = oe-ord.cust-no.
     RUN get-cust-attch .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_relnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_relnum C-Win
ON VALUE-CHANGED OF end_relnum IN FRAME FRAME-A /* Ending Release# */
DO:
  RUN new-relnum.
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


&Scoped-define SELF-NAME lv-termFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-termFile C-Win
ON HELP OF lv-termFile IN FRAME FRAME-A /* Terms File */
DO:
    DEF VAR chFile AS CHAR NO-UNDO.
    DEF VAR ll-ok  AS LOG  NO-UNDO.

    IF TRIM(lv-termPath) EQ "" 
      THEN lv-termPath = "C:\".

    SYSTEM-DIALOG GET-FILE chFile 
      TITLE "Select File"
      FILTERS "Text File (*.txt) " "*.txt"
      INITIAL-DIR lv-termPath
      MUST-EXIST
      USE-FILENAME
      UPDATE ll-ok.

    IF ll-ok 
      THEN 
       ASSIGN 
         lv-termFile = chFile
         lv-termFile:SCREEN-VALUE = lv-termFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-termFile C-Win
ON LEAVE OF lv-termFile IN FRAME FRAME-A /* Terms File */
DO:
    ASSIGN {&self-name}.  
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


&Scoped-define SELF-NAME rd_ack-ordmst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ack-ordmst C-Win
ON VALUE-CHANGED OF rd_ack-ordmst IN FRAME FRAME-A
DO:
    ASSIGN {&self-name}.
    IF rd_ack-ordmst = "M"  THEN do:
        FIND FIRST sys-ctrl NO-LOCK
                 WHERE sys-ctrl.company EQ cocode
                 AND sys-ctrl.name    EQ "ACKMASTER"
                 NO-ERROR.
             IF AVAIL sys-ctrl THEN
                 ASSIGN
                 v-print-fmt  = sys-ctrl.char-fld
                 v-print-head = sys-ctrl.log-fld
                 vcDefaultForm = v-print-fmt
                 v-fmt-int = sys-ctrl.int-fld
                 run_format:SCREEN-VALUE = v-print-fmt . 
        ASSIGN
    tb_reprint:SENSITIVE = NO 
    TG_cons_form:SENSITIVE = NO 
    TG_whs-mths:SENSITIVE = NO
    RS_whs-mths:SENSITIVE = NO
    tb_sch-rel:SENSITIVE = NO
    tb_inst:SENSITIVE = NO 
    tb_ship-to:SENSITIVE = NO
    tb_act-rel:SENSITIVE = NO
    tb_prt-revise:SENSITIVE = NO
    tb_prt-bom:SENSITIVE = NO 
    TG_print-due-cd:SENSITIVE = NO
    TG_print-pen-notes:SENSITIVE = NO
    tb_print-component:SENSITIVE = NO
    tb_hide_sell:SENSITIVE = NO 
    spec-code:SENSITIVE = NO
    lv-termFile:SENSITIVE = NO
    dept-code:SENSITIVE = NO
    tb_terms:SENSITIVE = NO
    tb_itempo:SENSITIVE = NO                           
    tb_untcnt:SENSITIVE = NO 
    tb_shpnot:SENSITIVE = NO.
    END.
    ELSE DO:  
        FIND FIRST sys-ctrl NO-LOCK
                 WHERE sys-ctrl.company EQ cocode
                 AND sys-ctrl.name    EQ "ACKHEAD"
                 NO-ERROR.
             IF AVAIL sys-ctrl THEN
                 ASSIGN
                 v-print-fmt  = sys-ctrl.char-fld
                 v-print-head = sys-ctrl.log-fld
                 vcDefaultForm = v-print-fmt
                 v-fmt-int = sys-ctrl.int-fld
                 run_format:SCREEN-VALUE = v-print-fmt .
        ASSIGN
            tb_reprint:SENSITIVE = YES 
            TG_cons_form:SENSITIVE = YES 
            TG_whs-mths:SENSITIVE = YES
            RS_whs-mths:SENSITIVE = YES
            tb_sch-rel:SENSITIVE = YES
            tb_inst:SENSITIVE = YES 
            tb_ship-to:SENSITIVE = YES
            tb_act-rel:SENSITIVE = YES
            tb_prt-revise:SENSITIVE = YES
            tb_prt-bom:SENSITIVE = YES 
            TG_print-due-cd:SENSITIVE = YES
            TG_print-pen-notes:SENSITIVE = YES
            tb_print-component:SENSITIVE = YES
            tb_hide_sell:SENSITIVE = YES 
            spec-code:SENSITIVE = YES
            lv-termFile:SENSITIVE = YES
            dept-code:SENSITIVE = YES
            tb_terms:SENSITIVE = YES
            tb_itempo:SENSITIVE = YES                           
            tb_untcnt:SENSITIVE = YES 
            tb_shpnot:SENSITIVE = YES.

        ASSIGN
            TG_whs-mths:SCREEN-VALUE      = "No"
            RS_whs-mths:SENSITIVE         = NO
            TG_whs-mths:SENSITIVE         = NO
            TG_cons_form:SCREEN-VALUE     = "No"
            TG_cons_form:SENSITIVE        = NO.
        IF LOOKUP(v-print-fmt,"Century,Fibrex,Allwest") = 0 THEN 
      ASSIGN
       tb_prt-bom = NO
       tb_prt-bom:SENSITIVE = NO
       tb_prt-bom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

        IF v-print-fmt EQ "PremierX" OR v-print-fmt = "ACPI" OR v-print-fmt EQ "PremierCX"
            THEN ASSIGN
            tb_itempo:HIDDEN = NO
            tb_hide_sell:HIDDEN = NO
            tb_itempo:SENSITIVE = YES
            tb_hide_sell:SENSITIVE = YES
            . 
        ELSE ASSIGN 
            tb_itempo:HIDDEN = YES
            tb_hide_sell:HIDDEN = YES .

        IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierCX"
            THEN ASSIGN
            tb_untcnt:HIDDEN = NO 
            tb_untcnt:SENSITIVE = YES.
        ELSE ASSIGN 
            tb_untcnt:HIDDEN = YES .

        IF v-print-fmt EQ "PremierX"
            THEN ASSIGN
            tb_shpnot:HIDDEN = NO 
            tb_shpnot:SENSITIVE = YES. 
        ELSE ASSIGN 
            tb_shpnot:HIDDEN = YES . 

        IF v-print-fmt EQ "Badger"
            THEN ASSIGN
            TG_print-due-cd:HIDDEN = NO
            TG_print-due-cd:SENSITIVE = YES. 
        ELSE ASSIGN 
            TG_print-due-cd:HIDDEN = YES .

        /*                                  */
        /*     IF v-print-fmt EQ "ACPI"     */
        /*          THEN ASSIGN             */
        /*             tb_itempo:HIDDEN = NO. */

        IF v-print-fmt EQ "Soule" OR v-print-fmt EQ "SouleUOM"
            THEN ASSIGN 
            tb_print-component:HIDDEN       = NO 
            tb_print-component:SENSITIVE       = YES.
        ELSE
            tb_print-component:HIDDEN       = YES .


    IF v-print-fmt EQ "Allwest" THEN ASSIGN TG_cons_form:SENSITIVE  = YES.

    IF v-print-fmt EQ "Indiana" THEN
      ASSIGN
       tb_sch-rel = NO
       tb_sch-rel:SENSITIVE = NO
       tb_sch-rel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
       tb_act-rel = NO
       tb_act-rel:SENSITIVE = NO
       tb_act-rel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

    IF v-print-fmt = "Dee" THEN
      ASSIGN
         TG_whs-mths:SENSITIVE = YES. 

    IF v-print-fmt EQ "Albert" THEN
       ASSIGN
         tb_prt-revise:SENSITIVE = NO
         tb_prt-revise = NO
         tb_prt-revise:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".
    /* gdm - 11091105*/
    ASSIGN tb_prt-revise:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

     /*IF v-print-fmt NE "Simkins" 
      THEN ASSIGN 
             tb_terms:HIDDEN       = YES
             lv-termFile:HIDDEN    = YES
             tb_terms:SENSITIVE    = NO
             lv-termFile:SENSITIVE = NO
             TG_print-pen-notes:HIDDEN     = YES
             TG_print-pen-notes:SENSITIVE  = YES.*/

    IF v-print-fmt EQ "Simkins" THEN DO:
        ASSIGN 
            tb_terms                 = NO
            tb_terms:SENSITIVE       = YES.

        ASSIGN lv-termFile:SENSITIVE = NO.

        IF tb_terms:SCREEN-VALUE EQ "YES" 
            THEN lv-termFile:SENSITIVE = YES.
    END.
    ELSE ASSIGN
          tb_terms:SENSITIVE       = NO
          lv-termFile:SCREEN-VALUE = ""
          lv-termFile              = ""
          lv-termFile:SENSITIVE    = NO.

    IF LOOKUP(v-print-fmt,"Soule,SouleUOM,ContSvc") = 0 THEN DO:
        IF tb_sch-rel:SCREEN-VALUE EQ "NO" AND
            tb_act-rel:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                tb_ship-to:SENSITIVE = NO
                tb_ship-to:SCREEN-VALUE = "NO".
    END.
    IF LOOKUP(v-print-fmt,"PremierX") <> 0 THEN DO:
        IF tb_sch-rel:SCREEN-VALUE EQ "NO" AND
            tb_act-rel:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                tb_shpnot:SENSITIVE = NO
                tb_shpnot:SCREEN-VALUE = "NO".
    END.
    IF v-print-fmt EQ "Accord" THEN DO:
         ASSIGN 
             TG_print-pen-notes:HIDDEN     = NO
             TG_print-pen-notes:SENSITIVE  = YES.

            IF tb_inst:SCREEN-VALUE EQ "Yes" THEN
             spec-code:HIDDEN IN FRAME FRAME-A = NO .
            ELSE
             spec-code:HIDDEN IN FRAME FRAME-A = YES .

             IF TG_print-pen-notes:SCREEN-VALUE EQ "Yes" THEN
                dept-code:HIDDEN IN FRAME FRAME-A = NO .
            ELSE
                dept-code:HIDDEN IN FRAME FRAME-A = YES .
     END.
     ELSE
         ASSIGN spec-code:HIDDEN IN FRAME FRAME-A = YES 
                    dept-code:HIDDEN IN FRAME FRAME-A = YES .
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS_whs-mths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS_whs-mths C-Win
ON VALUE-CHANGED OF RS_whs-mths IN FRAME FRAME-A
DO:
   ASSIGN RS_whs-mths.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec-code C-Win
ON HELP OF spec-code IN FRAME FRAME-A /* Spec Code */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DEF VAR ip-char-val AS CHAR NO-UNDO.
   DEF VAR i-cnt AS INT NO-UNDO.

   ASSIGN spec-code
          ip-char-val = "".

   EMPTY TEMP-TABLE tt-specCd.
   DO i-cnt = 1 TO NUM-ENTRIES(spec-code):

       IF TRIM(ENTRY(i-cnt,spec-code))  EQ "" THEN NEXT.

       CREATE tt-specCd.
       ASSIGN tt-char-val = TRIM(ENTRY(i-cnt,spec-code)).
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec-code C-Win
ON LEAVE OF spec-code IN FRAME FRAME-A /* Spec Code */
DO:
    ASSIGN {&self-name}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_act-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_act-rel C-Win
ON VALUE-CHANGED OF tb_act-rel IN FRAME FRAME-A /* Print Actual Releases? */
DO:
    RUN SetShipToToggle.
/*   assign tb_sch-rel tb_act-rel.     */
/*                                     */
/*   IF tb_act-rel EQ YES THEN         */
/*      tb_ship-to:SENSITIVE = YES.    */
/*   ELSE IF tb_sch-rel EQ NO THEN     */
/*      ASSIGN                         */
/*      tb_ship-to:SCREEN-VALUE = "NO" */
/*      tb_ship-to:SENSITIVE = NO.     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_billnotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_billnotes C-Win
ON VALUE-CHANGED OF tb_billnotes IN FRAME FRAME-A /* Print Bill Notes? */
DO:
  ASSIGN {&self-name}.
  IF v-print-fmt EQ "Accord" THEN DO: 
      IF tb_inst THEN
          spec-code:HIDDEN IN FRAME FRAME-A = NO .
      ELSE
          spec-code:HIDDEN IN FRAME FRAME-A = YES .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_hide_sell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_hide_sell C-Win
ON VALUE-CHANGED OF tb_hide_sell IN FRAME FRAME-A /* Hide Sell Price? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inst C-Win
ON VALUE-CHANGED OF tb_inst IN FRAME FRAME-A /* Print Spec Notes? */
DO:
  ASSIGN {&self-name}.
  IF v-print-fmt EQ "Accord" THEN DO: 
      IF tb_inst THEN
          spec-code:HIDDEN IN FRAME FRAME-A = NO .
      ELSE
          spec-code:HIDDEN IN FRAME FRAME-A = YES .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_itempo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_itempo C-Win
ON VALUE-CHANGED OF tb_itempo IN FRAME FRAME-A /* Print Item PO#s? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_itm-tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_itm-tot C-Win
ON VALUE-CHANGED OF tb_itm-tot IN FRAME FRAME-A /* Print Item Totals? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-component
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-component C-Win
ON VALUE-CHANGED OF tb_print-component IN FRAME FRAME-A /* Print Components */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-bom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-bom C-Win
ON VALUE-CHANGED OF tb_prt-bom IN FRAME FRAME-A /* Print Bill of Materials? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-revise
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-revise C-Win
ON VALUE-CHANGED OF tb_prt-revise IN FRAME FRAME-A /* Print REVISED? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_reprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_reprint C-Win
ON VALUE-CHANGED OF tb_reprint IN FRAME FRAME-A /* Do you want to reprint Acknowledgements? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sch-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sch-rel C-Win
ON VALUE-CHANGED OF tb_sch-rel IN FRAME FRAME-A /* Print Scheduled Releases? */
DO:
    RUN SetShipToToggle.
/*   assign tb_sch-rel tb_act-rel.         */
/*                                         */
/*   IF tb_sch-rel EQ YES THEN             */
/*      tb_ship-to:SENSITIVE = YES.        */
/*   ELSE IF tb_act-rel EQ NO THEN         */
/*      ASSIGN                             */
/*         tb_ship-to:SENSITIVE = NO       */
/*         tb_ship-to:SCREEN-VALUE = "NO". */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_ship-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_ship-to C-Win
ON VALUE-CHANGED OF tb_ship-to IN FRAME FRAME-A /* Print ShipTo Address */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_shpnot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_shpnot C-Win
ON VALUE-CHANGED OF tb_shpnot IN FRAME FRAME-A /* Print Ship Notes per Release? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_terms C-Win
ON VALUE-CHANGED OF tb_terms IN FRAME FRAME-A /* Print Terms? */
DO:
    ASSIGN {&self-name}.
    IF v-print-fmt EQ "Simkins" 
      THEN ASSIGN lv-termFile:SENSITIVE = {&self-name}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_untcnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_untcnt C-Win
ON VALUE-CHANGED OF tb_untcnt IN FRAME FRAME-A /* Print Unit Count? */
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


&Scoped-define SELF-NAME TG_cons_form
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_cons_form C-Win
ON VALUE-CHANGED OF TG_cons_form IN FRAME FRAME-A /* Consolidate Forms ? */
DO:
    ASSIGN TG_cons_form.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG_eml-push-att
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_eml-push-att C-Win
ON VALUE-CHANGED OF TG_eml-push-att IN FRAME FRAME-A /* Email Push Pin Attachment ? */
DO:
    ASSIGN TG_eml-push-att.  
    IF TG_eml-push-att:SCREEN-VALUE IN FRAME FRAME-A = "YES"  THEN
        sel-attch:HIDDEN IN FRAME FRAME-A = NO .
    ELSE sel-attch:HIDDEN IN FRAME FRAME-A = YES .
    RUN Get-cust-attch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG_preview
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_preview C-Win
ON VALUE-CHANGED OF TG_preview IN FRAME FRAME-A /* Show Preview Before Email? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG_print-due-cd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_print-due-cd C-Win
ON VALUE-CHANGED OF TG_print-due-cd IN FRAME FRAME-A /* Due Date Code? */
DO:
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG_print-pen-notes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_print-pen-notes C-Win
ON VALUE-CHANGED OF TG_print-pen-notes IN FRAME FRAME-A /* Print Pen Notes? */
DO:
    ASSIGN {&self-name}.
   IF v-print-fmt EQ "Accord" THEN DO: 
       IF TG_print-pen-notes THEN
           dept-code:HIDDEN IN FRAME FRAME-A = NO .
       ELSE
           dept-code:HIDDEN IN FRAME FRAME-A = YES .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME TG_whs-mths
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG_whs-mths C-Win
ON VALUE-CHANGED OF TG_whs-mths IN FRAME FRAME-A /* Warehouse Months */
DO:
   ASSIGN TG_whs-mths.

   IF TG_whs-mths = TRUE THEN
      ENABLE RS_whs-mths WITH FRAME {&FRAME-NAME}.
   ELSE 
      DISABLE RS_whs-mths WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON LEAVE OF run_format IN FRAME FRAME-A /* Warehouse Months */
DO:
   ASSIGN run_format.

   IF v-print-fmt NE run_format THEN DO:
       ASSIGN v-print-fmt =  run_format
              vcDefaultForm = v-print-fmt.
      RUN  RUN_format-value-changed .
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON HELP OF run_format IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO .
    IF rd_ack-ordmst:SCREEN-VALUE = "M" THEN
        RUN windows/l-syschrL.w (gcompany,"ACKMASTER",run_format:SCREEN-VALUE,OUTPUT char-val).
    ELSE
        RUN windows/l-syschrL.w (gcompany,"ACKHEAD",run_format:SCREEN-VALUE,OUTPUT char-val).

     IF char-val NE '' THEN
      run_format:SCREEN-VALUE = ENTRY(1,char-val).

     IF v-print-fmt NE run_format:SCREEN-VALUE THEN DO:
       ASSIGN v-print-fmt =  run_format
              vcDefaultForm = v-print-fmt.
      RUN  RUN_format-value-changed .
     END.

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

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "ACKHEAD"
      NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "ACKHEAD"
     sys-ctrl.descrip  = "Print headers on Order Acknowledgement?".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  END.

  ASSIGN
   v-print-fmt  = sys-ctrl.char-fld
   v-print-head = sys-ctrl.log-fld
   vcDefaultForm = v-print-fmt
   v-fmt-int = sys-ctrl.int-fld .

  RUN SetOEAckForm(v-print-fmt).
  viDefaultLinesPerPage = lines-per-page.

  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&frame-name}:
    {custom/usrprint.i}
    ASSIGN
      lines-per-page:SCREEN-VALUE = STRING(viDefaultLinesPerPage)
      lines-per-page = viDefaultLinesPerPage
      RS_whs-mths = "3"
      TG_whs-mths:SCREEN-VALUE      = "No"
      RS_whs-mths:SENSITIVE         = NO
      TG_whs-mths:SENSITIVE         = NO
      TG_cons_form:SCREEN-VALUE     = "No"
      TG_cons_form:SENSITIVE        = NO.

      ASSIGN
           vmDefaultForm  = AckMst-cha .

    IF LOOKUP(v-print-fmt,"Century,Fibrex,Allwest") = 0 THEN 
      ASSIGN
       tb_prt-bom = NO
       tb_prt-bom:SENSITIVE = NO
       tb_prt-bom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

    IF v-print-fmt EQ "Allwest" THEN ASSIGN TG_cons_form:SENSITIVE  = YES.

    IF v-print-fmt EQ "Indiana" THEN
      ASSIGN
       tb_sch-rel = NO
       tb_sch-rel:SENSITIVE = NO
       tb_sch-rel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
       tb_act-rel = NO
       tb_act-rel:SENSITIVE = NO
       tb_act-rel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

    IF v-print-fmt = "Dee" THEN
      ASSIGN
         TG_whs-mths:SENSITIVE = YES. 

    IF v-print-fmt EQ "Albert" THEN
       ASSIGN
         tb_prt-revise:SENSITIVE = NO
         tb_prt-revise = NO
         tb_prt-revise:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".
    /* gdm - 11091105*/
    ASSIGN tb_prt-revise:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

/*     /* task 05121402 */                           */
/*     IF v-print-fmt EQ "Badger" AND v-fmt-int EQ 1 */
/*       THEN ASSIGN                                 */
/*              TG_print-pdf-view:HIDDEN     = NO    */
/*              TG_print-pdf-view:SENSITIVE  = YES.  */
/*     ELSE                                          */
/*         ASSIGN                                    */
/*              TG_print-pdf-view:HIDDEN     = YES   */
/*              TG_print-pdf-view:SENSITIVE  = NO.   */


    /* gdm - 04160907*/
    IF v-print-fmt NE "Simkins" 
      THEN ASSIGN 
             tb_terms:HIDDEN       = YES
             lv-termFile:HIDDEN    = YES
             tb_terms:SENSITIVE    = NO
             lv-termFile:SENSITIVE = NO
             TG_print-pen-notes:HIDDEN     = YES
             TG_print-pen-notes:SENSITIVE  = YES.

    IF v-print-fmt EQ "PremierX" OR v-print-fmt = "ACPI" OR v-print-fmt EQ "PremierCX"
         THEN ASSIGN
           tb_itempo:HIDDEN = NO
           tb_hide_sell:HIDDEN = NO. 
        ELSE ASSIGN 
           tb_itempo:HIDDEN = YES
           tb_hide_sell:HIDDEN = YES .

    IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierCX" THEN
        ASSIGN
        tb_itm-tot:HIDDEN = NO . 
    ELSE
        tb_itm-tot:HIDDEN = YES  . 


    IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierCX"
         THEN ASSIGN
           tb_untcnt:HIDDEN = NO .
        ELSE ASSIGN 
           tb_untcnt:HIDDEN = YES .

    IF v-print-fmt EQ "PremierX"
            THEN ASSIGN
            tb_shpnot:HIDDEN = NO .
        ELSE ASSIGN 
            tb_shpnot:HIDDEN = YES . 

    IF v-print-fmt EQ "Badger"
         THEN ASSIGN
           TG_print-due-cd:HIDDEN = NO. 
        ELSE ASSIGN 
           TG_print-due-cd:HIDDEN = YES .
/*                                  */
/*     IF v-print-fmt EQ "ACPI"     */
/*          THEN ASSIGN             */
/*             tb_itempo:HIDDEN = NO. */

    IF v-print-fmt EQ "Soule" OR v-print-fmt EQ "SouleUOM"
      THEN ASSIGN 
             tb_print-component:HIDDEN       = NO .
        ELSE
             tb_print-component:HIDDEN       = YES .

    IF v-print-fmt EQ "Axis"
      THEN ASSIGN 
             tb_billnotes:HIDDEN       = NO .
        ELSE
             tb_billnotes:HIDDEN       = YES .

    IF v-print-fmt EQ "Simkins" THEN DO:      

      ASSIGN           
          lv-termFile =  lv-termFile:SCREEN-VALUE
          lv-termFile:SCREEN-VALUE = lv-termFile.

      IF TRIM(lv-termFile) EQ "" THEN DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company      EQ cocode
            AND sys-ctrl-shipto.NAME         EQ "ACKHEAD" 
            AND sys-ctrl-shipto.cust-vend    EQ YES 
            AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
        IF AVAIL sys-ctrl-shipto 
          THEN ASSIGN lv-termFile = sys-ctrl-shipto.char-fld.
      END.

      ASSIGN 
        tb_terms                 = NO
        tb_terms:SENSITIVE       = YES.

      IF TRIM(cocode) EQ "011"  OR
         TRIM(cocode) EQ "11" /* Landrum */
       THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms11.txt".
       ELSE 
        IF TRIM(cocode) EQ "012" OR
           TRIM(cocode) EQ "12" /* Marietta */
         THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms12.txt".
         ELSE 
          IF TRIM(cocode) EQ "060" OR
             TRIM(cocode) EQ "60" /* Harvard Folding Box */
            THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms60.txt".
            ELSE 
                IF TRIM(cocode) EQ "062" OR
                   TRIM(cocode) EQ "62" /* Ideal */
                  THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms62.txt".
                  ELSE ASSIGN lv-termFile:SCREEN-VALUE = lv-termFile.


      ASSIGN lv-termFile:SCREEN-VALUE = lv-termFile
             lv-termFile:SENSITIVE = NO.

      IF tb_terms:SCREEN-VALUE EQ "YES" 
        THEN lv-termFile:SENSITIVE = YES.

    END.
    ELSE ASSIGN
          tb_terms:SENSITIVE       = NO
          lv-termFile:SCREEN-VALUE = ""
          lv-termFile              = ""
          lv-termFile:SENSITIVE    = NO.


    IF LOOKUP(v-print-fmt,"Soule,SouleUOM,ContSvc") = 0 THEN DO:
        IF tb_sch-rel:SCREEN-VALUE EQ "NO" AND
            tb_act-rel:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                tb_ship-to:SENSITIVE = NO
                tb_ship-to:SCREEN-VALUE = "NO".
    END.
    IF LOOKUP(v-print-fmt,"PremierX") <> 0 THEN DO:
        IF tb_sch-rel:SCREEN-VALUE EQ "NO" AND
            tb_act-rel:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                tb_shpnot:SENSITIVE = NO
                tb_shpnot:SCREEN-VALUE = "NO".
    END.
    IF v-print-fmt EQ "Accord" THEN DO:
         ASSIGN 
             TG_print-pen-notes:HIDDEN     = NO
             TG_print-pen-notes:SENSITIVE  = YES.

            IF tb_inst:SCREEN-VALUE EQ "Yes" THEN
             spec-code:HIDDEN IN FRAME FRAME-A = NO .
            ELSE
             spec-code:HIDDEN IN FRAME FRAME-A = YES .

             IF TG_print-pen-notes:SCREEN-VALUE EQ "Yes" THEN
                dept-code:HIDDEN IN FRAME FRAME-A = NO .
            ELSE
                dept-code:HIDDEN IN FRAME FRAME-A = YES .
     END.
     ELSE
         ASSIGN spec-code:HIDDEN IN FRAME FRAME-A = YES 
                    dept-code:HIDDEN IN FRAME FRAME-A = YES .

         IF AckMst-log = TRUE THEN DO:
             rd_ack-ordmst:HIDDEN IN FRAME {&FRAME-NAME} = NO.
             IF rd_ack-ordmst:SCREEN-VALUE = "M"  THEN do:
                 FIND FIRST sys-ctrl NO-LOCK
                 WHERE sys-ctrl.company EQ cocode
                 AND sys-ctrl.name    EQ "ACKMASTER"
                 NO-ERROR.
             IF AVAIL sys-ctrl THEN
                 ASSIGN
                 v-print-fmt  = sys-ctrl.char-fld
                 v-print-head = sys-ctrl.log-fld
                 vcDefaultForm = v-print-fmt
                 v-fmt-int = sys-ctrl.int-fld .
 
                 ASSIGN
                 tb_reprint:SENSITIVE = NO 
                 TG_cons_form:SENSITIVE = NO 
                 TG_whs-mths:SENSITIVE = NO                         
                 RS_whs-mths:SENSITIVE = NO                         
                 tb_sch-rel:SENSITIVE = NO                          
                 tb_inst:SENSITIVE = NO                             
                 tb_ship-to:SENSITIVE = NO                          
                 tb_act-rel:SENSITIVE = NO                          
                 tb_prt-revise:SENSITIVE = NO                       
                 tb_prt-bom:SENSITIVE = NO                          
                 TG_print-due-cd:SENSITIVE = NO                     
                 TG_print-pen-notes:SENSITIVE = NO                  
                 tb_print-component:SENSITIVE = NO                  
                 tb_hide_sell:SENSITIVE = NO                        
                 spec-code:SENSITIVE = NO                           
                 lv-termFile:SENSITIVE = NO                         
                 dept-code:SENSITIVE = NO                           
                 tb_itempo:SENSITIVE = NO                           
                 tb_untcnt:SENSITIVE = NO  
                 tb_terms:SENSITIVE = NO 
                 tb_shpnot:SENSITIVE = NO.     
             END.
         END.
         ELSE
             rd_ack-ordmst:HIDDEN IN FRAME {&FRAME-NAME} = YES .

    IF TG_eml-push-att:SCREEN-VALUE EQ "Yes" THEN
      sel-attch:HIDDEN IN FRAME FRAME-A = NO .
    ELSE 
       sel-attch:HIDDEN IN FRAME FRAME-A = YES .
    RUN get-cust-attch .


    IF NOT lAsiUser THEN
         RUN_format:HIDDEN IN FRAME FRAME-A = YES .
     ELSE 
         RUN_format:SCREEN-VALUE IN FRAME FRAME-A = v-print-fmt .

    APPLY "entry" TO begin_ord-no.


  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BatchMail C-Win
PROCEDURE BatchMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF v-ack-master = YES THEN DO:

      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "ACKMASTER") THEN
     DO:
        IF CAN-FIND(FIRST b-oe-ord
           WHERE b-oe-ord.company  EQ cocode
             AND b-oe-ord.ord-no   GE ford-no
             AND b-oe-ord.ord-no   LE tord-no
             AND b-oe-ord.cust-no  GE begin_cust-no
             AND b-oe-ord.cust-no  LE end_cust-no
             AND b-oe-ord.ord-date GE fdate
             AND b-oe-ord.ord-date LE tdate
             AND b-oe-ord.ack-prnt EQ v-reprint) THEN
           FOR EACH b1-oe-ord FIELDS(company cust-no ord-no) WHERE
               b1-oe-ord.company  EQ cocode AND
               b1-oe-ord.ord-no   GE ford-no AND
               b1-oe-ord.ord-no   LE tord-no AND
               b1-oe-ord.cust-no  GE begin_cust-no AND
               b1-oe-ord.cust-no  LE end_cust-no AND
               b1-oe-ord.ord-date GE fdate AND
               b1-oe-ord.ord-date LE tdate AND
               b1-oe-ord.ack-prnt EQ v-reprint
               NO-LOCK
               BREAK BY b1-oe-ord.company
                     BY b1-oe-ord.cust-no:

               IF FIRST-OF(b1-oe-ord.cust-no) THEN DO:

                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "ACKMASTER" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b1-oe-ord.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     RUN SetAckMstForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetAckMstForm (vmDefaultForm).
                     v-print-fmt = vmDefaultForm.
                  END.
                  RUN output-to-mail (b1-oe-ord.cust-no, b1-oe-ord.ord-no).        

               END.
           END. /* FOR EACH*/
        ELSE
           MESSAGE "No Order Acknowledgements Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /*if can-find sys-ctrl-shipto*/
  ELSE
     DO:

        v-print-fmt = vmDefaultForm.
        RUN SetAckMstForm (v-print-fmt).
        RUN output-to-mail (begin_cust-no,begin_ord-no).
/*        RUN SetGlobalVariables(INPUT begin_ord-no).   */
/*        RUN run-report("", FALSE).                    */
/*        RUN GenerateEmail(begin_cust-no, end_cust-no).*/
     END.

    END.  /* end of v-ack-master log = yes */


    ELSE DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "ACKHEAD") THEN
     DO:
        IF CAN-FIND(FIRST b-oe-ord
           WHERE b-oe-ord.company  EQ cocode
             AND b-oe-ord.ord-no   GE ford-no
             AND b-oe-ord.ord-no   LE tord-no
             AND b-oe-ord.cust-no  GE begin_cust-no
             AND b-oe-ord.cust-no  LE end_cust-no
             AND b-oe-ord.ord-date GE fdate
             AND b-oe-ord.ord-date LE tdate
             AND b-oe-ord.ack-prnt EQ v-reprint) THEN
           FOR EACH b-oe-ord FIELDS(company cust-no ord-no) WHERE
               b-oe-ord.company  EQ cocode AND
               b-oe-ord.ord-no   GE ford-no AND
               b-oe-ord.ord-no   LE tord-no AND
               b-oe-ord.cust-no  GE begin_cust-no AND
               b-oe-ord.cust-no  LE end_cust-no AND
               b-oe-ord.ord-date GE fdate AND
               b-oe-ord.ord-date LE tdate AND
               b-oe-ord.ack-prnt EQ v-reprint
               NO-LOCK
               BREAK BY b-oe-ord.company
                     BY b-oe-ord.cust-no:

               IF FIRST-OF(b-oe-ord.cust-no) THEN DO:

                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "ACKHEAD" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-ord.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     RUN SetOEAckForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetOEAckForm (vcDefaultForm).
                     v-print-fmt = vcDefaultForm.
                  END.
                  RUN output-to-mail (b-oe-ord.cust-no, b-oe-ord.ord-no).       
/*                  RUN SetGlobalVariables(b-oe-ord.ord-no).            */
/*                  RUN run-report(b-oe-ord.cust-no, TRUE).             */
/*                  RUN GenerateEmail(b-oe-ord.cust-no,b-oe-ord.ord-no).*/
               END.
           END. /* FOR EACH*/
        ELSE
           MESSAGE "No Order Acknowledgements Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /*if can-find sys-ctrl-shipto*/
     ELSE DO:
        v-print-fmt = vcDefaultForm.
        RUN SetOEAckForm (v-print-fmt).
        RUN output-to-mail (begin_cust-no,begin_ord-no). 
/*        RUN SetGlobalVariables(INPUT begin_ord-no).    */
/*        RUN run-report("", FALSE).                     */
/*        RUN GenerateEmail(begin_cust-no, begin_ord-no).*/
     END.

    END. /* else of v-ack-master  */


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildToList C-Win
PROCEDURE buildToList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipRecKey    AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipEMail     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipCode      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opToList    AS CHARACTER NO-UNDO.

  DEF BUFFER b2-cust FOR cust.

  FOR EACH phone NO-LOCK 
     WHERE phone.table_rec_key EQ ipRecKey
        OR ipRecKey EQ 'ALL':

    IF CAN-FIND(FIRST emaildtl
                WHERE emaildtl.emailcod       EQ ipCode
                  AND emaildtl.table_rec_key  EQ phone.rec_key) 
       OR
       phone.titlcode EQ ipCode THEN
    DO:

      IF phone.e_mail NE '' AND NOT CAN-DO(opToList,phone.e_mail) THEN 
      DO:        
        opToList = opToList + (IF opToList NE '' THEN ',' 
                                                  ELSE '') 
                            + phone.e_mail.
      END.
    END.

  END. /* each phone */

  IF opToList EQ '' OR opToList = ? THEN DO: 

     FIND FIRST b2-cust 
           WHERE b2-cust.rec_key = ipRecKey
             AND b2-cust.active  = 'X' 
           NO-LOCK NO-ERROR.
     IF AVAIL b2-cust THEN DO: 

        FOR EACH phone NO-LOCK
           WHERE phone.table_rec_key = b2-cust.rec_key,
            EACH reftable NO-LOCK
           WHERE reftable.rec_key = phone.rec_key
             AND reftable.CODE    = ipCode:

          opToList = opToList + (IF opToList NE '' THEN ',' 
                                                   ELSE '') 
                              + phone.e_mail.
        END.

        IF opToList EQ '' OR opToList EQ ? THEN 
          opToList = b2-cust.email.
     END.
     ELSE opToList = ipEMail.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE email-albert C-Win 
PROCEDURE email-albert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-cust-from AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-cust-to AS CHAR NO-UNDO.

   {custom/asimail2.i &TYPE = "CUSTOMER"
                      &group-title='r-acknow.'
                      &begin_cust=ip-cust-from
                      &END_cust=ip-cust-to
                      &mail-subject="Acknowledgement"
                      &mail-body="Acknowledgement"
                      &mail-file=lv-pdf-file + ".pdf"}
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
  DISPLAY rd_ack-ordmst begin_ord-no end_ord-no begin_cust-no end_cust-no 
          begin_due-date end_due-date begin_relnum end_relnum tb_reprint 
          TG_cons_form TG_whs-mths RS_whs-mths tb_sch-rel tb_inst tb_ship-to 
          tb_act-rel tb_prt-revise tb_prt-bom tb_billnotes tb_terms tb_itempo 
          TG_print-pen-notes tb_untcnt tb_print-component TG_print-due-cd 
          tb_hide_sell tb_itm-tot tb_shpnot spec-code lv-termFile dept-code 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name sel-attch 
          TG_eml-push-att TG_preview td-show-parm run_format
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-8 RECT-9 rd_ack-ordmst begin_ord-no end_ord-no 
         begin_cust-no end_cust-no begin_due-date end_due-date begin_relnum 
         end_relnum tb_reprint TG_cons_form TG_whs-mths RS_whs-mths tb_sch-rel 
         tb_inst tb_ship-to tb_act-rel tb_prt-revise tb_prt-bom tb_billnotes 
         tb_terms tb_itempo TG_print-pen-notes tb_untcnt tb_print-component 
         TG_print-due-cd tb_hide_sell tb_itm-tot tb_shpnot spec-code 
         lv-termFile dept-code rd-dest lv-ornt lines-per-page lv-font-no 
         sel-attch TG_eml-push-att TG_preview td-show-parm run_format btn-ok btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateEmail C-Win
PROCEDURE GenerateEmail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER icCustNo AS CHAR NO-UNDO.
  DEF INPUT PARAM icOrdNo AS INT NO-UNDO.


  DEFINE VARIABLE vcSubject   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMailBody  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.

  DEF BUFFER b1-cust FOR cust.

  DEFINE VARIABLE ls-to-list          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-mailto           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-mailsubject      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-mailbody         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-mailattach       AS CHARACTER NO-UNDO.
  DEF VAR ls-to-list2 AS cha NO-UNDO.

  IF is-xprint-form THEN RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

  ASSIGN  vcSubject   = "ACKNOWLEDGEMENT:" + string(icOrdNo) + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
          vcMailBody  = "Please review attached Order Acknowledgement #: " + string(icOrdNo).       

  FIND b1-cust WHERE b1-cust.company = cocode AND b1-cust.cust-no = icCustNo NO-LOCK NO-ERROR.
  RUN buildToList (INPUT  b1-cust.rec_key,   /* Rec_Key        */
                    INPUT  b1-cust.email,     /* Email Address  */
                    INPUT  "r-acknow.",  /* Title          */
                    OUTPUT ls-to-list).    /* Recepients     */

   /* build list for shipto */
  ls-to-list2 = "".
  FOR EACH b2-oe-ord WHERE b2-oe-ord.company = cocode AND b2-oe-ord.ord-no = icOrdno NO-LOCK,
       EACH oe-rel WHERE oe-rel.company = cocode
                     AND oe-rel.ord-no = oe-ord.ord-no NO-LOCK  :
          FIND FIRST shipto WHERE shipto.company = cocode
                              AND shipto.cust-no = b2-oe-ord.cust-no
                              AND shipto.ship-no = oe-rel.ship-no NO-LOCK NO-ERROR.
          IF AVAIL shipto THEN DO:              
             RUN buildToList (INPUT  shipto.rec_key,   /* Rec_Key        */
                    INPUT  "",     /* Email Address  */
                    INPUT  'r-acknow.',      /* Title          */
                    OUTPUT ls-to-list2).    /* Recepients     */

          END.

  END.

  IF NOT is-xprint-form THEN
    OS-COPY VALUE(list-name) VALUE(lv-pdf-file + ".txt").
  IF ls-to-list2 <> "" THEN ls-to-list = ls-to-list + "," + ls-to-list2.       
  ASSIGN lv-mailto       = 'To:' + ls-to-list
         lv-mailsubject  = vcSubject
         lv-mailbody     = vcMailBody
         lv-mailattach   =  IF is-xprint-form THEN lv-pdf-file + ".pdf" ELSE lv-pdf-file + ".txt" /*list-name*/.

  IF lv-mailattach MATCHES('*xpr*') AND SEARCH('viewer.exe') NE ? THEN
      ASSIGN  FILE-INFO:FILE-NAME = 'viewer.exe'
              lv-mailattach       = FILE-INFO:FULL-PATHNAME + ',' + lv-mailattach.

   IF lv-attach-push NE "" AND TG_eml-push-att:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "Yes" THEN
            lv-mailattach = lv-mailattach + "," + lv-attach-push .

  RUN mail (lv-mailto,        /* Mail Recepients  */
            lv-mailsubject,   /* Subject          */
            lv-mailbody,      /* Body             */
            lv-mailattach,    /* Attachment       */
            1,                /* Mail Dialog Type */
            OUTPUT retcode).  /* Return Code      */

/*  RUN custom/xpmail2.p   (input   icRecType,  */
/*                          input   'R-ACKNOW.',*/
/*                          input   list-name,  */
/*                          input   icIdxKey,   */
/*                          input   vcSubject,  */
/*                          input   vcMailBody, */
/*                          OUTPUT  vcErrorMsg).*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateReport C-Win 
PROCEDURE GenerateReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

   DEFINE INPUT PARAMETER ip-cust-from AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER ip-cust-to AS CHAR NO-UNDO.
   DEFINE VARIABLE vcErrorMsg  AS CHARACTER  NO-UNDO.
DEF VAR gb AS CHAR NO-UNDO .
   IF v-print-fmt NE "Albert" THEN
      CASE rd-dest:
         WHEN 1 THEN RUN output-to-printer.
         WHEN 2 THEN RUN output-to-screen.
         WHEN 3 THEN RUN output-to-file.
         WHEN 4 THEN DO:
             IF is-xprint-form THEN DO:
                RUN output-to-fax-prt. /* create tif file */              
                {custom/asifaxm3.i &TYPE="Customer"
                              &begin_cust=ip-cust-from
                              &END_cust=ip-cust-to
                              &fax-subject="Acknowledgement"
                              &fax-body="Acknowledgement"
                              &fax-file=ls-fax-file
                              &end-widget=end_cust-no }      
             END.
             ELSE DO:
                 {custom/asifax3.i &begin_cust=ip-cust-from
                                  &END_cust=ip-cust-to
                                  &fax-subject="Acknowledgement"
                                  &fax-body="Acknowledgement"
                                  &fax-file=list-name
                                  &end-widget=end_cust-no }
             END.
         END.
         WHEN 5 THEN DO:
             IF is-xprint-form THEN DO:
/*                   RUN printfile (FILE-INFO:FILE-NAME). */
/*                 def var check-mail as logic init no no-undo.                               */
                RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
/*                 if v-fmt-int = 1 and v-print-fmt = "Badger" then do: /* task 05121402 */   */
/*                 if TG_print-pdf-view then                                                  */
/*                 OS-COMMAND NO-WAIT START AcroRd32.exe VALUE(SEARCH(lv-pdf-file + ".pdf")). */
/*                                                                                            */
/*                  MESSAGE "Would you like to send an acknowledgement?"                      */
/*                         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO    UPDATE check-mail.     */
/*                 if not check-mail then return .                                            */
/*                 end.                                                                       */

               IF NOT TG_eml-push-att THEN DO:
                {custom/asimail2.i &TYPE = "CUSTOMER"
                               &group-title='r-acknow.' /* v-prgmname */
                               &begin_cust=ip-cust-from
                               &END_cust=ip-cust-to
                               &mail-subject="Acknowledgement"
                               &mail-body="Acknowledgement"
                               &mail-file=lv-pdf-file + ".pdf"}  
               END.
                ELSE DO:
                     lv-attach-push = lv-attach-push + lv-pdf-file + ".pdf" .
                     RUN custom/xpmail2.p   (INPUT   "CUSTOMER",
                          INPUT   'r-acknow.',
                          INPUT   lv-attach-push,
                          INPUT   ip-cust-from,
                          INPUT   "Acknowledgement",
                          INPUT   "Acknowledgement",
                          OUTPUT  vcErrorMsg). 
                END.
             END.
             ELSE DO:
                 {custom/asimailr2.i &TYPE = "CUSTOMER"
                                    &group-title='r-acknow.' /* v-prgmname */
                                    &begin_cust=ip-cust-from
                                    &END_cust=ip-cust-to
                                    &mail-subject="Acknowledgement"
                                    &mail-body="Acknowledgement"
                                    &mail-file=list-name }

             END.
         END. 
         WHEN 6 THEN RUN output-to-port.
      END CASE.
      ELSE /*Albert*/
         IF rd-dest EQ 5 THEN
            RUN email-albert(ip-cust-from, ip-cust-to).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-cust-attch C-Win 
PROCEDURE Get-cust-attch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-attch-list AS CHAR NO-UNDO.
DO WITH FRAME {&frame-name}:

    IF TG_eml-push-att:SCREEN-VALUE = "YES" THEN DO:

        FIND FIRST cust WHERE cust.company EQ cocode
            AND cust.cust-no EQ begin_cust-no:SCREEN-VALUE  NO-LOCK NO-ERROR . 
        IF AVAIL cust THEN
            FOR EACH attach WHERE attach.rec_key = cust.rec_key
                               AND ATTACH.est-no = begin_ord-no:SCREEN-VALUE NO-LOCK:
            IF attach.attach-file <> ""  THEN
                v-attch-list = v-attch-list + attach.attach-file + "," .
            END.

    END. /* TG_eml-push-att */
            sel-attch:LIST-ITEMS = v-attch-list.
END.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelected C-Win 
PROCEDURE GetSelected :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
lv-attach-push = "".
DO WITH FRAME {&frame-name}:
    DO i = 1 TO sel-attch:NUM-ITEMS:
      IF sel-attch:IS-SELECTED(i) THEN
          lv-attach-push = lv-attach-push + TRIM(sel-attch:ENTRY(i)) + ",".
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
     lv-xpr-file = FILE-INFO:FULL-PATHNAME.

     RUN printfile (lv-xpr-file).
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
PROCEDURE output-to-mail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.
DEFINE INPUT PARAM icOrdNo AS INT NO-UNDO.

IF v-ack-master = YES THEN DO:

      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "ACKMASTER") THEN
     DO:
        IF CAN-FIND(FIRST b-oe-ord
           WHERE b-oe-ord.company  EQ cocode
             AND b-oe-ord.cust-no  EQ icCustNo
             AND b-oe-ord.ord-no   GE icOrdNo
             AND b-oe-ord.ack-prnt EQ v-reprint) THEN
           FOR EACH b-oe-ord FIELDS(company cust-no ord-no) WHERE
               b-oe-ord.company  EQ cocode AND
               b-oe-ord.cust-no  GE icCustNo AND
               b-oe-ord.ord-no   EQ icOrdNo AND
                b-oe-ord.ack-prnt EQ v-reprint
               NO-LOCK
               BREAK BY b-oe-ord.company
                     BY b-oe-ord.cust-no:

               IF FIRST-OF(b-oe-ord.cust-no) THEN DO:

                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "ACKMASTER" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-ord.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     RUN SetAckMstForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetAckMstForm (vmDefaultForm).
                     v-print-fmt = vmDefaultForm.
                  END.

                  RUN SetGlobalVariables(b-oe-ord.ord-no).
                  RUN run-report(b-oe-ord.cust-no, TRUE).
                  RUN GenerateEmail(b-oe-ord.cust-no,b-oe-ord.ord-no).
               END.
           END. /* FOR EACH*/
        ELSE
           MESSAGE "No Order Acknowledgements Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /*if can-find sys-ctrl-shipto*/
  ELSE
     DO:

        v-print-fmt = vmDefaultForm.
        RUN SetAckMstForm (v-print-fmt).
        RUN SetGlobalVariables(INPUT begin_ord-no).
        RUN run-report("", FALSE).
        RUN GenerateEmail(icCustNo,begin_ord-no).
     END.

    END.  /* end of v-ack-master log = yes */


    ELSE DO:
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
     sys-ctrl-shipto.company = cocode AND
     sys-ctrl-shipto.NAME = "ACKHEAD") THEN
     DO:
        IF CAN-FIND(FIRST b-oe-ord
           WHERE b-oe-ord.company  EQ cocode
             AND b-oe-ord.cust-no  EQ icCustNo
             AND b-oe-ord.ord-no   GE icOrdNo
             AND b-oe-ord.ack-prnt EQ v-reprint) THEN
           FOR EACH b-oe-ord FIELDS(company cust-no ord-no) WHERE
               b-oe-ord.company  EQ cocode AND
               b-oe-ord.cust-no  EQ icCustNo AND
               b-oe-ord.ord-no   GE icOrdNo AND
               b-oe-ord.ack-prnt EQ v-reprint
               NO-LOCK
               BREAK BY b-oe-ord.company
                     BY b-oe-ord.cust-no:

               IF FIRST-OF(b-oe-ord.cust-no) THEN DO:

                  FIND FIRST sys-ctrl-shipto WHERE
                       sys-ctrl-shipto.company = cocode AND
                       sys-ctrl-shipto.NAME = "ACKHEAD" AND
                       sys-ctrl-shipto.cust-vend = YES AND
                       sys-ctrl-shipto.cust-vend-no = b-oe-ord.cust-no AND
                       sys-ctrl-shipto.char-fld > ''
                       NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                  DO:
                     RUN SetOEAckForm (sys-ctrl-shipto.char-fld).
                     v-print-fmt = sys-ctrl-shipto.char-fld.
                  END.
                  ELSE
                  DO:
                     RUN SetOEAckForm (vcDefaultForm).
                     v-print-fmt = vcDefaultForm.
                  END.

                  RUN SetGlobalVariables(b-oe-ord.ord-no).
                  RUN run-report(b-oe-ord.cust-no, TRUE).
                  RUN GenerateEmail(b-oe-ord.cust-no,b-oe-ord.ord-no).
               END.
           END. /* FOR EACH*/
        ELSE
           MESSAGE "No Order Acknowledgements Were Printed."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END. /*if can-find sys-ctrl-shipto*/
     ELSE DO:
        v-print-fmt = vcDefaultForm.
        RUN SetOEAckForm (v-print-fmt).

        RUN SetGlobalVariables(INPUT begin_ord-no).
        RUN run-report("", FALSE).
        RUN GenerateEmail(icCustNo,begin_ord-no).
     END.

    END. /* else of v-ack-master  */



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
 IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
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
  ELSE
     RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* --------------------------------------------------- po/po-print.p 10/94 rd */
/* Purchase Order Print Program - P/O Module                                  */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.
DEFINE INPUT PARAM ip-sys-ctrl-shipto AS LOG NO-UNDO.

SESSION:SET-WAIT-STATE("general").

{sys/form/r-top.i}
{sys/inc/print1.i}

ASSIGN
 ford-no   = begin_ord-no
 tord-no   = end_ord-no
 fdate     = begin_due-date
 tdate     = END_due-date
 v-schrel  = tb_sch-rel
 v-actrel  = tb_act-rel
 v-shipto  = tb_ship-to
 v-reprint = tb_reprint
 v-prntinst = tb_inst
 v-rs-whs-mths = RS_whs-mths
 v-tg-whs-mths = TG_whs-mths
 v-print-pen-notes = TG_print-pen-notes
 spec-list     = spec-code
 v-dept-codes  = dept-code
 v-print-components = tb_print-component
 slBillNotes = tb_billnotes
 v-print-po = tb_itempo
 v-UntCnt = tb_untcnt
 v-Shpnot = tb_shpnot
 v-print-due = TG_print-due-cd
 v-print-tot = tb_itm-tot 
    .

 IF lAsiUser THEN DO:
     ASSIGN v-print-fmt =  run_format
         vcDefaultForm = v-print-fmt.
     RUN SetOEAckForm(v-print-fmt).
     viDefaultLinesPerPage = lines-per-page.
  END.

IF ip-sys-ctrl-shipto THEN
   ASSIGN
      fcust = icCustNo
      tcust = icCustNo.
ELSE
   ASSIGN
      fcust = begin_cust-no
      tcust = end_cust-no.

{sa/sa-sls01.i}

IF v-print-fmt EQ "Allwest" AND
   TG_cons_form AND
   ford-no NE 0 AND
   tord-no NE 0 AND
   TRIM(tcust) EQ "" 
  THEN tcust = "zzzzzzzz".

FOR EACH oe-ord
    WHERE oe-ord.company  EQ cocode
      AND oe-ord.ord-no   GE ford-no
      AND oe-ord.ord-no   LE tord-no
      AND oe-ord.cust-no  GE fcust
      AND oe-ord.cust-no  LE tcust
      AND oe-ord.ord-date GE fdate
      AND oe-ord.ord-date LE tdate
      AND oe-ord.ack-prnt EQ v-reprint
    NO-LOCK:    

  CREATE report.
  ASSIGN
   report.term-id = v-term
   report.rec-id  = RECID(oe-ord)
   report.key-01 = STRING(oe-ord.ack-prnt).

END.

{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

ASSIGN
 v-lines-per-page = lines-per-page
 v-term-id        = v-term.

IF IS-xprint-form THEN DO:
   CASE rd-dest:
       WHEN 1 THEN PUT  "<PRINTER?>".
       WHEN 2 THEN do:
           IF NOT lBussFormModle THEN
            PUT "<PREVIEW><MODAL=NO>". 
           ELSE
            PUT "<PREVIEW>".        
       END.
       WHEN 4 THEN DO:
             ls-fax-file = "c:\tmp\fx" + STRING(TIME) + ".tif".
             PUT UNFORMATTED "<PRINT=NO><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
       END.        
       WHEN 5 THEN DO:
           IF TG_preview THEN PUT "<PREVIEW>". ELSE PUT "<PRINT=NO>".
            IF LOOKUP(v-print-fmt,"Century,Unipak,Axis,Soule,SouleUOM,APC,Perform,Fibrex,Allwest,Simkins,HOPX,Carded") > 0 THEN
                PUT "<FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE IF LOOKUP(v-print-fmt,"Frankstn,3CPack,3CPackSD") > 0 THEN /* task 07211402 */
                 PUT "<PDF-LEFT=2mm><PDF-TOP=5mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE PUT "<PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
       END.
   END CASE.
   PUT "</PROGRESS>".
END.       
clXMLOutput = YES /* rstark 05291402 */.
IF v-ack-master THEN DO:
    RUN VALUE(v-program).
END.
ELSE DO:
    IF CAN-DO("Frankstn,3CPack,3CPackSD,Mirpkg,PPI,Indiana,ContSvc,HPB,Packrite",v-print-fmt) THEN RUN VALUE(v-program) (v-print-fmt).
    ELSE IF LOOKUP(v-print-fmt,"Century,Unipak,Axis,Soule,SouleUOM,APC,Perform,Fibrex,Dee,Allwest,Accord") > 0 THEN RUN VALUE(v-program) (tb_prt-revise).
    ELSE RUN VALUE(v-program).
END.

OUTPUT CLOSE.

FOR EACH report WHERE report.term-id EQ v-term-id:
   FIND oe-ord WHERE RECID(oe-ord) EQ report.rec-id.
   IF STRING(oe-ord.ack-prnt) <> report.key-01 AND oe-ord.ack-prnt THEN oe-ord.ack-prnt-date = TODAY.

   IF acksps-log THEN RUN oe/oe850ack.p (oe-ord.ord-no).
   
  DELETE report.
END.

IF v-ack-master = NO THEN
IF tb_prt-bom THEN DO:
   {sys/inc/outprt2.i 60 APPEND}
   RUN run-report-bom.
   OUTPUT CLOSE.
END.


RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-bom C-Win 
PROCEDURE run-report-bom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN   fjob-no   = FILL(" ",6 - LENGTH(STRING(begin_ord-no))) +
                          STRING(begin_ord-no)
     tjob-no   = FILL(" ",6 - LENGTH(STRING(end_ord-no))) +
                 STRING(end_ord-no)
     fjob-no2  = 0
     tjob-no2  = 99
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99")
     s-print-revised = tb_prt-revise.
     IF v-print-fmt EQ "CENTURY"  THEN
        RUN cerep/bomcbox.p (YES) .
     ELSE
         RUN cerep/bomcbox.p(NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAckMstForm C-Win 
PROCEDURE SetAckMstForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAM icPrintFormat  AS CHAR NO-UNDO.

   v-print-fmt = icPrintFormat.

   CASE icPrintFormat:

       WHEN "3CPack" THEN ASSIGN v-program = "oe/rep/ackm3cpk.p" is-xprint-form = YES lines-per-page = 65.
       OTHERWISE ASSIGN v-program = "oe/rep/ackm3cpk.p" is-xprint-form = YES lines-per-page = 65.

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetGlobalVariables C-Win 
PROCEDURE SetGlobalVariables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-ord-no AS INT NO-UNDO.

  ASSIGN 
     lv-pdf-file = v-dir + (IF v-print-fmt EQ "Century" OR 
                               v-print-fmt EQ "Allwest"
                              THEN "CBXOrder"
                              ELSE "Order") + STRING(ip-ord-no).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetOEAckForm C-Win 
PROCEDURE SetOEAckForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAM icPrintFormat  AS CHAR NO-UNDO.

   v-print-fmt = icPrintFormat.

   CASE icPrintFormat:
       WHEN "WesInd" THEN ASSIGN v-program = "oe/rep/wiackn.p" is-xprint-form = NO lines-per-page = 60.
       WHEN "HOP" THEN ASSIGN tb_ship-to = NO v-program = "oe/rep/acknhop.p" is-xprint-form = NO lines-per-page = 45.
       WHEN "Brick" THEN ASSIGN v-program = "oe/rep/ackbrick.p" is-xprint-form = NO lines-per-page = 60.  
       WHEN "Gulf" THEN ASSIGN v-program = "oe/rep/ackgulf.p" is-xprint-form = NO lines-per-page = 55.
       WHEN "Pacific" THEN ASSIGN v-program = "oe/rep/ackpacif.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Hopx" THEN ASSIGN v-program = "oe/rep/ackhopx.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Carded" THEN ASSIGN v-program = "oe/rep/ackcared.p" is-xprint-form = YES lines-per-page = 68.  /* Task 09181303  */
       WHEN "XPrint" OR WHEN "ackhead 1" OR WHEN "ackhead 2" THEN ASSIGN v-program = "oe/rep/ackxprnt.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "ackhead 10" OR WHEN "ackhead 20" THEN ASSIGN v-program = "oe/rep/ackxprnt10.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Shamrock-Ack" THEN ASSIGN v-program = "oe/rep/ackshamrock.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "ackhead10-CAN" THEN ASSIGN v-program = "oe/rep/ackcan.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Badger" THEN ASSIGN v-program = "oe/rep/ackbager.p" is-xprint-form = YES lines-per-page = 67. /* task 04021401 */
       WHEN "Hughes" THEN ASSIGN v-program = "oe/rep/ackhughes.p" is-xprint-form = YES lines-per-page = 65. 
       WHEN "Accord" THEN ASSIGN v-program = "oe/rep/ackacord.p" is-xprint-form = YES lines-per-page = 72.
       WHEN "Imperial" THEN ASSIGN v-program = "oe/rep/ackimprl.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Ruddx" THEN ASSIGN v-program = "oe/rep/ackruddx.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Shelby" THEN ASSIGN v-program = "oe/rep/ackshlby.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "PremierX" THEN ASSIGN v-program = "oe/rep/ackxprem.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "ACPI" THEN ASSIGN v-program = "oe/rep/ackacpi.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "PremierCX" THEN ASSIGN v-program = "oe/rep/ackcxprem.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Century" THEN ASSIGN v-program = "oe/rep/ackcentx.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "APC" THEN ASSIGN v-program = "oe/rep/ackxapc.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Perform" THEN ASSIGN v-program = "oe/rep/ackprfrm.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Unipak" THEN ASSIGN v-program = "oe/rep/ackunipk.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Axis" THEN ASSIGN v-program = "oe/rep/ackaxis.p" is-xprint-form = YES lines-per-page = 68.
       WHEN "Soule" THEN ASSIGN v-program = "oe/rep/acksoule.p" is-xprint-form = YES lines-per-page = 69. /*Soule */
       WHEN "SouleUOM" THEN ASSIGN v-program = "oe/rep/acksolUom.p" is-xprint-form = YES lines-per-page = 65. /*SouleUOM */ /*12031306*/
       WHEN "Oracle" THEN ASSIGN v-program = "oe/rep/ackoracl.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "OTTPkg" THEN ASSIGN v-program = "oe/rep/ackottpk.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Frankstn" OR WHEN "MirPkg" THEN ASSIGN v-program = "oe/rep/ackfrank.p" is-xprint-form = YES
                                                    lines-per-page = 65.
       WHEN "3CPack" THEN ASSIGN v-program = "oe/rep/ack3cpak.p" is-xprint-form = YES lines-per-page = 75.
       WHEN "3CPackSD" THEN ASSIGN v-program = "oe/rep/ack3cpaksd.p" is-xprint-form = YES lines-per-page = 75.
       WHEN "HPB" THEN ASSIGN v-program = "oe/rep/ackhpb.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "PPI" THEN ASSIGN v-program = "oe/rep/ackppi.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Southpak" THEN ASSIGN v-program = "oe/rep/acksthpk.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Indiana"  THEN ASSIGN v-program = "oe/rep/ackindc.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Fibrex" THEN ASSIGN v-program = "oe/rep/ackfibrex.p" is-xprint-form = YES lines-per-page = 69.
       WHEN "Albert" THEN ASSIGN v-program = "oe/rep/ackalbert.p" is-xprint-form = NO.
       WHEN "ContSvc" THEN ASSIGN v-program = "oe/rep/ackcontsvc.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Triad" THEN ASSIGN v-program = "oe/rep/acktriad.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Packrite" THEN ASSIGN v-program = "oe/rep/ackpkrit.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Dee" THEN ASSIGN v-program = "oe/rep/ackdee.p" is-xprint-form = YES lines-per-page = 69.
       WHEN "Allwest" THEN ASSIGN v-program = "oe/rep/ackallws.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Simkins" THEN ASSIGN v-program = "oe/rep/acksimkn.p" is-xprint-form = YES lines-per-page = 65.
       OTHERWISE ASSIGN v-program = "oe/rep/ackasi.p" is-xprint-form = NO lines-per-page = 55.

   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetShipToToggle C-Win 
PROCEDURE SetShipToToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN tb_sch-rel tb_act-rel.

        IF LOOKUP(v-print-fmt,"ContSvc,Soule,SouleUOM") = 0 THEN DO:
            IF tb_sch-rel OR tb_act-rel THEN
                tb_ship-to:SENSITIVE = YES.
            ELSE 
                ASSIGN
                    tb_ship-to:SENSITIVE = NO
                    tb_ship-to = NO.
                    tb_ship-to:SCREEN-VALUE = "NO".
        END. /*specific format check*/

        IF LOOKUP(v-print-fmt,"PremierX") <> 0 THEN DO:
            IF tb_sch-rel OR tb_act-rel THEN
                tb_shpnot:SENSITIVE = YES.
            IF tb_sch-rel EQ NO AND tb_act-rel EQ NO THEN
                ASSIGN tb_shpnot = NO
                tb_shpnot:SENSITIVE = NO
                tb_shpnot:SCREEN-VALUE = "NO".
        END. /*specific format check*/
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
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RUN_format-value-changed C-Win 
PROCEDURE RUN_format-value-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:

      RUN SetOEAckForm(v-print-fmt).
      viDefaultLinesPerPage = lines-per-page.
      IF LOOKUP(v-print-fmt,"Century,Fibrex,Allwest") = 0 THEN 
      ASSIGN
       tb_prt-bom = NO
       tb_prt-bom:SENSITIVE = NO
       tb_prt-bom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

    IF v-print-fmt EQ "Allwest" THEN ASSIGN TG_cons_form:SENSITIVE  = YES.

    IF v-print-fmt EQ "Indiana" THEN
      ASSIGN
       tb_sch-rel = NO
       tb_sch-rel:SENSITIVE = NO
       tb_sch-rel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
       tb_act-rel = NO
       tb_act-rel:SENSITIVE = NO
       tb_act-rel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

    IF v-print-fmt = "Dee" THEN
      ASSIGN
         TG_whs-mths:SENSITIVE = YES. 

    IF v-print-fmt EQ "Albert" THEN
       ASSIGN
         tb_prt-revise:SENSITIVE = NO
         tb_prt-revise = NO
         tb_prt-revise:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".
    /* gdm - 11091105*/
    ASSIGN tb_prt-revise:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".

/*     /* task 05121402 */                           */
/*     IF v-print-fmt EQ "Badger" AND v-fmt-int EQ 1 */
/*       THEN ASSIGN                                 */
/*              TG_print-pdf-view:HIDDEN     = NO    */
/*              TG_print-pdf-view:SENSITIVE  = YES.  */
/*     ELSE                                          */
/*         ASSIGN                                    */
/*              TG_print-pdf-view:HIDDEN     = YES   */
/*              TG_print-pdf-view:SENSITIVE  = NO.   */


    /* gdm - 04160907*/
    IF v-print-fmt NE "Simkins" 
      THEN ASSIGN 
             tb_terms:HIDDEN       = YES
             lv-termFile:HIDDEN    = YES
             tb_terms:SENSITIVE    = NO
             lv-termFile:SENSITIVE = NO
             TG_print-pen-notes:HIDDEN     = YES
             TG_print-pen-notes:SENSITIVE  = YES.

    IF v-print-fmt EQ "PremierX" OR v-print-fmt = "ACPI" OR v-print-fmt EQ "PremierCX"
         THEN ASSIGN
           tb_itempo:HIDDEN = NO
           tb_hide_sell:HIDDEN = NO. 
        ELSE ASSIGN 
           tb_itempo:HIDDEN = YES
           tb_hide_sell:HIDDEN = YES .

    IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierCX" THEN
        ASSIGN
        tb_itm-tot:HIDDEN = NO . 
    ELSE
        tb_itm-tot:HIDDEN = YES  . 


    IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "PremierCX"
         THEN ASSIGN
           tb_untcnt:HIDDEN = NO .
        ELSE ASSIGN 
           tb_untcnt:HIDDEN = YES .

    IF v-print-fmt EQ "PremierX"
            THEN ASSIGN
            tb_shpnot:HIDDEN = NO .
        ELSE ASSIGN 
            tb_shpnot:HIDDEN = YES . 

    IF v-print-fmt EQ "Badger"
         THEN ASSIGN
           TG_print-due-cd:HIDDEN = NO. 
        ELSE ASSIGN 
           TG_print-due-cd:HIDDEN = YES .
/*                                  */
/*     IF v-print-fmt EQ "ACPI"     */
/*          THEN ASSIGN             */
/*             tb_itempo:HIDDEN = NO. */

    IF v-print-fmt EQ "Soule" OR v-print-fmt EQ "SouleUOM"
      THEN ASSIGN 
             tb_print-component:HIDDEN       = NO .
        ELSE
             tb_print-component:HIDDEN       = YES .

    IF v-print-fmt EQ "Axis"
      THEN ASSIGN 
             tb_billnotes:HIDDEN       = NO .
        ELSE
             tb_billnotes:HIDDEN       = YES .

    IF v-print-fmt EQ "Simkins" THEN DO:      

      ASSIGN           
          lv-termFile =  lv-termFile:SCREEN-VALUE
          lv-termFile:SCREEN-VALUE = lv-termFile.

      IF TRIM(lv-termFile) EQ "" THEN DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
          WHERE sys-ctrl-shipto.company      EQ cocode
            AND sys-ctrl-shipto.NAME         EQ "ACKHEAD" 
            AND sys-ctrl-shipto.cust-vend    EQ YES 
            AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
        IF AVAIL sys-ctrl-shipto 
          THEN ASSIGN lv-termFile = sys-ctrl-shipto.char-fld.
      END.

      ASSIGN 
        tb_terms                 = NO
        tb_terms:SENSITIVE       = YES.

      IF TRIM(cocode) EQ "011"  OR
         TRIM(cocode) EQ "11" /* Landrum */
       THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms11.txt".
       ELSE 
        IF TRIM(cocode) EQ "012" OR
           TRIM(cocode) EQ "12" /* Marietta */
         THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms12.txt".
         ELSE 
          IF TRIM(cocode) EQ "060" OR
             TRIM(cocode) EQ "60" /* Harvard Folding Box */
            THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms60.txt".
            ELSE 
                IF TRIM(cocode) EQ "062" OR
                   TRIM(cocode) EQ "62" /* Ideal */
                  THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms62.txt".
                  ELSE ASSIGN lv-termFile:SCREEN-VALUE = lv-termFile.


      ASSIGN lv-termFile:SCREEN-VALUE = lv-termFile
             lv-termFile:SENSITIVE = NO.

      IF tb_terms:SCREEN-VALUE EQ "YES" 
        THEN lv-termFile:SENSITIVE = YES.

    END.
    ELSE ASSIGN
          tb_terms:SENSITIVE       = NO
          lv-termFile:SCREEN-VALUE = ""
          lv-termFile              = ""
          lv-termFile:SENSITIVE    = NO.


    IF LOOKUP(v-print-fmt,"Soule,SouleUOM,ContSvc") = 0 THEN DO:
        IF tb_sch-rel:SCREEN-VALUE EQ "NO" AND
            tb_act-rel:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                tb_ship-to:SENSITIVE = NO
                tb_ship-to:SCREEN-VALUE = "NO".
    END.
    IF LOOKUP(v-print-fmt,"PremierX") <> 0 THEN DO:
        IF tb_sch-rel:SCREEN-VALUE EQ "NO" AND
            tb_act-rel:SCREEN-VALUE EQ "NO" THEN
            ASSIGN
                tb_shpnot:SENSITIVE = NO
                tb_shpnot:SCREEN-VALUE = "NO".
    END.
    IF v-print-fmt EQ "Accord" THEN DO:
         ASSIGN 
             TG_print-pen-notes:HIDDEN     = NO
             TG_print-pen-notes:SENSITIVE  = YES.

            IF tb_inst:SCREEN-VALUE EQ "Yes" THEN
             spec-code:HIDDEN IN FRAME FRAME-A = NO .
            ELSE
             spec-code:HIDDEN IN FRAME FRAME-A = YES .

             IF TG_print-pen-notes:SCREEN-VALUE EQ "Yes" THEN
                dept-code:HIDDEN IN FRAME FRAME-A = NO .
            ELSE
                dept-code:HIDDEN IN FRAME FRAME-A = YES .
     END.
     ELSE
         ASSIGN spec-code:HIDDEN IN FRAME FRAME-A = YES 
                    dept-code:HIDDEN IN FRAME FRAME-A = YES .
       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

