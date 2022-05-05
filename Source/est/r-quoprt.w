&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-quoprt.w

  Description: Quote Printing

  Author: JLF

  Created: 09/20/02

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
DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

{est/printquo.i NEW}

DEFINE TEMP-TABLE tt-quote 
    FIELD row-id  AS ROWID
    FIELD tt-seq  AS INTEGER   INIT 999999999
    FIELD cust-no AS CHARACTER
    INDEX row-id row-id
    INDEX tt-seq tt-seq.

DEFINE TEMP-TABLE tt-quote2 
    FIELD row-id  AS ROWID
    FIELD tt-seq  AS INTEGER   INIT 999999999
    FIELD cust-no AS CHARACTER
    INDEX row-id row-id
    INDEX tt-seq tt-seq.

DEFINE BUFFER b-tt-quote FOR tt-quote.

DEFINE VARIABLE v-program            AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form       AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file          AS cha       NO-UNDO.
DEFINE VARIABLE lv-pdf-file          AS cha       NO-UNDO.
DEFINE VARIABLE v-tmp-lines-per-page AS INTEGER   NO-UNDO.
DEFINE VARIABLE vlSkipRec            AS LOG       NO-UNDO.
DEFINE VARIABLE vcDefaultForm        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-termPath          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE hdOutboundProcs      AS HANDLE    NO-UNDO.

{custom/xprint.i}

{methods/prgsecur.i}
{api/ttAPIOutboundEvent.i}

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr  AS INTEGER
    FIELD tt-FileName AS CHARACTER
    INDEX filelist IS PRIMARY TT-FILECTR.

DEFINE NEW SHARED VARIABLE v-print-item        AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-print-desc1       AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fg-desc2          AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fg-desc3          AS LOG       NO-UNDO.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection   AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter    AS CHARACTER NO-UNDO.

DEFINE            VARIABLE retcode             AS INTEGER   NO-UNDO.
DEFINE            VARIABLE cRtnChar            AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lRecFound           AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lBussFormModle      AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE dQuoteValue         AS DECIMAL   NO-UNDO .
DEFINE            VARIABLE cCheckLeftMarFormat AS CHARACTER INITIAL "QuoPrintVAL,quoprint 1,quoprint 2,quoprint 10,McElroy,quoprint 20,xprint,quoprint 11,GC,quoprint10-CAN,QuoPrint-Excel-Mex,Onducorr" NO-UNDO .

DEFINE            VARIABLE lAsiUser            AS LOGICAL   NO-UNDO .
DEFINE            VARIABLE hPgmSecurity        AS HANDLE    NO-UNDO.
DEFINE            VARIABLE lResult             AS LOG       NO-UNDO.
RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
RUN epCanAccess IN hPgmSecurity ("est/r-quoprt.w","", OUTPUT lResult).
DELETE OBJECT hPgmSecurity.

IF lResult THEN ASSIGN lAsiUser = YES .

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.

DEFINE BUFFER b1-cust     FOR cust.
DEFINE BUFFER b-quotehd   FOR quotehd.
DEFINE BUFFER b-est       FOR est.
DEFINE BUFFER b-quotehd-2 FOR quotehd.

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLQuote &Company=cocode} /* rstark 05181205 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 v-quo-list begin_cust end_cust ~
begin_quo# end_quo# tb_booked tb_inst tb_notesSpanPage begin_dept end_dept ~
rd_sort rs_note tb_note tb_prt-box tb_fg-desc2 tb_prt-desc1 ~
tb_boardDescription tb_comm tb_prt-comp tb_prt-quoimage tb_fg-desc3 ~
tb_print-2nd-dscr tb_prt-item tb_terms tb_prt-shp2 rs-act-inact lv-termFile ~
tb_BatchMail tb_HideDialog tb_page rd-dest run_format tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS v-quo-list begin_cust end_cust begin_quo# ~
end_quo# tb_booked tb_inst tb_notesSpanPage begin_dept end_dept lbl_sort-3 ~
rd_sort rs_note tb_note tb_prt-box tb_fg-desc2 tb_prt-desc1 ~
tb_boardDescription tb_comm tb_prt-comp tb_prt-quoimage tb_fg-desc3 ~
tb_print-2nd-dscr tb_prt-item tb_terms tb_prt-shp2 rs-act-inact lv-termFile ~
tb_BatchMail tb_HideDialog tb_page rd-dest run_format tbAutoClose ~
lbl_Item-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE VARIABLE v-quo-list          AS CHARACTER 
    VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
    SIZE 82 BY 3.1 NO-UNDO.

DEFINE VARIABLE begin_cust          AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_dept          AS CHARACTER FORMAT "X(2)" 
    LABEL "Beginning Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_quo#          AS INTEGER   FORMAT ">>>>>>>>" INITIAL 0 
    LABEL "Beginning Quote#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust            AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_dept            AS CHARACTER FORMAT "X(2)" INITIAL "zz" 
    LABEL "Ending Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_quo#            AS INTEGER   FORMAT ">>>>>>>>" INITIAL 99999999 
    LABEL "Ending Quote#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE lbl_Item-status     AS CHARACTER FORMAT "X(256)":U INITIAL "Item Status:" 
    LABEL "" 
    VIEW-AS TEXT 
    SIZE 13.4 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort-3          AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page      AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name        AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no          AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-termFile         AS CHARACTER FORMAT "X(256)":U 
    LABEL "Terms File" 
    VIEW-AS FILL-IN 
    SIZE 42.6 BY 1 NO-UNDO.

DEFINE VARIABLE run_format          AS CHARACTER FORMAT "X(30)":U 
    LABEL "Format" 
    VIEW-AS FILL-IN 
    SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE v-group-title       AS CHARACTER FORMAT "X(8)" INITIAL "EMAIL" 
    LABEL "Send to Title" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt             AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest             AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To File", 3
    SIZE 17 BY 5.76 NO-UNDO.

DEFINE VARIABLE rd_sort             AS CHARACTER INITIAL "Quote#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Estimate#", "Estimate#",
    "Cust Part#", "Cust Part#",
    "Quote#", "Quote#",
    "As Entered", "As Entered"
    SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE rs-act-inact        AS CHARACTER INITIAL "Both" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Active", "Active",
    "Inactive", "Inactive",
    "Both", "Both"
    SIZE 36.6 BY .95 NO-UNDO.

DEFINE VARIABLE rs_note             AS CHARACTER INITIAL "Corr" 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "Corr Note", "Corr",
    "Fold Note", "Fold",
    "Corr/Fold Note", "Corr/Fold",
    "No Note", "NoNote"
    SIZE 19 BY 3.43 NO-UNDO.

DEFINE VARIABLE tb_boardDescription AS CHARACTER INITIAL "Est" 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "Estimate Board Description", "Est",
    "Quote  Board Description", "Quote"
    SIZE 30 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 109 BY 6.48.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 109 BY 15.95.

DEFINE VARIABLE tbAutoClose       AS LOGICAL INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_BatchMail      AS LOGICAL INITIAL NO 
    LABEL "&Batch E-Mail" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_booked         AS LOGICAL INITIAL NO 
    LABEL "Booked Orders Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_comm           AS LOGICAL INITIAL NO 
    LABEL "Print SalesRep Commission?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fg-desc2       AS LOGICAL INITIAL YES 
    LABEL "Print FG Description 2?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_fg-desc3       AS LOGICAL INITIAL YES 
    LABEL "Print FG Description 3?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_HideDialog     AS LOGICAL INITIAL NO 
    LABEL "&Hide Dialog-Box" 
    VIEW-AS TOGGLE-BOX
    SIZE 19.4 BY .81 NO-UNDO.

DEFINE VARIABLE tb_inst           AS LOGICAL INITIAL NO 
    LABEL "Print Department Manufacturing Instructions?" 
    VIEW-AS TOGGLE-BOX
    SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tb_note           AS LOGICAL INITIAL YES 
    LABEL "Print Notes per Item or Form?" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE tb_notesSpanPage  AS LOGICAL INITIAL NO 
    LABEL "Instructions Span Multiple Pages" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE tb_page           AS LOGICAL INITIAL NO 
    LABEL "Separate Page Each Quote #" 
    VIEW-AS TOGGLE-BOX
    SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tb_print-2nd-dscr AS LOGICAL INITIAL NO 
    LABEL "Print 2nd Item Description Line?" 
    VIEW-AS TOGGLE-BOX
    SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-box        AS LOGICAL INITIAL NO 
    LABEL "Print Box Design?" 
    VIEW-AS TOGGLE-BOX
    SIZE 20.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_prt-comp       AS LOGICAL INITIAL YES 
    LABEL "Print Components?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-desc1      AS LOGICAL INITIAL YES 
    LABEL "Print FG Description 1?" 
    VIEW-AS TOGGLE-BOX
    SIZE 25.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-item       AS LOGICAL INITIAL NO 
    LABEL "Est/FG Item Name?" 
    VIEW-AS TOGGLE-BOX
    SIZE 23.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-quoimage   AS LOGICAL INITIAL YES 
    LABEL "Print Quote Image?" 
    VIEW-AS TOGGLE-BOX
    SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-shp2       AS LOGICAL INITIAL NO 
    LABEL "Print Ship To?" 
    VIEW-AS TOGGLE-BOX
    SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE tb_terms          AS LOGICAL INITIAL NO 
    LABEL "Print Terms?" 
    VIEW-AS TOGGLE-BOX
    SIZE 17.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm      AS LOGICAL INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    v-quo-list AT ROW 3.05 COL 15.6 NO-LABELS
    begin_cust AT ROW 6.38 COL 35.6 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 6.38 COL 78.6 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_quo# AT ROW 7.33 COL 35.6 COLON-ALIGNED HELP
    "Enter Beginning Quote Number"
    end_quo# AT ROW 7.33 COL 78.6 COLON-ALIGNED HELP
    "Enter Ending QuoteNumber"
    tb_booked AT ROW 8.38 COL 37.6 HELP
    "Check to only print quotes with booked orders" WIDGET-ID 22
    tb_inst AT ROW 9.52 COL 16.4
    tb_notesSpanPage AT ROW 9.52 COL 63.4
    begin_dept AT ROW 10.48 COL 35.6 COLON-ALIGNED HELP
    "Enter Beginning Department"
    end_dept AT ROW 10.48 COL 78.6 COLON-ALIGNED HELP
    "Enter Endng Department"
    lbl_sort-3 AT ROW 11.76 COL 17.8 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 11.76 COL 29.8 NO-LABELS
    rs_note AT ROW 12.48 COL 91 NO-LABELS WIDGET-ID 2
    tb_note AT ROW 13 COL 35.2 RIGHT-ALIGNED
    tb_prt-box AT ROW 13.05 COL 38.4
    tb_fg-desc2 AT ROW 13.05 COL 60
    tb_prt-desc1 AT ROW 13.05 COL 61.2
    tb_boardDescription AT ROW 13.1 COL 61.2 NO-LABELS
    tb_comm AT ROW 13.91 COL 35.2 RIGHT-ALIGNED
    tb_prt-comp AT ROW 13.91 COL 38.4
    tb_prt-quoimage AT ROW 13.95 COL 60
    tb_fg-desc3 AT ROW 14.76 COL 61.2
    tb_print-2nd-dscr AT ROW 14.81 COL 37 RIGHT-ALIGNED
    tb_prt-item AT ROW 14.81 COL 38.4
    tb_terms AT ROW 15.1 COL 38.4 WIDGET-ID 20
    tb_prt-shp2 AT ROW 15.67 COL 4.2 WIDGET-ID 6
    rs-act-inact AT ROW 15.86 COL 17.6 NO-LABELS WIDGET-ID 24
    lv-termFile AT ROW 15.86 COL 65 COLON-ALIGNED WIDGET-ID 18
    tb_BatchMail AT ROW 18.38 COL 30
    tb_HideDialog AT ROW 18.38 COL 50.6
    tb_page AT ROW 18.38 COL 71.6
    rd-dest AT ROW 18.57 COL 5 NO-LABELS
    lv-font-name AT ROW 19.24 COL 46 COLON-ALIGNED NO-LABELS
    lv-font-no AT ROW 19.33 COL 38 COLON-ALIGNED
    lines-per-page AT ROW 20.29 COL 104 COLON-ALIGNED
    lv-ornt AT ROW 20.52 COL 60 NO-LABELS
    td-show-parm AT ROW 20.76 COL 37
    v-group-title AT ROW 21.71 COL 41 COLON-ALIGNED HELP
    "Enter Email Title"
    run_format AT ROW 23.19 COL 79.8 COLON-ALIGNED WIDGET-ID 12
    tbAutoClose AT ROW 24.62 COL 31.2 WIDGET-ID 64
    btn-ok AT ROW 25.48 COL 31
    btn-cancel AT ROW 25.48 COL 65
    lbl_Item-status AT ROW 14.91 COL 2.6 COLON-ALIGNED WIDGET-ID 124
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 17.67 COL 4
    " Enter Quotes separated by comma" VIEW-AS TEXT
    SIZE 36 BY .62 AT ROW 2.19 COL 36.6
    BGCOLOR 14 
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    RECT-6 AT ROW 18.1 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 113.2 BY 26.81
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
        TITLE              = "Print Quotes"
        HEIGHT             = 25.86
        WIDTH              = 112.8
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
   FRAME-NAME                                                           */
ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_quo#:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_quo#:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_Item-status IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_Item-status:PRIVATE-DATA IN FRAME FRAME-A = "rs-act-inact".

/* SETTINGS FOR FILL-IN lbl_sort-3 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort-3:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lines-per-page:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-name:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-font-no:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lv-ornt:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_booked:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_comm IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_comm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fg-desc2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fg-desc3:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_HideDialog:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_inst:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_note IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_note:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_print-2nd-dscr IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_print-2nd-dscr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prt-desc1:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prt-item:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_terms:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN v-group-title IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    v-group-title:HIDDEN IN FRAME FRAME-A       = TRUE
    v-group-title:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    v-quo-list:RETURN-INSERTED IN FRAME FRAME-A = TRUE.

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
ON END-ERROR OF C-Win /* Print Quotes */
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
ON WINDOW-CLOSE OF C-Win /* Print Quotes */
    DO:
        /* This event will close the window and terminate the procedure.  */
        IF VALID-HANDLE (hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
            
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo# C-Win
ON LEAVE OF begin_quo# IN FRAME FRAME-A /* Beginning Quote# */
    DO:
        ASSIGN {&self-name}.  
        RUN activateBookedOrderOption.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_quo# C-Win
ON VALUE-CHANGED OF begin_quo# IN FRAME FRAME-A /* Beginning Quote# */
    DO:
        DO WITH FRAME {&FRAME-NAME}:

            IF rs_note:HIDDEN EQ NO AND
                INT(begin_quo#:SCREEN-VALUE) NE begin_quo# AND
                v-quo-list:SCREEN-VALUE EQ "" THEN
            DO:
                FIND FIRST b-quotehd-2 WHERE
                    b-quotehd-2.company EQ cocode AND
                    b-quotehd-2.loc     EQ locode AND
                    b-quotehd-2.q-no EQ INT(begin_quo#:SCREEN-VALUE)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE b-quotehd-2 THEN
                    FIND FIRST b-est WHERE
                        b-est.company EQ b-quotehd-2.company AND
                        b-est.est-no  EQ b-quotehd-2.est-no
                        NO-LOCK NO-ERROR.

                IF AVAILABLE b-est THEN
                DO:
                    IF b-est.est-type LE 4 THEN rs_note:SCREEN-VALUE = "Fold".
                    ELSE
                        rs_note:SCREEN-VALUE = "Corr".
                END.
            END.
        END.
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

        DEFINE VARIABLE lv-loc    LIKE quotehd.loc NO-UNDO.
        DEFINE VARIABLE li        AS INTEGER NO-UNDO.
        DEFINE VARIABLE lv-quo-no LIKE quotehd.q-no NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.          
        END.

        RUN SetGlobalVariables.

        lXMLOutput = rd-dest EQ iXMLOutput. /* rstark 05181205 */
        IF rd-dest = 1  THEN
            ASSIGN LvOutputSelection = "Printer".
        ELSE IF rd-dest = 2  THEN
                ASSIGN LvOutputSelection = "Screen". 
            ELSE IF rd-dest = 3  THEN
                    ASSIGN LvOutputSelection = "File". 
                ELSE IF rd-dest = 4  THEN
                        ASSIGN LvOutputSelection = "Fax". 
                    ELSE IF rd-dest = 5  THEN
                            ASSIGN LvOutputSelection = "Email".
                        ELSE IF rd-dest = 6  THEN
                                ASSIGN LvOutputSelection = "Port".

        IF tb_page:CHECKED AND
            (v-print-fmt EQ "CSC-EXCEL"     OR
            v-print-fmt EQ "PREMIER-EXCEL" OR
            v-print-fmt EQ "QuoPrint-Excel-Mex" OR
            v-print-fmt EQ "PREMIER-EXCEL-MCI" OR
            v-print-fmt EQ "BELL-EXCEL" OR
            v-print-fmt EQ "CCC-EXCEL" OR
            v-print-fmt EQ "TRILAKE-EXCEL" OR
            v-print-fmt EQ "FIBRE-EXCEL"   OR 
            v-print-fmt EQ "MSPACK-EXCEL"  OR /* gdm - 12030805 */ 
            v-print-fmt EQ "NOSCO-EXCEL") AND /* gdm - 11060808 */
            NOT CAN-FIND(FIRST sys-ctrl-shipto WHERE
            sys-ctrl-shipto.company = cocode AND
            sys-ctrl-shipto.NAME = "QUOPRINT") THEN
        DO:
            MESSAGE "'Separate Page Each Quote #' May not be Selected for Excel format."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
        END.

        IF rd-dest NE 5 THEN
        DO:
            ASSIGN
                fquote             = begin_quo#
                tquote             = end_quo#
                v-prt-box          = tb_prt-box
                s-sep-page         = tb_page
                s-prt-quoimage     = tb_prt-quoimage
                fcust              = begin_cust
                tcust              = end_cust
                ch-inst            = tb_inst
                fdept              = begin_dept
                tdept              = end_dept
                ch-note            = tb_note
                ch-sort            = SUBSTR(rd_sort,1,1)
                v-comm             = tb_comm
                ch-multi           = fquote NE tquote
                s-print-comp       = tb_prt-comp
                v-print-item       = tb_prt-item
                v-print-desc1      = tb_prt-desc1
                v-fg-desc2         = tb_fg-desc2
                v-fg-desc3         = tb_fg-desc3
                v-notesPageSpan    = tb_notesSpanPage
                v-boardDescription = tb_boardDescription
                s-print-2nd-dscr   = tb_print-2nd-dscr
                v-prt-shp2         = tb_prt-shp2
                v-terms            = tb_terms
                v-termfile         = lv-termFile.

            IF rs-act-inact:HIDDEN EQ NO THEN
                cItemStatus = SUBSTRING(rs-act-inact,1,1).

            IF rs_note:HIDDEN EQ NO THEN
                s-note-mode = rs_note.

            IF NOT lAsiUser AND CAN-FIND(FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "QUOPRINT") THEN
            DO:
                EMPTY TEMP-TABLE tt-quote.
                lv-loc = "".

                DO WHILE TRUE:

                    FIND FIRST quotehd
                        WHERE quotehd.company EQ cocode
                        AND quotehd.loc     GT lv-loc
                        USE-INDEX q-no NO-LOCK NO-ERROR.

                    IF NOT AVAILABLE quotehd THEN LEAVE.
                    lv-loc = quotehd.loc.
                    RELEASE quotehd.

                    FOR EACH quotehd WHERE
                        quotehd.company EQ cocode AND
                        quotehd.loc EQ lv-loc AND
                        quotehd.cust-no GE fcust AND
                        quotehd.cust-no LE tcust AND
                        quotehd.q-no    GE fquote AND
                        quotehd.q-no    LE tquote
                        NO-LOCK:

                        CREATE tt-quote.
                        ASSIGN
                            tt-quote.row-id  = ROWID(quotehd)
                            tt-quote.cust-no = quotehd.cust-no.
                    END.

                    DO li = 1 TO NUM-ENTRIES(v-quo-list):
                        RELEASE quotehd.
                        lv-quo-no = INT(ENTRY(li,v-quo-list)) NO-ERROR.
                        IF NOT ERROR-STATUS:ERROR AND lv-quo-no NE 0 THEN
                            FIND FIRST quotehd
                                WHERE quotehd.company EQ cocode
                                AND quotehd.loc     EQ lv-loc  
                                AND quotehd.q-no    EQ lv-quo-no
                                USE-INDEX q-no NO-LOCK NO-ERROR.

                        IF AVAILABLE quotehd THEN 
                        DO:
                            CREATE tt-quote.
                            ASSIGN
                                tt-quote.row-id  = ROWID(quotehd)
                                tt-quote.tt-seq  = li
                                tt-quote.cust-no = quotehd.cust-no.
                        END.
                    END.
                END. /*do while true*/

                FOR EACH b-tt-quote
                    BREAK BY b-tt-quote.row-id BY b-tt-quote.tt-seq:   
                    IF NOT FIRST-OF(b-tt-quote.row-id) THEN DELETE b-tt-quote.
                END.

                FOR EACH b-tt-quote
                    BREAK BY b-tt-quote.cust-no:

                    IF FIRST-OF (b-tt-quote.cust-no) THEN 
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company      = cocode AND
                            sys-ctrl-shipto.NAME         = "QUOPRINT" AND
                            sys-ctrl-shipto.cust-vend    = YES AND
                            sys-ctrl-shipto.cust-vend-no = b-tt-quote.cust-no AND
                            sys-ctrl-shipto.char-fld > '' 
                            NO-LOCK NO-ERROR.

                        IF AVAILABLE sys-ctrl-shipto THEN
                        DO:
                            RUN SetQuoForm (sys-ctrl-shipto.char-fld).
                            v-print-fmt = sys-ctrl-shipto.char-fld .
                            dQuoteValue   = sys-ctrl-shipto.dec-fld.
                        END.
                        ELSE
                        DO:
                            RUN SetQuoForm (vcDefaultForm).
                            v-print-fmt = vcDefaultForm.
                        END.

                        RUN SetGlobalVariables.
                        RUN run-report-sys-ctrl-shipto(b-tt-quote.cust-no).
                        RUN GenerateReport(b-tt-quote.cust-no,b-tt-quote.cust-no).
                    END.
                END.

            END. /* if can-find sys-ctrl-shipto*/
            ELSE /* not can-find sys-ctrl-shipto*/
            DO:
                v-print-fmt = vcDefaultForm.
                RUN SetGlobalVariables.
                RUN run-report(INPUT NO, INPUT "").
                RUN GenerateReport(begin_cust, end_cust).
            END.
        END.

        IF rd-dest = 5 THEN 
        DO:

            IF tb_page:CHECKED THEN
            DO:
                MESSAGE "'Separate Page Each Quote #' not valid in Email Mode."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN NO-APPLY.
            END.

            IF NOT tb_BatchMail:CHECKED AND
                begin_cust <> end_cust THEN
            DO:
                MESSAGE "Beginning Customer and Ending Customer must be the same for E-Mail."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO end_cust IN FRAME {&FRAME-NAME}.
                RETURN NO-APPLY.
            END.

            RUN output-to-mail.

            RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

            SESSION:SET-WAIT-STATE ("").
        END.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_quo#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_quo# C-Win
ON LEAVE OF end_quo# IN FRAME FRAME-A /* Ending Quote# */
    DO:
        ASSIGN {&self-name}.
        RUN activateBookedOrderOption.
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-termFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-termFile C-Win
ON HELP OF lv-termFile IN FRAME FRAME-A /* Terms File */
    DO:
        DEFINE VARIABLE chFile AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok  AS LOG       NO-UNDO.

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
                lv-termFile              = chFile
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-act-inact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-act-inact C-Win
ON VALUE-CHANGED OF rs-act-inact IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs_note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs_note C-Win
ON VALUE-CHANGED OF rs_note IN FRAME FRAME-A
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME run_format
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON HELP OF run_format IN FRAME FRAME-A /* Format */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO .
    
        RUN windows/l-syschrL.w (gcompany,"QuoPrint",run_format:SCREEN-VALUE,OUTPUT char-val).
        IF char-val NE '' THEN
            run_format:SCREEN-VALUE = ENTRY(1,char-val).
        IF v-print-fmt NE run_format:SCREEN-VALUE THEN 
        DO:
            ASSIGN 
                v-print-fmt   = run_format:SCREEN-VALUE
                vcDefaultForm = v-print-fmt.
            RUN SetQuoForm (vcDefaultForm).
            RUN  pRunFormatValueChanged .
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL run_format C-Win
ON LEAVE OF run_format IN FRAME FRAME-A /* Format */
    DO:
        ASSIGN run_format.

        IF v-print-fmt NE run_format THEN 
        DO:
            ASSIGN 
                v-print-fmt   = run_format
                vcDefaultForm = v-print-fmt.
            RUN SetQuoForm (vcDefaultForm).
            RUN  pRunFormatValueChanged .
      
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_BatchMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_BatchMail C-Win
ON VALUE-CHANGED OF tb_BatchMail IN FRAME FRAME-A /* Batch E-Mail */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_boardDescription
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_boardDescription C-Win
ON VALUE-CHANGED OF tb_boardDescription IN FRAME FRAME-A
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_booked
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_booked C-Win
ON VALUE-CHANGED OF tb_booked IN FRAME FRAME-A /* Booked Orders Only? */
    DO:
        ASSIGN {&self-name}.   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_comm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_comm C-Win
ON VALUE-CHANGED OF tb_comm IN FRAME FRAME-A /* Print SalesRep Commission? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fg-desc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg-desc2 C-Win
ON VALUE-CHANGED OF tb_fg-desc2 IN FRAME FRAME-A /* Print FG Description 2? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fg-desc3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg-desc3 C-Win
ON VALUE-CHANGED OF tb_fg-desc3 IN FRAME FRAME-A /* Print FG Description 3? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_HideDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_HideDialog C-Win
ON VALUE-CHANGED OF tb_HideDialog IN FRAME FRAME-A /* Hide Dialog-Box */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inst C-Win
ON VALUE-CHANGED OF tb_inst IN FRAME FRAME-A /* Print Department Manufacturing Instructions? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_note
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_note C-Win
ON VALUE-CHANGED OF tb_note IN FRAME FRAME-A /* Print Notes per Item or Form? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_notesSpanPage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_notesSpanPage C-Win
ON VALUE-CHANGED OF tb_notesSpanPage IN FRAME FRAME-A /* Instructions Span Multiple Pages */
    DO:
        ASSIGN {&SELF-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_print-2nd-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_print-2nd-dscr C-Win
ON VALUE-CHANGED OF tb_print-2nd-dscr IN FRAME FRAME-A /* Print 2nd Item Description Line? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-desc1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-desc1 C-Win
ON VALUE-CHANGED OF tb_prt-desc1 IN FRAME FRAME-A /* Print FG Description 1? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-item C-Win
ON VALUE-CHANGED OF tb_prt-item IN FRAME FRAME-A /* Est/FG Item Name? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_prt-shp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prt-shp2 C-Win
ON VALUE-CHANGED OF tb_prt-shp2 IN FRAME FRAME-A /* Print Ship To? */
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
            THEN  ASSIGN lv-termFile:SENSITIVE = {&self-name} .  

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


&Scoped-define SELF-NAME v-group-title
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-group-title C-Win
ON HELP OF v-group-title IN FRAME FRAME-A /* Send to Title */
    DO:
        DEFINE VARIABLE v-title AS cha NO-UNDO.

        RUN windows/l-ttlcod.w (FOCUS:SCREEN-VALUE, OUTPUT v-title).
        IF v-title <> "" THEN SELF:SCREEN-VALUE = ENTRY(1,v-title).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-group-title C-Win
ON LEAVE OF v-group-title IN FRAME FRAME-A /* Send to Title */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-quo-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-quo-list C-Win
ON LEAVE OF v-quo-list IN FRAME FRAME-A
    DO:
        DEFINE VARIABLE v-first-quote-no AS INTEGER NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:

            IF rs_note:HIDDEN EQ NO AND
                v-quo-list:SCREEN-VALUE NE "" THEN
            DO:
                v-first-quote-no = INT(ENTRY(1,v-quo-list)) NO-ERROR.

                IF v-first-quote-no NE 0 THEN
                DO:
                    FIND FIRST b-quotehd-2 WHERE
                        b-quotehd-2.company EQ cocode AND
                        b-quotehd-2.loc EQ locode AND
                        b-quotehd-2.q-no EQ v-first-quote-no
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE b-quotehd-2 THEN
                        FIND FIRST b-est WHERE
                            b-est.company EQ b-quotehd-2.company AND
                            b-est.est-no  EQ b-quotehd-2.est-no
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE b-est THEN
                    DO:
                        IF b-est.est-type LE 4 THEN
                            rs_note:SCREEN-VALUE = "Fold".
                        ELSE
                            rs_note:SCREEN-VALUE = "Corr".
                    END.
                END.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-quo-list C-Win
ON VALUE-CHANGED OF v-quo-list IN FRAME FRAME-A
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

    FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAILABLE quotehd THEN
        FIND FIRST est
            WHERE est.company EQ quotehd.company
            AND est.est-no  EQ quotehd.est-no
            NO-LOCK NO-ERROR.


    /*IF NOT AVAIL est THEN RETURN.*/

    ASSIGN
        begin_cust = quotehd.cust-no
        end_cust   = begin_cust
        begin_quo# = quotehd.q-no
        end_quo#   = begin_quo#.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "QUOPRINT"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "QUOPRINT"
            sys-ctrl.descrip = "Print Quote Headers on Quote Form?".
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.
    END.
    ASSIGN
        v-print-fmt      = sys-ctrl.char-fld
        CallingParameter = sys-ctrl.char-fld
        v-log            = sys-ctrl.log-fld
        vcDefaultForm    = v-print-fmt 
        dQuoteValue      = sys-ctrl.dec-fld.

    FIND FIRST users WHERE
        users.user_id EQ USERID("NOSWEAT")
        NO-LOCK NO-ERROR.

    IF AVAILABLE users AND users.user_program[2] NE "" THEN
        init-dir = users.user_program[2].
    ELSE
        init-dir = "c:\tmp".

    RUN SetQuoForm(v-print-fmt).
  
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    
    RUN enable_UI.

    DO WITH FRAME {&frame-name}:
        {custom/usrprint.i} 
        tb_booked = NO.
        tb_booked:SCREEN-VALUE = "NO".
        tb_booked:SENSITIVE = NO.
        /*when printing from estimate*/
        ASSIGN 
            v-quo-list:SCREEN-VALUE = "".

        ASSIGN 
            begin_cust:SCREEN-VALUE = quotehd.cust-no
            end_cust:SCREEN-VALUE   = begin_cust
            begin_quo#:SCREEN-VALUE = STRING(quotehd.q-no)
            end_quo#:SCREEN-VALUE   = STRING(begin_quo#).

        lines-per-page:SCREEN-VALUE = STRING(v-tmp-lines-per-page).
        DISABLE lines-per-page.
    
        RUN pRunFormatValueChanged .

        APPLY "entry" TO v-quo-list.
    END.

    {methods/nowait.i}
    APPLY "entry" TO v-quo-list IN FRAME {&FRAME-NAME}.

    IF NOT lAsiUser THEN
        RUN_format:HIDDEN IN FRAME FRAME-A = YES .
    ELSE 
        RUN_format:SCREEN-VALUE IN FRAME FRAME-A = v-print-fmt .

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateBookedOrderOption C-Win 
PROCEDURE activateBookedOrderOption :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF begin_quo# NE end_quo# 
            THEN tb_booked:SENSITIVE = YES.
        ELSE 
        DO:
            tb_booked = NO.
            tb_booked:SCREEN-VALUE = "NO".
            tb_booked:SENSITIVE = NO.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BatchMail C-Win 
PROCEDURE BatchMail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icBegCustNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icEndCustNo  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lv-loc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-quo-no AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-count  AS INTEGER   NO-UNDO.

    IF v-quo-list EQ "" THEN
        FOR EACH b1-cust WHERE
            b1-cust.company = cocode AND
            b1-cust.cust-no GE icBegCustNo AND
            b1-cust.cust-no LE icEndCustNo
            NO-LOCK,
            FIRST b-quotehd WHERE
            b-quotehd.company EQ cocode AND
            b-quotehd.loc EQ locode AND
            b-quotehd.cust-no EQ b1-cust.cust-no AND
            b-quotehd.q-no GE begin_quo# AND
            b-quotehd.q-no LE end_quo#
            NO-LOCK:

            RUN batchmail-2-proc.
        END.
    ELSE
    DO:
        EMPTY TEMP-TABLE tt-quote2.
        lv-loc = "".

        DO WHILE TRUE:

            FIND FIRST quotehd
                WHERE quotehd.company EQ cocode
                AND quotehd.loc     GT lv-loc
                USE-INDEX q-no NO-LOCK NO-ERROR.

            IF NOT AVAILABLE quotehd THEN LEAVE.
            lv-loc = quotehd.loc.
            RELEASE quotehd.

            FOR EACH quotehd WHERE
                quotehd.company EQ cocode AND
                quotehd.loc EQ lv-loc AND
                quotehd.cust-no GE icBegCustNo AND
                quotehd.cust-no LE icEndCustNo AND
                quotehd.q-no    GE begin_quo# AND
                quotehd.q-no    LE end_quo#
                NO-LOCK:

                CREATE tt-quote2.
                ASSIGN
                    tt-quote2.row-id  = ROWID(quotehd)
                    tt-quote2.cust-no = quotehd.cust-no
                    tt-quote2.tt-seq  = li-count
                    li-count          = li-count + 1.
            END.

            DO li = 1 TO NUM-ENTRIES(v-quo-list):

                lv-quo-no = INT(ENTRY(li,v-quo-list)) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR AND lv-quo-no NE 0 THEN
                    FIND FIRST quotehd
                        WHERE quotehd.company EQ cocode
                        AND quotehd.loc     EQ lv-loc  
                        AND quotehd.q-no    EQ lv-quo-no
                        AND quotehd.cust-no GE icBegCustNo
                        AND quotehd.cust-no LE icEndCustNo
                        USE-INDEX q-no NO-LOCK NO-ERROR.

                IF AVAILABLE quotehd THEN 
                DO:
                    CREATE tt-quote2.
                    ASSIGN
                        tt-quote2.row-id  = ROWID(quotehd)
                        tt-quote2.tt-seq  = li-count
                        tt-quote2.cust-no = quotehd.cust-no
                        li-count          = li-count + 1.
                END.

            END.
        END.

        FOR EACH tt-quote2
            BREAK BY tt-quote2.row-id BY tt-quote2.tt-seq:

            IF NOT FIRST-OF(tt-quote2.row-id) THEN DELETE tt-quote2.
        END.

        FOR EACH b1-cust WHERE
            b1-cust.company = cocode AND
            b1-cust.cust-no GE icBegCustNo AND
            b1-cust.cust-no LE icEndCustNo
            NO-LOCK,
            FIRST tt-quote2 WHERE
            tt-quote2.cust-no EQ b1-cust.cust-no,
            FIRST b-quotehd WHERE
            b-quotehd.company EQ cocode AND
            b-quotehd.loc EQ locode AND
            ROWID(b-quotehd) EQ tt-quote2.row-id
            NO-LOCK
            BY tt-quote2.tt-seq:

            RUN batchmail-2-proc.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE batchmail-2-proc C-Win 
PROCEDURE batchmail-2-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF tb_BatchMail:CHECKED IN FRAME {&FRAME-NAME} THEN
    DO:
        vlSkipRec = YES.

        FOR EACH phone WHERE 
            phone.table_rec_key = b1-cust.rec_key
            NO-LOCK:

            IF CAN-FIND (FIRST emaildtl WHERE
                emaildtl.emailcod  BEGINS 'r-quoprt' AND
                emaildtl.table_rec_key  = phone.rec_key) THEN 
            DO:
                vlSkipRec = NO.
                LEAVE.
            END.
        END.

        IF vlSkipRec THEN NEXT.
    END.

    FIND FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company      = cocode AND
        sys-ctrl-shipto.NAME         = "QUOPRINT" AND
        sys-ctrl-shipto.cust-vend    = YES AND
        sys-ctrl-shipto.cust-vend-no = b1-cust.cust-no AND
        sys-ctrl-shipto.char-fld > '' 
        NO-LOCK NO-ERROR.

    IF AVAILABLE sys-ctrl-shipto THEN
    DO:
        RUN SetQuoForm (sys-ctrl-shipto.char-fld).
        v-print-fmt = sys-ctrl-shipto.char-fld.
    END.
    ELSE
    DO:
        RUN SetQuoForm (vcDefaultForm).
        v-print-fmt = vcDefaultForm.
    END.

    IF tb_page:CHECKED AND
        (v-print-fmt EQ "CSC-EXCEL" OR
        v-print-fmt EQ "PREMIER-EXCEL" OR
        v-print-fmt EQ "QuoPrint-Excel-Mex" OR
        v-print-fmt EQ "PREMIER-EXCEL-MCI" OR
        v-print-fmt EQ "BELL-EXCEL" OR
        v-print-fmt EQ "CCC-EXCEL" OR
        v-print-fmt EQ "TRILAKE-EXCEL" OR
        v-print-fmt EQ "FIBRE-EXCEL" OR
        v-print-fmt EQ "MSPACK-EXCEL" OR
        v-print-fmt EQ "NOSCO-EXCEL") AND
        NOT CAN-FIND(FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company = cocode AND
        sys-ctrl-shipto.NAME = "QUOPRINT") THEN
    DO:
        MESSAGE "'Separate Page Each Quote #' May not be Selected for Excel format."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
    END.

    IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" AND
        v-print-fmt <> "PREMIER-EXCEL" AND
        v-print-fmt <> "QuoPrint-Excel-Mex" AND
        v-print-fmt <> "PREMIER-EXCEL-MCI" AND
        v-print-fmt <> "BELL-EXCEL" AND
        v-print-fmt <> "CCC-EXCEL" AND
        v-print-fmt <> "FIBRE-EXCEL"  AND
        v-print-fmt <> "NOSCO-EXCEL" AND 
        v-print-fmt <> "MSPACK-EXCEL" THEN
        lv-pdf-file = init-dir + "\" + (IF v-print-fmt EQ "Century" THEN "CBXQuote"
        ELSE "QT") + STRING(b-quotehd.q-no).
    ELSE
        ASSIGN
            is-xprint-form = TRUE
            lv-pdf-file    = init-dir + "\quote".

    RUN run-report (INPUT YES, INPUT b1-cust.cust-no).

    STATUS DEFAULT 'Now processing CUST: ' + b1-cust.cust-no + '....'.

    RUN GenerateMail(INPUT b-quotehd.q-no).

    STATUS DEFAULT ''.
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
    DISPLAY v-quo-list begin_cust end_cust begin_quo# end_quo# tb_booked tb_inst 
        tb_notesSpanPage begin_dept end_dept lbl_sort-3 rd_sort rs_note 
        tb_note tb_prt-box tb_fg-desc2 tb_prt-desc1 tb_boardDescription 
        tb_comm tb_prt-comp tb_prt-quoimage tb_fg-desc3 tb_print-2nd-dscr 
        tb_prt-item tb_terms tb_prt-shp2 rs-act-inact lv-termFile tb_BatchMail 
        tb_HideDialog tb_page rd-dest run_format tbAutoClose lbl_Item-status 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 v-quo-list begin_cust end_cust begin_quo# end_quo# 
        tb_booked tb_inst tb_notesSpanPage begin_dept end_dept rd_sort rs_note 
        tb_note tb_prt-box tb_fg-desc2 tb_prt-desc1 tb_boardDescription 
        tb_comm tb_prt-comp tb_prt-quoimage tb_fg-desc3 tb_print-2nd-dscr 
        tb_prt-item tb_terms tb_prt-shp2 rs-act-inact lv-termFile tb_BatchMail 
        tb_HideDialog tb_page rd-dest run_format tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateMail C-Win 
PROCEDURE GenerateMail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-quote-no AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF is-xprint-form THEN 
        DO:

            IF NOT(v-print-fmt EQ "CSC-EXCEL" OR
                v-print-fmt EQ "PREMIER-EXCEL" OR
                v-print-fmt EQ "QuoPrint-Excel-Mex" OR
                v-print-fmt EQ "PREMIER-EXCEL-MCI" OR
                v-print-fmt EQ "BELL-EXCEL" OR
                v-print-fmt EQ "CCC-EXCEL" OR
                v-print-fmt EQ "TRILAKE-EXCEL" OR
                v-print-fmt EQ "FIBRE-EXCEL" OR
                v-print-fmt EQ "NOSCO-EXCEL" OR
                v-print-fmt EQ "MSPACK-EXCEL") THEN
                RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

            IF v-print-fmt EQ "PREMIER-EXCEL" OR v-print-fmt EQ "QuoPrint-Excel-Mex" THEN 
            DO:
                FIND FIRST tt-filelist NO-LOCK NO-ERROR .
                IF AVAILABLE tt-filelist THEN 
                DO:
                    ASSIGN 
                        lv-pdf-file = tt-filelist.tt-FileName .
                END.
                IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', lv-pdf-file , ip-quote-no).
                ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  lv-pdf-file ,ip-quote-no).
            END.
            ELSE 
            DO:
                IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', lv-pdf-file + ".pdf", ip-quote-no).
                ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  lv-pdf-file + ".pdf",ip-quote-no).
            END.
        END.

        ELSE 
        DO:

            IF tb_HideDialog:CHECKED THEN RUN SendMail-1 (b1-cust.cust-no, 'Customer1', list-name, ip-quote-no).
            ELSE RUN SendMail-1 (b1-cust.cust-no, 'Customer',  list-name, ip-quote-no).
        END.
    END.
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
    DEFINE INPUT PARAMETER ip-from-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-to-cust-no AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" AND
            v-print-fmt <> "PREMIER-EXCEL" AND
            v-print-fmt <> "QuoPrint-Excel-Mex" AND
            v-print-fmt <> "PREMIER-EXCEL-MCI" AND
            v-print-fmt <> "BELL-EXCEL" AND
            v-print-fmt <> "CCC-EXCEL" AND
            v-print-fmt <> "FIBRE-EXCEL" AND 
            v-print-fmt <> "NOSCO-EXCEL" AND 
            v-print-fmt <> "MSPACK-EXCEL" THEN
        DO:
            CASE rd-dest:
                WHEN 1 THEN RUN output-to-printer.
                WHEN 2 THEN RUN output-to-screen.
                WHEN 3 THEN RUN output-to-file.
                WHEN 4 THEN 
                    DO:
                        {custom/asifax3.i &begin_cust=ip-from-cust-no 
                                &END_cust=ip-to-cust-no
                                &fax-subject="Quote"
                                &fax-body="Quote"
                                &fax-file=list-name
                                &end-widget=end_cust }
                    END.
                WHEN 6 THEN RUN output-to-port.
            END CASE. 
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
        RUN BatchMail (begin_cust, IF NOT tb_BatchMail:CHECKED THEN begin_cust
        ELSE end_cust).
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
    IF is-xprint-form THEN 
    DO:
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
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 

/*ELSE run scr-rpt.w (list-name,c-win:title). /* open file-name, title */  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallAPIOutbound C-Win
PROCEDURE pCallAPIOutbound PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFormat    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeID   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    IF v-print-fmt EQ "quoprint 11" OR v-print-fmt EQ "quoprint 20" OR v-print-fmt EQ "quoprint 10" THEN DO:
        system.SharedConfig:Instance:SetValue("SendQuote_Print2ndItemDescription", STRING(tb_print-2nd-dscr:CHECKED)).
        system.SharedConfig:Instance:SetValue("SendQuote_PrintBoxDesign", STRING(tb_prt-box:CHECKED)).
        system.SharedConfig:Instance:SetValue("SendQuote_PrintSetComponents", STRING(tb_prt-comp:CHECKED)).
        
        RUN Outbound_PrepareAndExecuteForScopeAndClient IN hdOutboundProcs (
            INPUT  cocode,                                         /* Company Code (Mandatory) */
            INPUT  "",                                             /* Location Code (Mandatory) */
            INPUT  "SendQuote",                                    /* API ID (Mandatory) */
            INPUT  ipcFormat,                                      /* Client ID */
            INPUT  ipcScopeID,                                     /* Scope ID */
            INPUT  ipcScopeType,                                   /* Scope Type */
            INPUT  "PrintQuote",                                   /* Trigger ID (Mandatory) */
            INPUT  "TTQuote",                                      /* Comma separated list of table names for which data being sent (Mandatory) */
            INPUT  STRING(TEMP-TABLE tt-quote:HANDLE),             /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
            INPUT  "Quote Print",                                  /* Primary ID for which API is called for (Mandatory) */   
            INPUT  "Quote print",                                  /* Event's description (Optional) */
            OUTPUT lSuccess,                                       /* Success/Failure flag */
            OUTPUT cMessage                                        /* Status message */
            ).

        system.SharedConfig:Instance:DeleteValue("SendQuote_Print2ndItemDescription").
        system.SharedConfig:Instance:DeleteValue("SendQuote_PrintBoxDesign").
        system.SharedConfig:Instance:DeleteValue("SendQuote_PrintSetComponents").

        RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).
        
        lcRequestData = "".
        
        FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
        IF AVAILABLE ttAPIOutboundEvent THEN DO:
            FIND FIRST apiOutboundEvent NO-LOCK
                 WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
                 NO-ERROR.
            IF AVAILABLE apiOutboundEvent THEN
                lcRequestData = apiOutboundEvent.requestData.
        END.    
        
        IF lcRequestData NE "" THEN
            COPY-LOB FROM lcRequestData TO FILE list-name.
            
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

        IF NOT AVAILABLE est OR est.est-type LE 4 THEN DISABLE tb_note tb_comm.
        IF NOT AVAILABLE est OR est.est-type LE 4 OR 
            (v-print-fmt NE "XPrint" AND v-print-fmt NE "RFC" AND v-print-fmt NE "quoprint 1" AND v-print-fmt NE "quoprint 2" AND v-print-fmt NE "quoprint 10" AND v-print-fmt NE "McElroy" AND v-print-fmt NE "QuoPrintVAL" AND
            v-print-fmt NE "quoprint 11" AND v-print-fmt NE "GC" AND v-print-fmt NE "quoprint 20" AND v-print-fmt NE "Chattanooga"  AND v-print-fmt NE "Printers"  AND v-print-fmt NE "Hughes" AND v-print-fmt NE "Simkins" AND v-print-fmt NE "Oklahoma")
            THEN 
        DO:
            ASSIGN
                tb_boardDescription:SCREEN-VALUE = 'Est'
                tb_boardDescription              = 'Est'.
            HIDE tb_boardDescription NO-PAUSE.
        END.
        
        IF v-print-fmt NE "10 Pitch" THEN DISABLE tb_note.
        ELSE ENABLE tb_note.

        IF v-print-fmt NE "Brick" AND
            v-print-fmt NE "ASI" AND v-print-fmt NE "PACIFIC"
            THEN DISABLE tb_comm.
        ELSE ENABLE tb_comm.

        IF v-print-fmt NE "StClair" 
            THEN ASSIGN
                tb_prt-item:HIDDEN  = YES  
                tb_prt-desc1:HIDDEN = YES  
                tb_fg-desc2:HIDDEN  = YES
                tb_fg-desc3:HIDDEN  = YES.
        ELSE ASSIGN
                tb_print-2nd-dscr:HIDDEN = YES
                tb_print-2nd-dscr        = NO
                tb_prt-item:HIDDEN       = YES
                tb_prt-item              = NO.

        IF is-xprint-form = NO THEN DISABLE tb_prt-box.
        
        IF v-print-fmt = "Century" THEN tb_prt-quoimage:HIDDEN = NO.
        ELSE tb_prt-quoimage:HIDDEN = YES.

        IF LOOKUP(v-print-fmt,"Fibrex,Boss,Protagon,Fibre-Excel,Loylang,LoylangBSF") EQ 0 THEN rs_note:HIDDEN = YES.
        ELSE
            IF AVAILABLE est AND est.est-type LE 4 THEN rs_note:SCREEN-VALUE = "Fold".
            ELSE
                IF AVAILABLE est AND est.est-type GT 4 THEN rs_note:SCREEN-VALUE = "Corr".

        v-quo-list:SCREEN-VALUE = TRIM(v-quo-list:SCREEN-VALUE).

        /* gdm - 04300907 */
        IF v-print-fmt NE "Simkins" 
            THEN ASSIGN 
                tb_prt-shp2:HIDDEN    = YES
                tb_terms:HIDDEN       = YES
                lv-termFile:HIDDEN    = YES
                tb_terms:SENSITIVE    = NO
                lv-termFile:SENSITIVE = NO.
        ELSE ASSIGN 
                tb_prt-shp2:HIDDEN    = NO
                tb_terms:HIDDEN       = NO
                lv-termFile:HIDDEN    = NO
                tb_terms:SENSITIVE    = YES
                lv-termFile:SENSITIVE = YES.

        IF v-print-fmt EQ "Premier-Excel" OR v-print-fmt EQ "QuoPrint-Excel-Mex" OR v-print-fmt EQ "Premier-Excel-Mci" 
            OR v-print-fmt EQ "CCC-Excel" 
            /*OR v-print-fmt EQ "Bell-Excel"*/
            THEN 
            ASSIGN rd_sort:SENSITIVE = NO.
        ELSE rd_sort:SENSITIVE = YES.

        IF v-print-fmt EQ "Peachtree" OR v-print-fmt EQ "Altex" THEN
            ASSIGN rs-act-inact:HIDDEN    = NO
                rs-act-inact:SENSITIVE = YES
                lbl_Item-status:HIDDEN = NO.
        ELSE
            ASSIGN rs-act-inact:HIDDEN    = YES
                lbl_Item-status:HIDDEN = YES. 

        IF v-print-fmt EQ "Simkins" THEN 
        DO:
            ASSIGN 
                lv-termFile              = lv-termFile:SCREEN-VALUE
                lv-termFile:SCREEN-VALUE = lv-termFile.

            IF lv-termFile:SCREEN-VALUE EQ "" THEN 
            DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                    WHERE sys-ctrl-shipto.company      EQ cocode
                    AND sys-ctrl-shipto.NAME         EQ "QUOPRINT"
                    AND sys-ctrl-shipto.cust-vend    EQ YES
                    AND sys-ctrl-shipto.char-fld     NE '' NO-ERROR.
                IF AVAILABLE sys-ctrl-shipto
                    THEN ASSIGN lv-termFile = sys-ctrl-shipto.char-fld.
            END.

            /*                                                                              */
            /*       IF TRIM(lv-termFile) NE ""                                             */
            /*         THEN                                                                 */
            /*          lv-termPath = SUBSTR(TRIM(lv-termFile),1,R-INDEX(lv-termFile,"\")). */
            /*                                                                              */
            /*       IF TRIM(lv-termPath) EQ ""                                             */
            /*         THEN lv-termPath = "C:\".                                            */
            /*                                                */
            /*       ASSIGN                                   */
            /*         tb_terms                 = NO          */
            /*         tb_terms:SENSITIVE       = YES         */
            /*         lv-termFile:SCREEN-VALUE = lv-termFile */
            /*         tb_terms:SCREEN-VALUE    = "NO".       */


            IF (TRIM(cocode) EQ "011" OR 
                TRIM(cocode) EQ "11") /* Landrum */
                THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms11.txt".
            ELSE 
                IF (TRIM(cocode) EQ "012" OR 
                    TRIM(cocode) EQ "12") /* Marietta */
                    THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms12.txt".
                ELSE 
                    IF (TRIM(cocode) EQ "060" /* Harvard Folding Box */ OR
                        TRIM(cocode) EQ "60")  OR
                        (TRIM(cocode) EQ "062" /* Ideal */ OR
                        TRIM(cocode) EQ "62")
                        THEN ASSIGN lv-termFile:SCREEN-VALUE = lv-termPath + "QuoteTerms60.txt".
                    ELSE ASSIGN lv-termFile:SCREEN-VALUE = lv-termFile.

            ASSIGN 
                lv-termFile:SCREEN-VALUE = lv-termFile
                lv-termFile:SENSITIVE    = NO.      

            IF tb_terms:SCREEN-VALUE EQ "YES" 
                THEN lv-termFile:SENSITIVE = YES.        
        END.
        ELSE ASSIGN
                tb_terms:SENSITIVE       = NO
                lv-termFile:SCREEN-VALUE = ""
                lv-termFile              = ""
                lv-termFile:SENSITIVE    = NO.
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
    DEFINE INPUT PARAMETER ip-mail-mode AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lv-quo-no LIKE quotehd.q-no NO-UNDO.
    DEFINE VARIABLE lv-loc    LIKE quotehd.loc INIT "" NO-UNDO.
    DEFINE VARIABLE li        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lIsAPIActive    AS LOGICAL   NO-UNDO.
    
    {sys/form/r-top.i}
    {sys/inc/print1.i}

    ASSIGN
        fquote             = begin_quo#
        tquote             = end_quo#
        v-prt-box          = tb_prt-box
        s-sep-page         = tb_page
        s-prt-quoimage     = tb_prt-quoimage
        fcust              = IF ip-mail-mode AND tb_BatchMail:CHECKED IN FRAME {&frame-name} THEN
            icCustNo 
         ELSE begin_cust
        tcust              = IF ip-mail-mode AND tb_BatchMail:CHECKED IN FRAME {&frame-name} THEN
            icCustNo 
         ELSE end_cust
        ch-inst            = tb_inst
        fdept              = begin_dept
        tdept              = end_dept
        ch-note            = tb_note
        ch-sort            = SUBSTR(rd_sort,1,1)
        v-comm             = tb_comm
        ch-multi           = fquote NE tquote
        s-print-comp       = tb_prt-comp
        v-print-item       = tb_prt-item
        v-print-desc1      = tb_prt-desc1
        v-fg-desc2         = tb_fg-desc2
        v-fg-desc3         = tb_fg-desc3
        v-notesPageSpan    = tb_notesSpanPage
        v-boardDescription = tb_boardDescription
        s-print-2nd-dscr   = tb_print-2nd-dscr
        v-prt-shp2         = tb_prt-shp2.
    IF dQuoteValue EQ 0 THEN dQuoteValue = 4 .
    IF rs-act-inact:HIDDEN EQ NO THEN
        cItemStatus = SUBSTRING(rs-act-inact,1,1).

    IF rs_note:HIDDEN EQ NO THEN
        s-note-mode = rs_note.

    IF v-program EQ "cec/quote/quoasi.p" THEN 
    DO:  
        MESSAGE "Format is invalid or not found - Set the proper format in NK1 - "  v-print-fmt VIEW-AS ALERT-BOX INFORMATION.
        RETURN.
    END.
    
    RUN Outbound_IsApiClientAndScopeActive IN hdOutboundProcs (cocode, "", "SendQuote", v-print-fmt, "", "", "PrintQuote", OUTPUT lIsAPIActive).
    
    IF NOT lIsAPIActive THEN
        {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-quote:
        DELETE tt-quote.
    END.

    DO WHILE TRUE:  /* because loc is in header */
        FIND FIRST quotehd
            WHERE quotehd.company EQ cocode
            AND quotehd.loc     GT lv-loc
            USE-INDEX q-no NO-LOCK NO-ERROR.

        IF NOT AVAILABLE quotehd THEN LEAVE.

        lv-loc = quotehd.loc.

        RELEASE quotehd.

        IF fcust EQ tcust AND fquote NE tquote THEN
            FOR EACH quotehd
                WHERE quotehd.cust-no GE fcust
                AND quotehd.cust-no LE tcust
                AND quotehd.company EQ cocode
                AND quotehd.loc     EQ lv-loc
                AND quotehd.q-no    GE fquote
                AND quotehd.q-no    LE tquote
                /*USE-INDEX cust2 */
                NO-LOCK:
                CREATE tt-quote.
                ASSIGN
                    tt-quote.row-id  = ROWID(quotehd)
                    tt-quote.cust-no = quotehd.cust-no
                    .
            END.

        ELSE
            FOR EACH quotehd
                WHERE quotehd.company EQ cocode
                AND quotehd.loc     EQ lv-loc
                AND quotehd.cust-no GE fcust
                AND quotehd.cust-no LE tcust
                AND quotehd.q-no    GE fquote
                AND quotehd.q-no    LE tquote
                USE-INDEX q-no NO-LOCK:
                CREATE tt-quote.
                ASSIGN
                    tt-quote.row-id  = ROWID(quotehd)
                    tt-quote.cust-no = quotehd.cust-no
                    .
            END.                                                

        DO li = 1 TO NUM-ENTRIES(v-quo-list):
            RELEASE quotehd.
            lv-quo-no = INT(ENTRY(li,v-quo-list)) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR AND lv-quo-no NE 0 THEN
                FIND FIRST quotehd
                    WHERE quotehd.company EQ cocode
                    AND quotehd.loc     EQ lv-loc  
                    AND quotehd.q-no    EQ lv-quo-no
                    AND quotehd.cust-no GE fcust
                    AND quotehd.cust-no LE tcust
                    USE-INDEX q-no NO-LOCK NO-ERROR.

            IF AVAILABLE quotehd THEN 
            DO:
                CREATE tt-quote.
                ASSIGN
                    tt-quote.row-id  = ROWID(quotehd)
                    tt-quote.tt-seq  = li
                    tt-quote.cust-no = quotehd.cust-no.
                    .
            END.
        END.
    END.
    FOR EACH tt-quote BREAK BY tt-quote.row-id BY tt-seq:   
        IF NOT FIRST-OF(tt-quote.row-id) THEN DELETE tt-quote.
    END.

    FOR EACH tt-quote BREAK BY tt-quote.row-id:
        ch-multi = NOT (FIRST(tt-quote.row-id) AND LAST(tt-quote.row-id)).
        LEAVE.
    END.

    IF NOT ch-multi THEN
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            tb_note:SCREEN-VALUE = "YES"
            tb_note.
    END.

    FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAILABLE quotehd THEN
        FIND FIRST est WHERE est.company EQ quotehd.company
            AND est.est-no  EQ quotehd.est-no
            NO-LOCK NO-ERROR.

    {sa/sa-sls01.i}

    IF NOT ch-multi AND quotehd.est-no NE "" THEN
        FOR EACH tt-quote,

            FIRST quotehd
            WHERE ROWID(quotehd) EQ tt-quote.row-id
            AND (CAN-FIND(FIRST est
            WHERE est.company EQ quotehd.company
            AND est.est-no  EQ quotehd.est-no) OR
            lookup(v-print-fmt,"Century,Unipak,PPI,Packrite") > 0 )
            NO-LOCK,

            FIRST quoteitm OF quotehd NO-LOCK,

            FIRST sman
            WHERE sman.company EQ quotehd.company
            AND sman.sman    EQ quotehd.sman
            NO-LOCK,

            FIRST carrier
            WHERE carrier.company EQ quotehd.company
            AND carrier.carrier EQ quotehd.carrier
            NO-LOCK,

            FIRST terms
            WHERE terms.company EQ quotehd.company
            AND terms.t-code  EQ quotehd.terms
            NO-LOCK,

            FIRST cust
            WHERE cust.company EQ quotehd.company
            AND cust.cust-no EQ quotehd.cust-no
            NO-LOCK

            TRANSACTION:

            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = quotehd.cust-no
                report.key-02  = IF ch-sort EQ "E" THEN quotehd.est-no              ELSE
                    IF ch-sort EQ "C" THEN quoteitm.part-no            ELSE
                    IF ch-sort EQ "A" THEN STRING(tt-seq,"9999999999") ELSE ""
                report.key-03  = STRING(quotehd.q-no,"9999999999")
                report.rec-id  = RECID(quotehd).
        END.

    ELSE    
        FOR EACH tt-quote,
            FIRST quotehd WHERE ROWID(quotehd) EQ tt-quote.row-id NO-LOCK,
            FIRST quoteitm OF quotehd NO-LOCK,
            FIRST sman
            WHERE sman.company EQ quotehd.company
            AND sman.sman    EQ quotehd.sman
            NO-LOCK,

            FIRST carrier
            WHERE carrier.company EQ quotehd.company
            AND carrier.carrier EQ quotehd.carrier
            NO-LOCK,

            FIRST terms
            WHERE terms.company EQ quotehd.company
            AND terms.t-code  EQ quotehd.terms
            NO-LOCK,

            FIRST cust
            WHERE cust.company EQ quotehd.company
            AND cust.cust-no EQ quotehd.cust-no
            NO-LOCK
            TRANSACTION:
            IF quotehd.est-no <> "" AND lookup(v-print-fmt,"Century,Unipak,PPI,Packrite") <= 0 THEN 
            DO:
                FIND FIRST est WHERE est.company EQ quotehd.company
                    AND est.est-no  EQ quotehd.est-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE est THEN  NEXT.
            END.
            IF tb_booked THEN 
            DO:
                IF quotehd.est-no = "" THEN NEXT.
                ELSE 
                DO:
                    FIND FIRST est WHERE est.company = quotehd.company 
                        AND est.est-no EQ quotehd.est-no NO-LOCK NO-ERROR.
                    IF v-print-fmt EQ "midwest" THEN 
                    DO:
                        IF AVAILABLE est THEN 
                        DO:
                            FIND FIRST oe-ordl NO-LOCK
                                WHERE oe-ordl.company EQ cocode
                                AND oe-ordl.i-no EQ quoteitm.i-no 
                                NO-ERROR.
                            FIND FIRST itemfg NO-LOCK
                                WHERE itemfg.company EQ cocode
                                AND itemfg.i-no EQ quoteitm.i-no 
                                NO-ERROR .
                            /* This... */
                            IF NOT AVAILABLE itemfg
                                OR (AVAILABLE itemfg AND NOT itemfg.stocked)
                                OR NOT AVAILABLE oe-ordl THEN 
                            DO:
                                IF AVAILABLE est AND est.ord-no EQ 0 THEN NEXT. 
                            END.
                        /* Should be functionally equivalent to this: (which throws a compile warning) */
                        /*                IF AVAIL itemfg AND itemfg.stocked AND AVAIL oe-ordl THEN TRUE . */
                        /*                ELSE IF AVAIL est AND est.ord-no EQ 0 THEN NEXT.                 */
                        END.
                    END.
                    ELSE
                        IF AVAILABLE est AND est.ord-no EQ 0 THEN NEXT.
                END.
            END.
            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = quotehd.cust-no
                report.key-02  = IF ch-sort EQ "E" THEN quotehd.est-no              ELSE
                    IF ch-sort EQ "C" THEN quoteitm.part-no            ELSE
                    IF ch-sort EQ "A" THEN STRING(tt-seq,"9999999999") ELSE ""
                report.key-03  = STRING(quotehd.q-no,"9999999999")
                report.rec-id  = RECID(quotehd).
        END.

    ASSIGN
        v-term-id        = v-term
        v-lines-per-page = lines-per-page.

    /* Check for XL also */
    IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" OR v-print-fmt = "FIBRE-EXCEL" OR v-print-fmt = "NOSCO-EXCEL" 
        OR v-print-fmt = "MSPACK-EXCEL" OR v-print-fmt = "PREMIER-EXCEL" OR v-print-fmt = "QuoPrint-Excel-Mex" OR v-print-fmt = "PREMIER-EXCEL-MCI" 
        OR v-print-fmt = "CCC-EXCEL" OR v-print-fmt = "BELL-EXCEL" THEN.
    ELSE
        IF IS-xprint-form THEN 
        DO:
            CASE rd-dest:
                WHEN 1 THEN 
                    DO:
                        IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) GT 0  THEN
                            PUT  "<PRINTER?><LEFT=" + trim(STRING(dQuoteValue)) + "mm></PROGRESS>" FORMAT "x(50)".
                        ELSE PUT  "<PRINTER?></PROGRESS>".
                    END.
                WHEN 2 THEN 
                    DO:
                        IF NOT lBussFormModle THEN 
                        DO:
                            IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) GT 0  THEN
                                PUT "<PREVIEW><LEFT=" + trim(STRING(dQuoteValue)) + "mm><MODAL=NO></PROGRESS>" FORMAT "x(50)". 
                            ELSE PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORMAT "x(50)". 
                        END.
                        ELSE 
                        DO:
                            IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) GT 0  THEN
                                PUT "<PREVIEW><LEFT=" + trim(STRING(dQuoteValue)) + "mm></PROGRESS>" FORMAT "x(50)".     
                            ELSE PUT "<PREVIEW></PROGRESS>" FORMAT "x(50)". 
                        END.
                    END.

                WHEN  4 THEN 
                    DO:
                        ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                        PUT UNFORMATTED 
                            "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
                    END.
                WHEN 5 THEN 
                    DO:
                        IF LOOKUP(v-print-fmt,"century,unipak,PPI,Packrite,Simkins") > 0 THEN       
                            PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)" "</PROGRESS>".
                        ELSE IF LOOKUP(v-print-fmt,"QuoPrintVAL") > 0 THEN
                                PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><LEFT=" + trim(STRING(dQuoteValue)) +  "mm><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)" "</PROGRESS>".
                            ELSE IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) > 0 THEN
                                    PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><LEFT=" + trim(STRING(dQuoteValue)) +  "mm><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)" "</PROGRESS>".
                                ELSE PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)" "</PROGRESS>".
                    END.
            END CASE.
        END.
    
    RUN pCallAPIOutbound(v-print-fmt, "", "").
    
    IF NOT lIsAPIActive THEN
        RUN VALUE(v-program).
    
    FOR EACH report WHERE report.term-id EQ v-term-id:
        DELETE report.
    END.

    ASSIGN
        tb_note:SCREEN-VALUE = "YES"
        /*rd_sort:SCREEN-VALUE = "Quote#"*/          /*Task# 12121314*/
        tb_note
        rd_sort.

    IF NOT ip-mail-mode THEN
    DO:
        RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
        SESSION:SET-WAIT-STATE ("").
    END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-sys-ctrl-shipto C-Win 
PROCEDURE run-report-sys-ctrl-shipto :
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.

    {sys/form/r-top.i}

    ASSIGN
        fcust = icCustNo
        tcust = icCustNo.

    IF v-program EQ "cec/quote/quoasi.p" THEN 
    DO:  
        MESSAGE "Format is invalid or not found - Set the proper format in NK1 - "  v-print-fmt VIEW-AS ALERT-BOX INFORMATION.
        RETURN.
    END.

    IF dQuoteValue EQ 0 THEN dQuoteValue = 4 .

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-quote WHERE
        tt-quote.cust-no EQ icCustno
        BREAK BY tt-quote.row-id BY tt-seq:   
        IF NOT FIRST-OF(tt-quote.row-id) THEN DELETE tt-quote.
    END.

    FOR EACH tt-quote WHERE
        tt-quote.cust-no EQ icCustno
        BREAK BY tt-quote.row-id:
        ch-multi = NOT (FIRST(tt-quote.row-id) AND LAST(tt-quote.row-id)).
        LEAVE.
    END.

    IF NOT ch-multi THEN
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            tb_note:SCREEN-VALUE = "YES"
            tb_note.
    END.

    FIND FIRST quotehd WHERE ROWID(quotehd) EQ ip-rowid NO-LOCK NO-ERROR.
    IF AVAILABLE quotehd THEN
        FIND FIRST est WHERE est.company EQ quotehd.company
            AND est.est-no  EQ quotehd.est-no
            NO-LOCK NO-ERROR.

    {sa/sa-sls01.i}

    IF NOT ch-multi AND quotehd.est-no NE "" THEN
        FOR EACH tt-quote WHERE
            tt-quote.cust-no EQ icCustno,
            FIRST quotehd
            WHERE ROWID(quotehd) EQ tt-quote.row-id
            AND (CAN-FIND(FIRST est
            WHERE est.company EQ quotehd.company
            AND est.est-no  EQ quotehd.est-no) OR
            lookup(v-print-fmt,"Century,Unipak,PPI,Packrite") > 0 )
            NO-LOCK,

            FIRST quoteitm OF quotehd NO-LOCK,

            FIRST sman
            WHERE sman.company EQ quotehd.company
            AND sman.sman    EQ quotehd.sman
            NO-LOCK,

            FIRST carrier
            WHERE carrier.company EQ quotehd.company
            AND carrier.carrier EQ quotehd.carrier
            NO-LOCK,

            FIRST terms
            WHERE terms.company EQ quotehd.company
            AND terms.t-code  EQ quotehd.terms
            NO-LOCK,

            FIRST cust
            WHERE cust.company EQ quotehd.company
            AND cust.cust-no EQ quotehd.cust-no
            NO-LOCK

            TRANSACTION:

            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = quotehd.cust-no
                report.key-02  = IF ch-sort EQ "E" THEN quotehd.est-no              ELSE
                    IF ch-sort EQ "C" THEN quoteitm.part-no            ELSE
                    IF ch-sort EQ "A" THEN STRING(tt-seq,"9999999999") ELSE ""
                report.key-03  = STRING(quotehd.q-no,"9999999999")
                report.rec-id  = RECID(quotehd).
        END.

    ELSE    
        FOR EACH tt-quote WHERE
            tt-quote.cust-no EQ icCustno,
            FIRST quotehd WHERE ROWID(quotehd) EQ tt-quote.row-id NO-LOCK,
            FIRST quoteitm OF quotehd NO-LOCK,
            FIRST sman
            WHERE sman.company EQ quotehd.company
            AND sman.sman    EQ quotehd.sman
            NO-LOCK,

            FIRST carrier
            WHERE carrier.company EQ quotehd.company
            AND carrier.carrier EQ quotehd.carrier
            NO-LOCK,

            FIRST terms
            WHERE terms.company EQ quotehd.company
            AND terms.t-code  EQ quotehd.terms
            NO-LOCK,

            FIRST cust
            WHERE cust.company EQ quotehd.company
            AND cust.cust-no EQ quotehd.cust-no
            NO-LOCK
            TRANSACTION:
            IF quotehd.est-no <> "" AND lookup(v-print-fmt,"Century,Unipak,PPI,Packrite") <= 0 THEN 
            DO:
                FIND FIRST est WHERE est.company EQ quotehd.company
                    AND est.est-no  EQ quotehd.est-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE est THEN  NEXT.
            END.

            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = quotehd.cust-no
                report.key-02  = IF ch-sort EQ "E" THEN quotehd.est-no              ELSE
                    IF ch-sort EQ "C" THEN quoteitm.part-no            ELSE
                    IF ch-sort EQ "A" THEN STRING(tt-seq,"9999999999") ELSE ""
                report.key-03  = STRING(quotehd.q-no,"9999999999")
                report.rec-id  = RECID(quotehd).
        END.

    ASSIGN
        v-term-id        = v-term
        v-lines-per-page = lines-per-page.

    /* Check for XL also */
    IF v-print-fmt = "CSC-EXCEL" OR v-print-fmt = "TRILAKE-EXCEL" OR v-print-fmt = "FIBRE-EXCEL" OR v-print-fmt = "NOSCO-EXCEL" 
        OR v-print-fmt = "MSPACK-EXCEL" OR v-print-fmt = "PREMIER-EXCEL" OR v-print-fmt = "QuoPrint-Excel-Mex" OR v-print-fmt = "PREMIER-EXCEL-MCI" 
        OR v-print-fmt = "CCC-EXCEL" OR v-print-fmt = "BELL-EXCEL" THEN.
    ELSE
        IF IS-xprint-form THEN 
        DO:
            CASE rd-dest:
                WHEN 1 THEN 
                    DO:
                        IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) GT 0  THEN
                            PUT  "<PRINTER?><LEFT=" + string(dQuoteValue) + "mm></PROGRESS>" FORMAT "x(50)".
                        ELSE PUT  "<PRINTER?></PROGRESS>" FORMAT "x(50)".
                    END.
                WHEN 2 THEN 
                    DO:
                        IF NOT lBussFormModle THEN 
                        DO:
                            IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) GT 0  THEN
                                PUT "<PREVIEW><LEFT=" + string(dQuoteValue) + "mm><MODAL=NO></PROGRESS>" FORMAT "x(50)". 
                            ELSE PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORMAT "x(50)". 
                        END.
                        ELSE 
                        DO:
                            IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) GT 0  THEN
                                PUT "<PREVIEW><LEFT=" + string(dQuoteValue) + "mm></PROGRESS>" FORMAT "x(50)".     
                            ELSE PUT "<PREVIEW></PROGRESS>" FORMAT "x(50)". 
                        END.
                    END.      

                WHEN  4 THEN 
                    DO:
                        ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                        PUT UNFORMATTED 
                            "<PRINTER?><EXPORT=" Ls-fax-file ",BW></PROGRESS>".
                    END.
                WHEN 5 THEN 
                    DO:
                        IF LOOKUP(v-print-fmt,"century,unipak,PPI,Packrite") > 0 THEN       
                            PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
                        ELSE IF LOOKUP(v-print-fmt,"QuoPrintVAL") > 0 THEN
                                PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=MS Mincho><LEFT=" + string( dQuoteValue) +  "mm><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)" "</PROGRESS>".
                            ELSE IF LOOKUP(v-print-fmt,cCheckLeftMarFormat) > 0 THEN
                                    PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><LEFT=" + string( dQuoteValue) +  "mm><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
                                ELSE PUT "<FORMAT=LETTER><PREVIEW><PDF-EXCLUDE=Arial,Courier New><PDF-LEFT=2mm><PDF-TOP=3mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
                    END.
            END CASE.
        END.

    RUN value(v-program).

    FOR EACH report WHERE report.term-id EQ v-term-id:
        DELETE report.
    END.

    ASSIGN
        tb_note:SCREEN-VALUE = "YES"
        /*rd_sort:SCREEN-VALUE = "Quote#"*/              /*Task# 12121314*/
        tb_note
        rd_sort.


    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE ("").


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMail-1 C-Win 
PROCEDURE SendMail-1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icIdxKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRecType  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-quote-no AS INTEGER NO-UNDO.

    DEFINE VARIABLE vcSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcMailBody AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcErrorMsg AS CHARACTER NO-UNDO.

    ASSIGN  
        vcSubject  = "Quote # " + STRING(ip-quote-no) + " " + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
        vcMailBody = "Please review attached quote(s)".

    RUN custom/xpmail2.p   (INPUT  icRecType,
        INPUT  'r-quoprt.',
        INPUT  icFileName,
        INPUT  icIdxKey,
        INPUT  vcSubject,
        INPUT  vcMailBody,
        OUTPUT vcErrorMsg).
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
    ASSIGN
        lv-pdf-file = INIT-dir + "\" + (IF v-print-fmt EQ "Century" THEN "CBXQuote"
                              ELSE "QT") + STRING(begin_quo#).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetQuoForm C-Win 
PROCEDURE SetQuoForm :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icPrintFormat AS CHARACTER NO-UNDO.
    IF INDEX("Pacific,Xprint,RFC,quoprint 1,quoprint 2,quoprint 10,McElroy,QuoPrintVAL,quoprint 11,GC,quoprint 20,Chattanooga,Printers,Hughes,SouthPak,ABox,Midwest,Axis,MWFIBRE,century,Concepts,oracle,Harwell,quoprint10-CAN,PremierX,Elite,Unipak,Ottpkg,Frankstn,Mirpkg,APC,Perform,FibreX,Boss,Protagon,Loylang,LoylangBSF,PPI,Packrite,Xprint30,StClair,AllWest,Soule,Sultana,SouleMed,Simkins,CCC,Peachtree,Altex,Oklahoma,Accord,Onducorr",icPrintFormat) > 0 THEN
        is-xprint-form = YES.     
    ELSE is-xprint-form = NO.

    CASE icPrintFormat:
        WHEN "HOP" THEN 
            ASSIGN 
                v-program      = "cec/quote/quohop.p" 
                lines-per-page = 37.
        WHEN "LandScap" THEN 
            ASSIGN 
                v-program      = "ce/quote/landquo.p" 
                lines-per-page = 56.
        WHEN "ContSrvc" OR 
        WHEN "Triad" THEN 
            ASSIGN 
                v-program      = "cec/quote/quocsc.p" 
                lines-per-page = 56.
        WHEN "Rudd" THEN 
            ASSIGN 
                v-program      = "cec/quote/quorudd.p" 
                lines-per-page = 66.
        WHEN "General" THEN 
            ASSIGN 
                v-program      = "cec/quote/quogener.p" 
                lines-per-page = 56.
        WHEN "10 Pitch" THEN 
            ASSIGN 
                v-program      = "cec/quote/prtquo10.p" 
                lines-per-page = 56.
        WHEN "Brick" THEN 
            ASSIGN 
                v-program      = "cec/quote/quobrick.p" 
                lines-per-page = 38.
        WHEN "Fibre" THEN 
            ASSIGN 
                v-program      = "cec/quote/quofibre.p" 
                lines-per-page = 52.
        WHEN "Harwell" THEN 
            ASSIGN 
                v-program      = "cec/quote/quohawl.p" 
                lines-per-page = 56.
        WHEN "quoprint10-CAN" THEN 
            ASSIGN 
                v-program      = "cec/quote/quo10can.p" 
                lines-per-page = 56.
        WHEN "Pacific" THEN 
            ASSIGN 
                v-program      = "cec/quote/quopacif.p" 
                lines-per-page = 66.
        WHEN "Abox" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoabox.p" 
                lines-per-page = 66.
        WHEN "Xprint" OR 
        WHEN "quoprint 1" OR 
        WHEN "quoprint 2" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxprnt.p" 
                lines-per-page = 66.
        WHEN "quoprint 10" OR 
        WHEN "quoprint 20" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxprnt10.p" 
                lines-per-page = 66.
        WHEN "McElroy" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoMcElroy.p" 
                lines-per-page = 66.
        WHEN "QuoPrintVAL" OR 
        WHEN "quoprint 20" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxprntval.p" 
                lines-per-page = 66.
        WHEN "quoprint 11" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxprnt11.p" 
                lines-per-page = 66.
        WHEN "GC" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoGc.p" 
                lines-per-page = 66.        
        WHEN "Printers" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoprnts.p" 
                lines-per-page = 66.
        WHEN "Hughes" THEN 
            ASSIGN 
                v-program      = "cec/quote/quohughes.p" 
                lines-per-page = 66.
        WHEN "Oklahoma" THEN 
            ASSIGN 
                v-program      = "cec/quote/quookla.p" 
                lines-per-page = 66.
        WHEN "FibreX" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxfib.p" 
                lines-per-page = 66.
        WHEN "Boss" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoboss.p" 
                lines-per-page = 66.
        WHEN "Protagon" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoprotg.p" 
                lines-per-page = 66.
        WHEN "Loylang" THEN 
            ASSIGN 
                v-program      = "cec/quote/quolylng.p" 
                lines-per-page = 66.
        WHEN "LoylangBSF" THEN 
            ASSIGN 
                v-program      = "cec/quote/quolylng.p" 
                lines-per-page = 66.
        WHEN "Frankstn" OR 
        WHEN "Mirpkg" THEN 
            ASSIGN 
                v-program      = "cec/quote/quofrank.p" 
                lines-per-page = 66.
        WHEN "Elite" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoelite.p" 
                lines-per-page = 66.
        WHEN "premierX" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxprem.p" 
                lines-per-page = 66.
        WHEN "Premier-Excel" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoprm-xl.p" 
                lines-per-page = 66.
        WHEN "QuoPrint-Excel-Mex" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoprm-mex.p" 
                lines-per-page = 66.
        WHEN "Premier-Excel-Mci" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoprm-mci.p" 
                lines-per-page = 66.
        WHEN "Bell-Excel" THEN 
            ASSIGN 
                v-program      = "cec/quote/quobell-xl.p" 
                lines-per-page = 66.
        WHEN "CCC-Excel" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoccc-xl.p" 
                lines-per-page = 66.
        WHEN "SouthPak" THEN 
            ASSIGN 
                v-program      = "cec/quote/quosthpk.p" 
                lines-per-page = 66.   
        WHEN "APC" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxapc.p" 
                lines-per-page = 66.
        WHEN "Perform" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoprfrm.p" 
                lines-per-page = 66.
        WHEN "Midwest" THEN 
            ASSIGN 
                v-program      = "cec/quote/quomwest.p" 
                lines-per-page = 66.
        WHEN "Axis" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoaxis.p" 
                lines-per-page = 66.
        WHEN "MWFIBRE" THEN 
            ASSIGN 
                v-program      = "cec/quote/quomwfib.p" 
                lines-per-page = 66.
        WHEN "Century" THEN 
            ASSIGN 
                v-program      = "cec/quote/quocentx.p" 
                lines-per-page = 66.
        WHEN "Unipak" THEN 
            ASSIGN 
                v-program      = "cec/quote/quounipk.p" 
                lines-per-page = 66.
        WHEN "Oracle" THEN 
            ASSIGN 
                v-program      = "cec/quote/quooracl.p" 
                lines-per-page = 66.
        WHEN "OTTPkg" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoottpk.p" 
                lines-per-page = 66.
        WHEN "CSC-EXCEL" THEN 
            ASSIGN 
                v-program      = "cec/quote/quocsc-xl.p" 
                lines-per-page = 66.
        WHEN "FIBRE-EXCEL" THEN 
            ASSIGN 
                v-program      = "cec/quote/quofib-xl.p" 
                lines-per-page = 66.
        WHEN "TRILAKE-EXCEL" THEN 
            ASSIGN 
                v-program      = "cec/quote/quotri-xl.p" 
                lines-per-page = 66.
        WHEN "Concepts" THEN 
            ASSIGN 
                v-program      = "cec/quote/quocorc.p" 
                lines-per-page = 66.
        WHEN "Accord" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoaccd.p" 
                lines-per-page = 66.
        WHEN "PPI" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoppi.p" 
                lines-per-page = 66.
        WHEN "Packrite" THEN 
            ASSIGN 
                v-program      = "cec/quote/quopkrit.p" 
                lines-per-page = 66.
        WHEN "NOSCO-EXCEL" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoknight-xl.p" 
                lines-per-page = 66. /* gdm - 11060808 */
        WHEN "Xprint30" THEN 
            ASSIGN 
                v-program      = "cec/quote/qoxpnt30.p" 
                lines-per-page = 66.
        WHEN "StClair" THEN 
            ASSIGN 
                v-program      = "cec/quote/qosclair.p" 
                lines-per-page = 66.
        WHEN "MSPACK-EXCEL" THEN 
            ASSIGN 
                v-program      = "cec/quote/quomsp-xl.p" 
                lines-per-page = 66.
        WHEN "Onducorr" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoOndu.p" 
                lines-per-page = 56.
        /* gdm - 04200908*/
        WHEN "AllWest" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoalwst.p" 
                lines-per-page = 66.        
        /* gdm - 04300907*/
        WHEN "Soule" THEN 
            ASSIGN 
                v-program      = "cec/quote/quosoule.p" 
                lines-per-page = 80.
        WHEN "Sultana" THEN 
            ASSIGN 
                v-program      = "cec/quote/quosult.p" 
                lines-per-page = 80.
        WHEN "SouleMed" THEN 
            ASSIGN 
                v-program      = "cec/quote/quosoulemed.p" 
                lines-per-page = 66.    
        WHEN "Simkins" THEN 
            ASSIGN 
                v-program      = "cec/quote/quosmkct.p" 
                lines-per-page = 66.
        WHEN "CCC" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoccc.p" 
                lines-per-page = 66.
        WHEN "Peachtree" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoxptree.p" 
                lines-per-page = 66.
        WHEN "Altex" THEN 
            ASSIGN 
                v-program      = "cec/quote/quoaltex.p" 
                lines-per-page = 66.
        WHEN "RFC" THEN 
            ASSIGN 
                v-program      = "cec/quote/quorfc.p" 
                lines-per-page = 66.
        WHEN "Chattanooga" THEN 
            ASSIGN 
                v-program      = "cec/quote/quochatt.p" 
                lines-per-page = 66.
        OTHERWISE 
        DO:
            IF AVAILABLE est AND est.est-type GT 4 THEN
                ASSIGN
                    v-program      = "cec/quote/quoasi.p"
                    lines-per-page = 56.

            ELSE
                ASSIGN
                    v-program      = "cec/quote/quoasi.p"
                    lines-per-page = IF v-log THEN 50 ELSE 56.
        END.
    END.

    v-tmp-lines-per-page = lines-per-page.

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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha     NO-UNDO.

    ASSIGN
        lv-frame-hdl = FRAME {&frame-name}:HANDLE
        lv-group-hdl = lv-frame-hdl:FIRST-CHILD
        lv-field-hdl = lv-group-hdl:FIRST-CHILD .

    IF v-print-fmt <> "CSC-EXCEL" AND v-print-fmt <> "TRILAKE-EXCEL" AND
        v-print-fmt <> "FIBRE-EXCEL" AND
        v-print-fmt <> "PREMIER-EXCEL" AND
        v-print-fmt <> "QuoPrint-Excel-Mex" AND
        v-print-fmt <> "PREMIER-EXCEL-MCI" AND
        v-print-fmt <> "BELL-EXCEL" AND
        v-print-fmt <> "CCC-EXCEL" AND
        v-print-fmt <> "NOSCO-EXCEL" THEN.
    ELSE 
    DO :

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
        PAGE.

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

