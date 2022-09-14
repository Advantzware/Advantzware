&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-rctcrd.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */     

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS cha       NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

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

DEFINE VARIABLE v-msf-rem   AS DECIMAL   FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE v-cst-rem   AS DECIMAL   FORMAT ">>>,>>9" NO-UNDO.
DEFINE VARIABLE v-s-vend    LIKE vend.vend-no INIT "" NO-UNDO.
DEFINE VARIABLE v-e-vend    LIKE vend.vend-no INIT "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE v-s-date    LIKE po-ord.po-date FORMAT "99/99/9999" NO-UNDO INIT 01/01/2000.
DEFINE VARIABLE v-e-date    LIKE po-ord.po-date FORMAT "99/99/9999" INIT TODAY NO-UNDO.

DEFINE VARIABLE v-cust-name LIKE oe-ord.cust-name.
DEFINE VARIABLE v-wid       LIKE po-ordl.s-wid.
DEFINE VARIABLE v-len       LIKE po-ordl.s-len.
DEFINE VARIABLE v-dep       LIKE item.s-dep.

DEFINE VARIABLE v-sortby    AS LOGICAL   FORMAT "J/C" INIT YES.
DEFINE VARIABLE v-bwt       LIKE item.basis-w.
DEFINE VARIABLE v-raw       LIKE po-ordl.i-no.
DEFINE VARIABLE v-fg        LIKE po-ordl.i-no.

DEFINE VARIABLE v-cust-vend AS CHARACTER FORMAT "x(26)" INIT "--------- VENDOR ---------".

DEFINE VARIABLE v-job-no    AS CHARACTER FORMAT "x(13)".

DEFINE TEMP-TABLE wk-sh-ord NO-UNDO
    FIELD due-date LIKE po-ordl.due-date
    FIELD rec-id   AS RECID.

DEFINE VARIABLE tot-cons-qty    LIKE po-ordl.cons-qty NO-UNDO.
DEFINE VARIABLE tot-rec-qty     AS DECIMAL   NO-UNDO.

DEFINE VARIABLE tot-msf-rem     AS DECIMAL   FORMAT "->>>,>>>,>>>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE tot-msf-rem-str AS CHARACTER FORMAT "x(60)" .

DEFINE VARIABLE v-trcv          AS DECIMAL   NO-UNDO.    /* format "->>,>>9.9" */ 
DEFINE VARIABLE v-char-ord-qty  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-rel-date      LIKE oe-rel.rel-date.
DEFINE VARIABLE v-mach          LIKE mach.m-code.
DEFINE VARIABLE v-vend          LIKE po-ord.vend-no.
DEFINE VARIABLE v-stat          AS CHARACTER. 
DEFINE STREAM st-excel.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE            VARIABLE v-print-fmt    AS CHARACTER.
DEFINE            VARIABLE is-xprint-form AS LOGICAL.
DEFINE            VARIABLE ls-fax-file    AS cha       NO-UNDO.
DEFINE NEW SHARED VARIABLE factor#        AS DECIMAL   NO-UNDO.

FIND sys-ctrl WHERE sys-ctrl.company = cocode
    AND sys-ctrl.name = "poprint" 
    NO-LOCK NO-ERROR.

factor# = IF AVAILABLE sys-ctrl AND can-do("Premier,Middlesx,16th's",sys-ctrl.char-fld)
    THEN .16 ELSE 1.


DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Vendor,Job#,Qty Ord,Received,Units,Sht W,Sht L,RMItem#," +
                           "Vendor Item#,Due Date,PO#,Mach,Schedule#,Customer Name,Pieces Over,Overrun" 
    cFieldListToSelect = "vend,job,qty-ord,rece,unit,sht-w,sht-l,rm-ino," +
                            "vend-ino,du-dt,po,mch,schd,cust-nam,pec-ovr,ovrrn"
    cFieldLength       = "8,13,11,11,11,9,9,15," + "15,10,6,6,10,30,13,13" 
    cFieldType         = "c,c,i,i,i,i,i,c," + "c,c,c,c,c,c,i,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Vendor,Job#,Qty Ord,Received,Units,Sht W,Sht L,RMItem#," +
                           "Vendor Item#,Due Date,PO#,Mach,Schedule#,Customer Name,Pieces Over,Overrun"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend end_vend ~
begin_due-date end_due-date tb_page rd_sort rd-potype sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_vend end_vend begin_due-date ~
end_due-date tb_page rd_sort rd-potype sl_avail sl_selected rd-dest ~
td-show-parm fi_file tb_OpenCSV tbAutoClose lbl_sort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.1.

DEFINE VARIABLE begin_due-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_due-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-rctcrd.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 43 BY 1
    FGCOLOR 0 .

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS TEXT 
    SIZE 5.8 BY .62 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 55 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "L" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd-potype      AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "O",
    "Closed", "C",
    "Both", "B"
    SIZE 32.8 BY .71 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Job#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Job#", "Job#",
    "Customer#", "Customer#"
    SIZE 27 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 4.81.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.95.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_page      AS LOGICAL   INITIAL NO 
    LABEL "Page Break/Vendor?" 
    VIEW-AS TOGGLE-BOX
    SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend AT ROW 2.43 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 2.43 COL 69 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_due-date AT ROW 3.52 COL 26 COLON-ALIGNED
    end_due-date AT ROW 3.52 COL 69 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    tb_page AT ROW 4.76 COL 64.8 RIGHT-ALIGNED WIDGET-ID 58
    rd_sort AT ROW 5.67 COL 40.2 NO-LABELS
    rd-potype AT ROW 6.71 COL 40.2 NO-LABELS
    sl_avail AT ROW 8.91 COL 3.2 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 8.91 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 8.91 COL 60.2 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 9.91 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 10.91 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 11.95 COL 40.4 WIDGET-ID 40
    btn_down AT ROW 13 COL 40.4 WIDGET-ID 42
    rd-dest AT ROW 15.43 COL 5.6 NO-LABELS
    lv-font-name AT ROW 15.62 COL 26 COLON-ALIGNED NO-LABELS
    lines-per-page AT ROW 15.62 COL 49 COLON-ALIGNED
    lv-ornt AT ROW 15.67 COL 31 NO-LABELS
    lv-font-no AT ROW 15.86 COL 33 COLON-ALIGNED
    tb_excel AT ROW 16.1 COL 55 RIGHT-ALIGNED
    td-show-parm AT ROW 17.38 COL 39
    fi_file AT ROW 18.19 COL 28 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.19 COL 87.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 20.19 COL 31.8 WIDGET-ID 16
    btn-ok AT ROW 21.1 COL 31
    btn-cancel AT ROW 21.1 COL 53.6
    lbl_sort AT ROW 5.86 COL 31.2 COLON-ALIGNED NO-LABELS
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 8.1 COL 7.4 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 8.1 COL 59.8 WIDGET-ID 44
    "Report For:" VIEW-AS TEXT
    SIZE 12 BY .91 AT ROW 6.57 COL 27.2
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 14.52 COL 4.4
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.62 COL 4.2
    RECT-6 AT ROW 14.86 COL 3.2
    RECT-7 AT ROW 1.95 COL 3.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 99.4 BY 24.14
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
        TITLE              = "Receipts Overage for Credit Report"
        HEIGHT             = 21.91
        WIDTH              = 95.8
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 204.8
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 204.8
        MAX-BUTTON         = NO
        RESIZE             = NO
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = 15
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
    begin_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_page IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_page:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Receipts Overage for Credit Report */
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
ON WINDOW-CLOSE OF C-Win /* Receipts Overage for Credit Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
    DO:
        DELETE PROCEDURE hdOutputProcs.
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
        IF rd-dest = 3 THEN
        DO:
            fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        RUN GetSelectionList.
        RUN run-report. 
        STATUS DEFAULT "Processing Complete".

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN 
                DO:
                    IF NOT tb_OpenCSV THEN 
                    DO:        
                        MESSAGE "CSV file have been created." SKIP(1)
                            "~"OK"~"Want to open CSV file?"
                            VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                            TITLE "" UPDATE lChoice AS LOGICAL.
                     
                        IF lChoice THEN
                        DO:
                            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                        END.
                    END.
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_vend
                            &END_cust=END_vend
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        APPLY "DEFAULT-ACTION" TO sl_avail.

    /*
    DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
      IF sl_avail:IS-SELECTED(i) AND
        (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR sl_selected:NUM-ITEMS = 0) THEN
      /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
          cSelectedList = cSelectedList +
                          entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
    END.
    cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
    sl_selected:LIST-ITEM-PAIRS = cSelectedList.
    sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
    DO:
        DEFINE VARIABLE cSelectedList AS cha NO-UNDO.

        RUN DisplaySelectionDefault.  /* task 04041406 */ 
        RUN DisplaySelectionList2 .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
    DO:
        RUN Move-Field ("Down").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME FRAME-A /* << Remove */
    DO:
        /* DO i = sl_selected:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
           IF sl_selected:IS-SELECTED(i) THEN
           ldummy = sl_selected:DELETE(i).
         END
         */
        APPLY "DEFAULT-ACTION" TO sl_selected  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
    DO:
        RUN Move-Field ("Up").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
    DO:
    // assign {&self-name}.
        fi_file = ''.
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


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        RUN pChangeDest.
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


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
    DO:

        IF (NOT CAN-DO(sl_selected:LIST-ITEMs,{&SELF-NAME}:SCREEN-VALUE) OR
            sl_selected:NUM-ITEMS = 0)
            THEN ASSIGN ldummy = sl_selected:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
                ldummy = {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                /* sl_selected:SCREEN-VALUE = sl_selected:ENTRY(sl_selected:NUM-ITEMS) */
                .


    /* for pairs
        DEF VAR cSelectedList AS cha NO-UNDO.
        cSelectedList = sl_Selected:LIST-ITEM-PAIRS.
        DO i = 1 TO sl_avail:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
        IF sl_avail:IS-SELECTED(i) AND
          (NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i)) OR
             sl_selected:NUM-ITEMS = 0) THEN
        /*ldummy = sl_selected:ADD-LAST(sl_avail:ENTRY(i)).*/
            cSelectedList = cSelectedList +
                            entry(i,cTextListToSelect) + "," + entry(i,cFieldListToSelect) + ",".
        MESSAGE i sl_avail:IS-SELECTED(i) NOT CAN-DO(sl_selected:LIST-ITEM-PAIRS,sl_avail:ENTRY(i))
            sl_selected:NUM-ITEMS
            SKIP cSelectedList
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
      cSelectedList = SUBSTRING(cSelectedList,1,LENGTH(cSelectedList) - 1).
      sl_selected:LIST-ITEM-PAIRS = cSelectedList.
      sl_avail:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
    DO:
        DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
            IF {&SELF-NAME}:IS-SELECTED(i) THEN 
            DO:
                ASSIGN 
                    ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
                    ldummy = /*{&SELF-NAME}:DELETE(i)*/
                       {&SELF-NAME}:DELETE({&SELF-NAME}:SCREEN-VALUE)
                    .
            END.           
        END.
        IF {&SELF-NAME}:NUM-ITEMS NE 0 THEN
            ASSIGN
                {&SELF-NAME}:SCREEN-VALUE = {&SELF-NAME}:ENTRY(1)
                .


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_page C-Win
ON VALUE-CHANGED OF tb_page IN FRAME FRAME-A /* Page Break/Vendor? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.

    ASSIGN
        begin_due-date = DATE(1,1,YEAR(TODAY))
        end_due-date   = TODAY.

    fi_file = "c:\tmp\rctcredt.csv".
    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR10" }
    ASSIGN
        td-show-parm:sensitive = lShowParameters
        td-show-parm:hidden    = NOT lShowParameters
        td-show-parm:visible   = lShowParameters
        .
    
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_vend.
    END.
    RUN pChangeDest.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

        cListContents = cListContents +                   
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToDefault)   .
    END.            
    sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
PROCEDURE DisplaySelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.

    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:

        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */

            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.

    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 C-Win 
PROCEDURE DisplaySelectionList2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS cha     NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER NO-UNDO.
  
    IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN 
    DO:
        RETURN.
    END.

    EMPTY TEMP-TABLE ttRptList.

    DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

        cListContents = cListContents +
            /* (IF cListContents = "" THEN ""  ELSE ",") +
             ENTRY(iCount,cTextListToSelect) + "," +
             ENTRY(1,cFieldListToSelect)
             paris */

            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .
    END.

    /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */

    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    {sys/ref/SelColCorrect.i}

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
    DISPLAY begin_vend end_vend begin_due-date end_due-date tb_page rd_sort 
        rd-potype sl_avail sl_selected rd-dest td-show-parm fi_file 
        tb_OpenCSV tbAutoClose lbl_sort 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend end_vend begin_due-date end_due-date tb_page 
        rd_sort rd-potype sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose 
        btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList C-Win 
PROCEDURE GetSelectionList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpList AS cha NO-UNDO.

    EMPTY TEMP-TABLE ttRptSelected.
    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    iColumnLength = 0.

    DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
        FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

        CREATE ttRptSelected.
        ASSIGN 
            ttRptSelected.TextList        = ENTRY(i,cTmpList)
            ttRptSelected.FieldList       = ttRptList.FieldList
            ttRptSelected.FieldLength     = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
            ttRptSelected.DisplayOrder    = i
            ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
            iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        .        

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field C-Win 
PROCEDURE Move-Field :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER move AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
        WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN
        DO:
            IF move = "Down" AND i NE sl_selected:NUM-ITEMS THEN
                ASSIGN
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF move = "Up" AND i NE 1 THEN
                    ASSIGN
                        ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
                        ldummy                   = sl_selected:DELETE(i + 1)
                        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i - 1)
                        .
            LEAVE.
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
    {custom/out2file.i}.

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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ po/rep/sh-apr.p 6/00 djk  */
    /* Sheets On Order Report                                                     */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-cust-vend    AS CHARACTER FORMAT "x(26)" INIT "--------- VENDOR ---------".

    DEFINE VARIABLE v-job-no       AS CHARACTER FORMAT "x(13)".
    DEFINE VARIABLE v-overpcs      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-schedule     AS cha       FORM "X(9)" NO-UNDO.
    DEFINE VARIABLE v-over-allow   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-comma        AS cha       FORMAT "x" INIT "," NO-UNDO.

    DEFINE VARIABLE cDisplay       AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha       NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," .        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ","
                .        
        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "LINEAL FEET,MSF,COST VALUE") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM st-excel TO VALUE(cFileName).
        PUT STREAM st-excel UNFORMATTED excelheader SKIP.
    /* "Job#,Qty Ord,Received,Units,Sht W,Sht L,RMItem#,Vendor Item#,Due Date,PO#,Mach,Schedule#,Customer Name,Pieces Over,Overrun"
      SKIP.*/
    END.
    /*
    form header
         "Vendor:"
         v-vend
         skip(1)
         "Job#"                     at 1
         "PO#"                      to 16
         "Qty Ord"                  to 25
         "Receivd"                  to 33
         "Customer Name"            at 35
         "   Due  "                 at 56
         "Units"                    at 65
         "Sht W"                    to 77
         "Sht L"                    to 84
         "Mach"                     at 86
         "FGItem#"                  at 93
         "RMItem#"                  at 108
         "MSF Rem"                  to 126
         "Ship?"                    at 128
         skip
         fill("-",132) format "x(132)"    
        with frame r-top.
    
    form v-job-no                   at 1     
         po-ordl.po-no              to 16
         v-char-ord-qty             to 25
         v-trcv                     to 33
         v-cust-name                at 35  format "x(20)"
         tt-report.key-07           at 56
         "_____"                    at 65
         po-ordl.s-wid              to 77  format "->>9.99"
         po-ordl.s-len              to 84  format "->>9.99"
         v-mach                     at 86
         v-fg                       at 93  format "x(15)"       
         v-raw                      at 108 format "x(10)"
         v-msf-rem                  to 126
         "_____"                    at 128    
        with down STREAM-IO WIDTH 132 no-labels no-box no-underline frame sh-ord.
    */

    /*
    form header
         "Vendor:"
         v-vend
         skip(1)
         "Job#"                     at 1
         "Qty Ord"  AT 12
         "Received"
         "Units "
         "  Sht W"
         "    Sht L"
         "RMItem#        "
         "Vendor Item#   "
         "Due Date"
         "   PO#"
         "Mach  "
         "Schedule#"
         "Customer Name       "
         " Pieces Over"
         "       Overrun"
         fill("-",168) format "x(168)"    
        with frame r-top WIDTH 168.
    
    form v-job-no                   at 1     
         v-char-ord-qty
         v-trcv
         "     "
         v-wid  format "->>>>9.99<<"
         v-len  format "->>>>9.99<<"
         v-raw
         po-ordl.vend-i-no
         tt-report.key-07
         po-ordl.po-no
         v-mach
         v-schedule
         v-cust-name FORM "x(20)"
         v-overpcs     FORM ">>,>>>,>>9.99"
         v-over-allow  FORM ">>,>>>,>>9.99"
         with down STREAM-IO WIDTH 168 no-labels no-box no-underline frame sh-ord.
    */
    FORM SKIP(1) tot-msf-rem-str AT 1
        WITH FRAME grand-total STREAM-IO WIDTH 132 NO-BOX NO-LABELS.
    {sa/sa-sls01.i}
    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-vend = begin_vend
        v-e-vend = end_vend
        v-s-date = begin_due-date
        v-e-date = end_due-date
        v-sortby = rd_sort EQ "Job#".

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.
    /*
    if v-name then v-cust-vend = "-------- CUSTOMER --------".
    DISP WITH frame sh-head.
    
      FOR each holder:
        delete holder.
      end.
    
      for each wk-sh-ord:
        delete wk-sh-ord.
      end.
      */
    FOR EACH po-ord
        WHERE po-ord.company EQ cocode
        AND ((INDEX("CXF",po-ord.stat) EQ 0 AND rd-potype = "O") OR /* open */
        (INDEX("CXF",po-ord.stat) > 0 AND rd-potype = "C") OR /* closed */
        rd-potype = "B")
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        NO-LOCK:

        FOR EACH po-ordl
            WHERE po-ordl.company EQ po-ord.company
            AND po-ordl.po-no   EQ po-ord.po-no
            AND po-ordl.item-type
            AND ((INDEX("CXF",po-ord.stat) EQ 0 AND rd-potype = "O") OR /* open */
            (INDEX("CXF",po-ord.stat) > 0 AND rd-potype = "C") OR /* closed */
            rd-potype = "B")
            AND po-ordl.due-date GE v-s-date
            AND po-ordl.due-date LE v-e-date
            NO-LOCK:

            {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

            RELEASE job-hdr.
            RELEASE oe-ord.
            RELEASE oe-ordl.

            ASSIGN
                v-job-no    = TRIM(po-ordl.job-no) + "-" + string(po-ordl.job-no2,"999")
                v-raw       = ""
                v-fg        = ""
                v-cust-name = "".

            IF TRIM(v-job-no) BEGINS "-" THEN v-job-no = "".

            IF po-ordl.item-type THEN 
            DO:
                v-raw = po-ordl.i-no.

                FIND FIRST job-hdr
                    WHERE job-hdr.company EQ cocode
                    AND job-hdr.job-no  EQ po-ordl.job-no
                    AND job-hdr.job-no2 EQ po-ordl.job-no2
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-hdr THEN 
                DO:
                    v-fg = job-hdr.i-no.

                    FIND FIRST oe-ordl
                        WHERE oe-ordl.company EQ cocode
                        AND oe-ordl.ord-no  EQ job-hdr.ord-no
                        AND oe-ordl.i-no    EQ v-fg
                        AND oe-ordl.job-no  EQ po-ordl.job-no
                        AND oe-ordl.job-no2 EQ po-ordl.job-no2
                        NO-LOCK NO-ERROR.
                END.
            END.

            ELSE 
            DO:
                v-fg = po-ordl.i-no.

                IF po-ordl.ord-no NE 0 THEN
                    FIND FIRST oe-ordl
                        WHERE oe-ordl.company EQ cocode
                        AND oe-ordl.ord-no  EQ po-ordl.ord-no
                        AND oe-ordl.i-no    EQ v-fg
                        NO-LOCK NO-ERROR.
            END.

            v-rel-date = ?.

            IF AVAILABLE oe-ordl THEN 
            DO:
                FIND FIRST oe-ord
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no  EQ oe-ordl.ord-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN v-cust-name = oe-ord.cust-name.

                FOR EACH oe-relh
                    WHERE oe-relh.company EQ cocode
                    AND oe-relh.ord-no  EQ oe-ordl.ord-no
                    AND CAN-FIND(FIRST oe-rell WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no
                    AND oe-rell.i-no    EQ oe-ordl.i-no
                    AND oe-rell.line    EQ oe-ordl.line
                    USE-INDEX r-no)
                    AND oe-relh.posted  EQ NO
                    USE-INDEX order NO-LOCK
                    BY oe-relh.rel-date:

                    LEAVE.
                END.

                IF AVAILABLE oe-relh THEN v-rel-date = oe-relh.rel-date.

                ELSE
                    FOR EACH oe-rel
                        WHERE oe-rel.company EQ cocode
                        AND oe-rel.ord-no  EQ oe-ordl.ord-no
                        AND oe-rel.i-no    EQ oe-ordl.i-no
                        AND oe-rel.line    EQ oe-ordl.line
                        NO-LOCK:

                        {oe/rel-stat.i v-stat}

                        IF INDEX("ILS",v-stat) NE 0                           AND
                            (oe-rel.rel-date LT v-rel-date OR v-rel-date EQ ?) THEN
                            v-rel-date = oe-rel.rel-date.
                    END.
            END.

            CREATE tt-report.
            ASSIGN
                tt-report.key-01 = po-ord.vend-no
                tt-report.key-02 = IF v-sortby THEN v-job-no ELSE v-cust-name
                tt-report.key-03 = v-job-no
                tt-report.key-04 = v-fg
                tt-report.key-05 = v-raw
                tt-report.key-06 = v-cust-name
                tt-report.key-07 = IF v-rel-date EQ ? THEN ""
                                             ELSE STRING(v-rel-date,"99/99/9999")
                tt-report.rec-id = RECID(po-ordl).
        END.
    END.

    /* if v-name then v-cust-vend = "-------- CUSTOMER --------".*/
    VIEW FRAME r-top.

    FOR EACH tt-report,
        EACH po-ordl WHERE RECID(po-ordl) EQ tt-report.rec-id NO-LOCK,
        FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK
        BREAK BY tt-report.key-01
        BY tt-report.key-02:

        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

        IF FIRST-OF(tt-report.key-01) THEN 
        DO:
            v-vend = tt-report.key-01.
            IF tb_page THEN
                PAGE.
        END.

        RELEASE item.
        RELEASE itemfg.

        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
        END.

        ELSE 
        DO:
            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
        END.

        ASSIGN
            v-len = po-ordl.s-len
            v-wid = po-ordl.s-wid
            v-dep = IF AVAILABLE item THEN item.s-dep ELSE 0
            v-bwt = 0.

        IF (v-len EQ 0 OR v-wid EQ 0 OR v-bwt EQ 0) THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ po-ordl.job-no
                AND job.job-no2 EQ po-ordl.job-no2
                NO-LOCK NO-ERROR.

            IF AVAILABLE job THEN 
            DO:
                FIND FIRST job-mat
                    WHERE job-mat.company EQ cocode
                    AND job-mat.job     EQ job.job
                    AND job-mat.i-no    EQ po-ordl.i-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-mat THEN
                    ASSIGN
                        v-len = IF v-len EQ 0 THEN job-mat.len     ELSE v-len
                        v-wid = IF v-wid EQ 0 THEN job-mat.wid     ELSE v-wid
                        v-bwt = IF v-bwt EQ 0 THEN job-mat.basis-w ELSE v-bwt.
            END.

            IF AVAILABLE item THEN 
            DO:
                IF po-ordl.item-type = YES THEN 
                    IF v-len EQ 0 THEN v-len = item.s-len.

                IF po-ordl.item-type = YES THEN 
                    IF v-wid EQ 0 THEN v-wid = IF item.r-wid NE 0 THEN item.r-wid
                        ELSE item.s-wid.

                IF po-ordl.item-type = YES THEN 
                    IF v-bwt EQ 0 THEN v-bwt = item.basis-w.
            END.
        END.

        IF po-ordl.cons-uom EQ "MSF" THEN
            v-msf-rem = po-ordl.cons-qty - po-ordl.t-rec-qty.

        ELSE 
        DO:
            RUN sys/ref/convquom.p(po-ordl.cons-uom, "MSF",
                v-bwt, v-len, v-wid, v-dep,
                (po-ordl.cons-qty - po-ordl.t-rec-qty),
                OUTPUT v-msf-rem).
        END.

        RUN sys/ref/convcuom.p(po-ordl.cons-uom, "MSF",
            v-bwt, v-len, v-wid, v-dep,
            po-ordl.cons-cost, OUTPUT v-cst-rem).

        ASSIGN
            tot-cons-qty = tot-cons-qty + po-ordl.cons-qty
            tot-rec-qty  = tot-rec-qty + t-rec-qty
            tot-msf-rem  = tot-msf-rem + v-msf-rem.

        IF v-msf-rem GE 0 OR v-cst-rem GE 0 THEN 
        DO:
            ASSIGN
                v-job-no    = tt-report.key-03
                v-fg        = tt-report.key-04
                v-raw       = tt-report.key-05
                v-cust-name = tt-report.key-06.

            /*    if (v-cust-name eq "") or (v-name eq no) then v-cust-name = fill("_",20). */

            IF po-ordl.ord-qty - trunc(po-ordl.ord-qty,0) NE 0 AND
                po-ordl.ord-qty LT 100000                       THEN
                v-char-ord-qty = po-ordl.ord-qty.
            ELSE
                v-char-ord-qty = po-ordl.ord-qty.

            IF po-ordl.t-rec-qty NE 0 THEN
                IF po-ordl.t-rec-qty - trunc(po-ordl.t-rec-qty,0) NE 0 AND
                    po-ordl.t-rec-qty LT 10000                          THEN
                    v-trcv = po-ordl.t-rec-qty.
                ELSE
                    v-trcv = po-ordl.t-rec-qty.
            ELSE
                v-trcv = 0.

            v-mach = "".

            FOR FIRST job
                WHERE job.company EQ po-ordl.company
                AND job.job-no  EQ po-ordl.job-no
                AND job.job-no2 EQ po-ordl.job-no2
                NO-LOCK,

                FIRST job-mch
                WHERE job-mch.company EQ job.company
                AND job-mch.job     EQ job.job
                AND job-mch.frm     EQ po-ordl.s-num
                AND (job-mch.dept   NE "DM" AND
                job-mch.dept   NE "PM")
                USE-INDEX line-idx NO-LOCK:

                v-mach = job-mch.m-code.
            END.
            /*
            display v-job-no                              
                    po-ordl.po-no
                    v-char-ord-qty                      
                    v-trcv
                    v-cust-name                         
                    tt-report.key-07
                      "________" when not v-preld @ tt-report.key-07
                    po-ordl.s-wid                       
                    po-ordl.s-len
                    v-mach
                      "______" when not v-pmach @ v-mach
                    v-fg when v-fgitem 
                    v-raw
                    v-msf-rem                            
                with frame sh-ord.
             */
            ASSIGN 
                v-over-allow = po-ordl.ord-qty + (po-ordl.ord-qty * po-ordl.over-pct / 100)             
                v-overpcs    = DEC(v-trcv) - v-over-allow
                .
            IF v-overpcs < 0 THEN v-overpcs = 0.
            ASSIGN 
                v-wid = po-ordl.s-wid
                v-len = po-ordl.s-len
                {po/calc16.i v-wid}
                {po/calc16.i v-len}.



            /*   DISPLAY v-job-no                   at 1     
                       v-char-ord-qty
                       v-trcv
                       "     "
                       v-wid
                       v-len
                       v-raw
                       po-ordl.vend-i-no
                       tt-report.key-07
                       po-ordl.po-no
                       v-mach
                       v-schedule
                       v-cust-name
                       v-overpcs     WHEN v-overpcs > 0
                       v-over-allow 
                       WITH FRAME sh-ord.
               down with frame sh-ord.
               IF tb_excel THEN
                   PUT STREAM st-excel
                       v-job-no v-comma
                       v-char-ord-qty  v-comma
                       v-trcv          v-comma
                       "     "        v-comma
                       v-wid  format "->>>>9.99<<" v-comma
                       v-len  format "->>>>9.99<<" v-comma
                       v-raw          v-comma
                       po-ordl.vend-i-no v-comma
                       tt-report.key-07  v-comma
                       po-ordl.po-no     v-comma
                       v-mach            v-comma
                       v-schedule        v-comma
                       v-cust-name       v-comma
                       v-overpcs      format ">>,>>>,>>9.99"   v-comma
                       v-over-allow  format ">>,>>>,>>9.99"
                       SKIP. */

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "vend"      THEN 
                        cVarValue = STRING(v-vend) .
                    WHEN "job"       THEN 
                        cVarValue = STRING(v-job-no) .
                    WHEN "qty-ord"   THEN 
                        cVarValue = STRING(v-char-ord-qty,">>>>,>>9.99") .
                    WHEN "rece"      THEN 
                        cVarValue = STRING(v-trcv,">>>>,>>9.99") .
                    WHEN "sht-w"     THEN 
                        cVarValue = STRING(v-wid,"->>>>9.99<<")  .
                    WHEN "sht-l"     THEN 
                        cVarValue = STRING(v-len,"->>>>9.99<<").
                    WHEN "rm-ino"    THEN 
                        cVarValue = STRING(v-raw).
                    WHEN "vend-ino"  THEN 
                        cVarValue = STRING(po-ordl.vend-i-no) .
                    WHEN "du-dt"     THEN 
                        cVarValue = IF tt-report.key-07 NE ? THEN STRING(tt-report.key-07) ELSE "" .
                    WHEN "po"        THEN 
                        cVarValue = STRING(po-ordl.po-no) .
                    WHEN "mch"       THEN 
                        cVarValue = STRING(v-mach) .
                    WHEN "schd"      THEN 
                        cVarValue = STRING(v-schedule)  .
                    WHEN "cust-nam"  THEN 
                        cVarValue = STRING(v-cust-name).
                    WHEN "pec-ovr"   THEN 
                        cVarValue = STRING(v-overpcs,">>,>>>,>>9.99").
                    WHEN "ovrrn"     THEN 
                        cVarValue = STRING(v-over-allow,">>,>>>,>>9.99") .

                END CASE.

                IF  cTmpField = "du-dt" THEN
                     cExcelVarValue = IF tt-report.key-07 NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",DATE(tt-report.key-07)) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM st-excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

        END.

        DELETE tt-report.
    END.

    ASSIGN
        tot-msf-rem-str = "".
    tot-msf-rem-str = "Total Remaining MSF: " +
        trim(STRING(tot-msf-rem,"->>>,>>>,>>>,>>>,>>9.99")).

    DISPLAY tot-msf-rem-str WITH FRAME grand-total.
    ASSIGN 
        tot-msf-rem-str = ""
        tot-msf-rem     = 0.

    OUTPUT CLOSE.

  IF rd-dest = 3 THEN DO:
     OUTPUT STREAM st-excel CLOSE.
  END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
        lv-field-hdl = lv-group-hdl:FIRST-CHILD.

    DO WHILE TRUE:
        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
            THEN 
        DO:
            IF lv-field-hdl:LABEL <> ? THEN 
                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ",".
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pChangeDest C-Win
PROCEDURE pChangeDest :
    /*------------------------------------------------------------------------------
     Purpose:    
     Parameters:  <none>
     Notes:      
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        IF rd-dest:SCREEN-VALUE EQ "3" THEN
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "Yes"
                fi_file:SENSITIVE       = YES
                tb_OpenCSV:SENSITIVE    = YES      
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO      
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\r-rctcrd.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
    RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

