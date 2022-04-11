&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-pofgh1.w

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
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

DEFINE VARIABLE v-s-pono    LIKE po-ord.po-no FORMAT ">>>>>>>>".
DEFINE VARIABLE v-e-pono    LIKE v-s-pono INIT 999999.
DEFINE VARIABLE v-s-date    LIKE po-ord.po-date FORMAT "99/99/9999" INIT "01/01/0001".
DEFINE VARIABLE v-e-date    LIKE v-s-date INIT TODAY.
DEFINE VARIABLE v-s-vend    LIKE po-ord.vend-no.
DEFINE VARIABLE v-e-vend    LIKE v-s-vend INIT "zzzzzzzz".
DEFINE VARIABLE v-s-item    LIKE po-ordl.i-no.
DEFINE VARIABLE v-e-item    LIKE v-s-item INIT "zzzzzzzzzzzzzzz".
DEFINE VARIABLE v-s-vitm    LIKE po-ordl.vend-i-no.
DEFINE VARIABLE v-e-vitm    LIKE v-s-vitm INIT "zzzzzzzzzzzzzzz".
DEFINE VARIABLE v-sdue-date LIKE po-ord.due-date FORMAT "99/99/9999" INIT "01/01/0001".
DEFINE VARIABLE v-edue-date LIKE v-sdue-date INIT TODAY.
DEFINE VARIABLE v-stat      AS CHARACTER FORMAT "!" INIT "A".
DEFINE VARIABLE v-type      AS CHARACTER FORMAT "!" INIT "B".
DEFINE VARIABLE v-sort      AS CHARACTER FORMAT "!" INIT "C".

DEFINE STREAM s-temp.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE VARIABLE v-print-fmt        AS CHARACTER.
DEFINE VARIABLE is-xprint-form     AS LOGICAL.
DEFINE VARIABLE ls-fax-file        AS CHARACTER NO-UNDO.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .

ASSIGN 
    cTextListToSelect  = "Customer,PO#,Vendor,Vendor Name,Cust Part#,FG Item#,Item Name,Width,Length," +
                           "PO Cost,UOM,Due Date,Recpt Date,Recpt Qty" 
    cFieldListToSelect = "cust,po,vend,vend-nam,cst-prt,ino,iname,wid,len," +
                            "po-cst,uom,du-dt,rcp-dt,rcp-qty"
    cFieldLength       = "8,6,8,25,32,15,30,8,8," + "9,5,10,10,12"
    cFieldType         = "c,c,c,c,c,c,c,i,i," + "i,c,c,c,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Customer,PO#,Vendor,Vendor Name,Cust Part#,FG Item#,Item Name,Width,Length," +
                           "PO Cost,UOM,Due Date,Recpt Date,Recpt Qty" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 end_po-no begin_po-no ~
begin_po-date end_po-date begin_vend end_vend begin_po-i-no end_po-i-no ~
begin_ctrl-no end_ctrl-no begin_r-date end_r-date begin_due-date ~
end_due-date rd_sort select-mat rd_show tb_po tb_fg tb_rm btn_SelectColumns ~
rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS end_po-no begin_po-no begin_po-date ~
end_po-date begin_vend end_vend begin_po-i-no end_po-i-no begin_ctrl-no ~
end_ctrl-no begin_r-date end_r-date begin_due-date end_due-date rd_sort ~
select-mat rd_show tb_po tb_fg tb_rm rd-dest td-show-parm fi_file ~
tb_OpenCSV tbAutoClose lbl_sort lbl_show 

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
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn_SelectColumns 
    LABEL "Select Columns" 
    SIZE 40 BY 1.48.

DEFINE VARIABLE begin_ctrl-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Vendor Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_due-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-date  AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-i-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_r-date   AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ctrl-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Venor Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Due Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-date    AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-i-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 999999 
    LABEL "Ending PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_r-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-pofgh1.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1
    FGCOLOR 0 .

DEFINE VARIABLE lbl_show       AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS TEXT 
    SIZE 7 BY .62 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS TEXT 
    SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE mat-types      AS CHARACTER FORMAT "X(256)":U 
    LABEL "Material Types" 
    VIEW-AS FILL-IN 
    SIZE .6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_show        AS CHARACTER INITIAL "Open" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "All PO's", "All PO's"
    SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Customer#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Name", "Name"
    SIZE 37 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 15.14.

DEFINE VARIABLE select-mat   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 28 BY 4.95 NO-UNDO.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 29 BY 1.43 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 22.2 BY 1.29 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_fg        AS LOGICAL   INITIAL YES 
    LABEL "Print FGs?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_po        AS LOGICAL   INITIAL NO 
    LABEL "Print POs Without History?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rm        AS LOGICAL   INITIAL YES 
    LABEL "Print RMs?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV  AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 14 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    end_po-no AT ROW 2.19 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending PO Number"
    begin_po-no AT ROW 2.24 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning PO Number"
    begin_po-date AT ROW 3.29 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning PO Date"
    end_po-date AT ROW 3.29 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending PO Date"
    begin_vend AT ROW 4.38 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 4.38 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_po-i-no AT ROW 5.48 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_po-i-no AT ROW 5.48 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_ctrl-no AT ROW 6.57 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning Control Number"
    end_ctrl-no AT ROW 6.57 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Control Number"
    begin_r-date AT ROW 7.67 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning PO Date"
    end_r-date AT ROW 7.67 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending PO Date"
    begin_due-date AT ROW 8.76 COL 28.6 COLON-ALIGNED HELP
    "Enter Beginning Due Date"
    end_due-date AT ROW 8.76 COL 69.6 COLON-ALIGNED HELP
    "Enter Ending Due Date"
    rd_sort AT ROW 10.52 COL 14 NO-LABELS
    select-mat AT ROW 11.19 COL 64 NO-LABELS
    rd_show AT ROW 11.67 COL 14 NO-LABELS
    tb_po AT ROW 12.62 COL 14
    mat-types AT ROW 12.86 COL 61 COLON-ALIGNED
    tb_fg AT ROW 13.48 COL 14
    tb_rm AT ROW 14.38 COL 14
    btn_SelectColumns AT ROW 17.05 COL 30 WIDGET-ID 10
    sl_avail AT ROW 19.43 COL 38.6 NO-LABELS WIDGET-ID 26
    rd-dest AT ROW 19.62 COL 6.4 NO-LABELS
    sl_selected AT ROW 19.67 COL 39.6 NO-LABELS WIDGET-ID 28
    tb_excel AT ROW 19.91 COL 57.6 RIGHT-ALIGNED
    lv-ornt AT ROW 20.1 COL 33.6 NO-LABELS
    lv-font-name AT ROW 20.14 COL 29.6 COLON-ALIGNED NO-LABELS
    lv-font-no AT ROW 20.14 COL 39.6 COLON-ALIGNED
    lines-per-page AT ROW 20.14 COL 55.6 COLON-ALIGNED
    td-show-parm AT ROW 21.57 COL 37
    fi_file AT ROW 22.43 COL 27.2 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22.52 COL 85.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 24.52 COL 31.2 WIDGET-ID 16
    btn-ok AT ROW 25.48 COL 31.4
    btn-cancel AT ROW 25.48 COL 51.6
    lbl_sort AT ROW 10.71 COL 2.4 COLON-ALIGNED NO-LABELS
    lbl_show AT ROW 11.86 COL 5 COLON-ALIGNED NO-LABELS
    "Select/Deselect RM Types" VIEW-AS TEXT
    SIZE 26 BY .81 AT ROW 10.38 COL 64.6
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 18.62 COL 4.2
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 15 
    RECT-6 AT ROW 18.95 COL 3
    RECT-7 AT ROW 1.57 COL 3.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 94.8 BY 26.14
    BGCOLOR 15 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "PO RM/FG History by Customer"
        HEIGHT             = 26.19
        WIDTH              = 94.8
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
    begin_ctrl-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_r-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_ctrl-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_r-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_show:PRIVATE-DATA IN FRAME FRAME-A = "rd_show".

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

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    mat-types:HIDDEN IN FRAME FRAME-A       = TRUE
    mat-types:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_show:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    select-mat:AUTO-RESIZE IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_po:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_rm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* PO RM/FG History by Customer */
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
ON WINDOW-CLOSE OF C-Win /* PO RM/FG History by Customer */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ctrl-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ctrl-no C-Win
ON LEAVE OF begin_ctrl-no IN FRAME FRAME-A /* Beginning Vendor Item# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date C-Win
ON LEAVE OF begin_po-date IN FRAME FRAME-A /* Beginning PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_r-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_r-date C-Win
ON LEAVE OF begin_r-date IN FRAME FRAME-A /* Beginning Receipt Date */
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
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DEFINE VARIABLE v-valid AS LOG NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
        IF rd-dest = 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
        RUN GetSelectionList.
        RUN run-report(OUTPUT v-valid).
        STATUS DEFAULT "Processing Complete".

        IF v-valid THEN
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
                            {custom/asimail.i &TYPE = "CUSTOMER"
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
        SESSION:SET-WAIT-STATE ("").
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
    DO:
        DEFINE VARIABLE cTextSelected AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTextListed   AS CHARACTER NO-UNDO.

        RUN displaySelectionList2.

        ASSIGN 
            cTextSelected = sl_selected:LIST-ITEMS
            cTextListed   = sl_avail:LIST-ITEMS.

        IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

        ASSIGN 
            sl_selected:LIST-ITEMS = cTextSelected
            sl_avail:LIST-ITEMS    = cTextListed.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ctrl-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ctrl-no C-Win
ON LEAVE OF end_ctrl-no IN FRAME FRAME-A /* Ending Venor Item# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME end_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-date C-Win
ON LEAVE OF end_po-date IN FRAME FRAME-A /* Ending PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-i-no C-Win
ON LEAVE OF end_po-i-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_r-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_r-date C-Win
ON LEAVE OF end_r-date IN FRAME FRAME-A /* Ending Receipt Date */
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


&Scoped-define SELF-NAME lbl_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lbl_show C-Win
ON LEAVE OF lbl_show IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lbl_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lbl_sort C-Win
ON LEAVE OF lbl_sort IN FRAME FRAME-A
    DO:
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


&Scoped-define SELF-NAME lv-font-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-name C-Win
ON LEAVE OF lv-font-name IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE        = ENTRY(1,char-val)
                LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON LEAVE OF lv-font-no IN FRAME FRAME-A /* Font */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt C-Win
ON LEAVE OF lv-ornt IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
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
        RUN pChangeDest.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
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


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
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
        DEF VAR cSelectedList AS CHARACTER NO-UNDO.
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


&Scoped-define SELF-NAME tb_fg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg C-Win
ON VALUE-CHANGED OF tb_fg IN FRAME FRAME-A /* Print FGs? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_po C-Win
ON VALUE-CHANGED OF tb_po IN FRAME FRAME-A /* Print POs Without History? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rm C-Win
ON VALUE-CHANGED OF tb_rm IN FRAME FRAME-A /* Print RMs? */
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
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.
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
        begin_po-date = DATE(1,1,YEAR(TODAY))
        end_po-date   = TODAY.

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    btn_SelectColumns:load-image("Graphics/32x32/selectColumns.png").
    RUN enable_UI.

    FOR EACH mat:
        v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:list-items = v-mat-list.

    DO i = 1 TO select-mat:num-items:
        IF TRIM(substr(select-mat:entry(i),1,5)) EQ "B" THEN 
        DO:
            select-mat:screen-value = ENTRY(i,v-mat-list).
            LEAVE.
        END.
    END.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR9" }
    ASSIGN
        td-show-parm:sensitive = lShowParameters
        td-show-parm:hidden    = NOT lShowParameters
        td-show-parm:visible   = lShowParameters
        .
    
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_po-no.
    END.
    
    RUN pChangeDest .

    cColumnInit   = NO .
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report C-Win 
PROCEDURE create-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-i-no LIKE po-ordl.i-no.
    DEFINE INPUT PARAMETER ip-uom  LIKE job-mat.qty-uom.

    CREATE tt-report.
    ASSIGN
        tt-report.term-id = ""
        tt-report.key-01  = IF AVAILABLE cust THEN 
                         ((IF v-sort EQ "N" THEN cust.name ELSE "          ") +
                          cust.cust-no)
                       ELSE ""
        tt-report.key-02  = STRING(po-ord.po-no,"9999999999")
        tt-report.key-03  = ip-i-no
        tt-report.key-04  = IF po-ordl.item-type THEN "" ELSE
                      /* IF rd_show-2 BEGINS "FG" THEN  ip-i-no ELSE */
                       IF AVAILABLE oe-ordl THEN oe-ordl.part-no ELSE itemfg.part-no 
        tt-report.key-07  = ip-uom
        tt-report.key-09  = IF AVAILABLE cust THEN cust.cust-no ELSE ""
        tt-report.rec-id  = RECID(po-ordl).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault C-Win 
PROCEDURE DisplaySelectionDefault :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

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

    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.

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
    DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
 
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
    DISPLAY end_po-no begin_po-no begin_po-date end_po-date begin_vend end_vend 
        begin_po-i-no end_po-i-no begin_ctrl-no end_ctrl-no begin_r-date 
        end_r-date begin_due-date end_due-date rd_sort select-mat rd_show 
        tb_po tb_fg tb_rm rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose 
        lbl_sort lbl_show 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 end_po-no begin_po-no begin_po-date end_po-date 
        begin_vend end_vend begin_po-i-no end_po-i-no begin_ctrl-no 
        end_ctrl-no begin_r-date end_r-date begin_due-date end_due-date 
        rd_sort select-mat rd_show tb_po tb_fg tb_rm btn_SelectColumns rd-dest 
        td-show-parm fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ po/rep/sh-pur.p 05/98 JLF */
    /* PO FG/RM History                                                           */
    /* -------------------------------------------------------------------------- */

    DEFINE OUTPUT PARAMETER op-valid AS LOG INIT TRUE NO-UNDO.
/*{sys/form/r-topw.f}*/

&SCOPED-DEFINE where-rm                                                        ~
        WHERE rm-rcpth.company    EQ po-ordl.company                           ~
          AND rm-rcpth.i-no       EQ po-ordl.i-no                              ~
          AND rm-rcpth.po-no      EQ TRIM(STRING(po-ordl.po-no,">>>>>9"))      ~
          AND rm-rcpth.job-no     EQ po-ordl.job-no                            ~
          AND rm-rcpth.job-no2    EQ po-ordl.job-no2                           ~
          AND (rm-rcpth.rita-code EQ "R" OR                                    ~
               rm-rcpth.rita-code EQ "A")                                      ~
          AND rm-rcpth.trans-date GE begin_r-date                              ~
          AND rm-rcpth.trans-date LE end_r-date

&SCOPED-DEFINE where-fg                                                        ~
        WHERE fg-rcpth.company    EQ po-ordl.company                           ~
          AND fg-rcpth.i-no       EQ po-ordl.i-no                              ~
          AND fg-rcpth.po-no      EQ TRIM(STRING(po-ordl.po-no,">>>>>9"))      ~
          AND fg-rcpth.job-no     EQ po-ordl.job-no                            ~
          AND fg-rcpth.job-no2    EQ po-ordl.job-no2                           ~
          AND (fg-rcpth.rita-code EQ "R" OR                                    ~
               fg-rcpth.rita-code EQ "A")                                      ~
          AND fg-rcpth.trans-date GE begin_r-date                              ~
          AND fg-rcpth.trans-date LE end_r-date

    DEFINE BUFFER xjob-mat FOR job-mat.

    DEFINE VARIABLE v-foot-rem     LIKE ap-invl.amt-msf NO-UNDO.
    DEFINE VARIABLE v-msf-cal      AS LOG       NO-UNDO.

    DEFINE VARIABLE v-mattype-list AS CHARACTER FORMAT "x(36)".
    DEFINE VARIABLE v-mat-dscr     AS CHARACTER FORMAT "x(20)" EXTENT 21.

    DEFINE VARIABLE v-balance      LIKE rm-rdtlh.qty.
    DEFINE VARIABLE v-cons-qty     AS INTEGER   FORMAT "->,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-cons-qty2    AS INTEGER   FORMAT "->,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-trns-date    AS CHARACTER NO-UNDO .

    DEFINE VARIABLE v-first        LIKE tt-report.key-02 EXTENT 4.
    DEFINE VARIABLE v-ord          LIKE rm-rdtlh.qty EXTENT 4.
    DEFINE VARIABLE v-qty          LIKE rm-rdtlh.qty EXTENT 4.
    DEFINE VARIABLE v-bal          LIKE rm-rdtlh.qty EXTENT 4.

    /*def var str-tit4                like str-tit3.*/
    DEFINE VARIABLE v-cust-no      LIKE cust.cust-no.

    DEFINE VARIABLE str_buffa      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str_buffb      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-comma        AS CHARACTER FORMAT "x" INITIAL "," NO-UNDO.
    DEFINE VARIABLE lv-label       AS CHARACTER EXTENT 20 NO-UNDO.
    DEFINE VARIABLE v-hdr          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-fg-rdtlhqty  AS INTEGER   FORMAT "->,>>>,>>9" NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-type      AS CHARACTER NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    /*form header str-tit4 format "x(130)" with frame r-top.*/

    FORM cust.cust-no           COLUMN-LABEL "Customer"
        po-ord.po-no           COLUMN-LABEL "PO #"
        vend.name              COLUMN-LABEL "Vendor"
        FORMAT "x(23)"
        tt-report.key-04 
        FORMAT "x(15)"
        po-ordl.i-name            COLUMN-LABEL "Item Name"
        FORMAT "x(25)"
        po-ordl.s-wid          COLUMN-LABEL "Width"
        po-ordl.s-len          COLUMN-LABEL "Length"
        po-ordl.cost           COLUMN-LABEL "PO Cost"
        FORMAT ">>>>>9.99<<"
        po-ordl.pr-uom         COLUMN-LABEL "UOM"
        rm-rcpth.trans-date    COLUMN-LABEL "Due!Rcpt Dt"
        FORMAT "99/99/9999"
        v-fg-rdtlhqty          COLUMN-LABEL "Recpt Qty"

        WITH FRAME main NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 136.

    FORM cust.cust-no           COLUMN-LABEL "Customer"
        po-ord.po-no           COLUMN-LABEL "PO #"
        vend.name              COLUMN-LABEL "Vendor"
        FORMAT "x(23)"
        tt-report.key-04 
        FORMAT "x(15)"
        po-ordl.i-name            COLUMN-LABEL "Item Name"
        FORMAT "x(25)"
        po-ordl.s-wid          COLUMN-LABEL "Width"
        po-ordl.s-len          COLUMN-LABEL "Length"
        po-ordl.cost           COLUMN-LABEL "PO Cost"
        FORMAT ">>>>>9.99<<"
        po-ordl.pr-uom         COLUMN-LABEL "UOM"
        rm-rcpth.trans-date    COLUMN-LABEL "Due!Rcpt Dt"
        FORMAT "99/99/9999"
        v-fg-rdtlhqty          COLUMN-LABEL "Recpt Qty"

        WITH FRAME main-2 NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 136.

    FORM cust.cust-no           COLUMN-LABEL "Customer"
        po-ord.po-no           COLUMN-LABEL "PO #"
        vend.name              COLUMN-LABEL "Vendor"
        FORMAT "x(23)"
        tt-report.key-04 
        FORMAT "x(15)"
        po-ordl.i-name            COLUMN-LABEL "Item Name"
        FORMAT "x(25)"
        po-ordl.s-wid          COLUMN-LABEL "Width"
        po-ordl.s-len          COLUMN-LABEL "Length"
        po-ordl.cost           COLUMN-LABEL "PO Cost"
        FORMAT ">>>>>9.99<<"
        po-ordl.pr-uom         COLUMN-LABEL "UOM"
        po-ordl.due-date    COLUMN-LABEL "Due Dt"
        rm-rcpth.trans-date    COLUMN-LABEL "Rcpt Dt"
        FORMAT "99/99/9999"
        v-fg-rdtlhqty          COLUMN-LABEL "Recpt Qty"

        WITH FRAME main2 NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 146.

    FORM cust.cust-no           COLUMN-LABEL "Customer"
        po-ord.po-no           COLUMN-LABEL "PO #"
        vend.name              COLUMN-LABEL "Vendor"
        FORMAT "x(23)"
        tt-report.key-04 
        FORMAT "x(15)"
        po-ordl.i-name            COLUMN-LABEL "Item Name"
        FORMAT "x(25)"
        po-ordl.s-wid          COLUMN-LABEL "Width"
        po-ordl.s-len          COLUMN-LABEL "Length"
        po-ordl.cost           COLUMN-LABEL "PO Cost"
        FORMAT ">>>>>9.99<<"
        po-ordl.pr-uom         COLUMN-LABEL "UOM"
        po-ordl.due-date    COLUMN-LABEL "Due Dt"
        rm-rcpth.trans-date    COLUMN-LABEL "Rcpt Dt"
        FORMAT "99/99/9999"
        v-fg-rdtlhqty          COLUMN-LABEL "Recpt Qty"

        WITH FRAME main2-2 NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 146.

    {ce/msfcalc.i} 

    ASSIGN
        str-tit2       = c-win:TITLE + " - PR9"
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-pono       = begin_po-no
        v-e-pono       = END_po-no
        v-s-date       = begin_po-date
        v-e-date       = end_po-date
        v-s-vend       = begin_vend
        v-e-vend       = end_vend
        v-s-item       = begin_po-i-no
        v-e-item       = end_po-i-no
        v-s-vitm       = begin_ctrl-no
        v-e-vitm       = END_ctrl-no
        v-sdue-date    = begin_due-date
        v-edue-date    = end_due-date
        v-sort         = SUBSTR(rd_sort,1,1)
        v-stat         = SUBSTR(rd_show,1,1)
        v-type         = IF tb_fg THEN
                 IF tb_rm THEN "B" ELSE "F"
               ELSE
                 IF tb_rm THEN "R" ELSE ""
        v-mattype-list = "".

    DO WITH FRAME {&frame-name}:          
        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mattype-list = v-mattype-list + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.

        IF LENGTH(TRIM(v-mattype-list)) EQ 0 THEN
        DO:
            MESSAGE "No Material Type Selected."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            op-valid = NO.
            LEAVE.
        END.

        IF substr(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) EQ "," THEN
            substr(v-mattype-list,LENGTH(TRIM(v-mattype-list)),1) = "".

        mat-types = v-mattype-list.

        DO i = 1 TO LENGTH(mat-types):
            IF substr(mat-types,i,1) EQ "," THEN substr(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.
    END.


    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
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

        IF LOOKUP(ttRptSelected.TextList, "Customer,PO#,Vendor,Vendor Name,Cust Part#,FG Item#,Item Name,Width,Length," +
            "PO Cost,UOM,Due Date,Recpt Date,Recpt Qty") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.
/*
DO WITH FRAME main:

 /*  tt-report.key-04:LABEL = rd_show-2. */

   IF TB_separate-dates = NO THEN
      v-hdr = "Customer,PO#,Vendor," + TRIM(tt-report.key-04:LABEL) +
              ",Item Name,Width,Length,PO Cost,UOM,Recpt Date,Recpt Qty".
   ELSE
      v-hdr = "Customer,PO#,Vendor," + TRIM(tt-report.key-04:LABEL) +
              ",Item Name,Width,Length,PO Cost,UOM,Due Date,Recpt Date,Recpt Qty".
END. */

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY WITH FRAME r-top.

    {porep/r-pofgh1N.i}

IF rd-dest = 3 THEN DO:
   OUTPUT STREAM s-temp CLOSE.
   IF tb_OpenCSV THEN
      OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&frame-name}:HANDLE.
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-pofgh1.csv".
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

