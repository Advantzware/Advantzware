&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-stajob.w

  Description: Finished Goods Inventory Status by Job

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000
  Modified: 03/17/2016 WFK
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
DEFINE VARIABLE list-name   AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir    AS CHARACTER NO-UNDO.

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

/*{sys/inc/custlistform.i ""IL8"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL   NO-UNDO.

DEFINE VARIABLE v-sales-rep      AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form   AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file      AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tt-oe-rel NO-UNDO
    FIELD rel-no   AS INTEGER 
    FIELD rel-date AS CHARACTER
    FIELD tot-qty  AS DECIMAL
    FIELD bl-ank   AS CHARACTER 
    FIELD link-no  AS INTEGER
    FIELD po-no    AS CHARACTER
    FIELD lot-no   AS CHARACTER
    FIELD qty      AS DECIMAL .

DEFINE BUFFER ref-lot-no FOR reftable.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.


ASSIGN 
    cTextListToSelect  = "CUSTOMER,PO #,SREP,ITEM #,CUST PART #,DESCRIPTION,JOB," +
                           "REL#,REL DATE,RFQ#,QTY ALLOCATED,QTY ON HAND,QTY ORDERED," +
                           "QTY SHIPPED,RECEIPT DATE,PRICE,VALUE,RELEASE QTY," +
                           "QTY PROD.,QTY BALANCE,ORDER DATE,SHIP DATE,WHSE," +
                           "RELEASE PO#,RELEASE LOT#,FG LOT#,SHIPTO,SHIPTO NAME,FG LOT QTY," + 
                           "FACTORY COST/M,TOT FACTORY COST,ON HAND COST,ORDER PRICE"
    cFieldListToSelect = "cust.cust-no,oe-ordl.po-no,sman,oe-ordl.i-no,oe-ordl.part-no,oe-ordl.i-name,v-job-no," +
                                "v-rel#,v-relDate,v-rfq,v-relQty,v-qty-onh,v-qty-ord," +
                                "li-ship-qty,v-rctDate,v-price,v-ext,relqty," +
                                "qty-pro,qty-bal,ord-date,ship-date,loc," +
                                "relpo,rellot,fg-lot,shipto,shipname,prodqty," +
                                "fac-costm,tot-fac-cost,on-hand-cost,ord-price"
    cFieldLength       = "8,15,4,15,15,15,9," + "9,11,10,13,11,11," + "11,12,11,15,11," + "10,11,10,9,5," +
                          "15,15,16,8,30,11," + "14,16,13,11"
    cFieldType         = "c,c,c,c,c,c,c," + "c,c,c,i,i,i," + "i,c,i,i,i," + "i,i,c,c,c," +
                        "c,c,c,c,c,i," + "i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "CUSTOMER,PO #,SREP,ITEM #,"
                         + "DESCRIPTION,WHSE,QTY ORDERED,QTY SHIPPED,REL#,REL DATE,RELEASE QTY,"
                         + "QTY ON HAND,ORDER PRICE,VALUE".


{sys/inc/oereordr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-8 tb_cust-list btnCustList ~
begin_cust end_cust begin_cust-po end_cust-po begin_slm end_slm ~
begin_job-no begin_job-no2 end_job-no end_job-no2 rd_itm-code tb_inactive ~
rd_ostat rd_smry-dtl tb_sort tb_inc-zero tb_inc-cust sl_avail sl_selected ~
Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_cust-po end_cust-po begin_slm end_slm begin_job-no begin_job-no2 ~
end_job-no end_job-no2 lbl_itm-code rd_itm-code tb_inactive lbl_ostat ~
rd_ostat lbl_print rd_smry-dtl tb_sort tb_inc-zero tb_inc-cust sl_avail ~
sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
    LABEL "&Cancel" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 16 BY 1.29.

DEFINE BUTTON btnCustList 
    LABEL "Preview" 
    SIZE 9.8 BY .81.

DEFINE BUTTON Btn_Add 
    LABEL "&Add >>" 
    SIZE 16 BY 1.

DEFINE BUTTON Btn_Def 
    LABEL "&Default" 
    SIZE 16 BY 1.

DEFINE BUTTON btn_down 
    LABEL "Move Down" 
    SIZE 16 BY 1.

DEFINE BUTTON Btn_Remove 
    LABEL "<< &Remove" 
    SIZE 16 BY 1.

DEFINE BUTTON btn_Up 
    LABEL "Move Up" 
    SIZE 16 BY 1.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1.

DEFINE VARIABLE begin_cust-po  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Customer PO#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-99":U INITIAL "00" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm      AS CHARACTER FORMAT "XXX":U 
    LABEL "Beginning Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust-po    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Customer PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm        AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
    LABEL "Ending Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\FGInventoryStatusByJob.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_itm-code   AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_ostat      AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_print      AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS FILL-IN 
    SIZE 7.6 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

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
    SIZE 16.4 BY 4.29 NO-UNDO.

DEFINE VARIABLE rd_itm-code    AS CHARACTER INITIAL "Both" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Stocked", "Stocked",
    "Custom", "Custom",
    "Both", "Both"
    SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE rd_ostat       AS CHARACTER INITIAL "Both" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "Both", "Both"
    SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE rd_smry-dtl    AS CHARACTER INITIAL "S" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Summary", "S",
    "Detail", "D"
    SIZE 30 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 5.24.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 93 BY 11.91.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_inactive  AS LOGICAL   INITIAL NO 
    LABEL "Included Inactive Items" 
    VIEW-AS TOGGLE-BOX
    SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inc-cust  AS LOGICAL   INITIAL NO 
    LABEL "Include Customer Owned Warehouse?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inc-zero  AS LOGICAL   INITIAL NO 
    LABEL "Include Zero Quantity On Hand?" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_sort      AS LOGICAL   INITIAL NO 
    LABEL "Sort By Part#?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_cust-list AT ROW 1.86 COL 31 WIDGET-ID 6
    btnCustList AT ROW 1.91 COL 73.8 WIDGET-ID 8
    begin_cust AT ROW 2.91 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 2.91 COL 72 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_cust-po AT ROW 3.86 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Customer PO Number"
    end_cust-po AT ROW 3.86 COL 72 COLON-ALIGNED HELP
    "Enter Ending Customer PO Number"
    begin_slm AT ROW 4.81 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slm AT ROW 4.81 COL 72 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    begin_job-no AT ROW 5.76 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 46
    begin_job-no2 AT ROW 5.76 COL 42 COLON-ALIGNED HELP
    "Enter Beginning Job Number" WIDGET-ID 48
    end_job-no AT ROW 5.76 COL 72 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 50
    end_job-no2 AT ROW 5.76 COL 84 COLON-ALIGNED HELP
    "Enter Ending Job Number" WIDGET-ID 52
    lbl_itm-code AT ROW 7.19 COL 15.4 COLON-ALIGNED NO-LABELS
    rd_itm-code AT ROW 7.19 COL 32.6 NO-LABELS
    tb_inactive AT ROW 7.24 COL 92.8 RIGHT-ALIGNED WIDGET-ID 58
    lbl_ostat AT ROW 8.14 COL 13.4 COLON-ALIGNED NO-LABELS
    rd_ostat AT ROW 8.14 COL 32.6 NO-LABELS
    lbl_print AT ROW 9.14 COL 20.8 COLON-ALIGNED NO-LABELS
    rd_smry-dtl AT ROW 9.14 COL 32.6 NO-LABELS
    tb_sort AT ROW 10.24 COL 55 RIGHT-ALIGNED
    tb_inc-zero AT ROW 11.19 COL 34
    tb_inc-cust AT ROW 12.05 COL 75 RIGHT-ALIGNED
    sl_avail AT ROW 14.14 COL 3 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 14.14 COL 64.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 14.24 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 15.33 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 16.48 COL 41 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 17.57 COL 41 WIDGET-ID 40
    btn_down AT ROW 18.67 COL 41 WIDGET-ID 42
    lv-font-no AT ROW 20.52 COL 44 COLON-ALIGNED
    lv-ornt AT ROW 20.52 COL 54 NO-LABELS
    lines-per-page AT ROW 20.52 COL 88 COLON-ALIGNED
    rd-dest AT ROW 20.76 COL 4.6 NO-LABELS
    lv-font-name AT ROW 21.71 COL 30 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 23 COL 29
    fi_file AT ROW 24 COL 27 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 24.1 COL 93.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 25.52 COL 29.2 WIDGET-ID 60
    btn-ok AT ROW 26.48 COL 29.2
    btn-cancel AT ROW 26.48 COL 53.8
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 13.43 COL 3 WIDGET-ID 38
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.1 COL 4.6
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 13.43 COL 62.2 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 19.95 COL 4.6
    RECT-6 AT ROW 20.29 COL 3
    RECT-8 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.8 BY 27.29
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
        TITLE              = "Finished Goods Inventory Status By Job"
        HEIGHT             = 27.29
        WIDTH              = 96.8
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
    begin_cust-po:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust-po:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_itm-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_itm-code:PRIVATE-DATA IN FRAME FRAME-A = "rd_itm-code".

/* SETTINGS FOR FILL-IN lbl_ostat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_ostat:PRIVATE-DATA IN FRAME FRAME-A = "rd_ostat".

/* SETTINGS FOR FILL-IN lbl_print IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_print:PRIVATE-DATA IN FRAME FRAME-A = "rd_smry-dtl".

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
    rd_itm-code:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_ostat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_smry-dtl:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inactive IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_inactive:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inc-cust IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_inc-cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_inc-zero:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sort IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    td-show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Inventory Status By Job */
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
ON WINDOW-CLOSE OF C-Win /* Finished Goods Inventory Status By Job */
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
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-po C-Win
ON LEAVE OF begin_cust-po IN FRAME FRAME-A /* Beginning Customer PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
        ASSIGN 
            {&self-name}:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM({&self-name}:SCREEN-VALUE))) +
                 TRIM({&self-name}:SCREEN-VALUE)  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
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
  
        /*task 11221301  */
        IF STRING(begin_job-no:SCREEN-VALUE) <> "" THEN
            ASSIGN begin_job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(begin_job-no:SCREEN-VALUE))) +
                 TRIM(begin_job-no:SCREEN-VALUE)  .

        IF STRING(end_job-no:SCREEN-VALUE) <> "" THEN
            ASSIGN end_job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(end_job-no:SCREEN-VALUE))) +
                 TRIM(end_job-no:SCREEN-VALUE)  . /* Task 11221301  */
        RUN GetSelectionList.
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT tb_cust-list OR  NOT AVAILABLE ttCustList THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust,
                INPUT end_cust).
        END.
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
                            "~"OK"~" to open CSV file?"
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
                    {custom/asifax.i &begin_cust=END_cust 
                            &END_cust=END_cust 
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust 
                             &END_cust=END_cust 
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust 
                                  &END_cust=END_cust 
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
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


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
    DO:
        RUN CustList.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
    DO:
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

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
        DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-po C-Win
ON LEAVE OF end_cust-po IN FRAME FRAME-A /* Ending Customer PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
        ASSIGN 
            {&self-name}:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM({&self-name}:SCREEN-VALUE))) +
                 TRIM({&self-name}:SCREEN-VALUE)  .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
DO:
   DEF VAR ls-filename AS CHARACTER NO-UNDO.
   DEF VAR ll-ok AS LOG NO-UNDO.

   SYSTEM-DIALOG GET-FILE ls-filename 
                 TITLE "Select File to Save "
                 FILTERS "Excel Files    (*.csv)" "*.csv",
                         "All Files    (*.*) " "*.*"
                 INITIAL-DIR "c:\tmp"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN SELF:SCREEN-VALUE = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
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


&Scoped-define SELF-NAME rd_itm-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_itm-code C-Win
ON VALUE-CHANGED OF rd_itm-code IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_ostat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ostat C-Win
ON VALUE-CHANGED OF rd_ostat IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
    DO:
        ASSIGN {&self-name}.
        EMPTY TEMP-TABLE ttCustList.
        RUN SetCustRange(INPUT tb_cust-list).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inactive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inactive C-Win
ON VALUE-CHANGED OF tb_inactive IN FRAME FRAME-A /* Included Inactive Items */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-cust C-Win
ON VALUE-CHANGED OF tb_inc-cust IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-zero C-Win
ON VALUE-CHANGED OF tb_inc-zero IN FRAME FRAME-A /* Include Zero Quantity On Hand? */
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


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort By Part#? */
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
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "IL8" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "IL8",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_cust.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'IL8',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""IL8""}

    IF ou-log THEN 
    DO:
        ASSIGN 
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
            tb_cust-list                                     = YES 
            .
        RUN SetCustRange(INPUT tb_cust-list).
    END.
    ELSE
        ASSIGN
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            .

    IF ou-log AND ou-cust-int = 0 THEN 
    DO:
        ASSIGN 
            tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME}    = YES
            btnCustList:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
            tb_cust-list                                     = NO
            .
        RUN SetCustRange(tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "YES").
    END.
    RUN pChangeDest.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCustList C-Win 
PROCEDURE BuildCustList :
    /*------------------------------------------------------------------------------
      Purpose:     Builds the temp table of customers   
      Parameters:  Company Code, Customer list logical and/or customer range
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-cust FOR cust.

    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.

    IF iplList THEN 
    DO:
        RUN sys/ref/CustList.p (INPUT ipcCompany,
            INPUT 'IL8',
            INPUT YES,
            OUTPUT lActive).
    END.
    ELSE 
    DO:
        FOR EACH bf-cust
            WHERE bf-cust.company EQ ipcCompany
            AND bf-cust.cust-no GE ipcBeginCust
            AND bf-cust.cust-no LE ipcEndCust
            NO-LOCK:
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bf-cust.cust-no
                ttCustList.log-fld = YES
                .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustList C-Win 
PROCEDURE CustList :
    /*------------------------------------------------------------------------------
      Purpose:  Display a UI of selected customers   
      Parameters:  
      Notes:       
    ------------------------------------------------------------------------------*/

    RUN sys/ref/CustListManager.w(INPUT cocode,
        INPUT 'IL8').


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
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
    END.
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
    DISPLAY tb_cust-list begin_cust end_cust begin_cust-po end_cust-po begin_slm 
        end_slm begin_job-no begin_job-no2 end_job-no end_job-no2 lbl_itm-code 
        rd_itm-code tb_inactive lbl_ostat rd_ostat lbl_print rd_smry-dtl 
        tb_sort tb_inc-zero tb_inc-cust sl_avail sl_selected rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-8 tb_cust-list btnCustList begin_cust end_cust 
        begin_cust-po end_cust-po begin_slm end_slm begin_job-no begin_job-no2 
        end_job-no end_job-no2 rd_itm-code tb_inactive rd_ostat rd_smry-dtl 
        tb_sort tb_inc-zero tb_inc-cust sl_avail sl_selected Btn_Def Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose 
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
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
    /* ------------------------------------------------ fg/rep/fg-xstat.p 3/94 RM */
    /* finished goods inventory status by customer report                         */
    /* -------------------------------------------------------------------------- */
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
    DEFINE VARIABLE v-row-id       AS ROWID     NO-UNDO.
    {sys/form/r-top5DL.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DEFINE VARIABLE v-sortby         AS LOG       FORMAT "Y/N" INIT "N".
    DEFINE VARIABLE v-job-no         AS CHARACTER FORMAT "x(9)".
    DEFINE VARIABLE v-ext            AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE fcst             AS ch        INIT " ".
    DEFINE VARIABLE tcst             LIKE fcst INIT "zzzzzzzzz".
    DEFINE VARIABLE fpo#             AS ch        INIT " ".
    DEFINE VARIABLE tpo#             LIKE fpo# INIT "zzzzzzzzz".
    DEFINE VARIABLE typex            AS ch        FORMAT "!" INIT "A".
    DEFINE VARIABLE fslm             LIKE cust.sman INIT " ".
    DEFINE VARIABLE tslm             LIKE cust.sman INIT "zzz".
    DEFINE VARIABLE zbal             AS LOG       FORMAT "Y/N".
    DEFINE VARIABLE v-rec-dat        AS LOG       FORMAT "Y/N" INIT NO.
    DEFINE VARIABLE v-prt-cpn        LIKE v-rec-dat INIT YES.
    DEFINE VARIABLE v-qty-onh        AS DECIMAL   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-qty-allo       AS DECIMAL   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-frst           AS LOG.
    DEFINE VARIABLE v-frst-ord       AS LOG.
    DEFINE VARIABLE v-tot-ord        AS DECIMAL   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-tot-allo       AS DECIMAL   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-tot-ship       AS DECIMAL   FORMAT "->>,>>>,>>9".
    DEFINE VARIABLE v-tot-onh        AS DECIMAL   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-tot-ext        AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-grand-tot-ord  AS DECIMAL   FORMAT "->>,>>>,>>9".
    DEFINE VARIABLE v-grand-tot-allo AS DECIMAL   FORMAT "->>,>>>,>>9".
    DEFINE VARIABLE v-grand-tot-ship AS DECIMAL   FORMAT "->>,>>>,>>9".
    DEFINE VARIABLE v-grand-tot-onh  AS DECIMAL   FORMAT "->>,>>>,>>9".
    DEFINE VARIABLE v-grand-tot-ext  AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-custown        AS LOG       FORMAT "Y/N" INIT "N".
    DEFINE VARIABLE v-frst-i-no      AS LOG.
    DEFINE VARIABLE v-print          AS LOG.
    DEFINE VARIABLE trans-date       LIKE fg-rcpts.trans-date.
    DEFINE VARIABLE ship-date        LIKE fg-rcpts.trans-date .
    DEFINE VARIABLE qty-prod         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-job            AS CHARACTER FORMAT "x(9)".
    DEFINE VARIABLE v-rec-found      AS LOG.
    DEFINE VARIABLE v-qty-job        LIKE v-qty-onh.
    DEFINE VARIABLE v-ext-job        LIKE v-ext.
    DEFINE BUFFER xbin  FOR fg-bin.
    DEFINE BUFFER xbin2 FOR fg-bin.
    DEFINE VARIABLE v-qty-ord        AS INTEGER.
    DEFINE VARIABLE v-int-rel        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-qty-ship       AS INTEGER.
    DEFINE VARIABLE v-disp-item      AS LOG.
    DEFINE VARIABLE v-ocb            AS CHARACTER FORMAT "x" INIT "B".
    DEFINE VARIABLE li-inv-qty       LIKE oe-ordl.inv-qty NO-UNDO.
    DEFINE VARIABLE li-ship-qty      LIKE oe-ordl.ship-qty NO-UNDO.
    DEFINE VARIABLE v-rel-no         LIKE oe-rel.rel-no NO-UNDO.
    DEFINE VARIABLE v-sched-qty      LIKE oe-rel.tot-qty NO-UNDO.
    DEFINE VARIABLE v-rel-date       LIKE oe-rel.rel-date NO-UNDO.
    DEFINE VARIABLE v-smry-dtl       AS CHARACTER FORMAT "x(9)".
    DEFINE VARIABLE v-rfq            LIKE quotehd.rfq NO-UNDO.
    DEFINE VARIABLE v-summ-temp      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vmat-cost        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vmach-cost       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vtot-costm       AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE vtot-job-cost    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lIncludeInactive AS LOGICAL   NO-UNDO.

    DEFINE BUFFER boe-ordl FOR oe-ordl.
    DEFINE BUFFER bcust    FOR cust.

    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    IF rd_smry-dtl = "S" THEN 
    DO:
        FORM
            cust.cust-no LABEL "CUSTOMER"
            oe-ordl.po-no LABEL "PO #"
            /*    cust.sman label "SMAN"*/
            oe-ordl.i-no  LABEL "ITEM #"
            oe-ordl.part-no LABEL "CUST PART #" FORMAT "x(15)"
            oe-ordl.i-name LABEL "DESCRIPTION" FORMAT "x(15)"

            /********    NEED JOB-NUMBER HERE!!!!! *************/
            v-job-no LABEL "JOB #" FORMAT "x(9)"

            /*    fg-bin.loc label "WHSE"*/
            oe-ordl.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QUANTITY! ORDERED"
            li-ship-qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QUANTITY! SHIPPED"
            v-qty-onh  COLUMN-LABEL "QUANTITY! ON HAND"
            oe-ordl.price FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx1 NO-BOX DOWN STREAM-IO WIDTH 150.

        FORM
            cust.cust-no LABEL "CUSTOMER"
            itemfg.cust-po-no LABEL "PO #" FORMAT "x(15)"
            cust.sman LABEL "SMAN"
            itemfg.i-no  LABEL "ITEM #"
            itemfg.part-no LABEL "CUST PART #" FORMAT "x(15)"
            itemfg.i-name LABEL "DESCRIPTION" FORMAT "x(15)"
            v-job COLUMN-LABEL "  JOB"
            v-qty-job  COLUMN-LABEL "QUANTITY! ON HAND"
            trans-date COLUMN-LABEL "RECEIPT!DATE"
            itemfg.sell-price FORMAT ">>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext-job FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx2 NO-BOX DOWN STREAM-IO WIDTH 150.

        FORM
            cust.cust-no LABEL "CUSTOMER"
            oe-ordl.po-no LABEL "PO #"
            cust.sman LABEL "SMAN"
            oe-ordl.i-no  LABEL "ITEM #"
            oe-ordl.i-name LABEL "DESCRIPTION" FORMAT "x(15)"
            fg-bin.loc LABEL "WHSE"
            oe-ordl.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QUANTITY! ORDERED"
            li-ship-qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QUANTITY! SHIPPED"
            v-qty-onh  COLUMN-LABEL "QUANTITY! ON HAND"
            oe-ordl.price FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx3 NO-BOX DOWN STREAM-IO WIDTH 132.

        FORM
            cust.cust-no LABEL "CUSTOMER"
            itemfg.cust-po-no LABEL "PO #"
            cust.sman LABEL "SMAN"
            itemfg.i-no  LABEL "ITEM #"
            itemfg.i-name LABEL "DESCRIPTION" FORMAT "x(15)"
            v-job COLUMN-LABEL "  JOB"
            v-qty-job  COLUMN-LABEL "QUANTITY! ON HAND"
            trans-date COLUMN-LABEL "RECEIPT!DATE"
            itemfg.sell-price FORMAT ">>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext-job FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx4 NO-BOX DOWN STREAM-IO WIDTH 132.
    END.
    ELSE 
    DO: 
        FORM
            cust.cust-no LABEL "CUSTOMER"
            oe-ordl.po-no LABEL "PO #"
            /*    cust.sman label "SMAN"*/
            oe-ordl.i-no  LABEL "ITEM #"
            oe-ordl.part-no LABEL "CUST PART #" FORMAT "x(15)"
            oe-ordl.i-name LABEL "DESCRIPTION" FORMAT "x(15)"

            /********    NEED JOB-NUMBER HERE!!!!! *************/
            v-job-no COLUMN-LABEL "JOB#!REL#" FORMAT "x(9)"

            /*    fg-bin.loc label "WHSE"*/
            oe-ordl.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY ORDERED!REL DATE"
            li-ship-qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY SHIPPED!SCHED REL QTY"
            v-qty-onh  COLUMN-LABEL "QUANTITY! ON HAND"
            oe-ordl.price FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx5 NO-BOX DOWN STREAM-IO WIDTH 160.
        FORM
            cust.cust-no LABEL "CUSTOMER"
            itemfg.cust-po-no LABEL "PO #" FORMAT "x(15)"
            cust.sman LABEL "SMAN"
            itemfg.i-no  LABEL "ITEM #"
            itemfg.part-no LABEL "CUST PART #" FORMAT "x(15)"
            itemfg.i-name LABEL "DESCRIPTION" FORMAT "x(15)"
            v-job COLUMN-LABEL "JOB!REL#"
            oe-ordl.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY ORDERED!REL DATE"
            li-ship-qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY SHIPPED!SCHED REL QTY"
            v-qty-job  COLUMN-LABEL "QTY ON HAND"
            trans-date COLUMN-LABEL "RECEIPT DATE"
            itemfg.sell-price FORMAT ">>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext-job FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx6 NO-BOX DOWN STREAM-IO WIDTH 190.

        FORM
            cust.cust-no LABEL "CUSTOMER"
            oe-ordl.po-no LABEL "PO #"
            cust.sman LABEL "SMAN"
            oe-ordl.i-no  LABEL "ITEM #"
            oe-ordl.i-name LABEL "DESCRIPTION" FORMAT "x(15)"
            fg-bin.loc COLUMN-LABEL "WHSE!REL#"
            oe-ordl.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY ORDERED!REL DATE"
            li-ship-qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY SHIPPED!SCHED REL QTY"
            v-qty-onh  COLUMN-LABEL "QTY ON HAND"
            oe-ordl.price FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx7 NO-BOX DOWN STREAM-IO WIDTH 150.

        FORM
            cust.cust-no LABEL "CUSTOMER"
            itemfg.cust-po-no LABEL "PO #"
            cust.sman LABEL "SMAN"
            itemfg.i-no  LABEL "ITEM #"
            itemfg.i-name LABEL "DESCRIPTION" FORMAT "x(15)"
            v-job COLUMN-LABEL "JOB!REL#"
            oe-ordl.qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY ORDERED!REL DATE"
            li-ship-qty FORMAT "->,>>>,>>9" COLUMN-LABEL "QTY SHIPPED!SCHED REL QTY"
            v-qty-job  COLUMN-LABEL "QTY ON HAND"
            trans-date COLUMN-LABEL "RECEIPT DATE"
            itemfg.sell-price FORMAT ">>>,>>9.99" COLUMN-LABEL "SELLING! PRICE"
            v-ext-job FORMAT "->>>,>>>,>>9.99"  COLUMN-LABEL "TOTAL!VALUE"
            WITH FRAME itemx8 NO-BOX DOWN STREAM-IO WIDTH 190.
    END.
    ASSIGN
        str-tit2         = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fcst             = begin_cust
        tcst             = end_cust
        fpo#             = begin_cust-po
        tpo#             = end_cust-po
        fslm             = begin_slm
        tslm             = END_slm 
        typex            = SUBSTR(rd_itm-code,1,1)
        v-ocb            = SUBSTR(rd_ostat,1,1)
        v-smry-dtl       = substr(rd_smry-dtl,1,1)
        v-sortby         = tb_sort
        zbal             = tb_inc-zero
        v-custown        = tb_inc-cust
        v-rec-dat        = NO /*tb_rcpt-date*/
        v-prt-cpn        = NO /*tb_part*/
        lIncludeInactive = tb_Inactive .

    IF typex EQ "B" THEN typex = "A".

    /*FIND FIRST fg-bin NO-LOCK NO-ERROR.*/

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

        IF LOOKUP(ttRptSelected.TextList, "QTY ALLOCATED,QTY ON HAND,QTY ORDERED,QTY SHIPPED,VALUE") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    {fg/rep/fg-xstatN.i}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetCustRange C-Win 
PROCEDURE SetCustRange :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            begin_cust:SENSITIVE  = NOT iplChecked
            end_cust:SENSITIVE    = NOT iplChecked
            begin_cust:VISIBLE    = NOT iplChecked
            end_cust:VISIBLE      = NOT iplChecked
            btnCustList:SENSITIVE = iplChecked
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
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
            fi_file:SCREEN-VALUE = "c:\tmp\FGInventoryStatusByJob.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

