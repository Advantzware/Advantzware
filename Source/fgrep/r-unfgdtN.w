&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-unfgdt.w

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

/*{sys/inc/custlistform.i ""IL3"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report.
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "Customer,Item Description,Ship To,Item #," +
                           "Cust PO #,Qty Ord,Inv Date,Inv #,Qty Ship," +
                           "Issued BOL No,Issued BOL Qty,Available" 
    cFieldListToSelect = "cust,i-name,ship-to,i-no," +
                            "cust-po,qty-ord,inv-dt,inv,qty-shp," + 
                            "bol-no,bol-qty,avl"
    cFieldLength       = "30,30,30,15," + "15,10,10,6,10," + "13,14,10"
    cFieldType         = "c,c,c,c," + "c,i,c,c,i," + "i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Customer,Item Description,Ship To,Item #," +
                           "Cust PO #,Qty Ord,Inv Date,Inv #,Qty Ship," +
                           "Issued BOL No,Issued BOL Qty,Available" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_ord-stat begin_ord-date ~
end_ord-date begin_ord-no end_ord-no tb_cust-list btnCustList begin_cust ~
end_cust begin_ship end_ship begin_i-no end_i-no begin_whse end_whse ~
rd_sort sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_ord-stat rd_ord-stat begin_ord-date ~
end_ord-date begin_ord-no end_ord-no tb_cust-list begin_cust end_cust ~
begin_ship end_ship begin_i-no end_i-no begin_whse end_whse lbl_sort ~
rd_sort sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE BUTTON btnCustList 
    LABEL "Preview" 
    SIZE 9.8 BY .81.

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

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Order Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ship     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Ship-To#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_whse     AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Order Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no     AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 
    LABEL "Ending Order#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ship       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Ship-To#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_whse       AS CHARACTER FORMAT "X(5)" INITIAL "zzz" 
    LABEL "Ending Warehouse" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\UnshippedFGDetail.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_ord-stat   AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?" 
    VIEW-AS FILL-IN 
    SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 93 
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
    SIZE 16 BY 4.1 NO-UNDO.

DEFINE VARIABLE rd_ord-stat    AS CHARACTER INITIAL "Open" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "All", "All"
    SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Customer#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Item#", "Item#"
    SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 4.76.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 10.48.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 5.95 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 34 BY 5.95 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    lbl_ord-stat AT ROW 1.86 COL 26 COLON-ALIGNED NO-LABELS
    rd_ord-stat AT ROW 1.86 COL 44 NO-LABELS
    begin_ord-date AT ROW 3.1 COL 26 COLON-ALIGNED
    end_ord-date AT ROW 3.1 COL 69 COLON-ALIGNED HELP
    "Enter Ending Order Date"
    begin_ord-no AT ROW 4.05 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_ord-no AT ROW 4.05 COL 69 COLON-ALIGNED HELP
    "Enter Ending Order Number"
    tb_cust-list AT ROW 5.19 COL 28.2 WIDGET-ID 6
    btnCustList AT ROW 5.24 COL 70.8 WIDGET-ID 8
    begin_cust AT ROW 6.33 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 6.33 COL 69 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_ship AT ROW 7.29 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Ship-To Number"
    end_ship AT ROW 7.29 COL 69 COLON-ALIGNED HELP
    "Enter Ending Ship-To Number"
    begin_i-no AT ROW 8.24 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 8.24 COL 69 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_whse AT ROW 9.19 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Warehouse"
    end_whse AT ROW 9.19 COL 69 COLON-ALIGNED HELP
    "Enter Ending Warehouse"
    lbl_sort AT ROW 10.62 COL 26 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 10.62 COL 35.4 NO-LABELS
    sl_avail AT ROW 12.86 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 12.91 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 12.91 COL 60.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 14.05 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 15.24 COL 40.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 16.43 COL 40.8 WIDGET-ID 40
    btn_down AT ROW 17.62 COL 40.8 WIDGET-ID 42
    lv-font-no AT ROW 19.57 COL 33 COLON-ALIGNED
    lv-ornt AT ROW 19.57 COL 44 NO-LABELS
    lines-per-page AT ROW 19.57 COL 88 COLON-ALIGNED
    rd-dest AT ROW 19.76 COL 5 NO-LABELS
    lv-font-name AT ROW 20.76 COL 30 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 21.81 COL 29
    fi_file AT ROW 22.76 COL 27 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22.86 COL 93.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 24.33 COL 29 WIDGET-ID 16
    btn-ok AT ROW 25.43 COL 29.2
    btn-cancel AT ROW 25.43 COL 54.6
    "Available Columns" VIEW-AS TEXT
    SIZE 20 BY .71 AT ROW 12.05 COL 3 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 12.1 COL 61.2 WIDGET-ID 44
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4.4
    BGCOLOR 15 
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 19 COL 4.4
    RECT-6 AT ROW 19.33 COL 3
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96.2 BY 26.14
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
        TITLE              = "Unshipped Finished Goods Detail"
        HEIGHT             = 26.14
        WIDTH              = 96.2
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
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_ship:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ord-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_ship:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_whse:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_ord-stat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_ord-stat:PRIVATE-DATA IN FRAME FRAME-A = "rd_ord-stat".

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
    rd_ord-stat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Unshipped Finished Goods Detail */
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
ON WINDOW-CLOSE OF C-Win /* Unshipped Finished Goods Detail */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
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


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* Beginning Order Date */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ship C-Win
ON LEAVE OF begin_ship IN FRAME FRAME-A /* Beginning Ship-To# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whse C-Win
ON LEAVE OF begin_whse IN FRAME FRAME-A /* Beginning Warehouse */
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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
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


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* Ending Order Date */
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
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ship C-Win
ON LEAVE OF end_ship IN FRAME FRAME-A /* Ending Ship-To# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whse C-Win
ON LEAVE OF end_whse IN FRAME FRAME-A /* Ending Warehouse */
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


&Scoped-define SELF-NAME rd_ord-stat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ord-stat C-Win
ON VALUE-CHANGED OF rd_ord-stat IN FRAME FRAME-A
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
        begin_ord-date = DATE(01,01,YEAR(TODAY))
        END_ord-date   = DATE(12,31,YEAR(TODAY)). 
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "IL3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "IL3",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO lbl_ord-stat.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'IL3',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""IL3""}

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
            INPUT 'IL3',
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-report C-Win 
PROCEDURE create-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-loc LIKE oe-boll.loc.


    FOR EACH oe-ordl OF oe-ord
        WHERE oe-ordl.i-no    GE begin_i-no
        AND oe-ordl.i-no    LE end_i-no
        NO-LOCK,

        EACH oe-rel
        WHERE oe-rel.company EQ cocode
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.ship-id GE begin_ship
        AND oe-rel.ship-id LE end_ship
        NO-LOCK,

        FIRST shipto OF oe-rel NO-LOCK:

        {custom/statusMsg.i " 'Processing FG Item  '  + oe-ordl.i-no"}

        {fg/rep/fg-unsum.i}

        IF v-loc GE begin_whse AND v-loc LE end_whse THEN 
        DO:
            CREATE tt-report.
            ASSIGN
                tt-report.rec-id = RECID(oe-rel)
                tt-report.key-01 = IF rd_sort EQ "Customer#" THEN oe-ord.cust-no
                                                     ELSE oe-ordl.i-no
                tt-report.key-02 = IF rd_sort EQ "Customer#" THEN oe-rel.ship-id
                                                     ELSE oe-ord.cust-no
                tt-report.key-03 = IF rd_sort EQ "Customer#" THEN oe-ordl.i-no
                                                     ELSE oe-rel.ship-id
                tt-report.key-08 = oe-ord.cust-no
                tt-report.key-09 = oe-rel.ship-id.
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
        INPUT 'IL3').


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
    DISPLAY lbl_ord-stat rd_ord-stat begin_ord-date end_ord-date begin_ord-no 
        end_ord-no tb_cust-list begin_cust end_cust begin_ship end_ship 
        begin_i-no end_i-no begin_whse end_whse lbl_sort rd_sort sl_avail 
        sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 rd_ord-stat begin_ord-date end_ord-date begin_ord-no 
        end_ord-no tb_cust-list btnCustList begin_cust end_cust begin_ship 
        end_ship begin_i-no end_i-no begin_whse end_whse rd_sort sl_avail 
        Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file 
        tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
            fi_file:SCREEN-VALUE = "c:\tmp\UnshippedFGDetail.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------- fg/rep/fg-undet.p 05/98 JLF */
    /* Detail of Unshipped Orders                                                 */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-ford         LIKE oe-ord.ord-no FORMAT ">>>>>>".
    DEFINE VARIABLE v-tord         LIKE v-ford INIT 999999.
    DEFINE VARIABLE v-fdate        AS DATE      FORMAT "99/99/9999" INIT 01/01/0001.
    DEFINE VARIABLE v-tdate        LIKE v-fdate INIT 12/31/9999.
    DEFINE VARIABLE v-fcust        LIKE oe-ord.cust-no INIT "".
    DEFINE VARIABLE v-tcust        LIKE v-fcust INIT "zzzzzzzz".
    DEFINE VARIABLE v-fship        LIKE oe-rel.ship-id INIT "".
    DEFINE VARIABLE v-tship        LIKE v-fship INIT "zzzzzzzz".
    DEFINE VARIABLE v-fitem        LIKE itemfg.i-no INIT "".
    DEFINE VARIABLE v-titem        LIKE v-fitem INIT "zzzzzzzzzzzzzzz".
    DEFINE VARIABLE v-floc         LIKE oe-rell.loc INIT "".
    DEFINE VARIABLE v-tloc         LIKE v-floc INIT "zzzzz".
    DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "O".
    DEFINE VARIABLE v-cust-sort    AS LOG       FORMAT "Cust/Item" INIT YES.

    DEFINE VARIABLE v-ono          AS INTEGER   FORMAT "->>>>>>>>9" EXTENT 4.
    DEFINE VARIABLE v-shp          LIKE v-ono.
    DEFINE VARIABLE v-bol          LIKE v-ono.

    DEFINE VARIABLE v-bal          AS INTEGER   FORMAT "->>>>>>>>9".

    DEFINE VARIABLE v-rel          AS LOG.
    DEFINE VARIABLE v-first        LIKE tt-report.key-02 EXTENT 3.
    DEFINE VARIABLE v-loc          LIKE oe-boll.loc.

    DEFINE VARIABLE v-label1       AS CHARACTER FORMAT "x(24)" EXTENT 2.
    DEFINE VARIABLE v-label2       AS CHARACTER FORMAT "x(21)" EXTENT 2.

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

&Scoped-define where-phrase where oe-ord.company  eq cocode  ~
                              and oe-ord.ord-no   le v-tord  ~
                              and oe-ord.ord-date ge v-fdate ~
                              and oe-ord.ord-date le v-tdate ~
                              and oe-ord.cust-no  EQ ttCustList.cust-no ~
                              and oe-ord.stat     ne "D"
    /* 
    form header
         str-tit4 format "x(130)"
         skip(1)
    
         v-label1[1]
         v-label2[1]
         "               "
         "     Qty  "
         "        "
         "      "
         "     Qty  "
         "     Issued BOL    "
         /* "  Qty Not " skip */
         skip
    
         v-label1[2]
         v-label2[2]
         "Cust PO #      "
         "   Ordered"
         "Inv Date"
         " Inv #"
         "   Shipped"
         "  Number        Qty"
         /* " Scheduled" skip */
         " Available" skip
    
         "------------------------"
         "---------------------"
         "---------------"
         "----------"
         "--------"
         "------"
         "----------"
         "-------------------"
         "----------" SKIP
    
       with frame r-top. */

    ASSIGN
        str-tit2    = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-stat      = SUBSTR(rd_ord-stat,1,1) 
        v-fdate     = begin_ord-date
        v-tdate     = end_ord-date
        v-ford      = begin_ord-no
        v-tord      = end_ord-no
        v-fcust     = begin_cust
        v-tcust     = end_cust
        v-fship     = begin_ship
        v-tship     = end_ship
        v-fitem     = begin_i-no
        v-titem     = end_i-no
        v-floc      = begin_whse
        v-tloc      = end_whse
        v-cust-sort = rd_sort EQ "Customer#" .

    /*str-tit3 = (if v-stat eq "O" then "Open"   else
                    if v-stat eq "C" then "Closed" else "All") + " Orders" + "  " +
                   "Order #:" + string(v-ford) + " - " + string(v-tord)    + "  " +
                   "Order Date:" + string(v-fdate) + " - " +
                                   string(v-tdate)                         + "  " +
                   "Cust #:" + v-fcust + " - " + v-tcust
        x = (132 - length(str-tit3)) / 2
        str-tit3 = fill(" ",x) + str-tit3
        str-tit4 = "Ship To #:" + v-fship + " - " + v-tship                + "  " +
                   "Item #:" + v-fitem + " - " + v-titem                   + "  " +
                   "Whse:"   + v-floc  + " - " + v-tloc
        x = (132 - length(str-tit4)) / 2
        str-tit4 = fill(" ",x) + str-tit4. */


    IF v-cust-sort THEN
        ASSIGN
            v-label1[1] = "Customer                "
            v-label2[1] = "Ship To              "
            v-label1[2] = "Item Description        "
            v-label2[2] = "Item #               ".

    ELSE
        ASSIGN
            v-label1[1] = "Item Description        "
            v-label2[1] = "Item #               "
            v-label1[2] = "Customer                "
            v-label2[2] = "Ship To              ".

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.


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

        IF LOOKUP(ttRptSelected.TextList, "Qty Ord,Qty Ship,Issued BOL Qty,Available") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*  ASSIGN
           excelheader = IF v-cust-sort THEN "Customer/Item Description,Ship To/Item #,"
                         ELSE "Item Description/Customer,Item #/Ship To,"
           excelheader = excelheader + "Cust PO #,Qty Ordered,"
                       + "Inv Date,Inv #,Qty Shipped,Issued BOL Number,Issued BOL Qty,"
                       + "Available". */

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    SESSION:SET-WAIT-STATE("general").

    DISPLAY WITH FRAME r-top.

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    IF v-stat EQ "O" THEN
        FOR EACH ttCustList 
            WHERE ttCustList.log-fld
            NO-LOCK,
            EACH oe-ord
            {&where-phrase}
          AND oe-ord.opened EQ YES
        USE-INDEX opened NO-LOCK:

    RUN create-report. 
END.

    ELSE
    IF v-stat EQ "C" THEN
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
        EACH oe-ord
        {&where-phrase}
          AND oe-ord.opened EQ NO
        USE-INDEX opened NO-LOCK:

RUN create-report. 
END.

    ELSE
    FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK,
      EACH oe-ord {&where-phrase} NO-LOCK:
RUN create-report. 
END.

FOR EACH tt-report WHERE tt-report.term-id EQ "",

    FIRST cust
    WHERE cust.company EQ cocode
    AND cust.cust-no EQ tt-report.key-08
    NO-LOCK,

    FIRST shipto
    WHERE shipto.company EQ cocode
    AND shipto.cust-no EQ tt-report.key-08
    AND shipto.ship-id EQ tt-report.key-09
    NO-LOCK

    BREAK BY tt-report.key-01
    BY tt-report.key-02
    BY tt-report.key-03:

    IF FIRST-OF(tt-report.key-01) THEN v-first[1] = tt-report.key-01.
    IF FIRST-OF(tt-report.key-02) THEN v-first[2] = tt-report.key-02.
    IF FIRST-OF(tt-report.key-03) THEN v-first[3] = tt-report.key-03.

    RELEASE oe-rell.

    FIND oe-rel WHERE RECID(oe-rel) EQ tt-report.rec-id NO-LOCK.

    FIND FIRST oe-ordl OF oe-rel NO-LOCK.

    {custom/statusMsg.i " 'Processing FG Item/Cust#  '  + oe-ordl.i-no + '/' +  cust.cust-no "}

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.

    FIND FIRST oe-ord OF oe-ordl NO-LOCK.

    /*  if v-cust-sort then do:
        if first-of(tt-report.key-01) or first-of(tt-report.key-02) then do:
          display cust.name               format "x(24)"
                                          when first-of(tt-report.key-01)
                  shipto.ship-name        format "x(21)"

              with frame detail no-labels no-box no-attr-space down STREAM-IO width 132.

          down with frame detail.
        end.

        if first-of(tt-report.key-03) then
          display itemfg.i-name when avail itemfg @ cust.name
                  oe-ordl.i-no                    @ shipto.ship-name

              with frame detail.
      end.

      else do:
        if first-of(tt-report.key-01) then do:
          display itemfg.i-name when avail itemfg @ cust.name
                  oe-ordl.i-no                    @ shipto.ship-name

              with frame detail.

          down with frame detail.
        end.

        if first-of(tt-report.key-02) or first-of(tt-report.key-03) then
          display cust.name               when first-of(tt-report.key-02)
                  shipto.ship-name

              with frame detail.
      end. */

    {fg/rep/fg-unsum.i}

    ASSIGN
        v-ono[3] = v-ono[3] + oe-rel.qty
        v-bal    = oe-rel.qty.

    IF AVAILABLE ar-invl THEN
        ASSIGN
            v-shp[3] = v-shp[3] + ar-invl.ship-qty
            v-bal    = v-bal    - ar-invl.ship-qty.

    ELSE
        IF AVAILABLE oe-rell        AND
            AVAILABLE oe-bolh        AND
            (NOT oe-bolh.posted) THEN
            ASSIGN
                v-bol[3] = v-bol[3] + oe-rell.qty
                v-bal    = v-bal    - oe-rell.qty.

    IF v-bal LT 0 THEN v-bal = 0.

    /*  display oe-rel.po-no
              oe-rel.qty                format "->>>>>>>>9"
              ar-inv.inv-date           FORMAT "99/99/99"
                                        when avail ar-inv and avail ar-invl
              ar-inv.inv-no             when avail ar-inv and avail ar-invl
              ar-invl.ship-qty          format "->>>>>>>>9"
                                        when avail ar-invl
              oe-bolh.bol-no            when avail oe-bolh
                                         and (not oe-bolh.posted)
              oe-rell.qty               format "->>>>>>>>9"
                                        when avail oe-rell
                                         and avail oe-bolh
                                         and (not oe-bolh.posted)
              v-bal

          with frame detail.
      down with frame detail. */

    /*  IF rd-dest = 3 THEN DO:
        PUT STREAM excel UNFORMATTED
            '"' (IF v-cust-sort THEN cust.name
                 ELSE itemfg.i-name)                 '",'
            '"' (IF v-cust-sort THEN shipto.ship-name
                ELSE oe-ordl.i-no)                   '",'
            SKIP.

        PUT STREAM excel UNFORMATTED
            '"' (IF v-cust-sort THEN itemfg.i-name
                 ELSE cust.NAME)                           '",'
            '"' (IF v-cust-sort THEN oe-ordl.i-no 
                ELSE shipto.ship-name)                     '",'
            '"' oe-rel.po-no                               '",'
            '"' STRING(oe-rel.qty,"->>>>>>>>9")            '",'
            '"' (IF AVAIL ar-inv AND AVAIL ar-invl AND
                 ar-inv.inv-date <> ? THEN STRING(ar-inv.inv-date)
                 ELSE "")                                  '",'
            '"' (IF AVAIL ar-inv AND AVAIL ar-invl THEN
                 STRING(ar-inv.inv-no) ELSE "")            '",'
            '"' (IF avail ar-invl THEN STRING(ar-invl.ship-qty,"->>>>>>>>9")
                 ELSE "")                                  '",'
            '"' (IF avail oe-bolh and (not oe-bolh.posted) THEN
                 STRING(oe-bolh.bol-no) ELSE "")                   '",'
            '"' (IF avail oe-rell and avail oe-bolh AND
                 (not oe-bolh.posted) THEN STRING(oe-rell.qty,"->>>>>>>>9")
                 ELSE "")                                  '",'
            '"'  STRING(v-bal,"->>>>>>>>9")                '",'
            SKIP.
        END. */

    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:    
            WHEN "cust"     THEN 
                cVarValue = cust.NAME .
            WHEN "i-name"   THEN 
                cvarValue = itemfg.i-name.
            WHEN "ship-to"  THEN 
                cVarValue = shipto.ship-name .
            WHEN "i-no"     THEN 
                cVarValue = oe-ordl.i-no .
            WHEN "cust-po"  THEN 
                cVarValue = oe-rel.po-no.
            WHEN "qty-ord"  THEN 
                cVarValue = STRING(oe-rel.qty,"->>>>>>>>9").
            WHEN "inv-dt"   THEN 
                cvarValue = IF AVAILABLE ar-inv AND AVAILABLE ar-invl AND ar-inv.inv-date <> ? THEN STRING(ar-inv.inv-date) ELSE "".
            WHEN "inv"      THEN 
                cVarValue = IF AVAILABLE ar-inv AND AVAILABLE ar-invl THEN STRING(ar-inv.inv-no) ELSE "".
            WHEN "qty-shp"  THEN 
                cVarValue = IF AVAILABLE ar-invl THEN STRING(ar-invl.ship-qty,"->>>>>>>>9") ELSE "".
            WHEN "bol-no"   THEN 
                cvarValue = IF AVAILABLE oe-bolh AND (NOT oe-bolh.posted) THEN STRING(oe-bolh.bol-no) ELSE "".
            WHEN "bol-qty"  THEN 
                cVarValue = IF AVAILABLE oe-rell AND AVAILABLE oe-bolh AND (NOT oe-bolh.posted) THEN STRING(oe-rell.qty,"->>>>>>>>>>>>9") ELSE "".
            WHEN "avl"      THEN 
                cVarValue = STRING(v-bal,"->>>>>>>>9") .

        END CASE.
        
        IF cTmpField = "inv-dt"   THEN 
           cExcelVarValue = IF AVAILABLE ar-inv AND AVAILABLE ar-invl AND ar-inv.inv-date <> ? THEN DYNAMIC-FUNCTION("sfFormat_Date",ar-inv.inv-date) ELSE "".
                
        ELSE cExcelVarValue = cVarValue.
        
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
        cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",". 
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            cExcelDisplay SKIP.
    END.

    IF LAST-OF(tt-report.key-03) THEN 
    DO:
        v-bal = v-ono[3] - v-shp[3] - v-bol[3].

        IF v-bal LT 0 THEN v-bal = 0.

        CLEAR FRAME detail NO-PAUSE.

        PUT SKIP(1).

        IF tt-report.key-03 NE v-first[3] THEN 
        DO:

            /* display "         Item Totals:"     when v-cust-sort
                                                 @ shipto.ship-name
                     "      Ship To Totals:"     when (not v-cust-sort)
                                                 @ shipto.ship-name
                     v-ono[3]                    @ oe-rel.qty
                     v-shp[3]                    @ ar-invl.ship-qty
                     v-bol[3]                    @ oe-rell.qty
                     v-bal
   
                 with frame detail.
             down with frame detail. */

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:    
                    WHEN "cust"     THEN 
                        cVarValue = "" .
                    WHEN "i-name"   THEN 
                        cvarValue = "" .
                    WHEN "ship-to"  THEN 
                        cVarValue = "" .
                    WHEN "i-no"     THEN 
                        cVarValue = "" .
                    WHEN "cust-po"  THEN 
                        cVarValue = "" .
                    WHEN "qty-ord"  THEN 
                        cVarValue = STRING(v-ono[3],"->>>>>>>>9").
                    WHEN "inv-dt"   THEN 
                        cvarValue = "".
                    WHEN "inv"      THEN 
                        cVarValue = "".
                    WHEN "qty-shp"  THEN 
                        cVarValue = STRING(v-shp[3],"->>>>>>>>9") .
                    WHEN "bol-no"   THEN 
                        cvarValue = "".
                    WHEN "bol-qty"  THEN 
                        cVarValue = STRING(v-bol[3],"->>>>>>>>>>>>9").
                    WHEN "avl"      THEN 
                        cVarValue = STRING(v-bal,"->>>>>>>>9") .

                END CASE.
                cExcelVarValue = cVarValue.  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
            END.
            IF v-cust-sort THEN 
            DO:
                PUT UNFORMATTED 
                    "    Item Totals:" SUBSTRING(cDisplay,17,300) SKIP.
            END.
            ELSE IF NOT v-cust-sort THEN 
                DO:
                    PUT UNFORMATTED 
                        "    Ship To Totals:" SUBSTRING(cDisplay,20,300) SKIP.
                END.

            PUT SKIP(1).
            IF NOT LAST-OF(tt-report.key-02) THEN PUT SKIP(1).
        END.

        ASSIGN
            v-ono[2] = v-ono[2] + v-ono[3]
            v-shp[2] = v-shp[2] + v-shp[3]
            v-bol[2] = v-bol[2] + v-bol[3]

            v-ono[3] = 0
            v-shp[3] = 0
            v-bol[3] = 0.
    END.

    IF LAST-OF(tt-report.key-02) THEN 
    DO:
        v-bal = v-ono[2] - v-shp[2] - v-bol[2].

        IF v-bal LT 0 THEN v-bal = 0.

        CLEAR FRAME detail NO-PAUSE.

        IF tt-report.key-02 NE v-first[2] THEN 
        DO:

            /* display "      Ship To Totals:"     when v-cust-sort
                                                 @ shipto.ship-name
                     "     Customer Totals:"     when (not v-cust-sort)
                                                 @ shipto.ship-name
                     v-ono[2]                    @ oe-rel.qty
                     v-shp[2]                    @ ar-invl.ship-qty
                     v-bol[2]                    @ oe-rell.qty
                     v-bal
   
                 with frame detail.
             down with frame detail. */

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:    
                    WHEN "cust"     THEN 
                        cVarValue = "" .
                    WHEN "i-name"   THEN 
                        cvarValue = "" .
                    WHEN "ship-to"  THEN 
                        cVarValue = "" .
                    WHEN "i-no"     THEN 
                        cVarValue = "" .
                    WHEN "cust-po"  THEN 
                        cVarValue = "" .
                    WHEN "qty-ord"  THEN 
                        cVarValue = STRING(v-ono[2],"->>>>>>>>9").
                    WHEN "inv-dt"   THEN 
                        cvarValue = "".
                    WHEN "inv"      THEN 
                        cVarValue = "".
                    WHEN "qty-shp"  THEN 
                        cVarValue = STRING(v-shp[2],"->>>>>>>>9") .
                    WHEN "bol-no"   THEN 
                        cvarValue = "".
                    WHEN "bol-qty"  THEN 
                        cVarValue = STRING(v-bol[2],"->>>>>>>>>>>>9").
                    WHEN "avl"      THEN 
                        cVarValue = STRING(v-bal,"->>>>>>>>9") .

                END CASE.
                cExcelVarValue = cVarValue.  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
            END.
            IF v-cust-sort THEN 
            DO:
                PUT UNFORMATTED 
                    "    Ship To Totals:" SUBSTRING(cDisplay,20,300) SKIP.
            END.
            ELSE IF NOT v-cust-sort THEN 
                DO:
                    PUT UNFORMATTED 
                        "    Customer Totals:" SUBSTRING(cDisplay,21,300) SKIP.
                END.

            PUT SKIP(1).
            IF NOT LAST-OF(tt-report.key-01) THEN PUT SKIP(1).
        END.

        ASSIGN
            v-ono[1] = v-ono[1] + v-ono[2]
            v-shp[1] = v-shp[1] + v-shp[2]
            v-bol[1] = v-bol[1] + v-bol[2]

            v-ono[2] = 0
            v-shp[2] = 0
            v-bol[2] = 0.
    END.

    IF LAST-OF(tt-report.key-01) THEN 
    DO:
        v-bal = v-ono[1] - v-shp[1] - v-bol[1].

        IF v-bal LT 0 THEN v-bal = 0.

        CLEAR FRAME detail NO-PAUSE.

        IF tt-report.key-01 NE v-first[1] THEN 
        DO:

            /*display "     Customer Totals:"     when v-cust-sort
                                                @ shipto.ship-name
                    "         Item Totals:"     when (not v-cust-sort)
                                                @ shipto.ship-name
                    v-ono[1]                    @ oe-rel.qty
                    v-shp[1]                    @ ar-invl.ship-qty
                    v-bol[1]                    @ oe-rell.qty
                    v-bal
  
                with frame detail.
            down with frame detail. */

            PUT SKIP str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:    
                    WHEN "cust"     THEN 
                        cVarValue = "" .
                    WHEN "i-name"   THEN 
                        cvarValue = "" .
                    WHEN "ship-to"  THEN 
                        cVarValue = "" .
                    WHEN "i-no"     THEN 
                        cVarValue = "" .
                    WHEN "cust-po"  THEN 
                        cVarValue = "" .
                    WHEN "qty-ord"  THEN 
                        cVarValue = STRING(v-ono[1],"->>>>>>>>9").
                    WHEN "inv-dt"   THEN 
                        cvarValue = "".
                    WHEN "inv"      THEN 
                        cVarValue = "".
                    WHEN "qty-shp"  THEN 
                        cVarValue = STRING(v-shp[1],"->>>>>>>>9") .
                    WHEN "bol-no"   THEN 
                        cvarValue = "".
                    WHEN "bol-qty"  THEN 
                        cVarValue = STRING(v-bol[1],"->>>>>>>>>>>>9").
                    WHEN "avl"      THEN 
                        cVarValue = STRING(v-bal,"->>>>>>>>9") .

                END CASE.
                cExcelVarValue = cVarValue.  
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
            END.
            IF v-cust-sort THEN 
            DO:
                PUT UNFORMATTED 
                    "    Customer Totals:" SUBSTRING(cDisplay,21,300) SKIP.
            END.
            ELSE IF NOT v-cust-sort THEN 
                DO:
                    PUT UNFORMATTED 
                        "    Item Totals:" SUBSTRING(cDisplay,17,300) SKIP.
                END.

            PUT SKIP(2).
        END.

        ASSIGN
            v-ono[4] = v-ono[4] + v-ono[1]
            v-shp[4] = v-shp[4] + v-shp[1]
            v-bol[4] = v-bol[4] + v-bol[1]

            v-ono[1] = 0
            v-shp[1] = 0
            v-bol[1] = 0.
    END.

    IF LAST(tt-report.key-01) THEN 
    DO:
        v-bal = v-ono[4] - v-shp[4] - v-bol[4].

        IF v-bal LT 0 THEN v-bal = 0.

        CLEAR FRAME detail NO-PAUSE.

        /* display "        Grand Totals:"  @ shipto.ship-name
                 v-ono[4]                 @ oe-rel.qty
                 v-shp[4]                 @ ar-invl.ship-qty
                 v-bol[4]                 @ oe-rell.qty
                 v-bal
 
             with frame detail.
         down with frame detail. */

        PUT SKIP str-line SKIP .
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:    
                WHEN "cust"     THEN 
                    cVarValue = "" .
                WHEN "i-name"   THEN 
                    cvarValue = "" .
                WHEN "ship-to"  THEN 
                    cVarValue = "" .
                WHEN "i-no"     THEN 
                    cVarValue = "" .
                WHEN "cust-po"  THEN 
                    cVarValue = "" .
                WHEN "qty-ord"  THEN 
                    cVarValue = STRING(v-ono[4],"->>>>>>>>9").
                WHEN "inv-dt"   THEN 
                    cvarValue = "".
                WHEN "inv"      THEN 
                    cVarValue = "".
                WHEN "qty-shp"  THEN 
                    cVarValue = STRING(v-shp[4],"->>>>>>>>9") .
                WHEN "bol-no"   THEN 
                    cvarValue = "".
                WHEN "bol-qty"  THEN 
                    cVarValue = STRING(v-bol[4],"->>>>>>>>>>>>9").
                WHEN "avl"      THEN 
                    cVarValue = STRING(v-bal,"->>>>>>>>9") .

            END CASE.
            cExcelVarValue = cVarValue.  
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",". 
        END.
        PUT UNFORMATTED 
            "    Grand Totals:" SUBSTRING(cDisplay,18,300) SKIP.

    END.

    v-first = "".
END.

IF rd-dest = 3 THEN 
DO:
    OUTPUT STREAM excel CLOSE.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE("").

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

