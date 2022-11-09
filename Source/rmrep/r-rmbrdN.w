&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep/r-rmbrd.w

  Description: Job Board/Printer Report

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
DEFINE VARIABLE list-name  AS CHARACTER NO-UNDO.
DEFINE VARIABLE llWasFound AS LOGICAL   NO-UNDO.
DEFINE VARIABLE init-dir   AS CHARACTER NO-UNDO.

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

DEFINE STREAM excel.
DEFINE VARIABLE v-avgcost   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-mat-list  AS CHARACTER NO-UNDO.


DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-po NO-UNDO
    FIELD i-no    AS CHARACTER
    FIELD po-line AS CHARACTER
    FIELD COUNT   AS INTEGER
    FIELD po-no   AS INTEGER
    INDEX i-no i-no po-no.

DEFINE TEMP-TABLE tt-job NO-UNDO
    FIELD i-no          AS CHARACTER
    FIELD job-no        AS CHARACTER
    FIELD job-no2       AS INTEGER
    FIELD seq           AS INTEGER
    FIELD resource      AS CHARACTER
    FIELD print-date    AS DATE      FORMAT "99/99/9999"
    FIELD start-date-su AS DATE
    FIELD start-time-su AS INTEGER
    FIELD alloc-qty     AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD m-code        AS CHARACTER
    FIELD required-qty  AS DECIMAL   FORMAT ">>>,>>>,>>9.99"
    FIELD cust-name     AS CHARACTER
    INDEX i-no   i-no   print-date seq m-code
    INDEX m-code m-code.


DEFINE VARIABLE lv-type-dscrs AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-type-codes AS CHARACTER NO-UNDO.
RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).

DEFINE TEMP-TABLE tt-mach NO-UNDO
    FIELD m-code AS CHARACTER
    INDEX m-code m-code.

DEFINE TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin.


DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg.


ASSIGN 
    cTextListToSelect  = "Whse,Item,Item Name,Description,Product Category,UOM,Cost,On Hand,On Order," +
                           "PO - Due Date,Quantity Available,Value,Order type" 
    cFieldListToSelect = "whse,item,item-name,desc,cat,uom,cost,qty-hand,qty-ord," +
                            "po-due-date,qty-abl,val,ord-type"
    cFieldLength       = "5,10,28,30,16,3,11,15,15," + "100,18,14,10"
    cFieldType         = "c,c,c,c,c,c,i,i,i," + "c,i,i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Whse,Item,Item Name,Description,Product Category,UOM,Cost,On Hand,On Order," +
                           "PO - Due Date,Quantity Available,Value,Order type"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 as-of-date begin_date end_date ~
begin_rm-no end_rm-no begin_procat end_procat begin_mach end_mach rd_qty ~
rd_item select-mat tb_zero-bal tb_qty-ord tb_neg tb_neg-hc tb_allocated ~
btn_SelectColumns rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date begin_date end_date begin_rm-no ~
end_rm-no begin_procat end_procat begin_mach end_mach rd_qty lbl_grnd-tot ~
rd_item lbl_itm-code select-mat tb_zero-bal tb_qty-ord tb_neg tb_neg-hc ~
tb_allocated rd-dest fi_file tb_OpenCSV tbAutoClose 

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
    SIZE 35 BY 1.19.

DEFINE VARIABLE as-of-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/01 
    LABEL "Inventory as of" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/00 
    LABEL "Beginning Run Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mach     AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat   AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning  Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no    AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning RM Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/25 
    LABEL "Ending Run Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach       AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat     AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzzz" 
    LABEL "Ending Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no      AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending RM Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-rmbrd.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1.

DEFINE VARIABLE lbl_grnd-tot   AS CHARACTER FORMAT "X(256)":U INITIAL "Calculate Value Using On Hand or Available?" 
    VIEW-AS FILL-IN 
    SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_itm-code   AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
    VIEW-AS FILL-IN 
    SIZE 13 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "8" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt        AS CHARACTER INITIAL "P" 
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
    SIZE 17 BY 5.24 NO-UNDO.

DEFINE VARIABLE rd_item        AS CHARACTER INITIAL "Both" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Estimated", "Estimated",
    "Real", "Real",
    "Both", "Both"
    SIZE 33 BY .95 NO-UNDO.

DEFINE VARIABLE rd_qty         AS CHARACTER INITIAL "Available" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Available", "Available",
    "On Hand", "Oh Hand"
    SIZE 27 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 6.43.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 14.76.

DEFINE VARIABLE select-mat   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 44 BY 5 NO-UNDO.

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

DEFINE VARIABLE tb_allocated AS LOGICAL   INITIAL NO 
    LABEL "Allocated Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE tb_neg       AS LOGICAL   INITIAL NO 
    LABEL "Negative Balances (On Hand < 0)?" 
    VIEW-AS TOGGLE-BOX
    SIZE 40 BY .95 NO-UNDO.

DEFINE VARIABLE tb_neg-hc    AS LOGICAL   INITIAL NO 
    LABEL "Negative Bal (On Hand-Committed < 0)?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE tb_qty-ord   AS LOGICAL   INITIAL YES 
    LABEL "Inc Qty On Order with Qty Avail?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_zero-bal  AS LOGICAL   INITIAL NO 
    LABEL "Include Zero Balances?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    as-of-date AT ROW 1.95 COL 65.4 COLON-ALIGNED WIDGET-ID 2
    begin_date AT ROW 3 COL 27.2 COLON-ALIGNED HELP
    "Enter Beginning Run-Start-Date" WIDGET-ID 34
    end_date AT ROW 3 COL 65.2 COLON-ALIGNED HELP
    "Enter Ending Run-Start-Date" WIDGET-ID 36
    begin_rm-no AT ROW 4.05 COL 27.2 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_rm-no AT ROW 4.05 COL 65.2 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_procat AT ROW 5.1 COL 27.2 COLON-ALIGNED HELP
    "Enter Beginning Category" WIDGET-ID 4
    end_procat AT ROW 5.1 COL 65.2 COLON-ALIGNED HELP
    "Enter Ending Category" WIDGET-ID 6
    begin_mach AT ROW 6.14 COL 27.2 COLON-ALIGNED HELP
    "Enter Beginning Machine" WIDGET-ID 30
    end_mach AT ROW 6.14 COL 65.2 COLON-ALIGNED HELP
    "Enter Ending Machine" WIDGET-ID 32
    sl_avail AT ROW 6.24 COL 4 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 6.24 COL 59.4 NO-LABELS WIDGET-ID 28
    rd_qty AT ROW 6.95 COL 61 NO-LABELS WIDGET-ID 16
    lbl_grnd-tot AT ROW 7.62 COL 15 COLON-ALIGNED NO-LABELS WIDGET-ID 14
    rd_item AT ROW 8.14 COL 61 NO-LABELS WIDGET-ID 10
    lbl_itm-code AT ROW 8.81 COL 47 COLON-ALIGNED NO-LABELS WIDGET-ID 8
    select-mat AT ROW 9.52 COL 4 NO-LABELS WIDGET-ID 28
    tb_zero-bal AT ROW 9.91 COL 55 WIDGET-ID 24
    tb_qty-ord AT ROW 10.86 COL 55 WIDGET-ID 22
    tb_neg AT ROW 11.81 COL 55 WIDGET-ID 20
    tb_neg-hc AT ROW 12.76 COL 55 WIDGET-ID 40
    tb_allocated AT ROW 13.71 COL 55 WIDGET-ID 38
    btn_SelectColumns AT ROW 14.86 COL 55.4 WIDGET-ID 10
    lv-ornt AT ROW 17.24 COL 31 NO-LABELS
    lines-per-page AT ROW 17.24 COL 84 COLON-ALIGNED
    rd-dest AT ROW 17.67 COL 5 NO-LABELS
    lv-font-no AT ROW 18.14 COL 35 COLON-ALIGNED
    td-show-parm AT ROW 18.14 COL 51
    lv-font-name AT ROW 19.1 COL 29 COLON-ALIGNED NO-LABELS
    fi_file AT ROW 21.76 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 21.86 COL 90 RIGHT-ALIGNED
    tbAutoClose AT ROW 23.86 COL 31 WIDGET-ID 42
    btn-ok AT ROW 24.81 COL 31
    btn-cancel AT ROW 24.81 COL 51
    "Select/Deselect Material Types" VIEW-AS TEXT
    SIZE 37 BY .62 AT ROW 8.81 COL 4 WIDGET-ID 26
    FONT 6
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 16.67 COL 4
    RECT-6 AT ROW 16.95 COL 3
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 100 BY 25.81
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
        TITLE              = "Job Material/Machine Report"
        HEIGHT             = 25.86
        WIDTH              = 100
        MAX-HEIGHT         = 53.71
        MAX-WIDTH          = 384
        VIRTUAL-HEIGHT     = 53.71
        VIRTUAL-WIDTH      = 384
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
    as-of-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_grnd-tot IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_grnd-tot:PRIVATE-DATA IN FRAME FRAME-A = "rd_qty".

/* SETTINGS FOR FILL-IN lbl_itm-code IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_itm-code:PRIVATE-DATA IN FRAME FRAME-A = "rd_item".

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
    rd_item:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_qty:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

ASSIGN 
    tb_allocated:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_neg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_neg-hc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_qty-ord:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_zero-bal:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Job Material/Machine Report */
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
ON WINDOW-CLOSE OF C-Win /* Job Material/Machine Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* Inventory as of */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Run Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning  Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning RM Item# */
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
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
  
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
  
        SESSION:SET-WAIT-STATE("general").
        RUN GetSelectionList.
        RUN run-report. 

        STATUS DEFAULT "Processing Complete". 
        SESSION:SET-WAIT-STATE("").


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
                    {custom/asifax.i &type= 'Board List'
                            &begin_cust= begin_rm-no
                            &END_cust=begin_rm-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:

                    {custom/asimailr.i &TYPE = "Board List"
                               &begin_cust= begin_rm-no
                               &END_cust=begin_rm-no
                               &mail-subject=c-win:title
                               &mail-body=c-win:title
                               &mail-file=list-name }

                END.
        END CASE.
  
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Run Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending RM Item# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME rd_item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_item C-Win
ON VALUE-CHANGED OF rd_item IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_qty C-Win
ON VALUE-CHANGED OF rd_qty IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_allocated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_allocated C-Win
ON VALUE-CHANGED OF tb_allocated IN FRAME FRAME-A /* Allocated Only? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_neg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_neg C-Win
ON VALUE-CHANGED OF tb_neg IN FRAME FRAME-A /* Negative Balances (On Hand < 0)? */
    DO:
        ASSIGN {&self-name}.
        IF tb_neg:SCREEN-VALUE EQ "Yes" THEN
            tb_neg-hc:SCREEN-VALUE = "No " .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_neg-hc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_neg-hc C-Win
ON VALUE-CHANGED OF tb_neg-hc IN FRAME FRAME-A /* Negative Bal (On Hand-Committed < 0)? */
    DO:
        ASSIGN {&self-name}.
        IF tb_neg-hc:SCREEN-VALUE EQ "Yes" THEN
            tb_neg:SCREEN-VALUE = "No " .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_qty-ord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_qty-ord C-Win
ON VALUE-CHANGED OF tb_qty-ord IN FRAME FRAME-A /* Inc Qty On Order with Qty Avail? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero-bal C-Win
ON VALUE-CHANGED OF tb_zero-bal IN FRAME FRAME-A /* Include Zero Balances? */
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

    as-of-date = TODAY.
    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "MR$" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    FOR EACH mat:
        v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
    END.
    IF substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
        substr(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

    select-mat:list-items = v-mat-list.

    FIND FIRST rm-ctrl WHERE rm-ctrl.company = cocode NO-LOCK NO-ERROR.

    v-avgcost = NOT AVAILABLE rm-ctrl OR rm-ctrl.avg-lst-cst.

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        as-of-date:SCREEN-VALUE = STRING(TODAY).
        APPLY "entry" TO as-of-date.
    END.
    RUN pChangeDest.
    cColumnInit = NO.
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
    DISPLAY as-of-date begin_date end_date begin_rm-no end_rm-no begin_procat 
        end_procat begin_mach end_mach rd_qty lbl_grnd-tot rd_item 
        lbl_itm-code select-mat tb_zero-bal tb_qty-ord tb_neg tb_neg-hc 
        tb_allocated rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 as-of-date begin_date end_date begin_rm-no end_rm-no 
        begin_procat end_procat begin_mach end_mach rd_qty rd_item select-mat 
        tb_zero-bal tb_qty-ord tb_neg tb_neg-hc tb_allocated btn_SelectColumns 
        rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    {custom\out2file.i}.

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
    RUN custom\d-print.w (list-name).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-mkbin C-Win 
PROCEDURE rm-mkbin :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER op-dec AS DECIMAL NO-UNDO.

    DEFINE VARIABLE v-r-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-i-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-t-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-qty  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cst  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-uom  AS CHARACTER NO-UNDO.


    FOR EACH tt-rm-bin:
        DELETE tt-rm-bin.
    END.

    FIND item WHERE ROWID(item) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE item THEN 
    DO:
    {rm/rmmkbin1.i as-of-date tt-}
    END.

    FOR EACH tt-rm-bin:
        op-dec = op-dec + tt-rm-bin.qty.
        DELETE tt-rm-bin.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-board C-Win 
PROCEDURE run-board :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* end ---------------------------------- copr. 1992  advanced software, inc. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------------- rm/menurep1.p 9/92 cd */
    /*                                                                            */
    /* raw materials costs - category sub menu                                    */
    /*                                                                            */
    /* -------------------------------------------------------------------------- */

    DEFINE VARIABLE v-po-line      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-value        AS DECIMAL   DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE rm-cst-amt     AS DECIMAL   DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE v-alloc-total  AS DECIMAL   DECIMALS 2 NO-UNDO.
    DEFINE VARIABLE li-seq         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-q-onh       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty          AS DECIMAL   FORMAT "->>>>9.99" NO-UNDO.
    DEFINE VARIABLE v-av           AS LOG       FORMAT "Avail/OnHand" INIT YES NO-UNDO.
    DEFINE VARIABLE v-type         AS CHARACTER FORMAT "!" INIT "B".
    DEFINE VARIABLE v-today-365    AS DATE      NO-UNDO.
    DEFINE VARIABLE v-mtype        AS CHARACTER FORMAT "x(47)" NO-UNDO.
    DEFINE VARIABLE v-len          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep          LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt          LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-mat-act-qty  LIKE mat-act.qty NO-UNDO.
    DEFINE VARIABLE noDate         AS LOGICAL   NO-UNDO. /* rstark 08111413 */
    DEFINE VARIABLE v-ord-type     AS CHARACTER.

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
    DEFINE VARIABLE cCustName      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE dReqTotal      AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE li             AS INTEGER   NO-UNDO.

    DEFINE VARIABLE lSchedule          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSchedule          AS CHARACTER NO-UNDO.

    RUN spGetSettingByName ("Schedule", OUTPUT cSchedule).
    IF cSchedule NE "" THEN
    ASSIGN lSchedule = LOGICAL(cSchedule).

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.  

    DEFINE VARIABLE cslist      AS CHARACTER NO-UNDO.
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


        IF LOOKUP(ttRptSelected.TextList, "Quantity,Amt MSF,Discount,Amount") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    /*run run-board.*/


    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    EMPTY TEMP-TABLE tt-po.
    EMPTY TEMP-TABLE tt-job.
    EMPTY TEMP-TABLE tt-mach.

    SESSION:SET-WAIT-STATE ("general").

    DO WITH FRAME {&FRAME-NAME}:

        DO i = 1 TO select-mat:num-items:
            IF select-mat:is-selected(i) THEN
                v-mtype = v-mtype + trim(substr(select-mat:entry(i),1,5)) + ",".
        END.

        IF LENGTH(TRIM(v-mtype)) EQ 0 THEN
        DO:
            MESSAGE "No Material Type Selected."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
        END.

        IF substr(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
            substr(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".
    END.

    ASSIGN
        /* rstark 08111413 */
        noDate      = lSchedule
        str-tit2    = c-win:TITLE
        v-av        = rd_qty BEGINS "Av"
        v-type      = substr(rd_item,1,1)
        v-today-365 = TODAY - 365
        {sys/inc/ctrtext.i str-tit2 112}.

    DISPLAY "" WITH FRAME r-top.

    FOR EACH job FIELDS(job job-no job-no2 est-no) WHERE
        job.company EQ cocode AND
        job.opened EQ YES
        NO-LOCK:

        {custom/statusMsg.i "'Processing... '"} 

        FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job EQ job.job
            AND job-hdr.job-no EQ job.job-no NO-ERROR .
        IF AVAILABLE job-hdr THEN
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ cocode 
                AND cust.cust-no EQ job-hdr.cust-no NO-ERROR .
        cCustName = IF AVAILABLE job-hdr AND AVAILABLE cust THEN cust.NAME ELSE "" .

        FOR EACH job-mat WHERE
            job-mat.company EQ cocode AND
            job-mat.job EQ job.job AND
            job-mat.all-flg EQ YES AND
            job-mat.i-no >= begin_rm-no AND
            job-mat.i-no <= end_rm-no
            NO-LOCK,
            FIRST ITEM WHERE
            item.company = cocode AND
            ITEM.i-no EQ job-mat.i-no AND
            lookup(item.mat-type,v-mtype) GT 0 AND
            ITEM.procat GE begin_procat AND
            ITEM.procat LE end_procat
            NO-LOCK:

            IF NOT (item.i-code EQ v-type OR v-type EQ "B") THEN NEXT.

            CREATE tt-job.
            ASSIGN 
                tt-job.i-no         = job-mat.i-no
                tt-job.job-no       = job-mat.job-no 
                tt-job.job-no2      = job-mat.job-no2
                tt-job.alloc-qty    = job-mat.qty-all
                tt-job.required-qty = job-mat.qty 
                tt-job.cust-name    = cCustName.
            IF tt-job.required-qty EQ ? THEN tt-job.required-qty = 0 .
            IF ITEM.cons-uom NE job-mat.qty-uom THEN
            DO:
                ASSIGN
                    v-bwt = job-mat.basis-w
                    v-len = job-mat.len
                    v-wid = job-mat.wid
                    v-dep = item.s-dep.

                IF v-len EQ 0 THEN v-len = item.s-len.
                IF v-wid EQ 0 THEN v-wid = IF item.r-wid NE 0 THEN item.r-wid
                    ELSE ITEM.s-wid.
                IF v-bwt EQ 0 THEN v-bwt = item.basis-w. 

                RUN custom/convquom.p (INPUT cocode,
                    INPUT job-mat.qty-uom,
                    INPUT ITEM.cons-uom,
                    INPUT v-bwt,
                    INPUT v-len,
                    INPUT v-wid,
                    INPUT v-dep,
                    INPUT tt-job.alloc-qty,
                    OUTPUT tt-job.alloc-qty).
            END.

            FOR EACH mat-act FIELDS(qty qty-uom) WHERE
                mat-act.company EQ job-mat.company AND
                mat-act.job     EQ job-mat.job AND
                mat-act.job-no  EQ job-mat.job-no AND
                mat-act.job-no2 EQ job-mat.job-no2 AND
                mat-act.rm-i-no EQ job-mat.rm-i-no
                NO-LOCK:

                v-mat-act-qty = mat-act.qty.

                IF ITEM.cons-uom NE job-mat.qty-uom THEN
                DO:
                    ASSIGN
                        v-bwt = job-mat.basis-w
                        v-len = job-mat.len
                        v-wid = job-mat.wid
                        v-dep = item.s-dep.

                    IF v-len EQ 0 THEN v-len = item.s-len.
                    IF v-wid EQ 0 THEN v-wid = IF item.r-wid NE 0 THEN item.r-wid
                        ELSE ITEM.s-wid.
                    IF v-bwt EQ 0 THEN v-bwt = item.basis-w. 

                    RUN custom/convquom.p (INPUT cocode,
                        INPUT job-mat.qty-uom,
                        INPUT ITEM.cons-uom,
                        INPUT v-bwt,
                        INPUT v-len,
                        INPUT v-wid,
                        INPUT v-dep,
                        INPUT mat-act.qty,
                        OUTPUT v-mat-act-qty).
                END.

            /*tt-job.alloc-qty = tt-job.alloc-qty - v-mat-act-qty.*/ /* task 12031413 */
            END.

            RELEASE job-mch.

            FOR EACH job-mch WHERE
                job-mch.company EQ cocode AND
                job-mch.job EQ job.job AND
                /*                 job-mch.frm EQ job-mat.frm AND */
                job-mch.run-complete EQ NO AND 
                job-mch.m-code GE begin_mach AND
                job-mch.m-code LE end_mach AND 
                ((job-mch.start-date GE begin_date AND
                job-mch.start-date LE end_date) OR noDate)                
                /*                 NO-LOCK,                            */
                /*                 FIRST mach WHERE                    */
                /*                       mach.company EQ cocode AND    */
                /*                       mach.loc EQ locode AND        */
                /*                       mach.m-code EQ job-mch.m-code */
                NO-LOCK:

                /*                 IF NOT(mach.dept[1] EQ "PR" OR      */
                /*                    mach.dept[2] EQ "PR" OR          */
                /*                    mach.dept[3] EQ "PR" OR          */
                /*                    mach.dept[4] EQ "PR") THEN NEXT. */
                IF NOT(job-mch.blank-no EQ 0 OR job-mat.blank-no EQ 0 OR
                    job-mch.blank-no EQ job-mat.blank-no) THEN
                    NEXT.

                ASSIGN
                    tt-job.m-code        = job-mch.m-code
                    tt-job.print-date    = job-mch.start-date
                    tt-job.start-date-su = job-mch.start-date-su
                    tt-job.start-time-su = job-mch.start-time-su.

                IF NOT CAN-FIND(FIRST tt-mach WHERE
                    tt-mach.m-code EQ job-mch.m-code) THEN
                DO:
                    CREATE tt-mach.
                    ASSIGN 
                        tt-mach.m-code = job-mch.m-code.
                    RELEASE tt-mach.
                END.

                LEAVE.
            END.

            RELEASE tt-job.
        END. /*each job-mat*/
    END. /*each job*/

    FOR EACH tt-mach:

        {custom/statusMsg.i "'Processing... '"} 

        li-seq = 0.

        FOR EACH job-mch WHERE
            job-mch.company EQ cocode AND
            job-mch.m-code EQ tt-mach.m-code AND
            job-mch.run-complete EQ NO AND
            ((job-mch.start-date-su NE ? AND
            job-mch.start-time-su NE ? AND
            job-mch.start-date-su GE v-today-365) OR noDate)
            NO-LOCK,
            FIRST job WHERE
            job.company EQ cocode AND
            job.job-no EQ job-mch.job-no AND
            job.job-no2 EQ job-mch.job-no2 AND
            job.opened EQ YES
            NO-LOCK
            BREAK BY job-mch.start-date-su
            BY job-mch.start-time-su:

            llWasFound = NO.
            FOR EACH tt-job WHERE
                tt-job.job-no EQ job-mch.job-no AND
                tt-job.job-no2 EQ job-mch.job-no2 AND
                tt-job.m-code EQ job-mch.m-code AND
                tt-job.seq EQ 0:
                llWasFound = YES.
            END.

            IF llWasFound THEN 
            DO:
                /* Only increment if tt-job's exists to update */
                li-seq = li-seq + 1.

                FOR EACH tt-job WHERE
                    tt-job.job-no EQ job-mch.job-no AND
                    tt-job.job-no2 EQ job-mch.job-no2 AND
                    tt-job.m-code EQ job-mch.m-code AND
                    tt-job.seq EQ 0
                    BY tt-job.start-date-su
                    BY tt-job.start-time-su:

                    ASSIGN
                        tt-job.seq      = li-seq
                        tt-job.resource = "#" + STRING(tt-job.seq) + " "
                                    + tt-job.m-code.
                END.
            END.
        /*             FIND FIRST tt-job WHERE                                */
        /*                  tt-job.job-no EQ job-mch.job-no AND               */
        /*                  tt-job.job-no2 EQ job-mch.job-no2 AND             */
        /*                  tt-job.m-code EQ job-mch.m-code AND               */
        /*                  tt-job.seq EQ 0                                   */
        /*                  NO-ERROR.                                         */
        /*                                                                    */
        /*             IF AVAIL tt-job THEN                                   */
        /*                ASSIGN                                              */
        /*                   tt-job.seq = li-seq                              */
        /*                   tt-job.resource = "#" + STRING(tt-job.seq) + " " */
        /*                                   + tt-job.m-code.                 */
        END.

    END.

    FOR EACH ITEM WHERE
        item.company = cocode AND
        ITEM.i-no >= begin_rm-no AND
        ITEM.i-no <= end_rm-no AND
        ITEM.procat >= begin_procat AND
        ITEM.procat <= end_procat AND
        lookup(item.mat-type,v-mtype) GT 0
        NO-LOCK:

        {custom/statusMsg.i "'Processing Item # ' + string(ITEM.i-no)"} 

        IF NOT(item.i-code  EQ v-type OR v-type EQ "B") THEN NEXT.

        FOR EACH po-ordl FIELDS(po-no ord-qty due-date) WHERE
            po-ordl.company EQ cocode AND
            po-ordl.i-no EQ item.i-no AND
            po-ordl.opened EQ YES AND
            po-ordl.item-type
            NO-LOCK:

            {custom/statusMsg.i "'Processing Item # ' + string(ITEM.i-no)"}


            FIND FIRST tt-po WHERE
                tt-po.i-no EQ ITEM.i-no AND
                tt-po.COUNT LT 3
                NO-ERROR.

            IF NOT AVAILABLE tt-po THEN
            DO:
                CREATE tt-po.
                ASSIGN 
                    tt-po.i-no  = ITEM.i-no
                    tt-po.po-no = po-ordl.po-no.
            END.

            ASSIGN
                tt-po.COUNT   = tt-po.COUNT + 1
                tt-po.po-line = tt-po.po-line
                             + (IF tt-po.COUNT NE 1 THEN " " ELSE "")
                             + STRING(po-ordl.po-no)
                             + " - "
                             + STRING(po-ordl.ord-qty)
                             + " due "
                             + DYNAMIC-FUNCTION("sfFormat_Date",po-ordl.due-date)
                             + ";".
            RELEASE tt-po.
        END.

        IF item.i-code EQ "E" THEN
            ASSIGN
                v-qty      = 0
                rm-cst-amt = 0
                lv-q-onh   = 0.
        ELSE 
        DO:

            IF rm-ctrl.avg-lst-cst = TRUE THEN  
                rm-cst-amt = item.avg-cost.
            ELSE
                rm-cst-amt = item.last-cost.

            IF as-of-date EQ TODAY THEN
                lv-q-onh = item.q-onh.
            ELSE
                RUN rm-mkbin (ROWID(item), OUTPUT lv-q-onh).

            ASSIGN
                v-qty   = lv-q-onh + (IF tb_qty-ord THEN item.q-ono ELSE 0) - item.q-comm
                v-value = (IF v-av THEN v-qty ELSE lv-q-onh) *
                        (IF v-avgcost THEN item.avg-cost ELSE item.last-cost).
        END.
        FIND FIRST tt-job WHERE tt-job.i-no = ITEM.i-no
            AND tt-job.resource NE "" NO-LOCK NO-ERROR.
        IF (tb_zero-bal OR v-qty NE 0)     AND
            ((NOT tb_neg OR lv-q-onh LT 0) AND (NOT tb_neg-hc OR v-qty LT 0)) AND 
            (NOT tb_allocated OR AVAILABLE tt-job) THEN 
        DO:

            FIND FIRST tt-po WHERE
                tt-po.i-no EQ ITEM.i-no
                NO-ERROR.

            FIND FIRST procat WHERE
                procat.company EQ cocode AND
                procat.procat EQ ITEM.procat
                NO-LOCK NO-ERROR.
            IF AVAILABLE tt-job THEN
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.i-no EQ po-ordl.i-no
                    AND oe-ordl.job-no EQ tt-job.job-no
                    AND oe-ordl.type-code NE "" NO-ERROR.   
            IF NOT AVAILABLE oe-ordl AND AVAILABLE tt-job THEN
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.job-no EQ tt-job.job-no
                    AND oe-ordl.type-code NE "" NO-ERROR.
            IF AVAIL(oe-ordl) THEN 
            DO:

                li = LOOKUP(oe-ordl.type-code,lv-type-codes) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN li = 0.
                IF li GT 0 AND li LE NUM-ENTRIES(lv-type-dscrs) THEN 
                DO:
                    v-ord-type = ENTRY(li,lv-type-dscrs).
                END.
                ELSE 
                    ASSIGN v-ord-type = "".
            END.
            ELSE
                ASSIGN v-ord-type = "".

            /* display
                item.loc
                item.i-no
                item.i-name
                procat.dscr WHEN AVAIL procat
                ITEM.cons-uom when item.i-code ne "E"
                rm-cst-amt when item.i-code ne "E"
                lv-q-onh when item.i-code ne "E"
                ITEM.q-ono when item.i-code ne "E"
                tt-po.po-line WHEN AVAIL tt-po @ v-po-line
                ITEM.q-avail when item.i-code ne "E"
                v-value when item.i-code ne "E"
                with frame itemx.*/

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "whse"    THEN 
                        cVarValue = STRING(item.loc,"x(5)") .
                    WHEN "item"   THEN 
                        cVarValue = STRING(item.i-no,"x(10)").
                    WHEN "item-name"   THEN 
                        cVarValue = STRING(item.i-name,"x(28)").
                    WHEN "desc"   THEN 
                        cVarValue = STRING(item.i-dscr,"x(30)").
                    WHEN "cat"  THEN 
                        cVarValue = IF AVAILABLE procat THEN STRING(procat.dscr,"x(16)") ELSE "" .
                    WHEN "uom"   THEN 
                        cVarValue = IF item.i-code NE "E" THEN STRING(ITEM.cons-uom,"x(3)") ELSE "" .
                    WHEN "cost"  THEN 
                        cVarValue = IF item.i-code NE "E" THEN STRING(rm-cst-amt,">>,>>9.9999") ELSE "" .
                    WHEN "qty-hand"   THEN 
                        cVarValue = IF item.i-code NE "E" THEN STRING(lv-q-onh,"->>>,>>>,>>9.99") ELSE "" .
                    WHEN "qty-ord"  THEN 
                        cVarValue = IF item.i-code NE "E" THEN STRING(ITEM.q-ono,"->>>,>>>,>>9.99") ELSE "" .

                    WHEN "po-due-date"  THEN 
                        cVarValue = IF AVAILABLE tt-po THEN STRING(tt-po.po-line,"x(100)") ELSE "" .
                    WHEN "qty-abl"   THEN 
                        cVarValue = IF item.i-code NE "E" THEN STRING(ITEM.q-avail,"->,>>>,>>>,>>9.99") ELSE "" .
                    WHEN "val"  THEN 
                        cVarValue = IF item.i-code NE "E" THEN STRING(v-value,"->>,>>>,>>9.99") ELSE "" .
                    WHEN "ord-type"  THEN 
                        cVarValue = STRING(v-ord-type).


                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            IF AVAILABLE tt-po THEN
                DELETE tt-po.

            FOR EACH tt-po WHERE
                tt-po.i-no EQ ITEM.i-no:

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "whse"    THEN 
                            cVarValue = "" .
                        WHEN "item"   THEN 
                            cVarValue = "".
                        WHEN "item-name"   THEN 
                            cVarValue = "" .
                        WHEN "desc"   THEN 
                            cVarValue = "".
                        WHEN "cat"  THEN 
                            cVarValue = "" .
                        WHEN "uom"   THEN 
                            cVarValue = "" .
                        WHEN "cost"  THEN 
                            cVarValue = ""  .
                        WHEN "qty-hand"   THEN 
                            cVarValue = ""  .
                        WHEN "qty-ord"  THEN 
                            cVarValue = ""  .
                        WHEN "po-due-date"  THEN 
                            cVarValue = STRING(tt-po.po-line,"x(100)") .
                        WHEN "qty-abl"   THEN 
                            cVarValue = "".
                        WHEN "val"  THEN 
                            cVarValue = "" .
                        WHEN "ord-type"  THEN 
                            cVarValue = "".
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED cDisplay SKIP.
                IF rd-dest EQ 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
                DELETE tt-po.
            END.

            v-alloc-total = 0.
            dReqTotal = 0 .
            .
            FOR EACH tt-job WHERE
                tt-job.i-no EQ ITEM.i-no AND
                tt-job.resource NE ""
                BREAK BY tt-job.i-no
                BY tt-job.print-date
                BY tt-job.seq
                BY tt-job.m-code:

                IF FIRST(tt-job.i-no) THEN PUT SKIP(1).

                v-alloc-total = v-alloc-total + tt-job.alloc-qty.
                dReqTotal = dReqTotal + tt-job.required-qty .

                IF FIRST(tt-job.i-no) THEN
                DO:
                    PUT SPACE(45) "Jobs            Resource   Start Date  Allocation    Alloc. Total    Required Qty  Required Qty Total Customer Name " SKIP
                        SPACE(45) "-------------   ---------- ----------  ------------- --------------- ------------- ------------------ ------------------------------" SKIP.

                    IF rd-dest EQ 3 THEN
                        PUT STREAM excel UNFORMATTED
                            '"' "" '",'
                            '"' "" '",'
                            '"' "" '",'
                            '"' "Jobs" '",'              
                            '"' "Resource" '",'  
                            '"' "Start Date" '",'
                            '"' "Allocation" '",'
                            '"' "Alloc. Total" '",'
                            '"' "Required Qty " '",'
                            '"' "Required Qty Total" '",'
                            '"' "Customer Name" '",'
                            SKIP.
                END.

                PUT SPACE(45) TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', tt-job.job-no, tt-job.job-no2))) FORM "x(13)"
                    SPACE(3) tt-job.resource FORMAT "X(10)" SPACE(1)
                    tt-job.print-date SPACE(1) tt-job.alloc-qty FORMAT "->>,>>>,>>9.99" SPACE(2)
                    v-alloc-total FORMAT "->>,>>>,>>9.99"  
                    SPACE(1) tt-job.required-qty FORMAT "->,>>>,>>9.99"
                    SPACE(1) dReqTotal FORMAT "->>,>>>,>>>,>>9.99"
                    SPACE(1) tt-job.cust-name FORMAT "x(30)" SKIP .

                IF rd-dest EQ 3 THEN
                    PUT STREAM excel UNFORMATTED
                        '"' "" '",'
                        '"' "" '",'
                        '"' "" '",'
                        '"' STRING(tt-job.job-no,"X(9)") '",'              
                        '"' STRING(tt-job.resource,"X(8)") '",'  
                        '"' (IF tt-job.print-date NE ? THEN
                        STRING(tt-job.print-date,"99/99/9999") ELSE "") '",'
                        '"' STRING(tt-job.alloc-qty,"->>>,>>>,>>9.99") '",'
                        '"' STRING(v-alloc-total,"->>>,>>>,>>9.99") '",'
                        '"' STRING(tt-job.required-qty,"->>>,>>>,>>9.99") '",'
                        '"' STRING(dReqTotal,"->>>,>>>,>>9.99") '",'
                        '"' STRING(tt-job.cust-name,"x(30)") '",'
                        SKIP.

                IF LAST(tt-job.i-no) THEN
                DO:
                    PUT SPACE(103) v-alloc-total SKIP(1).

                    IF rd-dest EQ 3 THEN
                        PUT STREAM excel UNFORMATTED
                            '"' "" '",'
                            '"' "" '",'
                            '"' "" '",'
                            '"' "" '",'              
                            '"' "" '",'  
                            '"' "" '",'
                            '"' "" '",'
                            '"' STRING(v-alloc-total,"->>>,>>>,>>9") '",'
                            '"' "" '",'
                            '"' STRING(dReqTotal,"->>>,>>>,>>9.99") '",'
                            SKIP(1).
                END.
            END.
        END. /*display item*/

    END. /*each item*/

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").


    OUTPUT CLOSE.
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE    NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

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
                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                    .
            ELSE 
            DO:  /* radio set */
                ASSIGN 
                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                    lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
                REPEAT:
                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".

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
            fi_file:SCREEN-VALUE = "c:\tmp\r-rmbrd.csv".   
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

