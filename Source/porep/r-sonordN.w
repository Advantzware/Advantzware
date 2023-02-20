&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-sonord.w

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE TEMP-TABLE wk-sh-ord NO-UNDO
    FIELD due-date LIKE po-ordl.due-date
    FIELD machine  AS CHARACTER
    FIELD key1     AS CHARACTER
    FIELD rec-id   AS RECID.

DEFINE VARIABLE ll-secure      AS LOGICAL   NO-UNDO.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vWidth             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vLength            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vFGItem#           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
/*  DATE  ORDER        QUANTITY           QUANTITY  ------ VENDOR ------              P/O FG ITEM         RM ITEM           MSF        COST
     DUE     NO         ORDERED UOM       RECEIVED  ------- NAME -------  MACHINE  NUMBER NUMBER          NUMBER         REMAIN      REMAIN
---
display wk-sh-ord.due-date
            oe-ord.ord-no when avail oe-ord ">>>>>9"
            ld-oqty @ po-ordl.ord-qty   ->>>,>>>,>>9.9<<<<<
            lv-uom  @ po-ordl.pr-qty-uom
            ld-rqty @ po-ordl.t-rec-qty
            v-prt-name
            wk-sh-ord.machine
            po-ordl.po-no >>>>>9
            job-hdr.i-no when avail job-hdr
            po-ordl.i-no
            v-msf-rem
            v-cst-rem when v-cost     
            po-ordl.s-wid >>,>>9.99<<<
            po-ordl.s-len

*/
ASSIGN 
    cTextListToSelect  = "Due Date,Order#,Customer Name,Vendor Name,Order Qty,Rcpt Qty," + 
                           "Cost Due,MSF Due,Machine#,P/O#,FG Item#,RM Item#,Width,Length,UOM,Job#,Job Due" 
    cFieldListToSelect = "due-date,v-ord-no,v-cust-name,v-vend-name,ld-oqty,ld-rqty," +
                            "v-cst-rem,v-msf-rem,wk-sh-ord.machine,po-ordl.po-no,vFGItem#,po-ordl.i-no,v-wid,v-len,lv-uom,v-job-no,v-jobDueDate"
    cFieldLength       = "8,8,30,30,15,15," + "10,10,8,6,15,15,11,11,4,13,10" 
    cFieldType         = "i,i,c,c,i,i," + "i,i,c,i,c,c,i,i,c,c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Due Date,Order#,Order Qty,UOM,Rcpt Qty,Customer Name,Vendor Name," + 
                              "Machine#,P/O#,FG Item#,RM Item#,MSF Due,Cost Due" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE NO

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend-no end_vend-no ~
begin_due-date end_due-date rd_uom rd_sort tb_date tb_closed sl_avail ~
sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest td-show-parm ~
fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel lbl_uom lbl_sort 
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date rd_uom rd_sort tb_date tb_closed sl_avail sl_selected rd-dest ~
td-show-parm fi_file tb_OpenCSV tbAutoClose lbl_uom lbl_sort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-vendor-name C-Win 
FUNCTION get-vendor-name RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Due Date" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-sonord.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1.

DEFINE VARIABLE lbl_show       AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS TEXT 
    SIZE 9.8 BY .62 NO-UNDO.

DEFINE VARIABLE lbl_uom        AS CHARACTER FORMAT "X(256)":U INITIAL "Print Qtys in Which UOM?" 
    VIEW-AS TEXT 
    SIZE 26 BY .62 NO-UNDO.

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

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_show        AS CHARACTER INITIAL "Customer Name" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer Name", "Customer Name",
    "Vendor Name", "Vendor Name"
    SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "D" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Machine", "M",
    "Due Date", "D",
    "Vendor Name", "V"
    SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE rd_uom         AS CHARACTER INITIAL "Purchased" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Purchased", "Purchased",
    "Consumption", "Consumption",
    "MSF", "MSF"
    SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.19.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 7.38.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.48 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.48 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_closed    AS LOGICAL   INITIAL NO 
    LABEL "Show Closed PO's (already received)" 
    VIEW-AS TOGGLE-BOX
    SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE tb_date      AS LOGICAL   INITIAL NO 
    LABEL "Display Subtotal?" 
    VIEW-AS TOGGLE-BOX
    SIZE 31 BY .86 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE tb_sheets    AS LOGICAL   INITIAL NO 
    LABEL "Print Cost Of Remaining Sheets?" 
    VIEW-AS TOGGLE-BOX
    SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_vend-no AT ROW 2.43 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend-no AT ROW 2.43 COL 69 COLON-ALIGNED HELP
    "Enter Ending Vendor number"
    begin_due-date AT ROW 3.52 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Due Date"
    end_due-date AT ROW 3.52 COL 69 COLON-ALIGNED HELP
    "Enter ending Due Date"
    rd_uom AT ROW 4.86 COL 40 NO-LABELS
    tb_sheets AT ROW 5.52 COL 104
    rd_sort AT ROW 5.86 COL 40.2 NO-LABELS WIDGET-ID 2
    tb_date AT ROW 6.91 COL 36
    lbl_show AT ROW 7.19 COL 107 COLON-ALIGNED NO-LABELS
    tb_closed AT ROW 7.76 COL 36 WIDGET-ID 8
    rd_show AT ROW 8.14 COL 98 NO-LABELS
    sl_avail AT ROW 10.05 COL 4 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 10.05 COL 61 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 10.1 COL 41.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 11.19 COL 41.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.29 COL 41.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.38 COL 41.8 WIDGET-ID 40
    btn_down AT ROW 14.48 COL 41.8 WIDGET-ID 42
    rd-dest AT ROW 16.81 COL 6.8 NO-LABELS
    lv-font-name AT ROW 16.81 COL 25 COLON-ALIGNED NO-LABELS
    lv-ornt AT ROW 16.81 COL 30 NO-LABELS
    tb_excel AT ROW 17.05 COL 59 RIGHT-ALIGNED
    lines-per-page AT ROW 17.05 COL 51 COLON-ALIGNED
    lv-font-no AT ROW 17.29 COL 40 COLON-ALIGNED
    td-show-parm AT ROW 18.81 COL 38.4
    fi_file AT ROW 19.62 COL 28 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 19.62 COL 87.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 21.71 COL 30 WIDGET-ID 16
    btn-ok AT ROW 22.52 COL 30
    btn-cancel AT ROW 22.52 COL 49
    lbl_uom AT ROW 5.05 COL 11 COLON-ALIGNED NO-LABELS
    lbl_sort AT ROW 6.05 COL 27.4 COLON-ALIGNED NO-LABELS WIDGET-ID 6
    "Available Columns" VIEW-AS TEXT
    SIZE 22 BY .62 AT ROW 9.33 COL 9.8 WIDGET-ID 38
    " Output Destination" VIEW-AS TEXT
    SIZE 20 BY .62 AT ROW 15.76 COL 5.4
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.38 COL 5
    BGCOLOR 15 
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 9.33 COL 60.6 WIDGET-ID 44
    RECT-6 AT ROW 16.05 COL 4
    RECT-7 AT ROW 1.71 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.05
    SIZE 138.6 BY 28.14
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
        TITLE              = "Sheets On Order Report"
        HEIGHT             = 23.19
        WIDTH              = 96.8
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
    begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_due-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    lbl_show:HIDDEN IN FRAME FRAME-A       = TRUE
    lbl_show:PRIVATE-DATA IN FRAME FRAME-A = "rd_show".

ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

ASSIGN 
    lbl_uom:PRIVATE-DATA IN FRAME FRAME-A = "rd_uom".

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

/* SETTINGS FOR RADIO-SET rd_show IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    rd_show:HIDDEN IN FRAME FRAME-A       = TRUE
    rd_show:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    rd_uom:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_sheets IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_sheets:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_sheets:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sheets On Order Report */
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
ON WINDOW-CLOSE OF C-Win /* Sheets On Order Report */
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


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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
        IF iColumnLength > 300 THEN
            MESSAGE "Report may not show all selected columns appropriately. " SKIP
                "Use Excel Output for all selected column values. (" iColumnLength ")"
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.

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
                    {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=end_vend-no
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend-no
                             &END_cust=end_vend-no
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
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


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
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
        RUN pChangeDest .
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


&Scoped-define SELF-NAME rd_uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_uom C-Win
ON VALUE-CHANGED OF rd_uom IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail C-Win
ON DEFAULT-ACTION OF sl_avail IN FRAME FRAME-A
    DO:

        IF (NOT CAN-DO(sl_selected:LIST-ITEMS,{&SELF-NAME}:SCREEN-VALUE) OR
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
                    ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
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


&Scoped-define SELF-NAME tb_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_date C-Win
ON VALUE-CHANGED OF tb_date IN FRAME FRAME-A /* Display Subtotal? */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sheets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sheets C-Win
ON VALUE-CHANGED OF tb_sheets IN FRAME FRAME-A /* Print Cost Of Remaining Sheets? */
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
        begin_due-date = TODAY
        end_due-date   = DATE(12,31,YEAR(TODAY)).

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
APPLY "entry" TO begin_vend-no.
END.

RUN pChangeDest .
  
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
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
            .    
    END.

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
            (IF cListContents = "" THEN ""  ELSE ",") +
            ENTRY(iCount,cTextListToSelect)   .
        CREATE ttRptList.
        ASSIGN 
            ttRptList.TextList  = ENTRY(iCount,cTextListToSelect)
            ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect) .
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
    DISPLAY begin_vend-no end_vend-no begin_due-date end_due-date rd_uom rd_sort 
        tb_date tb_closed sl_avail sl_selected rd-dest td-show-parm fi_file 
        tb_OpenCSV tbAutoClose lbl_uom lbl_sort 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_vend-no end_vend-no begin_due-date end_due-date 
        rd_uom rd_sort tb_date tb_closed sl_avail sl_selected Btn_Def Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file tb_OpenCSV 
        tbAutoClose btn-ok btn-cancel lbl_uom lbl_sort 
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
        IF AVAILABLE ttRptList THEN 
        DO:
            CREATE ttRptSelected.
            ASSIGN 
                ttRptSelected.TextList        = ENTRY(i,cTmpList)
                ttRptSelected.FieldList       = ttRptList.FieldList
                ttRptSelected.FieldLength     = INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
                ttRptSelected.DisplayOrder    = i
                ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
                iColumnLength                 = iColumnLength + ttRptSelected.FieldLength + 1.
        END.                          
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
    DEFINE INPUT PARAMETER MOVE AS CHARACTER NO-UNDO.

    DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
        WITH FRAME {&FRAME-NAME}:
        IF sl_selected:IS-SELECTED(i) THEN
        DO:
            IF MOVE = "Down" AND i NE sl_selected:NUM-ITEMS THEN
                ASSIGN
                    ldummy                   = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
                    ldummy                   = sl_selected:DELETE(i)
                    sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
                    .
            ELSE
                IF MOVE = "Up" AND i NE 1 THEN
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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT RESULT).
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
    RUN scr-rpt.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ po/rep/sh-ord.p 8/96 fwk  */
    /* Sheets On Order Report                                                     */
    /* -------------------------------------------------------------------------- */
    DEFINE VARIABLE str-tit4 AS cha NO-UNDO.
    DEFINE VARIABLE str-tit5 AS cha NO-UNDO.
    DEFINE VARIABLE str-line AS cha FORM "x(300)" NO-UNDO.
    {sys/FORM/r-top5DL3.f}
    DEFINE VARIABLE v-msf-rem       AS DECIMAL   FORMAT ">,>>>,>>9.999" NO-UNDO.
    DEFINE VARIABLE v-cst-rem       AS DECIMAL   FORMAT ">>>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-s-vend        LIKE vend.vend-no INIT "" NO-UNDO.
    DEFINE VARIABLE v-e-vend        LIKE vend.vend-no INIT "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE v-s-date        LIKE po-ord.po-date FORMAT "99/99/9999" INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-e-date        LIKE po-ord.po-date FORMAT "99/99/9999" INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-name          AS CHARACTER FORMAT "x(1)" INIT "C" NO-UNDO.
    DEFINE VARIABLE v-cost          AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-subtotal-flag AS LOG       FORMAT "Y/N" NO-UNDO.
    DEFINE VARIABLE v-prt-name      LIKE oe-ord.cust-name.
    DEFINE VARIABLE v-wid           LIKE po-ordl.s-wid.
    DEFINE VARIABLE v-len           LIKE po-ordl.s-len.
    DEFINE VARIABLE v-dep           LIKE ITEM.s-dep.
    DEFINE VARIABLE v-bwt           LIKE ITEM.basis-w.
    DEFINE VARIABLE v-s-num         LIKE po-ordl.s-num INIT 1 NO-UNDO.
    DEFINE VARIABLE ld-oqty         AS DECIMAL   FORM "->>>,>>>,>>9.9<<<<<" NO-UNDO.
    DEFINE VARIABLE ld-rqty         AS DECIMAL   FORM "->>>,>>>,>>9.9<<<<<" NO-UNDO.
    DEFINE VARIABLE lv-uom          LIKE po-ordl.pr-qty-uom NO-UNDO.
    DEFINE VARIABLE ld              AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-mach          AS CHARACTER EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-sortby        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-closed        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-mch-rowid     AS ROWID     NO-UNDO.
    DEFINE VARIABLE v-cust-vend     AS CHARACTER FORMAT "x(20)" INIT "------ VENDOR ------".
    DEFINE VARIABLE tot-cons-qty    LIKE po-ordl.cons-qty EXTENT 2 NO-UNDO.
    DEFINE VARIABLE tot-rec-qty     AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE tot-msf-rem     AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE tot-cst-rem     AS DECIMAL   EXTENT 2 NO-UNDO.
    DEFINE VARIABLE excelheader     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ord-no        AS INTEGER   FORM ">>>>>9" NO-UNDO.
    DEFINE VARIABLE vFGItem#        AS cha       FORM "x(15)" NO-UNDO.
    DEFINE BUFFER boe-ord  FOR oe-ord.
    DEFINE BUFFER bpo-ordl FOR po-ordl.
    DEFINE BUFFER bwk-sh   FOR wk-sh-ord.
    DEFINE VARIABLE cDisplay       AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha    NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA    NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha    NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha    NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha    NO-UNDO.
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE v-vend-name  AS cha     NO-UNDO.
    DEFINE VARIABLE iSubCount    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-jobDueDate AS DATE    NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    ASSIGN
        str-tit2        = TRIM(c-win:TITLE) + " (P-R-1)"
        {sys/inc/ctrtext.i str-tit2 112}
        v-s-vend        = begin_vend-no
        v-e-vend        = end_vend-no 
        v-s-date        = begin_due-date
        v-e-date        = end_due-date
        v-name          = SUBSTR(rd_show,1,1)  
        v-cost          = tb_sheets
        v-subtotal-flag = tb_date
        v-sortby        = rd_sort
        v-closed        = tb_closed.

    IF v-cost THEN 
    DO: 
        IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
        v-cost = ll-secure. 
    END.

    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ",".        
        ELSE 
            ASSIGN str-tit4    = str-tit4 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
           ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + ",".   

        IF LOOKUP(ttRptSelected.TextList, "Order Qty,Rcpt Qty,MSF Due,Cost Due") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF rd-dest = 3  THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.
    SESSION:SET-WAIT-STATE ("general").
    DISPLAY "" WITH FRAME r-top.
    FOR EACH wk-sh-ord:
        DELETE wk-sh-ord.
    END.

    po-ord:
    FOR EACH po-ord
        WHERE po-ord.company EQ cocode
        AND INDEX("XF",po-ord.stat) EQ 0 
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        NO-LOCK:

        IF v-closed = NO AND po-ord.stat = "C" THEN NEXT po-ord.

        po-ordl:
        FOR EACH po-ordl WHERE
            po-ordl.company EQ po-ord.company AND
            po-ordl.po-no   EQ po-ord.po-no AND
            po-ordl.item-type
            AND INDEX("XF",po-ordl.stat) EQ 0
            AND po-ordl.due-date GE v-s-date
            AND po-ordl.due-date LE v-e-date
            NO-LOCK:

            {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

            IF v-closed = NO AND po-ordl.stat = "C" THEN NEXT po-ordl.

            FIND FIRST ITEM
                WHERE ITEM.company EQ cocode
                AND ITEM.i-no    EQ po-ordl.i-no
                AND po-ordl.item-type
                NO-LOCK NO-ERROR.
            IF AVAILABLE ITEM AND INDEX("1234BPR",ITEM.mat-type) GT 0 THEN 
            DO:
                {po/po-fibr1.i v-mach[1] v-mach[2] v-mach[3] v-mach[4]}

                CREATE wk-sh-ord.
                ASSIGN
                    wk-sh-ord.due-date = po-ordl.due-date
                    wk-sh-ord.machine  = v-mach[1]
                    wk-sh-ord.rec-id   = RECID(po-ordl).
                /* Task 11071308 */
                IF wk-sh-ord.machine = "" THEN 
                DO:
                    FIND FIRST job-hdr
                        WHERE job-hdr.company EQ cocode
                        AND job-hdr.job-no  EQ po-ordl.job-no
                        AND job-hdr.job-no2 EQ po-ordl.job-no2
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE job-hdr THEN
                        FOR FIRST job
                            WHERE job.company EQ po-ordl.company
                            AND job.job-no  EQ po-ordl.job-no
                            AND job.job-no2 EQ po-ordl.job-no2
                            NO-LOCK,
                            FIRST job-mch 
                            WHERE job-mch.company EQ cocode
                            AND job-mch.job     EQ job.job
                            AND job-mch.job-no  EQ job.job-no
                            AND job-mch.job-no2 EQ job.job-no2
                            NO-LOCK  USE-INDEX line-idx.
                            wk-sh-ord.machine = job-mch.m-code .
                            LEAVE.
                        END.
                END.  /* Task 11071308 */

                IF v-sortby = "D" THEN 
                    ASSIGN wk-sh-ord.key1 = STRING(YEAR(po-ordl.due-date),"9999") +  
                                    STRING(MONTH(po-ordl.due-date),"99") + 
                                    STRING(DAY(po-ordl.due-date),"99").
                ELSE IF v-sortby = "M" THEN ASSIGN wk-sh-ord.key1 = wk-sh-ord.machine .
                    ELSE IF v-sortby = "V" THEN ASSIGN wk-sh-ord.key1 = STRING(get-vendor-name(),"x(20)").
            END.
        END.
    END.

    FOR EACH wk-sh-ord,
        EACH po-ordl WHERE RECID(po-ordl) EQ wk-sh-ord.rec-id NO-LOCK,

        FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK,

        FIRST ITEM
        WHERE ITEM.company EQ cocode
        AND ITEM.i-no    EQ po-ordl.i-no
        NO-LOCK

        BREAK BY wk-sh-ord.key1 
        BY wk-sh-ord.due-date:

      {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

        ASSIGN
            lv-uom  = IF rd_uom BEGINS "P" THEN po-ordl.pr-qty-uom ELSE
               IF rd_uom BEGINS "C" THEN po-ordl.cons-uom   ELSE "MSF"
            v-len   = po-ordl.s-len
            v-wid   = po-ordl.s-wid
            v-dep   = ITEM.s-dep
            v-bwt   = 0
            v-s-num = po-ordl.s-num.
        IF FIRST-OF(wk-sh-ord.key1) THEN
            ASSIGN iSubCount = 0.
        iSubCount = iSubCount + 1.
        {po/pol-dims.i}

        IF v-wid EQ 0 THEN v-wid = 12.
        IF v-len EQ 0 OR lv-uom EQ "ROLL" THEN v-len = 12.

        ld-oqty = po-ordl.ord-qty.

        IF po-ordl.pr-qty-uom NE lv-uom THEN
            RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, lv-uom,
                v-bwt, v-len, v-wid, v-dep,
                ld-oqty,
                OUTPUT ld-oqty).
        ld-rqty = po-ordl.t-rec-qty.

        IF po-ordl.cons-uom NE lv-uom THEN
            RUN sys/ref/convquom.p (po-ordl.cons-uom, lv-uom,
                v-bwt, v-len, v-wid, v-dep,
                ld-rqty,
                OUTPUT ld-rqty).

        IF po-ordl.item-type EQ YES   AND
            lv-uom NE po-ordl.cons-uom THEN 
        DO:

            ld[2] = 0.

            FOR EACH rm-rcpth NO-LOCK
                WHERE rm-rcpth.company   EQ po-ordl.company
                AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
                AND rm-rcpth.i-no      EQ po-ordl.i-no
                AND rm-rcpth.rita-code EQ "R",
                EACH rm-rdtlh NO-LOCK
                WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
                AND rm-rdtlh.job-no  EQ po-ordl.job-no
                AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
                AND rm-rdtlh.s-num   EQ po-ordl.s-num:

                IF lv-uom EQ "ROLL" AND rm-rdtlh.tag NE "" THEN ld[1] = 1.
                ELSE 
                DO:
                    ld[1] = rm-rdtlh.qty.

                    IF rm-rcpth.pur-uom NE lv-uom THEN
                        RUN sys/ref/convquom.p(rm-rcpth.pur-uom, lv-uom,
                            v-bwt, v-len, v-wid, v-dep,
                            ld[1],
                            OUTPUT ld[1]).
                END.

                ld[2] = ld[2] + ld[1].
            END.
        END.

        ELSE ld[2] = ld-rqty.

        v-msf-rem = ld-oqty - ld-rqty.

        IF lv-uom NE "MSF" THEN
            RUN sys/ref/convquom.p (lv-uom, "MSF",
                v-bwt, v-len, v-wid, v-dep,
                v-msf-rem,
                OUTPUT v-msf-rem).
        ld-rqty = ld[2].
        RUN sys/ref/convcuom.p (po-ordl.cons-uom, "MSF",
            v-bwt, v-len, v-wid, v-dep,
            po-ordl.cons-cost,
            OUTPUT v-cst-rem).

        ASSIGN
            v-cst-rem   = v-msf-rem * v-cst-rem
            v-prt-name  = ""
            v-vend-name = ""
            .

        FIND FIRST vend
            WHERE vend.company = cocode
            AND vend.vend-no = po-ord.vend-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE vend THEN ASSIGN v-vend-name = vend.NAME.

        RELEASE oe-ord.
        v-jobDueDate = ? .
        FIND FIRST job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ po-ordl.job-no
            AND job-hdr.job-no2 EQ po-ordl.job-no2
            AND job-hdr.job-no  NE ""
            NO-LOCK NO-ERROR.

        IF AVAILABLE job-hdr THEN v-jobDueDate = job-hdr.due-date.
        IF wk-sh-ord.machine = "" AND AVAILABLE job-hdr THEN 
        DO:

            v-mch-rowid = ?.
            FOR FIRST job-mch 
                WHERE job-mch.company = cocode
                AND job-mch.job-no  = job-hdr.job-no
                AND job-mch.job-no2 = job-hdr.job-no2
                NO-LOCK  USE-INDEX line-idx.
                v-mch-rowid = ROWID(job-mch).
                LEAVE.
            END.
            IF v-mch-rowid <> ? THEN
                FIND job-mch WHERE ROWID(job-mch) = v-mch-rowid NO-LOCK NO-ERROR.
            IF AVAILABLE job-mch THEN
                wk-sh-ord.machine = job-mch.m-code.
        END.

        IF po-ordl.ord-no NE 0 THEN
            FIND FIRST oe-ord
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ po-ordl.ord-no
                NO-LOCK NO-ERROR.

        IF NOT AVAILABLE oe-ord AND AVAILABLE job-hdr THEN
            FIND FIRST oe-ord
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ job-hdr.ord-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE oe-ord  THEN ASSIGN v-prt-name = oe-ord.cust-name
                .
        IF v-prt-name EQ "" THEN 
        DO:
            FIND FIRST cust WHERE cust.company EQ cocode
                AND cust.cust-no EQ po-ordl.cust-no NO-LOCK NO-ERROR.
            IF AVAILABLE cust THEN
                v-prt-name = cust.NAME .                                /* Task# 10081307 */
        END.
        IF v-prt-name EQ "" THEN 
        DO:
            IF AVAILABLE job-hdr
                THEN 
            DO:
                FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no EQ job-hdr.i-no NO-LOCK NO-ERROR.

                IF AVAILABLE itemfg THEN
                    v-prt-name = itemfg.cust-name .
            END.
        END.                                                        /* Task# 10081307 */

        IF v-msf-rem LT 0 THEN v-msf-rem = 0.
        IF v-cst-rem LT 0 THEN v-cst-rem = 0.
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".  
        IF AVAILABLE oe-ord THEN BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
        BUFFER bpo-ordl:FIND-BY-ROWID(ROWID(po-ordl),NO-LOCK) .
        BUFFER bwk-sh:FIND-BY-ROWID(ROWID(wk-sh-ord), NO-LOCK) .
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).         
            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                IF cFieldName BEGINS "po-ordl" THEN hField = BUFFER bpo-ordl:BUFFER-FIELD(cTmpField) .
                ELSE IF cFieldName BEGINS "oe-ord" THEN 
                    DO:
                        IF AVAILABLE boe-ord THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
                        ELSE hField = ?.
                    END.
                    ELSE hfield = BUFFER bwk-sh:BUFFER-FIELD(cTmpField).
                IF hField <> ? THEN 
                DO:                 
                    cTmpField = SUBSTRING(GetFieldValue(hField),1,INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                    IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + STRING(job-hdr.job-no2,"999") ELSE "".                  

                    cDisplay = cDisplay + cTmpField + 
                        FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                        .
                    cExcelDisplay = cExcelDisplay + QUOTER(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,GetFieldValue(hField))) + ",".                    
                END.
                ELSE 
                DO:
                    cTmpField = SUBSTRING(cFieldName,1,INT( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + QUOTER(" ") + ",".
                END.
            END.
            ELSE 
            DO:       
                CASE cTmpField:                                   
                {porep/r-sonordN1.i}
                END CASE.
                IF  cTmpField = "v-jobDueDate" THEN
                    cExcelVarValue = IF v-JobDueDate <> ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-jobDueDate) ELSE "".
                IF  cTmpField = "due-date" THEN
                    cExcelVarValue = IF bwk-sh.due-date <> ? THEN DYNAMIC-FUNCTION("sfFormat_Date",bwk-sh.due-date) ELSE "".
                ELSE
                    cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + QUOTER(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
            END.
        END.
        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3  THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.
        /*===== end of new ===== */


        ASSIGN
            tot-cons-qty[1] = tot-cons-qty[1] + ld-oqty
            tot-rec-qty[1]  = tot-rec-qty[1] + ld-rqty
            tot-msf-rem[1]  = tot-msf-rem[1] + v-msf-rem
            tot-cst-rem[1]  = tot-cst-rem[1] + v-cst-rem.

        /*===========*/
        IF LAST-OF(wk-sh-ord.key1) THEN 
        DO:
            IF v-subtotal-flag  /*AND iSubCount > 1*/ THEN 
            DO: 
                /* task 12311303  */
                PUT    SKIP  str-line SKIP .

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".  
                IF AVAILABLE oe-ord THEN BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
                BUFFER bpo-ordl:FIND-BY-ROWID(ROWID(po-ordl),NO-LOCK) .
                BUFFER bwk-sh:FIND-BY-ROWID(ROWID(wk-sh-ord), NO-LOCK) .
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).         
                    CASE cTmpField:                                   
                        WHEN "vFGItem#" THEN 
                            cVarValue =  "".
                        WHEN "wk-sh-ord.due-date" THEN 
                            cVarValue =  "".
                        WHEN "wk-sh-ord.machine" THEN 
                            cVarValue =  "".
                        WHEN "po-ordl.po-no" THEN 
                            cVarValue =  "".
                        WHEN "po-ordl.i-no" THEN 
                            cVarValue =  "".
                        WHEN "ld-oqty" THEN 
                            cVarValue = STRING(tot-cons-qty[1],"->>>,>>>,>>9.9<<<<<").
                        WHEN "ld-rqty" THEN 
                            cVarValue = STRING(tot-rec-qty[1],"->>>,>>>,>>9.9<<<<<").
                        WHEN "v-cst-rem" THEN 
                            cVarValue = STRING(tot-cst-rem[1],">>,>>>,>>9").
                        WHEN "v-msf-rem" THEN 
                            cVarValue = STRING(tot-msf-rem[1],">>,>>9.999").
                        WHEN "v-ord-no" THEN 
                            cVarValue = "".
                        WHEN "v-cust-name" THEN 
                            cVarValue = "".
                        WHEN "v-vend-name" THEN 
                            cVarValue = "".
                        WHEN "lv-uom" THEN 
                            cVarValue = "".
                        WHEN "v-wid" THEN 
                            cVarValue = "".
                        WHEN "v-len" THEN 
                            cVarValue = "".
                        WHEN "v-job-no" THEN 
                            cVarValue =  "".
                        WHEN "v-jobDueDate" THEN 
                            cVarValue =  "".

                    END CASE.
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
                /*END.*/
                END.
                PUT UNFORMATTED 
                    "        Sub Total:"  SUBSTRING(cDisplay,19,300) SKIP(1).
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        "SUB TOTAL" +   SUBSTRING(cExcelDisplay,3,300) SKIP.
                END.
            /* task 12311303  */
            /*PUT UNFORMATTED space(10) FILL('-',80) SKIP.
            PUT "Sub Total:" SPACE(30)    "Qty Ordered:" tot-cons-qty[1] 
                    "    Qty Received:" tot-rec-qty[1]              
                    "    MSF Remain:" tot-msf-rem[1]              
                    /*"    Cost Remain: " 
                    IF v-cost THEN tot-cst-rem[1]  ELSE 0*/
                SKIP(1).
    
            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   SKIP(1)
                   '"' "SUB TOTAL"                                      '",'
                   '"' "Qty Ordered"                                    '",'
                   '"' STRING(tot-cons-qty[1],"->>>,>>>,>>9.9<<<<<")    '",'
                   '"' "Qty Received"                                   '",'
                   '"' STRING(tot-rec-qty[1],"->>>,>>>,>>9.9<<<<<")     '",'
                   '"' "MSF Remain"                                     '",'
                   '"' STRING(tot-msf-rem[1],">,>>>,>>9.99")                '",'
                   /*'"' "Cost Remain"                                    '",'
                   '"' (IF v-cost THEN STRING(tot-cst-rem[1],">>>,>>9")
                        ELSE "")                                        '",'*/
                   SKIP.*/
            END.

            ASSIGN
                tot-cons-qty[2] = tot-cons-qty[2] + tot-cons-qty[1]
                tot-rec-qty[2]  = tot-rec-qty[2]  + tot-rec-qty[1]
                tot-msf-rem[2]  = tot-msf-rem[2]  + tot-msf-rem[1]
                tot-cst-rem[2]  = tot-cst-rem[2]  + tot-cst-rem[1]

                tot-cons-qty[1] = 0
                tot-rec-qty[1]  = 0
                tot-msf-rem[1]  = 0
                tot-cst-rem[1]  = 0.
        END.

        IF LAST(wk-sh-ord.key1) THEN 
        DO:
            /* task 12311303  */        
            PUT   
                SKIP  str-line SKIP .

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".  
            IF AVAILABLE oe-ord THEN BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
            BUFFER bpo-ordl:FIND-BY-ROWID(ROWID(po-ordl),NO-LOCK) .
            BUFFER bwk-sh:FIND-BY-ROWID(ROWID(wk-sh-ord), NO-LOCK) .
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).         
                CASE cTmpField:                                   
                    WHEN "vFGItem#" THEN 
                        cVarValue =  "".
                    WHEN "wk-sh-ord.due-date" THEN 
                        cVarValue =  "".
                    WHEN "wk-sh-ord.machine" THEN 
                        cVarValue =  "".
                    WHEN "po-ordl.po-no" THEN 
                        cVarValue =  "".
                    WHEN "po-ordl.i-no" THEN 
                        cVarValue =  "".
                    WHEN "ld-oqty" THEN 
                        cVarValue = STRING(tot-cons-qty[2],"->>>,>>>,>>9.9<<<<<").
                    WHEN "ld-rqty" THEN 
                        cVarValue = STRING(tot-rec-qty[2],"->>>,>>>,>>9.9<<<<<").
                    WHEN "v-cst-rem" THEN 
                        cVarValue = STRING(tot-cst-rem[2],">>,>>>,>>9").
                    WHEN "v-msf-rem" THEN 
                        cVarValue = STRING(tot-msf-rem[2],">>,>>9.999").
                    WHEN "v-ord-no" THEN 
                        cVarValue = "".
                    WHEN "v-cust-name" THEN 
                        cVarValue = "".
                    WHEN "v-vend-name" THEN 
                        cVarValue = "".
                    WHEN "lv-uom" THEN 
                        cVarValue = "".
                    WHEN "v-wid" THEN 
                        cVarValue = "".
                    WHEN "v-len" THEN 
                        cVarValue = "".
                    WHEN "v-job-no" THEN 
                        cVarValue =  "".
                    WHEN "v-jobDueDate" THEN 
                        cVarValue =  "".

                END CASE.
                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",INT(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",".            
            /*END.*/
            END.
            PUT UNFORMATTED 
                "            Total:"  SUBSTRING(cDisplay,19,300) SKIP(1).
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    "TOTAL " + SUBSTRING(cExcelDisplay,3,300) SKIP.
            END.

        /* task 12311303  */
        /*if v-subtotal-flag then do:      */
        /*PUT UNFORMATTED space(10) FILL('-',80) SKIP.
        PUT "Total:    " SPACE(30) "Qty Ordered:" tot-cons-qty[2] 
                "    Qty Received:" tot-rec-qty[2]              
                "    MSF Remain:" tot-msf-rem[2]              
                /*"    Cost Remain: " 
                IF v-cost THEN tot-cst-rem[2]  ELSE 0*/
            SKIP.

        IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' "TOTAL"                                            '",'
           '"' "Qty Ordered"                                    '",'
           '"' ""                                            '",'
           '"' STRING(tot-cons-qty[2],"->>>,>>>,>>9.9<<<<<")    '",'
           '"' "Qty Received"                                   '",'
           '"' ""                                            '",'
           '"' STRING(tot-rec-qty[2],"->>>,>>>,>>9.9<<<<<")     '",'
           '"' "MSF Remain"                                     '",'
           '"' STRING(tot-msf-rem[2],">,>>>,>>9.99")                '",'
           /*'"' "Cost Remain"                                    '",'
           '"' (IF v-cost THEN STRING(tot-cst-rem[2],">>>,>>9")
                ELSE "")                                        '",' */
           SKIP.*/
        END.
    /*end.*/
    /* =========   */
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
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
    DEFINE VARIABLE lv-frame-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-group-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field-hdl  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lv-field2-hdl AS HANDLE  NO-UNDO.
    DEFINE VARIABLE parm-fld-list AS cha     NO-UNDO.
    DEFINE VARIABLE parm-lbl-list AS cha     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-label      AS cha.

    lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
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
            ENTRY(i,parm-lbl-list) NE "" THEN 
        DO:

            lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                TRIM(ENTRY(i,parm-lbl-list)) + ":".

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
            fi_file:SCREEN-VALUE = "c:\tmp\r-sonord.csv".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-vendor-name C-Win 
FUNCTION get-vendor-name RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    FIND FIRST vend
        WHERE vend.company = cocode
        AND vend.vend-no = po-ord.vend-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE vend THEN 
        RETURN vend.NAME.
    ELSE
        RETURN "".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

