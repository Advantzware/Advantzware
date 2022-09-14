&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-detjnl.w

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

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL NO-UNDO.

{salrep/ttreport.i NEW}

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD w-cust-no  LIKE ar-inv.cust-no COLUMN-LABEL "Customer"
    FIELD w-inv-date LIKE ar-inv.inv-date COLUMN-LABEL "Invoice!Date"
    FIELD w-ord-no   LIKE ar-invl.ord-no COLUMN-LABEL "Order!Number"
    FIELD w-inv-no   LIKE ar-invl.inv-no COLUMN-LABEL "Invoice!Number"
    FIELD w-bol-no   LIKE ar-invl.inv-no COLUMN-LABEL " BOL!Number"
    FIELD w-recid    AS RECID
    FIELD w-type     AS INTEGER
    FIELD w-job-no   LIKE ar-invl.job-no.

DEFINE VARIABLE v-print-fmt    AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.


ASSIGN 
    cTextListToSelect  = "Cust#,Name,BOL#,C/R,INV Date,Order#,Inv#," +
                           "QTY Shipped/M,Sq Ft,Total Sq Ft,$/MSF,Prod,Inv Amount,Order Item Name,Job#"
    cFieldListToSelect = "cust,name,bol,ct,inv-date,ord,inv," +
                            "qty-ship,sqft,tot-sqt,msf,prod-code,inv-amt,i-name,job-no"
    cFieldLength       = "8,30,7,3,8,8,7," + "13,9,11,10,5,14,30,13"
    cFieldType         = "c,c,i,c,c,i,i," + "i,i,i,i,c,i,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Cust#,Name,BOL#,C/R,INV Date,Order#,Inv#," +
                           "QTY Shipped/M,Sq Ft,Total Sq Ft,$/MSF,Prod,Inv Amount" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_cust-no end_cust-no begin_inv-date end_inv-date tb_freight tb_summary ~
tb_fin-chg tb_cred sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust-no end_cust-no ~
begin_inv-date end_inv-date tb_freight tb_summary tb_fin-chg tb_cred ~
sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_inv-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_inv-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Invoice Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesJournal.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

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
    SIZE 15 BY 5 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.71.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 8.29.

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

DEFINE VARIABLE tb_cred      AS LOGICAL   INITIAL NO 
    LABEL "Sort By Credit Rating?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_fin-chg   AS LOGICAL   INITIAL NO 
    LABEL "Include Finance Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_freight   AS LOGICAL   INITIAL NO 
    LABEL "Show Freight Charges?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.5 BY .81 NO-UNDO.

DEFINE VARIABLE tb_summary   AS LOGICAL   INITIAL NO 
    LABEL "Summary Only?" 
    VIEW-AS TOGGLE-BOX
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_cust-list AT ROW 1.95 COL 31.2 WIDGET-ID 6
    btnCustList AT ROW 2 COL 63.2 WIDGET-ID 8
    begin_cust-no AT ROW 3 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 3 COL 68.8 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_inv-date AT ROW 3.95 COL 28.2 COLON-ALIGNED HELP
    "Enter Beginning Invoice Date"
    end_inv-date AT ROW 3.95 COL 68.8 COLON-ALIGNED HELP
    "Enter Ending Invoice Date"
    tb_freight AT ROW 5.43 COL 35.4
    tb_summary AT ROW 6.38 COL 35.4
    tb_fin-chg AT ROW 7.33 COL 35.4
    tb_cred AT ROW 8.29 COL 35.4
    sl_avail AT ROW 10.76 COL 4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 10.76 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 10.76 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 11.76 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.76 COL 40.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.81 COL 40.8 WIDGET-ID 40
    btn_down AT ROW 14.81 COL 40.8 WIDGET-ID 42
    lv-ornt AT ROW 16.71 COL 30 NO-LABELS
    lines-per-page AT ROW 16.71 COL 83 COLON-ALIGNED
    rd-dest AT ROW 17.19 COL 6 NO-LABELS
    lv-font-no AT ROW 17.43 COL 33 COLON-ALIGNED
    tb_excel AT ROW 17.67 COL 76 RIGHT-ALIGNED
    lv-font-name AT ROW 18.38 COL 27 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.57 COL 28.6
    fi_file AT ROW 21 COL 26.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 21.1 COL 92.3 RIGHT-ALIGNED
    tbAutoClose AT ROW 22.43 COL 28.6 WIDGET-ID 78
    btn-ok AT ROW 23.38 COL 28.6
    btn-cancel AT ROW 23.38 COL 48.6
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.05 COL 60.4 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 16.38 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.05 COL 4.4 WIDGET-ID 38
    RECT-6 AT ROW 16.71 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 24.71
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
        TITLE              = "Sales Analysis - Sales Journal"
        HEIGHT             = 24.71
        WIDTH              = 96
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
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_inv-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    tb_cred:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fin-chg:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_freight:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_summary:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Sales Analysis - Sales Journal */
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
ON WINDOW-CLOSE OF C-Win /* Sales Analysis - Sales Journal */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
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


&Scoped-define SELF-NAME begin_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv-date C-Win
ON LEAVE OF begin_inv-date IN FRAME FRAME-A /* Beginning Invoice Date */
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
        IF rd-dest EQ 3 THEN
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
                INPUT begin_cust-no,
                INPUT end_cust-no).
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
                    {custom/asifax.i &type="Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
        END CASE. 

        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE. 
        SESSION:SET-WAIT-STATE("").
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv-date C-Win
ON LEAVE OF end_inv-date IN FRAME FRAME-A /* Ending Invoice Date */
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


&Scoped-define SELF-NAME tb_cred
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cred C-Win
ON VALUE-CHANGED OF tb_cred IN FRAME FRAME-A /* Sort By Credit Rating? */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fin-chg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fin-chg C-Win
ON VALUE-CHANGED OF tb_fin-chg IN FRAME FRAME-A /* Include Finance Charges? */
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


&Scoped-define SELF-NAME tb_summary
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_summary C-Win
ON VALUE-CHANGED OF tb_summary IN FRAME FRAME-A /* Summary Only? */
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
        begin_inv-date = DATE(1,1,YEAR(TODAY))
        end_inv-date   = TODAY.

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
    {sys/inc/reportsConfigNK1.i "HB" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    RUN sys/inc/CustListForm.p ( "HB",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_cust-no.
    END.
    RUN pChangeDest.
    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'HB',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""HB""}

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
            INPUT 'HB',
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
        INPUT 'HB').


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
    DISPLAY tb_cust-list begin_cust-no end_cust-no begin_inv-date end_inv-date 
        tb_freight tb_summary tb_fin-chg tb_cred sl_avail sl_selected rd-dest 
        fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust-no end_cust-no 
        begin_inv-date end_inv-date tb_freight tb_summary tb_fin-chg tb_cred 
        sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ouput-to-port C-Win 
PROCEDURE ouput-to-port :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RUN custom/d-print.w (list-name).
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
    /***************************************************************************\
    *****************************************************************************
    **  Program: /u2/fold/all/dev/asi/sa/sa-slsj.p
    **       by: Dave Reimer, Update by Chris Heins 9507
    ** Descript: Sales analysis daily sales by category
    **
    *****************************************************************************
    \***************************************************************************/

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-qty          LIKE ar-invl.ship-qty FORMAT "->>>,>>9.999" DECIMALS 3.
    DEFINE VARIABLE v-sq-ft        LIKE itemfg.t-sqft FORMAT "->>>9.999".
    DEFINE VARIABLE v-tot-sf-sht   AS DECIMAL   FORMAT "->>,>>>,>>9".
    DEFINE VARIABLE v-tot-msf      AS DECIMAL   FORMAT "->>,>>9.99".
    DEFINE VARIABLE fdate          AS DATE      FORMAT "99/99/9999".
    DEFINE VARIABLE tdate          LIKE fdate.
    DEFINE VARIABLE fcust          AS ch        INIT " ".
    DEFINE VARIABLE tcust          LIKE fcust INIT "zzzzzzzz".
    DEFINE VARIABLE v-inc-fc       AS LOG       INIT NO NO-UNDO.
    DEFINE VARIABLE v-procat       LIKE itemfg.procat NO-UNDO.
    DEFINE VARIABLE v-amt          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE tot-qty        LIKE v-qty.
    DEFINE VARIABLE tot-sf-sht     LIKE v-tot-sf-sht.
    DEFINE VARIABLE tot-amt        LIKE ar-inv.tot-ord.
    DEFINE VARIABLE cust-qty       LIKE v-qty NO-UNDO.
    DEFINE VARIABLE cust-sf-sht    LIKE v-tot-sf-sht NO-UNDO.
    DEFINE VARIABLE cust-amt       LIKE ar-inv.tot-ord NO-UNDO.
    DEFINE VARIABLE w-qty          LIKE v-qty COLUMN-LABEL "Quantity!Shipped/M".
    DEFINE VARIABLE w-sq-ft        LIKE v-sq-ft COLUMN-LABEL "Sq Ft".
    DEFINE VARIABLE w-tot-sf-sht   LIKE v-tot-sf-sht COLUMN-LABEL "Total!Sq Ft".
    DEFINE VARIABLE w-tot-msf      LIKE v-tot-msf COLUMN-LABEL "$/MSF".
    DEFINE VARIABLE w-procat       LIKE itemfg.procat COLUMN-LABEL "Prod!Code".
    DEFINE VARIABLE w-amt          LIKE ar-invl.amt COLUMN-LABEL "Invoice!Amount".
    DEFINE VARIABLE cItemName      AS CHARACTER NO-UNDO.

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
    DEFINE BUFFER bf-ar-inv FOR ar-inv.

    FORM w-data.w-cust-no
        cust.name          FORMAT "x(26)"
        cust.cr-rating     LABEL "C R"
        w-data.w-inv-date
        w-data.w-ord-no
        w-data.w-inv-no
        w-qty
        w-sq-ft SPACE(2)
        w-tot-sf-sht
        w-tot-msf
        w-procat
        w-amt
        WITH NO-BOX FRAME itemx DOWN STREAM-IO WIDTH 180.

    FORM w-data.w-cust-no
        cust.name          FORMAT "x(26)"
        w-data.w-bol-no    
        cust.cr-rating     LABEL "C R"
        w-data.w-inv-date
        w-data.w-ord-no
        w-data.w-inv-no
        w-qty
        w-sq-ft SPACE(2)
        w-tot-sf-sht
        w-tot-msf
        w-procat
        w-amt
        WITH NO-BOX FRAME itemxb DOWN STREAM-IO WIDTH 180.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        fcust    = begin_cust-no
        tcust    = end_cust-no
        fdate    = begin_inv-date
        tdate    = end_inv-date
        v-inc-fc = tb_fin-chg.

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

        IF LOOKUP(ttRptSelected.TextList, "QTY Shipped/M,Total Sq Ft,Inv Amount") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*  excelheader = "Cust#,Name,BOL#,C/R,INV Date,Order#,Inv#," +
                        "QTY Shipped/M,Sq Ft,Total Sq Ft,$/MSF,Prod Code," +
                        "Inv Amount".*/


        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END. 

    DISPLAY "" WITH FRAME r-top.

    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        EACH cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ ttCustList.cust-no /*fcust*/
        /* and cust.cust-no le tcust*/
        NO-LOCK:
        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cocode
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date GE fdate
            AND ar-inv.inv-date LE tdate
            AND (ar-inv.type    NE "FC" OR v-inc-fc)
            NO-LOCK:

            {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

            IF tb_freight AND ar-inv.f-bill AND ar-inv.freight NE 0 THEN 
            DO:
                RUN salrep/invfrate.p (ROWID(ar-inv), "allsalesmen", "").

                FOR EACH tt-report2
                    WHERE tt-report2.key-10 EQ "ar-invf"
                    AND tt-report2.inv-no EQ ar-inv.inv-no,
                    FIRST ar-invl WHERE RECID(ar-invl) EQ tt-report2.rec-id NO-LOCK:
                    ASSIGN
                        tt-report2.cred   = IF tb_cred THEN cust.cr-rating ELSE ""
                        tt-report2.key-01 = STRING(cust.name,"x(40)") + cust.cust-no
                        tt-report2.key-02 = STRING(YEAR(ar-inv.inv-date),"9999") +
                                 STRING(MONTH(ar-inv.inv-date),"99")  +
                                 STRING(DAY(ar-inv.inv-date),"99")
                        tt-report2.key-03 = IF tb_summary THEN ""
                                 ELSE STRING(ar-invl.ord-no,"99999999")
                        tt-report2.key-04 = STRING(ar-inv.inv-no,"9999999999")
                        tt-report2.key-05 = "3"
                        tt-report2.key-06 = cust.cust-no
                        tt-report2.key-07 = STRING(ar-invl.job-no + "-" + STRING(ar-invl.job-no2, "999"))
                        tt-report2.rec-id = RECID(ar-inv).
                END.
            END.

            FOR EACH ar-invl
                WHERE ar-invl.x-no EQ ar-inv.x-no
                AND (ar-invl.billable OR NOT ar-invl.misc)
                NO-LOCK:

                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = ""
                    tt-report.cred    = IF tb_cred THEN cust.cr-rating ELSE ""
                    tt-report.rec-id  = RECID(ar-invl)
                    tt-report.key-01  = STRING(cust.name,"x(40)") + cust.cust-no
                    tt-report.key-02  = STRING(YEAR(ar-inv.inv-date),"9999") +
                               string(MONTH(ar-inv.inv-date),"99")  +
                               string(DAY(ar-inv.inv-date),"99")
                    tt-report.key-03  = IF tb_summary THEN ""
                               ELSE STRING(ar-invl.ord-no,"99999999")
                    tt-report.key-04  = STRING(ar-invl.inv-no,"9999999999")
                    tt-report.key-05  = "1"
                    tt-report.key-07  = STRING(ar-invl.job-no + "-" + STRING(ar-invl.job-no2, "999")) 
                    tt-report.key-06  = cust.cust-no.
            END.
        END.

        FOR EACH ar-cash
            WHERE ar-cash.company    EQ cocode
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE fdate
            AND ar-cash.check-date LE tdate
            AND ar-cash.posted     EQ YES
            NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no    EQ ar-cash.c-no
            AND ar-cashl.posted  EQ YES
            AND ar-cashl.memo    EQ YES
            AND CAN-FIND(FIRST account
            WHERE account.company EQ ar-cashl.company
            AND account.actnum  EQ ar-cashl.actnum
            AND account.type    EQ "R")
            NO-LOCK:

            FIND FIRST bf-ar-inv NO-LOCK 
                WHERE bf-ar-inv.company  EQ cocode
                AND bf-ar-inv.posted   EQ YES
                AND bf-ar-inv.cust-no  EQ cust.cust-no
                AND bf-ar-inv.inv-no EQ ar-cashl.inv-no NO-ERROR.
            IF AVAILABLE bf-ar-inv THEN 
            DO:
                IF NOT v-inc-fc THEN
                    IF bf-ar-inv.TYPE EQ "FC" THEN NEXT .
            END.
            
            RELEASE oe-retl.

            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            CREATE tt-report.

            ASSIGN
                tt-report.term-id = ""
                tt-report.cred    = IF tb_cred THEN cust.cr-rating ELSE ""
                tt-report.rec-id  = RECID(ar-cashl)
                tt-report.key-01  = STRING(cust.name,"x(40)") + cust.cust-no
                tt-report.key-02  = STRING(YEAR(ar-cash.check-date),"9999") +
                             string(MONTH(ar-cash.check-date),"99")  +
                             string(DAY(ar-cash.check-date),"99")
                tt-report.key-03  = IF tb_summary THEN ""
                             ELSE STRING(IF AVAILABLE oe-retl THEN oe-retl.ord-no
                                         ELSE 0,"99999999")
                tt-report.key-04  = STRING(ar-cashl.inv-no,"9999999999")
                tt-report.key-05  = "2"
                tt-report.key-06  = cust.cust-no
                tt-report.key-07  = IF AVAILABLE oe-retl THEN 
                                STRING(oe-retl.job-no + "-" + STRING(oe-retl.job-no2, "999")) ELSE "".
        END.
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-06
        NO-LOCK

        BREAK BY tt-report.cred
        BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.key-04

        WITH FRAME itemx DOWN

        TRANSACTION:

        {custom/statusMsg.i " 'Processing Customer#  '  + cust.cust-no "}

        CREATE w-data.
        ASSIGN
            w-data.w-cust-no  = tt-report.key-06
            w-data.w-inv-date = DATE(int(substr(tt-report.key-02,5,2)),
                                int(substr(tt-report.key-02,7,2)),
                                int(substr(tt-report.key-02,1,4)))
            w-data.w-ord-no   = int(tt-report.key-03)
            w-data.w-inv-no   = int(tt-report.key-04)
            w-data.w-type     = int(tt-report.key-05)
            w-data.w-recid    = tt-report.rec-id
            v-sq-ft           = 0
            w-data.w-job-no   = tt-report.key-07.

        cItemName = "".
        IF w-data.w-type EQ 1 THEN 
        DO:
            FIND FIRST ar-invl WHERE RECID(ar-invl) EQ w-data.w-recid NO-LOCK.

            ASSIGN
                v-amt     = ar-invl.amt
                v-qty     = ar-invl.inv-qty / 1000
                v-sq-ft   = 0
                v-procat  = "MISC"
                cItemName = ar-invl.i-name.

            IF v-sq-ft EQ ? THEN v-sq-ft = 0.

            IF NOT ar-invl.misc THEN 
            DO:
                FIND FIRST itemfg
                    WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ ar-invl.i-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN 
                DO:
                    v-procat = itemfg.procat.
                    cItemName = itemfg.i-name.            
                    RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT v-sq-ft).
                END.

                ELSE 
                DO:
                    FIND FIRST fgcat
                        WHERE fgcat.company EQ cocode
                        AND fgcat.glacc   EQ ar-invl.actnum
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE fgcat THEN v-procat = fgcat.procat.
                END.
            END.  /* else  */

            v-tot-sf-sht = ar-invl.ship-qty * v-sq-ft.

            IF v-tot-sf-sht NE 0 THEN
                v-tot-msf = (ar-invl.amt / (v-tot-sf-sht / 1000)).
            ELSE
                v-tot-msf = 0.

            ASSIGN
                w-bol-no     = ar-invl.bol-no
                w-qty        = v-qty
                w-sq-ft      = v-sq-ft
                w-tot-sf-sht = v-tot-sf-sht
                w-tot-msf    = v-tot-msf
                w-procat     = v-procat
                w-amt        = ar-invl.amt.
        END.

        ELSE
            IF w-data.w-type EQ 2 THEN 
            DO:
                FIND FIRST ar-cashl WHERE RECID(ar-cashl) EQ w-data.w-recid NO-LOCK.

                ASSIGN
                    w-bol-no     = 0
                    w-qty        = 0
                    w-sq-ft      = 0
                    w-tot-sf-sht = 0
                    w-tot-msf    = 0
                    w-amt        = ar-cashl.amt-paid - ar-cashl.amt-disc
                    w-procat     = IF w-amt LT 0 THEN "CRMEM" ELSE
                        IF w-amt GT 0 THEN "DBMEM" ELSE "MEMO".

                RELEASE itemfg.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    w-procat = "MISC".

                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.
              
                    IF AVAILABLE itemfg THEN
                        RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT v-sq-ft).
                    ELSE v-sq-ft = 0.

                    ASSIGN           
                        v-qty        = oe-retl.tot-qty-return / 1000
                        v-tot-sf-sht = oe-retl.tot-qty-return * v-sq-ft

                        w-qty        = w-qty + v-qty
                        w-sq-ft      = w-sq-ft + v-sq-ft
                        w-tot-sf-sht = w-tot-sf-sht + v-tot-sf-sht.
                END.

                ASSIGN
                    w-qty        = - w-qty
                    w-tot-sf-sht = - w-tot-sf-sht
                    w-tot-msf    = IF w-tot-sf-sht NE 0 THEN
                          (w-amt / (w-tot-sf-sht / 1000)) ELSE 0
                    w-procat     = IF AVAILABLE itemfg THEN itemfg.procat ELSE "MISC"
                    cItemName    = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "".
            END.

            ELSE
                IF w-data.w-type EQ 3 THEN 
                DO:
                    ASSIGN
                        w-bol-no     = 0
                        w-qty        = 0
                        w-sq-ft      = 0
                        w-tot-sf-sht = 0
                        w-tot-msf    = 0
                        w-procat     = ""
                        w-amt        = tt-report.freight
                        .
                END.

        ACCUMULATE w-qty (total by tt-report.cred).
        ACCUMULATE w-tot-sf-sht (total by tt-report.cred).
        ACCUMULATE w-amt (total by tt-report.cred).

        ACCUMULATE w-qty (total by tt-report.key-01).
        ACCUMULATE w-tot-sf-sht (total by tt-report.key-01).
        ACCUMULATE w-amt (total by tt-report.key-01).

        ACCUMULATE w-qty (total by tt-report.key-04).
        ACCUMULATE w-sq-ft (total by tt-report.key-04).
        ACCUMULATE w-tot-sf-sht (total by tt-report.key-04).
        ACCUMULATE w-tot-msf (total by tt-report.key-04).
        ACCUMULATE w-amt (total by tt-report.key-04).

        IF LINE-COUNTER GE (PAGE-SIZE - 1) THEN PAGE.

        IF FIRST-OF(tt-report.key-01) AND first-of(tt-report.key-03) THEN PUT SKIP(1).

        IF NOT tb_summary OR LAST-OF(tt-report.key-04) THEN 
        DO:


            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = STRING(w-data.w-cust-no,"x(8)") .
                    WHEN "name"   THEN 
                        cVarValue = STRING(cust.name,"x(30)").
                    WHEN "bol"   THEN 
                        cVarValue = STRING(w-data.w-bol-no,">>>>>>>").
                    WHEN "ct"  THEN 
                        cVarValue = STRING(cust.cr-rating,"x(3)") .
                    WHEN "inv-date"   THEN 
                        cVarValue = STRING(w-data.w-inv-date,"99/99/99") .
                    WHEN "ord"  THEN 
                        cVarValue = STRING(w-data.w-ord-no,">>>>>>>>") .
                    WHEN "inv"   THEN 
                        cVarValue = STRING(w-data.w-inv-no,">>>>>>>>") .
                    WHEN "qty-ship"  THEN 
                        cVarValue = IF tb_summary THEN STRING(ACCUMULATE TOTAL BY tt-report.key-04 w-qty,"->>>>,>>9.999") ELSE  STRING(w-qty,"->>>>,>>9.999") .
                    WHEN "sqft"  THEN 
                        cVarValue = IF tb_summary THEN STRING(ACCUMULATE TOTAL BY tt-report.key-04 w-sq-ft,"->>>9.999") ELSE STRING(w-sq-ft,"->>>9.999") .
                    WHEN "tot-sqt"   THEN 
                        cVarValue = IF tb_summary THEN STRING(ACCUMULATE TOTAL BY tt-report.key-04 w-tot-sf-sht,"->>,>>>,>>9") ELSE STRING(w-tot-sf-sht,"->>,>>>,>>9") .
                    WHEN "msf"  THEN 
                        cVarValue = IF tb_summary THEN STRING(ACCUMULATE TOTAL BY tt-report.key-04 w-tot-msf,"->>,>>9.99") ELSE STRING(w-tot-msf,"->>,>>9.99") .
                    WHEN "prod-code"   THEN 
                        cVarValue = STRING(w-procat,"x(5)") .
                    WHEN "inv-amt"  THEN 
                        cVarValue = IF tb_summary THEN STRING(ACCUMULATE TOTAL BY tt-report.key-04 w-amt,"->>,>>>,>>9.99") ELSE STRING(w-amt,"->>,>>>,>>9.99") .
                    WHEN "i-name"   THEN 
                        cVarValue = STRING(cItemName,"x(30)").
                    WHEN "job-no"  THEN 
                        cVarValue = IF TRIM(w-data.w-job-no) BEGINS "-" THEN "" ELSE STRING(w-data.w-job-no) .
                END CASE.

                IF  cTmpField = "inv-date" THEN
                     cExcelVarValue = IF w-data.w-inv-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",w-data.w-inv-date) ELSE "".
                ELSE cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs,cExcelVarValue)) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    cExcelDisplay SKIP.
            END.


        END. 

        IF LAST-OF(tt-report.key-01) THEN 
        DO:
            ASSIGN 
                cust-qty    = (ACCUMULATE TOTAL BY tt-report.key-01 w-qty)
                cust-sf-sht = (ACCUMULATE TOTAL BY tt-report.key-01 w-tot-sf-sht)
                cust-amt    = (ACCUMULATE TOTAL BY tt-report.key-01 w-amt).
            PUT SKIP.


            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = "" .
                    WHEN "name"   THEN 
                        cVarValue = "".
                    WHEN "bol"   THEN 
                        cVarValue = "".
                    WHEN "ct"  THEN 
                        cVarValue = "" .
                    WHEN "inv-date"   THEN 
                        cVarValue = "" .
                    WHEN "ord"  THEN 
                        cVarValue = "" .
                    WHEN "inv"   THEN 
                        cVarValue = "" .
                    WHEN "qty-ship"  THEN 
                        cVarValue = STRING(cust-qty,"->>>>,>>9.999") .
                    WHEN "sqft"  THEN 
                        cVarValue = "" .
                    WHEN "tot-sqt"   THEN 
                        cVarValue = STRING(cust-sf-sht,"->>,>>>,>>9") .
                    WHEN "msf"  THEN 
                        cVarValue = "" .
                    WHEN "prod-code"   THEN 
                        cVarValue = "" .
                    WHEN "inv-amt"  THEN 
                        cVarValue =  STRING(cust-amt,"->>,>>>,>>9.99") .
                    WHEN "i-name"   THEN 
                        cVarValue = "".
                    WHEN "job-no"  THEN 
                        cVarValue = "" .
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT str-line SKIP.
            PUT UNFORMATTED 
                "     CUSTOMER TOTALS" SUBSTRING(cDisplay,21,350) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    '               CUSTOMER TOTALS ,'
                    SUBSTRING(cExcelDisplay,4,350) SKIP(1).
            END.

        END.

        IF tb_cred AND last-of(tt-report.cred) THEN 
        DO:
            ASSIGN 
                cust-qty    = (ACCUMULATE TOTAL BY tt-report.cred w-qty)
                cust-sf-sht = (ACCUMULATE TOTAL BY tt-report.cred w-tot-sf-sht)
                cust-amt    = (ACCUMULATE TOTAL BY tt-report.cred w-amt).
            PUT SKIP.


            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "cust"    THEN 
                        cVarValue = "" .
                    WHEN "name"   THEN 
                        cVarValue = "".
                    WHEN "bol"   THEN 
                        cVarValue = "".
                    WHEN "ct"  THEN 
                        cVarValue = "" .
                    WHEN "inv-date"   THEN 
                        cVarValue = "" .
                    WHEN "ord"  THEN 
                        cVarValue = "" .
                    WHEN "inv"   THEN 
                        cVarValue = "" .
                    WHEN "qty-ship"  THEN 
                        cVarValue = STRING(cust-qty,"->>>>,>>9.999") .
                    WHEN "sqft"  THEN 
                        cVarValue = "" .
                    WHEN "tot-sqt"   THEN 
                        cVarValue = STRING(cust-sf-sht,"->>,>>>,>>9") .
                    WHEN "msf"  THEN 
                        cVarValue = "" .
                    WHEN "prod-code"   THEN 
                        cVarValue = "" .
                    WHEN "inv-amt"  THEN 
                        cVarValue =  STRING(cust-amt,"->>,>>>,>>9.99") .
                    WHEN "i-name"   THEN 
                        cVarValue = "".
                    WHEN "job-no"  THEN 
                        cVarValue = "" .
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT str-line SKIP .
            PUT UNFORMATTED 
                "CREDIT RATING TOTALS" SUBSTRING(cDisplay,21,350) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    '  CREDIT RATING TOTALS ,'
                    SUBSTRING(cExcelDisplay,4,350) SKIP(1).
            END.

        END.

        ASSIGN
            tot-qty    = tot-qty + w-qty
            /* tot-sq-ft     = tot-sq-ft     + w-sq-ft */
            tot-sf-sht = tot-sf-sht + w-tot-sf-sht
            tot-amt    = tot-amt  + w-amt.

        DELETE w-data.
        DELETE tt-report.
    END.  /* for each tt-report... */

    /* display final totals */
    PUT SKIP(2).



    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "cust"    THEN 
                cVarValue = "" .
            WHEN "name"   THEN 
                cVarValue = "".
            WHEN "bol"   THEN 
                cVarValue = "".
            WHEN "ct"  THEN 
                cVarValue = "" .
            WHEN "inv-date"   THEN 
                cVarValue = "" .
            WHEN "ord"  THEN 
                cVarValue = "" .
            WHEN "inv"   THEN 
                cVarValue = "" .
            WHEN "qty-ship"  THEN 
                cVarValue = STRING(tot-qty,"->>>>,>>9.999") .
            WHEN "sqft"  THEN 
                cVarValue = "" .
            WHEN "tot-sqt"   THEN 
                cVarValue = STRING(tot-sf-sht,"->>,>>>,>>9") .
            WHEN "msf"  THEN 
                cVarValue = "" .
            WHEN "prod-code"   THEN 
                cVarValue = "" .
            WHEN "inv-amt"  THEN 
                cVarValue =  STRING(tot-amt,"->>,>>>,>>9.99") .
            WHEN "i-name"   THEN 
                cVarValue =  "".
            WHEN "job-no"  THEN 
                cVarValue = "" .
        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
    PUT str-line SKIP.
    PUT UNFORMATTED 
        "        GRAND TOTALS" SUBSTRING(cDisplay,21,350) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            '               GRAND TOTALS ,'
            SUBSTRING(cExcelDisplay,4,350) SKIP(1).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

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
            begin_cust-no:SENSITIVE = NOT iplChecked
            end_cust-no:SENSITIVE   = NOT iplChecked
            begin_cust-no:VISIBLE   = NOT iplChecked
            end_cust-no:VISIBLE     = NOT iplChecked
            btnCustList:SENSITIVE   = iplChecked
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
    DEFINE VARIABLE lv-label      AS CHARACTER NO-UNDO.

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
                tb_excel                = YES
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO
                tb_excel                = NO
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\SalesJournal.csv".   
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

