&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-cashs2.w

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

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file    AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no  AS INTEGER 
    FIELD chk-inv AS LOG     INIT YES.
DEFINE TEMP-TABLE tt-report-inv NO-UNDO LIKE report 
    FIELD inv-no AS INTEGER  .


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
    cTextListToSelect  = "Slsmn,Rep Name,Customer,Cust Name,Terms,Inv Date,Check Date,Aging,Invoice#," +
                            "Inv Amount,Amt Paid,Discount,Bal Aft Pymt,Comm%,Comm" 
                          
    cFieldListToSelect = "rep,rep-name,cust,cust-name,term,invdate,chk-date,aging,inv," +
                            "inv-amt,amt-paid,disc,bal-af,comm%,comm"
    cFieldLength       = "5,30,8,30,5,10,10,10,8," + "15,15,12,15,7,14"
    cFieldType         = "c,c,c,c,c,c,c,i,i," + "i,i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Slsmn,Customer,Cust Name,Terms,Inv Date,Check Date,Aging,Invoice#," +
                            "Inv Amount,Amt Paid,Discount,Bal Aft Pymt,Comm%,Comm,"  .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date ~
begin_slsmn end_slsmn rd_sort tb_tdisc tb_prep days-old sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest tb_OpenCSV fi_file ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_slsmn end_slsmn ~
lbl_sort rd_sort lbl_days-old lbl_tdisc tb_tdisc lbl_incldprep tb_prep ~
days-old sl_avail sl_selected rd-dest tb_OpenCSV fi_file tbAutoClose

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

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_slsmn    AS CHARACTER FORMAT "XXX" 
    LABEL "Beginning SalesRep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE days-old       AS INTEGER   FORMAT ">>>>":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 6 BY .95 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Receipt Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_slsmn      AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
    LABEL "Ending SalesRep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesRepName.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 49 BY 1
    .

DEFINE VARIABLE lbl_days-old   AS CHARACTER FORMAT "X(256)":U INITIAL "Receipts After How Many Days" 
    VIEW-AS FILL-IN 
    SIZE 1 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_incldprep  AS CHARACTER FORMAT "X(256)":U INITIAL "Include Prep Charges?" 
    VIEW-AS FILL-IN 
    SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort?" 
    VIEW-AS FILL-IN 
    SIZE 7 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_tdisc      AS CHARACTER FORMAT "X(256)":U INITIAL "Include Terms Discount?" 
    VIEW-AS FILL-IN 
    SIZE 26 BY .95 NO-UNDO.

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
    SIZE 17.2 BY 4.76 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Invoice#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Invoice#", "Invoice#",
    "Sales Rep", "SalesRep#"
    SIZE 44 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.29.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 7.62.

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
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prep      AS LOGICAL   INITIAL YES 
    LABEL "" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE tb_show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tdisc     AS LOGICAL   INITIAL YES 
    LABEL "Include Terms Discount?" 
    VIEW-AS TOGGLE-BOX
    SIZE 3 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_date AT ROW 2.67 COL 28 COLON-ALIGNED HELP
    "Enter Beginning Receipt Date"
    end_date AT ROW 2.67 COL 71 COLON-ALIGNED HELP
    "Enter Ending Receipt Date"
    begin_slsmn AT ROW 3.62 COL 28 COLON-ALIGNED HELP
    "Enter Beginning SalesRep Number"
    end_slsmn AT ROW 3.62 COL 71 COLON-ALIGNED HELP
    "Enter Ending SalesRep Number"
    lbl_sort AT ROW 4.81 COL 28 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 4.81 COL 38 NO-LABELS
    lbl_days-old AT ROW 6.38 COL 4 NO-LABELS
    lbl_tdisc AT ROW 6.43 COL 10 COLON-ALIGNED NO-LABELS
    tb_tdisc AT ROW 6.43 COL 40 RIGHT-ALIGNED
    lbl_incldprep AT ROW 6.43 COL 40.6 COLON-ALIGNED NO-LABELS WIDGET-ID 46
    tb_prep AT ROW 6.48 COL 68.6 WIDGET-ID 48
    days-old AT ROW 7.62 COL 55 COLON-ALIGNED NO-LABELS
    sl_avail AT ROW 10.05 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 10.05 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 10.05 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 11.05 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.05 COL 40.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.1 COL 40.2 WIDGET-ID 40
    btn_down AT ROW 14.1 COL 40.2 WIDGET-ID 42
    rd-dest AT ROW 16 COL 4.8 NO-LABELS
    lines-per-page AT ROW 15.95 COL 87 COLON-ALIGNED
    lv-ornt AT ROW 16 COL 48 NO-LABELS
    lv-font-no AT ROW 15.86 COL 39 COLON-ALIGNED
    lv-font-name AT ROW 17.14 COL 29 COLON-ALIGNED NO-LABELS
    tb_show-parm AT ROW 18.81 COL 29.2
    tb_excel AT ROW 18.14 COL 92 RIGHT-ALIGNED
    tb_OpenCSV AT ROW 19.76 COL 92.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 21 COL 29.4 WIDGET-ID 64
    fi_file AT ROW 19.71 COL 27.2 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 21.95 COL 29
    btn-cancel AT ROW 21.95 COL 55
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 9.33 COL 60.4 WIDGET-ID 44
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 9.33 COL 3 WIDGET-ID 38
    "Days" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 7.81 COL 64
    "Show Only Invoices with Cash Receipts after" VIEW-AS TEXT
    SIZE 44 BY .62 AT ROW 7.81 COL 12
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 15.29 COL 4
    RECT-6 AT ROW 15.71 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95 BY 25.91
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
        TITLE              = "Cash Receipts by SalesRep Name"
        HEIGHT             = 22.48
        WIDTH              = 94.8
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 96.2
        VIRTUAL-HEIGHT     = 26.1
        VIRTUAL-WIDTH      = 96.2
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    days-old:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slsmn:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_days-old IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
    lbl_days-old:PRIVATE-DATA IN FRAME FRAME-A = "days-old".

/* SETTINGS FOR FILL-IN lbl_incldprep IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_incldprep:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lbl_tdisc IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_tdisc:PRIVATE-DATA IN FRAME FRAME-A = "tb_tdisc".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".
                
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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_prep:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_tdisc IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_tdisc:PRIVATE-DATA IN FRAME FRAME-A = "parm".
                
/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_show-parm:HIDDEN IN FRAME FRAME-A = TRUE.                

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cash Receipts by SalesRep Name */
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
ON WINDOW-CLOSE OF C-Win /* Cash Receipts by SalesRep Name */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slsmn C-Win
ON LEAVE OF begin_slsmn IN FRAME FRAME-A /* Beginning SalesRep# */
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
            ASSIGN 
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
                    {custom/asifax.i &begin_cust=begin_slsmn
                            &END_cust=END_slsmn
                            &fax-subject="Cash Receipts By Sales Rep"
                            &fax-body="Cash Receipts By Sales Rep"
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_slsmn
                             &END_cust=end_slsmn
                             &mail-subject="Cash Receipts By Sales Rep"
                             &mail-body="Cash Receipts By Sales Rep"
                             &mail-file=lv-pdf-file + ".pdf" }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_slsmn
                                  &END_cust=end_slsmn
                                  &mail-subject="Cash Receipts By Sales Rep"
                                  &mail-body="Cash Receipts By Sales Rep"
                                  &mail-file=list-name }

                    END.
 
                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE (""). 
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


&Scoped-define SELF-NAME days-old
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL days-old C-Win
ON LEAVE OF days-old IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slsmn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slsmn C-Win
ON LEAVE OF end_slsmn IN FRAME FRAME-A /* Ending SalesRep# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS cha NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG NO-UNDO.

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
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
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


&Scoped-define SELF-NAME rd_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_sort C-Win
ON VALUE-CHANGED OF rd_sort IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
        IF {&self-name}:SCREEN-VALUE = "SalesRep#" THEN 
            ASSIGN lv-ornt:SCREEN-VALUE = "L".
        ELSE
            ASSIGN lv-ornt:SCREEN-VALUE = "P".
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


&Scoped-define SELF-NAME tb_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prep C-Win
ON VALUE-CHANGED OF tb_prep IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Auto Run Excel? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_show-parm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_show-parm C-Win
ON VALUE-CHANGED OF tb_show-parm IN FRAME FRAME-A /* Show Parameters? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_tdisc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tdisc C-Win
ON VALUE-CHANGED OF tb_tdisc IN FRAME FRAME-A /* Include Terms Discount? */
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

    begin_date = DATE(1,1,YEAR(TODAY)).
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AR16" }
    ASSIGN
        tb_show-parm:SENSITIVE = lShowParameters
        tb_show-parm:HIDDEN    = NOT lShowParameters
        tb_show-parm:VISIBLE   = lShowParameters
        .
  
    {methods/nowait.i}
  
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date.
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
    DISPLAY begin_date end_date begin_slsmn end_slsmn lbl_sort rd_sort 
        lbl_days-old lbl_tdisc tb_tdisc lbl_incldprep tb_prep days-old 
        sl_avail sl_selected rd-dest tb_OpenCSV fi_file tbAutoClose
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_slsmn end_slsmn rd_sort 
        tb_tdisc tb_prep days-old sl_avail Btn_Def sl_selected Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest tb_OpenCSV fi_file 
        tbAutoClose btn-ok btn-cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gt-excel-1 C-Win 
PROCEDURE gt-excel-1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-paid AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dsc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-amt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-rem AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-perc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-com AS DECIMAL NO-UNDO.
  
/*PUT STREAM excel UNFORMATTED
    '"' ""                                         '",'
    '"' "Grand Totals:"                            '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' STRING(ip-amt,"->>,>>>,>>9.99")            '",' 
    '"' STRING(ip-paid,"->>,>>>,>>9.99")           '",'
    '"' STRING(ip-dsc,"->>,>>>,>>9.99")            '",'
    '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "") '",'
    '"' STRING(ip-perc,"->>9.99")                  '",'
    '"' STRING(ip-com,"->>,>>>,>>9.99")         '",'
    SKIP.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gt-excel-2 C-Win 
PROCEDURE gt-excel-2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-paid AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dsc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-amt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-rem AS DECIMAL NO-UNDO.
  
/*PUT STREAM excel UNFORMATTED
    '"' ""                                   '",'
    '"' "Grand Totals:"                      '",'
    '"' ""                                   '",'
    '"' ""                                   '",'
    '"' ""                                   '",'
    '"' ""                                   '",'
    '"' ""                                   '",'
    '"' ""                                   '",'
    '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
    '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
    '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
    '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "") '",'
    SKIP.*/
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
    /*    DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
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
    IF rd_sort = "SalesRep#" THEN
        ASSIGN lv-ornt = "L".
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
    IF rd_sort = "SalesRep#" THEN
        ASSIGN lv-ornt = "L".
    RUN scr-rpt.w (list-name,c-win:TITLE,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    SESSION:SET-WAIT-STATE("general").
    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE fdate          AS DATE      FORMAT "99/99/9999" INIT "01/01/0001" NO-UNDO.
    DEFINE VARIABLE tdate          LIKE fdate INIT 12/31/9999 NO-UNDO.
    DEFINE VARIABLE fsman          AS CHARACTER FORMAT "x(3)" NO-UNDO.
    DEFINE VARIABLE tsman          LIKE fsman INIT "zzz" NO-UNDO.
    DEFINE VARIABLE v-cust         AS LOG       FORMAT "Customer/Invoice" INIT NO NO-UNDO.
    DEFINE VARIABLE v-disc         AS LOG       FORMAT "Include/Exclude" INIT YES NO-UNDO.
    DEFINE VARIABLE v-days         AS INTEGER   FORMAT ">>>>" INIT 0 NO-UNDO.
    DEFINE VARIABLE sortby         AS CHARACTER INIT "Customer#" NO-UNDO.

    DEFINE VARIABLE v-sman         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-amt          LIKE ar-cashl.amt-paid EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-paid         LIKE v-amt NO-UNDO.
    DEFINE VARIABLE v-dsc          LIKE v-amt NO-UNDO.
    DEFINE VARIABLE v-com          LIKE ar-cashl.amt-paid NO-UNDO.
    DEFINE VARIABLE v-com-2        LIKE v-com NO-UNDO.
    DEFINE VARIABLE v-c-%          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tax          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-pct          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-misc         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-basis        LIKE sman.commbasis INIT "" NO-UNDO.

    DEFINE VARIABLE v-tot-amt      AS DECIMAL   EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-tot-rem      AS DECIMAL   EXTENT 4 NO-UNDO.
    DEFINE VARIABLE v-tot-com      LIKE v-tot-amt NO-UNDO.
    DEFINE VARIABLE v-tot-dsc      LIKE v-tot-amt NO-UNDO.
    DEFINE VARIABLE v-tot-paid     LIKE v-tot-amt NO-UNDO.
    DEFINE VARIABLE vcustno        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-aging        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-check-date   AS DATE      NO-UNDO.
    DEFINE VARIABLE v-inv-date     AS DATE      NO-UNDO.
    DEFINE VARIABLE v-disc-meth    AS logi      NO-UNDO INIT YES.
    DEFINE VARIABLE v-amt-full     AS DECIMAL   NO-UNDO FORM "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-inv-full     AS DECIMAL   NO-UNDO FORM "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-rem-bal      AS DECIMAL   NO-UNDO FORM "->>>,>>>,>>9.99".
    DEFINE VARIABLE v-dsc2         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tmp-amt-1    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-inv-found    AS LOG       NO-UNDO.

    DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j              AS INTEGER   NO-UNDO.

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

    {sys/form/r-top5L3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    ASSIGN  
        fdate  = begin_date
        tdate  = end_date
        fsman  = begin_slsmn
        tsman  = end_slsmn
        v-cust = rd_sort EQ "Customer#" OR rd_sort = "SalesRep#"
        sortby = rd_sort
        v-disc = tb_tdisc
        v-days = days-old.

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit3 = "Receipt Date: " + string(fdate,"99/99/9999") + " - " +
                                 string(tdate,"99/99/9999") +
              fill(" ",4) + "Sales Rep: " + fsman + " - " + tsman
        x        = (80 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3 
        str-tit2 = c-win:TITLE /*"CASH RECEIPTS BY SALESREP"*/
        {sys/inc/ctrtext.i str-tit  112}
        {sys/inc/ctrtext.i str-tit2 112}
        {sys/inc/ctrtext.i str-tit3 140}.

    /*form header 
                str-tit4 SKIP
                str-tit5 SKIP
    
        with frame r-top.*/

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

        IF LOOKUP(ttRptSelected.TextList, "Inv Amount,Amt Paid,Discount,Bal Aft Pymt,Comm%,Comm") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        /*IF v-cust THEN
           excelheader = "Slsmn,Customer,Terms,InvDate,CheckDate,Aging,Invoice#,Inv Amount,Amt Paid,Discount,Bal Aft Pymt,Comm%,Comm".
        ELSE
           excelheader = "Slsmn,Name,Customer,Terms,InvDate,CheckDate,Aging,Invoice#,Inv Amount,Amt Paid,Discount,Bal Aft Pymt".*/

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF tb_show-parm THEN RUN show-param.

    DISPLAY str-tit /*SKIP str-tit4 FORM "x(180)"
     SKIP str-tit5 FORM "x(180)"*/ WITH FRAME r-top STREAM-IO.
 
    {sa/sa-sls01.i}

    FOR EACH cust WHERE cust.company EQ cocode NO-LOCK:

        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

        IF v-days EQ 0 THEN
            FOR EACH ar-inv
                WHERE ar-inv.company  EQ cocode
                AND ar-inv.posted   EQ YES
                AND ar-inv.cust-no  EQ cust.cust-no
                AND ar-inv.inv-date GE fdate
                AND ar-inv.inv-date LE tdate
                AND ar-inv.terms    EQ "CASH"
                NO-LOCK,

                EACH ar-invl
                WHERE ar-invl.x-no EQ ar-inv.x-no
                NO-LOCK
        
                TRANSACTION:
         
                DO i = 1 TO 3:
                    v-sman = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                    ELSE ar-invl.sman[i].

                    IF v-sman   LT fsman                         OR
                        v-sman   GT tsman                         OR
                        (i NE 1 AND
                        (v-sman EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                    IF NOT CAN-FIND(FIRST tt-report
                        WHERE tt-report.term-id EQ v-term
                        AND tt-report.key-01  EQ v-sman
                        AND tt-report.inv-no  EQ ar-invl.inv-no
                        AND tt-report.rec-id  EQ recid(ar-invl)) THEN 
                    DO:
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = v-term
                            tt-report.key-01  = v-sman
                            tt-report.key-09  = cust.cust-no
                            tt-report.key-08  = cust.terms
                            tt-report.key-10  = cust.NAME
                            tt-report.rec-id  = RECID(ar-invl)
                            tt-report.inv-no  = ar-invl.inv-no.

                        IF v-cust THEN
                            ASSIGN
                                tt-report.key-02 = cust.cust-no
                                tt-report.key-03 = STRING(ar-invl.inv-no,"9999999999").
                        ELSE
                            ASSIGN
                                tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                                tt-report.key-03 = cust.cust-no.

                        RELEASE tt-report.
                    END.
                END.
            END.      

        FOR EACH ar-cash
            WHERE ar-cash.company    EQ cocode
            AND ar-cash.cust-no    EQ cust.cust-no
            AND ar-cash.check-date GE fdate
            AND ar-cash.check-date LE tdate
            AND ar-cash.posted     EQ YES
            AND ar-cash.check-no   NE 0
            NO-LOCK,

            EACH ar-cashl
            WHERE ar-cashl.c-no   EQ ar-cash.c-no
            AND ar-cashl.posted EQ YES
            AND ar-cashl.memo   EQ NO
            NO-LOCK
      
            TRANSACTION:

            IF NOT (v-days EQ 0 OR
                (ar-cash.check-date - ar-cashl.inv-date GT v-days AND
                ar-cashl.inv-no NE 0)) THEN NEXT.

            IF ar-cashl.inv-no NE 0 THEN
                FOR EACH ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    NO-LOCK:

                    DO i = 1 TO 3:
                        v-sman = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                        ELSE ar-invl.sman[i].
           
                        IF v-sman  LT fsman                          OR
                            v-sman  GT tsman                          OR
                            (i NE 1 AND
                            (v-sman EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
           
                        IF NOT CAN-FIND(FIRST tt-report
                            WHERE tt-report.term-id EQ v-term
                            AND tt-report.key-01  EQ v-sman
                            AND tt-report.inv-no  EQ ar-invl.inv-no
                            AND tt-report.rec-id  EQ recid(ar-cashl)) THEN 
                        DO:
                            CREATE tt-report.
                            ASSIGN
                                tt-report.term-id = v-term
                                tt-report.key-01  = v-sman
                                tt-report.key-09  = cust.cust-no
                                tt-report.key-08  = cust.terms
                                tt-report.key-10  = cust.NAME
                                tt-report.rec-id  = RECID(ar-cashl)
                                tt-report.inv-no  = ar-invl.inv-no.

                            IF v-cust THEN
                                ASSIGN
                                    tt-report.key-02 = cust.cust-no
                                    tt-report.key-03 = STRING(ar-invl.inv-no,"9999999999").
                            ELSE
                                ASSIGN
                                    tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                                    tt-report.key-03 = cust.cust-no.

                            RELEASE tt-report.
                        END.
                    END.
                END.
      
            ELSE
                IF cust.sman GE fsman AND
                    cust.sman LE tsman THEN 
                DO:
                    v-sman = cust.sman.
        
                    IF NOT CAN-FIND(FIRST tt-report
                        WHERE tt-report.term-id EQ v-term
                        AND tt-report.key-01  EQ v-sman
                        AND tt-report.inv-no  EQ ar-cashl.inv-no
                        AND tt-report.rec-id  EQ recid(ar-cashl)) THEN 
                    DO:
                        CREATE tt-report.
           
                        ASSIGN
                            tt-report.term-id = v-term
                            tt-report.key-01  = v-sman
                            tt-report.key-09  = cust.cust-no
                            tt-report.key-08  = cust.terms
                            tt-report.key-10  = cust.NAME
                            tt-report.rec-id  = RECID(ar-cashl)
                            tt-report.inv-no  = ar-cashl.inv-no.
           
                        IF v-cust THEN
                            ASSIGN
                                tt-report.key-02 = cust.cust-no
                                tt-report.key-03 = STRING(ar-cashl.inv-no,"9999999999").
                        ELSE
                            ASSIGN
                                tt-report.key-02 = STRING(ar-cashl.inv-no,"9999999999")
                                tt-report.key-03 = cust.cust-no.
           
                        RELEASE tt-report.
                    END.
                END.
        END.
    END.

    FOR EACH tt-report NO-LOCK:
        FIND FIRST tt-report-inv WHERE tt-report-inv.inv-no = tt-report.inv-no 
            AND  tt-report-inv.key-01 NE tt-report.key-01  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE tt-report-inv THEN 
        DO:
            CREATE  tt-report-inv.
            ASSIGN
                tt-report-inv.key-01 = tt-report.key-01
                tt-report-inv.key-02 = tt-report.key-02
                tt-report-inv.key-03 = tt-report.key-03
                tt-report-inv.inv-no = tt-report.inv-no  .
         
        END. 
        ELSE 
        DO:
            tt-report.chk-inv = FALSE .
        END.
          
    END.
    FOR EACH tt-report-inv NO-LOCK:
        DELETE tt-report-inv .
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ v-term,
  
        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK
          
        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.inv-no
      
        TRANSACTION:
        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}
        FIND FIRST sman
            WHERE sman.company EQ cocode
            AND sman.sman    EQ tt-report.key-01
            NO-LOCK NO-ERROR.

        /* IF sortby = "Salesrep#" AND FIRST-of(tt-report.key-01) THEN
            PUT SKIP
                "Sales Rep: " + string(tt-report.key-01) + " - "  FORM "x(20)" (IF AVAIL sman THEN sman.sname ELSE "") FORM "x(30)" SKIP(1).*/
    
        RELEASE ar-inv.
        RELEASE ar-cash.

        ASSIGN 
            v-check-date = ?
            v-inv-date   = ?
            v-amt-full   = 0
            v-inv-full   = 0
            v-dsc2       = 0
            v-misc       = 0.
    
        FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK NO-ERROR.    
        
        IF AVAILABLE ar-cashl THEN 
        DO:
            FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.
            ASSIGN
                v-dsc[1]     = IF v-disc THEN ar-cashl.amt-disc ELSE 0
                v-check-date = ar-cash.check-date
                v-amt-full   = ar-cashl.amt-paid
                v-inv-found  = NO.
      
            IF ar-cashl.inv-no NE 0 THEN
                FOR EACH ar-invl WHERE
                    ar-invl.company EQ cocode AND
                    ar-invl.cust-no EQ ar-cash.cust-no AND
                    ar-invl.inv-no  EQ ar-cashl.inv-no
                    NO-LOCK,
                    FIRST ar-inv WHERE
                    ar-inv.x-no EQ ar-invl.x-no
                    NO-LOCK
                    BREAK BY ar-invl.inv-no:
      
                    v-inv-found = YES.

                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-LOCK NO-ERROR.
           
                    RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                        (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""), 0,
                        cust.cust-no,
                        OUTPUT v-basis).
           
                    IF FIRST-OF(tt-report.inv-no) THEN
                    DO:
                        IF FIRST(ar-invl.inv-no) THEN
                            ASSIGN
                                v-amt      = 0
                                v-amt[1]   = ar-inv.tax-amt +
                               (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                                v-inv-full = v-amt[1]
                                v-com      = 0
                                v-com-2    = 0
                                v-inv-date = ar-inv.inv-date.
                
   
                        ASSIGN
                            v-inv-full = v-inv-full + ar-invl.amt.

                        IF NOT tb_prep AND ar-invl.misc THEN v-misc = v-misc + ar-invl.amt.
                        ELSE 
                        DO:
                            ASSIGN
                                v-amt[1]    = v-amt[1] + ar-invl.amt
                                v-tmp-amt-1 = v-amt[1].
    
                            IF ar-invl.sman[1] NE "" THEN
                            DO i = 1 TO 3:
                                IF tt-report.key-01 EQ ar-invl.sman[i] THEN 
                                DO:
                                    ASSIGN
                                        v-amt[2] = v-amt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100)
                                        v-com    = v-com +
                                      (((ar-invl.amt - IF v-basis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                                      ar-invl.s-pct[i] / 100) * ar-invl.s-comm[i] / 100).
                                    LEAVE.
                                END.
                            END.
               
                            ELSE
                                ASSIGN
                                    v-amt[2] = v-amt[2] + ar-invl.amt
                                    v-com    = v-com +
                                   ((ar-invl.amt - IF v-basis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                                   (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0)).
                    
                        END.
                    END. /*end FIRST-OF(tt-report.inv-no)*/
         
                END. /*end each ar-invl*/

            IF v-inv-found = NO THEN
                ASSIGN
                    v-amt[1]    = ar-cashl.amt-paid + v-dsc[1] - v-misc
                    v-tmp-amt-1 = v-amt[1]
                    v-amt[2]    = v-amt[1]
                    v-com       = v-amt[1] * (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0).

            ASSIGN
                v-pct    = v-amt[2] / v-tmp-amt-1
                v-amt[1] = (ar-cashl.amt-paid + v-dsc[1] - v-misc) * v-pct    /* task 02261403 */
                v-pct    = v-amt[1] / v-amt[2]
                v-com-2  = v-com * v-pct.

            RELEASE ar-inv.
        END.
    
        ELSE 
        DO:
            FIND ar-invl WHERE RECID(ar-invl) EQ tt-report.rec-id NO-LOCK.
            FIND FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
                NO-LOCK NO-ERROR.

            RUN custom/combasis.p (cocode, tt-report.key-01, cust.type,
                (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""), 0,
                cust.cust-no,
                OUTPUT v-basis).
            ASSIGN
                v-inv-full = ar-invl.amt .

            IF NOT tb_prep AND ar-invl.misc THEN NEXT.

            ASSIGN
                v-amt[1]    = ar-invl.amt 
                v-tmp-amt-1 = v-amt[1]
                v-com       = (ar-invl.amt - IF v-basis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                  (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0)
                v-com-2     = v-com.
        
        END.
    
        IF v-com-2  EQ ? THEN v-com-2 = 0.
        IF v-amt[1] EQ ? THEN
            ASSIGN v-amt[1]    = 0
                v-tmp-amt-1 = 0.
    
        ASSIGN
            v-c-%    = v-com-2 / v-amt[1] * 100
            v-amt[1] = v-tmp-amt-1. /*multiple payments against an invoice, reset v-amt[1] value*/
    
        IF v-c-% EQ ? THEN v-c-% = 0.
    
        v-paid[1] = v-amt-full.

        /* IF rd_printname = 1 THEN 
            ASSIGN vcustno = tt-report.key-10.
         ELSE
            ASSIGN vcustno = tt-report.key-09.*/

        v-aging = (IF v-check-date <> ? AND v-inv-date <> ? THEN v-check-date - v-inv-date ELSE 0).

        IF FIRST-OF(tt-report.inv-no) THEN
            v-rem-bal  = v-inv-full - v-amt-full - v-dsc[1].
        ELSE
            v-rem-bal  = v-rem-bal - v-amt-full - v-dsc[1].

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "rep"    THEN 
                    cVarValue = STRING(tt-report.key-01,"x(5)") .
                WHEN "rep-name"   THEN 
                    cVarValue = IF AVAILABLE sman THEN STRING(sman.sname,"x(30)") ELSE "".
                WHEN "cust"   THEN 
                    cVarValue = STRING(tt-report.key-09,"x(8)").
                WHEN "cust-name"  THEN 
                    cVarValue = STRING(tt-report.key-10,"x(30)") .
                WHEN "term"   THEN 
                    cVarValue = STRING(tt-report.key-08,"x(5)") .
                WHEN "invdate"  THEN 
                    cVarValue = IF v-inv-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-inv-date) ELSE "" .
                WHEN "chk-date"   THEN 
                    cVarValue = IF v-check-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-check-date) ELSE "" .
                WHEN "aging"  THEN 
                    cVarValue = STRING(v-aging,"->>,>>9.99") .

                WHEN "inv"   THEN 
                    cVarValue = IF AVAILABLE ar-cashl THEN  STRING(ar-cashl.inv-no,">>>>>>>>") ELSE IF AVAILABLE ar-inv THEN  STRING(ar-inv.inv-no,">>>>>>>>") ELSE "".
                WHEN "inv-amt"   THEN 
                    cVarValue = IF FIRST-OF(tt-report.inv-no) THEN STRING(v-inv-full,"->>>,>>>,>>9.99") ELSE "".
                WHEN "amt-paid"  THEN 
                    cVarValue = STRING(v-amt-full,"->>>,>>>,>>9.99") .
                WHEN "disc"   THEN 
                    cVarValue = STRING(v-dsc[1],"->>>>,>>9.99") .
                WHEN "bal-af"  THEN 
                    cVarValue = STRING(v-rem-bal,"->>>,>>>,>>9.99") .
                WHEN "comm%"   THEN 
                    cVarValue = STRING(v-c-%,"->>9.99") .
                WHEN "comm"  THEN 
                    cVarValue = STRING(v-com-2,"->>,>>>,>>9.99") .
                         
            END CASE.
                      
            cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
          
        PUT UNFORMATTED cDisplay SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.

     
        /* if v-cust THEN
         DO:
           display tt-report.key-01             format "x(3)"       label "Slsmn"
                     when first-of(tt-report.key-01)
                   vcustno  @ tt-report.key-09         format "x(20)"      label "Customer"
                   tt-report.key-08 FORM "x(5)" /*WHEN tb_printterms*/         LABEL "Terms"
                   v-inv-date                                    LABEL "Inv Date" FORM "99/99/99"
                   v-check-date /*WHEN tb_printcheckdt */            LABEL "Check Date" FORM "99/99/99"
                   v-aging /*WHEN tb_printaging*/                    LABEL "Aging"
                   ar-cashl.inv-no    when avail ar-cash         label "Invoice#"
                   ar-inv.inv-no   when avail ar-inv   @ ar-cashl.inv-no
                   v-inv-full WHEN FIRST-OF(tt-report.inv-no)    label "Inv Amount"
                   v-amt-full                                    label "Amt Paid"
                   v-dsc[1]                                      label "Discount"
                   v-rem-bal /*WHEN tb_rembalance */                 LABEL "Bal Aft Pymt"
                   v-c-%  format "->>9.99"                       label "Comm%"
                   v-com-2                                       label "Comm"
               with frame detail1 no-box no-attr-space stream-io down width 200.
     
           /*IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' (IF first-of(tt-report.key-01) THEN tt-report.key-01
                      ELSE "")                                         '",'
                 '"' vcustno                                           '",'
                 '"' (IF tb_printterms THEN tt-report.key-08 ELSE "")            '",'
                 '"' (IF tb_printcheckdt AND v-inv-date NE ? THEN
                         STRING(v-inv-date,"99/99/99") ELSE "")        '",'
                 '"' (IF tb_printcheckdt AND v-check-date NE ? THEN STRING(v-check-date,"99/99/99")
                      ELSE "")                                         '",'
                 '"' (IF tb_printaging THEN string(v-aging) ELSE "")   '",'
                 '"' (IF AVAIL ar-cash THEN STRING(ar-cashl.inv-no)
                      ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                      ELSE "")                                         '",'
                 '"' (IF first-of(tt-report.inv-no) THEN STRING(v-inv-full,"->>,>>>,>>9.99")
                      ELSE "")                                         '",'
                 '"' STRING(v-amt-full,"->>,>>>,>>9.99")               '",'
                 '"' STRING(v-dsc[1],"->>,>>>,>>9.99")                 '",'
                 '"' (IF tb_rembalance THEN STRING(v-rem-bal,"->>,>>>,>>9.99")
                      ELSE "")                                         '",'
                 '"'  STRING(v-c-%,"->>9.99")                          '",'
                 '"'  STRING(v-com-2,"->>,>>>,>>9.99")                   '",'
                SKIP.*/
         END.
         else
         DO:
           display tt-report.key-01         format "x(3)"   label "Slsmn"
                     when first-of(tt-report.key-01)
                   sman.sname                            label "Name"
                     when first-of(tt-report.key-01) and avail sman
                   vcustno @ cust.NAME   format "x(20)"  label "Customer"
                   tt-report.key-08 FORM "x(5)" /*WHEN tb_printterms*/ LABEL "Terms"
                   v-inv-date                            LABEL "Inv Date" FORM "99/99/99"
                   v-check-date /*WHEN tb_printcheckdt*/     LABEL "Check Date" FORM "99/99/99"
                   v-aging /*WHEN tb_printaging */           LABEL "Aging"
                   ar-cashl.inv-no    when avail ar-cash label "Invoice#"
                   ar-inv.inv-no   when avail ar-inv   @ ar-cashl.inv-no
                   v-inv-full WHEN first-of(tt-report.inv-no) label "Inv Amount"
                   v-amt-full                            label "Amt Paid"
                   v-dsc[1]                              label "Discount"
                   v-rem-bal /*WHEN tb_rembalance*/          LABEL "Bal Aft Pymt"
               with frame detail2 no-box no-attr-space stream-io down width 200.
     
           /*IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' (IF first-of(tt-report.key-01) THEN tt-report.key-01
                      ELSE "")                                         '",'
                 '"' (IF FIRST-OF(tt-report.key-01) AND AVAIL sman THEN
                         sman.sname ELSE "")                           '",'
                 '"' vcustno                                           '",'
                 '"' (IF tb_printterms THEN tt-report.key-08 ELSE "")            '",'
                 '"' (IF v-inv-date NE ? THEN STRING(v-inv-date,"99/99/99")
                      ELSE "")                                         '",'
                 '"' (IF tb_printcheckdt AND v-check-date NE ? THEN
                         STRING(v-check-date,"99/99/99") ELSE "")      '",'
                 '"' (IF tb_printaging THEN string(v-aging) ELSE "")   '",'
                 '"' (IF AVAIL ar-cash THEN STRING(ar-cashl.inv-no)
                      ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                      ELSE "")                                         '",'
                 '"' (IF first-of(tt-report.inv-no) THEN STRING(v-inv-full,"->>,>>>,>>9.99")
                      ELSE "")                                         '",'
                 '"' STRING(v-amt-full,"->>,>>>,>>9.99")               '",'
                 '"' STRING(v-dsc[1],"->>,>>>,>>9.99")                 '",'
                 '"' (IF tb_rembalance THEN STRING(v-rem-bal,"->>,>>>,>>9.99")
                      ELSE "")                                         '",'
                SKIP.*/
         END.*/
          
        ASSIGN
            v-tot-paid[1] = v-tot-paid[1] + v-paid[1]
            v-tot-dsc[1]  = v-tot-dsc[1] + v-dsc[1]
            v-tot-amt[1]  = v-tot-amt[1] + v-inv-full
            v-tot-com[1]  = v-tot-com[1] + v-com-2.

        IF LAST-OF(tt-report.inv-no) THEN
            v-tot-rem[1] = v-tot-rem[1] + v-rem-bal.
     
        IF LAST-OF(tt-report.key-02) THEN 
        DO:
            IF v-cust THEN 
            DO:
       
                v-c-% = v-tot-com[1] / v-tot-amt[1] * 100.
        
                IF v-c-% EQ ? THEN v-c-% = 0.

                /*display "Customer Totals:" @ tt-report.key-09
                        v-tot-paid[1]      @ v-amt-full
                        v-tot-dsc[1]       @ v-dsc[1]
                        v-tot-amt[1]       @ v-inv-full
                        v-tot-rem[1] /*WHEN tb_rembalance*/  @ v-rem-bal
                        v-c-%
                        v-tot-com[1]       @ v-com-2
                    with frame detail1.*/
                PUT str-line SKIP .
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "rep"    THEN 
                            cVarValue = "" .
                        WHEN "rep-name"   THEN 
                            cVarValue = "".
                        WHEN "cust"   THEN 
                            cVarValue = "".
                        WHEN "cust-name"  THEN 
                            cVarValue = "" .
                        WHEN "term"   THEN 
                            cVarValue = "" .
                        WHEN "invdate"  THEN 
                            cVarValue =  "" .
                        WHEN "chk-date"   THEN 
                            cVarValue = "" .
                        WHEN "aging"  THEN 
                            cVarValue = "" .
                        WHEN "inv"   THEN 
                            cVarValue = "".
                        WHEN "inv-amt"   THEN 
                            cVarValue =  STRING(v-tot-amt[1],"->>>,>>>,>>9.99").
                        WHEN "amt-paid"  THEN 
                            cVarValue = STRING(v-tot-paid[1],"->>>,>>>,>>9.99") .
                        WHEN "disc"   THEN 
                            cVarValue = STRING(v-tot-dsc[1],"->>>>,>>9.99") .
                        WHEN "bal-af"  THEN 
                            cVarValue = STRING(v-tot-rem[1],"->>>,>>>,>>9.99") .
                        WHEN "comm%"   THEN 
                            cVarValue = STRING(v-c-%,"->>9.99") .
                        WHEN "comm"  THEN 
                            cVarValue = STRING(v-tot-com[1],"->>,>>>,>>9.99") .
                         
                    END CASE.
                      
                    cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
          
                PUT UNFORMATTED 
                    "           Customer Totals: "  SUBSTRING(cDisplay,29,300) SKIP.
                IF tb_excel THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        ' Customer Totals: ,' 
                        SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.
          
                IF NOT LAST-OF(tt-report.key-01) THEN PUT SKIP(1).
            END.
    
            ASSIGN
                v-tot-paid[2] = v-tot-paid[2] + v-tot-paid[1]
                v-tot-dsc[2]  = v-tot-dsc[2] + v-tot-dsc[1]
                v-tot-amt[2]  = v-tot-amt[2] + v-tot-amt[1]
                v-tot-rem[2]  = v-tot-rem[2] + v-tot-rem[1]
                v-tot-com[2]  = v-tot-com[2] + v-tot-com[1] .

            IF tt-report.chk-inv EQ TRUE THEN ASSIGN
                    v-tot-paid[4] = v-tot-paid[4] + v-tot-paid[1].
            ASSIGN
                v-tot-dsc[4] = v-tot-dsc[4] + v-tot-dsc[1]
                v-tot-amt[4] = v-tot-amt[4] + v-tot-amt[1]
                v-tot-rem[4] = v-tot-rem[4] + v-tot-rem[1]
                v-tot-com[4] = v-tot-com[4] + v-tot-com[1] .


            ASSIGN
                v-tot-paid[1] = 0
                v-tot-dsc[1]  = 0
                v-tot-amt[1]  = 0
                v-tot-rem[1]  = 0
                v-tot-com[1]  = 0.

        END. /* last of key-02 */

        IF LAST-OF(tt-report.key-01) THEN 
        DO:
     
            v-c-% = v-tot-com[2] / v-tot-amt[2] * 100.
      
            IF v-c-% EQ ? THEN v-c-% = 0.

            /* if v-cust THEN
             DO:
               display "SalesRep Totals:" @ tt-report.key-09
                       v-tot-paid[2]      @ v-amt-full
                       v-tot-dsc[2]       @ v-dsc[1]
                       v-tot-amt[2]       @ v-inv-full
                       v-tot-rem[2] /*WHEN tb_rembalance*/  @ v-rem-bal
                       v-c-%
                       v-tot-com[2]       @ v-com-2
                   with frame detail1.
       
               IF tb_excel THEN
                 RUN sales-total-excel-1(v-tot-paid[2], v-tot-dsc[2], v-tot-amt[2], v-tot-rem[2], v-c-%, v-tot-com[2]).
             END.
                         
             else
             DO:
               display "SalesRep Totals:" @ cust.NAME
                       v-tot-paid[2]      @ v-amt-full
                       v-tot-dsc[2]       @ v-dsc[1]
                       v-tot-amt[2]       @ v-inv-full
                       v-tot-rem[2] /*WHEN tb_rembalance*/  @ v-rem-bal
                   with frame detail2.
       
               IF tb_excel THEN
                 RUN sales-total-excel-2(v-tot-paid[2], v-tot-dsc[2], v-tot-amt[2], v-tot-rem[2]).
             END.*/
            PUT str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rep"    THEN 
                        cVarValue = "" .
                    WHEN "rep-name"   THEN 
                        cVarValue = "".
                    WHEN "cust"   THEN 
                        cVarValue = "".
                    WHEN "cust-name"  THEN 
                        cVarValue = "" .
                    WHEN "term"   THEN 
                        cVarValue = "" .
                    WHEN "invdate"  THEN 
                        cVarValue =  "" .
                    WHEN "chk-date"   THEN 
                        cVarValue = "" .
                    WHEN "aging"  THEN 
                        cVarValue = "" .
                    WHEN "inv"   THEN 
                        cVarValue = "".
                    WHEN "inv-amt"   THEN 
                        cVarValue =  STRING(v-tot-amt[2],"->>>,>>>,>>9.99").
                    WHEN "amt-paid"  THEN 
                        cVarValue = STRING(v-tot-paid[2],"->>>,>>>,>>9.99") .
                    WHEN "disc"   THEN 
                        cVarValue = STRING(v-tot-dsc[2],"->>>>,>>9.99") .
                    WHEN "bal-af"  THEN 
                        cVarValue = STRING(v-tot-rem[2],"->>>,>>>,>>9.99") .
                    WHEN "comm%"   THEN 
                        cVarValue = STRING(v-c-%,"->>9.99") .
                    WHEN "comm"  THEN 
                        cVarValue = STRING(v-tot-com[2],"->>,>>>,>>9.99") .
                         
                END CASE.
                      
                cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED 
                "           SalesRep Totals: "  SUBSTRING(cDisplay,29,300) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    ' SalesRep Totals: ,' 
                    SUBSTRING(cExcelDisplay,4,300) SKIP.
            END.

            PUT SKIP(2).
    
            ASSIGN
                v-tot-paid[3] = v-tot-paid[3] + v-tot-paid[4]
                v-tot-dsc[3]  = v-tot-dsc[3] + v-tot-dsc[4]
                v-tot-amt[3]  = v-tot-amt[3] + v-tot-amt[4]
                v-tot-rem[3]  = v-tot-rem[3] + v-tot-rem[4]
                v-tot-com[3]  = v-tot-com[3] + v-tot-com[4] .
            ASSIGN   
                v-tot-paid[2] = 0
                v-tot-dsc[2]  = 0
                v-tot-amt[2]  = 0
                v-tot-rem[2]  = 0
                v-tot-com[2]  = 0.

            ASSIGN   
                v-tot-paid[4] = 0
                v-tot-dsc[4]  = 0
                v-tot-amt[4]  = 0
                v-tot-rem[4]  = 0
                v-tot-com[4]  = 0.

            IF sortby = "Salesrep#" THEN 
                PAGE.                  
        END. /* last of key-01 */

        IF LAST(tt-report.key-01) THEN 
        DO:
    
      
            v-c-% = v-tot-com[3] / v-tot-amt[3] * 100.
      
            IF v-c-% EQ ? THEN v-c-% = 0.

            /*  if v-cust then
              DO:
                display "   Grand Totals:" @ tt-report.key-09
                        v-tot-paid[3]      @ v-amt-full
                        v-tot-dsc[3]       @ v-dsc[1]
                        v-tot-amt[3]       @ v-inv-full
                        v-tot-rem[3] /*WHEN tb_rembalance*/  @ v-rem-bal
                        v-c-%
                        v-tot-com[3]       @ v-com-2
                    with frame detail1.
        
                IF tb_excel THEN
                  RUN gt-excel-1(v-tot-paid[3], v-tot-dsc[3], v-tot-amt[3], v-tot-rem[3], v-c-%, v-tot-com[3]).
                
              END.
              else
              DO:
                display "   Grand Totals:" @ cust.NAME
                        v-tot-paid[3]      @ v-amt-full
                        v-tot-dsc[3]       @ v-dsc[1]
                        v-tot-amt[3]       @ v-inv-full
                        v-tot-rem[3] /*WHEN tb_rembalance*/ @ v-rem-bal
                    with frame detail2.
        
                IF tb_excel THEN
                  RUN gt-excel-2(v-tot-paid[3], v-tot-dsc[3], v-tot-amt[3], v-tot-rem[3]).
              END.*/
            PUT str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rep"    THEN 
                        cVarValue = "" .
                    WHEN "rep-name"   THEN 
                        cVarValue = "".
                    WHEN "cust"   THEN 
                        cVarValue = "".
                    WHEN "cust-name"  THEN 
                        cVarValue = "" .
                    WHEN "term"   THEN 
                        cVarValue = "" .
                    WHEN "invdate"  THEN 
                        cVarValue =  "" .
                    WHEN "chk-date"   THEN 
                        cVarValue = "" .
                    WHEN "aging"  THEN 
                        cVarValue = "" .
                    WHEN "inv"   THEN 
                        cVarValue = "".
                    WHEN "inv-amt"   THEN 
                        cVarValue =  STRING(v-tot-amt[3],"->>>,>>>,>>9.99").
                    WHEN "amt-paid"  THEN 
                        cVarValue = STRING(v-tot-paid[3],"->>>,>>>,>>9.99") .
                    WHEN "disc"   THEN 
                        cVarValue = STRING(v-tot-dsc[3],"->>>>,>>9.99") .
                    WHEN "bal-af"  THEN 
                        cVarValue = STRING(v-tot-rem[3],"->>>,>>>,>>9.99") .
                    WHEN "comm%"   THEN 
                        cVarValue = STRING(v-c-%,"->>9.99") .
                    WHEN "comm"  THEN 
                        cVarValue = STRING(v-tot-com[3],"->>,>>>,>>9.99") .
                         
                END CASE.
                      
                cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED 
                "              Grand Totals: "  SUBSTRING(cDisplay,29,300) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    ' Grand Totals: ,' 
                    SUBSTRING(cExcelDisplay,4,300) SKIP.
            END.


        END.

        DELETE tt-report.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sales-total-excel-1 C-Win 
PROCEDURE sales-total-excel-1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-paid AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dsc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-amt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-rem AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-perc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-com AS DECIMAL NO-UNDO.

/*PUT STREAM excel UNFORMATTED
    '"' ""                                         '",'
    '"' "SalesRep Totals:"                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' ""                                         '",'
    '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
    '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
    '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
    '"' (IFtb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "") '",'  
    '"' STRING(ip-perc,"->>9.99")            '",'
    '"' STRING(ip-com,"->>,>>>,>>9.99")      '",'
    SKIP.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sales-total-excel-2 C-Win 
PROCEDURE sales-total-excel-2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-paid AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dsc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-amt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-rem AS DECIMAL NO-UNDO.

/*PUT STREAM excel UNFORMATTED
     '"' ""                                         '",'
     '"' "SalesRep Totals:"                         '",'
     '"' ""                                         '",'
     '"' ""                                         '",'
     '"' ""                                         '",'
     '"' ""                                         '",'
     '"' ""                                         '",'
     '"' ""                                         '",'
     '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
     '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
     '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
     '"' (IF tb_rembalance THEN STRING(ip-rem,"->>,>>>,>>9.99") ELSE "")  '",'
    SKIP.*/
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
            fi_file:SCREEN-VALUE = "c:\tmp\SalesRepName.csv".    
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

