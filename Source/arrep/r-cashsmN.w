&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-cashsm.w

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

DEFINE VARIABLE is-xprint-form AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file    AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

DEFINE VARIABLE str-line           AS CHARACTER FORM "x(300)" NO-UNDO.

DEFINE BUFFER b-itemfg FOR itemfg .

ASSIGN 
    cTextListToSelect  = "Slsmn,Sales Name,Customer,Cust Name,Date,Invoice,Paid,Discount,Amount," +
                           "Comm%,Comm"
    cFieldListToSelect = "sman,sman-name,cust,cust-name,date,inv,paid,disc,amt," +
                            "comm%,comm"
    cFieldLength       = "5,25,8,25,10,8,14,14,14," + "7,14"
    cFieldType         = "c,c,c,c,c,i,i,i,i," + "i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Slsmn,Sales Name,Customer,Date,Invoice,Paid,Discount,Amount," +
                           "Comm%,Comm" .

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
begin_slsmn end_slsmn rd_sort tb_tdisc days-old sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_slsmn end_slsmn ~
lbl_sort rd_sort tb_tdisc lbl_tdisc lbl_days-old days-old sl_avail ~
sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-cashsm.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_days-old   AS CHARACTER FORMAT "X(256)":U INITIAL "Receipts After How Many Days" 
    VIEW-AS FILL-IN 
    SIZE 1 BY .95 NO-UNDO.

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
    SIZE 15.4 BY 3.95 NO-UNDO.

DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Invoice#" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Customer#", "Customer#",
    "Invoice#", "Invoice#"
    SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 7.14
    BGCOLOR 15 .

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

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16.6 BY .81 DROP-TARGET NO-UNDO.

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
    begin_date AT ROW 2.38 COL 27.6 COLON-ALIGNED HELP
    "Enter Beginning Receipt Date"
    end_date AT ROW 2.38 COL 71.2 COLON-ALIGNED HELP
    "Enter Ending Receipt Date"
    begin_slsmn AT ROW 3.62 COL 27.4 COLON-ALIGNED HELP
    "Enter Beginning SalesRep Number"
    end_slsmn AT ROW 3.62 COL 71.2 COLON-ALIGNED HELP
    "Enter Ending SalesRep Number"
    lbl_sort AT ROW 5 COL 27 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 5 COL 38.4 NO-LABELS
    tb_tdisc AT ROW 6.24 COL 42.2 RIGHT-ALIGNED
    lbl_tdisc AT ROW 6.24 COL 40.8 COLON-ALIGNED NO-LABELS
    lbl_days-old AT ROW 7.33 COL 6 NO-LABELS
    days-old AT ROW 7.33 COL 63.2 COLON-ALIGNED NO-LABELS
    sl_avail AT ROW 9.33 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 9.33 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 9.33 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 10.33 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 11.33 COL 40.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 12.38 COL 40.2 WIDGET-ID 40
    btn_down AT ROW 13.38 COL 40.2 WIDGET-ID 42
    lv-font-name AT ROW 15.19 COL 28.4 COLON-ALIGNED NO-LABELS
    rd-dest AT ROW 15.71 COL 4.8 NO-LABELS
    lv-ornt AT ROW 15.81 COL 22.6 NO-LABELS
    lines-per-page AT ROW 16.24 COL 84 COLON-ALIGNED
    lv-font-no AT ROW 16.43 COL 60 COLON-ALIGNED
    tb_show-parm AT ROW 17.71 COL 27.8
    fi_file AT ROW 18.67 COL 25.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.76 COL 77
    tbAutoClose AT ROW 20 COL 27.6 WIDGET-ID 16
    btn-ok AT ROW 21 COL 27.6
    btn-cancel AT ROW 21 COL 52
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 8.62 COL 60.6 WIDGET-ID 44
    "Show Only Invoices with Cash Receipts after" VIEW-AS TEXT
    SIZE 44 BY .62 AT ROW 7.57 COL 21.2
    "Days" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 7.52 COL 73.4
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 14.57 COL 4.4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5.4
    BGCOLOR 15 
    "Available Columns" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 8.62 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 14.95 COL 3
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 94.8 BY 21.62
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
        TITLE              = "Cash Receipts by Sales Rep"
        HEIGHT             = 21.62
        WIDTH              = 94.8
        MAX-HEIGHT         = 24.95
        MAX-WIDTH          = 100.8
        VIRTUAL-HEIGHT     = 24.95
        VIRTUAL-WIDTH      = 100.8
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
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

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

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lbl_tdisc IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_tdisc:PRIVATE-DATA IN FRAME FRAME-A = "tb_tdisc".

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
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    tb_show-parm:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_tdisc IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_tdisc:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cash Receipts by Sales Rep */
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
ON WINDOW-CLOSE OF C-Win /* Cash Receipts by Sales Rep */
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


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
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

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "AR9" }
    ASSIGN
        tb_show-parm:SENSITIVE = lShowParameters
        tb_show-parm:HIDDEN    = NOT lShowParameters
        tb_show-parm:VISIBLE   = lShowParameters
        .
  
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_date.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-total C-Win 
PROCEDURE display-total :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER vname AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ip-paid AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-dsc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-amt AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-perc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-com AS DECIMAL NO-UNDO.

    DEFINE VARIABLE cDisplay       AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS cha    NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHA    NO-UNDO.
    DEFINE VARIABLE cVarValue      AS cha    NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS cha    NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS cha    NO-UNDO.
    DEFINE VARIABLE cFieldName     AS cha    NO-UNDO.

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "sman"    THEN 
                cVarValue = "" .
            WHEN "sman-name"   THEN 
                cVarValue = "".
            WHEN "cust"   THEN 
                cVarValue = "".
            WHEN "cust-name"   THEN 
                cVarValue = "".
            WHEN "date"  THEN 
                cVarValue =  "" .
            WHEN "inv"   THEN 
                cVarValue = "" .
            WHEN "paid"  THEN 
                cVarValue = STRING(ip-paid,"->>,>>>,>>9.99") .
            WHEN "disc"  THEN 
                cVarValue = STRING(ip-dsc,"->>,>>>,>>9.99") .
            WHEN "amt"   THEN 
                cVarValue = STRING(ip-amt,"->>,>>>,>>9.99") .
            WHEN "comm%"  THEN 
                cVarValue = STRING(ip-perc,"->>9.99") .
            WHEN "comm"  THEN 
                cVarValue = STRING(ip-com,"->>,>>>,>>9.99") .
        END CASE.

        cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
    PUT str-line SKIP .
    PUT UNFORMATTED 
        "           " vname SUBSTRING(cDisplay,33,300) SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            '                     ' vname ' ,' SUBSTRING(cExcelDisplay,4,300) SKIP.
    END.
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
    DISPLAY begin_date end_date begin_slsmn end_slsmn lbl_sort rd_sort tb_tdisc 
        lbl_tdisc lbl_days-old days-old sl_avail sl_selected rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_slsmn end_slsmn rd_sort 
        tb_tdisc days-old sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok 
        btn-cancel 
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
    DEFINE INPUT PARAMETER ip-perc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-com AS DECIMAL NO-UNDO.

    PUT STREAM excel UNFORMATTED
        '"' ""                                         '",'
        '"' "Grand Totals:"                            '",'
        '"' ""                                         '",'
        '"' ""                                         '",'
        '"' STRING(ip-paid,"->>,>>>,>>9.99")           '",'
        '"' STRING(ip-dsc,"->>,>>>,>>9.99")            '",'
        '"' STRING(ip-amt,"->>,>>>,>>9.99")            '",'
        '"' STRING(ip-perc,"->>9.99")                  '",'
        '"' STRING(ip-com,"->>,>>>,>>9.99")         '",'
        SKIP.
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

    PUT STREAM excel UNFORMATTED
        '"' ""                                   '",'
        '"' "Grand Totals:"                      '",'
        '"' ""                                   '",'
        '"' ""                                   '",'
        '"' ""                                   '",'
        '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
        '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
        '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
        SKIP.
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
            fi_file:SCREEN-VALUE = "c:\tmp\r-cashsm.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    SESSION:SET-WAIT-STATE("general").
    /*{sys/form/r-top3.f}*/

    DEFINE VARIABLE fdate          AS DATE      FORMAT "99/99/9999" INIT "01/01/0001" NO-UNDO.
    DEFINE VARIABLE tdate          LIKE fdate INIT 12/31/9999.
    DEFINE VARIABLE fsman          AS CHARACTER FORMAT "x(3)" NO-UNDO.
    DEFINE VARIABLE tsman          LIKE fsman INIT "zzz".
    DEFINE VARIABLE v-cust         AS LOG       FORMAT "Customer/Invoice" INIT NO NO-UNDO.
    DEFINE VARIABLE v-disc         AS LOG       FORMAT "Include/Exclude" INIT YES NO-UNDO.
    DEFINE VARIABLE v-days         AS INTEGER   FORMAT ">>>>" INIT 0 NO-UNDO.

    DEFINE VARIABLE v-sman         AS CHARACTER.
    DEFINE VARIABLE v-amt          LIKE ar-cashl.amt-paid EXTENT 2.
    DEFINE VARIABLE v-paid         LIKE v-amt.
    DEFINE VARIABLE v-dsc          LIKE v-amt.
    DEFINE VARIABLE v-com          LIKE ar-cashl.amt-paid.
    DEFINE VARIABLE v-c-%          AS DECIMAL.
    DEFINE VARIABLE v-tax          AS DECIMAL.
    DEFINE VARIABLE v-pct          AS DECIMAL.
    DEFINE VARIABLE v-basis        LIKE sman.commbasis INIT "" NO-UNDO.

    DEFINE VARIABLE v-tot-amt      AS DECIMAL   EXTENT 3.
    DEFINE VARIABLE v-tot-com      LIKE v-tot-amt.
    DEFINE VARIABLE v-tot-dsc      LIKE v-tot-amt.
    DEFINE VARIABLE v-tot-paid     LIKE v-tot-amt.


    DEFINE VARIABLE i              AS INTEGER.
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

    {sys/form/r-top5DL2.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFileName   LIKE fi_file NO-UNDO .

    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    ASSIGN  
        fdate  = begin_date
        tdate  = end_date
        fsman  = begin_slsmn
        tsman  = end_slsmn
        v-cust = rd_sort EQ "Customer#"
        v-disc = tb_tdisc
        v-days = days-old.

    ASSIGN
        str-line = ""
        str-tit  = coname + " - " + loname
        str-tit3 = "Receipt Date: " + string(fdate,"99/99/9999") + " - " +
                                 string(tdate,"99/99/9999") +
              fill(" ",4) + "Sales Rep: " + fsman + " - " + tsman
        x        = (80 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3 
        str-tit2 = c-win:TITLE /*"CASH RECEIPTS BY SALESREP"*/
        {sys/inc/ctrtext.i str-tit  90}
        {sys/inc/ctrtext.i str-tit2 90}
        {sys/inc/ctrtext.i str-tit3 114}.

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

        IF LOOKUP(ttRptSelected.TextList, "Paid,Discount,Amount,Comm%,Comm") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        /* IF v-cust THEN
           excelheader = "Slsmn,Customer,Date,Invoice,Paid,Discount,Amount,Comm%,Comm".
         ELSE
           excelheader = "Slsmn,Name,Customer,Date,Invoice,Paid,Discount,Amount".*/

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF tb_show-parm THEN RUN show-param.

    DISPLAY str-tit WITH FRAME r-top STREAM-IO.

    {sa/sa-sls01.i}

    FOR EACH cust WHERE cust.company EQ cocode NO-LOCK:
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

                {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

                DO i = 1 TO 3:
                    v-sman = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                    ELSE ar-invl.sman[i].

                    IF v-sman   LT fsman                         OR
                        v-sman   GT tsman                         OR
                        (i NE 1 AND
                        (v-sman EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                    FIND FIRST report
                        WHERE report.term-id EQ v-term
                        AND report.key-01  EQ v-sman
                        AND report.key-02  EQ string(ar-invl.inv-no,"9999999999")
                        AND report.rec-id  EQ recid(ar-invl)
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE report THEN 
                    DO:
                        CREATE report.
                        ASSIGN
                            report.term-id = v-term
                            report.key-01  = v-sman
                            report.key-02  = STRING(ar-invl.inv-no,"9999999999")
                            report.key-03  = cust.cust-no
                            report.key-09  = cust.cust-no
                            report.rec-id  = RECID(ar-invl).
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
            AND (v-days         EQ 0 OR
            (ar-cash.check-date - ar-cashl.inv-date GT v-days AND
            ar-cashl.inv-no NE 0))
            NO-LOCK

            TRANSACTION:

            {custom/statusMsg.i " 'Processing Customer#  '  + string(ar-cash.cust-no) "}

            IF ar-cashl.inv-no NE 0 THEN
                FOR EACH ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    NO-LOCK:

                    {custom/statusMsg.i " 'Processing Customer#  '  + string(ar-invl.cust-no) "}

                    DO i = 1 TO 3:
                        v-sman = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                        ELSE ar-invl.sman[i].

                        IF v-sman  LT fsman                          OR
                            v-sman  GT tsman                          OR
                            (i NE 1 AND
                            (v-sman EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

                        FIND FIRST report
                            WHERE report.term-id EQ v-term
                            AND report.key-01  EQ v-sman
                            AND report.key-02  EQ string(ar-invl.inv-no,"9999999999")
                            AND report.rec-id  EQ recid(ar-cashl)
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE report THEN 
                        DO:
                            CREATE report.
                            ASSIGN
                                report.term-id = v-term
                                report.key-01  = v-sman
                                report.key-02  = STRING(ar-invl.inv-no,"9999999999")
                                report.key-03  = cust.cust-no
                                report.key-09  = cust.cust-no
                                report.rec-id  = RECID(ar-cashl).
                        END.
                    END.
                END.

            ELSE
                IF cust.sman GE fsman AND
                    cust.sman LE tsman THEN 
                DO:
                    v-sman = cust.sman.

                    FIND FIRST report
                        WHERE report.term-id EQ v-term
                        AND report.key-01  EQ v-sman
                        AND report.key-02  EQ string(ar-cashl.inv-no,"9999999999")
                        AND report.rec-id  EQ recid(ar-cashl)
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE report THEN 
                    DO:
                        CREATE report.

                        ASSIGN
                            report.term-id = v-term
                            report.key-01  = v-sman
                            report.key-02  = STRING(ar-cashl.inv-no,"9999999999")
                            report.key-03  = cust.cust-no
                            report.key-09  = cust.cust-no
                            report.rec-id  = RECID(ar-cashl).
                    END.
                END.
        END.
    END.

    IF v-cust THEN
        FOR EACH report WHERE report.term-id EQ v-term:
            ASSIGN
                report.key-03 = report.key-02
                report.key-02 = report.key-09.
        END.

    FOR EACH report WHERE report.term-id EQ v-term,

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ report.key-09
        NO-LOCK

        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03

        TRANSACTION:

        {custom/statusMsg.i " 'Processing Customer#  '  + string(cust.cust-no) "}

        FIND FIRST sman
            WHERE sman.company EQ cocode
            AND sman.sman    EQ report.key-01
            NO-LOCK NO-ERROR.

        RELEASE ar-inv.
        RELEASE ar-cash.

        FIND ar-cashl WHERE RECID(ar-cashl) EQ report.rec-id NO-LOCK NO-ERROR.    

        IF AVAILABLE ar-cashl THEN 
        DO:
            FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

            ASSIGN
                v-dsc[1] = IF v-disc THEN ar-cashl.amt-disc ELSE 0
                v-amt[1] = ar-cashl.amt-paid + v-dsc[1]
                v-amt[2] = v-amt[1]
                v-com    = v-amt[1] * 
                  (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0).

            IF ar-cashl.inv-no NE 0 THEN
                FOR EACH ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    NO-LOCK,

                    FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK

                    BREAK BY ar-invl.inv-no:

                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ ar-invl.i-no
                        NO-LOCK NO-ERROR.

                    RUN custom/combasis.p (cocode, report.key-01, cust.type,
                        (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""), 0,
                        cust.cust-no,
                        OUTPUT v-basis).

                    IF FIRST(ar-invl.inv-no) THEN
                        ASSIGN
                            v-amt    = 0
                            v-amt[1] = ar-inv.tax-amt +
                      (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                            v-com    = 0.

                    v-amt[1] = v-amt[1] + ar-invl.amt.

                    IF ar-invl.sman[1] NE "" THEN
                    DO i = 1 TO 3:
                        IF report.key-01 EQ ar-invl.sman[i] THEN 
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

            ASSIGN
                v-pct    = v-amt[2] / v-amt[1]
                v-amt[1] = (ar-cashl.amt-paid + v-dsc[1]) * v-pct
                v-pct    = v-amt[1] / v-amt[2]
                v-com    = v-com * v-pct.

            RELEASE ar-inv.
        END.

        ELSE 
        DO:
            FIND ar-invl WHERE RECID(ar-invl) EQ report.rec-id NO-LOCK.
            FIND FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ ar-invl.i-no
                NO-LOCK NO-ERROR.

            RUN custom/combasis.p (cocode, report.key-01, cust.type,
                (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""), 0,
                cust.cust-no,
                OUTPUT v-basis).
            ASSIGN
                v-amt[1] = ar-invl.amt
                v-com    = (ar-invl.amt - IF v-basis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                  (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0).
        END.

        IF v-com    EQ ? THEN v-com    = 0.
        IF v-amt[1] EQ ? THEN v-amt[1] = 0.

        v-c-% = v-com / v-amt[1] * 100.

        IF v-c-% EQ ? THEN v-c-% = 0.

        v-paid[1] = v-amt[1] - v-dsc[1].

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "sman"    THEN 
                    cVarValue = STRING(report.key-01,"x(5)") .
                WHEN "sman-name"   THEN 
                    cVarValue = STRING(sman.sname,"x(25)").
                WHEN "cust"   THEN 
                    cVarValue = STRING(report.key-09,"x(8)").
                WHEN "cust-name"   THEN 
                    cVarValue = STRING(cust.NAME,"x(25)").
                WHEN "date"  THEN 
                    cVarValue = IF AVAILABLE ar-cash THEN DYNAMIC-FUNCTION("sfFormat_Date",ar-cash.check-date)
                                ELSE IF AVAILABLE ar-inv THEN DYNAMIC-FUNCTION("sfFormat_Date",ar-inv.inv-date) ELSE "" .
                WHEN "inv"   THEN 
                    cVarValue = IF AVAILABLE ar-cash THEN STRING(ar-cashl.inv-no,">>>>>>>>") ELSE IF AVAILABLE ar-inv THEN STRING(ar-inv.inv-no,">>>>>>>>") ELSE "" .
                WHEN "paid"  THEN 
                    cVarValue = STRING(v-paid[1],"->>,>>>,>>9.99") .
                WHEN "disc"  THEN 
                    cVarValue = STRING(v-dsc[1],"->>,>>>,>>9.99") .
                WHEN "amt"   THEN 
                    cVarValue = STRING(v-amt[1],"->>,>>>,>>9.99") .
                WHEN "comm%"  THEN 
                    cVarValue = STRING(v-c-%,"->>9.99") .
                WHEN "comm"  THEN 
                    cVarValue = STRING(v-com,"->>,>>>,>>9.99") .
            END CASE.

            cExcelVarValue = DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cVarValue).
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED cDisplay SKIP.
        IF rd-dest = 3 THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                cExcelDisplay SKIP.
        END.

        /* if v-cust THEN
         DO:
           display report.key-01             format "x(3)"       label "Slsmn"
                     when first-of(report.key-01)
                   report.key-09             format "x(16)"      label "Customer"
                   ar-cash.check-date when avail ar-cash         label "Date" FORM "99/99/99"
                     ar-inv.inv-date when avail ar-inv   @ ar-cash.check-date FORM "99/99/99"
                   ar-cashl.inv-no    when avail ar-cash         label "Invoice"
                     ar-inv.inv-no   when avail ar-inv   @ ar-cashl.inv-no
                   v-paid[1]                                     label "Paid"
                   v-dsc[1]                                      label "Discount"
                   v-amt[1]                                      label "Amount"
                   v-c-%                     format "->>9.99"    label "Comm%"
                   v-com                                         label "Comm"
               with frame detail1 no-box no-attr-space stream-io down width 200.
     
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' (IF first-of(report.key-01) THEN report.key-01
                      ELSE "")                                         '",'
                 '"' report.key-09                                     '",'
                 '"' (IF avail ar-cash AND ar-cash.check-date NE ? THEN
                         STRING(ar-cash.check-date,"99/99/99")
                      ELSE IF AVAIL ar-inv AND ar-inv.inv-date NE ? THEN
                         STRING(ar-inv.inv-date,"99/99/99")
                      ELSE "")                                         '",'
                 '"' (IF AVAIL ar-cash THEN STRING(ar-cashl.inv-no)
                      ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                      ELSE "")                                         '",'
                 '"' STRING(v-paid[1],"->>,>>>,>>9.99")                '",'
                 '"' STRING(v-dsc[1],"->>,>>>,>>9.99")                 '",'
                 '"' STRING(v-amt[1],"->>,>>>,>>9.99")                 '",'
                 '"' STRING(v-c-%,"->>9.99")                           '",'
                 '"' STRING(v-com,"->>,>>>,>>9.99")                    '",'
                SKIP.
         END.
         else
         DO:
           display report.key-01         format "x(3)"   label "Slsmn"
                     when first-of(report.key-01)
                   sman.sname                            label "Name"
                     when first-of(report.key-01) and avail sman
                   cust.name             format "x(20)"  label "Customer"
                   ar-cash.check-date when avail ar-cash label "Date" FORM "99/99/99"
                     ar-inv.inv-date when avail ar-inv   @ ar-cash.check-date FORM "99/99/99"
                   ar-cashl.inv-no    when avail ar-cash label "Invoice"
                     ar-inv.inv-no   when avail ar-inv   @ ar-cashl.inv-no
                   v-paid[1]                             label "Paid"
                   v-dsc[1]                              label "Discount"
                   v-amt[1]                              label "Amount"
               with frame detail2 no-box no-attr-space stream-io down width 200.
     
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 '"' (IF first-of(report.key-01) THEN report.key-01
                      ELSE "")                                         '",'
                 '"' (IF FIRST-OF(report.key-01) AND AVAIL sman THEN
                         sman.sname ELSE "")                           '",'
                 '"' cust.NAME                                         '",'
                 '"' (IF avail ar-cash AND ar-cash.check-date NE ? THEN
                         STRING(ar-cash.check-date,"99/99/99")
                      ELSE IF AVAIL ar-inv AND ar-inv.inv-date NE ? THEN
                         STRING(ar-inv.inv-date,"99/99/99")
                      ELSE "")                                         '",'
                 '"' (IF AVAIL ar-cash THEN STRING(ar-cashl.inv-no)
                      ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                      ELSE "")                                         '",'
                 '"' STRING(v-paid[1],"->>,>>>,>>9.99")                '",'
                 '"' STRING(v-dsc[1],"->>,>>>,>>9.99")                 '",'
                 '"' STRING(v-amt[1],"->>,>>>,>>9.99")                 '",'
                SKIP.
         END.*/

        ASSIGN
            v-tot-paid[1] = v-tot-paid[1] + v-paid[1]
            v-tot-dsc[1]  = v-tot-dsc[1] + v-dsc[1]
            v-tot-amt[1]  = v-tot-amt[1] + v-amt[1]
            v-tot-com[1]  = v-tot-com[1] + v-com.

        IF LAST-OF(report.key-02) THEN 
        DO:
            IF v-cust THEN 
            DO:
                PUT SKIP(1).

                /*clear frame detail1 no-pause.
                clear frame detail2 no-pause.*/

                v-c-% = v-tot-com[1] / v-tot-amt[1] * 100.

                IF v-c-% EQ ? THEN v-c-% = 0.

                /*display "Customer Totals:" @ report.key-09
                        v-tot-paid[1]      @ v-paid[1]
                        v-tot-dsc[1]       @ v-dsc[1]
                        v-tot-amt[1]       @ v-amt[1]
                        v-c-%
                        v-tot-com[1]       @ v-com
                    with frame detail1.*/

                RUN display-total("     Customer Totals:",v-tot-paid[1],v-tot-dsc[1],v-tot-amt[1],
                    v-c-%,v-tot-com[1]) .

                IF NOT LAST-OF(report.key-01) THEN PUT SKIP(1).

            /* IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' ""                                         '",'
                   '"' "Customer Totals:"                         '",'
                   '"' ""                                         '",'
                   '"' ""                                         '",'
                   '"' STRING(v-tot-paid[1],"->>,>>>,>>9.99")     '",'
                   '"' STRING(v-tot-dsc[1],"->>,>>>,>>9.99")      '",'
                   '"' STRING(v-tot-amt[1],"->>,>>>,>>9.99")      '",'
                   '"' STRING(v-c-%,"->>9.99")                    '",'
                   '"' STRING(v-tot-com[1],"->>,>>>,>>9.99")      '",'
                  SKIP.*/
            END.

            ASSIGN
                v-tot-paid[2] = v-tot-paid[2] + v-tot-paid[1]
                v-tot-dsc[2]  = v-tot-dsc[2] + v-tot-dsc[1]
                v-tot-amt[2]  = v-tot-amt[2] + v-tot-amt[1]
                v-tot-com[2]  = v-tot-com[2] + v-tot-com[1]

                v-tot-paid[1] = 0
                v-tot-dsc[1]  = 0
                v-tot-amt[1]  = 0
                v-tot-com[1]  = 0.
        END.

        IF LAST-OF(report.key-01) THEN 
        DO:
            PUT SKIP(1).

            /* clear frame detail1 no-pause.
             clear frame detail2 no-pause.*/

            v-c-% = v-tot-com[2] / v-tot-amt[2] * 100.

            IF v-c-% EQ ? THEN v-c-% = 0.

            /*if v-cust THEN
            DO:
              display "SalesRep Totals:" @ report.key-09
                      v-tot-paid[2]      @ v-paid[1]
                      v-tot-dsc[2]       @ v-dsc[1]
                      v-tot-amt[2]       @ v-amt[1]
                      v-c-%
                      v-tot-com[2]       @ v-com
                  with frame detail1.
      
              IF tb_excel THEN
                RUN sales-total-excel-1(v-tot-paid[2], v-tot-dsc[2], v-tot-amt[2], v-c-%, v-tot-com[2]).
            END.
      
            else
            DO:
              display "SalesRep Totals:" @ cust.name
                      v-tot-paid[2]      @ v-paid[1]
                      v-tot-dsc[2]       @ v-dsc[1]
                      v-tot-amt[2]       @ v-amt[1]
                  with frame detail2.
      
              IF tb_excel THEN
                RUN sales-total-excel-2(v-tot-paid[2], v-tot-dsc[2], v-tot-amt[2]).
            END.*/
            RUN display-total("     SalesRep Totals:",v-tot-paid[2],v-tot-dsc[2],v-tot-amt[2],
                v-c-%,v-tot-com[2]) .

            PUT SKIP(2).

            ASSIGN
                v-tot-paid[3] = v-tot-paid[3] + v-tot-paid[2]
                v-tot-dsc[3]  = v-tot-dsc[3] + v-tot-dsc[2]
                v-tot-amt[3]  = v-tot-amt[3] + v-tot-amt[2]
                v-tot-com[3]  = v-tot-com[3] + v-tot-com[2]

                v-tot-paid[2] = 0
                v-tot-dsc[2]  = 0
                v-tot-amt[2]  = 0
                v-tot-com[2]  = 0.
        END.

        IF LAST(report.key-01) THEN 
        DO:
            /*clear frame detail1 no-pause.
            clear frame detail2 no-pause.*/

            v-c-% = v-tot-com[3] / v-tot-amt[3] * 100.

            IF v-c-% EQ ? THEN v-c-% = 0.

            RUN display-total("        Grand Totals:",v-tot-paid[3],v-tot-dsc[3],v-tot-amt[3],
                v-c-%,v-tot-com[3]) .

        /*if v-cust then
        DO:
          display "   Grand Totals:" @ report.key-09
                  v-tot-paid[3]      @ v-paid[1]
                  v-tot-dsc[3]       @ v-dsc[1]
                  v-tot-amt[3]       @ v-amt[1]
                  v-c-%
                  v-tot-com[3]       @ v-com
              with frame detail1.
  
          IF tb_excel THEN
            RUN gt-excel-1(v-tot-paid[3], v-tot-dsc[3], v-tot-amt[3], v-c-%, v-tot-com[3]).
  
        END.
        else
        DO:
          display "   Grand Totals:" @ cust.name
                  v-tot-paid[3]      @ v-paid[1]
                  v-tot-dsc[3]       @ v-dsc[1]
                  v-tot-amt[3]       @ v-amt[1]
              with frame detail2.
  
          IF tb_excel THEN
            RUN gt-excel-2(v-tot-paid[3], v-tot-dsc[3], v-tot-amt[3]).
        END.*/
        END.
        DELETE report.
    END.

    /* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

    IF rd-dest = 3 THEN 
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
    DEFINE INPUT PARAMETER ip-perc AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-com AS DECIMAL NO-UNDO.

    PUT STREAM excel UNFORMATTED
        '"' ""                                         '",'
        '"' "SalesRep Totals:"                         '",'
        '"' ""                                         '",'
        '"' ""                                         '",'
        '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
        '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
        '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
        '"' STRING(ip-perc,"->>9.99")                    '",'
        '"' STRING(ip-com,"->>,>>>,>>9.99")      '",'
        SKIP.
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

    PUT STREAM excel UNFORMATTED
        '"' ""                                         '",'
        '"' "SalesRep Totals:"                         '",'
        '"' ""                                         '",'
        '"' ""                                         '",'
        '"' ""                                         '",'
        '"' STRING(ip-paid,"->>,>>>,>>9.99")     '",'
        '"' STRING(ip-dsc,"->>,>>>,>>9.99")      '",'
        '"' STRING(ip-amt,"->>,>>>,>>9.99")      '",'
        SKIP.
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

