&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:

  Created:

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

/*{sys/inc/custlistform.i ""AL3"" }*/

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-report 
    FIELD actnum  LIKE account.actnum
    FIELD cust-no LIKE cust.cust-no
    FIELD inv-no  LIKE ar-inv.inv-no
    FIELD jrnl    LIKE gltrans.jrnl
    FIELD tr-date LIKE ar-ledger.tr-date
    FIELD tr-num  LIKE ar-ledger.tr-num
    FIELD amt     AS DECIMAL
    FIELD po-no   LIKE ar-inv.po-no
    FIELD lot-no  LIKE ar-invl.lot-no
    FIELD i-no    LIKE ar-invl.i-no
    FIELD i-name  LIKE ar-invl.i-name
    INDEX detail actnum cust-no inv-no jrnl.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCustListActive   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.


ASSIGN 
    cTextListToSelect  = "GL Acct#,Description,Customer,Cust Name,Inv#,PO#,Customer Lot#,Item#,Item Desc,Run#,Date,Amount"
    cFieldListToSelect = "act,dscr,cust,cust-name,inv,po,cust-lot,itm,itm-dscr,run,date,amt"
    cFieldLength       = "25,30,8,30,7,15,15,15,30,6,10,14"
    cFieldType         = "c,c,c,c,c,c,c,c,c,c,c,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "GL Acct#,Description,Customer,Cust Name,Inv#,PO#,Customer Lot#,Item#,Item Desc,Run#,Date,Amount" .

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
begin_cust end_cust begin_date end_date begin_acct end_acct sl_avail ~
Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_date end_date begin_acct end_acct sl_avail sl_selected rd-dest ~
fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-iname C-Win 
FUNCTION get-iname RETURNS CHARACTER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
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

DEFINE VARIABLE begin_acct     AS CHARACTER FORMAT "X(20)":U 
    LABEL "Beginning Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Cust#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct       AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Cust#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ARDetailInvoice.csv" 
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
    "To CSV", 3
    SIZE 15 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 4.38.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.

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

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 36 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    tb_cust-list AT ROW 2.24 COL 21 WIDGET-ID 6
    btnCustList AT ROW 2.24 COL 65 WIDGET-ID 8
    begin_cust AT ROW 3.29 COL 19 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 3.29 COL 63 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_date AT ROW 4.33 COL 19 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_date AT ROW 4.33 COL 63 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_acct AT ROW 5.38 COL 19 COLON-ALIGNED HELP
    "Enter Beginning GL Account Number"
    end_acct AT ROW 5.38 COL 63 COLON-ALIGNED HELP
    "Enter Ending GL Account Number"
    sl_avail AT ROW 7.43 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 7.43 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 7.43 COL 61 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 8.43 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 9.43 COL 40.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 10.48 COL 40.2 WIDGET-ID 40
    btn_down AT ROW 11.48 COL 40.2 WIDGET-ID 42
    lv-font-no AT ROW 13.14 COL 42 COLON-ALIGNED
    lv-ornt AT ROW 13.14 COL 51 NO-LABELS
    lines-per-page AT ROW 13.14 COL 87 COLON-ALIGNED
    rd-dest AT ROW 13.38 COL 4.6 NO-LABELS
    lv-font-name AT ROW 14.1 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 15.24 COL 28.6
    tb_excel AT ROW 15.29 COL 92 RIGHT-ALIGNED
    fi_file AT ROW 16.1 COL 26.6 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 16.19 COL 78
    tbAutoClose AT ROW 17.43 COL 28.6 WIDGET-ID 64
    btn-ok AT ROW 18.33 COL 28.4
    btn-cancel AT ROW 18.33 COL 54
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 6.71 COL 3 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 6.71 COL 60.6 WIDGET-ID 44
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 12.67 COL 4
    RECT-6 AT ROW 13.05 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 23.71
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
        TITLE              = "AR Detail Invoice Report"
        HEIGHT             = 18.86
        WIDTH              = 94.6
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
    begin_acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* AR Detail Invoice Report */
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
ON WINDOW-CLOSE OF C-Win /* AR Detail Invoice Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_acct C-Win
ON LEAVE OF begin_acct IN FRAME FRAME-A /* Beginning Acct# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Cust# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
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

        RUN GetSelectionList.

        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ttCustList AND tb_cust-list THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust,
                INPUT END_cust).
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
        END CASE.
        IF tbAutoClose:CHECKED THEN 
            APPLY 'CLOSE' TO THIS-PROCEDURE.
        SESSION:SET-WAIT-STATE ("").

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


&Scoped-define SELF-NAME end_acct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_acct C-Win
ON LEAVE OF end_acct IN FRAME FRAME-A /* Ending Acct# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Cust# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,{&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Cust# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON HELP OF fi_file IN FRAME FRAME-A /* Name */
    DO:
        DEFINE VARIABLE ls-filename AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.

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

        RUN WINDOWS/l-fonts.w ({&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
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
    {sys/inc/reportsConfigNK1.i "AL3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "AL3",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_cust.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'AL3',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""AL3""}

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
            INPUT 'AL3',
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
        INPUT 'AL3').


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
    DISPLAY tb_cust-list begin_cust end_cust begin_date end_date begin_acct 
        end_acct sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_cust end_cust begin_date 
        end_date begin_acct end_acct sl_avail Btn_Def sl_selected Btn_Add 
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
    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    IF init-dir = "" THEN init-dir = "c:\temp" .
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

    IF NOT OKpressed THEN  RETURN NO-APPLY.


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
    /*{sys/form/r-topw.f}   */

    DEFINE VARIABLE lv-jrnl        LIKE gltrans.jrnl NO-UNDO.
    DEFINE VARIABLE li             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lj             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-amt         LIKE tt-report.amt EXTENT 3 NO-UNDO.
    DEFINE VARIABLE ld-tax-rate    AS DECIMAL   EXTENT 10 NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ttl-tax      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ttl-rate     AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE lSelected      AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE fcust          AS CHARACTER NO-UNDO .
    DEFINE VARIABLE tcust          AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cRecAccount    AS CHARACTER NO-UNDO.



    {custom/statusMsg.i " 'Processing...  '"}

    {sys/form/r-top5L3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    ASSIGN
        lSelected = tb_cust-list
        fcust     = begin_cust
        tcust     = END_cust  .

    FORM tt-report.actnum      COLUMN-LABEL "GL Acct#"
        SPACE(5)
        /* account.dscr          COLUMN-LABEL "Description" FORMAT "x(32)" */
        tt-report.cust-no     COLUMN-LABEL "Customer"      
        /*cust.name             COLUMN-LABEL "Name"        FORMAT "x(20)" */
        tt-report.inv-no      COLUMN-LABEL "Inv#"
        tt-report.tr-date     COLUMN-LABEL "Date"
        /*tt-report.jrnl        COLUMN-LABEL "Journal" */
        tt-report.po-no        COLUMN-LABEL "PO#"
        tt-report.lot-no      COLUMN-LABEL "Customer Lot#"
        tt-report.i-no        COLUMN-LABEL "Item#"
        /*tt-report.i-name      COLUMN-LABEL "Item Description" */
        tt-report.tr-num      COLUMN-LABEL "Run#"     
        tt-report.amt         COLUMN-LABEL "Amount"    FORMAT "->>>,>>>,>>>,>>9.99"

        WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132.


    SESSION:SET-WAIT-STATE ("general").

    FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK NO-ERROR.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}. 


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


        IF LOOKUP(ttRptSelected.TextList, "Amount") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*excelHeader = 'GL Acct#,Description,Customer,Cust Name,Inv#,PO#,Customer Lot#,Item#,Item Desc,Run#,Date,Amount'.*/
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelHeader,',','","') '"' SKIP.
    END. /* if tb_excel */

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
    END.

    IF td-show-parm THEN RUN show-param.

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    DISPLAY "" WITH FRAME r-top.
    FOR EACH ar-ledger
        WHERE ar-ledger.company EQ cocode
        /*AND ar-ledger.cust-no GE begin_cust
        AND ar-ledger.cust-no LE end_cust*/
        AND ar-ledger.cust-no  GE fcust
        AND ar-ledger.cust-no  LE tcust
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ ar-ledger.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND ar-ledger.cust-no NE ""
        AND ar-ledger.tr-date GE begin_date
        AND ar-ledger.tr-date LE end_date
        NO-LOCK:

        {custom/statusMsg.i " 'Processing Customer#  '  + ar-ledger.cust-no "}
     
        cRecAccount = STRING(DYNAMIC-FUNCTION("GL_GetAccountAR", ar-ledger.company, ar-ledger.cust-no)).
        IF ar-ledger.ref-num BEGINS "INV# " THEN 
        DO:
            FIND FIRST ar-inv
                WHERE ar-inv.company EQ ar-ledger.company
                AND ar-inv.inv-no  EQ INT(SUBSTR(ar-ledger.ref-num,6,10))
                NO-LOCK NO-ERROR.

            IF AVAILABLE ar-inv THEN 
            DO:
                lv-jrnl = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt
                    THEN "ARINV" ELSE "OEINV".

                CREATE tt-report.
                BUFFER-COPY ar-ledger TO tt-report
                    ASSIGN
                    tt-report.inv-no = ar-inv.inv-no
                    tt-report.jrnl   = lv-jrnl
                    tt-report.actnum = cRecAccount
                    tt-report.amt    = (IF lv-jrnl EQ "ARINV"
                           THEN ar-inv.net ELSE ar-inv.gross) * -1.

                IF ar-inv.tax-amt NE 0 THEN 
                DO:
                    RELEASE stax.
                    IF ar-inv.tax-code NE "" THEN
                        FIND FIRST stax
                            WHERE stax.company   EQ ar-inv.company
                            AND stax.tax-group EQ ar-inv.tax-code
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE stax THEN 
                    DO:
                        ASSIGN 
                            v-ttl-tax  = 0
                            v-ttl-rate = 0.


                        DO li = 1 TO EXTENT(stax.tax-rate1): 

                            IF stax.tax-rate1[li] = 0 THEN NEXT.

                            ld-tax-rate[li] = stax.tax-rate1[li].

                            IF stax.accum-tax AND li GT 1 THEN
                            DO lj = 1 TO li - 1: 

                                ld-tax-rate[li] = ld-tax-rate[li] +
                                    (ld-tax-rate[li] * (stax.tax-rate1[lj] / 100)).
                            END.
                            v-ttl-rate = v-ttl-rate + ld-tax-rate[li].
                        END.

                        DO li = 1 TO EXTENT(stax.tax-rate1):
                            IF stax.tax-rate1[li] = 0 THEN NEXT.
                            ASSIGN 
                                ld-tax-rate[li] = ROUND(ld-tax-rate[li] / v-ttl-rate *
                                           ar-inv.tax-amt,2)
                                v-ttl-tax       = v-ttl-tax + ld-tax-rate[li].
                        END.


                        IF ar-inv.tax-amt NE v-ttl-tax THEN
                            ld-tax-rate[1] = ld-tax-rate[1] +
                                (ar-inv.tax-amt - v-ttl-tax).

                        DO li = 1 TO EXTENT(stax.tax-rate1):
                            IF stax.tax-rate1[li] = 0 THEN NEXT.
                            CREATE tt-report.
                            ASSIGN 
                                tt-report.inv-no = ar-inv.inv-no
                                tt-report.jrnl   = lv-jrnl
                                tt-report.actnum = stax.tax-acc1[li]
                                tt-report.amt    = ld-tax-rate[li]
                                tt-report.po-no  = ar-inv.po-no.
                        END. /* 1 to 3 */
                    END. /* avail stax */

                    ELSE 
                    DO:
                        CREATE tt-report.
                        ASSIGN 
                            tt-report.inv-no = ar-inv.inv-no
                            tt-report.jrnl   = lv-jrnl
                            tt-report.actnum = ar-ctrl.stax
                            tt-report.amt    = ar-inv.tax-amt.
                    END.
                END.

                IF ar-inv.f-bill THEN 
                DO:
                    CREATE tt-report.
                    BUFFER-COPY ar-ledger TO tt-report
                        ASSIGN
                        tt-report.inv-no = ar-inv.inv-no
                        tt-report.jrnl   = lv-jrnl
                        tt-report.actnum = ar-ctrl.freight
                        tt-report.amt    = ar-inv.freight.
                END.

                FOR EACH ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no NO-LOCK:
                    CREATE tt-report.
                    BUFFER-COPY ar-ledger TO tt-report
                        ASSIGN
                        tt-report.inv-no  = ar-inv.inv-no
                        tt-report.jrnl    = lv-jrnl
                        tt-report.actnum  = ar-invl.actnum
                        tt-report.amt     = ar-invl.amt
                        tt-report.po-no   = ar-inv.po-no
                        tt-report.lot-no  = ar-invl.lot-no
                        tt-report.i-no    = IF ar-invl.i-no GT "" THEN ar-invl.i-no
                             ELSE ar-invl.i-dscr.
                    tt-report.i-name  = get-iname().
                END.
            END.
        END.

        ELSE
            IF ar-ledger.ref-num BEGINS "Memo#" THEN 
            DO:    
                FOR EACH ar-cash
                    WHERE ar-cash.company  EQ ar-ledger.company
                    AND ar-cash.posted   EQ YES
                    AND ar-cash.memo     EQ YES
                    AND ar-cash.cust-no  EQ ar-ledger.cust-no
                    AND ar-cash.check-no EQ INT(SUBSTR(ar-ledger.ref-num,6,8))
                    NO-LOCK,

                    EACH ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK:
                    /* gdm - 09240903 */
                    lv-jrnl = IF ar-cashl.amt-paid - ar-cashl.amt-disc GT 0
                        THEN "DBMEM" ELSE "CRMEM".

                    CREATE tt-report.
                    BUFFER-COPY ar-ledger TO tt-report
                        ASSIGN
                        tt-report.inv-no  = ar-cashl.inv-no
                        tt-report.jrnl    = lv-jrnl
                        tt-report.actnum  = cRecAccount
                        tt-report.amt     = (ar-cashl.amt-paid - ar-cashl.amt-disc) * -1
                        /*        tt-report.jrnl    = IF tt-report.amt < 0        */
                        /*                              THEN "DBMEM" ELSE "CRMEM" */
                        .

                    CREATE tt-report.
                    BUFFER-COPY ar-ledger TO tt-report
                        ASSIGN
                        tt-report.inv-no  = ar-cashl.inv-no
                        tt-report.jrnl    = lv-jrnl
                        tt-report.actnum  = ar-cashl.actnum
                        tt-report.amt     = ar-cashl.amt-paid - ar-cashl.amt-disc
                        /*        tt-report.jrnl    = IF tt-report.amt < 0        */
                        /*                              THEN "DBMEM" ELSE "CRMEM" */
                        .
                END.
            END.

            ELSE
                IF ar-ledger.ref-num BEGINS "CHK# " THEN 
                DO:
                    lv-jrnl = "CASHR".

                    FOR EACH ar-cash
                        WHERE ar-cash.company  EQ ar-ledger.company
                        AND ar-cash.posted   EQ YES
                        AND ar-cash.memo     EQ NO
                        AND ar-cash.cust-no  EQ ar-ledger.cust-no
                        AND ar-cash.check-no EQ INT64(SUBSTR(ar-ledger.ref-num,6,12))
                        NO-LOCK :

                        /*  FIRST ar-cashl WHERE ar-cashl.c-no EQ ar-cash.c-no NO-LOCK: */
                        FOR EACH ar-cashl WHERE ar-cashl.c-no = ar-cash.c-no NO-LOCK
                            BREAK BY ar-cashl.inv-no :

                            CREATE tt-report.
                            BUFFER-COPY ar-ledger TO tt-report
                                ASSIGN
                                tt-report.inv-no  = ar-cashl.inv-no
                                tt-report.jrnl    = lv-jrnl
                                tt-report.actnum  = cRecAccount
                                tt-report.amt     = ar-cashl.amt-paid + ar-cashl.amt-disc.

                            CREATE tt-report.
                            BUFFER-COPY ar-ledger TO tt-report
                                ASSIGN
                                tt-report.inv-no  = ar-cashl.inv-no
                                tt-report.jrnl    = lv-jrnl
                                tt-report.actnum  = ar-cashl.actnum
                                tt-report.amt     = ar-cashl.amt-paid * -1.

                            IF ar-cashl.amt-disc NE 0 THEN 
                            DO:
                                CREATE tt-report.
                                BUFFER-COPY ar-ledger TO tt-report
                                    ASSIGN
                                    tt-report.inv-no  = ar-cashl.inv-no
                                    tt-report.jrnl    = "CRDIS"
                                    tt-report.actnum  = ar-ctrl.discount
                                    tt-report.amt     = ar-cashl.amt-disc * -1.
                            END.
                        END.
                    END.
                END.
    END.

    FOR EACH tt-report
        WHERE tt-report.actnum GE begin_acct
        AND tt-report.actnum LE end_acct
        AND tt-report.actnum NE ""
        AND tt-report.amt    NE 0
        USE-INDEX detail
        BREAK BY tt-report.actnum
        BY tt-report.cust-no
        BY tt-report.inv-no
        BY tt-report.jrnl:

        {custom/statusMsg.i " 'Processing Customer#  '  + tt-report.cust-no "}

        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ tt-report.actnum
            NO-LOCK NO-ERROR.

        FIND FIRST cust
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ tt-report.cust-no
            NO-LOCK NO-ERROR.

        /* DISPLAY tt-report.actnum WHEN FIRST-OF(tt-report.actnum)
               /*  account.dscr WHEN AVAIL account AND FIRST-OF(tt-report.actnum)
                     "Not on File" WHEN NOT AVAIL account AND FIRST-OF(tt-report.actnum)
                       @ account.dscr */
                 tt-report.cust-no WHEN FIRST-OF(tt-report.cust-no)
                 /*cust.name WHEN AVAIL cust AND FIRST-OF(tt-report.cust-no)
                     "Not on File" WHEN NOT AVAIL cust AND FIRST-OF(tt-report.cust-no)
                       @ cust.name*/
                 tt-report.inv-no
                /* tt-report.jrnl */
                 tt-report.po-no
                 tt-report.lot-no
                 tt-report.i-no
                /* tt-report.i-name */
                 tt-report.tr-num
                 tt-report.tr-date
                 tt-report.amt
             WITH FRAME detail.
         DOWN WITH FRAME detail.
       
         IF FIRST-OF(tt-report.actnum) AND AVAIL(account) THEN
             PUT account.dscr FORMAT "x(28)".
       
         IF FIRST-OF(tt-report.cust-no) AND AVAIL(cust) THEN
             PUT cust.NAME AT 31. */

        /*IF FIRST-OF(tt-report.actnum) OR FIRST-OF(tt-report.cust-no) THEN 
      
        PUT tt-report.i-name AT 90 SKIP.                                                            */

        /* IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
              '"' (IF FIRST-OF(tt-report.actnum) THEN tt-report.actnum ELSE "") '",'
              '"' (IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN account.dscr
                   ELSE IF NOT AVAIL account AND FIRST-OF(tt-report.actnum) THEN
                       "Not on File"
                   ELSE "")                              '",'
              '"'  (IF FIRST-OF(tt-report.cust-no) THEN tt-report.cust-no
                    ELSE "")                                                   '",'
             '"'  (IF FIRST-OF(tt-report.cust-no) AND AVAIL(cust) THEN cust.NAME
                    ELSE "")                                                   '",'
              '"' tt-report.inv-no                                             '",' 
              '"' tt-report.po-no                                              '",'
              '"' tt-report.lot-no                                             '",'
              '"' tt-report.i-no                                               '",'
              '"' tt-report.i-name                                             '",'
              '"' tt-report.tr-num                                             '",'
              '"' (IF tt-report.tr-date NE ? THEN
                   STRING(tt-report.tr-date,"99/99/9999") ELSE "")             '",'
              '"' STRING(tt-report.amt,'->>,>>>,>>9.99')                       '",'
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
                WHEN "act"                   THEN 
                    cVarValue = (IF FIRST-OF(tt-report.actnum) THEN STRING(tt-report.actnum) ELSE "") .
                WHEN "dscr"             THEN 
                    cVarValue = (IF AVAILABLE account AND FIRST-OF(tt-report.actnum) THEN account.dscr ELSE IF NOT AVAILABLE account AND FIRST-OF(tt-report.actnum) THEN "Not on File" ELSE "").
                WHEN "cust"                 THEN 
                    cVarValue = IF FIRST-OF(tt-report.cust-no) THEN tt-report.cust-no ELSE "".
                WHEN "cust-name"        THEN 
                    cVarValue = IF FIRST-OF(tt-report.cust-no) AND AVAIL(cust) THEN cust.NAME ELSE "".
                WHEN "inv"                     THEN 
                    cVarValue = STRING(tt-report.inv-no) .
                WHEN "po"                 THEN 
                    cVarValue = STRING(tt-report.po-no) .
                WHEN "cust-lot"            THEN 
                    cVarValue = STRING(tt-report.lot-no) .
                WHEN "itm"                     THEN 
                    cVarValue = STRING(tt-report.i-no) .
                WHEN "itm-dscr"            THEN 
                    cVarValue = STRING(tt-report.i-name) .
                WHEN "run"            THEN 
                    cVarValue = STRING(tt-report.tr-num) .
                WHEN "date"            THEN 
                    cVarValue = IF tt-report.tr-date NE ? THEN STRING(tt-report.tr-date,"99/99/9999") ELSE "".
                WHEN "amt"                 THEN 
                    cVarValue = STRING(tt-report.amt,'->>,>>>,>>9.99') .

            END CASE.

            cExcelVarValue = cVarValue.
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

        lv-amt[1] = lv-amt[1] + tt-report.amt.

        IF LAST-OF(tt-report.cust-no) THEN 
        DO:
            PUT SKIP(1).

            /* UNDERLINE tt-report.amt.
             DISPLAY "Customer" @ tt-report.cust-no
                     "Totals"   @ tt-report.inv-no
                     lv-amt[1]  @ tt-report.amt.
             DOWN. */

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
                    WHEN "act"                   THEN 
                        cVarValue = "".
                    WHEN "dscr"             THEN 
                        cVarValue = "".
                    WHEN "cust"                 THEN 
                        cVarValue = "".
                    WHEN "cust-name"        THEN 
                        cVarValue = "".
                    WHEN "inv"                     THEN 
                        cVarValue = "" .
                    WHEN "po"                 THEN 
                        cVarValue = "".
                    WHEN "cust-lot"            THEN 
                        cVarValue = "".
                    WHEN "itm"                     THEN 
                        cVarValue = "".
                    WHEN "itm-dscr"            THEN 
                        cVarValue = "".
                    WHEN "run"            THEN 
                        cVarValue = "".
                    WHEN "date"            THEN 
                        cVarValue = "".
                    WHEN "amt"                 THEN 
                        cVarValue = STRING(lv-amt[1],'->>,>>>,>>9.99') .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "   Customer Totals" SUBSTRING(cDisplay,19,300) SKIP.


            ASSIGN
                lv-amt[2] = lv-amt[2] + lv-amt[1]
                lv-amt[1] = 0.

            PUT SKIP(1).
        END.

        IF LAST-OF(tt-report.actnum) THEN 
        DO:
            PUT SKIP(1).

            /*  UNDERLINE tt-report.amt.
              DISPLAY "   Acct#" @ tt-report.cust-no
                      "Totals"   @ tt-report.inv-no
                      lv-amt[2]  @ tt-report.amt.
              DOWN. */

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
                    WHEN "act"                   THEN 
                        cVarValue = "".
                    WHEN "dscr"             THEN 
                        cVarValue = "".
                    WHEN "cust"                 THEN 
                        cVarValue = "".
                    WHEN "cust-name"        THEN 
                        cVarValue = "".
                    WHEN "inv"                     THEN 
                        cVarValue = "" .
                    WHEN "po"                 THEN 
                        cVarValue = "".
                    WHEN "cust-lot"            THEN 
                        cVarValue = "".
                    WHEN "itm"                     THEN 
                        cVarValue = "".
                    WHEN "itm-dscr"            THEN 
                        cVarValue = "".
                    WHEN "run"            THEN 
                        cVarValue = "".
                    WHEN "date"            THEN 
                        cVarValue = "".
                    WHEN "amt"                 THEN 
                        cVarValue = STRING(lv-amt[2],'->>,>>>,>>9.99') .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "   Acct# Totals" SUBSTRING(cDisplay,16,300) SKIP.

            /*     IF tb_excel THEN                                 */
            /*       PUT STREAM excel UNFORMATTED                   */
            /*          '"' ""                                 '",' */
            /*          '"' ""                                 '",' */
            /*          '"' "Acct#"                            '",' */
            /*          '"' "Totals"                           '",' */
            /*          '"' ""                                 '",' */
            /*          '"' ""                                 '",' */
            /*          '"' ""                                 '",' */
            /*          '"' STRING(lv-amt[2],'->>,>>>,>>9.99') '",' */
            /*         SKIP.                                        */

            ASSIGN
                lv-amt[3] = lv-amt[3] + lv-amt[2]
                lv-amt[2] = 0.

            PUT SKIP(3).
        END.

        IF LAST(tt-report.actnum) THEN 
        DO:
            PUT SKIP(1).

            /*  UNDERLINE tt-report.amt.
              DISPLAY "   Grand" @ tt-report.cust-no
                      "Totals"   @ tt-report.inv-no
                      lv-amt[3]  @ tt-report.amt.
              DOWN. */


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
                    WHEN "act"                   THEN 
                        cVarValue = "".
                    WHEN "dscr"             THEN 
                        cVarValue = "".
                    WHEN "cust"                 THEN 
                        cVarValue = "".
                    WHEN "cust-name"        THEN 
                        cVarValue = "".
                    WHEN "inv"                     THEN 
                        cVarValue = "" .
                    WHEN "po"                 THEN 
                        cVarValue = "".
                    WHEN "cust-lot"            THEN 
                        cVarValue = "".
                    WHEN "itm"                     THEN 
                        cVarValue = "".
                    WHEN "itm-dscr"            THEN 
                        cVarValue = "".
                    WHEN "run"            THEN 
                        cVarValue = "".
                    WHEN "date"            THEN 
                        cVarValue = "".
                    WHEN "amt"                 THEN 
                        cVarValue = STRING(lv-amt[3],'->>,>>>,>>9.99') .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "   Grand Totals" SUBSTRING(cDisplay,16,300) SKIP.

        /*     IF tb_excel THEN                                 */
        /*       PUT STREAM excel UNFORMATTED                   */
        /*          '"' ""                                 '",' */
        /*          '"' ""                                 '",' */
        /*          '"' ""                                 '",' */
        /*          '"' "Grand Totals"                     '",' */
        /*          '"' ""                                 '",' */
        /*          '"' ""                                 '",' */
        /*          '"' ""                                 '",' */
        /*          '"' STRING(lv-amt[3],'->>,>>>,>>9.99') '",' */
        /*         SKIP.                                        */
        END.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

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
            fi_file:SCREEN-VALUE = "c:\tmp\ARDetailInvoice.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-iname C-Win 
FUNCTION get-iname RETURNS CHARACTER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    RETURN IF AVAILABLE ar-invl THEN (IF ar-invl.i-name EQ "" THEN
        ar-invl.part-dscr1 ELSE ar-invl.i-name) ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

