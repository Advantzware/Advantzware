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

DEFINE TEMP-TABLE tt-report 
    FIELD actnum  LIKE account.actnum
    FIELD vend-no LIKE vend.vend-no
    FIELD inv-no  LIKE ap-inv.inv-no
    FIELD jrnl    LIKE glhist.jrnl
    FIELD tr-date LIKE ap-ledger.tr-date
    FIELD trnum   LIKE ap-ledger.trnum
    FIELD amt     AS DECIMAL
    FIELD dscr    LIKE ap-invl.dscr
    FIELD file-id AS INTEGER
    FIELD row-id  AS ROWID
    INDEX detail actnum vend-no inv-no jrnl
    INDEX row-id row-id.

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

DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAPInvoiceLength   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNK1Value          AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "APInvoiceLength", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cNK1Value, OUTPUT lRecFound).
IF lRecFound THEN
    lAPInvoiceLength = LOGICAL(cNK1Value) NO-ERROR.

ASSIGN 
    cTextListToSelect  = "GL Acct#,Description,Vendor,Inv#,Journal,Run#,Date,Amount"
    cFieldListToSelect = "glact,act-dscr,vend,inv,jrnl,run,date,amt"
    cFieldType         = "c,c,c,c,c,c,c,i" 
    .
IF lAPInvoiceLength THEN
    ASSIGN cFieldLength = "25,30,8,20,9,7,10,19" .
ELSE
    ASSIGN cFieldLength = "25,30,8,12,9,7,10,19" .
       
       
{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "GL Acct#,Description,Vendor,Inv#,Journal,Run#,Date,Amount".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_run end_run begin_vend ~
end_vend begin_date end_date begin_acct end_acct td-sub-total sl_avail ~
Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_run end_run begin_vend end_vend ~
begin_date end_date begin_acct end_acct td-sub-total sl_avail sl_selected ~
rd-dest fi_file tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_acct     AS CHARACTER FORMAT "X(20)":U 
    LABEL "Beginning Acct#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_run      AS INTEGER   FORMAT "->>>>>>>":U INITIAL 0 
    LABEL "Beginning Run#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vend#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_acct       AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzzzzzzzzzzz" 
    LABEL "Ending Acct#" 
    VIEW-AS FILL-IN 
    SIZE 26.8 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_run        AS INTEGER   FORMAT "->>>>>>>":U INITIAL 99999999 
    LABEL "Ending Run#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vend#" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\APAccountsByVendor.csv" 
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
    SIZE 91 BY 6.91.

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

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE td-sub-total AS LOGICAL   INITIAL NO 
    LABEL "Include Subtotals?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_run AT ROW 2.76 COL 19.4 COLON-ALIGNED HELP
    "Enter Beginning Run" WIDGET-ID 58
    end_run AT ROW 2.76 COL 63.4 COLON-ALIGNED HELP
    "Enter Ending Vendor Number" WIDGET-ID 60
    begin_vend AT ROW 3.81 COL 19.4 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend AT ROW 3.81 COL 63.4 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_date AT ROW 4.86 COL 19.4 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_date AT ROW 4.86 COL 63.4 COLON-ALIGNED HELP
    "Enter Ending Vendor Number"
    begin_acct AT ROW 5.91 COL 19.4 COLON-ALIGNED HELP
    "Enter Beginning GL Account Number"
    end_acct AT ROW 5.91 COL 63.4 COLON-ALIGNED HELP
    "Enter Ending GL Account Number"
    td-sub-total AT ROW 7 COL 21.4 WIDGET-ID 62
    sl_avail AT ROW 9.33 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 9.33 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 9.33 COL 60.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 10.33 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 11.33 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 12.38 COL 40.4 WIDGET-ID 40
    btn_down AT ROW 13.38 COL 40.4 WIDGET-ID 42
    lv-font-no AT ROW 15.05 COL 36 COLON-ALIGNED
    lv-ornt AT ROW 15.05 COL 45 NO-LABELS
    lines-per-page AT ROW 15.05 COL 87 COLON-ALIGNED
    rd-dest AT ROW 15.24 COL 4.4 NO-LABELS
    lv-font-name AT ROW 16 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 16.95 COL 92 RIGHT-ALIGNED
    td-show-parm AT ROW 17.1 COL 27.6
    fi_file AT ROW 17.95 COL 25.4 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.05 COL 92.2 RIGHT-ALIGNED
    tbAutoClose AT ROW 19.33 COL 27.4 WIDGET-ID 78
    btn-ok AT ROW 20.24 COL 27.4
    btn-cancel AT ROW 20.24 COL 52
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 8.62 COL 60.4 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 14.57 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 4
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 8.62 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 14.95 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 25.14
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
        TITLE              = "AP Accounts by Vendor"
        HEIGHT             = 20.62
        WIDTH              = 95
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_run:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_acct:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_run:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* AP Accounts by Vendor */
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
ON WINDOW-CLOSE OF C-Win /* AP Accounts by Vendor */
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


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_run C-Win
ON LEAVE OF begin_run IN FRAME FRAME-A /* Beginning Run# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend C-Win
ON LEAVE OF begin_vend IN FRAME FRAME-A /* Beginning Vend# */
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_run C-Win
ON LEAVE OF end_run IN FRAME FRAME-A /* Ending Run# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vend# */
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


&Scoped-define SELF-NAME td-sub-total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL td-sub-total C-Win
ON VALUE-CHANGED OF td-sub-total IN FRAME FRAME-A /* Include Subtotals? */
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
    {sys/inc/reportsConfigNK1.i "VR11" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-void-date C-Win 
PROCEDURE check-void-date :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID.
    DEFINE OUTPUT PARAMETER op-date AS DATE.
    DEFINE BUFFER bf-ap-inv    FOR ap-inv.
    DEFINE BUFFER bf-ap-pay    FOR ap-pay.
    DEFINE BUFFER bf-ap-payl   FOR ap-payl.
    DEFINE BUFFER bf-ap-ledger FOR ap-ledger.
    DEFINE VARIABLE t-dscr   AS CHARACTER.
    DEFINE VARIABLE v-refnum AS CHARACTER.
    op-date = ?.

    FIND bf-ap-payl WHERE ROWID(bf-ap-payl) = ip-rowid NO-LOCK NO-ERROR.

    FOR EACH bf-ap-pay WHERE bf-ap-pay.company EQ cocode
        AND bf-ap-pay.c-no    EQ bf-ap-payl.c-no
        NO-LOCK:
        FIND FIRST bf-ap-inv
            WHERE bf-ap-inv.company  EQ cocode
            AND bf-ap-inv.vend-no  EQ bf-ap-payl.vend-no
            AND bf-ap-inv.inv-no   EQ bf-ap-payl.inv-no
            AND bf-ap-inv.posted   EQ YES    
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bf-ap-inv THEN
            RETURN.

        t-dscr = "Payment".

        IF bf-ap-payl.memo THEN t-dscr = "CR MEMO".

        IF bf-ap-payl.amt-paid            LT 0  AND
            bf-ap-payl.memo                EQ NO AND
            bf-ap-inv.net + bf-ap-inv.freight GT 0  THEN t-dscr = "Void Chk".
        IF TRUE /* t-dscr = "Void Chk" */ THEN 
        DO:

            v-refnum = "VOIDED CHECK"
                + string(bf-ap-pay.check-no, "zzzzzzz9").
            FIND FIRST bf-ap-ledger WHERE bf-ap-ledger.company EQ bf-ap-pay.company 
                AND bf-ap-ledger.vend-no EQ bf-ap-pay.vend-no 
                AND bf-ap-ledger.refnum = v-refnum
                NO-LOCK NO-ERROR.
        END.
        IF AVAILABLE bf-ap-ledger THEN
            op-date = bf-ap-ledger.tr-date.
        ELSE
            op-date = ?.

    END.

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
    DISPLAY begin_run end_run begin_vend end_vend begin_date end_date begin_acct 
        end_acct td-sub-total sl_avail sl_selected rd-dest fi_file tb_OpenCSV 
        tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_run end_run begin_vend end_vend begin_date 
        end_date begin_acct end_acct td-sub-total sl_avail Btn_Def sl_selected 
        Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV 
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
    /*      DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.      */
    /*                                                         */
    /*      if init-dir = "" then init-dir = "c:\temp" .       */
    /*      SYSTEM-DIALOG GET-FILE list-name                   */
    /*          TITLE      "Enter Listing Name to SAVE AS ..." */
    /*          FILTERS    "Listing Files (*.rpt)" "*.rpt",    */
    /*                     "All Files (*.*)" "*.*"             */
    /*          INITIAL-DIR init-dir                           */
    /*          ASK-OVERWRITE                                  */
    /*     /*     CREATE-TEST-FILE*/                           */
    /*          SAVE-AS                                        */
    /*          USE-FILENAME                                   */
    /*                                                         */
    /*          UPDATE OKpressed.                              */
    /*                                                         */
    /*      IF NOT OKpressed THEN  RETURN NO-APPLY.            */
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

    DEFINE VARIABLE lv-jrnl        LIKE glhist.jrnl NO-UNDO.
    DEFINE VARIABLE li             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-bank-code   LIKE bank.bank-code NO-UNDO.
    DEFINE VARIABLE lv-check-no    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li-check-no    LIKE ap-pay.check-no NO-UNDO.
    DEFINE VARIABLE li-line        LIKE ap-payl.line NO-UNDO.
    DEFINE VARIABLE li-lines       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-amt         LIKE tt-report.amt EXTENT 3 NO-UNDO.
    DEFINE VARIABLE lv-excel-descr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-void-date    AS DATE      NO-UNDO.
    DEFINE VARIABLE v-temp-date    AS DATE      NO-UNDO.
    DEFINE VARIABLE v-refnum       AS CHARACTER.
    DEFINE BUFFER bf-ap-ledger FOR ap-ledger.

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
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.


    {sys/form/r-top5L3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

&SCOPED-DEFINE where-ap-pay                             ~
        WHERE ap-pay.company    EQ cocode               ~
          AND ap-pay.c-no       EQ ap-payl.c-no         ~
          AND ap-pay.vend-no    EQ ap-ledger.vend-no    ~
          AND ap-pay.check-date EQ ap-ledger.ref-date   ~
          AND ap-pay.posted     EQ YES                  ~
          AND ap-pay.memo       EQ YES

    FORM tt-report.actnum      COLUMN-LABEL "GL Acct#"
        account.dscr          COLUMN-LABEL "Description" FORMAT "x(32)"
        tt-report.vend-no     COLUMN-LABEL "Vendor"      
        /*vend.name             COLUMN-LABEL "Name"        FORMAT "x(20)"*/
        tt-report.inv-no      COLUMN-LABEL "Inv#"
        tt-report.jrnl        COLUMN-LABEL "Journal"
        tt-report.trnum       COLUMN-LABEL "Run#"
        tt-report.tr-date     COLUMN-LABEL "Date"
        tt-report.amt         COLUMN-LABEL "Amount"    FORMAT "->>>,>>>,>>>,>>9.99"

        WITH FRAME detail NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 132.


    FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.

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

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-report:
        DELETE tt-report.
    END.

    /*IF tb_excel THEN DO:
       OUTPUT STREAM excel TO VALUE(cFileName).
       EXPORT STREAM excel DELIMITER ","       
           "GL Acct#"
           "Description"
           "Vendor"
           "Inv#"
           "Journal"
           "Run#"
           "Date"
           "Amount"
           SKIP.
    END.*/

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /*  IF tb_detail THEN
            excelheader = "Inv#,Account,Vendor,Description,Date,Amount".
          ELSE
            excelheader = "Vendor,Name,Inv#,Date,Amount". */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    DISPLAY "" WITH FRAME r-top.

    FOR EACH ap-ledger
        WHERE ap-ledger.company EQ cocode
        AND ap-ledger.vend-no GE begin_vend
        AND ap-ledger.vend-no LE end_vend
        AND ap-ledger.vend-no NE ""
        AND ap-ledger.tr-date GE begin_date
        AND ap-ledger.tr-date LE end_date
        AND ap-ledger.trnum GE begin_run
        AND ap-ledger.trnum LE end_run
        NO-LOCK:

        {custom/statusMsg.i " 'Processing Vendor#  '  + ap-ledger.vend-no "}

        IF ap-ledger.refnum BEGINS "INV# " THEN 
        DO:
            FIND FIRST ap-inv
                WHERE ap-inv.company EQ ap-ledger.company
                AND ap-inv.vend-no EQ ap-ledger.vend-no
                AND ap-inv.inv-no  EQ SUBSTR(ap-ledger.refnum,6,20)
                NO-LOCK NO-ERROR.

            lv-jrnl = "ACPAY".

            IF AVAILABLE ap-inv THEN 
            DO:
                CREATE tt-report.
                BUFFER-COPY ap-ledger TO tt-report
                    ASSIGN
                    tt-report.inv-no = ap-inv.inv-no
                    tt-report.jrnl   = lv-jrnl
                    tt-report.actnum = ap-ctrl.payables
                    tt-report.amt    = (ap-inv.net + ap-inv.freight) * -1.

                CREATE tt-report.
                BUFFER-COPY ap-ledger TO tt-report
                    ASSIGN
                    tt-report.inv-no = ap-inv.inv-no
                    tt-report.jrnl   = lv-jrnl
                    tt-report.actnum = ap-ctrl.freight
                    tt-report.amt    = ap-inv.freight.

                FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no:
                    CREATE tt-report.
                    BUFFER-COPY ap-ledger TO tt-report
                        ASSIGN
                        tt-report.inv-no  = ap-inv.inv-no
                        tt-report.jrnl    = lv-jrnl
                        tt-report.actnum  = ap-invl.actnum
                        tt-report.amt     = ap-invl.amt
                        tt-report.dscr    = ap-invl.dscr
                        tt-report.file-id = 1
                        tt-report.row-id  = ROWID(ap-invl).
                END.
            END.
        END.

        ELSE
            IF ap-ledger.refnum BEGINS "MEMO#" THEN 
            DO:
                lv-jrnl = "APMEM".

                FOR EACH ap-payl
                    WHERE ap-payl.inv-no EQ SUBSTR(ap-ledger.refnum,6,20)
                    AND NOT CAN-FIND(FIRST tt-report WHERE tt-report.row-id EQ ROWID(ap-payl))
                    NO-LOCK,

                    FIRST ap-pay {&where-ap-pay} NO-LOCK

        BREAK BY ap-payl.c-no:

                CREATE tt-report.
                BUFFER-COPY ap-ledger TO tt-report
                    ASSIGN
                    tt-report.inv-no  = ap-payl.inv-no
                    tt-report.jrnl    = lv-jrnl
                    tt-report.actnum  = ap-ctrl.payables
                    tt-report.amt     = ap-payl.amt-paid - ap-payl.amt-disc
                    tt-report.file-id = 2
                    tt-report.row-id  = ROWID(ap-payl).

                CREATE tt-report.
                BUFFER-COPY ap-ledger TO tt-report
                    ASSIGN
                    tt-report.inv-no  = ap-payl.inv-no
                    tt-report.jrnl    = lv-jrnl
                    tt-report.actnum  = ap-payl.actnum
                    tt-report.amt     = (ap-payl.amt-paid - ap-payl.amt-disc) * -1
                    tt-report.file-id = 2
                    tt-report.row-id  = ROWID(ap-payl).
            END.
    END.

  ELSE
  IF ap-ledger.refnum BEGINS "CHK# " AND
     LENGTH(ap-ledger.refnum) GE 11  THEN DO:
ASSIGN
    lv-jrnl      = "CDISB"
    lv-bank-code = ""
    lv-check-no  = "".

DO li = 1 TO LENGTH(ap-ledger.refnum):
    IF LENGTH(TRIM(lv-check-no)) GE 4                                AND
        SUBSTR(lv-check-no,LENGTH(TRIM(lv-check-no)) - 3,4) EQ " CD#" THEN
        lv-bank-code = lv-bank-code + SUBSTR(ap-ledger.refnum,li,1).
    ELSE
        lv-check-no  = lv-check-no + SUBSTR(ap-ledger.refnum,li,1).
END.
ASSIGN
    lv-check-no = SUBSTR(lv-check-no,6,LENGTH(TRIM(lv-check-no)) - 9)
    li-check-no = INT(lv-check-no) NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company   EQ ap-ledger.company
        AND bank.bank-code EQ lv-bank-code
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company   EQ ap-ledger.company
        AND ap-pay.check-act EQ bank.actnum
        AND ap-pay.check-no  EQ li-check-no
        AND ap-pay.vend-no   EQ ap-ledger.vend-no
        AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK,

        EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:

        {custom/statusMsg.i " 'Processing Vendor#  '  + ap-pay.vend-no "}

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
            ASSIGN
            tt-report.jrnl    = lv-jrnl
            tt-report.actnum  = bank.actnum
            tt-report.amt     = ap-payl.amt-paid * -1
            tt-report.file-id = 2
            tt-report.row-id  = ROWID(ap-payl).

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
            ASSIGN
            tt-report.jrnl    = lv-jrnl
            tt-report.actnum  = ap-payl.actnum
            tt-report.amt     = ap-payl.amt-paid
            tt-report.file-id = 2
            tt-report.row-id  = ROWID(ap-payl).
    END.
END.

  ELSE
  IF ap-ledger.refnum BEGINS "AC" THEN DO:
ASSIGN
    lv-jrnl     = "APCKR" 
    li-check-no = INT(SUBSTR(ap-ledger.refnum,3,8)) NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company EQ ap-ledger.company
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company   EQ ap-ledger.company
        AND ap-pay.check-act EQ bank.actnum
        AND ap-pay.check-no  EQ li-check-no
        AND ap-pay.vend-no   EQ ap-ledger.vend-no
        AND ap-pay.bank-code EQ bank.bank-code
        AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK,

        EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:

        {custom/statusMsg.i " 'Processing Vendor#  '  + ap-pay.vend-no "}

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
            ASSIGN
            tt-report.inv-no  = ap-payl.inv-no
            tt-report.jrnl    = lv-jrnl
            tt-report.actnum  = bank.actnum
            tt-report.amt     = ap-payl.amt-paid * -1
            tt-report.file-id = 2
            tt-report.row-id  = ROWID(ap-payl).


        IF ap-payl.amt-disc NE 0 THEN 
        DO:
            CREATE tt-report.
            BUFFER-COPY ap-ledger TO tt-report
                ASSIGN
                tt-report.inv-no  = ap-payl.inv-no
                tt-report.jrnl    = lv-jrnl
                tt-report.actnum  = ap-ctrl.discount
                tt-report.amt     = ap-payl.amt-disc * -1
                tt-report.file-id = 2
                tt-report.row-id  = ROWID(ap-payl).
        END.

        CREATE tt-report.
        BUFFER-COPY ap-ledger TO tt-report
            ASSIGN
            tt-report.inv-no  = ap-payl.inv-no
            tt-report.jrnl    = lv-jrnl
            tt-report.actnum  = ap-ctrl.payables
            tt-report.amt     = ap-payl.amt-paid + ap-payl.amt-disc
            tt-report.file-id = 2
            tt-report.row-id  = ROWID(ap-payl).
    END.
END.

  ELSE
  IF ap-ledger.refnum BEGINS "VOIDED CHECK" THEN DO:
ASSIGN
    lv-jrnl     = "APVOIDCK" 
    li-check-no = INT(SUBSTR(ap-ledger.refnum,13,8)) NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN
    FOR EACH bank
        WHERE bank.company EQ ap-ledger.company
        NO-LOCK,

        FIRST ap-pay
        WHERE ap-pay.company    EQ ap-ledger.company
        AND ap-pay.check-act  EQ bank.actnum
        AND ap-pay.check-no   EQ li-check-no
        AND ap-pay.vend-no    EQ ap-ledger.vend-no
        AND ap-pay.bank-code  EQ bank.bank-code
        AND ap-pay.cleared    EQ YES
        AND ap-pay.reconciled EQ ?
        AND CAN-FIND(FIRST ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no)
        NO-LOCK:

        ASSIGN
            li-lines = 0
            li-line  = 0
            li       = 0.

        FOR EACH ap-payl WHERE ap-payl.c-no EQ ap-pay.c-no NO-LOCK:
            li-lines = li-lines + 1.
        END.
        li-lines = li-lines / 2.

        FOR EACH ap-payl FIELDS(LINE) WHERE
            ap-payl.c-no EQ ap-pay.c-no
            NO-LOCK
            BY ap-payl.line:
            ASSIGN
                li-line = ap-payl.line
                li      = li + 1.
            IF li GE li-lines THEN LEAVE.
        END.

        FOR EACH ap-payl
            WHERE ap-payl.c-no EQ ap-pay.c-no
            AND ap-payl.line LE li-line
            NO-LOCK:

            CREATE tt-report.
            BUFFER-COPY ap-ledger TO tt-report
                ASSIGN
                tt-report.inv-no  = ap-payl.inv-no
                tt-report.jrnl    = lv-jrnl
                tt-report.actnum  = bank.actnum
                tt-report.amt     = ap-payl.amt-paid
                tt-report.file-id = 2
                tt-report.row-id  = ROWID(ap-payl).

            IF ap-payl.amt-disc NE 0 THEN 
            DO:
                CREATE tt-report.
                BUFFER-COPY ap-ledger TO tt-report
                    ASSIGN
                    tt-report.inv-no  = ap-payl.inv-no
                    tt-report.jrnl    = lv-jrnl
                    tt-report.actnum  = ap-ctrl.discount
                    tt-report.amt     = ap-payl.amt-disc
                    tt-report.file-id = 2
                    tt-report.row-id  = ROWID(ap-payl).
            END.

            CREATE tt-report.
            BUFFER-COPY ap-ledger TO tt-report
                ASSIGN
                tt-report.inv-no  = ap-payl.inv-no
                tt-report.jrnl    = lv-jrnl
                tt-report.actnum  = ap-ctrl.payables
                tt-report.amt     = (ap-payl.amt-paid + ap-payl.amt-disc) * -1
                tt-report.file-id = 2
                tt-report.row-id  = ROWID(ap-payl).
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
    BY tt-report.vend-no
    BY tt-report.trnum
    BY tt-report.inv-no
    BY tt-report.jrnl:

    FIND FIRST account
        WHERE account.company EQ cocode
        AND account.actnum  EQ tt-report.actnum
        NO-LOCK NO-ERROR.

    FIND FIRST vend
        WHERE vend.company EQ cocode
        AND vend.vend-no EQ tt-report.vend-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE vend THEN
        {custom/statusMsg.i " 'Processing Vendor#  '  + vend.vend-no "}

    lv-amt[1] = lv-amt[1] + tt-report.amt. 
  
    lv-excel-descr = account.dscr.
    IF tt-report.dscr NE "" THEN 
    DO:
        /* IF AVAIL account AND FIRST-OF(tt-report.actnum) THEN DOWN.
         DISPLAY tt-report.dscr @ account.dscr. */
        lv-excel-descr = tt-report.dscr.
    END. 

    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "".

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
        CASE cTmpField:             
            WHEN "glact"           THEN 
                cVarValue = STRING(tt-report.actnum).
            WHEN "act-dscr"        THEN 
                cVarValue = STRING(lv-excel-descr,"x(30)").
            WHEN "vend"            THEN 
                cVarValue = STRING(tt-report.vend-no).
            WHEN "inv"             THEN 
                cVarValue = IF lAPInvoiceLength THEN STRING(tt-report.inv-no,"x(20)") ELSE STRING(tt-report.inv-no,"x(12)").
            WHEN "jrnl"            THEN 
                cVarValue = STRING(tt-report.jrnl).
            WHEN "run"             THEN 
                cVarValue = STRING(tt-report.trnum).
            WHEN "date"            THEN 
                cVarValue = STRING(tt-report.tr-date).
            WHEN "amt"             THEN 
                cVarValue = STRING(tt-report.amt,"->>>,>>>,>>>,>>9.99").
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

    IF LAST-OF(tt-report.vend-no) THEN 
    DO:
        IF td-sub-total THEN
        DO:      
            PUT SKIP(1). 
    
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
                    WHEN "glact"                THEN 
                        cVarValue = "".
                    WHEN "act-dscr"        THEN 
                        cVarValue = "".
                    WHEN "vend"      THEN  
                        cVarValue = "".
                    WHEN "inv"             THEN 
                        cVarValue = "".
                    WHEN "jrnl"                   THEN 
                        cVarValue = "".
                    WHEN "run"              THEN 
                        cVarValue = "".
                    WHEN "date"               THEN 
                        cVarValue = "".
                    WHEN "amt"         THEN 
                        cVarValue = STRING(lv-amt[1],"->>>,>>>,>>>,>>9.99").
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT UNFORMATTED 
                "  Vendor Total" SUBSTRING(cDisplay,15,300) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    "Vendor Total " + substring(cExcelDisplay,3,300) SKIP.
            END.
            PUT SKIP(1).
        END.
        ASSIGN
            lv-amt[2] = lv-amt[2] + lv-amt[1]
            lv-amt[1] = 0.

    
    END.

    IF LAST-OF(tt-report.actnum) THEN 
    DO:
        IF td-sub-total THEN
        DO:
            PUT SKIP(1).   
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
                    WHEN "glact"                THEN 
                        cVarValue = "".
                    WHEN "act-dscr"        THEN 
                        cVarValue = "".
                    WHEN "vend"      THEN  
                        cVarValue = "".
                    WHEN "inv"             THEN 
                        cVarValue = "".
                    WHEN "jrnl"                   THEN 
                        cVarValue = "".
                    WHEN "run"              THEN 
                        cVarValue = "".
                    WHEN "date"               THEN 
                        cVarValue = "".
                    WHEN "amt"         THEN 
                        cVarValue = STRING(lv-amt[2],"->>>,>>>,>>>,>>9.99").
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT UNFORMATTED 
                "  Acct# Total" SUBSTRING(cDisplay,14,300) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    "Acct# Total " + substring(cExcelDisplay,3,300) SKIP.
            END.
            PUT SKIP(3).
        END.
        ASSIGN 
            lv-amt[3] = lv-amt[3] + lv-amt[2]
            lv-amt[2] = 0.

    
    END.

    IF LAST(tt-report.actnum) THEN 
    DO:
        PUT SKIP(1). 
  
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
                WHEN "glact"                THEN 
                    cVarValue = "".
                WHEN "act-dscr"        THEN 
                    cVarValue = "".
                WHEN "vend"      THEN  
                    cVarValue = "".
                WHEN "inv"             THEN 
                    cVarValue = "".
                WHEN "jrnl"                   THEN 
                    cVarValue = "".
                WHEN "run"              THEN 
                    cVarValue = "".
                WHEN "date"               THEN 
                    cVarValue = "".
                WHEN "amt"         THEN 
                    cVarValue = STRING(lv-amt[3],"->>>,>>>,>>>,>>9.99").
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        PUT UNFORMATTED 
            "  Grand Total" SUBSTRING(cDisplay,14,300) SKIP.
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                "Grand Total " + substring(cExcelDisplay,3,300) SKIP.
        END.

    END.
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN 
DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_OpenCSV THEN
        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
END.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2004 Advanced Software, Inc. */

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
            fi_file:SCREEN-VALUE = "c:\tmp\APAccountsByVendor.csv".    
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

