&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: arrep\r-depsit.w

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

/*{sys/inc/custlistform.i ""AR8"" }*/

{sys/ref/CustList.i NEW}

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-check-no    LIKE ar-cash.check-no NO-UNDO.
DEFINE VARIABLE lv-inv-no      LIKE ar-cashl.inv-no NO-UNDO.
DEFINE VARIABLE lv-amt         LIKE ar-cash.check-amt NO-UNDO.
DEFINE VARIABLE lv-cust-no     LIKE cust.cust-no NO-UNDO.
DEFINE VARIABLE lv-name        LIKE cust.name NO-UNDO.

DEFINE TEMP-TABLE tt-glhist NO-UNDO LIKE glhist
    FIELD VOID AS LOG.

DEFINE STREAM excel.

DEFINE VARIABLE glCustListActive   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.


ASSIGN 
    cTextListToSelect  = "BANK,ACCT NUMBER,CUST NUMBER,DATE,TRANSACTION#,DAILY TOTAL,CHECK#,INV#"
    cFieldListToSelect = "bank,act,cust,date,trns,ttl,chk,inv"
    cFieldLength       = "8,20,30,10,12,14,12,7"
    cFieldType         = "c,c,c,c,c,i,c,c"
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "BANK,ACCT NUMBER,CUST NUMBER,DATE,TRANSACTION#,DAILY TOTAL,CHECK#,INV#" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_date end_date begin_bank ~
end_bank tb_cust-list btnCustList begin_cust end_cust sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV ~
tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_date end_date begin_bank end_bank ~
tb_cust-list begin_cust end_cust sl_avail sl_selected rd-dest fi_file ~
tb_OpenCSV tbAutoClose 

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

DEFINE VARIABLE begin_bank     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Bank" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Cust#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999" INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_bank       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Bank" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Cust#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999" INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\BankDeposit.csv" 
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
    SIZE 17.2 BY 4.76 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.29.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 5.71.

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
    SIZE 30.8 BY .95 NO-UNDO.

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
    begin_date AT ROW 2.67 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 2.67 COL 64 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_bank AT ROW 3.71 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Bank"
    end_bank AT ROW 3.71 COL 64 COLON-ALIGNED HELP
    "Enter Ending Bank"
    tb_cust-list AT ROW 5.05 COL 29.2 WIDGET-ID 6
    btnCustList AT ROW 5.14 COL 66 WIDGET-ID 8
    begin_cust AT ROW 6.19 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 6.19 COL 64 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    sl_avail AT ROW 8.38 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 8.38 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 8.38 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 9.38 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 10.38 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 11.43 COL 40.4 WIDGET-ID 40
    btn_down AT ROW 12.43 COL 40.4 WIDGET-ID 42
    rd-dest AT ROW 14.33 COL 4.8 NO-LABELS
    lv-font-no AT ROW 14.33 COL 34 COLON-ALIGNED
    lv-ornt AT ROW 14.33 COL 45 NO-LABELS
    lines-per-page AT ROW 14.33 COL 87 COLON-ALIGNED
    lv-font-name AT ROW 15.52 COL 29 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 16.71 COL 92 RIGHT-ALIGNED
    td-show-parm AT ROW 17.24 COL 28.8
    fi_file AT ROW 18.14 COL 26.8 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.24 COL 92.2 RIGHT-ALIGNED
    tbAutoClose AT ROW 19.33 COL 28.8 WIDGET-ID 64
    btn-ok AT ROW 20.29 COL 28.8
    btn-cancel AT ROW 20.29 COL 55
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 7.67 COL 60.4 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 13.62 COL 4
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.48 COL 4
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 7.67 COL 3 WIDGET-ID 38
    RECT-6 AT ROW 14.05 COL 3
    RECT-7 AT ROW 1.95 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 20.76
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
        TITLE              = "Bank Deposit Report"
        HEIGHT             = 20.76
        WIDTH              = 95.8
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
    begin_bank:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_bank:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Bank Deposit Report */
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
ON WINDOW-CLOSE OF C-Win /* Bank Deposit Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_bank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_bank C-Win
ON LEAVE OF begin_bank IN FRAME FRAME-A /* Beginning Bank */
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
                INPUT tb_cust-list AND glCustListActive,
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
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_bank
                            &END_cust=END_bank
                            &fax-subject="Bank Deposit List"
                            &fax-body="Bank Deposit List"
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_bank
                             &END_cust=end_bank
                             &mail-subject="Bank Deposit List"
                             &mail-body="Bank Deposit List"
                             &mail-file=lv-pdf-file + ".pdf" }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_bank
                                  &END_cust=end_bank
                                  &mail-subject="Bank Deposit List"
                                  &mail-body="Bank Deposit List"
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


&Scoped-define SELF-NAME end_bank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_bank C-Win
ON LEAVE OF end_bank IN FRAME FRAME-A /* Ending Bank */
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

    ASSIGN
        begin_date = DATE(1,1,YEAR(TODAY))
        end_date   = TODAY.

    FIND FIRST bank WHERE bank.company EQ gcompany NO-LOCK NO-ERROR.
    IF AVAILABLE bank THEN
        ASSIGN
            begin_bank = bank.bank-code
            end_bank   = bank.bank-code.

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "AR8" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "AR8",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.     
        APPLY "entry" TO begin_bank.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'AR8',
        INPUT NO,
        OUTPUT glCustListActive).

    {sys/inc/chblankcust.i ""AR8""}

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
            INPUT 'AR8',
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
        INPUT 'AR8').


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
    DISPLAY begin_date end_date begin_bank end_bank tb_cust-list begin_cust 
        end_cust sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_date end_date begin_bank end_bank tb_cust-list 
        btnCustList begin_cust end_cust sl_avail Btn_Def sl_selected Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose 
        btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-detail-values C-Win 
PROCEDURE get-detail-values :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-jrnl LIKE glhist.jrnl    NO-UNDO.
    DEFINE INPUT PARAMETER ip-dscr LIKE glhist.tr-dscr NO-UNDO.
    DEFINE INPUT PARAMETER ip-amt  LIKE glhist.tr-amt  NO-UNDO.
    DEFINE INPUT PARAMETER ip-void AS LOG               NO-UNDO.

    ASSIGN
        lv-check-no = 0
        lv-inv-no   = 0
        lv-amt      = 0
        lv-cust-no  = ""
        lv-name     = "".

    IF ip-jrnl EQ "CASHR" THEN 
    DO:
        lv-check-no = INT64(SUBSTR(ip-dscr,INDEX(ip-dscr," Inv# ") - 12,12)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN lv-check-no = 0.

        lv-inv-no = INT(SUBSTR(ip-dscr,INDEX(ip-dscr," Inv# ") + 6,10)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN lv-inv-no = 0.

        RELEASE ar-inv.
        IF lv-inv-no NE 0 THEN
            FIND FIRST ar-inv NO-LOCK
                WHERE ar-inv.company EQ cocode
                AND ar-inv.inv-no  EQ lv-inv-no
                NO-ERROR.

        IF lv-check-no NE 0 THEN
            FOR EACH ar-cash FIELDS(company cust-no c-no) NO-LOCK
                WHERE ar-cash.company  EQ cocode
                AND ar-cash.memo     EQ NO
                AND ar-cash.check-no EQ lv-check-no
                AND (NOT AVAILABLE ar-inv OR ar-cash.cust-no EQ ar-inv.cust-no),
                FIRST cust FIELDS(NAME)
                WHERE cust.company EQ ar-cash.company
                AND cust.cust-no EQ ar-cash.cust-no
                NO-LOCK:

                ASSIGN
                    lv-cust-no = ar-cash.cust-no
                    lv-name    = cust.name.

                IF lv-inv-no EQ 0 OR ip-void THEN lv-amt = ip-amt.

                ELSE
                    FOR EACH ar-cashl FIELDS(amt-paid)
                        WHERE ar-cashl.c-no   EQ ar-cash.c-no
                        AND ar-cashl.inv-no EQ lv-inv-no
                        NO-LOCK:

                        lv-amt = ar-cashl.amt-paid.
                        LEAVE.
                    END.
            END.
    END.

    ELSE 
    DO:      
        FIND FIRST ar-mcash WHERE ar-mcash.m-no EQ INT(ip-dscr) NO-LOCK NO-ERROR.
        IF AVAILABLE ar-mcash THEN
            ASSIGN
                lv-check-no = ar-mcash.m-no
                lv-name     = ar-mcash.payer
                lv-amt      = ar-mcash.check-amt.
    END.

    IF lv-name EQ "" THEN lv-name = lv-cust-no.

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
    /* ---------------------------------------------- gl/rep/gl-recon.p 06/97 FWK */
    /* Bank Statement                                                             */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-top.f}*/

    DEFINE VARIABLE v-s-date       AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "From Date" INIT ?.
    DEFINE VARIABLE v-e-date       AS DATE      FORMAT "99/99/9999" NO-UNDO LABEL "To Date" INIT TODAY.
    DEFINE VARIABLE v-s-bank       LIKE bank.bank-code LABEL "From Bank" NO-UNDO.
    DEFINE VARIABLE v-e-bank       LIKE bank.bank-code LABEL "To Bank" NO-UNDO.
    DEFINE VARIABLE date-loop      AS DATE      NO-UNDO.
    DEFINE VARIABLE tot-daily      AS DECIMAL   FORMAT "->>,>>>,>>9.99" INIT 0 EXTENT 2 NO-UNDO.
    DEFINE VARIABLE bank-tot       AS DECIMAL   FORMAT "->>,>>>,>>9.99" INIT 0 NO-UNDO.
    DEFINE VARIABLE dep-tot        AS DECIMAL   FORMAT "->>,>>>,>>9.99" INIT 0 NO-UNDO.
    DEFINE VARIABLE v-frst         AS LOG       INIT YES NO-UNDO.
    DEFINE VARIABLE v-date         AS DATE      NO-UNDO.
    DEFINE VARIABLE v-tr-num       AS CHARACTER FORMAT "x(26)" NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-void         AS LOG       NO-UNDO.
    DEFINE VARIABLE fcust          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tcust          AS CHARACTER NO-UNDO.

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


    {sys/form/r-top5L3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    FORM
        bank.bank-code AT 1 COLUMN-LABEL "BANK"
        bank.actnum AT 10 FORMAT "x(20)" COLUMN-LABEL "ACCOUNT NUMBER"
        v-date AT 31 FORMAT "99/99/99" COLUMN-LABEL "DATE"
        v-tr-num AT 40 COLUMN-LABEL "TRANSACTION#"
        tot-daily[2] TO 80 COLUMN-LABEL "DAILY TOTAL"
        WITH FRAME day-log NO-BOX STREAM-IO WIDTH 80 DOWN.

    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-date  = begin_date
        v-e-date  = end_date
        v-s-bank  = begin_bank
        v-e-bank  = end_bank
        lSelected = tb_cust-list
        fcust     = begin_cust
        tcust     = END_cust .


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


        IF LOOKUP(ttRptSelected.TextList, "DAILY TOTAL") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 

    END.
    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld EQ TRUE NO-ERROR.
        IF AVAILABLE ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld EQ TRUE NO-ERROR.
        IF AVAILABLE ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
    END.     

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).
        /* excelheader = "BANK,ACCT/CUST NUMBER,DATE,TRANSACTION#,DAILY TOTAL". */
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    /*display str-tit with frame r-top.*/
    DISPLAY "" WITH FRAME r-top.
    /*
    IF tb_det THEN
    DO WITH FRAME day-log:
       bank.actnum:LABEL = "ACCT/CUST NUMBER".
    END. */

    SESSION:SET-WAIT-STATE ("general").

    EMPTY TEMP-TABLE tt-glhist.

    FOR EACH ar-cash NO-LOCK
        WHERE ar-cash.company   EQ cocode
        AND ar-cash.posted    EQ YES
        AND ar-cash.bank-code GE v-s-bank
        AND ar-cash.bank-code LE v-e-bank
        AND ar-cash.cust-no   GE fcust
        AND ar-cash.cust-no   LE tcust
        :
        IF lselected AND
            NOT CAN-FIND(FIRST ttCustList
            WHERE ttCustList.cust-no EQ ar-cash.cust-no
            AND ttCustList.log-fld) THEN NEXT.

        FOR EACH ar-cashl NO-LOCK
            WHERE ar-cashl.c-no EQ ar-cash.c-no
            :
            {custom/statusMsg.i " 'Processing Bank#  '  + ar-cash.bank-code "}
            v-void = ar-cashl.voided EQ YES.

            IF NOT v-void THEN
                FIND FIRST ar-ledger NO-LOCK
                    WHERE ar-ledger.company EQ ar-cash.company
                    AND ar-ledger.cust-no EQ ar-cash.cust-no
                    AND ar-ledger.ref-num EQ "CHK# " + STRING(ar-cash.check-no,"999999999999")
                    AND ar-ledger.tr-date GE v-s-date
                    AND ar-ledger.tr-date LE v-e-date
                    NO-ERROR.
            ELSE
                FIND FIRST ar-ledger NO-LOCK
                    WHERE ar-ledger.company EQ ar-cash.company
                    AND ar-ledger.cust-no EQ ar-cash.cust-no
                    AND ar-ledger.ref-num EQ "VOIDED CHK# " + STRING(ar-cash.check-no,"999999999999")
                    AND ar-ledger.tr-date GE v-s-date
                    AND ar-ledger.tr-date LE v-e-date
                    NO-ERROR.

            IF NOT AVAILABLE ar-ledger THEN NEXT.

            CREATE tt-glhist.
            ASSIGN
                tt-glhist.company = ar-cash.company
                tt-glhist.actnum  = ar-cash.bank-code
                tt-glhist.jrnl    = "CASHR"
                tt-glhist.tr-dscr = ar-cash.cust-no + " " +
                             STRING(ar-cash.check-no,"999999999999") +
                             " Inv# " + STRING(ar-cashl.inv-no)
                tt-glhist.tr-date = ar-ledger.tr-date
                tt-glhist.tr-amt  = ar-cashl.amt-paid
                tt-glhist.tr-num  = ar-ledger.tr-num
                tt-glhist.VOID    = v-void
                .
            RELEASE tt-glhist.
        END. /* each ar-cashl */
    END.

    IF TRIM(begin_cust) EQ "" AND
        end_cust EQ "zzzzzzzz" AND NOT lselected THEN
        FOR EACH ar-mcash
            WHERE ar-mcash.company   EQ cocode
            AND ar-mcash.posted    EQ YES
            AND ar-mcash.bank-code GE v-s-bank
            AND ar-mcash.bank-code LE v-e-bank
            NO-LOCK,
            FIRST ar-ledger
            WHERE ar-ledger.company EQ ar-mcash.company
            AND ar-ledger.cust-no EQ ""
            AND ar-ledger.ref-num EQ STRING(ar-mcash.m-no) + " " + ar-mcash.payer
            AND INDEX(ar-ledger.ref-num,ar-mcash.payer) GT 0
            AND ar-ledger.tr-date GE v-s-date
            AND ar-ledger.tr-date LE v-e-date
            NO-LOCK:
            {custom/statusMsg.i " 'Processing Bank#  '  + ar-mcash.bank-code "}
            CREATE tt-glhist.
            ASSIGN
                tt-glhist.company = ar-mcash.company
                tt-glhist.actnum  = ar-mcash.bank-code
                tt-glhist.jrnl    = "MCSHREC"
                tt-glhist.tr-dscr = STRING(ar-mcash.m-no)
                tt-glhist.tr-date = ar-ledger.tr-date
                tt-glhist.tr-amt  = ar-ledger.amt
                tt-glhist.tr-num  = ar-ledger.tr-num.
            RELEASE tt-glhist.
        END.

    FOR EACH bank
        WHERE bank.company   EQ cocode
        AND bank.bank-code GE v-s-bank
        AND bank.bank-code LE v-e-bank
        NO-LOCK                     
        BREAK BY bank.bank-code:
        {custom/statusMsg.i " 'Processing Bank#  '  + bank.bank-code "}
        ASSIGN
            v-frst   = YES
            bank-tot = 0.

        DO date-loop = v-s-date TO v-e-date:
            FOR EACH tt-glhist
                WHERE tt-glhist.company EQ bank.company
                AND tt-glhist.actnum  EQ bank.bank-code
                AND tt-glhist.tr-date EQ date-loop
                NO-LOCK

                BREAK BY tt-glhist.tr-date
                BY tt-glhist.tr-num
                BY tt-glhist.tr-dscr:

                /*  IF v-frst THEN
                  DO:
                    display bank.bank-code when v-frst
                            bank.actnum when v-frst
            
                        with frame day-log.
            
                    IF tb_excel THEN
                      PUT STREAM excel UNFORMATTED
                          '"' (IF v-frst THEN bank.bank-code ELSE "") '",'
                          '"' (IF v-frst THEN bank.actnum ELSE "")    '",'
                          SKIP.
                  END. */

                /*  IF tb_det THEN DO:
                     IF v-frst THEN DOWN WITH FRAME day-log. */

                RUN get-detail-values (tt-glhist.jrnl, tt-glhist.tr-dscr, tt-glhist.tr-amt,
                    tt-glhist.VOID).

                IF lv-amt EQ 0 THEN lv-amt = tt-glhist.tr-amt.

                /*  IF lv-check-no NE 0 THEN DO:
                    DISPLAY lv-name     @ bank.actnum
                            "Chk " + TRIM(STRING(lv-check-no,">>>>>>>>>>")) + " " +
                            (IF lv-inv-no NE 0 THEN
                               "Inv " + TRIM(STRING(lv-inv-no,">>>>>>>>")) ELSE "")
                                        @ v-tr-num
                            lv-amt      @ tot-daily[2]
         
                        WITH FRAME day-log.
                    DOWN WITH FRAME day-log.
         
                    IF tb_excel THEN
                      PUT STREAM excel UNFORMATTED
                          '"' ""      '",'
                          '"' lv-name '",'
                          '"' ""      '",'
                          '"' "Chk " + TRIM(STRING(lv-check-no,">>>>>>>>>>")) + " " +
                              (IF lv-inv-no NE 0 THEN
                              "Inv " + TRIM(STRING(lv-inv-no,">>>>>>>>")) ELSE "")  '",'
                          '"' STRING(lv-amt,"->>,>>>,>>9.99") '",'
                         SKIP.
                  END. */
                /*  END. */

                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField: 
                        WHEN "bank"   THEN 
                            cVarValue = IF v-frst THEN bank.bank-code ELSE "" .
                        WHEN "act"    THEN 
                            cVarValue = IF v-frst THEN bank.actnum ELSE "" .
                        WHEN "cust"   THEN 
                            cVarValue = lv-name .
                        WHEN "date"   THEN 
                            cVarValue = "" .
                        WHEN "trns"   THEN 
                            cVarValue = "" .
                        WHEN "ttl"    THEN 
                            cVarValue = STRING(lv-amt,"->>,>>>,>>9.99") .
                        WHEN "chk"    THEN 
                            cVarValue = IF lv-check-no NE 0 THEN STRING(lv-check-no,">>>>>>>>>>>>")  ELSE "" .
                        WHEN "inv"    THEN 
                            cVarValue = IF lv-inv-no NE 0 THEN STRING(lv-inv-no,">>>>>>>>") ELSE ""  .

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

                ASSIGN
                    v-frst       = NO
                    tot-daily[1] = tot-daily[1] + tt-glhist.tr-amt.

                IF LAST-OF(tt-glhist.tr-num) THEN 
                DO:
                    /* IF tb_det THEN
                     DO:
                       UNDERLINE tot-daily[2] WITH FRAME day-log.
             
                       if tb_excel then
                         PUT STREAM excel UNFORMATTED
                             '"' "" '",'
                             '"' "" '",'
                             '"' "" '",'
                             '"' "" '",'
                             '"' "----------------------" '",'
                            SKIP.
                     END. */

                    /*  display tt-glhist.tr-date @ v-date
                              TRIM(STRING(tt-glhist.trnum,">>>>>>>>>>")) @ v-tr-num
                              tot-daily[1] @ tot-daily[2]
              
                          with frame day-log.
                      down with frame day-log.
              
                      IF tb_excel THEN
                        PUT STREAM excel UNFORMATTED
                            '"' "" '",'
                            '"' "" '",'
                            '"' (IF tt-glhist.tr-date NE ? THEN STRING(tt-glhist.tr-date)
                                 ELSE "") '",'
                            '"' TRIM(STRING(tt-glhist.trnum,">>>>>>>>>>")) '",'
                            '"' STRING(tot-daily[1],"->>,>>>,>>9.99") '",'
                           SKIP. */
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
                            WHEN "bank"   THEN 
                                cVarValue = "" .
                            WHEN "act"    THEN 
                                cVarValue = "" .
                            WHEN "cust"   THEN 
                                cVarValue = "" .
                            WHEN "date"   THEN 
                                cVarValue = IF tt-glhist.tr-date NE ? THEN STRING(tt-glhist.tr-date) ELSE "" .
                            WHEN "trns"   THEN 
                                cVarValue = TRIM(STRING(tt-glhist.tr-num,">>>>>>>>>>")) .
                            WHEN "ttl"    THEN 
                                cVarValue = STRING(tot-daily[1],"->>,>>>,>>9.99") .
                            WHEN "chk"    THEN 
                                cVarValue = "" .
                            WHEN "inv"    THEN 
                                cVarValue = ""  .

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

                    /*IF tb_det AND NOT LAST-OF(tt-glhist.tr-date) THEN PUT SKIP(1). */

                    ASSIGN
                        tot-daily[2] = tot-daily[2] + tot-daily[1]
                        tot-daily[1] = 0.
                END.

                IF LAST-OF(tt-glhist.tr-date) THEN 
                DO:
                    /* UNDERLINE tot-daily[2] WITH FRAME day-log.
                     IF tb_det THEN DO:
                       down with frame day-log.
                       UNDERLINE tot-daily[2] WITH FRAME day-log.
             
                       IF tb_excel THEN
                         PUT STREAM excel UNFORMATTED
                             '"' ""           '",'
                             '"' ""           '",'
                             '"' ""           '",'
                             '"' ""           '",'
                             '"' "----------------------" '",'
                             SKIP.
                     END. */

                    /*   display tot-daily[2]
               
                           with frame day-log.
                       down with frame day-log.
               
                       IF tb_excel THEN
                         PUT STREAM excel UNFORMATTED
                              '"' ""           '",'
                              '"' ""           '",'
                              '"' ""           '",'
                              '"' ""           '",'
                              '"' STRING(tot-daily[2],"->>,>>>,>>9.99") '",'
                             SKIP.
               
                       PUT SKIP(1). */
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
                            WHEN "bank"   THEN 
                                cVarValue = "" .
                            WHEN "act"    THEN 
                                cVarValue = "" .
                            WHEN "cust"   THEN 
                                cVarValue = "" .
                            WHEN "date"   THEN 
                                cVarValue = "" .
                            WHEN "trns"   THEN 
                                cVarValue = "" .
                            WHEN "ttl"    THEN 
                                cVarValue = STRING(tot-daily[2],"->>,>>>,>>9.99") .
                            WHEN "chk"    THEN 
                                cVarValue = "" .
                            WHEN "inv"    THEN 
                                cVarValue = ""  .

                        END CASE.

                        cExcelVarValue = cVarValue.
                        cDisplay = cDisplay + cVarValue +
                            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                    END.

                    PUT UNFORMATTED cDisplay SKIP(1).
                    IF tb_excel THEN 
                    DO:
                        PUT STREAM excel UNFORMATTED  
                            cExcelDisplay SKIP.
                    END.

                    ASSIGN
                        bank-tot     = bank-tot + tot-daily[2]
                        tot-daily[2] = 0.
                END.
            END.
        END. /* date-loop */

        IF bank-tot NE 0 THEN
        DO:
            /* put skip
                 "--------------"  to 80
                 skip
                 "Total Deposits"  at 40
                 bank-tot          to 80
                 skip(2).
         
             IF tb_excel THEN
             DO:
               PUT STREAM excel UNFORMATTED SKIP(1).
               PUT STREAM excel UNFORMATTED
                   '"' ""               '",'
                   '"' ""               '",'
                   '"' ""               '",'
                   '"' "Total Deposits" '",'
                   '"' STRING(bank-tot,"->>,>>>,>>9.99") '",'
                  SKIP(2).
             END. */
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
                    WHEN "bank"   THEN 
                        cVarValue = "" .
                    WHEN "act"    THEN 
                        cVarValue = "" .
                    WHEN "cust"   THEN 
                        cVarValue = "" .
                    WHEN "date"   THEN 
                        cVarValue = "" .
                    WHEN "trns"   THEN 
                        cVarValue = "" .
                    WHEN "ttl"    THEN 
                        cVarValue = STRING(bank-tot,"->>,>>>,>>9.99") .
                    WHEN "chk"    THEN 
                        cVarValue = "" .
                    WHEN "inv"    THEN 
                        cVarValue = ""  .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED  
                "       Total Deposits" SUBSTRING(cDisplay,22,300) SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    "  Total Deposits  " + substring(cExcelDisplay,3,300) SKIP.
            END.
        END.


        dep-tot = dep-tot + bank-tot.
    END.

    /*if dep-tot ne 0 then put "Total Deposits for All Banks" dep-tot to 80 skip.
    
    IF tb_excel THEN
      PUT STREAM excel UNFORMATTED
           '"' "Total Deposits for All Banks"   '",'
           '"' ""                               '",'
           '"' ""                               '",'
           '"' ""                               '",'
           '"' STRING(dep-tot,"->>,>>>,>>9.99") '",'
          SKIP. */
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
            WHEN "bank"   THEN 
                cVarValue = "" .
            WHEN "act"    THEN 
                cVarValue = "" .
            WHEN "cust"   THEN 
                cVarValue = "" .
            WHEN "date"   THEN 
                cVarValue = "" .
            WHEN "trns"   THEN 
                cVarValue = "" .
            WHEN "ttl"    THEN 
                cVarValue = STRING(dep-tot,"->>,>>>,>>9.99") .
            WHEN "chk"    THEN 
                cVarValue = "" .
            WHEN "inv"    THEN 
                cVarValue = ""  .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.

    PUT UNFORMATTED  
        "       Total Deposits for All Banks" SUBSTRING(cDisplay,36,300) SKIP.
    IF tb_excel THEN 
    DO:
        PUT STREAM excel UNFORMATTED  
            "  Total Deposits for All Banks  " + substring(cExcelDisplay,3,300) SKIP.
    END.

    IF tb_excel THEN 
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
            fi_file:SCREEN-VALUE = "c:\tmp\BankDeposit.csv".    
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

