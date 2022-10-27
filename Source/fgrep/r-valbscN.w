&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-valbsc.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/*  Mod: Ticket - 103137  Format Change for Order No. and Job No.       */          

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

/*{sys/inc/custlistform.i ""IL13"" }*/
{sys/ref/CustList.i NEW}

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report 
    FIELD qty AS DECIMAL.
DEFINE NEW SHARED TEMP-TABLE tt-file NO-UNDO
    FIELD tt-sman    LIKE sman.sman
    FIELD tt-cust-no LIKE cust.cust-no
    FIELD tt-i-no    LIKE itemfg.i-no .

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
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN 
    cTextListToSelect  = "CUST#,FG ITEM#,CUSTOMER PART#,DESCRIPTION,CUST PO#,JOB#,ORDERED QTY,SHIPPED QTY," +
                           "ON-HAND QTY,RECEIPT DATE,SELL PRICE,TOTAL VALUE"
    cFieldListToSelect = "cust,fgitem,cust-part,desc,cust-po,job,ord-qty,ship-qty," +
                            "oh-hand-qty,rec-date,sell-pr,tot-val"
    cFieldLength       = "8,15,15,30,15,13,12,12," + "12,12,12,15"
    cFieldType         = "c,c,c,c,c,c,i,i," + "i,c,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "CUST#,FG ITEM#,CUSTOMER PART#,DESCRIPTION,CUST PO#,JOB#,ORDERED QTY,SHIPPED QTY," +
                           "ON-HAND QTY,RECEIPT DATE,SELL PRICE,TOTAL VALUE" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 as-of-date tb_cust-list ~
btnCustList begin_cust-no end_cust-no begin_slm end_slm tb_zero tb_whs ~
tb_detail sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date tb_cust-list begin_cust-no ~
end_cust-no begin_slm end_slm tb_zero tb_whs tb_detail sl_avail sl_selected ~
rd-dest fi_file tb_OpenCSV tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE as-of-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/01 
    LABEL "As of" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust-no  AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_slm      AS CHARACTER FORMAT "XXX":U 
    LABEL "Beginning Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no    AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_slm        AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
    LABEL "Ending Sales Rep#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\SalesValueByCustomer.csv" 
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
    SIZE 16.2 BY 4.05 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 4.91.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 8.33.

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
    SIZE 36.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_detail    AS LOGICAL   INITIAL NO 
    LABEL "Detail?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_whs       AS LOGICAL   INITIAL NO 
    LABEL "Include Customer Owned Warehouse?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zero      AS LOGICAL   INITIAL NO 
    LABEL "Include Zero Quantity Items?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    as-of-date AT ROW 2.24 COL 27.8 COLON-ALIGNED
    tb_cust-list AT ROW 3.33 COL 30 WIDGET-ID 6
    btnCustList AT ROW 3.33 COL 72.6 WIDGET-ID 8
    begin_cust-no AT ROW 4.29 COL 27.8 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust-no AT ROW 4.29 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_slm AT ROW 5.24 COL 27.8 COLON-ALIGNED HELP
    "Enter Beginning Sales Rep Number"
    end_slm AT ROW 5.24 COL 70.8 COLON-ALIGNED HELP
    "Enter Ending Sales Rep Number"
    tb_zero AT ROW 6.43 COL 30
    tb_whs AT ROW 7.48 COL 30
    tb_detail AT ROW 8.57 COL 30
    sl_avail AT ROW 10.81 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 10.81 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 10.81 COL 61.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 11.81 COL 40.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.81 COL 40.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.86 COL 40.8 WIDGET-ID 40
    btn_down AT ROW 14.86 COL 40.8 WIDGET-ID 42
    lv-font-no AT ROW 16.76 COL 34 COLON-ALIGNED
    lines-per-page AT ROW 16.81 COL 87.6 COLON-ALIGNED
    lv-ornt AT ROW 16.86 COL 43.6 NO-LABELS
    rd-dest AT ROW 16.95 COL 4.8 NO-LABELS
    lv-font-name AT ROW 17.95 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 19.05 COL 28.2
    fi_file AT ROW 19.95 COL 26 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 20.05 COL 92.8 RIGHT-ALIGNED
    tbAutoClose AT ROW 21.62 COL 28 WIDGET-ID 62
    btn-ok AT ROW 22.67 COL 28
    btn-cancel AT ROW 22.67 COL 56
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.1 COL 3 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.1 COL 61.4 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 16.14 COL 4.2
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 15 
    RECT-6 AT ROW 16.57 COL 3
    RECT-7 AT ROW 1.71 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 23.19
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
        TITLE              = "Sales Value by Sales Rep by Customer"
        HEIGHT             = 23.19
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
    as-of-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_slm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_slm:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
    tb_detail:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_whs:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_zero:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Sales Value by Sales Rep by Customer */
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
ON WINDOW-CLOSE OF C-Win /* Sales Value by Sales Rep by Customer */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
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
  
        FIND FIRST  ttCustList NO-LOCK NO-ERROR.
        IF NOT tb_cust-list OR  NOT AVAILABLE ttCustList THEN 
        DO:
            EMPTY TEMP-TABLE ttCustList.
            RUN BuildCustList(INPUT cocode,
                INPUT tb_cust-list AND glCustListActive ,
                INPUT begin_cust-no,
                INPUT END_cust-no).
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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
                    END.
                END. /* WHEN 3 THEN DO: */
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=END_cust-no
                            &END_cust=END_cust-no
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
                             &begin_cust= END_cust-no
                             &END_cust=END_cust-no
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust-no
                                  &END_cust=END_cust-no
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
    DO:
        DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
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


&Scoped-define SELF-NAME tb_detail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_detail C-Win
ON VALUE-CHANGED OF tb_detail IN FRAME FRAME-A /* Detail? */
    DO:
        IF {&self-name}:SCREEN-VALUE EQ "YES" THEN lv-ornt:SCREEN-VALUE = "L".
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


&Scoped-define SELF-NAME tb_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_whs C-Win
ON VALUE-CHANGED OF tb_whs IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Include Zero Quantity Items? */
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
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "IL13" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "IL13",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO as-of-date.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'IL13',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""IL13""}

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
            INPUT 'IL13',
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
        INPUT 'IL13').


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
    DISPLAY as-of-date tb_cust-list begin_cust-no end_cust-no begin_slm end_slm 
        tb_zero tb_whs tb_detail sl_avail sl_selected rd-dest fi_file 
        tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 as-of-date tb_cust-list btnCustList begin_cust-no 
        end_cust-no begin_slm end_slm tb_zero tb_whs tb_detail sl_avail 
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
    /* ----------------------------------------------- fg/rep/valbycs.p 12/02 JLF */
    /*  finished goods sales value by salesrep by customer                        */
    /* -------------------------------------------------------------------------- */

    {sys/form/r-topw3.f}

    DEFINE VARIABLE vdat           AS DATE      INIT TODAY FORMAT "99/99/9999".
    DEFINE VARIABLE fcus           LIKE itemfg.cust-no.
    DEFINE VARIABLE tcus           LIKE fcus INIT "zzzzzzzz".
    DEFINE VARIABLE fsls           LIKE cust.sman.
    DEFINE VARIABLE tsls           LIKE fsls INIT "zzz".
    DEFINE VARIABLE vzer           AS LOG       FORMAT "Y/N" INIT NO.
    DEFINE VARIABLE vwhs           LIKE vzer.
    DEFINE VARIABLE cSlsRep        AS CHARACTER NO-UNDO .
    DEFINE VARIABLE v-sman         LIKE sman.sman.
    DEFINE VARIABLE v-sname        LIKE sman.sname.
    DEFINE VARIABLE v-frst         AS LOG       NO-UNDO.
    DEFINE VARIABLE v-print        AS LOG       NO-UNDO.
    DEFINE VARIABLE v-bin          AS LOG       NO-UNDO.
    DEFINE VARIABLE v-price        LIKE itemfg.sell-price NO-UNDO.
    DEFINE VARIABLE v-uom          LIKE itemfg.sell-uom NO-UNDO.
    DEFINE VARIABLE v-cas-cnt      LIKE itemfg.case-count NO-UNDO.
    DEFINE VARIABLE v-binqty       AS DECIMAL.
    DEFINE VARIABLE v-ord          AS DECIMAL   EXTENT 4.
    DEFINE VARIABLE v-shp          AS DECIMAL   EXTENT 4.
    DEFINE VARIABLE v-qoh          AS DECIMAL   EXTENT 4.
    DEFINE VARIABLE v-ext          AS DECIMAL   EXTENT 4.
    DEFINE VARIABLE v-date         AS DATE      NO-UNDO.
    DEFINE VARIABLE v-ord-qty      LIKE oe-ordl.qty.
    DEFINE VARIABLE v-job          AS CHARACTER FORMAT "x(13)" NO-UNDO.
    DEFINE VARIABLE li-inv-qty     LIKE oe-ordl.inv-qty NO-UNDO.
    DEFINE VARIABLE li-ship-qty    LIKE oe-ordl.ship-qty NO-UNDO.
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

    /*{sys/form/r-top5DL3.f} */
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.   


    FORM HEADER
        SKIP(1)
        "Sales Rep:" v-sman v-sname
        SKIP(1)
        str-tit4 SKIP
        str-tit5 SKIP
        WITH FRAME r-top.

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        vdat     = as-of-date
        fcus     = begin_cust-no
        tcus     = end_cust-no
        fsls     = begin_slm
        tsls     = end_slm
        vzer     = tb_zero
        vwhs     = tb_whs.

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

        IF LOOKUP(ttRptSelected.TextList, "ORDERED QTY,SHIPPED QTY,ON-HAND QTY,TOTAL VALUE") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE("general").

    VIEW FRAME r-top.

    /*IF tb_detail THEN VIEW FRAME r-top2.
                 ELSE VIEW FRAME r-top1.*/

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    EMPTY TEMP-TABLE tt-file.
    FOR EACH ttCustList 
        WHERE ttCustList.log-fld
        NO-LOCK,
        EACH cust
        WHERE cust.company EQ cocode
        /*and cust.cust-no ge fcus*/
        AND cust.cust-no EQ ttCustList.cust-no
        /*and cust.sman    ge fsls
        and cust.sman    le tsls*/
        NO-LOCK,

        EACH itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.cust-no EQ cust.cust-no
        USE-INDEX customer NO-LOCK 

        BREAK BY cust.sman
        BY cust.cust-no:

        {custom/statusMsg.i " 'Processing Customer#/FG item  '  + cust.cust-no + '/' + itemfg.i-no "}


        RUN fg/fgSlsRep.p (INPUT itemfg.company,
            INPUT itemfg.cust-no,
            INPUT itemfg.part-no,
            INPUT itemfg.i-no,
            OUTPUT cSlsRep).
        CREATE tt-file.
        ASSIGN
            tt-file.tt-sman    = IF cSlsRep = "" THEN cust.sman ELSE cSlsRep 
            tt-file.tt-cust-no = cust.cust-no
            tt-file.tt-i-no    = itemfg.i-no  .
    END.

    FOR EACH tt-file WHERE
        tt-file.tt-sman GE fsls
        AND tt-file.tt-sman LE tsls
        ,    
        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ tt-cust-no
        NO-LOCK,    
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ tt-i-no
        NO-LOCK    
        BREAK BY tt-sman
        BY tt-cust-no :
        {custom/statusMsg.i " 'Processing Customer#/FG item  '  + cust.cust-no + '/' + itemfg.i-no "}

        IF FIRST-OF(tt-sman) THEN 
        DO:
            FIND FIRST sman
                WHERE sman.company EQ cust.company
                AND sman.sman    EQ tt-file.tt-sman
                NO-LOCK NO-ERROR.
            ASSIGN
                v-sman  = tt-file.tt-sman
                v-sname = IF AVAILABLE sman THEN sman.sname ELSE "Not on file".
            PAGE.
        END.

        IF FIRST-OF(tt-cust-no) THEN
            ASSIGN
                v-frst  = YES
                v-print = NO.

        v-bin = NO.

        FOR EACH tt-report:
            DELETE tt-report.
        END.

        FOR EACH fg-bin
            WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ itemfg.i-no
            AND (vwhs OR (fg-bin.loc NE "CUST" AND fg-bin.cust-no EQ ""))
            USE-INDEX i-no,

            EACH fg-rcpth
            WHERE fg-rcpth.company      EQ cocode
            AND fg-rcpth.i-no         EQ itemfg.i-no
            AND fg-rcpth.job-no       EQ fg-bin.job-no
            AND fg-rcpth.job-no2      EQ fg-bin.job-no2
            AND fg-rcpth.trans-date   LE vdat
            NO-LOCK USE-INDEX tran,

            EACH fg-rdtlh
            WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
            AND fg-rdtlh.loc          EQ fg-bin.loc
            AND fg-rdtlh.loc-bin      EQ fg-bin.loc-bin
            AND fg-rdtlh.tag          EQ fg-bin.tag
            AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
            NO-LOCK USE-INDEX rm-rdtl

            BREAK BY fg-bin.job-no
            BY fg-bin.job-no2
            BY fg-bin.loc
            BY fg-bin.loc-bin
            BY fg-bin.tag
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

            v-bin = YES.        

            IF INDEX("RATE",fg-rcpth.rita-code) NE 0 THEN
                v-binqty = v-binqty + fg-rdtlh.qty.

            ELSE
                IF fg-rcpth.rita-code EQ "C" THEN
                    v-binqty = fg-rdtlh.qty.

                ELSE
                    IF fg-rcpth.rita-code EQ "S" THEN
                        v-binqty = v-binqty - fg-rdtlh.qty.

            IF LAST-OF(fg-bin.tag) THEN
                ASSIGN
                    v-qoh[1] = v-qoh[1] + v-binqty
                    v-binqty = 0.

            IF LAST-OF(fg-bin.job-no2) THEN 
            DO:
                FIND LAST oe-ordl
                    WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.i-no    EQ fg-bin.i-no
                    AND oe-ordl.job-no  EQ fg-bin.job-no
                    AND oe-ordl.job-no2 EQ fg-bin.job-no2
                    USE-INDEX item NO-LOCK NO-ERROR.

                IF AVAILABLE oe-ordl THEN
                    FIND FIRST oe-ord
                        WHERE oe-ord.company EQ cocode
                        AND oe-ord.ord-no  EQ oe-ordl.ord-no
                        NO-LOCK.

                v-date = IF AVAILABLE oe-ordl THEN oe-ordl.req-date
                ELSE ?.

                CREATE tt-report.
                ASSIGN
                    tt-report.key-01 = IF v-date EQ ? THEN ""
                              ELSE STRING(YEAR(v-date),"9999") +
                                   string(MONTH(v-date),"99")  +
                                   string(DAY(v-date),"99")
                    tt-report.qty    = v-qoh[1]
                    v-qoh[1]         = 0
                    tt-report.rec-id = RECID(fg-bin).
            END.
        END.

        FOR EACH tt-report,
            FIRST fg-bin WHERE RECID(fg-bin) EQ tt-report.rec-id NO-LOCK

            BY tt-report.key-01
            BY fg-bin.job-no
            BY fg-bin.job-no2:

            v-qoh[1] = tt-report.qty.

            ASSIGN
                v-ord-qty = 0
                v-price   = itemfg.sell-price
                v-uom     = itemfg.sell-uom
                v-cas-cnt = itemfg.case-count
                v-job     = TRIM(fg-bin.job-no) + "-" + STRING(fg-bin.job-no2,"999").

            IF TRIM(v-job) EQ "-000" THEN v-job = "".

            FOR EACH oe-ordl
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.i-no    EQ fg-bin.i-no
                AND oe-ordl.job-no  EQ fg-bin.job-no
                AND oe-ordl.job-no2 EQ fg-bin.job-no2
                USE-INDEX item NO-LOCK,
                FIRST oe-ord OF oe-ordl 
                WHERE oe-ord.ord-date LE vdat
                NO-LOCK
                BREAK BY oe-ordl.company
                BY oe-ordl.req-date:

                RUN oe/ordlsqty.p (ROWID(oe-ordl),
                    OUTPUT li-inv-qty, OUTPUT li-ship-qty).

                ASSIGN
                    v-ord-qty = oe-ordl.qty
                    v-price   = oe-ordl.price
                    v-uom     = oe-ordl.pr-uom
                    v-cas-cnt = itemfg.case-count

                    v-ord[1]  = v-ord[1] + oe-ordl.qty
                    v-shp[1]  = v-shp[1] + li-ship-qty.
            END.

            IF v-uom EQ "L" AND v-ord-qty NE 0 THEN
                v-ext[1] = v-price / v-ord-qty * v-qoh[1].

            ELSE
                IF v-uom EQ "CS"  AND
                    v-cas-cnt NE 0 THEN
                    v-ext[1] = (v-qoh[1] * v-price) / v-cas-cnt.

                ELSE 
                DO:
                    v-ext[1] = v-qoh[1] * v-price.
                    FIND FIRST uom
                        WHERE uom.uom  EQ v-uom
                        AND uom.mult NE 0
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE uom THEN v-ext[1] = v-ext[1] / uom.mult.
                END.

            IF v-qoh[1] NE 0 OR vzer THEN
                IF tb_detail THEN
                    FOR EACH oe-ordl
                        WHERE oe-ordl.company EQ cocode
                        AND oe-ordl.i-no    EQ fg-bin.i-no
                        AND oe-ordl.job-no  EQ fg-bin.job-no
                        AND oe-ordl.job-no2 EQ fg-bin.job-no2
                        USE-INDEX item NO-LOCK,
                        FIRST oe-ord OF oe-ordl 
                        WHERE oe-ord.ord-date LE vdat
                        NO-LOCK
                        BREAK BY oe-ordl.company
                        BY oe-ordl.req-date:

                        RUN oe/ordlsqty.p (ROWID(oe-ordl),
                            OUTPUT li-inv-qty, OUTPUT li-ship-qty).

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
                                    cVarValue = IF v-frst  THEN STRING(cust.cust-no,"x(8)") ELSE "".
                                WHEN "fgitem"   THEN 
                                    cVarValue = IF FIRST(oe-ordl.company) THEN STRING(itemfg.i-no,"x(15)") ELSE "".
                                WHEN "cust-part"   THEN 
                                    cVarValue = IF FIRST(oe-ordl.company) THEN STRING(oe-ordl.part-no,"x(15)") ELSE "".
                                WHEN "desc"  THEN 
                                    cVarValue = IF FIRST(oe-ordl.company) THEN STRING(itemfg.i-name,"x(30)") ELSE "".
                                WHEN "cust-po"   THEN 
                                    cVarValue = STRING(oe-ordl.po-no,"x(15)") .
                                WHEN "job"  THEN 
                                    cVarValue = IF FIRST(oe-ordl.company) THEN STRING(v-job,"x(13)") ELSE "" .
                                WHEN "ord-qty"   THEN 
                                    cVarValue = STRING(oe-ordl.qty,"->>>,>>>,>>9") .
                                WHEN "ship-qty"  THEN 
                                    cVarValue = STRING(li-ship-qty,"->>>,>>>,>>9") .
                                WHEN "oh-hand-qty"   THEN 
                                    cVarValue = IF LAST(oe-ordl.company) THEN STRING(v-qoh[1],"->>>,>>>,>>9") ELSE "" .
                                WHEN "rec-date"  THEN 
                                    cVarValue = STRING(oe-ordl.req-date,"99/99/9999") .
                                WHEN "sell-pr"   THEN 
                                    cVarValue = STRING(oe-ordl.price,"->>>>,>>9.99") .
                                WHEN "tot-val"  THEN 
                                    cVarValue = IF LAST(oe-ordl.company) THEN STRING(v-ext[1],"->>>,>>>,>>9.99") ELSE "" .

                            END CASE.
                            
                            IF cTmpField = "rec-date"   THEN 
                               cExcelVarValue = DYNAMIC-FUNCTION("sfFormat_Date",oe-ordl.req-date).
                
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

                    /* DISPLAY cust.cust-no        WHEN v-frst
                             itemfg.i-no         WHEN FIRST(oe-ordl.company)
                             oe-ordl.part-no     WHEN FIRST(oe-ordl.company)
                             itemfg.i-name       WHEN FIRST(oe-ordl.company)
                             oe-ordl.po-no
                             v-job               WHEN FIRST(oe-ordl.company)
                             oe-ordl.qty
                             li-ship-qty
                             v-qoh[1]            WHEN LAST(oe-ordl.company)
                             oe-ordl.req-date
                             oe-ordl.price
                             v-ext[1]            WHEN LAST(oe-ordl.company)
                       WITH FRAME detail2.
                     DOWN WITH FRAME detail2.
                     */
                    END.

                ELSE 
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
                                cVarValue = IF v-frst THEN STRING(cust.cust-no,"x(8)") ELSE "" .
                            WHEN "fgitem"   THEN 
                                cVarValue = STRING(itemfg.i-no,"x(15)").
                            WHEN "cust-part"   THEN 
                                cVarValue = /*STRING(oe-ordl.part-no,"x(15)")*/ "".
                            WHEN "desc"  THEN 
                                cVarValue = STRING(itemfg.i-name,"x(30)") .
                            WHEN "cust-po"   THEN 
                                cVarValue = /*STRING(oe-ordl.po-no,"x(15)")*/ "" .
                            WHEN "job"  THEN 
                                cVarValue = STRING(v-job,"x(13)")  .
                            WHEN "ord-qty"   THEN 
                                cVarValue = STRING(v-ord[1],"->>>,>>>,>>9") .
                            WHEN "ship-qty"  THEN 
                                cVarValue = STRING(v-shp[1],"->>>,>>>,>>9") .
                            WHEN "oh-hand-qty"   THEN 
                                cVarValue = STRING(v-qoh[1],"->>>,>>>,>>9") .
                            WHEN "rec-date"  THEN 
                                cVarValue = IF v-date NE ? THEN STRING(v-date,"99/99/9999") ELSE "" .
                            WHEN "sell-pr"   THEN 
                                cVarValue = STRING(v-price,"->>>>,>>9.99") .
                            WHEN "tot-val"  THEN 
                                cVarValue = STRING(v-ext[1],"->>>,>>>,>>9.99") .

                        END CASE.
                        
                        IF cTmpField = "rec-date"   THEN 
                               cExcelVarValue = IF v-date NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",v-date) ELSE "" .
                
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
                /* DISPLAY cust.cust-no        WHEN v-frst
                         itemfg.i-no
                         itemfg.i-name
                         v-job
                         v-ord[1]
                         v-shp[1]
                         v-qoh[1]
                         v-date
                         v-price
                         v-ext[1]
                     WITH FRAME detail1.
                 DOWN WITH FRAME detail1.
                 */

                END.

            ASSIGN
                v-frst   = NO
                v-ord[2] = v-ord[2] + v-ord[1]
                v-qoh[2] = v-qoh[2] + v-qoh[1]
                v-ext[2] = v-ext[2] + v-ext[1]
                v-ord[1] = 0
                v-shp[1] = 0
                v-qoh[1] = 0
                v-ext[1] = 0
                v-print  = YES.

            DELETE tt-report.
        END.

        IF vzer AND NOT v-bin THEN 
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
                        cVarValue = IF v-frst THEN STRING(cust.cust-no,"x(8)") ELSE "" .
                    WHEN "fgitem"   THEN 
                        cVarValue = STRING(itemfg.i-no,"x(15)").
                    WHEN "cust-part"   THEN 
                        cVarValue = STRING(itemfg.part-no,"x(15)").
                    WHEN "desc"  THEN 
                        cVarValue = STRING(itemfg.i-name,"x(30)") .
                    WHEN "cust-po"   THEN 
                        cVarValue = /*STRING(oe-ordl.po-no,"x(15)")*/ "" .
                    WHEN "job"  THEN 
                        cVarValue = ""  .
                    WHEN "ord-qty"   THEN 
                        cVarValue = /*STRING(v-ord[1],">>>>,>>>,>>9")*/ STRING("           0") .
                    WHEN "ship-qty"  THEN 
                        cVarValue = /*STRING(v-shp[1],">>>>,>>>,>>9")*/ STRING("           0") .
                    WHEN "oh-hand-qty"   THEN 
                        cVarValue = /*STRING(v-qoh[1],">>>>,>>>,>>9")*/ STRING("           0") .
                    WHEN "rec-date"  THEN 
                        cVarValue = /*STRING(v-date,"99/99/9999")*/ "" .
                    WHEN "sell-pr"   THEN 
                        cVarValue = STRING(itemfg.sell-price,"->>>>,>>9.99") .
                    WHEN "tot-val"  THEN 
                        cVarValue = /*STRING(v-ext[1],"->,>>>,>>9.99")*/ STRING("              0") .

                END CASE.

                cExcelVarValue = cVarValue.
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
            /* DISPLAY cust.cust-no          WHEN v-frst
                     itemfg.i-no
                     itemfg.part-no        @ oe-ordl.part-no
                     itemfg.i-name
                     ""                    @ oe-ordl.po-no
                     0                     @ oe-ordl.qty
                     0                     @ li-ship-qty
                     0                     @ v-qoh[1]
                     itemfg.sell-price     @ oe-ordl.price
                     0                     @ v-ext[1]
                 WITH FRAME detail2.
             DOWN WITH FRAME detail2.
   
            */


            ASSIGN
                v-frst  = NO
                v-print = YES.
        END.

        IF LAST-OF(tt-cust-no) THEN 
        DO:
            IF v-print                 AND
                (v-qoh[2] NE 0 OR vzer) THEN 
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
                            cVarValue =   "" .
                        WHEN "fgitem"   THEN 
                            cVarValue = "".
                        WHEN "cust-part"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue = "" .
                        WHEN "cust-po"   THEN 
                            cVarValue =   "" .
                        WHEN "job"  THEN 
                            cVarValue = ""  .
                        WHEN "ord-qty"   THEN 
                            cVarValue = STRING(v-ord[2],"->>>,>>>,>>9") .
                        WHEN "ship-qty"  THEN 
                            cVarValue = STRING(v-shp[2],"->>>,>>>,>>9") .
                        WHEN "oh-hand-qty"   THEN 
                            cVarValue = STRING(v-qoh[2],"->>>,>>>,>>9") .
                        WHEN "rec-date"  THEN 
                            cVarValue =  "" .
                        WHEN "sell-pr"   THEN 
                            cVarValue = "" .
                        WHEN "tot-val"  THEN 
                            cVarValue = STRING(v-ext[2],"->>>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                PUT str-line SKIP .
                PUT UNFORMATTED 
                    "            Customer Total" SUBSTRING(cDisplay,27,300) SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        'Customer Total ,' SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.


                PUT SKIP(1).
            END.

            ASSIGN
                v-ord[3] = v-ord[3] + v-ord[2]
                v-shp[3] = v-shp[3] + v-shp[2]
                v-qoh[3] = v-qoh[3] + v-qoh[2]
                v-ext[3] = v-ext[3] + v-ext[2]

                v-ord[2] = 0
                v-shp[2] = 0
                v-qoh[2] = 0
                v-ext[2] = 0.
        END.

        IF LAST-OF(tt-sman) THEN 
        DO:
            IF v-print                 AND
                (v-qoh[3] NE 0 OR vzer) THEN 
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
                            cVarValue =   "" .
                        WHEN "fgitem"   THEN 
                            cVarValue = "".
                        WHEN "cust-part"   THEN 
                            cVarValue = "".
                        WHEN "desc"  THEN 
                            cVarValue = "" .
                        WHEN "cust-po"   THEN 
                            cVarValue =   "" .
                        WHEN "job"  THEN 
                            cVarValue = ""  .
                        WHEN "ord-qty"   THEN 
                            cVarValue = STRING(v-ord[3],"->>>,>>>,>>9") .
                        WHEN "ship-qty"  THEN 
                            cVarValue = STRING(v-shp[3],"->>>,>>>,>>9") .
                        WHEN "oh-hand-qty"   THEN 
                            cVarValue = STRING(v-qoh[3],"->>>,>>>,>>9") .
                        WHEN "rec-date"  THEN 
                            cVarValue =  "" .
                        WHEN "sell-pr"   THEN 
                            cVarValue = "" .
                        WHEN "tot-val"  THEN 
                            cVarValue = STRING(v-ext[3],"->>>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.
                PUT str-line SKIP .
                PUT UNFORMATTED 
                    "            SalesRep Total" SUBSTRING(cDisplay,27,300) SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM excel UNFORMATTED  
                        'SalesRep Total ,' SUBSTRING(cExcelDisplay,4,300) SKIP.
                END.

                PUT SKIP(1).
            END.

            ASSIGN
                v-ord[4] = v-ord[4] + v-ord[3]
                v-shp[4] = v-shp[4] + v-shp[3]
                v-qoh[4] = v-qoh[4] + v-qoh[3]
                v-ext[4] = v-ext[4] + v-ext[3]

                v-ord[3] = 0
                v-shp[3] = 0
                v-qoh[3] = 0
                v-ext[3] = 0.
        END.

        IF LAST(tt-sman)         AND
            (v-qoh[4] NE 0 OR vzer) THEN 
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
                        cVarValue =   "" .
                    WHEN "fgitem"   THEN 
                        cVarValue = "".
                    WHEN "cust-part"   THEN 
                        cVarValue = "".
                    WHEN "desc"  THEN 
                        cVarValue = "" .
                    WHEN "cust-po"   THEN 
                        cVarValue =   "" .
                    WHEN "job"  THEN 
                        cVarValue = ""  .
                    WHEN "ord-qty"   THEN 
                        cVarValue = STRING(v-ord[4],"->>>,>>>,>>9") .
                    WHEN "ship-qty"  THEN 
                        cVarValue = STRING(v-shp[4],"->>>,>>>,>>9") .
                    WHEN "oh-hand-qty"   THEN 
                        cVarValue = STRING(v-qoh[4],"->>>,>>>,>>9") .
                    WHEN "rec-date"  THEN 
                        cVarValue =  "" .
                    WHEN "sell-pr"   THEN 
                        cVarValue = "" .
                    WHEN "tot-val"  THEN 
                        cVarValue = STRING(v-ext[4],"->>>,>>>,>>9.99") .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT str-line SKIP .
            PUT UNFORMATTED 
                "               Grand Total" SUBSTRING(cDisplay,27,300) SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM excel UNFORMATTED  
                    'Grand Total ,' SUBSTRING(cExcelDisplay,4,300) SKIP.
            END.

            /*IF tb_detail THEN
              PUT "--------------" TO 164 SKIP
                  "   Grand Total" AT 126 v-ext[4] TO 164 FORMAT "->>,>>>,>>9.99".
    
            ELSE
              PUT "--------------" TO 132 SKIP
                  "   Grand Total" AT 94  v-ext[4] TO 132 FORMAT "->>,>>>,>>9.99".*/

            PUT SKIP(1).
        END.
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2002 Advanced Software, Inc. */

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
                .
        ELSE
            ASSIGN
                tb_OpenCSV:SCREEN-VALUE = "NO"
                fi_file:SENSITIVE       = NO
                tb_OpenCSV:SENSITIVE    = NO      
                .
        ASSIGN 
            fi_file:SCREEN-VALUE = "c:\tmp\SalesValueByCustomer.csv".    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

