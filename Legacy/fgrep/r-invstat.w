&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/27/2018

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
DEFINE VARIABLE list-name   AS cha       NO-UNDO.
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

/*{sys/inc/custlistform.i ""IR10"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL NO-UNDO.

DEFINE VARIABLE is-xprint-form   AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file      AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE iGrandTot          AS INTEGER NO-UNDO .
DEFINE VARIABLE dGrandTot          AS DECIMAL NO-UNDO .
DEFINE VARIABLE iSubTot            AS INTEGER NO-UNDO .
DEFINE VARIABLE dSubTot            AS DECIMAL NO-UNDO .

DEFINE TEMP-TABLE tt-order NO-UNDO 
    FIELD iOrder        AS INTEGER
    FIELD iLine         AS INTEGER
    FIELD dDate         AS DATE 
    FIELD cItem         AS CHARACTER
    FIELD cstyle        AS CHARACTER
    FIELD cstyledscr    AS CHARACTER
    FIELD iBackLogQty   AS INTEGER
    FIELD dBackLogValue AS DECIMAL
    FIELD iOnHandQty    AS INTEGER 
    FIELD dOnHandValue  AS DECIMAL .



ASSIGN 
    cTextListToSelect  = "Sales Ord,Ord Date,Com Date,Item Description," + 
                            "PR CD,Order Qty,Made Qty,Shipped Qty,Bal/Run Qty,On-Hand Qty," +
                            "Price Per,Total Value"
    cFieldListToSelect = "sales-ord,ord-date,com-date,item-dscr," + 
                            "pr-cd,ord-qty,made-qty,ship-qty,bal-qty,onhnd-qty," + 
                            "prc-per,tot-val"
    cFieldLength       = "9,10,10,30," + "5,12,12,12,12,12," + "17,14"
    cFieldType         = "i,c,c,c," + "i,i,i,i,i,i," + "c,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Sales Ord,Ord Date,Com Date,Item Description,PR CD,Order Qty,Made Qty,Shipped Qty,Bal/Run Qty,On-Hand Qty,Price Per,Total Value" .

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
btnCustList begin_cust end_cust begin_i-no end_i-no sl_avail Btn_Def ~
sl_selected Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS as-of-date tb_cust-list begin_cust ~
end_cust begin_i-no end_i-no sl_avail sl_selected rd-dest lv-ornt ~
lines-per-page lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel ~
fi_file 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD producedQty C-Win 
FUNCTION producedQty RETURNS INTEGER
    ( INPUT ipItem AS CHARACTER,INPUT ipJob AS CHARACTER,INPUT ipJob2 AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel AUTO-END-KEY 
    LABEL "&Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
    LABEL "&OK" 
    SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
    LABEL "Preview" 
    SIZE 9.8 BY .81.

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

DEFINE VARIABLE as-of-date     AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/01 
    LABEL "As of" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_cust     AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust       AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Customer#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-invstat.csv" 
    LABEL "If Yes, File Name" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    FGCOLOR 9 .

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
    "To File", 3,
    "To Fax", 4,
    "To Email", 5,
    "To Port Directly", 6
    SIZE 21 BY 6.67 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 8.57.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 94 BY 6.52.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL   INITIAL NO 
    LABEL "Use Defined Customer List" 
    VIEW-AS TOGGLE-BOX
    SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .95
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_runExcel  AS LOGICAL   INITIAL NO 
    LABEL "Auto Run Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL YES 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    as-of-date AT ROW 2.05 COL 28.6 COLON-ALIGNED WIDGET-ID 60
    tb_cust-list AT ROW 3.24 COL 30.2 WIDGET-ID 6
    btnCustList AT ROW 3.29 COL 62.2 WIDGET-ID 8
    begin_cust AT ROW 4.38 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Customer Number"
    end_cust AT ROW 4.38 COL 70 COLON-ALIGNED HELP
    "Enter Ending Customer Number"
    begin_i-no AT ROW 5.33 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_i-no AT ROW 5.33 COL 70 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    sl_avail AT ROW 7.38 COL 2.8 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 7.52 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 7.52 COL 60.8 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 8.52 COL 40.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 9.57 COL 40.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 10.62 COL 40.2 WIDGET-ID 40
    btn_down AT ROW 11.67 COL 40.2 WIDGET-ID 42
    rd-dest AT ROW 14.33 COL 5 NO-LABELS
    lv-ornt AT ROW 14.33 COL 30 NO-LABELS
    lines-per-page AT ROW 14.33 COL 83 COLON-ALIGNED
    lv-font-no AT ROW 16.48 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 17.43 COL 28 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 18.86 COL 30
    tb_excel AT ROW 19.57 COL 71 RIGHT-ALIGNED
    tb_runExcel AT ROW 19.57 COL 93 RIGHT-ALIGNED
    fi_file AT ROW 20.52 COL 49 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 21.95 COL 19
    btn-cancel AT ROW 21.95 COL 57
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 6.67 COL 3.4 WIDGET-ID 38
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 13.38 COL 2
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 2 
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 6.67 COL 60.4 WIDGET-ID 44
    RECT-6 AT ROW 13.14 COL 1
    RECT-7 AT ROW 6.52 COL 1 WIDGET-ID 58
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 95.2 BY 22.81.


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
        TITLE              = "Inventory Status Report By Customer"
        HEIGHT             = 23.14
        WIDTH              = 97
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
    begin_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cust:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Status Report By Customer */
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
ON WINDOW-CLOSE OF C-Win /* Finished Goods Status Report By Customer */
    DO:
        /* This event will close the window and terminate the procedure.  */
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


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
        SESSION:SET-WAIT-STATE("general").
        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
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


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Ending Customer# */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.

        RUN WINDOWS/l-cust.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
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
    RUN enable_UI.

    {methods/nowait.i}

    RUN sys/inc/CustListForm.p ( "IR10",cocode, 
        OUTPUT ou-log,
        OUTPUT ou-cust-int) .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_cust.
    END.

    RUN sys/ref/CustList.p (INPUT cocode,
        INPUT 'IR10',
        INPUT NO,
        OUTPUT glCustListActive).
    {sys/inc/chblankcust.i ""IR10""}

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
            INPUT 'IR10',
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
        INPUT 'IR10').


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
    DEFINE VARIABLE cTmpList      AS cha     NO-UNDO.

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

    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        IF LOOKUP(ENTRY(iCount,cTmpList), cTextListToSelect) = 0 THEN
            ldummy = sl_selected:DELETE(ENTRY(iCount,cTmpList)).
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
    DISPLAY as-of-date tb_cust-list begin_cust end_cust begin_i-no end_i-no 
        sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
        lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 as-of-date tb_cust-list btnCustList begin_cust end_cust 
        begin_i-no end_i-no sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
        btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
        tb_excel tb_runExcel fi_file btn-ok btn-cancel 
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
    */                                  /* use-dialog(1) and landscape(2) */

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
    /* ----------------------------------------------- fg/rep/fg-strep.p 4/96 fwk */
    /* FINISHED GOODS STATUS REPORT  By Customer                                             */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-ino             LIKE itemfg.i-no EXTENT 2 INITIAL [" ", "ZZZZZZZZZZZZZZZ"].
    DEFINE VARIABLE v-cust            AS CHARACTER FORMAT "x(8)" EXTENT 2 INITIAL [" ", "ZZZZZZZZ"].
    DEFINE VARIABLE v-custown         AS LOGICAL   FORMAT "Y/N" INITIAL "N".
    DEFINE VARIABLE sort-opt          AS CHARACTER NO-UNDO INITIAL "C" FORMAT "!".
    DEFINE VARIABLE pcat              AS LOGICAL   INITIAL NO.
    DEFINE VARIABLE v-first           AS LOGICAL   INITIAL NO NO-UNDO.
    DEFINE VARIABLE v-dscr            AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-qty             AS INTEGER   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-inv-qty         LIKE oe-ordl.inv-qty FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-ship-qty        LIKE oe-ordl.ship-qty FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-q-onh           AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-job-no          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE v-frst-i-no       AS LOGICAL   INITIAL NO NO-UNDO.

    DEFINE VARIABLE iMadeQty          LIKE fg-bin.qty NO-UNDO.
    DEFINE VARIABLE iBalQty           LIKE fg-act.qty NO-UNDO.
    DEFINE VARIABLE cPricePer         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dvalue            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBackLogvalue     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOnHandvalue      AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE iTotOrdQty        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotMadeQty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotShipQty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotBalQty        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotOnHQty        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dTotValQty        AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE iGrTotOrdQty      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iGrTotMadeQty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iGrTotShipQty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iGrTotBalQty      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iGrTotOnHQty      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dGrTotValQty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCurrentYearMonth AS INTEGER   NO-UNDO .
    DEFINE VARIABLE iLastYearMonth    AS INTEGER   NO-UNDO .
    DEFINE VARIABLE cStyle            AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cDisplay          AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay     AS cha       NO-UNDO.
    DEFINE VARIABLE hField            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField         AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue         AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue    AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList     AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName        AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4          AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5          AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line          AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE lSelected         AS LOGICAL   INIT YES NO-UNDO.
    DEFINE VARIABLE iLineCount        AS INTEGER   NO-UNDO .
    DEFINE VARIABLE dtPreMonthDate    AS DATE      NO-UNDO .
    DEFINE VARIABLE dtPreYearDate     AS DATE      NO-UNDO .
    {sys/form/r-topsw.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-period FOR period .

    FORM HEADER
        "Order# Item#           Description                 Order Qty Shipped Qty  OnHand Qty Date    P.O. Number      Job Number"                 
        WITH FRAME f-top STREAM-IO WIDTH 132 NO-BOX PAGE-TOP.

    FORM
        cust.name LABEL "Customer Name" 
        WITH SIDE-LABELS DOWN STREAM-IO WIDTH 132 FRAME custname.

    FORM
        oe-ord.ord-no 
        oe-ordl.i-no 
        v-dscr 
        v-qty
        v-ship-qty
        v-q-onh
        oe-ord.ord-date 
        oe-ord.po-no 
        oe-ord.job-no 
        oe-ord.job-no2 

        WITH NO-LABELS DOWN STREAM-IO WIDTH 132 FRAME itemx.
    EMPTY TEMP-TABLE tt-order .

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

        IF LOOKUP(ttRptSelected.TextList, "Order Qty,Made Qty,Shipped Qty,Bal/Run Qty,On-Hand Qty,Total Value") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .  
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(fi_file).
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-cust[1] = begin_cust
        v-cust[2] = end_cust
        v-ino[1]  = begin_i-no
        v-ino[2]  = end_i-no.
    lSelected   = tb_cust-list .
    iCurrentYearMonth = INTEGER(SUBSTRING(STRING(as-of-date),1,2)) .

 
    {sys/inc/print1.i}


    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    IF lselected THEN 
    DO:
        FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
        IF AVAILABLE ttCustList THEN ASSIGN v-cust[1] = ttCustList.cust-no .
        FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
        IF AVAILABLE ttCustList THEN ASSIGN v-cust[2] = ttCustList.cust-no .
    END.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    FOR EACH oe-ord
        WHERE oe-ord.company  EQ cocode
        AND oe-ord.cust-no  GE v-cust[1]
        AND oe-ord.cust-no  LE v-cust[2]
        AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ oe-ord.cust-no
        AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
        AND oe-ord.ord-date LE as-of-date
        AND oe-ord.opened EQ YES
        USE-INDEX ord-no NO-LOCK,

        FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ cocode
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
        AND b-oe-ordl.i-no    GE v-ino[1]
        AND b-oe-ordl.i-no    LE v-ino[2]
        AND b-oe-ordl.i-no    NE ""
        NO-LOCK,

        FIRST cust
        {sys/ref/custW.i}
      and cust.cust-no eq oe-ord.cust-no
    no-lock

    break by cust.cust-no:


    {custom/statusMsg.i "'Processing Customer # ' + cust.cust-no"}
        
    IF FIRST-OF(cust.cust-no) THEN
        ASSIGN
            v-first     = YES
            iTotOrdQty  = 0
            iTotMadeQty = 0
            iTotShipQty = 0
            iTotBalQty  = 0
            iTotOnHQty  = 0 
            dTotValQty  = 0.
    ASSIGN
        cPricePer  = ""
        dvalue     = 0
        v-qty      = 0
        v-q-onh    = 0
        v-ship-qty = 0.
        
    FOR EACH oe-ordl OF oe-ord WHERE 
        oe-ordl.i-no GE v-ino[1] AND 
        oe-ordl.i-no LE v-ino[2]
        NO-LOCK USE-INDEX ord-no:
                
        FIND FIRST itemfg WHERE itemfg.company = cocode AND
            itemfg.i-no = oe-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
                    
            ASSIGN 
                v-dscr    = itemfg.i-name
                v-qty     = oe-ordl.qty
                v-q-onh   = itemfg.q-onh
                cPricePer = STRING(oe-ordl.price,"->>,>>>,>>9.99") + string(oe-ordl.pr-uom,"x(3)").
                    
            RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT v-inv-qty, OUTPUT v-ship-qty).
                    
                    
            v-job-no = TRIM(STRING(oe-ord.job-no) + "-" + STRING(oe-ord.job-no2)) .
            v-job-no = IF v-job-no EQ "-0" OR v-job-no EQ "-00" THEN "" ELSE TRIM(v-job-no)  .
                   
            cStyle = itemfg.style .
            FIND FIRST eb NO-LOCK 
                WHERE eb.company EQ cocode 
                AND eb.est-no EQ oe-ordl.est-no 
                AND eb.stock-no EQ oe-ordl.i-no NO-ERROR .
            IF AVAILABLE eb THEN
                cStyle = eb.style .

            iMadeQty = producedQty(oe-ordl.i-no,oe-ordl.job-no,oe-ordl.job-no2) .

            iBalQty = v-qty - iMadeQty .
            v-q-onh = iMadeQty - v-inv-qty .

            RUN sys/ref/convcuom.p (oe-ordl.pr-uom, "EA", 0, 0, 0, 0,
                oe-ordl.price, OUTPUT dvalue).

            dOnHandvalue = dvalue * v-q-onh .

            dBackLogvalue = dvalue * v-qty .

            IF FIRST(cust.cust-no) THEN
                PUT str-tit4 SKIP
                    str-tit5 SKIP .

            IF iLineCount > (lines-per-page - 10) THEN 
            DO:
                PAGE.
                PUT str-tit4 SKIP
                    str-tit5 SKIP .
                iLineCount = 0 .
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
                    WHEN "sales-ord" THEN cVarValue = STRING(oe-ord.ord-no,">>>>>>>>9") .
                    WHEN "ord-date"  THEN cVarValue = IF oe-ord.ord-date NE ? THEN STRING(oe-ord.ord-date,"99/99/9999") ELSE "" .
                    WHEN "com-date"  THEN cVarValue = IF oe-ord.due-date NE ? THEN STRING(oe-ord.due-date,"99/99/9999") ELSE "" .
                    WHEN "item-dscr" THEN cVarValue = STRING(v-dscr,"x(30)") .
                    WHEN "pr-cd"     THEN cVarValue = STRING(cStyle,"x(6)") .
                    WHEN "ord-qty"   THEN cVarValue = STRING(v-qty,"->>>,>>>,>>9") .
                    WHEN "made-qty"  THEN cVarValue = STRING(iMadeQty,"->>>,>>>,>>9") .
                    WHEN "ship-qty"  THEN cVarValue = STRING(v-inv-qty,"->>>,>>>,>>9")  .
                    WHEN "bal-qty"   THEN cVarValue = STRING(iBalQty,"->>>,>>>,>>9")  .
                    WHEN "onhnd-qty" THEN cVarValue = STRING(v-q-onh,"->>,>>>,>>9").
                    WHEN "prc-per"   THEN cVarValue = STRING(cPricePer,"X(17)").
                    WHEN "tot-val"   THEN cVarValue = STRING(dOnHandvalue,"->>,>>>,>>9.99")  .
                                 
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
            iLineCount = iLineCount + 1.

                  
            ASSIGN
                iTotOrdQty  = iTotOrdQty + v-qty 
                iTotMadeQty = iTotMadeQty + iMadeQty
                iTotShipQty = iTotShipQty + v-ship-qty
                iTotBalQty  = iTotBalQty  + iBalQty
                iTotOnHQty  = iTotOnHQty  + v-q-onh
                dTotValQty  = dTotValQty  + dOnHandvalue .
                        
                    

            ASSIGN
                iGrTotOrdQty  = iGrTotOrdQty + iTotOrdQty 
                iGrTotMadeQty = iGrTotMadeQty + iTotMadeQty
                iGrTotShipQty = iGrTotShipQty + iTotShipQty
                iGrTotBalQty  = iGrTotBalQty  + iTotBalQty
                iGrTotOnHQty  = iGrTotOnHQty  + iTotOnHQty
                dGrTotValQty  = dGrTotValQty  + dTotValQty.

            CREATE tt-order .
            ASSIGN
                tt-order.iOrder        = oe-ordl.ord-no
                tt-order.iLine         = oe-ordl.LINE
                tt-order.dDate         = oe-ord.ord-date
                tt-order.cItem         = oe-ordl.i-no
                tt-order.cstyle        = cStyle
                tt-order.cstyledscr    = ""
                tt-order.iBackLogQty   = v-qty
                tt-order.dBackLogValue = dBackLogvalue
                tt-order.iOnHandQty    = v-q-onh
                tt-order.dOnHandValue  = dOnHandvalue .

        END.

    END.  /* for each oe-ordl  */


    IF LAST-OF(cust.cust-no)  THEN 
    DO:
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
        PUT SKIP str-line SKIP .
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:
                WHEN "sales-ord" THEN cVarValue = "" .
                WHEN "ord-date"  THEN cVarValue = "" .
                WHEN "com-date"  THEN cVarValue =  "" .
                WHEN "item-dscr" THEN cVarValue =  "".
                WHEN "pr-cd"     THEN cVarValue = "".
                WHEN "ord-qty"   THEN cVarValue = STRING(iTotOrdQty,"->>>,>>>,>>9") .
                WHEN "made-qty"  THEN cVarValue = STRING(iTotMadeQty,"->>>,>>>,>>9") .
                WHEN "ship-qty"  THEN cVarValue = STRING(iTotShipQty,"->>>,>>>,>>9")  .
                WHEN "bal-qty"   THEN cVarValue = STRING(iTotBalQty,"->>>,>>>,>>9")  .
                WHEN "onhnd-qty" THEN cVarValue = STRING(iTotOnHQty,"->>,>>>,>>9").
                WHEN "prc-per"   THEN cVarValue = "".
                WHEN "tot-val"   THEN cVarValue = STRING(dTotValQty,"->>,>>>,>>9.99")  .
            END.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        PUT UNFORMATTED 
            "   SUB TOTALS:" SUBSTRING(cDisplay,15,300) SKIP(1).
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED
                " SUB TOTALS: " + substring(cExcelDisplay,3,300) SKIP.
        END.
    END.

END. /* for each cust  */

ASSIGN 
    cDisplay       = ""
    cTmpField      = ""
    cVarValue      = ""
    cExcelDisplay  = ""
    cExcelVarValue = "".
PUT SKIP str-line SKIP .
DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
    CASE cTmpField:
        WHEN "sales-ord" THEN cVarValue = "" .
        WHEN "ord-date"  THEN cVarValue = "" .
        WHEN "com-date"  THEN cVarValue =  "" .
        WHEN "item-dscr" THEN cVarValue = "" .
        WHEN "pr-cd"     THEN cVarValue = "".
        WHEN "ord-qty"   THEN cVarValue = STRING(iGrTotOrdQty,"->>>,>>>,>>9") .
        WHEN "made-qty"  THEN cVarValue = STRING(iGrTotMadeQty,"->>>,>>>,>>9") .
        WHEN "ship-qty"  THEN cVarValue = STRING(iGrTotShipQty,"->>>,>>>,>>9")  .
        WHEN "bal-qty"   THEN cVarValue = STRING(iGrTotBalQty,"->>>,>>>,>>9")  .
        WHEN "onhnd-qty" THEN cVarValue = STRING(iGrTotOnHQty,"->>,>>>,>>9").
        WHEN "prc-per"   THEN cVarValue = "".
        WHEN "tot-val"   THEN cVarValue = STRING(dGrTotValQty,"->>,>>>,>>9.99")  .
    END.
    cExcelVarValue = cVarValue.
    cDisplay = cDisplay + cVarValue +
        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
END.
PUT UNFORMATTED 
    "   GRAND TOTALS:" SUBSTRING(cDisplay,17,300) SKIP(1).
IF tb_excel THEN 
DO:
    PUT STREAM excel UNFORMATTED
        " GRAND TOTALS: " + substring(cExcelDisplay,3,300) SKIP.
END.

FIND FIRST period                   
    WHERE period.company EQ cocode
    AND period.pst     LE as-of-date
    AND period.pend    GE as-of-date
    NO-LOCK NO-ERROR.

IF AVAILABLE period THEN 
DO:
    RUN pPrintData ( YES,"****** RECAP BY STYLE NUMBER FOR " +  STRING(period.pnum) + "/" + STRING(period.yr) + " ******",period.pst,period.pend) .
END.

dtPreYearDate = DATE(MONTH(as-of-date),DAY(as-of-date),YEAR(as-of-date) - 1).
       
IF MONTH(as-of-date) EQ 1 THEN
    dtPreMonthDate = DATE(12,DAY(as-of-date),YEAR(as-of-date) - 1).
ELSE
    dtPreMonthDate = DATE(MONTH(as-of-date) - 1,DAY(as-of-date),YEAR(as-of-date)).

FIND FIRST bf-period NO-LOCK
    WHERE bf-period.company EQ cocode
    AND bf-period.pst     LE dtPreMonthDate
    AND bf-period.pend    GE dtPreMonthDate
    NO-ERROR.

IF AVAILABLE bf-period THEN 
DO:
    RUN pPrintData ( YES,"****** RECAP BY STYLE NUMBER FOR " +  STRING(bf-period.pnum) + "/" + STRING(bf-period.yr) + " ******",bf-period.pst,bf-period.pend) .
END.

FIND FIRST bf-period NO-LOCK
    WHERE bf-period.company EQ cocode
    AND bf-period.pst     LE dtPreYearDate
    AND bf-period.pend    GE dtPreYearDate
    NO-ERROR.

IF AVAILABLE bf-period THEN 
DO:
    RUN pPrintData ( YES,"****** RECAP BY STYLE NUMBER FOR " +  STRING(bf-period.pnum) + "/" + STRING(bf-period.yr) + " ******",bf-period.pst,bf-period.pend) .
END.



IF tb_excel THEN 
DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrintData C-Win 
PROCEDURE pPrintData :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplChecked AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER cPrintLabel AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER dBegData AS DATE NO-UNDO .
    DEFINE INPUT PARAMETER dEndData AS DATE NO-UNDO .
    DEFINE VARIABLE iLineCount AS INTEGER NO-UNDO .

    PAGE .
    PUT 
        SPACE(15) cPrintLabel FORMAT "x(100)" SKIP
        "                                                     BACK LOG     TOTAL VALUES " SKIP
        "       STYLE#      DESCRIPTION                       BAL TO RUN   DOLLARS       "  SKIP 
        "       ----------- ------------------------------ ------------- --------------" SKIP.
      
    FOR EACH tt-order NO-LOCK
        WHERE tt-order.dDate GE dBegData
        AND  tt-order.dDate LE dEndData
        BREAK BY tt-order.cStyle :

        FIND FIRST style NO-LOCK 
            WHERE style.company EQ cocode 
            AND style.style EQ tt-order.cStyle NO-ERROR .

        tt-order.cstyledscr =  IF AVAILABLE style THEN style.Dscr ELSE "" .

        IF FIRST(tt-order.cStyle) THEN
            ASSIGN iGrandTot = 0
                dGrandTot = 0 
                iSubTot   = 0
                dSubTot   = 0 .

        IF iLineCount > (lines-per-page - 5) THEN 
        DO:
            PAGE .
            PUT 
                SPACE(15) cPrintLabel FORMAT "x(100)" SKIP
                "                                                     BACK LOG     TOTAL VALUES " SKIP
                "       STYLE#      DESCRIPTION                       BAL TO RUN   DOLLARS       "  SKIP 
                "       ----------- ------------------------------ ------------- --------------" SKIP.
            iLineCount = 0 .
        END.

        IF FIRST-OF(tt-order.cStyle) THEN
            ASSIGN
                iSubTot = 0
                dSubTot = 0 .

        iGrandTot = iGrandTot + tt-order.iBackLogQty .
        dGrandTot = dGrandTot + tt-order.dBackLogValue .
        iSubTot = iSubTot + tt-order.iBackLogQty .
        dSubTot = dSubTot + tt-order.dBackLogValue .
           
        IF LAST-OF(tt-order.cStyle) THEN 
        DO:
            PUT SPACE(7) tt-order.cStyle FORMAT "x(12)"
                tt-order.cstyledscr  FORMAT "x(30)"
                iSubTot FORMAT "->,>>>,>>>,>>9"
                dSubTot FORMAT "->>>,>>>,>>9.99" SKIP .
            iLineCount = iLineCount + 1 .
        END.
           

        IF LAST (tt-order.cStyle) THEN 
        DO:
            PUT
                "       ----------- ------------------------------ ------------- --------------" SKIP
                SPACE(20) "*** GRAND TOTAL ***" SPACE(10)
                iGrandTot FORMAT "->,>>>,>>>,>>9"  dGrandTot FORMAT "->>>,>>>,>>9.99" SKIP .


        END.

    END.

    PAGE .
    PUT 
        SPACE(15) cPrintLabel FORMAT "x(100)" SKIP
        "                                                        ON HAND   TOTAL VALUES" SKIP
        "       STYLE#      DESCRIPTION                           BAANCE     DOLLARS   "  SKIP 
        "       ----------- ------------------------------ ------------- --------------" SKIP.

      
    FOR EACH tt-order NO-LOCK
        WHERE tt-order.dDate GE dBegData
        AND  tt-order.dDate LE dEndData 
        BREAK BY tt-order.cStyle :

        FIND FIRST style NO-LOCK 
            WHERE style.company EQ cocode 
            AND style.style EQ tt-order.cStyle NO-ERROR .

        tt-order.cstyledscr =  IF AVAILABLE style THEN style.Dscr ELSE "" .

        IF FIRST(tt-order.cStyle) THEN
            ASSIGN iGrandTot = 0
                dGrandTot = 0 
                iSubTot   = 0
                dSubTot   = 0.

        IF iLineCount > (lines-per-page - 5) THEN 
        DO:
            PAGE .
            PUT 
                SPACE(15) cPrintLabel FORMAT "x(100)" SKIP
                "                                                        ON HAND   TOTAL VALUES" SKIP
                "       STYLE#      DESCRIPTION                           BAANCE     DOLLARS   "  SKIP 
                "       ----------- ------------------------------ ------------- --------------" SKIP.
            iLineCount = 0 .
        END.
           
        IF FIRST-OF(tt-order.cStyle) THEN
            ASSIGN
                iSubTot = 0
                dSubTot = 0 .

        iGrandTot = iGrandTot + tt-order.iOnHandQty .
        dGrandTot = dGrandTot + tt-order.dOnHandValue .
        iSubTot = iSubTot + tt-order.iOnHandQty .
        dSubTot = dSubTot + tt-order.dOnHandValue .

        IF LAST-OF(tt-order.cStyle) THEN 
        DO:
            PUT SPACE(7) tt-order.cStyle FORMAT "x(12)"
                tt-order.cstyledscr  FORMAT "x(30)"
                iSubTot FORMAT "->,>>>,>>>,>>9"
                dSubTot FORMAT "->>>,>>>,>>9.99" SKIP .
            iLineCount = iLineCount + 1 .
        END.
          
        IF LAST (tt-order.cStyle) THEN 
        DO:
            PUT
                "       ----------- ------------------------------ ------------- --------------" SKIP
                SPACE(20) "*** GRAND TOTAL ***" SPACE(10)
                iGrandTot FORMAT "->,>>>,>>>,>>9"  dGrandTot FORMAT "->>>,>>>,>>9.99" SKIP .


        END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION producedQty C-Win 
FUNCTION producedQty RETURNS INTEGER
    ( INPUT ipItem AS CHARACTER,INPUT ipJob AS CHARACTER,INPUT ipJob2 AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cJobNo   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cINo     AS CHARACTER NO-UNDO.

    IF ipJob NE "" THEN 
    DO:
        ASSIGN 
            cINo    = ipItem
            cJobNo  = ipJob
            iJobNo2 = ipJob2
            .
        RUN fg/GetProductionQty.p (INPUT cocode,
            INPUT cJobNo,
            INPUT iJobNo2,
            INPUT cINo,
            INPUT NO,
            OUTPUT rtnValue).
    END. /* avail job-hdr */
    /*opBalance = rtnValue.*/
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

