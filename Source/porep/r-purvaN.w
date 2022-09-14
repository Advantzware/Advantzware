&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: porep\r-purvaN.w
  Description: PO Purchased Variance Logical Yes
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Ron Stark
  Created: 07/29/2014
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
DEFINE VARIABLE list-name AS cha       NO-UNDO.
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

DEFINE VARIABLE v-print-fmt    AS CHARACTER.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS cha       NO-UNDO.

DEFINE TEMP-TABLE tt-ei NO-UNDO
    FIELD std-uom AS CHARACTER.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

DEFINE TEMP-TABLE temp-adder NO-UNDO
    FIELD adder       LIKE ITEM.i-no
    FIELD adder-index AS INTEGER
    INDEX temp-adder-index adder-index ASC.

DEFINE BUFFER xjob-mat FOR job-mat.

DEFINE STREAM st-excel.

DEFINE VARIABLE ldummy             AS LOG     NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha     NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha     NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha     NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha     NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha       NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO .
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
ASSIGN 
    cTextListToSelect  = "PO #,Vendor #,Job #,Item #,Due Date,Rec Date,MSF,Vendor $," +
                           "Bought $,Diff $,MPV %,Overs %,Adder code1,Adder code2"
    cFieldListToSelect = "po-no,vend,job,item,due-dt,rcd-dt,msf,vend-$," +
                                "bught,diff,mpv,ovr,addr,addr2"
    cFieldLength       = "6,8,13,15,10,10,10,14," + "13,14,11,11,15,15"
    cFieldType         = "i,c,c,c,c,c,i,i," + "i,i,i,i,c,c"
    .

{sys/inc/ttRptSel.i}

ASSIGN 
    cTextListToDefault = "PO #,Vendor #,Job #,Item #,Due Date,Rec Date,MSF,Vendor $," +
                           "Bought $,Diff $,MPV %,Overs %,Adder code1,Adder code2" .


{sys/inc/oereordr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 end_po-no begin_po-no ~
begin_po-date end_po-date begin_vend-no end_vend-no begin_po-i-no ~
end_po-i-no begin_job-no end_job-no tb_receipt begin_rec-date end_rec-date ~
rd_vend-cost tb_mpv tb_repeat tb_overs tb_adder sl_avail sl_selected ~
Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file ~
tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS end_po-no begin_po-no begin_po-date ~
end_po-date begin_vend-no end_vend-no begin_po-i-no end_po-i-no ~
begin_job-no end_job-no tb_receipt begin_rec-date end_rec-date rd_vend-cost ~
tb_mpv tb_repeat tb_overs tb_adder sl_avail sl_selected rd-dest ~
td-show-parm fi_file tb_OpenCSV tbAutoClose lbl_vend-cost 

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

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(9)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-date  AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-i-no  AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 
    LABEL "Beginning PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_rec-date AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Rec Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no  AS CHARACTER FORMAT "X(8)":U 
    LABEL "Beginning Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(9)":U INITIAL "zzzzzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-date    AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending PO Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-i-no    AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no      AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 999999 
    LABEL "Ending PO#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rec-date   AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Rec Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
    LABEL "Ending Vendor#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-purvar.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE
    SIZE 43 BY 1
    FGCOLOR 0 .

DEFINE VARIABLE lbl_vend-cost  AS CHARACTER FORMAT "X(256)":U INITIAL "Show Vendor $ from..." 
    VIEW-AS TEXT 
    SIZE 23 BY .62 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS TEXT 
    SIZE 62 BY .62 NO-UNDO.

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

DEFINE VARIABLE rd_vend-cost   AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Vendor Matrix", "Vendor Matrix",
    "Invoiced Amt", "Invoiced Amt"
    SIZE 38 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.24.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 11.43.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33.8 BY 5.43 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33.8 BY 5.43 NO-UNDO.

DEFINE VARIABLE tbAutoClose  AS LOGICAL   INITIAL NO 
    LABEL "Auto Close" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_adder     AS LOGICAL   INITIAL NO 
    LABEL "Adder Codes" 
    VIEW-AS TOGGLE-BOX
    SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_mpv       AS LOGICAL   INITIAL NO 
    LABEL "MPV %" 
    VIEW-AS TOGGLE-BOX
    SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE tb_overs     AS LOGICAL   INITIAL NO 
    LABEL "Overs %" 
    VIEW-AS TOGGLE-BOX
    SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE tb_receipt   AS LOGICAL   INITIAL NO 
    LABEL "Only Purchase Orders With Receipts?" 
    VIEW-AS TOGGLE-BOX
    SIZE 42 BY .81 NO-UNDO.

DEFINE VARIABLE tb_repeat    AS LOGICAL   INITIAL NO 
    LABEL "Repeat PO#/Vendor#?" 
    VIEW-AS TOGGLE-BOX
    SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.6 BY .81
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    end_po-no AT ROW 2.19 COL 69 COLON-ALIGNED HELP
    "Enter Ending PO Number"
    begin_po-no AT ROW 2.24 COL 26 COLON-ALIGNED HELP
    "Enter Beginning PO Number"
    begin_po-date AT ROW 3.29 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Due Date"
    end_po-date AT ROW 3.29 COL 69 COLON-ALIGNED HELP
    "Enter ending Due Date"
    begin_vend-no AT ROW 4.38 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Vendor Number"
    end_vend-no AT ROW 4.38 COL 69 COLON-ALIGNED HELP
    "Enter Ending Vendor number"
    begin_po-i-no AT ROW 5.48 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Order Number"
    end_po-i-no AT ROW 5.48 COL 69 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    begin_job-no AT ROW 6.57 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 6.57 COL 69 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    tb_receipt AT ROW 7.71 COL 28 WIDGET-ID 62
    begin_rec-date AT ROW 8.57 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Rec Date" WIDGET-ID 58
    end_rec-date AT ROW 8.57 COL 69 COLON-ALIGNED HELP
    "Enter ending Rec Date" WIDGET-ID 60
    rd_vend-cost AT ROW 9.76 COL 28 NO-LABELS
    tb_mpv AT ROW 10.81 COL 56
    tb_repeat AT ROW 10.86 COL 28.4
    tb_overs AT ROW 11.76 COL 28.4
    tb_adder AT ROW 11.76 COL 56
    sl_avail AT ROW 14 COL 4.2 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 14 COL 60.4 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 14.19 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 15.19 COL 41 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 16.19 COL 41 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 17.24 COL 41 WIDGET-ID 40
    btn_down AT ROW 18.29 COL 41 WIDGET-ID 42
    rd-dest AT ROW 20.76 COL 6 NO-LABELS
    lv-font-no AT ROW 21.19 COL 35 COLON-ALIGNED
    lv-ornt AT ROW 21.24 COL 31 NO-LABELS
    lines-per-page AT ROW 21.24 COL 84 COLON-ALIGNED
    tb_excel AT ROW 21.43 COL 53 RIGHT-ALIGNED
    td-show-parm AT ROW 22.81 COL 39.6
    fi_file AT ROW 23.57 COL 28 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 23.71 COL 87.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 26.05 COL 32.2 WIDGET-ID 16
    btn-ok AT ROW 26.86 COL 31.4
    btn-cancel AT ROW 26.86 COL 50.4
    lbl_vend-cost AT ROW 9.86 COL 3 COLON-ALIGNED NO-LABELS
    lv-font-name AT ROW 21.43 COL 29 COLON-ALIGNED NO-LABELS
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 13.33 COL 11.4 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 13.38 COL 60.6 WIDGET-ID 44
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.38 COL 5.4
    BGCOLOR 15 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 19.91 COL 5.4
    RECT-6 AT ROW 20.24 COL 4.2
    RECT-7 AT ROW 1.71 COL 4.2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 95.8 BY 29.1
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
        TITLE              = "PO Purchased Variance"
        HEIGHT             = 27.48
        WIDTH              = 95.6
        MAX-HEIGHT         = 29.1
        MAX-WIDTH          = 95.8
        VIRTUAL-HEIGHT     = 29.1
        VIRTUAL-WIDTH      = 95.8
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
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rec-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_po-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_rec-date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_vend-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_vend-cost IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_vend-cost:PRIVATE-DATA IN FRAME FRAME-A = "rd_vend-cost".

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
    rd_vend-cost:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PO Purchased Variance */
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
ON WINDOW-CLOSE OF C-Win /* PO Purchased Variance */
    DO:
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date C-Win
ON LEAVE OF begin_po-date IN FRAME FRAME-A /* Beginning PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no C-Win
ON LEAVE OF begin_po-no IN FRAME FRAME-A /* Beginning PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rec-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rec-date C-Win
ON LEAVE OF begin_rec-date IN FRAME FRAME-A /* Beginning Rec Date */
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
        /* This event will close the window and terminate the procedure.  */
        DELETE PROCEDURE hdOutputProcs.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
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
                    {custom/asifax.i &begin_cust=begin_po-no
                            &END_cust=END_po-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-date C-Win
ON LEAVE OF end_po-date IN FRAME FRAME-A /* Ending PO Date */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-i-no C-Win
ON LEAVE OF end_po-i-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_po-no C-Win
ON LEAVE OF end_po-no IN FRAME FRAME-A /* Ending PO# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rec-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rec-date C-Win
ON LEAVE OF end_rec-date IN FRAME FRAME-A /* Ending Rec Date */
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


&Scoped-define SELF-NAME tb_adder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_adder C-Win
ON VALUE-CHANGED OF tb_adder IN FRAME FRAME-A /* Adder Codes */
    DO:
        ASSIGN {&self-name}
            tb_mpv
            tb_overs.

        IF tb_adder THEN
            ASSIGN lv-ornt:SCREEN-VALUE = "L"
                lv-ornt:SENSITIVE    = NO.
        ELSE
            IF NOT tb_overs AND
                NOT tb_mpv THEN
                ASSIGN
                    lv-ornt:SENSITIVE    = YES
                    lv-ornt:SCREEN-VALUE = "P".
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


&Scoped-define SELF-NAME tb_mpv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_mpv C-Win
ON VALUE-CHANGED OF tb_mpv IN FRAME FRAME-A /* MPV % */
    DO:
        ASSIGN {&self-name}
            tb_overs
            tb_adder.

        IF tb_mpv THEN
            ASSIGN lv-ornt:SCREEN-VALUE = "L"
                lv-ornt:SENSITIVE    = NO.
        ELSE
            IF NOT tb_overs AND
                NOT tb_adder THEN
                ASSIGN
                    lv-ornt:SENSITIVE    = YES
                    lv-ornt:SCREEN-VALUE = "P".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_overs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_overs C-Win
ON VALUE-CHANGED OF tb_overs IN FRAME FRAME-A /* Overs % */
    DO:
        ASSIGN {&self-name}
            tb_mpv
            tb_adder.

        IF tb_overs THEN
            ASSIGN lv-ornt:SCREEN-VALUE = "L"
                lv-ornt:SENSITIVE    = NO.
        ELSE
            IF NOT tb_mpv AND
                NOT tb_adder THEN
                ASSIGN
                    lv-ornt:SENSITIVE    = YES
                    lv-ornt:SCREEN-VALUE = "P".
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_receipt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_receipt C-Win
ON VALUE-CHANGED OF tb_receipt IN FRAME FRAME-A /* Only Purchase Orders With Receipts? */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_repeat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_repeat C-Win
ON VALUE-CHANGED OF tb_repeat IN FRAME FRAME-A /* Repeat PO#/Vendor#? */
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
        begin_po-date = DATE(1,1,YEAR(TODAY))
        end_po-date   = TODAY.

    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_po-no.

        IF tb_mpv:SCREEN-VALUE  = "YES" OR
            tb_overs:SCREEN-VALUE = "YES" OR
            tb_adder:SCREEN-VALUE = "YES" THEN
            ASSIGN
                lv-ornt:SENSITIVE    = NO
                lv-ornt:SCREEN-VALUE = "L".
    END.

    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "PR5" }
    ASSIGN
        td-show-parm:sensitive IN FRAME FRAME-A = lShowParameters
        td-show-parm:hidden IN FRAME FRAME-A    = NOT lShowParameters
        td-show-parm:visible IN FRAME FRAME-A   = lShowParameters
        .
    RUN pChangeDest.  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adder-proc C-Win 
PROCEDURE adder-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE viIndex AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE temp-adder.

    FIND FIRST job WHERE
        job.company EQ po-ordl.company AND
        job.job-no  EQ po-ordl.job-no AND
        job.job-no2 EQ po-ordl.job-no2
        NO-LOCK NO-ERROR.

    IF AVAILABLE job THEN
        FIND FIRST xjob-mat WHERE
            xjob-mat.company  EQ job.company AND
            xjob-mat.job      EQ job.job AND
            xjob-mat.job-no   EQ job.job-no AND
            xjob-mat.job-no2  EQ job.job-no2 AND
            xjob-mat.frm      EQ po-ordl.s-num AND
            xjob-mat.blank-no EQ po-ordl.b-num
            USE-INDEX seq-idx
            NO-LOCK NO-ERROR.

    IF AVAILABLE xjob-mat THEN 
        FOR EACH job-mat WHERE
            job-mat.company  EQ xjob-mat.company AND
            job-mat.job      EQ xjob-mat.job AND
            job-mat.frm      EQ xjob-mat.frm AND
            job-mat.job-no   EQ xjob-mat.job-no AND
            job-mat.job-no2  EQ xjob-mat.job-no2
            USE-INDEX seq-idx
            NO-LOCK,
            FIRST ITEM WHERE
            item.company  EQ job-mat.company AND
            item.i-no     EQ job-mat.i-no AND
            item.mat-type EQ "A"
            NO-LOCK:

            CREATE temp-adder.
            ASSIGN 
                temp-adder.adder       = ITEM.i-no
                viIndex                = viIndex + 1
                temp-adder.adder-index = viIndex.
            RELEASE temp-adder.
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
    DISPLAY end_po-no begin_po-no begin_po-date end_po-date begin_vend-no 
        end_vend-no begin_po-i-no end_po-i-no begin_job-no end_job-no 
        tb_receipt begin_rec-date end_rec-date rd_vend-cost tb_mpv tb_repeat 
        tb_overs tb_adder sl_avail sl_selected rd-dest td-show-parm fi_file 
        tb_OpenCSV tbAutoClose lbl_vend-cost 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 end_po-no begin_po-no begin_po-date end_po-date 
        begin_vend-no end_vend-no begin_po-i-no end_po-i-no begin_job-no 
        end_job-no tb_receipt begin_rec-date end_rec-date rd_vend-cost tb_mpv 
        tb_repeat tb_overs tb_adder sl_avail sl_selected Btn_Def Btn_Add 
        Btn_Remove btn_Up btn_down rd-dest td-show-parm fi_file tb_OpenCSV 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildTTVendItemCost C-Win 
PROCEDURE pBuildTTVendItemCost PRIVATE :
    /*------------------------------------------------------------------------------ 
      Purpose:  Populates tt-ei and tt-eiv from vendItemCost and vendItemcostLevel tables  
      Parameters:  <none>
      Notes:    
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemId   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorId AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO INITIAL 1.

    FIND FIRST vendItemCost NO-LOCK  
        WHERE vendItemCost.company  EQ ipcCompany
        AND vendItemCost.itemID   EQ ipcItemID
        AND vendItemcost.itemType EQ ipcItemtype
        AND vendItemCost.vendorID EQ ipcVendorId
        NO-ERROR.
           
    IF AVAILABLE(vendItemCost) THEN 
    DO:
        CREATE tt-ei.
        ASSIGN 
            iIndex        = 1
            tt-ei.std-uom = vendItemCost.vendorUom
            .
            
        CREATE tt-eiv.
        FOR EACH  vendItemCostLevel NO-LOCK
            WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID :
            ASSIGN
                tt-eiv.run-cost[iIndex] = vendItemCostLevel.costPerUOM
                tt-eiv.run-qty[iIndex]  = vendItemCostLevel.quantityBase
                iIndex                  = iIndex + 1
                .                                          
        END.    
    END.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ----------------------------------------------- po/rep/po-pvar.p 11/98 FWK */
    /* PO Purchased Variance                                                      */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-foot-rem     LIKE ap-invl.amt-msf NO-UNDO.
    DEFINE VARIABLE v-msf-cal      AS LOG       NO-UNDO.

    DEFINE VARIABLE v-s-pono       LIKE po-ord.po-no.
    DEFINE VARIABLE v-e-pono       LIKE v-s-pono INIT 999999.
    DEFINE VARIABLE v-s-date       LIKE po-ord.po-date FORMAT "99/99/9999" INIT "01/01/0001".
    DEFINE VARIABLE v-e-date       LIKE v-s-date INIT TODAY.
    DEFINE VARIABLE v-s-vend       LIKE po-ord.vend-no.
    DEFINE VARIABLE v-e-vend       LIKE v-s-vend INIT "zzzzzzzz".
    DEFINE VARIABLE v-s-item       LIKE po-ordl.i-no.
    DEFINE VARIABLE v-tt-ei        LIKE v-s-item INIT "zzzzzzzzzzzzzzz".
    DEFINE VARIABLE v-s-job        LIKE po-ordl.job-no.
    DEFINE VARIABLE v-e-job        LIKE v-s-job INIT "zzzzzzzzz".
    DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "A".
    DEFINE VARIABLE v-type         AS CHARACTER FORMAT "!" INIT "B".
    DEFINE VARIABLE v-sort         AS CHARACTER FORMAT "!" INIT "V".
    DEFINE VARIABLE vb-rec-date    LIKE po-ord.po-date FORMAT "99/99/9999" INIT "01/01/0001".
    DEFINE VARIABLE ve-rec-date    LIKE v-s-date INIT TODAY.

    DEFINE VARIABLE v-mattype-list AS CHARACTER FORMAT "x(36)".
    DEFINE VARIABLE v-mat-dscr     AS CHARACTER FORMAT "x(20)" EXTENT 21.

    DEFINE VARIABLE v-bal          LIKE rm-rdtlh.qty.

    DEFINE VARIABLE v-first        LIKE report.key-02 EXTENT 4.
    DEFINE VARIABLE v-ord          LIKE rm-rdtlh.qty EXTENT 4.
    DEFINE VARIABLE v-qty          LIKE rm-rdtlh.qty EXTENT 4.

    DEFINE VARIABLE v-tot-msf      AS DECIMAL   FORMAT ">>,>>9.999" INIT 0 NO-UNDO.
    DEFINE VARIABLE v-tot-vend     AS DECIMAL   FORMAT ">>,>>9.999" INIT 0 NO-UNDO.
    DEFINE VARIABLE v-uom-vend     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE diff-price     AS DECIMAL   FORMAT "->>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-vend-cost    AS DECIMAL   FORMAT "->>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL   FORMAT "->>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-sub-msf      AS DECIMAL   FORMAT "->>,>>>,>>9.999" NO-UNDO. 
    DEFINE VARIABLE v-sub-diff     AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-sub-vend     AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-sub-bght     AS DECIMAL   FORMAT "->>>>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-grand-msf    AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-grand-diff   AS DECIMAL   FORMAT "->>>>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-grand-vend   AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-grand-bght   AS DECIMAL   FORMAT "->>>>>>,>>9.99" NO-UNDO.

    DEFINE VARIABLE ii             AS INTEGER.
    DEFINE VARIABLE ld             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-bld-job      AS CHARACTER FORMAT "x(9)".
    DEFINE VARIABLE v-inv-cost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-po-cost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-mpv          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-line-num     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-overs        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ord-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-inv-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE excelheader    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE viIndex        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE receiptDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE DueDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE v-moa-cols     AS LOG       NO-UNDO.
    DEFINE VARIABLE ld-dim-charge  AS DECIMAL   NO-UNDO.

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
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE v-page         AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE vaddr          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vaddr2         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
//DEFINE VARIABLE cFileName LIKE fi_file NO-UNDO .

    DEFINE VARIABLE cReturnValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lError         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

    {sys/form/r-top5DL3.f}
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


    FORM po-ord.po-no          COLUMN-LABEL "PO #" SPACE(2)
        po-ord.vend-no        COLUMN-LABEL "Vendor #" SPACE(2)
        v-bld-job             COLUMN-LABEL "Job #" SPACE(2)
        po-ordl.i-no          COLUMN-LABEL "Item #" SPACE(2)
        DueDate               COLUMN-LABEL "Due Date" SPACE(2)
        receiptDate           COLUMN-LABEL "Received" SPACE(2)
        v-tot-msf             COLUMN-LABEL "MSF" SPACE(3)
        v-vend-cost           COLUMN-LABEL "Vendor $" SPACE(3)
        v-cost                COLUMN-LABEL "Bought $" SPACE(3)
        diff-price            COLUMN-LABEL "Diff $"
        WITH FRAME main NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 180.

    FORM po-ord.po-no          COLUMN-LABEL "PO #" SPACE(2)
        po-ord.vend-no        COLUMN-LABEL "Vendor #!/Adders" SPACE(2)
        v-bld-job             COLUMN-LABEL "Job #" SPACE(2)
        po-ordl.i-no          COLUMN-LABEL "Item #" SPACE(2)
        DueDate               COLUMN-LABEL "Due Date" SPACE(2)
        receiptDate           COLUMN-LABEL "Received" SPACE(2)
        v-tot-msf             COLUMN-LABEL "MSF" SPACE(3)
        v-vend-cost           COLUMN-LABEL "Vendor $" SPACE(3)
        v-cost                COLUMN-LABEL "Bought $" SPACE(3)
        diff-price            COLUMN-LABEL "Diff $"
        v-mpv                 COLUMN-LABEL "MPV %"  
        v-overs               COLUMN-LABEL "Overs %" 
        WITH FRAME main-b NO-BOX NO-ATTR-SPACE DOWN STREAM-IO WIDTH 180.

    {ce/msfcalc.i}

    ASSIGN
        str-tit2    = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-s-pono    = begin_po-no
        v-e-pono    = end_po-no
        v-s-date    = begin_po-date
        v-e-date    = end_po-date
        v-s-vend    = begin_vend-no
        v-e-vend    = end_vend-no
        v-s-item    = begin_po-i-no
        v-tt-ei     = end_po-i-no
        v-s-job     = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', begin_job-no)) 
        v-e-job     = STRING(DYNAMIC-FUNCTION('sfFormat_SingleJob', END_job-no)) 
        vb-rec-date = begin_rec-date
        ve-rec-date = end_rec-date .

    IF tb_mpv OR tb_overs OR tb_adder THEN
        v-moa-cols = YES.


    DEFINE VARIABLE cslist AS cha NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF NOT tb_mpv AND LOOKUP(ttRptSelected.TextList, "MPV %") <> 0 THEN NEXT.
        IF NOT tb_overs AND LOOKUP(ttRptSelected.TextList, "Overs %") <> 0 THEN NEXT.
        IF NOT tb_adder AND LOOKUP(ttRptSelected.TextList, "Adder code1,Adder code2") <> 0 THEN NEXT.

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

        IF LOOKUP(ttRptSelected.TextList, "MSF,Vendor $,Bought $,Diff $") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    IF rd-dest = 3 THEN 
    DO:
        OUTPUT STREAM st-excel TO VALUE(cFileName).
        PUT STREAM st-excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.
 
    RUN sys/ref/nk1look.p(
        INPUT  cocode,         /* Company       */
        INPUT  "vendItemCost", /* Sys-Ctrl Name */
        INPUT  "L",            /* Logical       */
        INPUT  NO,             /* Check by cust */
        INPUT  YES,            /* Use Cust      */
        INPUT  "",             /* Customer      */
        INPUT  "",             /* Ship-to       */
        OUTPUT cReturnValue,
        OUTPUT lRecFound
        ).

    {sa/sa-sls01.i}

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY "" WITH FRAME r-top.

    SESSION:SET-WAIT-STATE ("general").

    IF rd-dest = 3 AND fi_file NE '' THEN 
    DO:
    /*OUTPUT STREAM st-excel TO VALUE(fi_file).
    excelheader = "PO #,Vendor #".
    IF tb_adder THEN
       excelheader = excelheader + "/Adders".
    excelheader = excelheader
                + ",Job #,Item #,Due Date,Received,MSF,Vendor $,Bought $,Diff $".
    IF tb_mpv THEN
       excelheader = excelheader + ",MPV %".
    IF tb_overs THEN
       excelheader = excelheader + ",Overs %".
    PUT STREAM st-excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.*/
    END.

    FOR EACH po-ord 
        WHERE po-ord.company EQ cocode
        AND po-ord.po-no   GE v-s-pono
        AND po-ord.po-no   LE v-e-pono
        AND po-ord.po-date GE v-s-date
        AND po-ord.po-date LE v-e-date
        AND po-ord.vend-no GE v-s-vend
        AND po-ord.vend-no LE v-e-vend
        NO-LOCK,

        EACH po-ordl
        WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no EQ po-ord.po-no
        AND po-ordl.i-no      GE v-s-item
        AND po-ordl.i-no      LE v-tt-ei
        AND FILL(" ", iJobLen - length(TRIM(po-ordl.job-no))) + trim(po-ordl.job-no) GE v-s-job
        AND FILL(" ", iJobLen - length(TRIM(po-ordl.job-no))) + trim(po-ordl.job-no) LE v-e-job
        NO-LOCK BREAK BY po-ord.po-no
        BY po-ord.po-date
        BY po-ordl.po-no 
        BY po-ord.vend-no 
        BY po-ordl.job-no 
        BY po-ordl.job-no2 
        BY po-ordl.i-no:
        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

        FIND FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
            AND po-ordl.item-type
            NO-LOCK NO-ERROR.

        ASSIGN 
            v-bld-job = "".
        DO ii = 1 TO 6: 
            IF SUBSTRING(po-ordl.job-no,ii,1) NE " " THEN
                ASSIGN v-bld-job = TRIM(v-bld-job +
           substring(po-ordl.job-no,ii,1)).
        END. 
        IF v-bld-job NE "      " THEN
            ASSIGN v-bld-job = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', v-bld-job, po-ordl.job-no2)) .

        IF po-ordl.pr-qty-uom EQ "MSF" THEN
            v-tot-msf = po-ordl.ord-qty.
        ELSE
            RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                (IF AVAILABLE item THEN item.basis-w ELSE 0),
                po-ordl.s-len, po-ordl.s-wid,
                (IF AVAILABLE item THEN item.s-dep ELSE 0),
                po-ordl.ord-qty, OUTPUT v-tot-msf).

        DEFINE VARIABLE pr-ct AS INTEGER NO-UNDO.

        ASSIGN
            v-vend-cost = 0
            receiptDate = ?   .

        /* IF po-ord.received THEN DO:*/
        IF po-ordl.item-type THEN 
        DO:
            FIND FIRST rm-rcpth NO-LOCK
                WHERE rm-rcpth.company EQ cocode
                AND rm-rcpth.i-no EQ po-ordl.i-no
                AND (((rm-rcpth.rita-code EQ "R" OR rm-rcpth.rita-code EQ "A")))
                AND rm-rcpth.po-no      EQ trim(STRING(po-ordl.po-no,">>>>>9")) 
                AND  rm-rcpth.trans-date GE vb-rec-date  
                AND rm-rcpth.trans-date  LE ve-rec-date     NO-ERROR.
            IF AVAILABLE rm-rcpth THEN receiptDate = rm-rcpth.trans-date.
            IF tb_receipt THEN
                IF NOT AVAILABLE rm-rcpth THEN NEXT .
        END.
        ELSE 
        DO:
            FIND FIRST fg-rcpth NO-LOCK
                WHERE fg-rcpth.company EQ cocode
                AND fg-rcpth.i-no EQ po-ordl.i-no
                AND fg-rcpth.po-no      EQ trim(STRING(po-ordl.po-no,">>>>>9"))
                AND (fg-rcpth.rita-code EQ "R" OR
                fg-rcpth.rita-code EQ "A") 
                AND fg-rcpth.trans-date GE vb-rec-date  
                AND fg-rcpth.trans-date LE ve-rec-date  NO-ERROR.
            IF AVAILABLE fg-rcpth THEN receiptDate = fg-rcpth.trans-date .
            IF tb_receipt THEN
                IF NOT AVAILABLE fg-rcpth THEN NEXT .
        END.
        /*END.*/

        IF rd_vend-cost BEGINS "Vend" THEN 
        DO:

            EMPTY TEMP-TABLE tt-ei.
            EMPTY TEMP-TABLE tt-eiv.
    
            IF lRecFound AND LOGICAL(cReturnValue) THEN 
                RUN pBuildTTVendItemCost( 
                    INPUT cocode,                                     /*Company code  */
                    INPUT po-ordl.i-no,                               /*Item number   */                
                    INPUT (IF po-ordl.item-type THEN "RM" ELSE "FG"), /*Item type     */
                    INPUT po-ord.vend-no                              /*Vendor number */
                    ).
                
            ELSE 
            DO: 
                /* If item type is RM (Raw Material) */
                IF po-ordl.item-type THEN 
                DO:
                    FIND FIRST e-item
                        WHERE e-item.company EQ cocode
                        AND e-item.i-no    EQ po-ordl.i-no
                        NO-LOCK NO-ERROR.
        
                    IF AVAILABLE e-item THEN 
                    DO:
                        CREATE tt-ei.
                        ASSIGN 
                            tt-ei.std-uom = e-item.std-uom.
                
                        FIND FIRST e-item-vend OF e-item
                            WHERE e-item-vend.vend-no EQ po-ord.vend-no
                            NO-LOCK NO-ERROR.
        
                        IF AVAILABLE e-item-vend THEN 
                        DO:
                            CREATE tt-eiv.
                            DO pr-ct = 1 TO 10:
                                ASSIGN
                                    tt-eiv.run-qty[pr-ct]  = e-item-vend.run-qty[pr-ct]
                                    tt-eiv.run-cost[pr-ct] = e-item-vend.run-cost[pr-ct]
                                    .
                            END.
               
                            IF AVAILABLE e-item-vend THEN 
                            DO:
                                DO pr-ct = 1 TO 10:
                                    ASSIGN
                                        tt-eiv.run-qty[pr-ct + 10]  = e-item-vend.runQtyXtra[pr-ct]
                                        tt-eiv.run-cost[pr-ct + 10] = e-item-vend.runCostXtra[pr-ct]
                                        .
                                END.
                            END.
                        END.
                    END.
                END.
                ELSE 
                DO: /* If item type is FG (Finished Goods) */
                    FIND FIRST e-itemfg
                        WHERE e-itemfg.company EQ cocode
                        AND e-itemfg.i-no    EQ po-ordl.i-no
                        NO-LOCK NO-ERROR.
    
                    IF AVAILABLE e-itemfg THEN 
                    DO:
      
                        CREATE tt-ei.
                        BUFFER-COPY e-itemfg TO tt-ei.
    
                        FIND FIRST e-itemfg-vend OF e-itemfg
                            WHERE e-itemfg-vend.vend-no EQ po-ord.vend-no
                            NO-LOCK NO-ERROR.
    
                        IF AVAILABLE e-itemfg-vend THEN 
                        DO:
                            CREATE tt-eiv.
                            DO pr-ct = 1 TO 10:
                                ASSIGN
                                    tt-eiv.run-qty[pr-ct]  = e-itemfg-vend.run-qty[pr-ct]
                                    tt-eiv.run-cost[pr-ct] = e-itemfg-vend.run-cost[pr-ct]
                                    .
                            END.
                        END.
                    END.
                END. 
            END.

            FIND FIRST tt-eiv NO-ERROR.

            IF AVAILABLE tt-eiv THEN 
            DO:
                FIND FIRST tt-ei NO-ERROR.

                v-uom-vend = IF AVAILABLE tt-ei THEN tt-ei.std-uom ELSE "EA".

                IF po-ordl.pr-qty-uom EQ v-uom-vend THEN
                    v-tot-vend = po-ordl.ord-qty.
                ELSE
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, v-uom-vend,
                        (IF AVAILABLE item THEN item.basis-w ELSE 0),
                        po-ordl.s-len, po-ordl.s-wid,
                        (IF AVAILABLE item THEN item.s-dep ELSE 0),
                        po-ordl.ord-qty, OUTPUT v-tot-vend).

                IF AVAILABLE tt-eiv THEN 
                DO:
                    ld-dim-charge = 0.
                    IF LOGICAL(cReturnValue) AND AVAILABLE(vendItemCost) THEN
                        RUN GetDimCharge (
                            INPUT ROWID(vendItemCost),        /*VendItemCost RowID*/
                            INPUT po-ordl.s-wid,              /*Width             */
                            INPUT po-ordl.s-len,              /*Length            */
                            INPUT-OUTPUT ld-dim-charge,       /*Dim charge        */
                            OUTPUT lError,                    /*Success flag      */
                            OUTPUT cMessage                   /*Message           */
                            ) NO-ERROR.               
                    ELSE IF AVAILABLE(e-item-vend)  THEN
                            RUN est/dim-charge.p (
                                INPUT e-item-vend.rec_key,
                                INPUT po-ordl.s-wid,
                                INPUT po-ordl.s-len,
                                INPUT-OUTPUT ld-dim-charge
                                ).     
                          
                    DO pr-ct = 1 TO 20:
                        IF tt-eiv.run-qty[pr-ct] GE v-tot-vend THEN 
                        DO:
                            v-vend-cost = (tt-eiv.run-cost[pr-ct] + ld-dim-charge) * v-tot-vend.
                            LEAVE.
                        END.
                    END.
                END.
            END.
        END.

        ELSE RUN sys/inc/po-invqa.p (RECID(po-ordl), OUTPUT ld, OUTPUT v-vend-cost).

        v-line-num = ((po-ordl.po-no * 1000) + po-ordl.line).

        IF tb_mpv OR tb_overs THEN
        DO:
            FIND FIRST ap-invl WHERE
                ap-invl.company EQ cocode AND
                ap-invl.po-no EQ po-ord.po-no AND
                ap-invl.LINE EQ v-line-num
                NO-LOCK NO-ERROR.

            IF AVAILABLE ap-invl THEN
            DO:
                IF ap-invl.pr-qty-uom EQ "MSF" THEN
                    v-inv-cost = ap-invl.unit-pr.
                ELSE
                    RUN sys/ref/convcuom.p(ap-invl.pr-qty-uom, "MSF",
                        (IF AVAILABLE item THEN item.basis-w ELSE 0),
                        po-ordl.s-len, po-ordl.s-wid,
                        (IF AVAILABLE item THEN item.s-dep ELSE 0),
                        ap-invl.unit-pr, OUTPUT v-inv-cost).

                IF po-ordl.pr-uom EQ "MSF" THEN
                    v-po-cost = po-ordl.cost.
                ELSE
                    RUN sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                        (IF AVAILABLE item THEN item.basis-w ELSE 0),
                        po-ordl.s-len, po-ordl.s-wid,
                        (IF AVAILABLE item THEN item.s-dep ELSE 0),
                        po-ordl.cost, OUTPUT v-po-cost).

                IF po-ordl.pr-qty-uom EQ "MSF" THEN
                    v-ord-qty = po-ordl.ord-qty.
                ELSE
                    RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                        (IF AVAILABLE item THEN item.basis-w ELSE 0),
                        po-ordl.s-len, po-ordl.s-wid,
                        (IF AVAILABLE item THEN item.s-dep ELSE 0),
                        po-ordl.ord-qty, OUTPUT v-ord-qty).

                IF ap-invl.cons-uom EQ "MSF" THEN
                    v-inv-qty = ap-invl.qty. 
                ELSE
                    RUN sys/ref/convquom.p(ap-invl.cons-uom, "MSF",
                        (IF AVAILABLE item THEN item.basis-w ELSE 0),
                        po-ordl.s-len, po-ordl.s-wid,
                        (IF AVAILABLE item THEN item.s-dep ELSE 0),
                        ap-invl.qty, OUTPUT v-inv-qty).

                ASSIGN
                    v-mpv   = IF v-po-cost NE 0 THEN (v-inv-cost / v-po-cost) * 100
                 ELSE 0
                    v-overs = IF v-ord-qty NE 0 THEN (v-inv-qty / v-ord-qty) * 100
                   ELSE 0.

                RELEASE ap-invl.
            END.
            ELSE
                ASSIGN
                    v-mpv   = 0
                    v-overs = 0.
        END.

        IF tb_adder THEN
            RUN adder-proc.

        ASSIGN 
            v-cost       = po-ordl.t-cost
            DueDate      = po-ordl.due-date
            diff-price   = v-cost - v-vend-cost 
            v-sub-msf    = v-sub-msf + v-tot-msf
            v-sub-diff   = v-sub-diff + diff-price
            v-sub-vend   = v-sub-vend + v-vend-cost
            v-sub-bght   = v-sub-bght + v-cost
            v-grand-msf  = v-grand-msf + v-tot-msf
            v-grand-diff = v-grand-diff + diff-price
            v-grand-vend = v-grand-vend + v-vend-cost
            v-grand-bght = v-grand-bght + v-cost.

        DO WITH FRAME main:

            /*IF NOT v-moa-cols THEN
            DO:
          
              IF tb_repeat THEN DISPLAY po-ord.po-no WITH FRAME main.
              ELSE IF FIRST-OF(po-ord.po-no) THEN DISPLAY po-ord.po-no WITH FRAME main.
              IF tb_repeat THEN DISPLAY po-ord.vend-no WITH FRAME main.
              ELSE IF FIRST-OF(po-ord.vend-no) THEN DISPLAY po-ord.vend-no WITH FRAME main.
              DISPLAY
               v-bld-job
               po-ordl.i-no
               DueDate 
               receiptDate
               v-tot-msf
               v-vend-cost
               v-cost
               diff-price
               WITH FRAME main. 
          
            END.
            ELSE
            DO:
          
                PUT SPACE (1).
              IF tb_repeat THEN DISPLAY po-ord.po-no WITH FRAME main-b.
              ELSE IF FIRST-OF(po-ord.po-no) THEN DISPLAY po-ord.po-no WITH FRAME main-b.
              IF tb_repeat THEN DISPLAY po-ord.vend-no WITH FRAME main-b.
              ELSE IF FIRST-OF(po-ord.vend-no) THEN DISPLAY po-ord.vend-no
                                                    WITH FRAME main-b.
              DISPLAY
               v-bld-job
               po-ordl.i-no
               DueDate 
               receiptDate
               v-tot-msf
               v-vend-cost
               v-cost
               diff-price
               v-mpv WHEN tb_mpv
               v-overs WHEN tb_overs  WITH FRAME main-b.
            END.
            IF tb_adder THEN
               FOR EACH temp-adder:
                   PUT SPACE(8) temp-adder.adder SKIP.
               END.
            IF tb_excel AND fi_file NE '' THEN
            DO:
               PUT STREAM st-excel UNFORMATTED
                 '"' po-ord.po-no '",'
                 '"' po-ord.vend-no '",'
                 '"' v-bld-job '",'
                 '"' REPLACE(REPLACE(po-ordl.i-no,',',' '),'"','~'~'') '",'
                 '"' (IF po-ordl.due-date NE ? THEN STRING(po-ordl.due-date) ELSE '') '",'
                 '"' (IF receiptDate NE ? THEN STRING(receiptDate) ELSE '') '",'
                 '"' v-tot-msf '",'
                 '"' v-vend-cost '",'
                 '"' v-cost '",'
                 '"' diff-price '",'
                 '"' IF tb_mpv THEN STRING(v-mpv) ELSE "" '",'
                 '"' IF tb_overs THEN STRING(v-overs) ELSE "" '",' SKIP.
          
                IF tb_adder THEN
                   FOR EACH temp-adder:
                       PUT STREAM st-excel UNFORMATTED
                           '"' ""               '",'
                           '"' temp-adder.adder '",' SKIP.
                   END.
            END.*/

            IF tb_adder THEN 
            DO:
                vaddr = "" .
                vaddr2 = "" .
                i = 1 .
                FOR EACH temp-adder:
                    IF i EQ 1 THEN
                        ASSIGN vaddr = temp-adder.adder .
                    IF i EQ 2 THEN
                        ASSIGN vaddr2 = temp-adder.adder .
                    i = i + 1.
                END.

            END.

            IF tb_repeat THEN 
            DO:
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "" .

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                    IF NOT tb_mpv AND LOOKUP(cTmpField, "mpv") <> 0 THEN NEXT.
                    IF NOT tb_overs AND LOOKUP(cTmpField, "ovr") <> 0 THEN NEXT.
                    IF NOT tb_adder AND LOOKUP(cTmpField, "addr") <> 0 THEN NEXT.
                    IF NOT tb_adder AND LOOKUP(cTmpField, "addr2") <> 0 THEN NEXT.
                    CASE cTmpField:               
                        WHEN "po-no" THEN 
                            cVarValue = STRING(po-ord.po-no) .
                        WHEN "vend" THEN 
                            cVarValue = STRING(po-ord.vend-no) .
                        WHEN "job" THEN 
                            cVarValue = STRING(v-bld-job).
                        WHEN "item" THEN 
                            cVarValue = STRING(po-ordl.i-no).
                        WHEN "due-dt" THEN 
                            cVarValue = IF DueDate NE ? THEN STRING(DueDate) ELSE "".
                        WHEN "rcd-dt" THEN 
                            cVarValue = IF receiptDate NE ? THEN STRING(receiptDate) ELSE "".
                        WHEN "msf" THEN 
                            cVarValue = IF v-tot-msf NE ? THEN STRING(v-tot-msf,">>,>>9.999") ELSE "".
                        WHEN "vend-$" THEN 
                            cVarValue = IF v-vend-cost NE ? THEN STRING(v-vend-cost,"->>,>>>,>>9.99") ELSE "".
                        WHEN "bught" THEN 
                            cVarValue = IF v-cost NE ? THEN STRING(v-cost,"->,>>>,>>9.99") ELSE "".
                        WHEN "diff" THEN 
                            cVarValue = IF diff-price NE ? THEN STRING(diff-price,"->>,>>>,>>9.99") ELSE "".
                        WHEN "mpv" THEN 
                            cVarValue = IF tb_mpv AND v-mpv NE ? THEN STRING(v-mpv,"->>>,>>9.99") ELSE "".
                        WHEN "ovr" THEN 
                            cVarValue = IF tb_overs AND v-overs NE ? THEN STRING(v-overs,"->>>,>>9.99") ELSE "".                        
                        WHEN "addr" THEN 
                            cVarValue = IF tb_adder THEN STRING(vaddr,"x(15)") ELSE "". 
                        WHEN "addr2" THEN 
                            cVarValue = IF tb_adder THEN STRING(vaddr2,"x(15)") ELSE "". 

                    END CASE.

                    IF  cTmpField = "due-dt" THEN
                         cExcelVarValue = IF DueDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",DueDate) ELSE "".
                    ELSE IF  cTmpField = "rcd-dt" THEN
                         cExcelVarValue = IF receiptDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",receiptDate) ELSE "".
                    ELSE cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
                END.
                PUT UNFORMATTED cDisplay SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM st-excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.
            ELSE 
            DO:
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "" .

                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                    IF NOT tb_mpv AND LOOKUP(cTmpField, "mpv") <> 0 THEN NEXT.
                    IF NOT tb_overs AND LOOKUP(cTmpField, "ovr") <> 0 THEN NEXT.
                    IF NOT tb_adder AND LOOKUP(cTmpField, "addr") <> 0 THEN NEXT.
                    IF NOT tb_adder AND LOOKUP(cTmpField, "addr2") <> 0 THEN NEXT.

                    CASE cTmpField:               
                        WHEN "po-no" THEN 
                            cVarValue = IF FIRST-OF(po-ord.vend-no) THEN STRING(po-ord.po-no) ELSE "".
                        WHEN "vend" THEN 
                            cVarValue = IF FIRST-OF(po-ord.vend-no) THEN STRING(po-ord.vend-no) ELSE "".
                        WHEN "job" THEN 
                            cVarValue = STRING(v-bld-job).
                        WHEN "item" THEN 
                            cVarValue = STRING(po-ordl.i-no).
                        WHEN "due-dt" THEN 
                            cVarValue = IF DueDate NE ? THEN STRING(DueDate) ELSE "".
                        WHEN "rcd-dt" THEN 
                            cVarValue = IF receiptDate NE ? THEN STRING(receiptDate) ELSE "".
                        WHEN "msf" THEN 
                            cVarValue = IF v-tot-msf NE ? THEN STRING(v-tot-msf,">>,>>9.999") ELSE "".
                        WHEN "vend-$" THEN 
                            cVarValue = IF v-vend-cost NE ? THEN STRING(v-vend-cost,"->>,>>>,>>9.99") ELSE "".
                        WHEN "bught" THEN 
                            cVarValue = IF v-cost NE ? THEN STRING(v-cost,"->,>>>,>>9.99") ELSE "".
                        WHEN "diff" THEN 
                            cVarValue = IF diff-price NE ? THEN STRING(diff-price,"->>,>>>,>>9.99") ELSE "".
                        WHEN "mpv" THEN 
                            cVarValue = IF tb_mpv AND v-mpv NE ? THEN STRING(v-mpv,"->>>,>>9.99") ELSE "".
                        WHEN "ovr" THEN 
                            cVarValue = IF tb_overs AND v-overs NE ? THEN STRING(v-overs,"->>>,>>9.99") ELSE "".                        
                        WHEN "addr" THEN 
                            cVarValue = IF tb_adder THEN STRING(vaddr,"x(15)") ELSE "". 
                        WHEN "addr2" THEN 
                            cVarValue = IF tb_adder THEN STRING(vaddr2,"x(15)") ELSE "". 

                    END CASE.

                    IF  cTmpField = "due-dt" THEN
                         cExcelVarValue = IF DueDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",DueDate) ELSE "".
                    ELSE IF  cTmpField = "rcd-dt" THEN
                         cExcelVarValue = IF receiptDate NE ? THEN DYNAMIC-FUNCTION("sfFormat_Date",receiptDate) ELSE "".
                    ELSE cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(DYNAMIC-FUNCTION("FormatForCSV" IN hdOutputProcs, cExcelVarValue)) + ",".            
                END.
                PUT UNFORMATTED cDisplay SKIP.
                IF rd-dest = 3 THEN 
                DO:
                    PUT STREAM st-excel UNFORMATTED  
                        cExcelDisplay SKIP.
                END.
            END.
        END.

        DOWN WITH FRAME main.

        IF LAST-OF(po-ord.po-no) THEN
        DO:
            /*put     "----------" at 69
                    "-----------" at 82
                    "-----------" at 96
                    "-----------" at 110 skip
                    "Sub Totals:" to 30
                    v-sub-msf  to 76
                    v-sub-vend to 91
                    v-sub-bght to 105
                    v-sub-diff to 120 skip(1).
            IF tb_excel AND fi_file NE '' THEN
            PUT STREAM st-excel UNFORMATTED
              SKIP(1)
              '"",'
              '"",'
              '"Sub Totals:",'
              '"",'
              '"",'
              '"",'
              '"' v-sub-msf '",'
              '"' v-sub-vend '",'
              '"' v-sub-bght '",'
              '"' v-sub-diff '"' SKIP(1). */
            PUT    SKIP  str-line SKIP .
            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "" .

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

                CASE cTmpField:               
                    WHEN "po-no" THEN 
                        cVarValue = "".
                    WHEN "vend" THEN 
                        cVarValue = "".
                    WHEN "job" THEN 
                        cVarValue = "".
                    WHEN "item" THEN 
                        cVarValue = "".
                    WHEN "due-dt" THEN 
                        cVarValue = "".
                    WHEN "rcd-dt" THEN 
                        cVarValue = "".
                    WHEN "msf" THEN 
                        cVarValue = STRING(v-sub-msf,"->>,>>>,>>9.999").
                    WHEN "vend-$" THEN 
                        cVarValue = STRING(v-sub-vend,"->>>,>>>,>>9.99").
                    WHEN "bught" THEN 
                        cVarValue = STRING(v-sub-bght,"->>>>>>,>>9.99").
                    WHEN "diff" THEN 
                        cVarValue = STRING(v-sub-diff,"->>>,>>>,>>9.99").
                    WHEN "mpv" THEN 
                        cVarValue = "".
                    WHEN "ovr" THEN 
                        cVarValue = "".                        
                    WHEN "addr" THEN 
                        cVarValue = "". 
                    WHEN "addr2" THEN 
                        cVarValue = "" .

                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT UNFORMATTED  
                "    Sub Totals:" SUBSTRING(cDisplay,21,300) SKIP.
            IF rd-dest = 3 THEN 
            DO:
                PUT STREAM st-excel UNFORMATTED  
                    " Sub Totals: " + substring(cExcelDisplay,3,300) SKIP.
            END.

            ASSIGN 
                v-sub-msf  = 0
                v-sub-diff = 0
                v-sub-vend = 0
                v-sub-bght = 0.
        END.


    END.

    /*put     "----------" at 69
            "-----------" at 82
            "-----------" at 96
            "-----------" at 110 skip
            "Grand Totals:" to 30
            v-grand-msf  to 76
            v-grand-vend to 91
            v-grand-bght to 105
            v-grand-diff to 120 skip(1).
  
    IF tb_excel AND fi_file NE '' THEN
    PUT STREAM st-excel UNFORMATTED
      SKIP(1)
      '"",'
      '"",'
      '"Grand Totals:",'
      '"",'
      '"",'
      '"",'
      '"' v-grand-msf '",'
      '"' v-grand-vend '",'
      '"' v-grand-bght '",'
      '"' v-grand-diff '"' SKIP(1). */
    PUT    SKIP  str-line SKIP .
    ASSIGN 
        cDisplay       = ""
        cTmpField      = ""
        cVarValue      = ""
        cExcelDisplay  = ""
        cExcelVarValue = "" .

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).

        CASE cTmpField:               
            WHEN "po-no" THEN 
                cVarValue = "".
            WHEN "vend" THEN 
                cVarValue = "".
            WHEN "job" THEN 
                cVarValue = "".
            WHEN "item" THEN 
                cVarValue = "".
            WHEN "due-dt" THEN 
                cVarValue = "".
            WHEN "rcd-dt" THEN 
                cVarValue = "".
            WHEN "msf" THEN 
                cVarValue = STRING(v-grand-msf,"->>,>>>,>>9.999").
            WHEN "vend-$" THEN 
                cVarValue = STRING(v-grand-vend,"->>>,>>>,>>9.99").
            WHEN "bught" THEN 
                cVarValue = STRING(v-grand-bght,"->>>>>>,>>9.99").
            WHEN "diff" THEN 
                cVarValue = STRING(v-grand-diff,"->>>,>>>,>>9.99").
            WHEN "mpv" THEN 
                cVarValue = "".
            WHEN "ovr" THEN 
                cVarValue = "".                        
            WHEN "addr" THEN 
                cVarValue = "".
            WHEN "addr2" THEN 
                cVarValue = "" .

        END CASE.

        cExcelVarValue = cVarValue.
        cDisplay = cDisplay + cVarValue +
            FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
        cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
    END.
    PUT UNFORMATTED  
        "    Grand Totals:" SUBSTRING(cDisplay,23,300) SKIP.
    IF rd-dest = 3 THEN 
    DO:
        PUT STREAM st-excel UNFORMATTED  
            " Grand Totals: " + substring(cExcelDisplay,3,300) SKIP.
    END.

    ASSIGN 
        v-grand-msf  = 0
        v-grand-diff = 0
        v-grand-vend = 0
        v-grand-bght = 0.

    IF rd-dest = 3 AND cFileName NE '' THEN
        OUTPUT STREAM st-excel CLOSE.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

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
            fi_file:SCREEN-VALUE = "c:\tmp\r-purvar.csv".   
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

