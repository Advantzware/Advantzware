&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: pcrep\r-maceff.w

  Description: Machine Efficiency

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

{cec/msfcalc.i}

DEFINE TEMP-TABLE work-tmp NO-UNDO
    FIELD job                LIKE job.job
    FIELD frm                LIKE job-mch.frm
    FIELD blank-no           LIKE job-mch.blank-no
    FIELD sort-field         AS CHARACTER
    FIELD dept               AS CHARACTER FORMAT 'xx'
    FIELD m-code             LIKE mach.m-code
    FIELD sch-m-code         LIKE mach.m-code
    FIELD pass               LIKE job-mch.pass
    FIELD r-act-hrs          AS DECIMAL   FORMAT '>>>>9.99'
    FIELD m-act-hrs          AS DECIMAL   FORMAT '>>>>9.99'
    FIELD dt-chg-hrs         AS DECIMAL   FORMAT '>>>>9.99'
    FIELD dt-nochg-hrs       AS DECIMAL   FORMAT '>>>>9.99'
    FIELD qty                AS DECIMAL   FORMAT '>>>>>>>>9'
    FIELD msf                AS DECIMAL   FORMAT '>>>>>>.999'
    FIELD qty-fg-rec         AS DECIMAL
    FIELD msf-fg-rec         AS DECIMAL
    FIELD qty-sheets         AS DECIMAL
    FIELD msf-sheets         AS DECIMAL
    FIELD qty-scrap-rec      AS DECIMAL
    FIELD msf-scrap-received AS DECIMAL
    FIELD shift-sort         AS CHARACTER
    FIELD job-no             AS CHARACTER
    FIELD job-no2            AS INTEGER
    FIELD est-no             AS INTEGER
    FIELD job-hr-total       AS DECIMAL
    FIELD tot-mr-hours       AS DECIMAL
    FIELD i-no               AS CHARACTER 
    FIELD cust-no            AS CHARACTER
    FIELD i-name             AS CHARACTER 
    FIELD tot-lin-ft         AS DECIMAL
    INDEX idx1     m-code shift-sort job      job-no job-no2 frm  blank-no
    INDEX work-tmp job    frm        blank-no dept   m-code  pass sort-field
    INDEX job      job-no job-no2.

DEFINE TEMP-TABLE work-rep NO-UNDO
    FIELD sort-field         AS CHARACTER
    FIELD dept               AS CHARACTER FORMAT 'xx'
    FIELD m-code             LIKE mach.m-code
    FIELD sch-m-code         LIKE mach.m-code
    FIELD no-jobs            AS INTEGER
    FIELD no-setups          AS INTEGER
    FIELD r-std-hrs          AS DECIMAL   FORMAT '>>>>9.99'
    FIELD r-act-hrs          AS DECIMAL   FORMAT '>>>>9.99'
    FIELD m-std-hrs          AS DECIMAL   FORMAT '>>>>9.99'
    FIELD m-act-hrs          AS DECIMAL   FORMAT '>>>>9.99'
    FIELD dt-chg-hrs         AS DECIMAL   FORMAT '>>>>9.99'
    FIELD dt-nochg-hrs       AS DECIMAL   FORMAT '>>>>9.99'
    FIELD qty                AS DECIMAL   FORMAT '>>>>>>>>9'
    FIELD msf                AS DECIMAL   FORMAT '>>>>>>.999'
    FIELD qty-fg-rec         AS DECIMAL
    FIELD msf-fg-rec         AS DECIMAL
    FIELD qty-scrap-rec      AS DECIMAL
    FIELD msf-scrap-received AS DECIMAL
    FIELD perc-total-scrap   AS DECIMAL
    FIELD qty-sheets         AS DECIMAL
    FIELD cust-no            AS CHARACTER
    FIELD i-no               AS CHARACTER
    FIELD i-name             AS CHARACTER
    FIELD job-no             AS CHARACTER
    FIELD job-no2            AS INTEGER
    FIELD tot-lin-ft         AS DECIMAL
    INDEX work-rep sort-field dept m-code.

DEFINE TEMP-TABLE work-rep-copy NO-UNDO LIKE work-rep.

DEFINE VARIABLE v-print-fmt    AS CHARACTER.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-shifts      AS LOG       NO-UNDO.

DEFINE BUFFER b-mch-act FOR mch-act.
DEFINE BUFFER b-job-cod FOR job-code.
DEFINE STREAM excel.
DEFINE VARIABLE v-t-sqft           LIKE itemfg.t-sqft NO-UNDO.


DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_msf             AS LOGICAL   INITIAL YES NO-UNDO.
DEFINE VARIABLE dTotLinearFeet     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cFileName          AS CHARACTER NO-UNDO.

ASSIGN 
    cTextListToSelect  = "SHIFT,MACH CODE,DESCRIPTION,QUANTITY,MSF,QTY HOUR,RUN HOURS,MR HOURS," + 
                    "D/T CHGBL,TOTAL CHARGE,D/T No CHARGE,TOTAL HOURS,STD HOURS," +
                    "EFFIC PERCENT,PERCENT UTILIZED,D/T PERCENT,QTY FG RECEIVED,MSF FG RECEIVED," +
                    "SCRAP QTY,SCRAP MSF,% OF TOT SCRAP,JOB #,JOB DESCRIPTION,CUSTOMER,CUSTOMER NAME," +
                    "TOTAL LINEAR FEET,AVERAGE LINEAR FEET/HR"
    cFieldListToSelect = "sft,mach-code,desc,qty,msf,qty-hr,run-hrs,mr-hrs," +
                            "dt-chg,tot-crg,dt-crg,tot-hrs,std-hrs," +
                            "eff-per,per-util,dt-per,qty-fg-rec,msf-fg-rec," +
                            "scr-qty,scr-msf,tot-scrap,job-no,job-dscr,cust-no,cust-name," +
                            "tot-linear-feet,avg-linear-hr"
    cFieldLength       = "5,6,20,10,10,8,8,8," + "8,8,8,8,8," + "8,8,8,10,10," + "10,10,9,10,30,8,30," + "13,14"
    cFieldType         = "c,c,c,i,i,i,i,i," + "i,i,i,i,i," + "i,i,i,i,i," + "i,i,i,c,c,c,c," + "i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "SHIFT,MACH CODE,DESCRIPTION,QUANTITY,MSF,QTY HOUR,RUN HOURS,MR HOURS," + 
                    "D/T CHGBL,TOTAL CHARGE,D/T No CHARGE,TOTAL HOURS,STD HOURS," +
                    "EFFIC PERCENT,PERCENT UTILIZED,D/T PERCENT,QTY FG RECEIVED,MSF FG RECEIVED,SCRAP QTY,SCRAP MSF,% OF TOT SCRAP" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rs_machine begin_dept end_dept ~
begin_mach end_mach begin_date end_date select-shift tb_fold tb_corr ~
rd_sort sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS rs_machine begin_dept end_dept begin_mach ~
end_mach begin_date end_date lbl_select-shift select-shift tb_fold tb_corr ~
lbl_sort rd_sort sl_avail sl_selected rd-dest fi_file tb_OpenCSV ~
tbAutoClose 

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

DEFINE VARIABLE begin_date       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_dept       AS CHARACTER FORMAT "X(4)" 
    LABEL "Beginning Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_mach       AS CHARACTER FORMAT "X(6)" 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date         AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_dept         AS CHARACTER FORMAT "X(4)" INITIAL "zzzz" 
    LABEL "Ending Department" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_mach         AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file          AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\MachineEfficiency.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_select-shift AS CHARACTER FORMAT "X(256)":U INITIAL "Select/Deselect Shifts" 
    VIEW-AS FILL-IN 
    SIZE 27 BY .95
    FONT 6 NO-UNDO.

DEFINE VARIABLE lbl_sort         AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY .95 NO-UNDO.

DEFINE VARIABLE lines-per-page   AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "Lines Per Page" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name     AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
    VIEW-AS FILL-IN 
    SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no       AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
    LABEL "Font" 
    VIEW-AS FILL-IN 
    SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE shifts           AS CHARACTER FORMAT "X(256)":U 
    LABEL "Shifts" 
    VIEW-AS FILL-IN 
    SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt          AS CHARACTER INITIAL "P" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Portrait", "P",
    "Landscape", "L"
    SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest          AS INTEGER   INITIAL 2 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To Email", 5,
    "To CSV", 3
    SIZE 17 BY 4.81 NO-UNDO.

DEFINE VARIABLE rd_sort          AS CHARACTER INITIAL "Dept" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Dept", "Dept",
    "Industry", "Industry",
    "Shift", "Shift"
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rs_machine       AS CHARACTER INITIAL "Machine" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Machine", "Machine",
    "Scheduled Machine", "Schedule"
    SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 97 BY 5.4.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 97 BY 10.48.

DEFINE VARIABLE select-shift AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 28 BY 4.05 NO-UNDO.

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

DEFINE VARIABLE tb_corr      AS LOGICAL   INITIAL YES 
    LABEL "Corrugated?" 
    VIEW-AS TOGGLE-BOX
    SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tb_fold      AS LOGICAL   INITIAL YES 
    LABEL "Folding?" 
    VIEW-AS TOGGLE-BOX
    SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    rs_machine AT ROW 1.86 COL 50 HELP
    "parm" NO-LABELS WIDGET-ID 2
    begin_dept AT ROW 3.29 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Department"
    end_dept AT ROW 3.29 COL 72 COLON-ALIGNED HELP
    "Enter Ending Department"
    begin_mach AT ROW 4.24 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Machine"
    end_mach AT ROW 4.24 COL 72 COLON-ALIGNED HELP
    "Enter Ending Machine"
    begin_date AT ROW 5.19 COL 29 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 5.19 COL 72 COLON-ALIGNED HELP
    "Enter Ending Date"
    lbl_select-shift AT ROW 6.38 COL 68.2 COLON-ALIGNED NO-LABELS
    select-shift AT ROW 7.33 COL 70.2 HELP
    "Enter description of this Material Type." NO-LABELS
    tb_fold AT ROW 7.43 COL 20
    tb_corr AT ROW 8.38 COL 20
    lbl_sort AT ROW 10.05 COL 11 COLON-ALIGNED NO-LABELS
    rd_sort AT ROW 10.05 COL 23 NO-LABELS
    shifts AT ROW 10.05 COL 64.2 COLON-ALIGNED
    sl_avail AT ROW 12.62 COL 3 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 12.62 COL 43.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 12.62 COL 66.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 13.62 COL 43.2 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 14.62 COL 43.2 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 15.67 COL 43.2 WIDGET-ID 40
    btn_down AT ROW 16.71 COL 43.2 WIDGET-ID 42
    lv-ornt AT ROW 18.33 COL 51.8 NO-LABELS
    lines-per-page AT ROW 18.33 COL 90 COLON-ALIGNED
    lv-font-no AT ROW 18.38 COL 42 COLON-ALIGNED
    rd-dest AT ROW 18.57 COL 4.8 NO-LABELS
    lv-font-name AT ROW 19.43 COL 34.6 COLON-ALIGNED NO-LABELS
    tb_excel AT ROW 20.52 COL 98 RIGHT-ALIGNED
    td-show-parm AT ROW 21.29 COL 31.4
    fi_file AT ROW 22.19 COL 29.4 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 22.24 COL 97.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 23.62 COL 31.4 WIDGET-ID 64
    btn-ok AT ROW 24.57 COL 31.2
    btn-cancel AT ROW 24.57 COL 57
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 11.91 COL 3 WIDGET-ID 38
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.05 COL 4.4
    BGCOLOR 15 
    "Total By:" VIEW-AS TEXT
    SIZE 9 BY .62 AT ROW 2 COL 40.2 WIDGET-ID 6
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 17.86 COL 4.4
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 11.91 COL 66.4 WIDGET-ID 44
    RECT-6 AT ROW 18.14 COL 3
    RECT-7 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 101 BY 27.91
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
        TITLE              = "Machine Efficiency Report"
        HEIGHT             = 25.14
        WIDTH              = 101.2
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_dept:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_select-shift IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

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

/* SETTINGS FOR FILL-IN shifts IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    shifts:HIDDEN IN FRAME FRAME-A       = TRUE
    shifts:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_corr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-R                                         */
ASSIGN 
    tb_excel:HIDDEN IN FRAME FRAME-A       = TRUE
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_fold:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Machine Efficiency Report */
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
ON WINDOW-CLOSE OF C-Win /* Machine Efficiency Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
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


&Scoped-define SELF-NAME begin_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_dept C-Win
ON LEAVE OF begin_dept IN FRAME FRAME-A /* Beginning Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mach C-Win
ON LEAVE OF begin_mach IN FRAME FRAME-A /* Beginning Machine */
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
                    {custom/asifax.i &type=''
                            &begin_cust=begin_mach
                            &END_cust= begin_mach
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END. 
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_mach
                             &END_cust=begin_mach
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_mach
                                  &END_cust=begin_mach
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }
                    END.
                END.
            WHEN 6 THEN RUN OUTPUT-to-port.

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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
    DO:
        ASSIGN {&self-name}. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_dept
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_dept C-Win
ON LEAVE OF end_dept IN FRAME FRAME-A /* Ending Department */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_mach
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mach C-Win
ON LEAVE OF end_mach IN FRAME FRAME-A /* Ending Machine */
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


&Scoped-define SELF-NAME rs_machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs_machine C-Win
ON VALUE-CHANGED OF rs_machine IN FRAME FRAME-A
    DO:
        DO WITH FRAME {&FRAME-NAME}:

            ASSIGN rs_machine.

            IF rs_machine = "Schedule" THEN
                ASSIGN begin_mach:LABEL = "Beginning Sch. Machine"
                    end_mach:LABEL   = "Ending Sch. Machine".
            ELSE
                ASSIGN begin_mach:LABEL = "Beginning Machine"
                    end_mach:LABEL   = "Ending Machine".

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME select-shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-shift C-Win
ON VALUE-CHANGED OF select-shift IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shifts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shifts C-Win
ON LEAVE OF shifts IN FRAME FRAME-A /* Shifts */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lv-shift-list AS CHARACTER NO-UNDO.
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
        begin_date = DATE(MONTH(TODAY),1,YEAR(TODAY))
        end_date   = TODAY.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "DR1" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    IF CONNECTED("emptrack") THEN
        RUN pcrep/defshift.p (cocode, OUTPUT lv-shift-list).

    ll-shifts = lv-shift-list NE "".
    IF ll-shifts THEN select-shift:LIST-ITEMS = lv-shift-list.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_dept.
        IF NOT ll-shifts THEN
            ASSIGN
                select-shift:HIDDEN     = YES
                lbl_select-shift:HIDDEN = YES.

        IF rs_machine:SCREEN-VALUE = "Machine" THEN
            ASSIGN begin_mach:LABEL = "Beginning Machine"
                end_mach:LABEL   = "Ending Machine".
        ELSE
            ASSIGN begin_mach:LABEL = "Beginning Sch. Machine"
                end_mach:LABEL   = "Ending Sch. Machine".   
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
    DISPLAY rs_machine begin_dept end_dept begin_mach end_mach begin_date end_date 
        lbl_select-shift select-shift tb_fold tb_corr lbl_sort rd_sort 
        sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 rs_machine begin_dept end_dept begin_mach end_mach 
        begin_date end_date select-shift tb_fold tb_corr rd_sort sl_avail 
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
    {custom/out2file.i}.

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
    /* ------------------------------------------------ pc/rep/mch-eff.p 8/94 gb */
    /* Machine Efficiency Report                                            */
    /* -------------------------------------------------------------------------- */

    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-date                AS DATE      EXTENT 2 FORMAT "99/99/9999" NO-UNDO.
    DEFINE VARIABLE v-dept                AS ch        FORMAT 'x(4)' EXTENT 2 INITIAL [" ","ZZZZ"].
    DEFINE VARIABLE v-mach                LIKE mach.m-code EXTENT 2 INITIAL [" ","ZZZZZZ"].
    DEFINE VARIABLE mr-hr                 LIKE job-mch.mr-hr NO-UNDO.
    DEFINE VARIABLE run-hr                LIKE job-mch.run-hr NO-UNDO.
    DEFINE VARIABLE chg-hrs               AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dt-hrs                AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE tot-hrs               AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE eff-pct               AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.
    DEFINE VARIABLE pct-utl               AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.
    DEFINE VARIABLE pct-dt                AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.
    DEFINE VARIABLE qty-hr                AS INTEGER   FORMAT '>>>>>>9' NO-UNDO.
    DEFINE VARIABLE std-hrs               AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE diff-hrs              AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE sort-run-hrs          AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE sort-mr-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE sort-chg-hrs          AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE sort-nochg-hrs        AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE sort-std-hrs          AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE sort-qty              AS INTEGER   FORMAT '>>>>>>>>>9' NO-UNDO.
    DEFINE VARIABLE sort-qty-sheets       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sort-msf              AS DECIMAL   FORMAT '>>>>>9.999' NO-UNDO.
    DEFINE VARIABLE sort-qty-fg-rec       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sort-msf-fg-rec       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sort-qty-scrap-rec    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sort-msf-scrap-rec    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sort-perc-total-scrap AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sort-shift-jobs       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sort-shift-setups     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dpt-run-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dpt-mr-hrs            AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dpt-chg-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dpt-nochg-hrs         AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dpt-std-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dpt-qty               AS INTEGER   FORMAT '>>>>>>>>>9' NO-UNDO.
    DEFINE VARIABLE dpt-msf               AS DECIMAL   FORMAT '>>>>>9.999' NO-UNDO.
    DEFINE VARIABLE rep-run-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE rep-mr-hrs            AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE rep-chg-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE rep-nochg-hrs         AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE rep-std-hrs           AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE rep-qty               AS INTEGER   FORMAT '>>>>>>>>>9' NO-UNDO.
    DEFINE VARIABLE rep-msf               AS DECIMAL   FORMAT '>>>>>9.999' NO-UNDO.
    DEFINE VARIABLE v-up                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-on                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-out                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-sort               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-industries         AS CHARACTER INIT ",1,2,X" NO-UNDO.
    DEFINE VARIABLE lv-ind-list           AS CHARACTER INIT "Both,Folding,Corrugated,eXclude" NO-UNDO.
    DEFINE VARIABLE lv-shifts             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tot-scrap-pct         AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.

    DEFINE VARIABLE cDisplay              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField                AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4              AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5              AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line              AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5L3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    FORM HEADER
        /* lv-sort FORMAT "x(200)"*/
        SKIP(1)

        WITH FRAME r-top2 NO-LABELS NO-BOX WIDTH 200 STREAM-IO NO-UNDERLINE PAGE-TOP.


    ASSIGN
        str-tit2  = c-win:TITLE + " (D-R-1)"
        {sys/inc/ctrtext.i str-tit2 112}

        v-dept[1] = begin_dept
        v-dept[2] = end_dept
        v-mach[1] = begin_mach
        v-mach[2] = end_mach
        v-date[1] = begin_date
        v-date[2] = end_date.

    DO WITH FRAME {&FRAME-NAME}:
        IF ll-shifts THEN 
        DO:
            DO i = 1 TO select-shift:NUM-ITEMS:
                IF select-shift:IS-SELECTED(i) THEN
                    lv-shifts = lv-shifts + TRIM(SUBSTR(select-shift:ENTRY(i),1,5)) + ",".
            END.
            IF lv-shifts NE "" THEN
                IF SUBSTR(lv-shifts,LENGTH(TRIM(lv-shifts)),1) EQ "," THEN
                    SUBSTR(lv-shifts,LENGTH(TRIM(lv-shifts)),1) = "".

            shifts = lv-shifts.

            DO i = 1 TO LENGTH(shifts):
                IF SUBSTR(shifts,i,1) EQ "," THEN SUBSTR(shifts,i,1) = " ".
            END.

            DISPLAY shifts.
        END.

        shifts:HIDDEN = YES.
    END.


    DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF ttRptSelected.TextList = "MACH CODE" THEN ASSIGN
                str-tit3    = str-tit3 + "  MACH" + " "
                str-tit4    = str-tit4 + "  CODE" + " "
                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                excelheader = excelHeader + ttRptSelected.TextList + "," . 
        ELSE IF ttRptSelected.TextList = "QTY HOUR" THEN ASSIGN
                    str-tit3    = str-tit3 + "     QTY" + " "
                    str-tit4    = str-tit4 + "    HOUR" + " "
                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                    excelheader = excelHeader + ttRptSelected.TextList + "," . 
            ELSE IF ttRptSelected.TextList = "RUN HOURS" THEN ASSIGN
                        str-tit3    = str-tit3 + "     RUN" + " "
                        str-tit4    = str-tit4 + "   HOURS" + " "
                        str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                        excelheader = excelHeader + ttRptSelected.TextList + "," .
                ELSE IF ttRptSelected.TextList = "MR HOURS" THEN ASSIGN
                            str-tit3    = str-tit3 + "      MR" + " "
                            str-tit4    = str-tit4 + "   HOURS" + " "
                            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                            excelheader = excelHeader + ttRptSelected.TextList + "," .
                    ELSE IF ttRptSelected.TextList = "D/T CHGBL" THEN ASSIGN
                                str-tit3    = str-tit3 + "     D/T" + " "
                                str-tit4    = str-tit4 + "   CHGBL" + " "
                                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                excelheader = excelHeader + ttRptSelected.TextList + "," .

                        ELSE IF ttRptSelected.TextList = "TOTAL CHARGE" THEN ASSIGN
                                    str-tit3    = str-tit3 + "   TOTAL" + " "
                                    str-tit4    = str-tit4 + "  CHARGE" + " "
                                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                    excelheader = excelHeader + ttRptSelected.TextList + "," .
                            ELSE IF ttRptSelected.TextList = "D/T No CHARGE" THEN ASSIGN
                                        str-tit3    = str-tit3 + "  D/T No" + " "
                                        str-tit4    = str-tit4 + "  CHARGE" + " "
                                        str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                        excelheader = excelHeader + ttRptSelected.TextList + "," .
                                ELSE IF ttRptSelected.TextList = "TOTAL HOURS" THEN ASSIGN
                                            str-tit3    = str-tit3 + "   TOTAL" + " "
                                            str-tit4    = str-tit4 + "   HOURS" + " "
                                            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                            excelheader = excelHeader + ttRptSelected.TextList + "," .
                                    ELSE IF ttRptSelected.TextList = "STD HOURS" THEN ASSIGN
                                                str-tit3    = str-tit3 + "     STD" + " "
                                                str-tit4    = str-tit4 + "   HOURS" + " "
                                                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                excelheader = excelHeader + ttRptSelected.TextList + "," .
                                        ELSE IF ttRptSelected.TextList = "EFFIC PERCENT" THEN ASSIGN
                                                    str-tit3    = str-tit3 + "   EFFIC" + " "
                                                    str-tit4    = str-tit4 + " PERCENT" + " "
                                                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                    excelheader = excelHeader + ttRptSelected.TextList + "," .
                                            ELSE IF ttRptSelected.TextList = "PERCENT UTILIZED" THEN ASSIGN
                                                        str-tit3    = str-tit3 + " PERCENT" + " "
                                                        str-tit4    = str-tit4 + "UTILIZED" + " "
                                                        str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                        excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                ELSE IF ttRptSelected.TextList = "D/T PERCENT" THEN ASSIGN
                                                            str-tit3    = str-tit3 + "     D/T" + " "
                                                            str-tit4    = str-tit4 + " PERCENT" + " "
                                                            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                            excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                    ELSE IF ttRptSelected.TextList = "QTY FG RECEIVED" THEN ASSIGN
                                                                str-tit3    = str-tit3 + "    QTY FG" + " "
                                                                str-tit4    = str-tit4 + "  RECEIVED" + " "
                                                                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                        ELSE IF ttRptSelected.TextList = "MSF FG RECEIVED" THEN ASSIGN
                                                                    str-tit3    = str-tit3 + "    MSF FG" + " "
                                                                    str-tit4    = str-tit4 + "  RECEIVED" + " "
                                                                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                    excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                            ELSE IF ttRptSelected.TextList = "SCRAP QTY" THEN ASSIGN
                                                                        str-tit3    = str-tit3 + "     SCRAP" + " "
                                                                        str-tit4    = str-tit4 + "       QTY" + " "
                                                                        str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                        excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                                ELSE IF ttRptSelected.TextList = "SCRAP MSF" THEN ASSIGN
                                                                            str-tit3    = str-tit3 + "     SCRAP" + " "
                                                                            str-tit4    = str-tit4 + "       MSF" + " "
                                                                            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                            excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                                    ELSE IF ttRptSelected.TextList = "% OF TOT SCRAP" THEN ASSIGN
                                                                                str-tit3    = str-tit3 + " % OF TOT" + " "
                                                                                str-tit4    = str-tit4 + "    SCRAP" + " "
                                                                                str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                                excelheader = excelHeader + ttRptSelected.TextList + "," .
                                                                        ELSE IF ttRptSelected.TextList = "AVERAGE LINEAR FEET/HR" THEN ASSIGN
                                                                                    str-tit3    = str-tit3 + "AVERAGE LINEAR" + " "
                                                                                    str-tit4    = str-tit4 + "       FEET/HR" + " "
                                                                                    str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                                    excelheader = excelHeader + ttRptSelected.TextList + "," . 
                                                                            ELSE IF ttRptSelected.TextList = "TOTAL LINEAR FEET" THEN ASSIGN
                                                                                        str-tit3    = str-tit3 + " TOTAL LINEAR" + " "
                                                                                        str-tit4    = str-tit4 + "         FEET" + " "
                                                                                        str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                                        excelheader = excelHeader + ttRptSelected.TextList + "," .          
               
                                                                                ELSE 
                                                                                DO:

                                                                                    IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
                                                                                        THEN ASSIGN str-tit4    = str-tit4 + ttRptSelected.TextList + " "
                                                                                            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                                            str-tit3    = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
                                                                                            excelheader = excelHeader + ttRptSelected.TextList + "," .        
                                                                                    ELSE 
                                                                                        ASSIGN str-tit4    = str-tit4 + 
                 (IF ttRptSelected.HeadingFromLeft THEN
                     ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
                     ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                                                                                            str-tit5    = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                                                                                            str-tit3    = str-tit3 + FILL(" ",ttRptSelected.FieldLength) + " "
                                                                                            excelheader = excelHeader + ttRptSelected.TextList + ","
                                                                                            .        
                                                                                END.

        cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "DESCRIPTION,QUANTITY,MSF,QTY HOUR,RUN HOURS,MR HOURS,D/T CHGBL,TOTAL CHARGE,D/T No CHARGE,TOTAL HOURS,STD HOURS,EFFIC PERCENT,PERCENT UTILIZED,D/T PERCENT") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    VIEW FRAME r-top.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(cFileName).

        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END. 

    SESSION:SET-WAIT-STATE ("general").

    {pcrep/r-maceffN.i}

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
    END.

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

    PAGE.

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
            fi_file:SCREEN-VALUE = "c:\tmp\MachineEfficiency.csv".   
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

