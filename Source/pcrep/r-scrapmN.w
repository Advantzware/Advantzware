&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE tt-report 
    FIELD job-no  AS CHARACTER
    FIELD job-no2 AS INTEGER
    FIELD sheet   AS INTEGER 
    FIELD blanks  AS INTEGER
    FIELD scraps  AS INTEGER 
    FIELD shtmsf  AS INTEGER
    FIELD blnmsf  AS INTEGER
    FIELD scrpmsf AS INTEGER.

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

DEFINE STREAM s-temp.

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
    cTextListToSelect  = "Machine#,Shift,Job#,Form#,Shts Rec'd,Shts/Msf Rec'd,#Up," + 
                                               "FG Rec'd,FG/Msf Rec'd,Shts Waste,Shts Msf Waste,Wst Qty %,Wst Msf %,JobQty," +
                                               "JobMSF,Rec'd QtyVar,Rec'd  MsfVar,% Rec'd Qty,% Rec'd Msf"
    cFieldListToSelect = "mach,shift,job,form,no-sht,sht-msf,no-up," +
                                        "no-fg,no-msf,scrp-sht,scrp-msf,tot-scrp,tot-scrp-msf,job-qty," +
                                        "job-msf,rec-var,rec-var-msf,rece,rece-msf"
    cFieldLength       = "8,5,13,5,13,14,4," + "13,13,13,14,11,11,13," + "13,13,13,11,11"
    cFieldType         = "c,c,c,c,i,i,i," + "i,i,i,i,i,i,i," + "i,i,i,i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Machine#,Shift,Job#,Form#,Shts Rec'd,Shts/Msf Rec'd,#Up," + 
                                               "FG Rec'd,FG/Msf Rec'd,Shts Waste,Shts Msf Waste,Wst Qty %,Wst Msf %,JobQty," +
                                               "JobMSF,Rec'd QtyVar,Rec'd  MsfVar,% Rec'd Qty,% Rec'd Msf" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_jstat begin_mach end_mach ~
begin_shift end_shift begin_date end_date sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose ~
btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_jstat rd_jstat begin_mach end_mach ~
begin_shift end_shift lbl_beg-date lbl_end-date begin_date end_date ~
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

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_mach     AS CHARACTER FORMAT "X(6)" 
    LABEL "Beginning Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_shift    AS INTEGER   FORMAT ">>" INITIAL 1 
    LABEL "Beginning Shift" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_mach       AS CHARACTER FORMAT "X(6)" INITIAL "zzzzzz" 
    LABEL "Ending Machine" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_shift      AS INTEGER   FORMAT ">>" INITIAL 99 
    LABEL "Ending Shift" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ScrapReportByMachine.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 50 BY 1.

DEFINE VARIABLE lbl_beg-date   AS CHARACTER FORMAT "X(256)":U INITIAL "Beginning Job Close Date:" 
    VIEW-AS FILL-IN 
    SIZE 27 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_end-date   AS CHARACTER FORMAT "X(256)":U INITIAL "Ending Job Close Date:" 
    VIEW-AS FILL-IN 
    SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE lbl_jstat      AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?" 
    VIEW-AS FILL-IN 
    SIZE 13 BY .95 NO-UNDO.

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
    SIZE 15 BY 5.1 NO-UNDO.

DEFINE VARIABLE rd_jstat       AS CHARACTER INITIAL "All" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Open", "Open",
    "Closed", "Closed",
    "All", "All"
    SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.71.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.91.

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
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 15.4 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    lbl_jstat AT ROW 2.67 COL 34 COLON-ALIGNED NO-LABELS
    rd_jstat AT ROW 2.67 COL 49 NO-LABELS
    begin_mach AT ROW 3.86 COL 30.8 COLON-ALIGNED HELP
    "Enter Beginning Machine"
    end_mach AT ROW 3.86 COL 73 COLON-ALIGNED HELP
    "Enter Ending Machine"
    begin_shift AT ROW 4.81 COL 30.8 COLON-ALIGNED HELP
    "Enter Beginning Shift"
    end_shift AT ROW 4.81 COL 73 COLON-ALIGNED HELP
    "Enter Ending Shift"
    lbl_beg-date AT ROW 5.71 COL 6 NO-LABELS WIDGET-ID 2
    lbl_end-date AT ROW 5.71 COL 52 NO-LABELS WIDGET-ID 4
    begin_date AT ROW 5.76 COL 30.8 COLON-ALIGNED NO-LABELS
    end_date AT ROW 5.76 COL 73 COLON-ALIGNED HELP
    "Enter Ending Due Date" NO-LABELS
    sl_avail AT ROW 8.52 COL 4 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 8.52 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 8.52 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 9.52 COL 40.4 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 10.52 COL 40.4 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 11.57 COL 40.4 WIDGET-ID 40
    btn_down AT ROW 12.57 COL 40.4 WIDGET-ID 42
    lv-ornt AT ROW 14.62 COL 31 NO-LABELS
    lines-per-page AT ROW 14.62 COL 84 COLON-ALIGNED
    rd-dest AT ROW 14.95 COL 6 NO-LABELS
    lv-font-no AT ROW 15.29 COL 35 COLON-ALIGNED
    tb_excel AT ROW 15.43 COL 77 RIGHT-ALIGNED
    lv-font-name AT ROW 16 COL 29 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 17.67 COL 27.8
    fi_file AT ROW 18.62 COL 25.8 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.71 COL 92.6 RIGHT-ALIGNED
    tbAutoClose AT ROW 20.38 COL 27.8 WIDGET-ID 78
    btn-ok AT ROW 21.29 COL 27.8
    btn-cancel AT ROW 21.29 COL 47.8
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 7.67 COL 60.2 WIDGET-ID 44
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 14.29 COL 5
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 7.67 COL 4.4 WIDGET-ID 38
    RECT-6 AT ROW 14.57 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 22.48
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
        TITLE              = "Scrap Report"
        HEIGHT             = 22.48
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
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_mach:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_shift:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_beg-date IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl_end-date IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lbl_jstat IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_jstat:PRIVATE-DATA IN FRAME FRAME-A = "rd_jstat".

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
    rd_jstat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Scrap Report */
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
ON WINDOW-CLOSE OF C-Win /* Scrap Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A
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


&Scoped-define SELF-NAME begin_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_shift C-Win
ON LEAVE OF begin_shift IN FRAME FRAME-A /* Beginning Shift */
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
        IF rd-dest EQ 3 THEN
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
                    ELSE DO:
                        OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
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
ON LEAVE OF end_date IN FRAME FRAME-A
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


&Scoped-define SELF-NAME end_shift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_shift C-Win
ON LEAVE OF end_shift IN FRAME FRAME-A /* Ending Shift */
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


&Scoped-define SELF-NAME rd_jstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_jstat C-Win
ON VALUE-CHANGED OF rd_jstat IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.

        IF rd_jstat:SCREEN-VALUE = "All" THEN
            ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Start/Close:" 
                lbl_end-date:SCREEN-VALUE = "Ending Job Start/Close:" .
        ELSE  IF rd_jstat:SCREEN-VALUE = "Open" THEN
                ASSIGN lbl_beg-date:SCREEN-VALUE = "  Beginning Job Start Date:" 
                    lbl_end-date:SCREEN-VALUE = " Ending Job Start date:" .
            ELSE
                ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Close Date:" 
                    lbl_end-date:SCREEN-VALUE = "Ending Job Close date:" .

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
        begin_date = DATE (1,1,YEAR(TODAY))
        END_date   = DATE (12,31,YEAR(TODAY)).

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
    {sys/inc/reportsConfigNK1.i "DR12" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.

IF rd_jstat:SCREEN-VALUE = "All" THEN
    ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Start/Close:" 
        lbl_end-date:SCREEN-VALUE = "Ending Job Start/Close:" .
ELSE  IF rd_jstat:SCREEN-VALUE = "Open" THEN
        ASSIGN lbl_beg-date:SCREEN-VALUE = "  Beginning Job Start Date:" 
            lbl_end-date:SCREEN-VALUE = " Ending Job Start date:" .
    ELSE
        ASSIGN lbl_beg-date:SCREEN-VALUE = " Beginning Job Close Date:" 
            lbl_end-date:SCREEN-VALUE = "Ending Job Close date:" .

APPLY "entry" TO begin_mach.
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
    DISPLAY lbl_jstat rd_jstat begin_mach end_mach begin_shift end_shift 
        lbl_beg-date lbl_end-date begin_date end_date sl_avail sl_selected 
        rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 rd_jstat begin_mach end_mach begin_shift end_shift 
        begin_date end_date sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
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
            fi_file:SCREEN-VALUE = "c:\tmp\ScrapReportByMachine.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ pc/rep/scrap1.p 04/01 JLF */
    /* Scrap Report - by Machine                                                  */
    /* ---------------------------------------------------------------------------*/
    /*{sys/form/r-top3w.f}*/

    DEFINE BUFFER bjob-mch FOR job-mch.
    DEFINE BUFFER bf-eb    FOR eb. 

    DEFINE VARIABLE v-fmch         LIKE mch-act.m-code.
    DEFINE VARIABLE v-tmch         LIKE v-fmch INIT "zzzzzz".
    DEFINE VARIABLE v-fshf         LIKE mch-act.shift FORMAT ">>" INIT 1.
    DEFINE VARIABLE v-tshf         LIKE v-fshf INIT 99.
    DEFINE VARIABLE v-fdat         LIKE job.start-date FORMAT "99/99/9999" INIT 01/01/0001.
    DEFINE VARIABLE v-tdat         LIKE v-fdat INIT 12/31/9999.
    DEFINE VARIABLE v-q-m          AS LOG       FORMAT "Qty/MSF" INIT YES.
    DEFINE VARIABLE v-stat         AS CHARACTER FORMAT "!" INIT "O".

    DEFINE VARIABLE v-up           AS INTEGER.
    DEFINE VARIABLE v-sheetup      AS DECIMAL.
    DEFINE VARIABLE v-on           AS INTEGER.
    DEFINE VARIABLE v-out          AS INTEGER.
    DEFINE VARIABLE v-qty          AS DECIMAL.
    DEFINE VARIABLE v-qty2         AS DECIMAL.

    DEFINE VARIABLE v-sheets       AS DECIMAL   FORMAT "->>>>,>>>,>>9" EXTENT 4.
    DEFINE VARIABLE v-sht-msf      AS DECIMAL   FORMAT "->>>>,>>>,>>9" EXTENT 4.
    DEFINE VARIABLE v-blanks       AS DECIMAL   FORMAT "->>>>,>>>,>>9" EXTENT 4.
    DEFINE VARIABLE v-blnk-msf     AS DECIMAL   FORMAT "->>>>,>>>,>>9" EXTENT 4.
    DEFINE VARIABLE v-scraps       AS DECIMAL   FORMAT "->>>>,>>>,>>9" EXTENT 4.
    DEFINE VARIABLE v-scrp-msf     AS DECIMAL   FORMAT "->>>>,>>>,>>9" EXTENT 4.
    DEFINE VARIABLE v-job-qty      AS DECIMAL   FORMAT "->>>>,>>>,>>9".
    DEFINE VARIABLE v-job-msf      AS DECIMAL   FORMAT "->>>>,>>>,>>9".
    DEFINE VARIABLE v-pct          AS DECIMAL   FORMAT "->>,>>9.999" EXTENT 2.
    DEFINE VARIABLE v-pct-msf      AS DECIMAL   FORMAT "->>,>>9.999" EXTENT 2.
    DEFINE VARIABLE v-forms        AS INTEGER.
    DEFINE VARIABLE v-first-24     AS CHARACTER FORMAT "x(24)".

    DEFINE VARIABLE v-lab          AS CHARACTER FORMAT "x(13)" EXTENT 4.

    DEFINE VARIABLE str_buffa      AS CHARACTER NO-UNDO.
    /*def var v-hdr       as   char init
    "Machine#,Shift,Job#,Form#,#ofSheetsReceived,#Up,#ofFGReceived,#ScrapSheets,%TotScrap,JobQty,RecVariance,%Received".
    def var v-hdr1      as   char init
    "Machine#,Shift,Job#,Form#,SheetsReceivedMSF,#Up,FGReceivedMSF,ScrapMSF,%TotScrap,JobMSF,RecVariance,%Received". */
    DEFINE VARIABLE v-comma        AS CHARACTER FORMAT "x" INIT "," NO-UNDO.

    DEFINE VARIABLE cDisplay       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelDisplay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE str-tit4       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE str-tit5       AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE str-line       AS cha       FORM "x(300)" NO-UNDO.

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    FOR EACH tt-report NO-LOCK :
        DELETE tt-report.
    END.


    SESSION:SET-WAIT-STATE ("general").

    ASSIGN
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-stat   = SUBSTR(rd_jstat,1,1)
        v-fmch   = begin_mach
        v-tmch   = end_mach
        v-fshf   = begin_shift
        v-tshf   = end_shift
        /*v-q-m      = rd_print EQ "Qty"*/
        v-fdat   = begin_date
        v-tdat   = end_date. 

    IF v-q-m THEN
        ASSIGN
            v-lab[1] = "  # of Sheets"
            v-lab[2] = "      # of FG"
            v-lab[3] = "#Scrap Sheets"
            v-lab[4] = "      Job Qty".

    ELSE
        ASSIGN
            v-lab[1] = "MSF of Sheets"
            v-lab[2] = "    MSF of FG"
            v-lab[3] = "    Scrap MSF"
            v-lab[4] = "      Job MSF".

        {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.


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

        IF LOOKUP(ttRptSelected.TextList, "Shts Rec'd,Shts/Msf Rec'd,FG Rec'd,FG/Msf Rec'd,Shts Waste,Shts Msf Waste,Wst Qty %,Wst Msf %") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM s-temp TO VALUE(cFileName).
        /* excelheader = "MACHINE,PROMISED/JOB DATE,CUSTOMER,CUSTOMER PART #,JOB #,"
                      + "TOTAL KICKS,KICKS REMAINING,MSF BALANCE,SHEET SIZE,"
                      + "BOARD RECEIVED". */
        PUT STREAM s-temp UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    VIEW FRAME r-top.


    FOR EACH mch-act
        WHERE mch-act.company EQ cocode
        AND mch-act.m-code  GE v-fmch
        AND mch-act.m-code  LE v-tmch
        AND mch-act.shift   GE v-fshf
        AND mch-act.shift   LE v-tshf
        USE-INDEX operation NO-LOCK,

        FIRST job
        WHERE job.company            EQ cocode
        AND job.job                EQ mch-act.job
        AND job.job-no             EQ mch-act.job-no
        AND job.job-no2            EQ mch-act.job-no2
        AND (v-stat                EQ "A"                   OR
        (v-stat               EQ "O" AND job.opened)   OR
        (v-stat               EQ "C" AND NOT job.opened))
        AND ((job.close-date       GE v-fdat AND
        job.close-date       LE v-tdat AND
        NOT job.opened)                       OR
        (job.start-date       GE v-fdat AND
        job.start-date       LE v-tdat AND
        job.opened))
        USE-INDEX job NO-LOCK

        BREAK BY mch-act.m-code
        BY mch-act.shift
        BY mch-act.job
        BY mch-act.job-no
        BY mch-act.job-no2
        BY mch-act.frm
        BY mch-act.blank-no:

        {custom/statusMsg.i "'Processing Machine Code ' + string(mch-act.m-code)"} 

        IF FIRST-OF(mch-act.m-code) THEN PAGE.        

        FIND FIRST est
            WHERE est.company EQ job.company
            AND est.est-no  EQ job.est-no
            NO-LOCK NO-ERROR.

        IF LAST-OF(mch-act.blank-no) THEN 
        DO:
            IF NOT v-q-m THEN
                FIND FIRST itemfg
                    WHERE itemfg.company EQ mch-act.company
                    AND itemfg.i-no EQ mch-act.i-no
                    NO-LOCK NO-ERROR.
            RUN fg/GetProductionQty.p (INPUT mch-act.company,
                INPUT mch-act.job-no,
                INPUT mch-act.job-no2,
                INPUT mch-act.i-no,
                INPUT NO,
                OUTPUT v-blanks[1]).
            v-blnk-msf[1] = v-blanks[1] * (IF AVAILABLE itemfg
                THEN itemfg.t-sqft
                ELSE 1) / 1000.
            FOR EACH job-hdr
                WHERE job-hdr.company   EQ cocode
                AND job-hdr.job       EQ mch-act.job
                AND job-hdr.job-no    EQ mch-act.job-no
                AND job-hdr.job-no2   EQ mch-act.job-no2
                AND job-hdr.frm       EQ mch-act.frm
                AND (job-hdr.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0)
                NO-LOCK:

                v-job-qty = v-job-qty + job-hdr.qty.

                FIND FIRST itemfg
                    WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ job-hdr.i-no
                    NO-LOCK NO-ERROR.
                v-job-msf = v-job-msf +
                    (job-hdr.qty * (IF AVAILABLE itemfg THEN itemfg.t-sqft
                    ELSE 1) / 1000).              
            END.

            IF AVAILABLE est THEN
                FOR EACH eb
                    WHERE eb.company   EQ est.company
                    AND eb.est-no    EQ est.est-no
                    AND eb.form-no   EQ mch-act.frm
                    AND (eb.blank-no EQ mch-act.blank-no OR mch-act.blank-no EQ 0)
                    NO-LOCK:

                    v-up = v-up + eb.num-up.
                END.

            ELSE v-up = v-up + 1.
        END.

        IF LAST-OF(mch-act.frm) THEN 
        DO:
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ mch-act.job
                AND job-mat.job-no  EQ mch-act.job-no
                AND job-mat.job-no2 EQ mch-act.job-no2
                AND job-mat.frm     EQ mch-act.frm
                USE-INDEX seq-idx NO-LOCK,

                FIRST item
                WHERE item.company  EQ cocode
                AND item.i-no     EQ job-mat.i-no
                AND item.mat-type EQ "B"
                NO-LOCK,

                EACH mat-act
                WHERE mat-act.company EQ cocode
                AND mat-act.job     EQ job-mat.job
                AND mat-act.job-no  EQ job-mat.job-no
                AND mat-act.job-no2 EQ job-mat.job-no2
                AND mat-act.s-num   EQ job-mat.frm
                AND mat-act.b-num   EQ job-mat.blank-no
                AND mat-act.i-no    EQ job-mat.i-no
                USE-INDEX job NO-LOCK:

                RUN sys/ref/convquom.p(job-mat.qty-uom, "EA",
                    job-mat.basis-w, job-mat.len,
                    job-mat.wid, item.s-dep,
                    mat-act.qty, OUTPUT v-qty).

                v-sheets[1] = v-sheets[1] + v-qty.
                v-sheetup   = job-mat.n-up .

                RUN sys/ref/convquom.p(job-mat.qty-uom, "MSF",
                    job-mat.basis-w, job-mat.len,
                    job-mat.wid, item.s-dep,
                    mat-act.qty, OUTPUT v-qty2).

                v-sht-msf[1] = v-sht-msf[1] + v-qty2.
            END.

            IF v-sheets[1] EQ 0 THEN      /* get sheets from slitter */
                RUN sys/inc/slitshts.p (ROWID(job),mch-act.frm, OUTPUT v-sheets[1]).
            IF v-sht-msf[1] EQ 0 THEN      /* get sheets from slitter */
                RUN sys/inc/slitshts.p (ROWID(job),mch-act.frm, OUTPUT v-sht-msf[1]).

            RELEASE ef.

            IF AVAILABLE est THEN
                FIND FIRST ef
                    WHERE ef.company EQ est.company
                    AND ef.est-no  EQ est.est-no
                    AND ef.form-no EQ mch-act.frm
                    NO-LOCK NO-ERROR.

                    {sys/inc/roundup.i v-sheets[1]}

            IF v-up EQ 0 THEN v-up = 1.

            IF AVAILABLE ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).

            v-up  = v-up * v-out.

            IF v-sheetup = 0 THEN
                ASSIGN v-sheetup = 1.

            /*2 pc box test*/
            IF AVAILABLE est AND est.est-type EQ 2 OR est.est-type EQ 6 
                THEN 
            DO:
                FIND FIRST eb 
                    WHERE eb.company EQ est.company 
                    AND eb.est-no = est.est-no 
                    AND eb.form-no = 0
                    NO-LOCK NO-ERROR.
                IF AVAILABLE eb THEN
                    FIND FIRST bf-eb WHERE bf-eb.company = eb.company
                        AND bf-eb.est-no = eb.est-no
                        AND bf-eb.form-no = 1
                        AND bf-eb.stock-no EQ eb.stock-no
                        AND bf-eb.part-no EQ eb.part-no
                        AND recid(bf-eb) <>  RECID(eb) 
                        NO-LOCK NO-ERROR.
                IF AVAILABLE bf-eb AND bf-eb.cust-% > 0 THEN
                    v-sheetup =  v-sheetup / bf-eb.cust-%.
                ELSE IF AVAILABLE bf-eb AND bf-eb.quantityPerSet > 0 THEN
                        v-sheetup =  v-sheetup / bf-eb.quantityPerSet.
            END. /*2 pc box test*/

            ASSIGN
                v-scraps[1]   = v-sheets[1] - (v-blanks[1] / (v-sheetup))
                v-scrp-msf[1] = v-sht-msf[1] - (v-blnk-msf[1] / 1)
                v-pct[1]      = v-scraps[1] / v-sheets[1] * 100
                v-pct-msf[1]  = v-scrp-msf[1] / v-sht-msf[1] * 100
                v-pct[2]      = v-blanks[1] / v-job-qty * 100.
            v-pct-msf[2]    = v-blnk-msf[1] / v-job-msf * 100.

            IF v-pct[1] EQ ? THEN v-pct[1] = 0.
            IF v-pct-msf[1] EQ ? THEN v-pct-msf[1] = 0.
            IF v-pct[2] EQ ? THEN v-pct[2] = 0.
            IF v-pct-msf[2] EQ ? THEN v-pct-msf[2] = 0.

            v-first-24 = STRING(mch-act.m-code,"x(8)") + " "         +
                string(mch-act.shift,">>")    + fill(" ",4) +
                string(TRIM(job.job-no) + "-" +
                TRIM(string(job.job-no2,">99")),"x(9)").


            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "mach"        THEN 
                        cVarValue = STRING(mch-act.m-code) .
                    WHEN "shift"        THEN 
                        cVarValue = STRING(mch-act.shift,">>") .
                    WHEN "job"            THEN 
                        cVarValue = STRING((job.job-no) + "-" + string(job.job-no2,"999"))  .
                    WHEN "form"       THEN 
                        cVarValue = STRING(mch-act.frm,">>>>>").

                    WHEN "no-sht"         THEN 
                        cVarValue = STRING(v-sheets[1],"->>>>,>>>,>>9") .
                    WHEN "sht-msf"        THEN 
                        cVarValue = STRING(v-sht-msf[1],"->>>>,>>>,>>9") .

                    WHEN "no-up"     THEN 
                        cVarValue = STRING(v-sheetup,">9.9").

                    WHEN "no-fg"          THEN 
                        cVarValue = STRING(v-blanks[1],"->>>>,>>>,>>9") .
                    WHEN "no-msf"         THEN 
                        cVarValue = STRING(v-blnk-msf[1],"->>>>,>>>,>>9") .

                    WHEN "scrp-sht"       THEN 
                        cVarValue = STRING(v-scraps[1],"->>>>,>>>,>>9") .
                    WHEN "scrp-msf"       THEN 
                        cVarValue = STRING(v-scrp-msf[1],"->>>>,>>>,>>9") .

                    WHEN "tot-scrp"       THEN 
                        cVarValue = STRING(v-pct[1],"->>,>>9.999") .
                    WHEN "tot-scrp-msf"       THEN 
                        cVarValue = STRING(v-pct-msf[1],"->>,>>9.999") .

                    WHEN "job-qty"        THEN 
                        cVarValue = STRING(v-job-qty,"->>,>>9.999") .
                    WHEN "job-msf"        THEN 
                        cVarValue = STRING(v-job-msf,"->>,>>9.999") .

                    WHEN "rec-var"        THEN 
                        cVarValue = STRING((v-job-qty - v-blanks[1]),"->>,>>>,>>9.9<<<") .
                    WHEN "rec-var-msf"        THEN 
                        cVarValue = STRING((v-job-msf - v-blnk-msf[1]),"->>,>>>,>>9.9<<<") .

                    WHEN "rece"           THEN 
                        cVarValue = STRING(v-pct[2],"->>,>>9.999") .
                    WHEN "rece-msf"           THEN 
                        cVarValue = STRING(v-pct-msf[2],"->>,>>9.999") .


                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            ASSIGN
                v-sheets[2]   = v-sheets[2] + v-sheets[1]
                v-sht-msf[2]  = v-sht-msf[2] + v-sht-msf[1]
                v-blanks[2]   = v-blanks[2] + v-blanks[1]
                v-blnk-msf[2] = v-blnk-msf[2] + v-blnk-msf[1]
                v-scraps[2]   = v-scraps[2] + v-scraps[1]
                v-scrp-msf[2] = v-scrp-msf[2] + v-scrp-msf[1]

                v-sheets[1]   = 0
                v-sht-msf[1]  = 0
                v-blanks[1]   = 0
                v-blnk-msf[1] = 0
                v-scraps[1]   = 0
                v-scrp-msf[1] = 0
                v-job-qty     = 0
                v-up          = 0
                v-sheetup     = 0
                v-forms       = v-forms + 1.
        END.

        IF LAST-OF(mch-act.job-no2) THEN 
        DO:
            IF v-forms GT 1 THEN 
            DO:
                /*underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.*/

                v-pct[1] = v-scraps[2] / v-sheets[2] * 100.
                v-pct-msf[1] = v-scrp-msf[2] / v-sht-msf[2] * 100.

                IF v-pct[1] EQ ? THEN v-pct[1] = 0.
                IF v-pct-msf[1] EQ ? THEN v-pct-msf[1] = 0.


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
                        WHEN "mach"        THEN 
                            cVarValue = "" .
                        WHEN "shift"        THEN 
                            cVarValue = "" .
                        WHEN "job"            THEN 
                            cVarValue = "" .
                        WHEN "form"       THEN 
                            cVarValue = "" .

                        WHEN "no-sht"         THEN 
                            cVarValue = STRING(v-sheets[2],"->>>>,>>>,>>9") .
                        WHEN "sht-msf"        THEN 
                            cVarValue = STRING(v-sht-msf[2],"->>>>,>>>,>>9") .

                        WHEN "no-up"     THEN 
                            cVarValue = "" .

                        WHEN "no-fg"          THEN 
                            cVarValue = STRING(v-blanks[2],"->>>>,>>>,>>9") .
                        WHEN "no-msf"         THEN 
                            cVarValue = STRING(v-blnk-msf[2],"->>>>,>>>,>>9") .

                        WHEN "scrp-sht"       THEN 
                            cVarValue = STRING(v-scraps[2],"->>>>,>>>,>>9") .
                        WHEN "scrp-msf"       THEN 
                            cVarValue = STRING(v-scrp-msf[2],"->>>>,>>>,>>9") .

                        WHEN "tot-scrp"       THEN 
                            cVarValue = STRING(v-pct[1],"->>,>>9.999") .
                        WHEN "tot-scrp-msf"       THEN 
                            cVarValue = STRING(v-pct-msf[1],"->>,>>9.999") .

                        WHEN "job-qty"        THEN 
                            cVarValue = "" .
                        WHEN "job-msf"        THEN 
                            cVarValue = "" .

                        WHEN "rec-var"        THEN 
                            cVarValue = "" .
                        WHEN "rec-var-msf"        THEN 
                            cVarValue = "" .

                        WHEN "rece"           THEN 
                            cVarValue = "" .
                        WHEN "rece-msf"           THEN 
                            cVarValue = "" .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED 
                    "   Job Totals *" SUBSTRING(cDisplay,16,300) SKIP.
            END.

            PUT SKIP(1).

            CREATE tt-report .
            ASSIGN 
                tt-report.job-no  = mch-act.job-no 
                tt-report.job-no2 = mch-act.job-no2 
                tt-report.sheet   = v-sheets[2] 
                tt-report.shtmsf  = v-sht-msf[2]
                tt-report.blanks  = v-blanks[2]  
                tt-report.blnmsf  = v-blnk-msf[2]  
                tt-report.scraps  = v-scraps[2]
                tt-report.scrpmsf = v-scrp-msf[2] .

            ASSIGN
                v-sheets[3]   = v-sheets[3] + v-sheets[2]
                v-sht-msf[3]  = v-sht-msf[3] + v-sht-msf[2]
                v-blanks[3]   = v-blanks[3] + v-blanks[2]
                v-blnk-msf[3] = v-blnk-msf[3] + v-blnk-msf[2]
                v-scraps[3]   = v-scraps[3] + v-scraps[2]
                v-scrp-msf[3] = v-scrp-msf[3] + v-scrp-msf[2]

                v-sheets[2]   = 0
                v-sht-msf[2]  = 0
                v-blanks[2]   = 0
                v-blnk-msf[2] = 0
                v-scraps[2]   = 0
                v-scrp-msf[2] = 0
                v-forms       = 0.
        END.

        IF LAST-OF(mch-act.shift) THEN 
        DO:

            v-pct[1] = v-scraps[3] / v-sheets[3] * 100.
            v-pct-msf[1] = v-scrp-msf[3] / v-sht-msf[3] * 100.

            IF v-pct[1] EQ ? THEN v-pct[1] = 0.
            IF v-pct-msf[1] EQ ? THEN v-pct-msf[1] = 0.


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
                    WHEN "mach"        THEN 
                        cVarValue = "" .
                    WHEN "shift"        THEN 
                        cVarValue = "" .
                    WHEN "job"            THEN 
                        cVarValue = "" .
                    WHEN "form"       THEN 
                        cVarValue = "" .

                    WHEN "no-sht"         THEN 
                        cVarValue = STRING(v-sheets[3],"->>>>,>>>,>>9") .
                    WHEN "sht-msf"        THEN 
                        cVarValue = STRING(v-sht-msf[3],"->>>>,>>>,>>9") .

                    WHEN "no-up"     THEN 
                        cVarValue = "" .

                    WHEN "no-fg"          THEN 
                        cVarValue = STRING(v-blanks[3],"->>>>,>>>,>>9") .
                    WHEN "no-msf"         THEN 
                        cVarValue = STRING(v-blnk-msf[3],"->>>>,>>>,>>9") .

                    WHEN "scrp-sht"       THEN 
                        cVarValue = STRING(v-scraps[3],"->>>>,>>>,>>9") .
                    WHEN "scrp-msf"       THEN 
                        cVarValue = STRING(v-scrp-msf[3],"->>>>,>>>,>>9") .

                    WHEN "tot-scrp"       THEN 
                        cVarValue = STRING(v-pct[1],"->>,>>9.999") .
                    WHEN "tot-scrp-msf"       THEN 
                        cVarValue = STRING(v-pct-msf[1],"->>,>>9.999") .

                    WHEN "job-qty"        THEN 
                        cVarValue = "" .
                    WHEN "job-msf"        THEN 
                        cVarValue = "" .

                    WHEN "rec-var"        THEN 
                        cVarValue = "" .
                    WHEN "rec-var-msf"        THEN 
                        cVarValue = "" .

                    WHEN "rece"           THEN 
                        cVarValue = "" .
                    WHEN "rece-msf"           THEN 
                        cVarValue = "" .


                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "   Shift Totals *" SUBSTRING(cDisplay,18,300) SKIP.

            PUT SKIP(1).

            ASSIGN

                v-sheets[3]   = 0
                v-sht-msf[3]  = 0
                v-blanks[3]   = 0
                v-blnk-msf[3] = 0
                v-scraps[3]   = 0
                v-scrp-msf[3] = 0   .
        END.

        IF LAST-OF(mch-act.m-code) THEN 
        DO:
            /*underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap.
            underline v-sheets[1] v-blanks[1] v-scraps[1] v-pct[1] with frame scrap. */

            FOR EACH tt-report NO-LOCK
                BREAK BY tt-report.job-no
                BY tt-report.job-no2:
                IF FIRST-OF(tt-report.job-no2) THEN
                    ASSIGN
                        v-sheets[4]   = v-sheets[4] + tt-report.sheet 
                        v-sht-msf[4]  = v-sht-msf[4] + tt-report.shtmsf 
                        v-blanks[4]   = v-blanks[4] + tt-report.blanks 
                        v-blnk-msf[4] = v-blnk-msf[4] + tt-report.blnmsf 
                        v-scraps[4]   = v-scraps[4] + tt-report.scraps 
                        v-scrp-msf[4] = v-scrp-msf[4] + tt-report.scrpmsf .
            END.

            v-pct[1] = v-scraps[4] / v-sheets[4] * 100.
            v-pct-msf[1] = v-scrp-msf[4] / v-sht-msf[4] * 100.

            IF v-pct[1] EQ ? THEN v-pct[1] = 0.
            IF v-pct-msf[1] EQ ? THEN v-pct-msf[1] = 0.


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
                    WHEN "mach"        THEN 
                        cVarValue = "" .
                    WHEN "shift"        THEN 
                        cVarValue = "" .
                    WHEN "job"            THEN 
                        cVarValue = "" .
                    WHEN "form"       THEN 
                        cVarValue = "" .

                    WHEN "no-sht"         THEN 
                        cVarValue = STRING(v-sheets[4],"->>>>,>>>,>>9") .
                    WHEN "sht-msf"        THEN 
                        cVarValue = STRING(v-sht-msf[4],"->>>>,>>>,>>9") .

                    WHEN "no-up"     THEN 
                        cVarValue = "" .

                    WHEN "no-fg"          THEN 
                        cVarValue = STRING(v-blanks[4],"->>>>,>>>,>>9") .
                    WHEN "no-msf"         THEN 
                        cVarValue = STRING(v-blnk-msf[4],"->>>>,>>>,>>9") .

                    WHEN "scrp-sht"       THEN 
                        cVarValue = STRING(v-scraps[4],"->>>>,>>>,>>9") .
                    WHEN "scrp-msf"       THEN 
                        cVarValue = STRING(v-scrp-msf[4],"->>>>,>>>,>>9") .

                    WHEN "tot-scrp"       THEN 
                        cVarValue = STRING(v-pct[1],"->>,>>9.999") .
                    WHEN "tot-scrp-msf"       THEN 
                        cVarValue = STRING(v-pct-msf[1],"->>,>>9.999") .

                    WHEN "job-qty"        THEN 
                        cVarValue = "" .
                    WHEN "job-msf"        THEN 
                        cVarValue = "" .

                    WHEN "rec-var"        THEN 
                        cVarValue = "" .
                    WHEN "rec-var-msf"        THEN 
                        cVarValue = "" .

                    WHEN "rece"           THEN 
                        cVarValue = "" .
                    WHEN "rece-msf"           THEN 
                        cVarValue = "" .


                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED 
                "   Machine Totals *" SUBSTRING(cDisplay,20,300) SKIP.

            FOR EACH tt-report NO-LOCK :
                DELETE tt-report.
            END.

            PUT SKIP(1).

            ASSIGN
                v-sheets[4]   = 0
                v-sht-msf[4]  = 0
                v-blanks[4]   = 0
                v-blnk-msf[4] = 0
                v-scraps[4]   = 0
                v-scrp-msf[4] = 0    .
        END.
    END.

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM s-temp CLOSE.
    END.

    SESSION:SET-WAIT-STATE("").

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).  

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

