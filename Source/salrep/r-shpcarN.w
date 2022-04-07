&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: salrep\r-shpcar.w

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

DEFINE VARIABLE v-per-rpt   AS LOG       FORMAT "PTD/YTD" INIT YES NO-UNDO.

DEFINE VARIABLE v-date      AS DATE      EXTENT 2 INIT [01/01/01, TODAY] NO-UNDO.
DEFINE VARIABLE v-carr      LIKE ar-inv.carrier EXTENT 2 INIT ["","zzzzz"] NO-UNDO.
DEFINE VARIABLE v-sumdet    AS LOG       FORMAT "Summary/Detail" INIT YES NO-UNDO.

DEFINE VARIABLE v-frst      AS LOG       EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-prof      LIKE ar-invl.amt NO-UNDO.
DEFINE VARIABLE v-gp        AS DECIMAL   FORMAT ">>9.99" NO-UNDO.
DEFINE VARIABLE v-year      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-inv-no    LIKE ar-invl.inv-no NO-UNDO.
DEFINE VARIABLE v-procat    LIKE itemfg.procat NO-UNDO.
DEFINE VARIABLE v-amt       LIKE ar-invl.amt NO-UNDO.
DEFINE VARIABLE v-cost      LIKE ar-invl.t-cost NO-UNDO.
DEFINE VARIABLE v-cust-part LIKE ar-invl.part-no NO-UNDO.

DEFINE VARIABLE v-tot-samt  AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-tot-cost  AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3 NO-UNDO.

DEFINE VARIABLE v-head      AS CHARACTER FORMAT "x(132)" EXTENT 3 NO-UNDO.

DEFINE TEMP-TABLE w-carr NO-UNDO
    FIELD carrier LIKE ar-inv.carrier
    FIELD samt    LIKE ar-invl.amt
    FIELD cost    LIKE ar-invl.amt.

DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.

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

ASSIGN 
    cTextListToSelect  = "Carrier,Cust #,Name,Part #,Invoice #,Cat.,Amount,Cost,GP" 
    cFieldListToSelect = "carr,cust,name,part,inv,cat,amt,cost,gp"  
    cFieldLength       = "7,8,30,32,9,5,12,12,8" 
    cFieldType         = "c,c,c,c,i,c,i,i,i"
    .



{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Carrier,Cust #,Name,Part #,Invoice #,Cat.,Amount,Cost,GP"     .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 rd_ptd begin_period begin_date ~
end_date begin_carr end_carr sl_avail sl_selected Btn_Def Btn_Add ~
Btn_Remove btn_Up btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_ptd rd_ptd begin_period begin_date ~
end_date begin_carr end_carr sl_avail sl_selected rd-dest fi_file ~
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

DEFINE VARIABLE begin_carr     AS CHARACTER FORMAT "X(5)" 
    LABEL "Beginning Carrier" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_period   AS INTEGER   FORMAT ">>":U INITIAL 99 
    LABEL "For Period?" 
    VIEW-AS FILL-IN 
    SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE end_carr       AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" 
    LABEL "Ending Carrier" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\ShippingCarrier.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 49 BY 1.

DEFINE VARIABLE lbl_ptd        AS CHARACTER FORMAT "X(256)":U INITIAL "PTD / YTD?" 
    VIEW-AS FILL-IN 
    SIZE 14 BY 1 NO-UNDO.

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
    SIZE 15 BY 5.19 NO-UNDO.

DEFINE VARIABLE rd_ptd         AS CHARACTER INITIAL "PTD" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "PTD", "PTD",
    "YTD", "YTD"
    SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 6.19.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 5.91.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.95 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.95 NO-UNDO.

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
    lbl_ptd AT ROW 2.33 COL 12 COLON-ALIGNED NO-LABELS
    rd_ptd AT ROW 2.33 COL 28 NO-LABELS
    begin_period AT ROW 3.71 COL 26 COLON-ALIGNED
    begin_date AT ROW 4.91 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 4.91 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_carr AT ROW 5.86 COL 26 COLON-ALIGNED HELP
    "Enter Beginning Carrier"
    end_carr AT ROW 5.86 COL 69 COLON-ALIGNED HELP
    "Enter Ending Carrier"
    sl_avail AT ROW 8.48 COL 4 NO-LABELS WIDGET-ID 26
    sl_selected AT ROW 8.48 COL 60.6 NO-LABELS WIDGET-ID 28
    Btn_Def AT ROW 8.57 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    Btn_Add AT ROW 9.71 COL 40.6 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 10.91 COL 40.6 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 12.1 COL 40.6 WIDGET-ID 40
    btn_down AT ROW 13.29 COL 40.6 WIDGET-ID 42
    lv-ornt AT ROW 15.29 COL 30 NO-LABELS
    lines-per-page AT ROW 15.29 COL 83 COLON-ALIGNED
    rd-dest AT ROW 15.57 COL 6 NO-LABELS
    lv-font-no AT ROW 16.24 COL 33 COLON-ALIGNED
    tb_excel AT ROW 16.24 COL 78 RIGHT-ALIGNED
    lv-font-name AT ROW 17.19 COL 27 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 18.38 COL 28.4
    fi_file AT ROW 19.57 COL 26.4 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 19.67 COL 92.4 RIGHT-ALIGNED
    tbAutoClose AT ROW 21.33 COL 28.4 WIDGET-ID 64
    btn-ok AT ROW 22.24 COL 28.4
    btn-cancel AT ROW 22.24 COL 48.4
    "Available Columns" VIEW-AS TEXT
    SIZE 20 BY .95 AT ROW 7.52 COL 4.2 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 7.67 COL 60.4 WIDGET-ID 44
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 14.76 COL 5
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    RECT-6 AT ROW 15.05 COL 4
    RECT-7 AT ROW 1.52 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 96 BY 23.33
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
        TITLE              = "Shipping Carrier Report"
        HEIGHT             = 23.33
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
    begin_carr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_period:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_carr:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_ptd IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_ptd:PRIVATE-DATA IN FRAME FRAME-A = "rd_ptd".

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
    rd_ptd:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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
ON END-ERROR OF C-Win /* Shipping Carrier Report */
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
ON WINDOW-CLOSE OF C-Win /* Shipping Carrier Report */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_carr C-Win
ON LEAVE OF begin_carr IN FRAME FRAME-A /* Beginning Carrier */
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


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON LEAVE OF begin_period IN FRAME FRAME-A /* For Period? */
    DO:
        ASSIGN {&self-name}.

        RUN show-period-dates.
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
        SESSION:SET-WAIT-STATE(""). 

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
                    {custom/asifax.i &begin_cust=begin_carr
                            &END_cust=END_carr
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
                END.
            WHEN 5 THEN 
                DO:
                    IF is-xprint-form THEN 
                    DO:
                        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                        {custom/asimail.i &TYPE = ''
                             &begin_cust= begin_carr
                             &END_cust=end_carr
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_carr
                                  &END_cust=end_carr
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
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


&Scoped-define SELF-NAME end_carr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_carr C-Win
ON LEAVE OF end_carr IN FRAME FRAME-A /* Ending Carrier */
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


&Scoped-define SELF-NAME rd_ptd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_ptd C-Win
ON VALUE-CHANGED OF rd_ptd IN FRAME FRAME-A
    DO:
        ASSIGN {&self-name}.

        IF rd_ptd EQ "YTD" THEN 
        DO:
            FIND FIRST period
                WHERE period.company EQ gcompany
                AND period.yr      EQ v-year
                NO-LOCK NO-ERROR.

            begin_date = IF AVAILABLE period THEN period.pst
            ELSE DATE(1,1,YEAR(TODAY)).

            DISPLAY begin_date WITH FRAME FRAME-A IN WINDOW C-Win.
        END.

        RUN show-period-dates.
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
        begin_date = TODAY
        end_date   = TODAY.

    FIND FIRST period
        WHERE period.company EQ gcompany
        AND period.pst     LE TODAY
        AND period.pend    GE TODAY
        AND period.pstat
        NO-LOCK NO-ERROR.

    IF AVAILABLE period THEN
        ASSIGN
            begin_period = period.pnum
            v-year       = period.yr
            begin_date   = period.pst.

    ELSE
        ASSIGN
            begin_period = MONTH(TODAY)
            v-year       = YEAR(TODAY).

    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "HR6" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
APPLY "entry" TO rd_ptd IN FRAME {&FRAME-NAME}.
END.
{methods/nowait.i}
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
    DISPLAY lbl_ptd rd_ptd begin_period begin_date end_date begin_carr end_carr 
        sl_avail sl_selected rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 rd_ptd begin_period begin_date end_date begin_carr 
        end_carr sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up 
        btn_down rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
/*    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/sa-comm.p 3/96 JLF */
    /* Commission Summary / Detail Report                                         */
    /* -------------------------------------------------------------------------- */
    SESSION:SET-WAIT-STATE ("general").
    /*{sys/form/r-top3w.f}*/

    DEFINE VARIABLE v-per-rpt      AS LOG       FORMAT "PTD/YTD" INIT YES NO-UNDO.
    DEFINE VARIABLE v-period       AS INTEGER   FORMAT ">9" INIT 1 NO-UNDO.
    DEFINE VARIABLE v-date         AS DATE      EXTENT 2 INIT [01/01/01, TODAY] NO-UNDO.
    DEFINE VARIABLE v-carr         LIKE ar-inv.carrier EXTENT 2 INIT ["","zzzzz"] NO-UNDO.
    DEFINE VARIABLE v-sumdet       AS LOG       FORMAT "Summary/Detail" INIT YES NO-UNDO. 
    DEFINE VARIABLE v-frst         AS LOG       EXTENT 2 NO-UNDO.
    DEFINE VARIABLE v-prof         LIKE ar-invl.amt NO-UNDO.
    DEFINE VARIABLE v-gp           AS DECIMAL   FORMAT ">>9.99" NO-UNDO.
    DEFINE VARIABLE v-year         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inv-no       LIKE ar-invl.inv-no NO-UNDO.
    DEFINE VARIABLE v-procat       LIKE itemfg.procat NO-UNDO.
    DEFINE VARIABLE v-amt          LIKE ar-invl.amt NO-UNDO.
    DEFINE VARIABLE v-cost         LIKE ar-invl.t-cost NO-UNDO.
    DEFINE VARIABLE v-cust-part    LIKE ar-invl.part-no NO-UNDO.

    DEFINE VARIABLE v-tot-samt     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-tot-cost     AS DECIMAL   FORMAT "->>>>>>>9.99" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-head         AS CHARACTER FORMAT "x(132)" EXTENT 3 NO-UNDO.
    DEFINE VARIABLE v-current-page AS INTEGER   NO-UNDO.

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

    {sys/form/r-top5DL3.f} 
    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.

    {custom/statusMsg.i "'Processing...'"} 

    FORMAT HEADER
        v-head[1]               /* skip */
        v-head[2]              /*  SKIP */
        v-head[3]

        WITH NO-LABELS NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 FRAME f-top PAGE-TOP. 

    {sa/sa-sls01.i}
    ASSIGN
        str-tit2  = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-per-rpt = rd_ptd EQ "PTD"
        v-period  = begin_period 
        v-date[1] = begin_date 
        v-date[2] = end_date
        v-carr[1] = begin_carr
        v-carr[2] = END_carr
        /* v-sumdet    = tb_sum-det */ .

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

        IF LOOKUP(ttRptSelected.TextList, "Qty Shipped,Freight,Sales Amt,Full Cost,Profit") <> 0    THEN
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
        PUT STREAM excel UNFORMATTED 
            '"' REPLACE(excelheader,',','","') '"' SKIP.
    END.

    IF td-show-parm THEN RUN show-param.

    DISPLAY WITH FRAME r-top.

    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
    FOR EACH cust
        WHERE cust.company EQ cocode
        NO-LOCK:

        FOR EACH ar-inv
            WHERE ar-inv.company  EQ cocode
            AND ar-inv.posted   EQ YES
            AND ar-inv.cust-no  EQ cust.cust-no
            AND ar-inv.inv-date GE v-date[1]
            AND ar-inv.inv-date LE v-date[2]
            AND ar-inv.carrier  GE v-carr[1]
            AND ar-inv.carrier  LE v-carr[2]
            NO-LOCK,

            EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
            AND ar-invl.misc EQ NO
            NO-LOCK

            TRANSACTION:

            {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

            CREATE report.
            ASSIGN
                report.term-id = v-term
                report.key-01  = ar-inv.carrier
                report.key-02  = cust.cust-no
                report.key-03  = STRING(ar-inv.inv-no,"999999")
                report.key-04  = ar-invl.i-no
                report.key-10  = "ar-invl"
                report.rec-id  = RECID(ar-invl).
        END.
    END.

    input-work:
    FOR EACH report
        WHERE report.term-id EQ v-term,

        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ report.key-02
        NO-LOCK

        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03 WITH FRAME detail:

      {custom/statusMsg.i "'Processing Customer # ' + string(cust.cust-no)"} 

        IF FIRST(report.key-01)    THEN v-frst[1] = YES.
        IF FIRST-OF(report.key-01) THEN v-frst[2] = YES.

        RELEASE ar-invl.
        RELEASE ar-cashl.

        IF report.key-10 EQ "ar-invl" THEN
            FIND ar-invl WHERE RECID(ar-invl) EQ report.rec-id NO-LOCK NO-ERROR.

        IF AVAILABLE ar-invl THEN 
        DO:
            RELEASE itemfg.
            IF NOT ar-invl.misc THEN
                FIND FIRST itemfg
                    WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ ar-invl.i-no
                    NO-LOCK NO-ERROR.

            ASSIGN
                v-inv-no    = ar-invl.inv-no
                v-procat    = IF ar-invl.misc THEN "MISC" ELSE
                     IF AVAILABLE itemfg THEN itemfg.procat ELSE "ARINV"
                v-amt       = ar-invl.amt
                v-cost      = ar-invl.t-cost
                v-cust-part = ar-invl.part-no.
        END.

        ELSE
            IF report.key-10 EQ "ar-cashl" THEN
                FIND ar-cashl WHERE RECID(ar-cashl) EQ report.rec-id NO-LOCK NO-ERROR.

        IF AVAILABLE ar-cashl THEN 
        DO:
            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

            IF AVAILABLE oe-retl THEN 
            DO:
                RELEASE itemfg.

                FIND FIRST ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ cust.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    AND ar-invl.i-no    EQ oe-retl.i-no
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    NO-LOCK NO-ERROR.

                IF AVAILABLE ar-invl THEN 
                DO:
                    IF NOT ar-invl.misc THEN
                        FIND FIRST itemfg
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no    EQ oe-retl.i-no
                            NO-LOCK NO-ERROR.

                    ASSIGN
                        v-inv-no = ar-invl.inv-no
                        v-procat = IF ar-invl.misc THEN "MISC" ELSE
                       IF AVAILABLE itemfg THEN itemfg.procat ELSE "CRMEMO"
                        v-amt    = (ar-cashl.amt-paid - ar-cashl.amt-disc) 
                        v-cost   = - (oe-retl.cost * oe-retl.tot-qty-return).
                END.
            END.
        END.

        IF v-cost EQ ? THEN v-cost = 0.

        ASSIGN
            v-prof        = v-amt - v-cost
            v-gp          = ROUND(v-prof / v-amt * 100,2)

            v-tot-samt[1] = v-tot-samt[1] + v-amt
            v-tot-cost[1] = v-tot-cost[1] + v-cost.

        IF v-gp EQ ? THEN v-gp = 0.

        IF FIRST-OF(report.key-01) AND NOT FIRST(report.key-01) THEN PAGE.

        /*  if not v-sumdet then
          DO: 
            display report.key-01           when first-of(report.key-01)
                                            format "x(5)"
                                            COLUMN-LABEL "Carrier"
                    space(4)
                    report.key-02           when first-of(report.key-02)
                                            COLUMN-LABEL "Cust #"
                    space(2)
                    cust.name               when first-of(report.key-02)
                                            format "x(22)"
                                            COLUMN-LABEL "Name"
                    space(2)
                    v-cust-part             COLUMN-LABEL "Part #"
                    space(2)
                    v-inv-no                COLUMN-LABEL "Invoice #"
                    space(2)
                    v-procat                COLUMN-LABEL "Cat."
                    space(2)
                    v-amt                   format "->>>>>>>9.99"
                                            COLUMN-LABEL "Amount"
                    space(2)
                    v-cost                  format "->>>>>>>9.99"
                                            COLUMN-LABEL "Cost"
                    space(2)
                    v-gp                    format "->>>9.99"
                                            COLUMN-LABEL "GP"
      
                with frame detail NO-BOX STREAM-IO width 132.
              DOWN WITH FRAME detail.
            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' IF first-of(report.key-01) THEN report.key-01
                       ELSE ""                                       '",'
                   '"' IF first-of(report.key-02) THEN report.key-02
                       ELSE ""                                       '",'
                   '"' IF first-of(report.key-02) THEN cust.NAME
                       ELSE ""                                       '",'
                   '"' v-cust-part                                   '",'
                   '"' v-inv-no                                      '",'
                   '"' v-procat                                      '",'
                   '"' STRING(v-amt,"->>>>>>>9.99")                  '",'
                   '"' STRING(v-cost,"->>>>>>>9.99")                 '",'
                   '"' STRING(v-gp,"->>>9.99")                       '",'
                   SKIP. */
        /* END. */

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "carr" THEN 
                    cVarValue = IF FIRST-OF(report.key-01) THEN report.key-01 ELSE "" .
                WHEN "cust" THEN 
                    cVarValue = IF FIRST-OF(report.key-02) THEN report.key-02 ELSE "" .
                WHEN "name" THEN 
                    cVarValue = IF FIRST-OF(report.key-02) THEN cust.NAME ELSE ""  .
                WHEN "part" THEN 
                    cVarValue = v-cust-part .                  
                WHEN "inv"  THEN 
                    cVarValue = STRING(v-inv-no) .                      
                WHEN "cat"  THEN 
                    cVarValue = v-procat    .                  
                WHEN "amt"  THEN 
                    cVarValue = STRING(v-amt,"->>>>>>>9.99") . 
                WHEN "cost" THEN 
                    cVarValue = STRING(v-cost,"->>>>>>>9.99") .
                WHEN "gp"   THEN 
                    cVarValue = STRING(v-gp,"->>>9.99") .      

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

        IF LAST-OF(report.key-02) THEN 
        DO:

            v-cost = (v-tot-samt[1] - v-tot-cost[1]) / v-tot-samt[1] * 100.

            IF v-cost = ? THEN v-cost = 0.

            /*   if v-sumdet then
               DO:
                 IF PAGE-NUMBER <> v-current-page THEN DO:
                     v-current-page = PAGE-NUMBER.
                     PUT "Carrier" SPACE(1) 
                         "Cust #  " SPACE(2)
                         "Name                                " SPACE(2)
                         "Amount       " SPACE(2)
                         "Cost     " SPACE(2)
                         "GP %    "
                         SKIP.
                     PUT "-----" SPACE(3) 
                         "--------" SPACE(2)
                         "------------------------------" SPACE(2)
                         " -----------" SPACE(2)
                         "    -------" SPACE(2)
                         " --------"
                         SKIP.
                 END.
                 PUT report.key-01         format "x(5)"
                         space(3)
                         report.key-02         
                         space(2)
                         cust.NAME             
                         space(2)
                         v-tot-samt[1]         
                         space(2)
                         v-tot-cost[1]         
                         space(2)
                         v-cost                format "->>>9.99"
         
                         SKIP.
                     /* with frame summary no-box STREAM-IO width 132. */
         
                 IF tb_excel THEN
                    PUT STREAM excel UNFORMATTED
                        '"' report.key-01                         '",'
                        '"' report.key-02                         '",'
                        '"' cust.NAME                                '",'
                        '"' ""                                       '",'
                        '"' ""                                       '",'
                        '"' ""                                       '",'
                        '"' STRING(v-tot-samt[1],"->>>>>>>9.99")     '",'
                        '"' STRING(v-tot-cost[1],"->>>>>>>9.99")     '",'
                        '"' STRING(v-cost,"->>>9.99")                '",'
                        SKIP.
               END. 
         
               else do: */
            FIND FIRST w-carr
                WHERE w-carr.carr EQ report.key-01
                NO-ERROR.
            IF NOT AVAILABLE w-carr THEN 
            DO:
                CREATE w-carr.
                w-carr.carr = report.key-01.
            END.

            ASSIGN
                w-carr.samt = w-carr.samt + v-tot-samt[1]
                w-carr.cost = w-carr.cost + v-tot-cost[1].
            /* end. */

            ASSIGN
                v-tot-samt[2] = v-tot-samt[2] + v-tot-samt[1]
                v-tot-cost[2] = v-tot-cost[2] + v-tot-cost[1]
                v-tot-samt[1] = 0
                v-tot-cost[1] = 0.
        END. 

        /*   if v-sumdet               and
              last-of(report.key-01) then do:
       
             v-cost = (v-tot-samt[2] - v-tot-cost[2]) / v-tot-samt[2] * 100.
       
             if v-cost = ? then v-cost = 0.
       
             if ((not v-frst[2]) and (not last(report.key-01))) or
                ((not v-frst[1]) and last(report.key-01))       then
             DO:
               display skip(1)
                       "Carrier Totals:"
                       space(35)
                       v-tot-samt[2]
                       space(2)
                       v-tot-cost[2]
                       space(2)
                       v-cost              format "->>>9.99"
                       skip(1)
       
                   with frame carrier-sum no-box no-labels STREAM-IO width 132.
       
               IF tb_excel THEN
                  PUT STREAM excel UNFORMATTED
                      SKIP(1)
                      '"' "Carrier Totals:"                        '",'
                      '"' ""                                       '",'
                      '"' ""                                       '",'
                      '"' ""                                       '",'
                      '"' ""                                       '",'
                      '"' ""                                       '",'
                      '"' STRING(v-tot-samt[2],"->>>>>>>9.99")     '",'
                      '"' STRING(v-tot-cost[2],"->>>>>>>9.99")     '",'
                      '"' STRING(v-cost,"->>>9.99")                '",'
                      SKIP(1).
             END.
       
             assign
              v-frst[1]     = no
              v-tot-samt[3] = v-tot-samt[3] + v-tot-samt[2]
              v-tot-cost[3] = v-tot-cost[3] + v-tot-cost[2]
              v-tot-samt[2] = 0
              v-tot-cost[2] = 0.
           end. */

        IF LAST-OF(report.key-02) THEN v-frst[2] = NO.

        DELETE report.
    END.  /* input-work */

    /* if not v-sumdet then do: */

    ASSIGN
        str-tit2  = "S A L E S   B Y   C A R R I E R  -  R E C A P"
        str-tit3  = (IF v-per-rpt THEN "P" ELSE "Y") +
                "TD (" + string(v-date[1]) + "-" + string(v-date[2]) + ")"

        v-head[2] = "Carrier                                           Total Sa" +
                 "les $        Cost $      GP %"
        v-head[3] = FILL("-",87)

        x         = (115 - length(str-tit2)) / 2
        str-tit2  = FILL(" ",x) + str-tit2
        x         = (132 - length(str-tit3)) / 2
        str-tit3  = FILL(" ",x) + str-tit3
        str-tit4  = "" 
        str-tit5  = ""   .

    PAGE.
    DISPLAY WITH FRAME f-top.
    ASSIGN
        v-tot-samt[3] = 0
        v-tot-cost[3] = 0.

    IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            SKIP(1)
            '"' "Carrier"                                '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' "Total Sales $"                          '",'
            '"' "Total Cost $"                           '",'
            '"' "Total GP %"                             '",'
            SKIP. 

    recap-work:

    FOR EACH w-carr
        BREAK BY w-carr.carr:

        ASSIGN
            v-cost        = (w-carr.samt - w-carr.cost) / w-carr.samt * 100

            v-tot-samt[3] = v-tot-samt[3] + w-carr.samt
            v-tot-cost[3] = v-tot-cost[3] + w-carr.cost.

        IF v-cost = ? THEN v-cost = 0.

        DISPLAY w-carr.carr         FORMAT "x(5)"
            SPACE(46)
            w-carr.samt         FORMAT "->>>>>>>9.99"
            SPACE(2)
            w-carr.cost         FORMAT "->>>>>>>9.99"
            SPACE(2)
            v-cost              FORMAT "->>>9.99"

            WITH FRAME carrier-det NO-BOX NO-LABELS STREAM-IO WIDTH 132.

        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                '"' w-carr.carr                              '",'
                '"' ""                                       '",'
                '"' ""                                       '",'
                '"' ""                                       '",'
                '"' ""                                       '",'
                '"' ""                                       '",'
                '"' STRING(w-carr.samt,"->>>>>>>9.99")     '",'
                '"' STRING(w-carr.cost,"->>>>>>>9.99")     '",'
                '"' STRING(v-cost,"->>>9.99")                '",'
                SKIP.
    END.  /* recap-work */
    /* end. */

    v-cost = (v-tot-samt[3] - v-tot-cost[3]) / v-tot-samt[3] * 100.

    IF v-cost = ? THEN v-cost = 0.

    DISPLAY SKIP(1)
        "Grand Totals:"
        SPACE(37)
        v-tot-samt[3]
        SPACE(2)
        v-tot-cost[3]
        SPACE(2)
        v-cost                FORMAT "->>>9.99"

        WITH FRAME grand-tot NO-BOX NO-LABELS STREAM-IO WIDTH 132.

    IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
            SKIP(1)
            '"' "Grand Totals:"                          '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' ""                                       '",'
            '"' STRING(v-tot-samt[3],"->>>>>>>9.99")     '",'
            '"' STRING(v-tot-cost[3],"->>>>>>>9.99")     '",'
            '"' STRING(v-cost,"->>>9.99")                '",'
            SKIP. 


    /* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE ("").
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
                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF rd_ptd EQ "PTD" THEN 
    DO:
        FIND FIRST period
            WHERE period.company EQ gcompany
            AND period.yr      EQ v-year
            AND period.pnum    EQ begin_period
            NO-LOCK NO-ERROR.

        IF AVAILABLE period THEN 
        DO:
            ASSIGN
                v-year     = period.yr
                begin_date = period.pst
                end_date   = IF period.pend LT TODAY THEN period.pend ELSE TODAY.

            DISPLAY begin_date end_date WITH FRAME FRAME-A IN WINDOW C-Win.
        END.

        ELSE
            IF LASTKEY NE -1 THEN 
            DO: 
                MESSAGE begin_period "is not a valid period. "
                    VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
    END.

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
            fi_file:SCREEN-VALUE = "c:\tmp\ShippingCarrier.csv".   
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

