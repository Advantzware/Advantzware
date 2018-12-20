&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  File: jcrep\r-brdreN.w
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

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.


ASSIGN 
    cTextListToSelect  = "Job #,Item ID,Item Name,Category,Cust Part #,Style,Job Qty,Estimate,Form,Blank,Order,Customer,Customer Name," + 
                           "Calc Tim,Calc By,Std Prep Material,Std Misc Material,Std Prep Labor,Std Misc Labor,Std Direct Material,Std Direct Labor," +
                           "Std Var Overhead,Std Fixed Overhead,Std Total Factory,Std Commission,Std Freight,Std Full Cost,Std Gross Profit,Std Gross Margin," +
                           "Std Net Profit,Std Net Margin,Std Sell Price,Sell Price,Booked/Std Price," +
                           "Std Net Margin(Act Price),Std Gross Margin(Act Price),Act Sell Price/MSF,Sales Group,Job Created Date,Job Start Date,Job Qty MSF (FG),Std Sell Price/MSF,Cust Type Desc,Job Status,Order Status"
    cFieldListToSelect = "job,item,item-name,category,cust-part,style,job-qty,est,form,blank,order,cust,cust-name," +
                            "calc-tm,calc-by,std-prp-mat,std-mis-mat,std-prp-lab,std-misc-lab,std-dir-mat,std-dir-lab," +
                            "std-var-over,std-fix-over,std-tot-fac,std-comm,std-frt,std-ful-cst,std-gross-prft,std-gross-mar," +
                            "std-net-prft,std-net-mar,std-sel-price,ord-itm-s-price,book-std-price," +
                            "std-net-margin-act-pr,std-gross-margin-act-pr,act-sell-pri-msf,sales-grp,job-create,job-start,job-qty-msf-fg,std-sell-price-msf,cust-type,job-stat,order-stat"
    cFieldLength       = "9,15,25,11,15,6,13,8,4,5,7,8,30," + "23,8,17,17,14,14,18,16," + "16,18,17,14,11,13,16,16," + "14,14,14,13,16," + "25,27,19,11,16,14,16,18,20,10,12"
    cFieldType         = "c,c,c,c,c,c,c,i,i,i,i,c,c," + "c,c,c,c,c,c,c,c," + "c,c,c,c,c,c,c,c," + "c,c,c,c,c," + "i,i,i,c,c,c,i,i,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Job #,Item ID,Item Name,Category,Cust Part #,Style,Job Qty,Estimate,Form,Blank,Order,Customer,Customer Name," + 
                           "Calc Tim,Calc By,Std Prep Material,Std Misc Material,Std Prep Labor,Std Misc Labor,Std Direct Material,Std Direct Labor," +
                           "Std Var Overhead,Std Fixed Overhead,Std Total Factory,Std Commission,Std Freight,Std Full Cost,Std Gross Profit,Std Gross Margin," +
                           "Std Net Profit,Std Net Margin,Std Sell Price,Sell Price,Booked/Std Price".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_job-no begin_job-no2 ~
end_job-no end_job-no2 begin_est end_est begin_date end_date ~
begin_i-no end_i-no rd_sort tb_totals sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_job-no begin_job-no2 end_job-no ~
end_job-no2 begin_est end_est begin_date end_date begin_i-no ~
end_i-no lbl_sort rd_sort tb_totals sl_avail sl_selected rd-dest lv-ornt ~
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

DEFINE VARIABLE begin_est      AS CHARACTER FORMAT "X(8)" 
    LABEL "Beginning Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-99":U INITIAL "00" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_i-no     AS CHARACTER FORMAT "X(15)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_est        AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
    LABEL "Ending Estimate#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-jobcstdet.csv" 
    LABEL "If Yes, File Name" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    FGCOLOR 9 .

DEFINE VARIABLE lbl_sort       AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd-dest        AS INTEGER   INITIAL 1 
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "To Printer", 1,
    "To Screen", 2,
    "To File", 3,
    "To Fax", 4,
    "To Email", 5,
    "To Port Directly", 6
    SIZE 20 BY 6.67 NO-UNDO.

/*DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Job" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimate", "Estimate",
"Job", "Job",
"Date", "Date"
     SIZE 38 BY 1 NO-UNDO.*/
DEFINE VARIABLE rd_sort        AS CHARACTER INITIAL "Job" 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Job", "Job",
    "Estimate", "Estimate",
    "Date", "Date"
    SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 9.19.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 92 BY 9.05.

DEFINE VARIABLE sl_avail     AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected  AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_excel     AS LOGICAL   INITIAL YES 
    LABEL "Export To Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_runExcel  AS LOGICAL   INITIAL NO 
    LABEL "Auto Run Excel?" 
    VIEW-AS TOGGLE-BOX
    SIZE 21 BY .81
    BGCOLOR 3 NO-UNDO.

DEFINE VARIABLE tb_totals    AS LOGICAL   INITIAL NO 
    LABEL "Print Totals" 
    VIEW-AS TOGGLE-BOX
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL YES 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 22 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_job-no AT ROW 3.24 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 3.24 COL 39 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 3.24 COL 69 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 3.24 COL 81 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    begin_est AT ROW 4.33 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Estimate" WIDGET-ID 64
    end_est AT ROW 4.33 COL 69 COLON-ALIGNED HELP
    "Enter Ending Estimate" WIDGET-ID 66
    begin_date AT ROW 5.43 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 5.43 COL 69 COLON-ALIGNED HELP
    "Enter Ending Date"
    begin_i-no AT ROW 6.52 COL 27 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_i-no AT ROW 6.52 COL 69 COLON-ALIGNED HELP
    "Enter Ending Item Number"
    lbl_sort AT ROW 7.81 COL 27.4 COLON-ALIGNED NO-LABELS WIDGET-ID 74
    rd_sort AT ROW 7.81 COL 39.4 NO-LABELS WIDGET-ID 76
    tb_totals AT ROW 8.91 COL 29.4 WIDGET-ID 80
    sl_avail AT ROW 10.81 COL 3.6 NO-LABELS WIDGET-ID 26
    Btn_Def AT ROW 10.81 COL 39.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 56
    sl_selected AT ROW 10.81 COL 59 NO-LABELS WIDGET-ID 28
    Btn_Add AT ROW 11.81 COL 39.8 HELP
    "Add Selected Table to Tables to Audit" WIDGET-ID 32
    Btn_Remove AT ROW 12.81 COL 39.8 HELP
    "Remove Selected Table from Tables to Audit" WIDGET-ID 34
    btn_Up AT ROW 13.86 COL 39.8 WIDGET-ID 40
    btn_down AT ROW 14.86 COL 39.8 WIDGET-ID 42
    rd-dest AT ROW 17.29 COL 4 NO-LABELS
    lv-ornt AT ROW 17.29 COL 31 NO-LABELS
    lines-per-page AT ROW 17.29 COL 84 COLON-ALIGNED
    lv-font-no AT ROW 19.05 COL 34 COLON-ALIGNED
    lv-font-name AT ROW 20 COL 28 COLON-ALIGNED NO-LABELS
    td-show-parm AT ROW 21.19 COL 30
    tb_excel AT ROW 22.81 COL 49.8 RIGHT-ALIGNED
    tb_runExcel AT ROW 22.81 COL 71 RIGHT-ALIGNED
    fi_file AT ROW 23.76 COL 27.8 COLON-ALIGNED HELP
    "Enter File Name"
    btn-ok AT ROW 25.43 COL 25
    btn-cancel AT ROW 25.43 COL 56
    "Available Columns" VIEW-AS TEXT
    SIZE 29 BY .62 AT ROW 10.1 COL 4.4 WIDGET-ID 38
    "Selected Columns(In Display Order)" VIEW-AS TEXT
    SIZE 34 BY .62 AT ROW 10.05 COL 58.6 WIDGET-ID 44
    "Selection Parameters" VIEW-AS TEXT
    SIZE 21 BY .71 AT ROW 1.24 COL 5
    BGCOLOR 2 
    "Output Destination" VIEW-AS TEXT
    SIZE 18 BY .62 AT ROW 16.33 COL 3
    RECT-6 AT ROW 16 COL 1
    RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1.6 ROW 1.24
    SIZE 93.4 BY 25.81.


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
        TITLE              = "Estimated Job Cost Detail"
        HEIGHT             = 26.1
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
    begin_est:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_est:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_i-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    fi_file:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    lbl_sort:PRIVATE-DATA IN FRAME FRAME-A = "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
    rd_sort:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_excel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
    tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    tb_totals:PRIVATE-DATA IN FRAME FRAME-A = "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Board Reconcilation */
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
ON WINDOW-CLOSE OF C-Win /* Board Reconcilation */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON HELP OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
    DO:
        DEFINE VARIABLE char-val    AS cha   NO-UNDO.
        DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

        RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

        IF char-val <> "" THEN 
        DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE     = eb.est-no
                    lv-eb-tmpid            = RECID(eb)    
                    begin_est:SCREEN-VALUE = eb.est-no.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_est C-Win
ON LEAVE OF begin_est IN FRAME FRAME-A /* Beginning Estimate# */
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


&Scoped-define SELF-NAME begin_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no C-Win
ON LEAVE OF begin_job-no IN FRAME FRAME-A /* Beginning Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_job-no2 C-Win
ON LEAVE OF begin_job-no2 IN FRAME FRAME-A
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
        RUN run-report. 
        STATUS DEFAULT "Processing Complete".

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
            WHEN 4 THEN 
                DO:
                    /*run output-to-fax.*/
                    {custom/asifax.i &begin_cust=begin_job-no
                            &END_cust=END_job-no
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
                             &begin_cust= begin_job-no
                             &END_cust=end_job-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
                    END.
                    ELSE 
                    DO:
                        {custom/asimailr.i &TYPE = ''
                                  &begin_cust= begin_job-no
                                  &END_cust=end_job-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

                    END.

                END. 
            WHEN 6 THEN RUN output-to-port.
        END CASE. 
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


&Scoped-define SELF-NAME end_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON HELP OF end_est IN FRAME FRAME-A /* Ending Estimate# */
    DO:
        DEFINE VARIABLE char-val    AS cha   NO-UNDO.
        DEFINE VARIABLE lv-eb-tmpid AS RECID NO-UNDO.

        RUN windows/l-est.w (g_company,g_loc,FOCUS:SCREEN-VALUE, OUTPUT char-val).

        IF char-val <> "" THEN 
        DO:                 
            FIND FIRST eb WHERE STRING(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN ASSIGN FOCUS:SCREEN-VALUE   = eb.est-no
                    lv-eb-tmpid          = RECID(eb)    
                    end_est:SCREEN-VALUE = eb.est-no.

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_est C-Win
ON LEAVE OF end_est IN FRAME FRAME-A /* Ending Estimate# */
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


&Scoped-define SELF-NAME end_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no C-Win
ON LEAVE OF end_job-no IN FRAME FRAME-A /* Ending Job# */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_job-no2 C-Win
ON LEAVE OF end_job-no2 IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_totals
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_totals C-Win
ON VALUE-CHANGED OF tb_totals IN FRAME FRAME-A /* Print Totals */
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
        begin_date = DATE(MONTH(TODAY),1,YEAR(TODAY))
        end_date   = DATE(TODAY).

    RUN DisplaySelectionList. 
    RUN enable_UI.

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_i-no.
    END.

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
    DISPLAY begin_job-no begin_job-no2 end_job-no end_job-no2 begin_est end_est 
        begin_date end_date begin_i-no end_i-no lbl_sort rd_sort 
        tb_totals sl_avail sl_selected rd-dest lv-ornt lines-per-page 
        lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_job-no begin_job-no2 end_job-no end_job-no2 
        begin_est end_est begin_date end_date begin_i-no end_i-no 
        rd_sort tb_totals sl_avail Btn_Def sl_selected Btn_Add Btn_Remove 
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
    /* ----------------------------------------------- jc/rep/jc-mrec.p 07/98 JLF */
    /* Job Material Reconciliation Report                                         */
    /* -------------------------------------------------------------------------- */
    /*{sys/form/r-topw.f}*/

    DEFINE VARIABLE v-fdate          AS DATE      FORMAT "99/99/9999" INIT 01/01/0001 NO-UNDO.
    DEFINE VARIABLE v-tdate          LIKE v-fdate INIT 12/31/9999 NO-UNDO.
    DEFINE VARIABLE v-fjob           LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-tjob           LIKE v-fjob INIT "zzzzzz" NO-UNDO.
    DEFINE VARIABLE v-fjob2          LIKE job.job-no2 NO-UNDO.
    DEFINE VARIABLE v-tjob2          LIKE v-fjob2 INIT 99 NO-UNDO.

    DEFINE VARIABLE iSetMult         AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE cDisplay         AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelDisplay    AS cha       NO-UNDO.
    DEFINE VARIABLE hField           AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField        AS CHA       NO-UNDO.
    DEFINE VARIABLE cVarValue        AS cha       NO-UNDO.
    DEFINE VARIABLE cExcelVarValue   AS cha       NO-UNDO.
    DEFINE VARIABLE cSelectedList    AS cha       NO-UNDO.
    DEFINE VARIABLE cFieldName       AS cha       NO-UNDO.
    DEFINE VARIABLE str-tit4         AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5         AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-line         AS cha       FORM "x(300)" NO-UNDO.
    DEFINE VARIABLE dInvdt           AS DATE      NO-UNDO.
    DEFINE VARIABLE cOrdStat         AS CHARACTER FORMAT "!" INITIAL "O" NO-UNDO.



    DEFINE VARIABLE dQtyInM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyPerSet       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOrdPricePerM    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iEstType         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE fest             LIKE est.est-no INIT "" NO-UNDO.
    DEFINE VARIABLE test             LIKE fest INIT "zzzzz" NO-UNDO.
    DEFINE VARIABLE v-sort           AS CHARACTER FORMAT "!" INIT "J" NO-UNDO.
    DEFINE VARIABLE cToTJobQty       LIKE dQtyInM NO-UNDO.
    DEFINE VARIABLE cToTStdFac       LIKE costHeader.stdCostTotalFactory NO-UNDO.
    DEFINE VARIABLE cToTStdComm      LIKE costHeader.stdCostCommission NO-UNDO.
    DEFINE VARIABLE cToTStdFrt       LIKE costHeader.stdCostFreight NO-UNDO.
    DEFINE VARIABLE iLineCount       AS INTEGER   NO-UNDO .
    DEFINE VARIABLE cOrderStatus     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSqft            LIKE itemfg.t-sqft FORMAT ">,>>9.999" NO-UNDO.
    DEFINE VARIABLE dPricePerMsf     AS DECIMAL   COLUMN-LABEL "$/MSF" NO-UNDO.
    DEFINE VARIABLE dStdSellPriceMsf AS DECIMAL   COLUMN-LABEL "$/MSF" NO-UNDO.
    DEFINE VARIABLE dJobQtyMsf       AS DECIMAL   NO-UNDO .
    {sys/form/r-topsw.f}

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.


    DEFINE VARIABLE cslist      AS cha       NO-UNDO.
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

        IF LOOKUP(ttRptSelected.TextList, "Job Qty,Std Total Factory,Std Commission,Std Freight") <> 0    THEN
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
        str-tit2 = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-fdate  = begin_date
        v-tdate  = end_date

        v-fjob   = FILL(" ",6 - length(TRIM(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
        v-tjob   = FILL(" ",6 - length(TRIM(end_job-no)))   +
              trim(end_job-no)   + string(int(end_job-no2),"99")
        fest     = FILL(" ",8 - LENGTH(TRIM(begin_est))) + TRIM(begin_est)
        test     = FILL(" ",8 - LENGTH(TRIM(end_est))) + TRIM(end_est)
        v-sort   = SUBSTR(rd_sort,1,1)  .


    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH tt-report WHERE tt-report.term-id EQ "":
        DELETE tt-report.
    END.

    DISPLAY WITH FRAME r-top.
    PUT  str-tit4 FORMAT "x(520)"
        SKIP
        str-tit5 FORMAT "x(520)"
        SKIP .

    iLineCount = 0 .
    FOR EACH costHeader NO-LOCK 
        WHERE costHeader.company EQ gcompany
        AND costHeader.calculationTime GE DATETIME(v-fdate)
        AND costHeader.calculationTime LE DATETIME(v-tdate + 1)
        AND costHeader.jobNo  GE substr(v-fjob,1,6)
        AND costHeader.jobNo  LE substr(v-tjob,1,6)
        AND fill(" ",6 - length(TRIM(costHeader.jobNo))) +
        trim(costHeader.jobNo) + string(costHeader.jobNo2,"99")
        GE v-fjob
        AND fill(" ",6 - length(TRIM(costHeader.jobNo))) +
        trim(costHeader.jobNo) + string(costHeader.jobNo2,"99")
        LE v-tjob
        AND costHeader.estimateNo GE fest
        AND costHeader.estimateNo LE test
        AND costHeader.fgItemID GE begin_i-no
        AND costHeader.fgItemID LE end_i-no
        AND costHeader.jobNo NE ""
        AND costHeader.isItem,
        FIRST job NO-LOCK 
        WHERE job.company EQ costHeader.company
        AND job.job-no EQ costHeader.jobNo
        AND job.job-no2 EQ costHeader.JobNo2,
        FIRST eb NO-LOCK 
        WHERE eb.company EQ costHeader.company
        AND eb.est-no EQ costHeader.estimateNo
        AND eb.form-no EQ costHeader.formNo
        AND eb.blank-no EQ costHeader.blankNo,
        FIRST cust NO-LOCK
        WHERE cust.company EQ eb.company
        AND cust.cust-no EQ eb.cust-no
        BREAK BY (IF v-sort BEGINS "J" THEN costHeader.jobNo ELSE IF v-sort BEGINS "E" THEN costHeader.estimateNo ELSE STRING(DATE(costHeader.calculationTime))) 
        :   
                
        FIND FIRST est NO-LOCK WHERE 
            est.company EQ eb.company AND 
            est.est-no EQ eb.est-no 
            NO-ERROR.
        IF AVAILABLE est THEN iEstType = est.est-type.        
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ costHeader.company
            AND itemfg.i-no EQ costHeader.fgItemID
            NO-ERROR.

        FIND FIRST job-hdr NO-LOCK 
            WHERE job-hdr.company EQ costHeader.company
            AND job-hdr.job-no EQ costHeader.jobNo
            AND job-hdr.job-no2 EQ costHeader.jobNo2
            AND job-hdr.i-no EQ costHeader.fgItemID
            NO-ERROR.   
        
        IF AVAILABLE job-hdr THEN 
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ job-hdr.company
                AND oe-ordl.ord-no EQ job-hdr.ord-no
                AND oe-ordl.i-no EQ job-hdr.i-no
                NO-ERROR.              
        ELSE RELEASE oe-ordl .
                      
        IF iEstType EQ 6 AND costHeader.formNo NE 0 THEN
            dQtyPerSet = costHeader.quantityPerSet.
        ELSE 
            dQtyPerSet = 1.
            
        dQtyInM = IF AVAILABLE job-hdr THEN job-hdr.qty / 1000 ELSE costHeader.quantityMaster * dQtyPerSet / 1000.
    
        IF AVAILABLE oe-ordl THEN 
        DO:
            RUN sys\ref\convptom.p (oe-ordl.pr-uom, oe-ordl.price, oe-ordl.qty, oe-ordl.cas-cnt, OUTPUT dOrdPricePerM).
        END.
        ELSE 
        DO:
            RUN sys\ref\convptom.p (itemfg.sell-uom, itemfg.sell-price, INT(dQtyInM * 1000), itemfg.case-count, OUTPUT dOrdPricePerM).
        END.

        IF iLineCount GE (lines-per-page - 10)  THEN 
        DO:
            PAGE.
            PUT str-tit4 FORMAT "x(520)"
                SKIP
                str-tit5 FORMAT "x(520)"
                SKIP .
            iLineCount = 0 .
        END.

        IF AVAILABLE itemfg THEN 
            RUN fg/GetFGArea.p (ROWID(itemfg), "MSF", OUTPUT dSqft).  
        ELSE 
            dSqft = 0.
          
        dSqft = dSqft * (IF AVAILABLE job-hdr THEN job-hdr.qty ELSE 0).
        dPricePerMsf = dOrdPricePerM / dSqft .
        dStdSellPriceMsf = (costHeader.stdSellPrice / dQtyInM) / dSqft .

        dJobQtyMsf = dSqft .
         
        IF AVAILABLE oe-ordl THEN 
        DO:
            cOrderStatus = oe-ordl.stat .
            IF cOrderStatus EQ "" THEN 
            DO:
                FIND FIRST oe-ord NO-LOCK
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.ord-no EQ oe-ordl.ord-no
                    NO-ERROR.
                cOrderStatus = oe-ord.stat .
            END.

            RUN oe/getStatusDesc.p( INPUT cOrderStatus, OUTPUT cResult) .
            IF cResult NE "" THEN
                cOrderStatus  = cResult .
        END.
        ELSE 
        DO:
            cOrderStatus = "" .
        END.

        FIND FIRST custype NO-LOCK
            WHERE custype.company = cocode
            AND custype.custype = cust.TYPE 
            NO-ERROR .
                   

        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "job"       THEN 
                    cVarValue = STRING(costHeader.jobNo + "-" + STRING(costHeader.jobNo2,"99")).
                WHEN "item"      THEN 
                    cVarValue = STRING(costHeader.fgItemID,"X(15)").
                WHEN "item-name"    THEN 
                    cVarValue = IF AVAILABLE itemfg THEN SUBSTRING(itemfg.i-name,1,25) ELSE "".
                WHEN "category"    THEN 
                    cVarValue = IF AVAILABLE itemfg THEN itemfg.procat ELSE eb.procat.
                WHEN "cust-part"    THEN 
                    cVarValue = IF AVAILABLE itemfg THEN itemfg.part-no ELSE eb.part-no.
                WHEN "style" THEN 
                    cVarValue = IF AVAILABLE itemfg THEN itemfg.style ELSE eb.style .
                WHEN "job-qty"      THEN 
                    cVarValue = IF dQtyInM NE 0 THEN STRING((dQtyInM * 1000),"->>>>,>>>,>>9") ELSE "".
                WHEN "est" THEN 
                    cVarValue = STRING(costHeader.estimateNo,"x(8)").
                WHEN "form"     THEN 
                    cVarValue = STRING(costHeader.formNo,">>>9")  .
                WHEN "blank"      THEN 
                    cVarValue = STRING(costHeader.blankNo,">>>>9") .
                WHEN "order"   THEN 
                    cVarValue = IF AVAILABLE job-hdr THEN STRING(job-hdr.ord-no,">>>>>>9") ELSE "".
                WHEN "cust"     THEN 
                    cVarValue = IF AVAILABLE cust THEN STRING(cust.cust-no,"x(8)") ELSE "".
                WHEN "cust-name"  THEN 
                    cVarValue = IF AVAILABLE cust THEN STRING(cust.NAME,"x(30)") ELSE "".
                WHEN "calc-tm"  THEN 
                    cVarValue = STRING(costHeader.calculationTime)   .
                WHEN "calc-by"    THEN 
                    cVarValue = STRING(costHeader.calculatedBy,"x(8)").
                WHEN "std-prp-mat"    THEN 
                    cVarValue = STRING((costHeader.stdCostPrepMaterial / dQtyInM),"->>>>,>>>,>>>,>>9").
                WHEN "std-mis-mat"       THEN 
                    cVarValue = STRING((costHeader.stdCostMiscMaterial / dQtyInM),"->>>>,>>>,>>>,>>9").
                WHEN "std-prp-lab"      THEN 
                    cVarValue = STRING((costHeader.stdCostPrepLabor / dQtyInM),"->,>>>,>>>,>>9").
                WHEN "std-misc-lab"    THEN 
                    cVarValue = STRING((costHeader.stdCostMiscLabor / dQtyInM),"->,>>>,>>>,>>9").
                WHEN "std-dir-mat"    THEN 
                    cVarValue = STRING((costHeader.stdCostDirectMaterial / dQtyInM),"->>,>>>,>>>,>>9.99").
                WHEN "std-dir-lab"    THEN 
                    cVarValue = STRING((costHeader.stdCostDirectLabor / dQtyInM),"->>>>,>>>,>>9.99").
                WHEN "std-var-over" THEN 
                    cVarValue = STRING((costHeader.stdCostVariableOverhead / dQtyInM),"->>>>,>>>,>>9.99").
                WHEN "std-fix-over"      THEN 
                    cVarValue = STRING((costHeader.stdCostFixedOverhead / dQtyInM),"->>,>>>,>>>,>>9.99").
                WHEN "std-tot-fac" THEN 
                    cVarValue = STRING((costHeader.stdCostTotalFactory / dQtyInM),"->,>>>,>>>,>>9.99").
                WHEN "std-comm"     THEN 
                    cVarValue = STRING((costHeader.stdCostCommission / dQtyInM),"->>,>>>,>>9.99").
                WHEN "std-frt"      THEN 
                    cVarValue = STRING((costHeader.stdCostFreight / dQtyInM),"->>>,>>9.99").
                WHEN "std-ful-cst"   THEN 
                    cVarValue = STRING((costHeader.stdCostFull / dQtyInM),"->,>>>,>>9.99").
                WHEN "std-gross-prft"     THEN 
                    cVarValue = STRING((costHeader.stdProfitGross / dQtyInM),"->>>>,>>>,>>9.99").
                WHEN "std-gross-mar"  THEN 
                    cVarValue = IF costHeader.stdSellPrice GT 0 THEN STRING(((costHeader.stdProfitGross / costHeader.stdSellPrice) * 100),"->>,>>>,>>>,>>9%") ELSE "".
                WHEN "std-net-prft"  THEN 
                    cVarValue = STRING((costHeader.stdProfitNet / dQtyInM),"->>,>>>,>>9.99").
                WHEN "std-net-mar"    THEN 
                    cVarValue = IF costHeader.stdSellPrice GT 0 THEN STRING(((costHeader.stdProfitNet / costHeader.stdSellPrice) * 100),"->>>>,>>>,>>9%") ELSE "".
                WHEN "std-sel-price"    THEN 
                    cVarValue = STRING((costHeader.stdSellPrice / dQtyInM),"->>,>>>,>>9.99").
                WHEN "ord-itm-s-price"   THEN 
                    cVarValue = STRING((dOrdPricePerM ),"->,>>>,>>9.99").
                WHEN "book-std-price"     THEN 
                    cVarValue = IF costHeader.stdSellPrice GT 0 THEN STRING(((dOrdPricePerM / (costHeader.stdSellPrice / dQtyInM )) * 100),"->>,>>>,>>>,>>9%") ELSE "".

                WHEN "std-net-margin-act-pr" THEN 
                    cVarValue = IF dOrdPricePerM GT 0 THEN STRING((((costHeader.stdProfitNet / dQtyInM  ) / (dOrdPricePerM )) * 100),"->>>>,>>>,>>9%") ELSE "".
                WHEN "std-gross-margin-act-pr" THEN 
                    cVarValue = IF dOrdPricePerM GT 0 THEN STRING((((costHeader.stdProfitGross / dQtyInM  ) / (dOrdPricePerM )) * 100),"->>>>,>>>,>>9%") ELSE "".
                WHEN "act-sell-pri-msf"     THEN 
                    cVarValue = IF dPricePerMsf NE ? THEN STRING(dPricePerMsf,"->>,>>>,>>9.99") ELSE "".
                WHEN "sales-grp"      THEN 
                    cVarValue = IF AVAILABLE oe-ordl THEN  STRING(oe-ordl.s-man[1]) ELSE STRING(cust.sman).
                WHEN "job-create"   THEN 
                    cVarValue = IF job.create-date NE ? THEN STRING(job.create-date) ELSE "".
                WHEN "job-start"     THEN 
                    cVarValue = IF job.start-date NE ? THEN STRING(job.start-date) ELSE "".
                WHEN "job-qty-msf-fg"  THEN 
                    cVarValue = IF dJobQtyMsf NE ? THEN  STRING(dJobQtyMsf,"->>>>,>>>,>>9.99") ELSE "" .
                WHEN "std-sell-price-msf"  THEN 
                    cVarValue = IF dStdSellPriceMsf NE ? THEN STRING(dStdSellPriceMsf,"->>,>>>,>>9.99") ELSE "".
                WHEN "cust-type"    THEN 
                    cVarValue = IF AVAILABLE custype THEN STRING(custype.dscr,"x(20)")  ELSE "".
                WHEN "job-stat"    THEN 
                    cVarValue = STRING(job.stat,"x(10)").
                WHEN "order-stat"   THEN 
                    cVarValue = STRING(cOrderStatus,"x(12)") .
                
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
        iLineCount = iLineCount + 1 .

        ASSIGN
            cToTJobQty  = cToTJobQty + (dQtyInM * 1000)
            cToTStdFac  = cToTStdFac + (costHeader.stdCostTotalFactory / dQtyInM)
            cToTStdComm = cToTStdComm + (costHeader.stdCostCommission / dQtyInM)
            cToTStdFrt  = cToTStdFrt +  (costHeader.stdCostFreight / dQtyInM).


    END.

    IF tb_totals THEN  
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
                WHEN "job"                THEN 
                    cVarValue = "".
                WHEN "item"               THEN 
                    cVarValue = "".
                WHEN "item-name"          THEN 
                    cVarValue = "".
                WHEN "category"           THEN 
                    cVarValue = "".
                WHEN "cust-part"          THEN 
                    cVarValue = "".
                WHEN "style"              THEN 
                    cVarValue = "".
                WHEN "job-qty"            THEN 
                    cVarValue = IF dQtyInM NE 0 THEN STRING(cToTJobQty,"->>>>,>>>,>>9") ELSE "".
                WHEN "est"                THEN 
                    cVarValue = "".
                WHEN "form"               THEN 
                    cVarValue = "".
                WHEN "blank"              THEN 
                    cVarValue = "".
                WHEN "order"              THEN 
                    cVarValue = "".
                WHEN "cust"               THEN 
                    cVarValue = "".
                WHEN "cust-name"          THEN 
                    cVarValue = "".
                WHEN "calc-tm"            THEN 
                    cVarValue = "".
                WHEN "calc-by"            THEN 
                    cVarValue = "".
                WHEN "std-prp-mat"        THEN 
                    cVarValue = "".
                WHEN "std-mis-mat"        THEN 
                    cVarValue = "".
                WHEN "std-prp-lab"        THEN 
                    cVarValue = "".
                WHEN "std-misc-lab"       THEN 
                    cVarValue = "".
                WHEN "std-dir-mat"        THEN 
                    cVarValue = "".
                WHEN "std-dir-lab"        THEN 
                    cVarValue = "".
                WHEN "std-var-over"       THEN 
                    cVarValue = "".
                WHEN "std-fix-over"       THEN 
                    cVarValue = "".
                WHEN "std-tot-fac"        THEN 
                    cVarValue = STRING(cToTStdFac,"->,>>>,>>>,>>9.99").
                WHEN "std-comm"           THEN 
                    cVarValue = STRING(cToTStdComm,"->>,>>>,>>9.99").
                WHEN "std-frt"            THEN 
                    cVarValue = STRING(cToTStdFrt,"->>>,>>9.99").
                WHEN "std-ful-cst"        THEN 
                    cVarValue = "".
                WHEN "std-gross-prft"     THEN 
                    cVarValue = "".
                WHEN "std-gross-mar"      THEN 
                    cVarValue = "".
                WHEN "std-net-prft"       THEN 
                    cVarValue = "".
                WHEN "std-net-mar"        THEN 
                    cVarValue = "".
                WHEN "std-sel-price"      THEN 
                    cVarValue = "".
                WHEN "ord-itm-s-price"    THEN 
                    cVarValue = "".
                WHEN "book-std-price"     THEN 
                    cVarValue = "".
                WHEN "std-net-margin-act-pr"   THEN 
                    cVarValue = "".
                WHEN "std-gross-margin-act-pr" THEN 
                    cVarValue = "".
                WHEN "act-sell-pri-msf"        THEN 
                    cVarValue =  "".
                WHEN "sales-grp"               THEN 
                    cVarValue =  "".
                WHEN "job-create"              THEN 
                    cVarValue =  "".
                WHEN "job-start"               THEN 
                    cVarValue =  "".
                WHEN "job-qty-msf-fg"          THEN 
                    cVarValue =  "".
                WHEN "std-sell-price-msf"      THEN 
                    cVarValue =  "".
                WHEN "cust-type"               THEN 
                    cVarValue =  "".
                WHEN "job-stat"                THEN 
                    cVarValue =  "".
                WHEN "order-stat"              THEN 
                    cVarValue =  "".
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.
        PUT str-line SKIP.
        PUT UNFORMATTED 
            "   Job Totals" SUBSTRING(cDisplay,14,300) SKIP.

        PUT SKIP(1).
        /*PUT UNFORMATTED cDisplay SKIP.*/
        IF tb_excel THEN 
        DO:
            PUT STREAM excel UNFORMATTED  
                "job Totals" + substring(cExcelDisplay,3,300) SKIP.
              
          

        END.

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
