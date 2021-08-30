&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: rmrep\r-inkglu.w

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

DEFINE VARIABLE v-print-fmt                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form              AS LOGICAL.
DEFINE VARIABLE ls-fax-file                 AS CHARACTER NO-UNDO.

DEFINE VARIABLE v-fitem                     LIKE rm-rcpth.i-no.
DEFINE VARIABLE v-titem                     LIKE v-fitem INIT "zzzzzzzzzz".
DEFINE VARIABLE v-fpcat                     LIKE item.procat.
DEFINE VARIABLE v-tpcat                     LIKE v-fpcat INIT "zzzzz".
DEFINE VARIABLE v-fdate                     AS DATE      FORMAT "99/99/9999" INIT 01/01/0001.
DEFINE VARIABLE v-tdate                     LIKE v-fdate INIT TODAY.
DEFINE VARIABLE v-fjob                      LIKE job.job-no.
DEFINE VARIABLE v-tjob                      LIKE v-fjob INIT "zzzzzz".
DEFINE VARIABLE v-fjob2                     LIKE job.job-no2 FORMAT "99".
DEFINE VARIABLE v-tjob2                     LIKE v-fjob2 INIT 99.
DEFINE VARIABLE v-mtype                     AS CHARACTER FORMAT "x(47)".
DEFINE VARIABLE v-export                    AS LOG       INIT NO FORMAT "Y/N".
DEFINE VARIABLE v-exp-name                  AS CHARACTER FORMAT "x(40)" INIT "rmtrans3.csv".
DEFINE VARIABLE v-fCat                      LIKE itemfg.procat NO-UNDO.
DEFINE VARIABLE v-tCat                      LIKE itemfg.procat NO-UNDO.
DEFINE VARIABLE v-job-no                    AS CHARACTER FORMAT "x(9)".
DEFINE VARIABLE v-rm-qty                    AS DECIMAL.
DEFINE VARIABLE v-qty                       AS DECIMAL   FORMAT "->,>>>,>>>,>>9.9<<<<" EXTENT 3.
DEFINE VARIABLE v-m-code                    LIKE mach.m-code FORMAT "x(6)".
DEFINE VARIABLE v-board                     LIKE item.i-no EXTENT 2.
DEFINE VARIABLE v-brd-qty                   AS INTEGER   FORMAT "->,>>>,>>>,>>9" EXTENT 2.
DEFINE VARIABLE v-first                     AS LOG       EXTENT 3.
DEFINE VARIABLE v-board-msf                 AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-board-weight              AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-wax-coat-weight           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tot-board-weight          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tot-wax-coat-weight       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tot-job-hdr-qty           AS DECIMAL   FORMAT "->,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-grand-tot-board-weight    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-grand-tot-wax-coat-weight AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-grand-tot-job-hdr-qty     AS DECIMAL   FORMAT "->,>>>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-trans-qty             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-grand-tot-trans-qty       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-wid                       LIKE job-mat.wid NO-UNDO.
DEFINE VARIABLE v-len                       LIKE job-mat.len NO-UNDO.
DEFINE VARIABLE v-basis-w                   LIKE job-mat.basis-w NO-UNDO.

DEFINE STREAM s-temp.

DEFINE TEMP-TABLE tt-inks-glues
    FIELD trans-date LIKE rm-rcpth.trans-date 
    FIELD job-no     AS CHARACTER FORMAT "x(9)"
    FIELD qty        AS DECIMAL   FORMAT "->,>>>,>>>,>>9.9<<<<"
    FIELD m-code     LIKE mach.m-code FORMAT "x(6)"
    FIELD board      LIKE item.i-no 
    FIELD brd-qty    AS INTEGER   FORMAT "->,>>>,>>>,>>9"
    FIELD i-no       LIKE rm-rcpth.i-no
    FIELD procat     LIKE itemfg.procat.

DEFINE TEMP-TABLE tt-wax-coats
    FIELD trans-date      LIKE rm-rcpth.trans-date 
    FIELD job-no          AS CHARACTER FORMAT "x(9)"
    FIELD qty             AS DECIMAL   FORMAT "->,>>>,>>>,>>9"
    FIELD board           LIKE item.i-no 
    FIELD brd-qty         AS INTEGER   FORMAT "->,>>>,>>>,>>9"
    FIELD i-no            LIKE rm-rcpth.i-no
    FIELD board-msf       AS DECIMAL
    FIELD board-weight    AS DECIMAL
    FIELD shrink          LIKE ITEM.shrink
    FIELD wax-coat-weight AS DECIMAL
    FIELD procat          LIKE itemfg.procat
    FIELD m-code          LIKE mach.m-code FORMAT "x(6)"
    FIELD qty-iss         AS DECIMAL   FORMAT "->,>>>,>>>,>>9.9<<<<" 
    FIELD v-board         AS DECIMAL .

DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO .

FORM tt-wax-coats.trans-date LABEL "Issue Date"
    tt-wax-coats.job-no     LABEL "   Job #"
    tt-wax-coats.board      LABEL "Board"
    tt-wax-coats.brd-qty    LABEL "Sheets Issued"
    tt-wax-coats.i-no       LABEL "Wax/Coat"
    tt-wax-coats.board-msf  LABEL "Board MSF"
    tt-wax-coats.board-weight LABEL "Board Weight"
    tt-wax-coats.shrink       LABEL "Pickup%"
    tt-wax-coats.wax-coat-weight LABEL "Wax/Coat Weight"
    tt-wax-coats.procat LABEL "Product Category"
    tt-wax-coats.qty    LABEL "Produced"
    SKIP
    WITH FRAME item-x NO-BOX DOWN STREAM-IO WIDTH 180.

FORM    
    "------------"        AT 70
    "---------------"     AT 92
    "--------------"      AT 125
    SKIP
    "SUB TOTAL"           AT 37
    v-tot-board-weight    AT 72
    v-tot-wax-coat-weight AT 97
    v-tot-job-hdr-qty     AT 125
    SKIP(2)
    WITH FRAME item-b NO-BOX DOWN NO-LABELS NO-ATTR STREAM-IO WIDTH 180.

FORM    
    "------------"        AT 70
    "---------------"     AT 92
    "--------------"          AT 125
    SKIP
    "GRAND TOTAL"         AT 37
    v-grand-tot-board-weight    AT 72
    v-grand-tot-wax-coat-weight AT 97  
    v-grand-tot-job-hdr-qty     AT 125
    WITH FRAME item-c NO-BOX DOWN NO-LABELS NO-ATTR STREAM-IO WIDTH 180.

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cColumnInit        AS LOG       INIT YES NO-UNDO.


ASSIGN 
    cTextListToSelect  = "RM Item,Issue Date,Job #,Board,Sheets Issued,Board MSF,Board Weight," +
                           "Pickup%,Wax-Coat Weight,Produced,Qty Issued/Lbs,Machine,FG Category"
    cFieldListToSelect = "rm-item,date,job,board,sht-iss,brd-msf,brd-wit," +
                            "pck,wax-coat,prod,qty-iss,mach,fg-cat"
    cFieldLength       = "10,10,9,10,13,10,12," + "8,15,14,16,7,11"
    cFieldType         = "c,c,c,c,i,i,i," + "i,i,i,i,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "RM Item,Issue Date,Job #,Board,Sheets Issued,Board MSF,Board Weight," +
                           "Pickup%,Wax-Coat Weight,Produced,Qty Issued/Lbs,Machine,FG Category"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_rm-no end_rm-no ~
begin_procat end_procat begin_cat end_cat begin_date end_date begin_job-no ~
begin_job-no2 end_job-no end_job-no2 select-mat TG_sort-cat ~
btn_SelectColumns rd-dest fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_rm-no end_rm-no begin_procat ~
end_procat begin_cat end_cat begin_date end_date begin_job-no begin_job-no2 ~
end_job-no end_job-no2 select-mat TG_sort-cat rd-dest fi_file tb_OpenCSV ~
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

DEFINE BUTTON btn_SelectColumns 
    LABEL "Select Columns" 
    SIZE 43 BY 1.19.

DEFINE VARIABLE begin_cat      AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning FG Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_date     AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 
    LABEL "Beginning Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no   AS CHARACTER FORMAT "X(6)":U 
    LABEL "Beginning Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE begin_job-no2  AS CHARACTER FORMAT "-99":U INITIAL "00" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat   AS CHARACTER FORMAT "X(5)":U 
    LABEL "Beginning RM Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no    AS CHARACTER FORMAT "X(10)":U 
    LABEL "Beginning Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat        AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending FG Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date       AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 
    LABEL "Ending Date" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no     AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" 
    LABEL "Ending Job#" 
    VIEW-AS FILL-IN 
    SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE end_job-no2    AS CHARACTER FORMAT "-99":U INITIAL "99" 
    LABEL "" 
    VIEW-AS FILL-IN 
    SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat     AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
    LABEL "Ending RM Category" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no      AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
    LABEL "Ending Item#" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(256)" INITIAL "c:~\tmp~\r-inkglu.csv" 
    LABEL "Name" 
    VIEW-AS FILL-IN NATIVE 
    SIZE 43 BY 1.

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

DEFINE VARIABLE mat-types      AS CHARACTER FORMAT "X(256)":U 
    LABEL "Material Types" 
    VIEW-AS FILL-IN 
    SIZE 1 BY 1 NO-UNDO.

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
    SIZE 17 BY 5.19 NO-UNDO.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 6.19.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90 BY 11.86.

DEFINE VARIABLE select-mat   AS CHARACTER 
    VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
    SIZE 44 BY 3.57 NO-UNDO.

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

DEFINE VARIABLE tb_OpenCSV   AS LOGICAL   INITIAL NO 
    LABEL "Open CSV?" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL   INITIAL NO 
    LABEL "Show Parameters?" 
    VIEW-AS TOGGLE-BOX
    SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG_sort-cat  AS LOGICAL   INITIAL NO 
    LABEL "Sort by FG Category" 
    VIEW-AS TOGGLE-BOX
    SIZE 23 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
    begin_rm-no AT ROW 2.19 COL 28.4 COLON-ALIGNED HELP
    "Enter Beginning Item Number"
    end_rm-no AT ROW 2.19 COL 69.4 COLON-ALIGNED HELP
    "Enter Ending Item number"
    begin_procat AT ROW 3.14 COL 28.4 COLON-ALIGNED HELP
    "Enter Begining Category"
    end_procat AT ROW 3.14 COL 69.4 COLON-ALIGNED HELP
    "Enter Ending Category"
    begin_cat AT ROW 4.1 COL 28.4 COLON-ALIGNED HELP
    "Enter Beginning Category" WIDGET-ID 10
    end_cat AT ROW 4.1 COL 69.4 COLON-ALIGNED HELP
    "Enter Ending Category" WIDGET-ID 12
    begin_date AT ROW 5.14 COL 28.4 COLON-ALIGNED HELP
    "Enter Beginning Date"
    end_date AT ROW 5.14 COL 69.4 COLON-ALIGNED HELP
    "Enter ending Date"
    begin_job-no AT ROW 6.1 COL 28.4 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    begin_job-no2 AT ROW 6.1 COL 40.4 COLON-ALIGNED HELP
    "Enter Beginning Job Number"
    end_job-no AT ROW 6.1 COL 69.4 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    end_job-no2 AT ROW 6.1 COL 81.4 COLON-ALIGNED HELP
    "Enter Ending Job Number"
    select-mat AT ROW 8.14 COL 25 HELP
    "Enter description of this Material Type." NO-LABELS
    mat-types AT ROW 8.86 COL 21 COLON-ALIGNED
    TG_sort-cat AT ROW 9.52 COL 70 WIDGET-ID 6
    btn_SelectColumns AT ROW 11.95 COL 25.6 WIDGET-ID 10
    lv-ornt AT ROW 14.1 COL 37 NO-LABELS
    lines-per-page AT ROW 14.1 COL 81 COLON-ALIGNED
    sl_avail AT ROW 14.33 COL 4 NO-LABELS WIDGET-ID 26
    rd-dest AT ROW 14.57 COL 6 NO-LABELS
    sl_selected AT ROW 14.57 COL 25 NO-LABELS WIDGET-ID 28
    td-show-parm AT ROW 15.05 COL 47
    lv-font-no AT ROW 15.14 COL 35 COLON-ALIGNED
    lv-font-name AT ROW 16.1 COL 28 COLON-ALIGNED NO-LABELS
    fi_file AT ROW 18.48 COL 29 COLON-ALIGNED HELP
    "Enter File Name"
    tb_OpenCSV AT ROW 18.57 COL 75
    tbAutoClose AT ROW 20.29 COL 31 WIDGET-ID 42
    btn-ok AT ROW 21.29 COL 31
    btn-cancel AT ROW 21.29 COL 51
    " Selection Parameters" VIEW-AS TEXT
    SIZE 21.2 BY .71 AT ROW 1.14 COL 5
    " Output Destination" VIEW-AS TEXT
    SIZE 19 BY .62 AT ROW 13.57 COL 5
    "Select/Deselect Material Types" VIEW-AS TEXT
    SIZE 38 BY .62 AT ROW 7.43 COL 28
    FONT 6
    RECT-6 AT ROW 13.86 COL 4
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
        TITLE              = "Transaction History"
        HEIGHT             = 22.48
        WIDTH              = 96
        MAX-HEIGHT         = 45.05
        MAX-WIDTH          = 256
        VIRTUAL-HEIGHT     = 45.05
        VIRTUAL-WIDTH      = 256
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
    begin_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    btn-cancel:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    btn-ok:PRIVATE-DATA IN FRAME FRAME-A = "ribbon-button".

ASSIGN 
    end_cat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_date:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_job-no2:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_procat:PRIVATE-DATA IN FRAME FRAME-A = "parm".

ASSIGN 
    end_rm-no:PRIVATE-DATA IN FRAME FRAME-A = "parm".

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

/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    mat-types:HIDDEN IN FRAME FRAME-A       = TRUE
    mat-types:PRIVATE-DATA IN FRAME FRAME-A = "parm".

/* SETTINGS FOR SELECTION-LIST sl_avail IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_avail:HIDDEN IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
    sl_selected:HIDDEN IN FRAME FRAME-A = TRUE.

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
ON END-ERROR OF C-Win /* Transaction History */
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
ON WINDOW-CLOSE OF C-Win /* Transaction History */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning FG Category */
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


&Scoped-define SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning RM Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
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
        DEFINE VARIABLE v-valid AS LOG NO-UNDO.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}.
        END.
  
        IF rd-dest EQ 3 THEN
        DO:
            ASSIGN 
                fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
            fi_file:SCREEN-VALUE =  cFileName.
        END.
  
        RUN GetSelectionList.
        DO i = 1 TO select-mat:NUM-ITEMS:
            IF select-mat:IS-SELECTED(i) THEN
            DO:
                v-valid = YES.
                LEAVE.
            END.
        END.

        IF v-valid THEN
        DO:
            SESSION:SET-WAIT-STATE("general").
            RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
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
                                "~"OK"~"Want to open CSV file?"
                                VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                                TITLE "" UPDATE lChoice AS LOGICAL.
                 
                            IF lChoice THEN
                            DO:
                                OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName)).
                            END.
                        END.
                    END. /* WHEN 3 THEN DO: */
                WHEN 4 THEN 
                    DO:
              /*run output-to-fax.*/
                        {custom/asifax.i &type= ''
                               &begin_cust= "begin_procat"
                               &END_cust= "begin_procat" 
                               &fax-subject=c-win:title
                               &fax-body=c-win:title
                               &fax-file=list-name }
                    END. 
                WHEN 5 THEN 
                    DO:
                        IF is-xprint-form THEN 
                        DO:
                            {custom/asimail.i &TYPE = ''
                                &begin_cust= "begin_procat"
                                &END_cust= "begin_procat"
                                &mail-subject=c-win:title
                                &mail-body=c-win:title
                                &mail-file=list-name }
                        END.
                        ELSE 
                        DO:
                            {custom/asimailr.i &TYPE = ''
                                     &begin_cust="begin_procat"
                                     &END_cust="begin_procat"
                                     &mail-subject=c-win:title
                                     &mail-body=c-win:title
                                     &mail-file=list-name }
                        END.
                    END.
            END CASE.
     
     
            IF tbAutoClose:CHECKED THEN 
                APPLY 'CLOSE' TO THIS-PROCEDURE.
     
        END.
        ELSE
            MESSAGE "No Material Type Selected."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
    DO:
        DEFINE VARIABLE cTextSelected AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTextListed   AS CHARACTER NO-UNDO.

        RUN displaySelectionList2.

        ASSIGN 
            cTextSelected = sl_selected:LIST-ITEMS
            cTextListed   = sl_avail:LIST-ITEMS.

        IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

        ASSIGN 
            sl_selected:LIST-ITEMS = cTextSelected
            sl_avail:LIST-ITEMS    = cTextListed.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending FG Category */
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


&Scoped-define SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending RM Category */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME mat-types
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mat-types C-Win
ON LEAVE OF mat-types IN FRAME FRAME-A /* Material Types */
    DO:
        ASSIGN {&self-name}.
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


&Scoped-define SELF-NAME select-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL select-mat C-Win
ON VALUE-CHANGED OF select-mat IN FRAME FRAME-A
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
DEFINE VARIABLE v-mat-list AS CHARACTER NO-UNDO.
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

    end_date = TODAY.
    RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    RUN enable_UI.
    {methods/nowait.i}
    {sys/inc/reportsConfigNK1.i "MR@" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        RUN DisplaySelectionList2.
        APPLY "entry" TO begin_rm-no.
    END.
cColumnInit   = NO .
RUN pChangeDest.
RUN fill-select-box.

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
    DISPLAY begin_rm-no end_rm-no begin_procat end_procat begin_cat end_cat 
        begin_date end_date begin_job-no begin_job-no2 end_job-no end_job-no2 
        select-mat TG_sort-cat rd-dest fi_file tb_OpenCSV tbAutoClose 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    ENABLE RECT-6 RECT-7 begin_rm-no end_rm-no begin_procat end_procat begin_cat 
        end_cat begin_date end_date begin_job-no begin_job-no2 end_job-no 
        end_job-no2 select-mat TG_sort-cat btn_SelectColumns rd-dest fi_file 
        tb_OpenCSV tbAutoClose btn-ok btn-cancel 
        WITH FRAME FRAME-A IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fill-select-box C-Win 
PROCEDURE fill-select-box :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        v-mat-list = "".

        /* IF RS_material = 1 THEN*/
        FOR EACH mat WHERE INDEX("GIVW",mat.mat) GT 0 NO-LOCK:
        {custom/statusMsg.i "'Processing... '"} 
            v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
        END.
        /*ELSE
           FOR EACH mat NO-LOCK WHERE INDEX("VW",mat.mat) GT 0 BY mat.mat DESC:
              v-mat-list = v-mat-list + STRING(mat.mat,"x(5)") + " " + mat.dscr + ",".
           END.*/

        IF SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) EQ "," THEN
            SUBSTR(v-mat-list,LENGTH(TRIM(v-mat-list)),1) = "".

        select-mat:LIST-ITEMS = v-mat-list.
    END.

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

     IF NOT OKpressed THEN  RETURN NO-APPLY.  */

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
            fi_file:SCREEN-VALUE = "c:\tmp\r-inkglu.csv".   
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-coat-wax C-Win 
PROCEDURE print-coat-wax :
    /*{sys/form/r-topw.f}*/

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

    DEFINE VARIABLE cslist      AS CHARACTER NO-UNDO.
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


        IF LOOKUP(ttRptSelected.TextList, "Board Weight,Wax-Coat Weight,Produced,Qty Issued/Lbs") <> 0    THEN
            ASSIGN
                str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
            str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
    END.

    DEFINE BUFFER b-item FOR ITEM.

    {custom/statusMsg.i "'Processing... '"} 

    FOR EACH tt-wax-coats:
        DELETE tt-wax-coats.
    END.



    DEFINE VARIABLE v-hdr AS CHARACTER INIT "Issue Date,Job#,Board,Sheets Issued,Wax/Coat,Board MSF,Board Weight,Pickup%,Wax/Coat Weight,Product Category,Produced" NO-UNDO.

    ASSIGN
        str-tit2   = c-win:TITLE
        {sys/inc/ctrtext.i str-tit2 112}

        v-fitem    = begin_rm-no
        v-titem    = end_rm-no
        v-fpcat    = begin_procat
        v-tpcat    = end_procat
        v-fdate    = begin_date
        v-tdate    = end_date
        v-fjob     = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
              TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
        v-tjob     = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
              TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
        v-export   = rd-dest EQ 3
        v-exp-name = cFileName
        v-fCat     = begin_cat
        v-tCat     = END_cat
        v-mtype    = "".

    DO WITH FRAME {&FRAME-NAME}:          
        DO i = 1 TO select-mat:NUM-ITEMS:
            IF select-mat:IS-SELECTED(i) THEN
                v-mtype = v-mtype + TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) + ",".
        END.

        IF SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
            SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

        mat-types = v-mtype.
        DO i = 1 TO LENGTH(mat-types):
            IF SUBSTR(mat-types,i,1) EQ "," THEN 
                SUBSTR(mat-types,i,1) = " ".
        END.
        DISPLAY mat-types.
        mat-types:HIDDEN = YES.
    END.

    v-qty = 0.
    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN 
        RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    IF v-export THEN 
    DO:
        OUTPUT STREAM s-temp TO VALUE(v-exp-name).
        PUT STREAM s-temp UNFORMATTED excelheader SKIP.
    END.

    FOR EACH rm-rcpth WHERE rm-rcpth.company    EQ cocode
        AND rm-rcpth.i-no       GE v-fitem
        AND rm-rcpth.i-no       LE v-titem
        AND rm-rcpth.trans-date GE v-fdate
        AND rm-rcpth.trans-date LE v-tdate
        AND rm-rcpth.rita-code  EQ "I"
        USE-INDEX i-no NO-LOCK,
        EACH rm-rdtlh WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        AND rm-rdtlh.job-no    GE substr(v-fjob,1,6)
        AND rm-rdtlh.job-no    LE substr(v-tjob,1,6)
        AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
        TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") GE v-fjob
        AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
        TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") LE v-tjob NO-LOCK,
        FIRST item WHERE item.company EQ cocode
        AND item.i-no    EQ rm-rcpth.i-no
        AND item.procat  GE v-fpcat
        AND item.procat  LE v-tpcat
        AND INDEX(v-mtype,item.mat-type) GT 0
        AND INDEX("VWGI",item.mat-type) GT 0 NO-LOCK
        BREAK BY rm-rcpth.trans-date
        BY rm-rcpth.job-no
        BY rm-rcpth.job-no2
        BY item.mat-type
        BY item.i-no
        BY rm-rdtlh.s-num
        BY rm-rdtlh.b-num TRANSACTION:

        {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

        IF FIRST-OF(rm-rcpth.trans-date) THEN v-first[1] = YES.

        v-job-no = FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
            TRIM(rm-rdtlh.job-no) + "-" + STRING(rm-rdtlh.job-no2,"99").

        IF v-job-no BEGINS "-" THEN v-job-no = "".

        v-rm-qty = rm-rdtlh.qty.

        IF rm-rcpth.pur-uom NE "LB" THEN
            RUN sys/ref/convquom.p(rm-rcpth.pur-uom, "LB", 0, 0, 0, 0, v-rm-qty, OUTPUT v-rm-qty).

        ASSIGN
            v-qty[1] = v-qty[1] + v-rm-qty
            v-qty[3] = v-qty[3] + v-rm-qty.

        IF LAST-OF(rm-rdtlh.b-num) THEN 
        DO: 
            ASSIGN
                v-board   = ""
                v-brd-qty = 0
                v-m-code  = ""
                v-len     = 0
                v-wid     = 0
                v-basis-w = 0.

            FOR EACH job WHERE job.company EQ rm-rdtlh.company
                AND job.job-no  EQ rm-rdtlh.job-no
                AND job.job-no2 EQ rm-rdtlh.job-no2 NO-LOCK,
                EACH job-hdr NO-LOCK WHERE job-hdr.company = job.company
                AND job-hdr.job-no  = job.job-no
                AND job-hdr.job-no2 = job.job-no2
                AND (job-hdr.frm = rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                AND (job-hdr.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0),
                FIRST itemfg WHERE itemfg.company = job-hdr.company
                AND itemfg.i-no    = job-hdr.i-no
                AND itemfg.procat  GE v-fCat
                AND itemfg.procat  LE v-tCat NO-LOCK:

                FOR EACH job-mat WHERE job-mat.company EQ job.company
                    AND job-mat.job     EQ job.job
                    AND job-mat.job-no  EQ job.job-no
                    AND job-mat.job-no2 EQ job.job-no2
                    AND (job-mat.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                    AND (job-mat.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)
                    USE-INDEX seq-idx NO-LOCK,
                    FIRST b-item WHERE b-item.company  EQ job-mat.company
                    AND b-item.i-no     EQ job-mat.i-no
                    AND INDEX("1234BPR",b-item.mat-type) GT 0 NO-LOCK:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                    IF v-board[1] EQ "" THEN 
                        v-board[1] = b-item.i-no.
                    ASSIGN
                        v-len     = job-mat.len
                        v-wid     = job-mat.wid
                        v-basis-w = job-mat.basis-w.


                    FOR EACH mat-act WHERE mat-act.company EQ cocode
                        AND mat-act.job     EQ job-mat.job
                        AND mat-act.job-no  EQ job-mat.job-no
                        AND mat-act.job-no2 EQ job-mat.job-no2
                        AND mat-act.s-num   EQ job-mat.frm
                        AND mat-act.b-num   EQ job-mat.blank-no
                        AND mat-act.i-no    EQ job-mat.i-no
                        USE-INDEX job NO-LOCK:

                        IF v-board[2] EQ "" THEN 
                            v-board[2] = b-item.i-no.

                        RUN sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                            job-mat.len, job-mat.wid, b-item.s-dep,
                            mat-act.qty, OUTPUT v-brd-qty[1]).

                        v-brd-qty[2] = v-brd-qty[2] + v-brd-qty[1].
                    END. 
                END. 

                FOR EACH mch-act WHERE mch-act.company EQ cocode
                    AND mch-act.job-no  EQ job.job-no
                    AND mch-act.job-no2 EQ job.job-no2
                    AND (mch-act.frm    EQ rm-rdtlh.s-num OR mch-act.frm EQ 0)
                    AND (mat-act.b-num EQ rm-rdtlh.b-num OR mat-act.b-num EQ 0)  NO-LOCK,

                    FIRST mach WHERE mach.company EQ cocode
                    AND mach.loc     EQ locode 
                    AND mach.m-code  EQ mch-act.m-code
                    AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                    mach.dept[2] EQ "GL" OR
                    mach.dept[3] EQ "GL" OR
                    mach.dept[4] EQ "GL")) OR
                    (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                    mach.dept[2] EQ "PR" OR
                    mach.dept[3] EQ "PR" OR
                    mach.dept[4] EQ "PR"))) NO-LOCK
                    BREAK BY mch-act.frm      DESCENDING
                    BY mch-act.blank-no DESCENDING:

                    IF (item.mat-type EQ "G"               AND
                        mch-act.frm      EQ rm-rdtlh.s-num AND
                        mch-act.blank-no EQ rm-rdtlh.b-num AND
                        mch-act.dept EQ "GL")                   OR
                        (item.mat-type EQ "I" AND
                        mch-act.dept EQ "PR")                   OR
                        last(mch-act.frm)                        THEN 
                    DO:

                        v-m-code = mach.m-code.
                        LEAVE.
                    END.
                END. 
                          
                IF v-m-code EQ "" THEN
                    FOR EACH job-mch WHERE job-mch.company EQ cocode
                        AND job-mch.job     EQ job.job
                        AND job-mch.job-no  EQ job.job-no
                        AND job-mch.job-no2 EQ job.job-no2
                        AND (job-mch.frm    EQ rm-rdtlh.s-num OR job-mch.frm EQ 0)
                        AND (job-mch.blank-no EQ rm-rdtlh.b-num OR job-mch.blank-no EQ 0) NO-LOCK,
                        FIRST mach WHERE mach.company EQ cocode
                        AND mach.loc     EQ locode 
                        AND mach.m-code  EQ job-mch.m-code
                        AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                        mach.dept[2] EQ "GL" OR
                        mach.dept[3] EQ "GL" OR
                        mach.dept[4] EQ "GL")) OR
                        (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                        mach.dept[2] EQ "PR" OR
                        mach.dept[3] EQ "PR" OR
                        mach.dept[4] EQ "PR"))) NO-LOCK

                        BREAK BY job-mch.frm      DESCENDING
                        BY job-mch.blank-no DESCENDING:

                   {custom/statusMsg.i "'Processing Item # ' + string(rm-rcpth.i-no)"} 

                        IF (item.mat-type    EQ "G"               AND
                            job-mch.frm      EQ rm-rdtlh.s-num    AND
                            job-mch.blank-no EQ rm-rdtlh.b-num    AND
                            job-mch.dept EQ "GL")                  OR
                            (item.mat-type EQ "I"                  AND
                            job-mch.dept EQ "PR")                  OR
                            LAST(job-mch.frm)                          THEN 
                        DO:

                            v-m-code = mach.m-code.
                            LEAVE.
                        END.
                    END. 

                IF v-board[2] EQ "" THEN 
                    v-board[2] = v-board[1].

                ASSIGN
                    v-board-msf       = 0
                    v-board-weight    = 0
                    v-wax-coat-weight = 0
                    v-board-msf       = (v-brd-qty[2] * ((v-len * v-wid) / 144)) / 1000
                    v-board-weight    = (v-board-msf * v-basis-w)
                    v-wax-coat-weight = v-board-weight * item.shrink.


                CREATE tt-wax-coats.
                ASSIGN
                    tt-wax-coats.trans-date      = rm-rcpth.trans-date
                    tt-wax-coats.job-no          = v-job-no
                    tt-wax-coats.qty             = job-hdr.qty
                    tt-wax-coats.board           = v-board[2] 
                    tt-wax-coats.brd-qty         = v-brd-qty[2]
                    tt-wax-coats.i-no            = rm-rcpth.i-no
                    tt-wax-coats.board-msf       = v-board-msf
                    tt-wax-coats.board-weight    = v-board-weight
                    tt-wax-coats.shrink          = ITEM.shrink 
                    tt-wax-coats.wax-coat-weight = v-wax-coat-weight
                    tt-wax-coats.procat          = itemfg.procat 
                    tt-wax-coats.m-code          = v-m-code 
                    tt-wax-coats.qty-iss         = v-qty[1]
                    .
                ASSIGN 
                    v-qty[1] = 0 .
            END.
        END.

    END.
    ASSIGN
        v-tot-board-weight          = 0
        v-tot-wax-coat-weight       = 0
        v-tot-job-hdr-qty           = 0
        v-grand-tot-board-weight    = 0
        v-grand-tot-wax-coat-weight = 0
        v-grand-tot-job-hdr-qty     = 0
        v-tot-trans-qty             = 0
        v-grand-tot-trans-qty       = 0 .

    IF TG_sort-cat = NO THEN 
    DO:
        FOR EACH tt-wax-coats BREAK BY tt-wax-coats.trans-date:

        {custom/statusMsg.i "'Processing Item # ' + string(tt-wax-coats.i-no)"} 

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rm-item"    THEN 
                        cVarValue = STRING(tt-wax-coats.i-no,"x(10)") .
                    WHEN "date"   THEN 
                        cVarValue =  STRING(tt-wax-coats.trans-date,"99/99/9999").
                    WHEN "job"   THEN 
                        cVarValue = STRING(tt-wax-coats.job-no,"x(9)").
                    WHEN "board"  THEN 
                        cVarValue = STRING(tt-wax-coats.board,"x(8)") .
                    WHEN "sht-iss"   THEN 
                        cVarValue = STRING(tt-wax-coats.brd-qty,"->>>>,>>>,>>9") .
                    WHEN "brd-msf"  THEN 
                        cVarValue = STRING(tt-wax-coats.board-msf,"->>,>>9.99") .
                    WHEN "brd-wit"   THEN 
                        cVarValue =  STRING(tt-wax-coats.board-weight,"->>>>,>>9.99") .
                    WHEN "pck"  THEN 
                        cVarValue =  STRING(tt-wax-coats.shrink,">>9.9999")  .

                    WHEN "wax-coat"  THEN 
                        cVarValue =  STRING(tt-wax-coats.wax-coat-weight,"->>>,>>>,>>9.99")  .
                    WHEN "prod"  THEN 
                        cVarValue =  STRING(tt-wax-coats.qty,"->,>>>,>>>,>>9")  .
                    WHEN "qty-iss"  THEN 
                        cVarValue =  STRING(tt-wax-coats.qty-iss,"->,>>>,>>9.99999")  .
                    WHEN "mach"   THEN 
                        cVarValue =  STRING(tt-wax-coats.m-code,"x(7)")  .
                    WHEN "fg-cat"  THEN 
                        cVarValue =  STRING(tt-wax-coats.procat,"x(11)") .
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            ASSIGN 
                v-tot-board-weight          = v-tot-board-weight + tt-wax-coats.board-weight
                v-tot-wax-coat-weight       = v-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
                v-tot-job-hdr-qty           = v-tot-job-hdr-qty + tt-wax-coats.qty
                v-grand-tot-board-weight    = v-grand-tot-board-weight + tt-wax-coats.board-weight
                v-grand-tot-wax-coat-weight = v-grand-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
                v-grand-tot-job-hdr-qty     = v-grand-tot-job-hdr-qty + tt-wax-coats.qty.
            ASSIGN 
                v-tot-trans-qty       = v-tot-trans-qty + tt-wax-coats.qty-iss
                v-grand-tot-trans-qty = v-grand-tot-trans-qty + tt-wax-coats.qty-iss.

            IF LAST-OF(tt-wax-coats.trans-date) THEN 
            DO:
                /*DISPLAY      
                   v-tot-board-weight 
                   v-tot-wax-coat-weight
                   v-tot-job-hdr-qty
                   SKIP
                   WITH FRAME item-b NO-LABELS.*/
                PUT str-line SKIP.
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "rm-item"    THEN 
                            cVarValue =  "" .
                        WHEN "date"   THEN 
                            cVarValue =   "" .
                        WHEN "job"   THEN 
                            cVarValue =  "".
                        WHEN "board"  THEN 
                            cVarValue =  "" .
                        WHEN "sht-iss"   THEN 
                            cVarValue = "" .
                        WHEN "brd-msf"  THEN 
                            cVarValue = "" .
                        WHEN "brd-wit"   THEN 
                            cVarValue =  STRING(v-tot-board-weight,"->>>>,>>9.99") .
                        WHEN "pck"  THEN 
                            cVarValue =  ""  .

                        WHEN "wax-coat"  THEN 
                            cVarValue =  STRING(v-tot-wax-coat-weight,"->>>,>>>,>>9.99")  .
                        WHEN "prod"  THEN 
                            cVarValue =  STRING(v-tot-job-hdr-qty,"->,>>>,>>>,>>9")  .
                        WHEN "qty-iss"  THEN 
                            cVarValue =  STRING(v-tot-trans-qty,"->,>>>,>>9.99999")  .
                        WHEN "mach"   THEN 
                            cVarValue =  ""  .
                        WHEN "fg-cat"  THEN 
                            cVarValue =  "" .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED  
                    "            SUB TOTAL: " SUBSTRING(cDisplay,24,300) SKIP(1).
                IF rd-dest EQ 3 THEN 
                DO:
                    PUT STREAM s-temp UNFORMATTED 
                        ' SUB TOTAL ,'  SUBSTRING(cExcelDisplay,4,300) SKIP(1).
                END.
                ASSIGN 
                    v-tot-board-weight    = 0
                    v-tot-wax-coat-weight = 0
                    v-tot-job-hdr-qty     = 0
                    v-tot-trans-qty       = 0.
            END.


        END.

        /*DISPLAY
           v-grand-tot-board-weight 
           v-grand-tot-wax-coat-weight
           v-grand-tot-job-hdr-qty
           SKIP
           WITH FRAME item-c NO-LABELS.*/
        PUT str-line SKIP.
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "rm-item"    THEN 
                    cVarValue =  "" .
                WHEN "date"   THEN 
                    cVarValue =   "" .
                WHEN "job"   THEN 
                    cVarValue =  "".
                WHEN "board"  THEN 
                    cVarValue =  "" .
                WHEN "sht-iss"   THEN 
                    cVarValue = "" .
                WHEN "brd-msf"  THEN 
                    cVarValue = "" .
                WHEN "brd-wit"   THEN 
                    cVarValue =  STRING(v-grand-tot-board-weight,"->>>>,>>9.99") .
                WHEN "pck"  THEN 
                    cVarValue =  ""  .

                WHEN "wax-coat"  THEN 
                    cVarValue =  STRING(v-grand-tot-wax-coat-weight,"->>>,>>>,>>9.99")  .
                WHEN "prod"  THEN 
                    cVarValue =  STRING(v-grand-tot-job-hdr-qty,"->,>>>,>>>,>>9")  .
                WHEN "qty-iss"  THEN 
                    cVarValue =  STRING(v-grand-tot-trans-qty,"->,>>>,>>9.99999")  .
                WHEN "mach"   THEN 
                    cVarValue =  ""  .
                WHEN "fg-cat"  THEN 
                    cVarValue =  "" .
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED  
            "            GRAND TOTAL: " SUBSTRING(cDisplay,26,300) SKIP(1).
        IF rd-dest EQ 3 THEN 
        DO:
            PUT STREAM s-temp UNFORMATTED 
                ' GRAND TOTAL ,'  SUBSTRING(cExcelDisplay,4,300) SKIP(1).
        END.

    END.
    ELSE 
    DO:
        FOR EACH tt-wax-coats BREAK BY tt-wax-coats.procat
            BY tt-wax-coats.trans-date:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-wax-coats.i-no)"} 

            ASSIGN 
                cDisplay       = ""
                cTmpField      = ""
                cVarValue      = ""
                cExcelDisplay  = ""
                cExcelVarValue = "".
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                CASE cTmpField:             
                    WHEN "rm-item"    THEN 
                        cVarValue = STRING(tt-wax-coats.i-no,"x(10)") .
                    WHEN "date"   THEN 
                        cVarValue =  STRING(tt-wax-coats.trans-date,"99/99/9999").
                    WHEN "job"   THEN 
                        cVarValue = STRING(tt-wax-coats.job-no,"x(9)").
                    WHEN "board"  THEN 
                        cVarValue = STRING(tt-wax-coats.board,"x(8)") .
                    WHEN "sht-iss"   THEN 
                        cVarValue = STRING(tt-wax-coats.brd-qty,"->>>>,>>>,>>9") .
                    WHEN "brd-msf"  THEN 
                        cVarValue = STRING(tt-wax-coats.board-msf,"->>,>>9.99") .
                    WHEN "brd-wit"   THEN 
                        cVarValue =  STRING(tt-wax-coats.board-weight,"->>>>,>>9.99") .
                    WHEN "pck"  THEN 
                        cVarValue =  STRING(tt-wax-coats.shrink,">>9.9999")  .

                    WHEN "wax-coat"  THEN 
                        cVarValue =  STRING(tt-wax-coats.wax-coat-weight,"->>>,>>>,>>9.99")  .
                    WHEN "prod"  THEN 
                        cVarValue =  STRING(tt-wax-coats.qty,"->,>>>,>>>,>>9")  .
                    WHEN "qty-iss"  THEN 
                        cVarValue =  STRING(tt-wax-coats.qty-iss,"->,>>>,>>9.99999")  .
                    WHEN "mach"   THEN 
                        cVarValue =  STRING(tt-wax-coats.m-code,"x(7)")  .
                    WHEN "fg-cat"  THEN 
                        cVarValue =  STRING(tt-wax-coats.procat,"x(11)") .
                END CASE.

                cExcelVarValue = cVarValue.
                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN 
            DO:
                PUT STREAM s-temp UNFORMATTED  
                    cExcelDisplay SKIP.
            END.

            ASSIGN 
                v-tot-board-weight          = v-tot-board-weight + tt-wax-coats.board-weight
                v-tot-wax-coat-weight       = v-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
                v-tot-job-hdr-qty           = v-tot-job-hdr-qty + tt-wax-coats.qty
                v-grand-tot-board-weight    = v-grand-tot-board-weight + tt-wax-coats.board-weight
                v-grand-tot-wax-coat-weight = v-grand-tot-wax-coat-weight + tt-wax-coats.wax-coat-weight
                v-grand-tot-job-hdr-qty     = v-grand-tot-job-hdr-qty + tt-wax-coats.qty.
            ASSIGN 
                v-tot-trans-qty       = v-tot-trans-qty + tt-wax-coats.qty-iss
                v-grand-tot-trans-qty = v-grand-tot-trans-qty + tt-wax-coats.qty-iss.

            IF LAST-OF(tt-wax-coats.procat) THEN 
            DO:
                /* DISPLAY      
                    v-tot-board-weight 
                    v-tot-wax-coat-weight
                    v-tot-job-hdr-qty
                    SKIP
                    WITH FRAME item-b NO-LABELS.*/
                PUT str-line SKIP.
                ASSIGN 
                    cDisplay       = ""
                    cTmpField      = ""
                    cVarValue      = ""
                    cExcelDisplay  = ""
                    cExcelVarValue = "".
                DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                    cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                        WHEN "rm-item"    THEN 
                            cVarValue =  "" .
                        WHEN "date"   THEN 
                            cVarValue =   "" .
                        WHEN "job"   THEN 
                            cVarValue =  "".
                        WHEN "board"  THEN 
                            cVarValue =  "" .
                        WHEN "sht-iss"   THEN 
                            cVarValue = "" .
                        WHEN "brd-msf"  THEN 
                            cVarValue = "" .
                        WHEN "brd-wit"   THEN 
                            cVarValue =  STRING(v-tot-board-weight,"->>>>,>>9.99") .
                        WHEN "pck"  THEN 
                            cVarValue =  ""  .

                        WHEN "wax-coat"  THEN 
                            cVarValue =  STRING(v-tot-wax-coat-weight,"->>>,>>>,>>9.99")  .
                        WHEN "prod"  THEN 
                            cVarValue =  STRING(v-tot-job-hdr-qty,"->,>>>,>>>,>>9")  .
                        WHEN "qty-iss"  THEN 
                            cVarValue =  STRING(v-tot-trans-qty,"->,>>>,>>9.99999")  .
                        WHEN "mach"   THEN 
                            cVarValue =  ""  .
                        WHEN "fg-cat"  THEN 
                            cVarValue =  "" .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                END.

                PUT UNFORMATTED  
                    "            SUB TOTAL: " SUBSTRING(cDisplay,24,300) SKIP(1).
                IF rd-dest EQ 3 THEN 
                DO:
                    PUT STREAM s-temp UNFORMATTED 
                        ' SUB TOTAL ,'  SUBSTRING(cExcelDisplay,4,300) SKIP(1).
                END.

                ASSIGN 
                    v-tot-board-weight    = 0
                    v-tot-wax-coat-weight = 0
                    v-tot-job-hdr-qty     = 0
                    v-tot-trans-qty       = 0 .
            END.

        END.

        /*DISPLAY
           v-grand-tot-board-weight 
           v-grand-tot-wax-coat-weight
           v-grand-tot-job-hdr-qty
           SKIP
           WITH FRAME item-c NO-LABELS.*/
        PUT str-line SKIP.
        ASSIGN 
            cDisplay       = ""
            cTmpField      = ""
            cVarValue      = ""
            cExcelDisplay  = ""
            cExcelVarValue = "".
        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            CASE cTmpField:             
                WHEN "rm-item"    THEN 
                    cVarValue =  "" .
                WHEN "date"   THEN 
                    cVarValue =   "" .
                WHEN "job"   THEN 
                    cVarValue =  "".
                WHEN "board"  THEN 
                    cVarValue =  "" .
                WHEN "sht-iss"   THEN 
                    cVarValue = "" .
                WHEN "brd-msf"  THEN 
                    cVarValue = "" .
                WHEN "brd-wit"   THEN 
                    cVarValue =  STRING(v-grand-tot-board-weight,"->>>>,>>9.99") .
                WHEN "pck"  THEN 
                    cVarValue =  ""  .

                WHEN "wax-coat"  THEN 
                    cVarValue =  STRING(v-grand-tot-wax-coat-weight,"->>>,>>>,>>9.99")  .
                WHEN "prod"  THEN 
                    cVarValue =  STRING(v-grand-tot-job-hdr-qty,"->,>>>,>>>,>>9")  .
                WHEN "qty-iss"  THEN 
                    cVarValue =  STRING(v-grand-tot-trans-qty,"->,>>>,>>9.99999")  .
                WHEN "mach"   THEN 
                    cVarValue =  ""  .
                WHEN "fg-cat"  THEN 
                    cVarValue =  "" .
            END CASE.

            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
        END.

        PUT UNFORMATTED  
            "            GRAND TOTAL: " SUBSTRING(cDisplay,26,300) SKIP(1).
        IF rd-dest EQ 3 THEN 
        DO:
            PUT STREAM s-temp UNFORMATTED 
                ' GRAND TOTAL ,'  SUBSTRING(cExcelDisplay,4,300) SKIP(1).
        END.
    END.

    IF v-export THEN 
    DO:
        OUTPUT STREAM s-temp CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-inks-glues C-Win 
PROCEDURE print-inks-glues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    {sys/form/r-topw.f}

    DEFINE BUFFER b-item FOR ITEM.

    FORM tt-inks-glues.trans-date     LABEL "Issue Date"
        v-job-no               LABEL "   Job #"
        v-board[2]             LABEL "Board"
        v-brd-qty[2]           LABEL "Sheets Issued"
        rm-rcpth.i-no          LABEL "Ink/Glue"
        v-qty[1]               LABEL "Qty Issued/Lbs"
        v-m-code               LABEL "Machine"
        tt-inks-glues.procat   LABEL "FG Category"

        SKIP
        WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 132.

    FORM    
        "----------------"    AT 59
        SKIP
        "SUB TOTAL"           AT 37
        v-tot-trans-qty       FORMAT "->,>>>,>>>,>>9.9<<<<" AT 59           
        SKIP(2)
        WITH FRAME itemb NO-BOX DOWN NO-LABELS NO-ATTR STREAM-IO WIDTH 132.

    FORM    
        "----------------"    AT 59
        SKIP
        "GRAND TOTAL"         AT 37
        v-grand-tot-trans-qty       FORMAT "->,>>>,>>>,>>9.9<<<<" AT 59           
        SKIP(2)
        WITH FRAME itemc NO-BOX DOWN NO-LABELS NO-ATTR STREAM-IO WIDTH 132.

    DEFINE VARIABLE v-hdr AS CHARACTER INIT "Issue Date,Job#,Board,Sheets Issued,Ink/Glue,Qty Issued/Lbs,Machine,FG Category," NO-UNDO.

    ASSIGN
        str-tit2   = c-win:TITLE
   {sys/inc/ctrtext.i str-tit2 112}

        v-fitem    = begin_rm-no
        v-titem    = end_rm-no
        v-fpcat    = begin_procat
        v-tpcat    = end_procat
        v-fdate    = begin_date
        v-tdate    = end_date
        v-fjob     = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) +
              TRIM(begin_job-no) + STRING(INT(begin_job-no2),"99")
        v-tjob     = FILL(" ",6 - LENGTH(TRIM(end_job-no)))   +
              TRIM(end_job-no)   + STRING(INT(end_job-no2),"99")
        v-exp-name = cFileName
        v-fCat     = begin_cat
        v-tCat     = END_cat
        v-mtype    = "".

    FOR EACH tt-inks-glues:
        DELETE tt-inks-glues.
    END.
    DO WITH FRAME {&FRAME-NAME}:          
        DO i = 1 TO select-mat:NUM-ITEMS:
            IF select-mat:IS-SELECTED(i) THEN
                v-mtype = v-mtype + TRIM(SUBSTR(select-mat:ENTRY(i),1,5)) + ",".
        END.

        IF SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) EQ "," THEN
            SUBSTR(v-mtype,LENGTH(TRIM(v-mtype)),1) = "".

        mat-types = v-mtype.

        DO i = 1 TO LENGTH(mat-types):
            IF SUBSTR(mat-types,i,1) EQ "," THEN 
                SUBSTR(mat-types,i,1) = " ".
        END.

        DISPLAY mat-types.

        mat-types:HIDDEN = YES.
    END.

    v-qty = 0.

{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN 
        RUN show-param.

    SESSION:SET-WAIT-STATE ("general").

    DISPLAY "" WITH FRAME r-top.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM s-temp TO VALUE(v-exp-name).
        PUT STREAM s-temp UNFORMATTED v-hdr SKIP.
    END.


    FOR EACH rm-rcpth WHERE rm-rcpth.company    EQ cocode
        AND rm-rcpth.i-no       GE v-fitem
        AND rm-rcpth.i-no       LE v-titem
        AND rm-rcpth.trans-date GE v-fdate
        AND rm-rcpth.trans-date LE v-tdate
        AND rm-rcpth.rita-code  EQ "I"
        USE-INDEX i-no NO-LOCK,
        EACH rm-rdtlh WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        AND rm-rdtlh.job-no    GE substr(v-fjob,1,6)
        AND rm-rdtlh.job-no    LE substr(v-tjob,1,6)
        AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
        TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") GE v-fjob
        AND FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
        TRIM(rm-rdtlh.job-no) + STRING(rm-rdtlh.job-no2,"99") LE v-tjob NO-LOCK,
        FIRST item WHERE item.company EQ cocode
        AND item.i-no    EQ rm-rcpth.i-no
        AND item.procat  GE v-fpcat
        AND item.procat  LE v-tpcat
        AND INDEX(v-mtype,item.mat-type) GT 0
        AND INDEX("GI",item.mat-type) GT 0 NO-LOCK
        BREAK BY rm-rcpth.trans-date
        BY rm-rcpth.job-no
        BY rm-rcpth.job-no2
        BY item.mat-type
        BY item.i-no
        BY rm-rdtlh.s-num
        BY rm-rdtlh.b-num TRANSACTION:

        IF FIRST-OF(rm-rcpth.trans-date) THEN v-first[1] = YES.

        v-job-no = FILL(" ",6 - LENGTH(TRIM(rm-rdtlh.job-no))) +
            TRIM(rm-rdtlh.job-no) + "-" + STRING(rm-rdtlh.job-no2,"99").

        IF v-job-no BEGINS "-" THEN v-job-no = "".

        v-rm-qty = rm-rdtlh.qty.

        IF rm-rcpth.pur-uom NE "LB" THEN
            RUN sys/ref/convquom.p(rm-rcpth.pur-uom, "LB", 0, 0, 0, 0, v-rm-qty, OUTPUT v-rm-qty).

        ASSIGN
            v-qty[1] = v-qty[1] + v-rm-qty
            v-qty[3] = v-qty[3] + v-rm-qty.

        IF LAST-OF(rm-rdtlh.b-num) THEN 
        DO: 
            ASSIGN
                v-board   = ""
                v-brd-qty = 0
                v-m-code  = "".


            FOR EACH job WHERE job.company EQ rm-rdtlh.company
                AND job.job-no  EQ rm-rdtlh.job-no
                AND job.job-no2 EQ rm-rdtlh.job-no2 NO-LOCK,
                EACH job-hdr NO-LOCK WHERE job-hdr.company = job.company
                AND job-hdr.job-no  = job.job-no
                AND job-hdr.job-no2 = job.job-no2
                AND (job-hdr.frm = rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                AND (job-hdr.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0),
                FIRST itemfg WHERE itemfg.company = job-hdr.company
                AND itemfg.i-no    = job-hdr.i-no
                AND itemfg.procat  GE v-fCat
                AND itemfg.procat  LE v-tCat NO-LOCK:

                FOR EACH job-mat WHERE job-mat.company EQ job.company
                    AND job-mat.job     EQ job.job
                    AND job-mat.job-no  EQ job.job-no
                    AND job-mat.job-no2 EQ job.job-no2
                    AND (job-mat.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                    AND (job-mat.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)
                    USE-INDEX seq-idx NO-LOCK,
                    FIRST b-item WHERE b-item.company  EQ job-mat.company
                    AND b-item.i-no     EQ job-mat.i-no
                    AND INDEX("1234BPR",b-item.mat-type) GT 0 NO-LOCK:

                    IF v-board[1] EQ "" THEN 
                        v-board[1] = b-item.i-no.

                    FOR EACH mat-act WHERE mat-act.company EQ cocode
                        AND mat-act.job     EQ job-mat.job
                        AND mat-act.job-no  EQ job-mat.job-no
                        AND mat-act.job-no2 EQ job-mat.job-no2
                        AND mat-act.s-num   EQ job-mat.frm
                        AND mat-act.b-num   EQ job-mat.blank-no
                        AND mat-act.i-no    EQ job-mat.i-no
                        USE-INDEX job NO-LOCK:

                        IF v-board[2] EQ "" THEN 
                            v-board[2] = b-item.i-no.

                        RUN sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                            job-mat.len, job-mat.wid, b-item.s-dep,
                            mat-act.qty, OUTPUT v-brd-qty[1]).

                        v-brd-qty[2] = v-brd-qty[2] + v-brd-qty[1].
                    END. /* FOR EACH mat-act */
                END. /* FOR EACH job-mat */

                FOR EACH mch-act WHERE mch-act.company EQ cocode
                    AND mch-act.job-no  EQ job.job-no
                    AND mch-act.job-no2 EQ job.job-no2
                    AND (mch-act.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                    AND (mat-act.b-num EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0)  NO-LOCK,

                    FIRST mach WHERE mach.company EQ cocode
                    AND mach.loc     EQ locode 
                    AND mach.m-code  EQ mch-act.m-code
                    AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                    mach.dept[2] EQ "GL" OR
                    mach.dept[3] EQ "GL" OR
                    mach.dept[4] EQ "GL")) OR
                    (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                    mach.dept[2] EQ "PR" OR
                    mach.dept[3] EQ "PR" OR
                    mach.dept[4] EQ "PR"))) NO-LOCK
                    BREAK BY mch-act.frm      DESCENDING
                    BY mch-act.blank-no DESCENDING:

                    IF (item.mat-type EQ "G"               AND
                        mch-act.frm      EQ rm-rdtlh.s-num AND
                        mch-act.blank-no EQ rm-rdtlh.b-num AND
                        mch-act.dept EQ "GL")                   OR
                        (item.mat-type EQ "I" AND
                        mch-act.dept EQ "PR")                   OR
                        last(mch-act.frm)                        THEN 
                    DO:

                        v-m-code = mach.m-code.
                        LEAVE.
                    END.
                END. /* FOR EACH mch-act */

                IF v-m-code EQ "" THEN
                    FOR EACH job-mch WHERE job-mch.company EQ cocode
                        AND job-mch.job     EQ job.job
                        AND job-mch.job-no  EQ job.job-no
                        AND job-mch.job-no2 EQ job.job-no2
                        AND (job-mch.frm    EQ rm-rdtlh.s-num OR rm-rdtlh.s-num EQ 0)
                        AND (job-mch.blank-no EQ rm-rdtlh.b-num OR rm-rdtlh.b-num EQ 0) NO-LOCK,
                        FIRST mach WHERE mach.company EQ cocode
                        AND mach.loc     EQ locode 
                        AND mach.m-code  EQ job-mch.m-code
                        AND ((item.mat-type EQ "G" AND (mach.dept[1] EQ "GL" OR
                        mach.dept[2] EQ "GL" OR
                        mach.dept[3] EQ "GL" OR
                        mach.dept[4] EQ "GL")) OR
                        (item.mat-type EQ "I" AND (mach.dept[1] EQ "PR" OR
                        mach.dept[2] EQ "PR" OR
                        mach.dept[3] EQ "PR" OR
                        mach.dept[4] EQ "PR"))) NO-LOCK

                        BREAK BY job-mch.frm      DESCENDING
                        BY job-mch.blank-no DESCENDING:

                        IF (item.mat-type    EQ "G"               AND
                            job-mch.frm      EQ rm-rdtlh.s-num    AND
                            job-mch.blank-no EQ rm-rdtlh.b-num    AND
                            job-mch.dept EQ "GL")                  OR
                            (item.mat-type EQ "I"                  AND
                            job-mch.dept EQ "PR")                  OR
                            LAST(job-mch.frm)                          THEN 
                        DO:

                            v-m-code = mach.m-code.
                            LEAVE.
                        END.
                    END. /* FOR EACH job-mch */

                IF v-board[2] EQ "" THEN 
                    v-board[2] = v-board[1].
                CREATE tt-inks-glues.
                ASSIGN
                    tt-inks-glues.trans-date = rm-rcpth.trans-date 
                    tt-inks-glues.job-no     = v-job-no
                    tt-inks-glues.qty        = v-qty[1]
                    tt-inks-glues.m-code     = v-m-code
                    tt-inks-glues.board      = v-board[2] 
                    tt-inks-glues.brd-qty    = v-brd-qty[2]
                    tt-inks-glues.i-no       = rm-rcpth.i-no
                    tt-inks-glues.procat     = itemfg.procat.

                /*             DISPLAY                                */
                /*                rm-rcpth.trans-date WHEN v-first[1] */
                /*                v-job-no                            */
                /*                v-board[2]                          */
                /*                v-brd-qty[2]                        */
                /*                rm-rcpth.i-no                       */
                /*                v-qty[1]                            */
                /*                v-m-code                            */
                /*                WITH FRAME itemx.                   */
                /*             DOWN WITH FRAME itemx.                 */


                ASSIGN
                    v-qty[2]   = v-qty[2] + v-qty[1]
                    v-qty[1]   = 0
                    v-first[1] = NO.

            END. /* FOR EACH job */

        /*          IF LAST(rm-rcpth.trans-date) THEN DO: */
        /*             UNDERLINE v-qty[1]                 */
        /*             WITH FRAME itemx.                  */
        /*                                                */
        /*             DISPLAY                            */
        /*                "GRAND TOTALS" @ rm-rcpth.i-no  */
        /*                v-qty[3]       @ v-qty[1]       */
        /*                WITH FRAME itemx.               */
        /*          END.                                  */
        END. /* IF last-of(rm-rdtlh.b-num) */
    END.

    ASSIGN
        v-tot-trans-qty       = 0
        v-grand-tot-trans-qty = 0.

    IF TG_sort-cat = NO THEN 
    DO:
        FOR EACH tt-inks-glues BREAK BY tt-inks-glues.trans-date:
            DISPLAY 
                tt-inks-glues.trans-date  
                WHEN FIRST-OF(tt-inks-glues.trans-date)
                tt-inks-glues.job-no      @ v-job-no
                tt-inks-glues.board       @ v-board[2]
                tt-inks-glues.brd-qty     @ v-brd-qty[2]
                tt-inks-glues.i-no        @ rm-rcpth.i-no
                tt-inks-glues.qty         @ v-qty[1]
                tt-inks-glues.m-code      @ v-m-code
                tt-inks-glues.procat      
                WITH FRAME itemx.
            DOWN WITH FRAME itemx.

            ASSIGN 
                v-tot-trans-qty       = v-tot-trans-qty + tt-inks-glues.qty
                v-grand-tot-trans-qty = v-grand-tot-trans-qty + tt-inks-glues.qty.

            IF LAST-OF(tt-inks-glues.trans-date) THEN 
            DO:
                DISPLAY      
                    v-tot-trans-qty     
                    SKIP
                    WITH FRAME itemb NO-LABELS.
                v-tot-trans-qty = 0.
            END.

            IF rd-dest EQ 3 THEN
                PUT STREAM s-temp UNFORMATTED
                    TRIM(STRING(tt-inks-glues.trans-date,tt-inks-glues.trans-date:FORMAT))
                    + "," +
                    TRIM(tt-inks-glues.job-no)                            + "," +
                    TRIM(tt-inks-glues.board)                             + "," +
                    TRIM(STRING(tt-inks-glues.brd-qty,"->>>>>>>>>9"))     + "," +
                    TRIM(tt-inks-glues.i-no)                              + "," +
                    TRIM(STRING(tt-inks-glues.qty,"->>>>>>>>>9.9<<<<"))   + "," +
                    TRIM(tt-inks-glues.m-code)                            + "," +
                    TRIM(tt-inks-glues.procat)
                    SKIP.
        END.

        DISPLAY      
            v-grand-tot-trans-qty    
            SKIP
            WITH FRAME itemc NO-LABELS.
    END.
    ELSE 
    DO:
        FOR EACH tt-inks-glues BREAK BY tt-inks-glues.procat
            BY tt-inks-glues.trans-date:
            DISPLAY 
                tt-inks-glues.trans-date  
                WHEN FIRST-OF(tt-inks-glues.trans-date)
                tt-inks-glues.job-no      @ v-job-no
                tt-inks-glues.board       @ v-board[2]
                tt-inks-glues.brd-qty     @ v-brd-qty[2]
                tt-inks-glues.i-no        @ rm-rcpth.i-no
                tt-inks-glues.qty         @ v-qty[1]
                tt-inks-glues.m-code      @ v-m-code
                tt-inks-glues.procat      
                WHEN FIRST-OF(tt-inks-glues.procat)
                WITH FRAME itemx.
            DOWN WITH FRAME itemx.

            ASSIGN 
                v-tot-trans-qty       = v-tot-trans-qty + tt-inks-glues.qty
                v-grand-tot-trans-qty = v-grand-tot-trans-qty + tt-inks-glues.qty.

            IF LAST-OF(tt-inks-glues.procat) THEN 
            DO:
                DISPLAY      
                    v-tot-trans-qty     
                    SKIP
                    WITH FRAME itemb NO-LABELS.
                v-tot-trans-qty = 0.
            END.
            IF rd-dest EQ 3 THEN
                PUT STREAM s-temp UNFORMATTED
                    TRIM(STRING(tt-inks-glues.trans-date,tt-inks-glues.trans-date:FORMAT))
                    + "," +
                    TRIM(tt-inks-glues.job-no)                            + "," +
                    TRIM(tt-inks-glues.board)                             + "," +
                    TRIM(STRING(tt-inks-glues.brd-qty,"->>>>>>>>>9"))     + "," +
                    TRIM(tt-inks-glues.i-no)                              + "," +
                    TRIM(STRING(tt-inks-glues.qty,"->>>>>>>>>9.9<<<<"))   + "," +
                    TRIM(tt-inks-glues.m-code)                            + "," +
                    TRIM(tt-inks-glues.procat)
                    SKIP.
        END.

        DISPLAY      
            v-grand-tot-trans-qty    
            SKIP
            WITH FRAME itemc NO-LABELS.
    END.

    IF rd-dest EQ 3 THEN 
    DO:
        OUTPUT STREAM s-temp CLOSE.
        IF tb_OpenCSV THEN
            OS-COMMAND NO-WAIT VALUE(SEARCH(cFileName)).
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
    /* ---------------------------------------------- rm/rep/rmtrans3.p 10/03 JLF */
    /* raw materials - Ink/Glue Consumption Report                                */
    /* -------------------------------------------------------------------------- */

    /*IF RS_material = 1 THEN
       RUN print-inks-glues. */

    RUN print-coat-wax.

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

