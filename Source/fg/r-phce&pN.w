&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fg\r-phce&p.w

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
DEFINE VARIABLE v-postable  AS LOG       NO-UNDO.
DEFINE VARIABLE v-invalid   AS LOG       NO-UNDO.
DEFINE VARIABLE tran-date   AS DATE      NO-UNDO.
DEFINE VARIABLE tran-period AS INTEGER   NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEFINE BUFFER b-fg-rctd FOR fg-rctd.
DEFINE BUFFER b-itemfg  FOR itemfg.
DEFINE BUFFER b-fg-bin  FOR fg-bin.

DEFINE            VARIABLE v-post-date   AS DATE      INIT TODAY NO-UNDO.
DEFINE            VARIABLE v-gl          AS LOG       INIT NO NO-UNDO.

DEFINE            VARIABLE save_id       AS RECID.
DEFINE            VARIABLE v-qty-onh     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-temp-cost   AS DECIMAL   FORMAT "->>>>>9.99" NO-UNDO.
DEFINE            VARIABLE time_stamp    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-cum-qty     AS DECIMAL   FORMAT "->>>>>>9" NO-UNDO.
DEFINE            VARIABLE v-tot-value   AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v-sell-price  LIKE itemfg.sell-price .
DEFINE            VARIABLE v-tot-price   AS DECIMAL   FORMAT "->>>,>>>,>>9.99".
DEFINE            VARIABLE v-item-tot    AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE v-std-cost    AS DECIMAL   FORMAT ">>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-q-adj-ytd   AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-adj-qty     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-dscr        LIKE account.dscr NO-UNDO.
DEFINE            VARIABLE v-disp-actnum LIKE account.actnum NO-UNDO.
DEFINE            VARIABLE v-disp-amt    AS DECIMAL   FORMAT ">>,>>>,>>9.99cr" NO-UNDO.
DEFINE            VARIABLE v-cost        LIKE itemfg.std-tot-cost EXTENT 4 NO-UNDO.
DEFINE            VARIABLE v-uom         LIKE itemfg.prod-uom NO-UNDO.

DEFINE NEW SHARED VARIABLE v-trnum       AS INTEGER.

DEFINE TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd.

DEFINE TEMP-TABLE tt-fg-bin NO-UNDO
    FIELD fgRctdRowID AS ROWID
    FIELD rct-date    AS DATE
    FIELD i-no        AS CHARACTER    
    FIELD i-name      AS CHARACTER
    FIELD part-no     AS CHARACTER
    FIELD sell-price  LIKE itemfg.sell-price
    FIELD job-no      AS CHARACTER  
    FIELD job-no2     AS INTEGER
    FIELD loc         AS CHARACTER
    FIELD loc-bin     AS CHARACTER
    FIELD tag         AS CHARACTER
    FIELD on-hand-qty AS INTEGER
    FIELD counted-qty AS INTEGER
    FIELD pur-uom     AS CHARACTER
    FIELD std-cost    AS DECIMAL   FORMAT ">>>,>>9.99<<"
    FIELD tot-value   AS DECIMAL   FORMAT "->>>,>>>,>>9.99"
    FIELD seq-no      AS INTEGER
    FIELD count-trans AS LOG
    FIELD createGL    AS LOGICAL
    FIELD glDscr      AS CHARACTER
    FIELD v-cost      LIKE itemfg.std-tot-cost EXTENT 4
    FIELD v-uom       AS CHARACTER
    FIELD v-adj-qty   AS INTEGER
    INDEX fgRctdRowID fgRctdRowID
    INDEX i-no   i-no   job-no job-no2 loc loc-bin tag
    INDEX seq-no seq-no.

{oe/invwork.i new}
{sys/FORM/r-topw.f}

{fg/fullset.i NEW}

DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-count-qty        LIKE fg-rctd.t-qty NO-UNDO.
DEFINE VARIABLE v-variance         LIKE fg-rctd.t-qty NO-UNDO.
DEFINE VARIABLE cFieldType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRtnChar           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCreatGlAccount    AS LOGICAL   NO-UNDO .

ASSIGN 
    cTextListToSelect  = "Trans. Date,Item,Description,Customer Part#," +
                           "Job#,Whse,Bin,Tag,O/H Qty," +
                           "Sell Value,Cost/M,Total Cost,Count Qty,Variance,Overs/Unders"
    cFieldListToSelect = "tt-fg-bin.rct-date,tt-fg-bin.i-no,tt-fg-bin.i-name,tt-fg-bin.part-no," +
                            "tt-fg-bin.job-no,tt-fg-bin.loc,tt-fg-bin.loc-bin,tt-fg-bin.tag,tt-fg-bin.on-hand-qty," +
                            "tt-fg-bin.sell-price,tt-fg-bin.std-cost,tt-fg-bin.tot-value,tt-fg-bin.counted-qty,v-variance,over-under"    
    cFieldLength       = "11,15,20,15," + "13,5,8,8,8," + "10,10,10,10,10,12"
    cFieldType         = "C,c,c,c," + "c,c,c,c,i," + "i,i,i,i,i,i".

{sys/inc/ttRptSel.i}
ASSIGN 
    cTextListToDefault = "Trans. Date,Item,Description,Customer Part#," +
                           "Tag,Whse,Bin,Sell Value,Cost/M,Total Cost,Job#".

RUN sys/ref/nk1look.p (INPUT cocode, "AdjustGL", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lCreatGlAccount = LOGICAL(cRtnChar) NO-ERROR.

/*

display tt-fg-bin.rct-date when first-of(tt-fg-bin.i-no)
            tt-fg-bin.i-no     when first-of(tt-fg-bin.i-no)
            tt-fg-bin.i-name
            tt-fg-bin.part-no
            tt-fg-bin.tag
            SUBSTR(tt-fg-bin.tag,16,8) WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no @ tt-fg-bin.tag
            tt-fg-bin.on-hand-qty
            tt-fg-bin.counted-qty WHEN tt-fg-bin.counted-qty NE 0
            tt-fg-bin.sell-price
            /*tt-fg-bin.pur-uom*/
            tt-fg-bin.loc
            tt-fg-bin.loc-bin
            tt-fg-bin.std-cost WHEN tt-fg-bin.count-trans
            tt-fg-bin.tot-value WHEN tt-fg-bin.count-trans
            tt-fg-bin.job-no
            tt-fg-bin.job-no2

    form fg-rctd.rct-date     column-label "TRANS.!DATE"
         fg-rctd.i-no           label "ITEM"
         fg-rctd.i-name         format "x(20)" label "DESCRIPTION"
         itemfg.part-no         format "x(15)" label "Customer Part#"
         fg-rctd.job-no         label "   JOB" space(0) "-" space(0)
         fg-rctd.job-no2        label "# " format "99"
         fg-rctd.loc             label "WHSE"
         fg-rctd.loc-bin         label "BIN"
         fg-rctd.tag             label "TAG" FORM "x(23)"
         fg-rctd.t-qty           format "->>>>>>9" label "QUANTITY"
         v-sell-price            format "->>>,>>>,>>9.99" LABEL "Selling Value"
         fg-rctd.pur-uom        label "UOM"
         v-std-cost        label "COST/UOM"
         v-tot-value             label "TOTAL COST"
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 post-date begin_userid ~
end_userid tg_ShowOHCounted tg_ShowOHNotCounted tg_TotalByItem ~
tg_PrintSubTotal sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS post-date begin_userid end_userid ~
tg_ShowOHCounted tg_ShowOHNotCounted tg_TotalByItem tg_PrintSubTotal ~
sl_avail sl_selected rd-dest tbAutoClose 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE begin_userid AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning User ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_userid AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending User ID" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE post-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Post Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3
     SIZE 16 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 4.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 10.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_CheckQty AS LOGICAL INITIAL no 
     LABEL "Checking Qty On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tg_PrintSubTotal AS LOGICAL INITIAL no 
     LABEL "Print Sub Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tg_show-inv AS LOGICAL INITIAL no 
     LABEL "Show On-Hand Inventory?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ShowOHCounted AS LOGICAL INITIAL no 
     LABEL "Show Items O/H = Counted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE tg_ShowOHNotCounted AS LOGICAL INITIAL no 
     LABEL "Show Items O/H Not = Counted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE tg_TotalByItem AS LOGICAL INITIAL no 
     LABEL "Totals By Item?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     post-date AT ROW 3.38 COL 25 COLON-ALIGNED
     begin_userid AT ROW 5.05 COL 25.2 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     end_userid AT ROW 5.05 COL 66.2 COLON-ALIGNED HELP
          "Enter the Ending User ID"
     tg_ShowOHCounted AT ROW 6.57 COL 30.6 WIDGET-ID 48
     tg_ShowOHNotCounted AT ROW 7.52 COL 30.6 WIDGET-ID 50
     tg_TotalByItem AT ROW 8.43 COL 30.6 WIDGET-ID 52
     tg_PrintSubTotal AT ROW 9.43 COL 30.6 WIDGET-ID 54
     tg_show-inv AT ROW 9.62 COL 61.6 WIDGET-ID 2
     tg_CheckQty AT ROW 10.43 COL 61.6 WIDGET-ID 46
     sl_avail AT ROW 12.19 COL 3 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 12.19 COL 62.8 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 12.24 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.24 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.24 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.24 COL 40 WIDGET-ID 40
     btn_down AT ROW 16.24 COL 40 WIDGET-ID 42
     lv-font-no AT ROW 17.91 COL 34 COLON-ALIGNED
     lv-ornt AT ROW 17.91 COL 44 NO-LABEL
     lines-per-page AT ROW 17.91 COL 87 COLON-ALIGNED
     rd-dest AT ROW 18 COL 5 NO-LABEL
     lv-font-name AT ROW 19.1 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.86 COL 28.2
     tbAutoClose AT ROW 22.05 COL 28.2 WIDGET-ID 64
     btn-ok AT ROW 23 COL 28
     btn-cancel AT ROW 23 COL 51.2
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 60.4 WIDGET-ID 44
     "This procedure will post all finished goods physical count transactions." VIEW-AS TEXT
          SIZE 82 BY .95 AT ROW 2.14 COL 8
          FONT 6
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.48 COL 3 WIDGET-ID 38
     " Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.05 COL 4
     " Output Destination" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 17.24 COL 4
     RECT-6 AT ROW 17.67 COL 3
     RECT-7 AT ROW 1.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95 BY 23.57
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
         TITLE              = "Post Physical Counts"
         HEIGHT             = 23.57
         WIDTH              = 95
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
       begin_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lines-per-page IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lines-per-page:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-name:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN lv-font-no IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-font-no:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET lv-ornt IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lv-ornt:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       post-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX td-show-parm IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       td-show-parm:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tg_CheckQty IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tg_CheckQty:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tg_PrintSubTotal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tg_show-inv IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tg_show-inv:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       tg_ShowOHCounted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg_ShowOHNotCounted:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg_TotalByItem:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Post Physical Counts */
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
ON WINDOW-CLOSE OF C-Win /* Post Physical Counts */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_userid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_userid C-Win
ON LEAVE OF begin_userid IN FRAME FRAME-A /* Beginning User ID */
DO:
        ASSIGN begin_userid.
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
        FOR EACH work-job:
            DELETE work-job.
        END.
      
        DEFINE VARIABLE lv-post AS LOG NO-UNDO.
  
        RUN pCheckDate.
        IF v-invalid THEN RETURN NO-APPLY.

        ASSIGN /*rd-dest
         post-date
         tg_account
         v-post-date = post-date
         tg_show-inv
         v-gl = tg_account
         td-show-parm
         tg_CheckQty tg_ShowOHCounted tg_ShowOHNotCounted tg_totalByItem*/
            {&DISPLAYED-OBJECTS} .

        RUN GetSelectionList.

        /* New business logic program. */
        /*
        RUN fg/phyctpst.p (INPUT begin_userid:SCREEN-VALUE, /* Begin user ID */
                           INPUT end_userid:SCREEN-VALUE, /* End User ID */
                           INPUT v-post-date, /* Post Date */
                           INPUT int(lv-font-no), /* Font number */
                           INPUT lv-ornt, /* orientation */
                           INPUT lines-per-page, /* Lines per page */
                           INPUT c-win:title, /* window title */
                           INPUT td-show-parm, /* show parameters */
                           INPUT tg_show-inv, /* show on-hand inventory */
                           INPUT tg_account, /* create GL accts */
                           INPUT rd-dest). /* Destination (1-printer,2-screen,3-file) */
        */

        IF lCreatGlAccount THEN 
        DO:
            RUN fg/d-fginvp.w (OUTPUT tran-date, OUTPUT tran-period).     
            IF tran-date = ? THEN RETURN NO-APPLY.
        END.
        ASSIGN 
            post-date   = tran-date
            v-post-date = tran-date
            uperiod     = tran-period
            udate       = tran-date
            .

        /* gdm - 12050809*/
        ASSIGN
            begin_userid = begin_userid:SCREEN-VALUE
            end_userid   = end_userid:SCREEN-VALUE .
  
        ASSIGN 
            v-gl = lCreatGlAccount .
  
        RUN run-report-inv.

        CASE rd-dest:
            WHEN 1 THEN RUN output-to-printer.
            WHEN 2 THEN RUN output-to-screen.
            WHEN 3 THEN RUN output-to-file.
        END CASE.

        IF v-postable THEN 
        DO:

            lv-post = NO.

            MESSAGE "Post To Finished Goods?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE lv-post.

            IF lv-post THEN 
            DO:
                RUN cpost.
                MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
            END.
        END.

        ELSE MESSAGE "Nothing available for posting..." VIEW-AS ALERT-BOX ERROR.
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


&Scoped-define SELF-NAME end_userid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_userid C-Win
ON LEAVE OF end_userid IN FRAME FRAME-A /* Ending User ID */
DO:
        ASSIGN end_userid.
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


&Scoped-define SELF-NAME post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL post-date C-Win
ON LEAVE OF post-date IN FRAME FRAME-A /* Post Date */
DO:
        ASSIGN {&self-name}.
    END.
/*
  if lastkey ne -1 then do:
    run check-date.
    if v-invalid then return no-apply.
  end.
END.

  */

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

    post-date = TODAY.
    RUN DisplaySelectionList.
    btn-ok:LOAD-IMAGE("Graphics/32x32/Ok.png").
    btn-cancel:LOAD-IMAGE("Graphics/32x32/cancel.png").
    Btn_Def:LOAD-IMAGE("Graphics/32x32/default.png").
    Btn_Add:LOAD-IMAGE("Graphics/32x32/additem.png").
    Btn_Remove:LOAD-IMAGE("Graphics/32x32/remove.png").
    btn_Up:LOAD-IMAGE("Graphics/32x32/moveup.png").
    btn_down:LOAD-IMAGE("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "IC3" }
    ASSIGN
        td-show-parm:SENSITIVE = lShowParameters
        td-show-parm:HIDDEN    = NOT lShowParameters
        td-show-parm:VISIBLE   = lShowParameters
        .

    {methods/nowait.i}

    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
RUN DisplaySelectionList2.
APPLY "entry" TO post-date.
END.
IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cpost C-Win 
PROCEDURE cpost :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCreateGL AS LOGICAL NO-UNDO.
    
    SESSION:SET-WAIT-STATE("general").
    
    EMPTY TEMP-TABLE work-job.

    IF v-gl AND uperiod GT 0 THEN DO:
        RUN pGetNextGLTransactionNo (OUTPUT v-trnum).
        lCreateGL = TRUE.
    END.
            
    FOR EACH fg-rctd NO-LOCK
        WHERE fg-rctd.company   EQ cocode
          AND fg-rctd.rita-code EQ "C"
          AND ((begin_userid    LE "" AND
                end_userid      GE "") OR
               (fg-rctd.created-by GE begin_userid AND  
                fg-rctd.created-by LE end_userid))
        BREAK BY fg-rctd.i-no 
              BY fg-rctd.loc
              BY fg-rctd.loc-bin
              BY fg-rctd.tag DESCENDING:  
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ cocode
               AND itemfg.i-no    EQ fg-rctd.i-no
             NO-ERROR.
        IF NOT AVAILABLE itemfg THEN 
            NEXT.                    

        RUN pPostCount(ROWID(fg-rctd), v-trnum, lCreateGL, LAST-OF(fg-rctd.i-no), LAST-OF(fg-rctd.loc-bin)).
    END.

    SESSION:SET-WAIT-STATE("").

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

    /*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
    /*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
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
    DEFINE VARIABLE cTmpList      AS CHARACTER NO-UNDO.

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
    sl_avail:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListContents. 

    DO iCount = 1 TO sl_selected:NUM-ITEMS:
        ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
    END.

    cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    DO iCount = 1 TO sl_selected:NUM-ITEMS: /* task 08191414 */
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
  DISPLAY post-date begin_userid end_userid tg_ShowOHCounted tg_ShowOHNotCounted 
          tg_TotalByItem tg_PrintSubTotal sl_avail sl_selected rd-dest 
          tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 post-date begin_userid end_userid tg_ShowOHCounted 
         tg_ShowOHNotCounted tg_TotalByItem tg_PrintSubTotal sl_avail 
         sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down rd-dest 
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
    DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

    IF init-dir = "" THEN init-dir = "c:\temp" .
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

    IF NOT OKpressed THEN  RETURN NO-APPLY.


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
    DEFINE VARIABLE printok   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
    DEFINE VARIABLE result    AS LOGICAL   NO-UNDO.

    /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
         IF NOT printok THEN
         RETURN NO-APPLY.
    */

    /* Use Progress Print. Always use Font#9 in Registry (set above) */
    /*
         RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                                INPUT 3, INPUT 3, INPUT 0, INPUT 0, OUTPUT result).
                                        /* use-dialog(1) and landscape(2) */
    */
    RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt). /* open file-name, title */ 

/*IF NOT RESULT THEN v-postable = NO. */

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
    RUN scr-rpt-d.w (list-name,c-win:TITLE,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCheckDate C-Win 
PROCEDURE pCheckDate :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DO WITH FRAME {&frame-name}:
        v-invalid = NO.
    
        RUN GL_CheckModClosePeriod(INPUT cocode, INPUT DATE(post-date), INPUT "FG", OUTPUT cMessage, OUTPUT lSuccess ) .  
        IF NOT lSuccess THEN 
        DO:
            MESSAGE cMessage VIEW-AS ALERT-BOX INFORMATION.
            v-invalid = YES.
        END.       
    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetNextGLTransactionNo C-Win 
PROCEDURE pGetNextGLTransactionNo :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiTransactionNo AS INTEGER NO-UNDO.
    /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
             WHERE gl-ctrl.company EQ cocode 
             NO-ERROR NO-WAIT.
        IF AVAILABLE gl-ctrl THEN DO:
            ASSIGN 
                opiTransactionNo = gl-ctrl.trnum + 1
                gl-ctrl.trnum    = opiTransactionNo
                .
            FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
            LEAVE.
        END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPostCount C-Win 
PROCEDURE pPostCount :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriFGRctd         AS ROWID   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiGLTransactionNo AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCreateGL        AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplLastItem        AS LOGICAL NO-UNDO.
    DEFINE INPUT  PARAMETER iplLastLocBin      AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE lLastItem AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lLastBin  AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER b2-fg-bin  FOR fg-bin.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    EMPTY TEMP-TABLE w-fg-rctd.
    EMPTY TEMP-TABLE work-job.
    
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    
    MAIN-BLOCK:
    DO TRANSACTION ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FOR FIRST fg-rctd NO-LOCK
            WHERE ROWID(fg-rctd) EQ ipriFGRctd,  
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ fg-rctd.i-no
              AND itemfg.isaset
              AND itemfg.alloc:
    
            RUN fg/fullset.p (ROWID(itemfg)).
    
            FOR EACH tt-fg-set,
                FIRST b-itemfg
                WHERE b-itemfg.company EQ cocode
                  AND b-itemfg.i-no    EQ tt-fg-set.part-no
                NO-LOCK:
    
                x = 1.
                FOR EACH w-fg-rctd BY w-fg-rctd.r-no DESCENDING:
                    LEAVE.
                END.
                IF AVAILABLE w-fg-rctd THEN x = w-fg-rctd.r-no + 1.
                FOR EACH b-fg-rctd NO-LOCK BY b-fg-rctd.r-no DESCENDING:
                    IF b-fg-rctd.r-no GE X THEN X = b-fg-rctd.r-no + 1.
                    LEAVE.
                END.
    
                FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
                IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GE x THEN x = fg-rcpth.r-no + 1.
    
                CREATE w-fg-rctd.
                BUFFER-COPY fg-rctd TO w-fg-rctd
                    ASSIGN
                    w-fg-rctd.i-no   = b-itemfg.i-no
                    w-fg-rctd.i-name = b-itemfg.i-name
                    w-fg-rctd.r-no   = x.
    
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ itemfg.i-no
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                    AND fg-bin.tag     EQ fg-rctd.tag
                    AND fg-bin.job-no  EQ fg-rctd.job-no
                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                    AND fg-bin.cust-no EQ fg-rctd.cust-no
                    USE-INDEX co-ino NO-ERROR.
                v-adj-qty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) * tt-fg-set.part-qty-dec.
    
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-itemfg.i-no
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                    AND fg-bin.tag     EQ fg-rctd.tag
                    AND fg-bin.job-no  EQ fg-rctd.job-no
                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                    AND fg-bin.cust-no EQ fg-rctd.cust-no
                    USE-INDEX co-ino NO-ERROR.
                v-adj-qty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) - v-adj-qty.
    
                IF v-adj-qty LT 0 THEN v-adj-qty = 0.
    
                ASSIGN 
                    w-fg-rctd.t-qty = (fg-rctd.t-qty * tt-fg-set.part-qty-dec) + v-adj-qty.
            END.
        END.
    
        FOR EACH w-fg-rctd
            BREAK BY w-fg-rctd.i-no 
            BY w-fg-rctd.loc
            BY w-fg-rctd.loc-bin
            BY w-fg-rctd.tag DESCENDING:
            ASSIGN
                lLastBin  = LAST-OF(w-fg-rctd.loc-bin)
                lLastItem = LAST-OF(w-fg-rctd.i-no)
                .            
            {fg/fg-cpostLimit.i w-}
        END.
        
        ASSIGN
            lLastBin  = iplLastLocBin
            lLastItem = iplLastItem
            .
            
        FOR FIRST fg-rctd EXCLUSIVE-LOCK
            WHERE ROWID(fg-rctd) EQ ipriFGRctd:             
            {fg/fg-cpostLimit.i}
        END.
        
        FIND FIRST tt-fg-bin
             WHERE tt-fg-bin.fgRctdRowID EQ ipriFGRctd
             NO-ERROR.
        IF AVAILABLE tt-fg-bin THEN DO:
            IF tt-fg-bin.createGL AND tt-fg-bin.count-trans THEN
                RUN oe/invposty.p (
                    INPUT 0, 
                    INPUT tt-fg-bin.i-no, 
                    INPUT tt-fg-bin.v-adj-qty, 
                    INPUT tt-fg-bin.v-uom,
                    INPUT tt-fg-bin.v-cost[1], 
                    INPUT tt-fg-bin.v-cost[2], 
                    INPUT tt-fg-bin.v-cost[3], 
                    INPUT tt-fg-bin.v-cost[4], 
                    INPUT tt-fg-bin.glDscr
                    ).
        END. 
    
        IF iplCreateGL THEN DO:
            FOR EACH work-job BREAK BY work-job.actnum:
                RUN GL_SpCreateGLHist(
                    INPUT cocode,
                    INPUT work-job.actnum,
                    INPUT "ADJUST",
                    INPUT IF work-job.fg THEN "FG Adjustment entries FG" ELSE "FG Adjustment entries COGS",
                    INPUT udate,
                    INPUT IF work-job.fg THEN - work-job.amt ELSE work-job.amt,
                    INPUT ipiGLTransactionNo,
                    INPUT uperiod,
                    INPUT "A",
                    INPUT udate,
                    INPUT work-job.cDesc,
                    INPUT "FG"
                    ).
            END. /* each work-job */
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
DEFINE VARIABLE cDisplay      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO.
    DEFINE BUFFER bitemfg FOR itemfg.
    DEFINE VARIABLE str-tit4     AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5     AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE BUFFER bfg-rctd FOR fg-rctd .

    /*{sys/form/r-top5DL.f} */

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "FINISHED GOODS PHYSICAL COUNT - POSTING REPORT"
        str-tit3 = ""
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (112 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.
    /*
    form fg-rctd.rct-date     column-label "TRANS.!DATE"
         fg-rctd.i-no           label "ITEM"
         fg-rctd.i-name         format "x(20)" label "DESCRIPTION"
         itemfg.part-no         format "x(15)" label "Customer Part#"
         fg-rctd.job-no         label "   JOB" space(0) "-" space(0)
         fg-rctd.job-no2        label "# " format "99"
         fg-rctd.loc             label "WHSE"
         fg-rctd.loc-bin         label "BIN"
         fg-rctd.tag             label "TAG" FORM "x(23)"
         fg-rctd.t-qty           format "->>>>>>9" label "QUANTITY"
         v-sell-price            format "->>>,>>>,>>9.99" LABEL "Selling Value"
         /*fg-rctd.pur-uom        label "UOM"*/
         v-std-cost        label "COST/UOM"
         v-tot-value             label "TOTAL COST"
         SKIP
         with frame itemx no-box down width 175 STREAM-IO.
    */
    FORM v-disp-actnum LABEL "G/L ACCOUNT NUMBER"
        v-dscr        LABEL "DESCRIPTION"
        udate         LABEL "DATE"   
        v-disp-amt    LABEL "AMOUNT" SKIP
        WITH DOWN WIDTH 130 FRAME gldetail STREAM-IO.

    time_stamp = STRING(TIME,"hh:mmam").

    SESSION:SET-WAIT-STATE("general").  
    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
                str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                .        
        ELSE 
            ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                .        
    END.

    {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    DISPLAY str-tit4 str-tit5  WITH WIDTH 210 STREAM-IO FRAME r-top. 
    /*      VIEW FRAME r-top.*/

    v-postable = NO.

    /* gdm - 12050809 */
    FOR EACH fg-rctd NO-LOCK 
        WHERE fg-rctd.company   EQ cocode
        AND fg-rctd.rita-code EQ "C"
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.created-by GE begin_userid 
        AND fg-rctd.created-by LE end_userid,
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
        BREAK BY fg-rctd.i-no:

        ASSIGN
            v-sell-price = itemfg.sell-price .

        IF FIRST-OF(fg-rctd.i-no) THEN 
        DO:
            /* put skip(1).*/
            ASSIGN
                v-cum-qty   = 0
                v-tot-price = 0.
        END.

        FIND FIRST fg-bin
            WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
            AND fg-bin.job-no  EQ fg-rctd.job-no
            AND fg-bin.job-no2 EQ fg-rctd.job-no2
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE fg-bin AND fg-rctd.tag NE "" THEN
            FIND FIRST fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ fg-rctd.i-no
                AND fg-bin.loc     EQ fg-rctd.loc
                AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                AND fg-bin.tag     EQ ""
                AND fg-bin.job-no  EQ fg-rctd.job-no
                AND fg-bin.job-no2 EQ fg-rctd.job-no2
                NO-LOCK NO-ERROR.

        IF AVAILABLE fg-bin AND fg-bin.pur-uom NE "" THEN
        DO:
            IF fg-bin.pur-uom EQ "M" THEN
                v-std-cost = fg-rctd.std-cost.
            ELSE
                RUN sys/ref/convcuom.p(fg-bin.pur-uom, "M", 0, 0, 0, 0,
                    fg-rctd.std-cost, OUTPUT v-std-cost).
        END.
        ELSE
            IF itemfg.prod-uom EQ "M" THEN
                v-std-cost = fg-rctd.std-cost.
            ELSE
                RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                    fg-rctd.std-cost, OUTPUT v-std-cost).

        IF AVAILABLE fg-bin THEN 
        DO: 
            IF fg-bin.job-no NE "" THEN
                FIND LAST oe-ordl WHERE
                    oe-ordl.company EQ fg-bin.company AND
                    oe-ordl.job-no EQ fg-bin.job-no AND
                    oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                    oe-ordl.i-no EQ fg-bin.i-no AND
                    (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                    USE-INDEX job
                    NO-LOCK NO-ERROR.
            ELSE 
                FIND LAST oe-ordl WHERE
                    oe-ordl.company EQ fg-bin.company AND
                    oe-ordl.job-no EQ fg-bin.job-no AND
                    oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                    oe-ordl.i-no EQ fg-bin.i-no AND
                    (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                    USE-INDEX item
                    NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ordl THEN
                ASSIGN
                    v-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) .
        END.

        ASSIGN
            v-tot-value = v-std-cost * fg-rctd.t-qty
            v-cum-qty   = v-cum-qty + fg-rctd.t-qty
            v-tot-price = v-tot-price + v-sell-price .

        BUFFER bfg-rctd:FIND-BY-ROWID(ROWID(fg-rctd), NO-LOCK) .
        BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK) .
        /*
        display fg-rctd.rct-date when first-of(fg-rctd.i-no)
                fg-rctd.i-no       when first-of(fg-rctd.i-no)
                fg-rctd.i-name
                itemfg.part-no
                fg-rctd.tag
                fg-rctd.t-qty
                fg-rctd.pur-uom
                fg-rctd.loc
                fg-rctd.loc-bin
                v-sell-price
                v-std-cost
                v-tot-value
                fg-rctd.job-no
                fg-rctd.job-no2
            with frame itemx.
        down with frame itemx.
        */
        ASSIGN 
            cDisplay  = ""
            cTmpField = ""
            cVarValue = ""
            .

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                IF cTmpField = "Part-no" THEN hField = BUFFER bitemfg:BUFFER-FIELD(cTmpField).
                ELSE hField = BUFFER bfg-rctd:BUFFER-FIELD(cTmpField).
                IF hField <> ? THEN 
                DO:
                    cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                    IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = IF cTmpField <> "" THEN TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cTmpField, fg-rctd.job-no2))) ELSE "".                  

                    IF ENTRY(i,cSelectedList) BEGINS "TAG" THEN 
                        cTmpField = SUBSTRING(fg-rctd.tag, LENGTH(fg-rctd.tag) - 4).

                    cDisplay = cDisplay + cTmpField + 
                        FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                        .

                END.
                ELSE 
                DO:
                    cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).

                END.
            END.
            ELSE 
            DO: 
                CASE cTmpField:               
                    WHEN "v-std-cost" THEN 
                        cVarValue = STRING(v-std-cost).
                    WHEN "v-tot-value" THEN 
                        cVarValue = STRING(v-tot-value).
                END CASE.

                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 

            END.
        END.
        PUT UNFORMATTED cDisplay SKIP.

        IF AVAILABLE fg-bin THEN
            ASSIGN
                v-cost[1] = fg-bin.std-lab-cost
                v-cost[2] = fg-bin.std-fix-cost
                v-cost[3] = fg-bin.std-var-cost
                v-cost[4] = fg-bin.std-mat-cost
                v-uom     = fg-bin.pur-uom.
        ELSE   
            ASSIGN
                v-cost[1] = itemfg.std-lab-cost
                v-cost[2] = itemfg.std-fix-cost
                v-cost[3] = itemfg.std-var-cost
                v-cost[4] = itemfg.std-mat-cost
                v-uom     = itemfg.prod-uom.

        v-adj-qty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) - fg-rctd.t-qty.
        
        cDescription = IF fg-rctd.job-no NE "" THEN "Job: " + 
                       TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', fg-rctd.job-no, fg-rctd.job-no2)))
                       ELSE IF fg-rctd.po-no NE "" THEN "PO: " + string(fg-rctd.po-no,"999999") + "-" + STRING(fg-rctd.po-line,"999") ELSE "".

        /*Invoicing  - Post Invoicing Transactions - Job Costing*/
        RUN oe/invposty.p (0, itemfg.i-no, v-adj-qty, v-uom,
            v-cost[1], v-cost[2], v-cost[3], v-cost[4], cDescription).

        v-item-tot = v-item-tot + v-tot-value.

        IF LAST-OF(fg-rctd.i-no) THEN 
        DO:
            IF  tg_totalByItem THEN
                PUT "--------" TO 106 "--------------" TO 122 "---------------" TO 149 SKIP
                    "Item Total"                       TO 97
                    v-cum-qty                          TO 106
                    v-tot-price                        TO 122
                    v-item-tot                         TO 149 SKIP.

            v-item-tot = 0.
        END.
        v-postable = YES.
    END. /* each fg-rctd */

    IF v-gl THEN
        FOR EACH work-job BREAK BY work-job.actnum:

            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ work-job.actnum
                NO-LOCK NO-ERROR.

            ASSIGN
                v-dscr        = IF AVAILABLE account THEN account.dscr
                     ELSE "ACCOUNT NOT FOUND - " + work-job.actnum
                v-disp-actnum = work-job.actnum.

            IF work-job.fg THEN
                v-disp-amt = - work-job.amt.
            ELSE
                v-disp-amt = work-job.amt.

            DISPLAY v-disp-actnum v-dscr udate v-disp-amt
                WITH FRAME gldetail.
            DOWN WITH FRAME gldetail.
        END. /* each work-job */

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-inv C-Win 
PROCEDURE run-report-inv :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-seq-no      AS INTEGER   INIT 1 NO-UNDO.

    DEFINE VARIABLE cDisplay      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hField        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTmpField     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVarValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO.
    DEFINE BUFFER bitemfg FOR itemfg.
    DEFINE VARIABLE str-tit4     AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE str-tit5     AS cha       FORM "x(200)" NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE BUFFER btt-fg-bin FOR tt-fg-bin .

    /*{sys/form/r-top5DL.f} */

    cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


    ASSIGN
        str-tit  = coname + " - " + loname
        str-tit2 = "FINISHED GOODS PHYSICAL COUNT - POSTING REPORT"
        str-tit3 = ""
        x        = (112 - length(str-tit)) / 2
        str-tit  = FILL(" ",x) + str-tit
        x        = (112 - length(str-tit2)) / 2
        str-tit2 = FILL(" ",x) + str-tit2
        x        = (132 - length(str-tit3)) / 2
        str-tit3 = FILL(" ",x) + str-tit3.

    FORM tt-fg-bin.rct-date FORMAT "99/99/99" COLUMN-LABEL "TRANS.!DATE"
        tt-fg-bin.i-no     LABEL "ITEM"
        tt-fg-bin.i-name   FORMAT "x(20)" LABEL "DESCRIPTION"
        tt-fg-bin.part-no  FORMAT "x(15)" LABEL "Customer Part#"
        tt-fg-bin.job-no   LABEL "   JOB" SPACE(0) "-" SPACE(0)
        tt-fg-bin.job-no2  LABEL "# " FORMAT "999"
        tt-fg-bin.loc      LABEL "WHSE"
        tt-fg-bin.loc-bin  LABEL "BIN"
        tt-fg-bin.tag      LABEL "TAG" FORM "x(8)"
        tt-fg-bin.on-hand-qty FORMAT "->>>>>>9" LABEL "O/H Qty."
        tt-fg-bin.counted-qty FORMAT "->>>>>>9" LABEL "Cnt Qty."
        tt-fg-bin.sell-price FORMAT "->>>,>>>,>>9.99" LABEL "Selling Value."
        /*tt-fg-bin.pur-uom  label "UOM"*/
        tt-fg-bin.std-cost LABEL "COST/UOM"
        tt-fg-bin.tot-value LABEL "TOTAL COST"
        SKIP
        WITH FRAME itemx2 NO-BOX DOWN WIDTH 170 STREAM-IO.

    FORM v-disp-actnum LABEL "G/L ACCOUNT NUMBER"
        v-dscr        LABEL "DESCRIPTION"
        udate         LABEL "DATE"   
        v-disp-amt    LABEL "AMOUNT" SKIP
        WITH DOWN WIDTH 133 FRAME gldetail2 STREAM-IO.

    time_stamp = STRING(TIME,"hh:mmam").

    SESSION:SET-WAIT-STATE("general").  

    FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

        IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
            THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
                str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                .        
        ELSE 
            ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
                str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
                .        
    END.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(lines-per-page)}
    IF td-show-parm THEN RUN show-param.

    DISPLAY str-tit4 str-tit5  WITH WIDTH 210 STREAM-IO FRAME r-top. 

    v-postable = NO.

    EMPTY TEMP-TABLE tt-fg-bin.

    FOR EACH fg-rctd NO-LOCK 
        WHERE fg-rctd.company   EQ cocode
        AND fg-rctd.rita-code EQ "C"
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.created-by GE begin_userid 
        AND fg-rctd.created-by LE end_userid, 
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
        BREAK BY fg-rctd.i-no:

        CREATE tt-fg-bin.
        ASSIGN 
            tt-fg-bin.fgRctdRowID = ROWID(fg-rctd)
            tt-fg-bin.rct-date    = fg-rctd.rct-date
            tt-fg-bin.i-no        = fg-rctd.i-no
            tt-fg-bin.i-name      = fg-rctd.i-name
            tt-fg-bin.job-no      = fg-rctd.job-no  
            tt-fg-bin.job-no2     = fg-rctd.job-no2
            tt-fg-bin.loc         = fg-rctd.loc
            tt-fg-bin.loc-bin     = fg-rctd.loc-bin
            tt-fg-bin.tag         = fg-rctd.tag
            tt-fg-bin.counted-qty = fg-rctd.t-qty
            tt-fg-bin.pur-uom     = fg-rctd.pur-uom
            tt-fg-bin.seq-no      = v-seq-no
            tt-fg-bin.count-trans = YES
            tt-fg-bin.createGL    = YES
            tt-fg-bin.part-no     = itemfg.part-no
            tt-fg-bin.sell-price  = itemfg.sell-price
            v-seq-no              = v-seq-no + 1.

        FIND FIRST fg-bin WHERE
            fg-bin.company EQ cocode AND
            fg-bin.i-no    EQ fg-rctd.i-no AND
            fg-bin.loc     EQ fg-rctd.loc AND
            fg-bin.loc-bin EQ fg-rctd.loc-bin AND
            fg-bin.tag     EQ fg-rctd.tag AND
            fg-bin.job-no  EQ fg-rctd.job-no AND
            fg-bin.job-no2 EQ fg-rctd.job-no2
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE fg-bin AND fg-rctd.tag NE "" THEN
            FIND FIRST fg-bin WHERE
                fg-bin.company EQ cocode AND
                fg-bin.i-no    EQ fg-rctd.i-no AND
                fg-bin.loc     EQ fg-rctd.loc AND
                fg-bin.loc-bin EQ fg-rctd.loc-bin AND
                fg-bin.tag     EQ "" AND
                fg-bin.job-no  EQ fg-rctd.job-no AND
                fg-bin.job-no2 EQ fg-rctd.job-no2
                NO-LOCK NO-ERROR.

        IF AVAILABLE fg-bin AND fg-bin.pur-uom NE "" THEN
        DO:
            IF fg-bin.pur-uom EQ "M" THEN
                v-std-cost = fg-rctd.std-cost.
            ELSE
                RUN sys/ref/convcuom.p(fg-bin.pur-uom, "M", 0, 0, 0, 0,
                    fg-rctd.std-cost, OUTPUT v-std-cost).
        END.
        ELSE
            IF itemfg.prod-uom EQ "M" THEN
                v-std-cost = fg-rctd.std-cost.
            ELSE
                RUN sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                    fg-rctd.std-cost, OUTPUT v-std-cost).

        ASSIGN
            tt-fg-bin.std-cost  = v-std-cost
            tt-fg-bin.tot-value = v-std-cost * (fg-rctd.t-qty / 1000).

        IF AVAILABLE fg-bin THEN
            ASSIGN
                tt-fg-bin.v-cost[1]   = fg-bin.std-lab-cost
                tt-fg-bin.v-cost[2]   = fg-bin.std-fix-cost
                tt-fg-bin.v-cost[3]   = fg-bin.std-var-cost
                tt-fg-bin.v-cost[4]   = fg-bin.std-mat-cost
                tt-fg-bin.v-uom       = fg-bin.pur-uom
                tt-fg-bin.on-hand-qty = fg-bin.qty.
        ELSE   
            ASSIGN
                tt-fg-bin.v-cost[1] = itemfg.std-lab-cost
                tt-fg-bin.v-cost[2] = itemfg.std-fix-cost
                tt-fg-bin.v-cost[3] = itemfg.std-var-cost
                tt-fg-bin.v-cost[4] = itemfg.std-mat-cost
                tt-fg-bin.v-uom     = itemfg.prod-uom.

        IF AVAILABLE fg-bin THEN 
        DO: 
            IF fg-bin.job-no NE "" THEN
                FIND LAST oe-ordl WHERE
                    oe-ordl.company EQ fg-bin.company AND
                    oe-ordl.job-no EQ fg-bin.job-no AND
                    oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                    oe-ordl.i-no EQ fg-bin.i-no AND
                    (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                    USE-INDEX job
                    NO-LOCK NO-ERROR.
            ELSE 
                FIND LAST oe-ordl WHERE
                    oe-ordl.company EQ fg-bin.company AND
                    oe-ordl.job-no EQ fg-bin.job-no AND
                    oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                    oe-ordl.i-no EQ fg-bin.i-no AND
                    (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                    USE-INDEX item
                    NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ordl THEN
                ASSIGN
                    tt-fg-bin.sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) .
        END.


        tt-fg-bin.v-adj-qty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) - fg-rctd.t-qty.

        RELEASE fg-bin.

    /*v-postable = YES.*/
    /*BV - per Joe, report is not supposed to print each fg-bin, just the on-hand qty column */
    /*       IF LAST-OF(fg-rctd.i-no) THEN                       */
    /*          FOR EACH fg-bin WHERE                            */
    /*              fg-bin.company EQ cocode AND                 */
    /*              fg-bin.i-no    EQ tt-fg-bin.i-no AND         */
    /*              fg-bin.qty     NE 0                          */
    /*              USE-INDEX co-ino                             */
    /*              NO-LOCK:                                     */
    /*                                                           */
    /*              FIND FIRST tt-fg-bin WHERE                   */
    /*                   tt-fg-bin.i-no    eq fg-bin.i-no AND    */
    /*                   tt-fg-bin.loc     eq fg-bin.loc AND     */
    /*                   tt-fg-bin.loc-bin eq fg-bin.loc-bin AND */
    /*                   tt-fg-bin.tag     eq fg-bin.tag AND     */
    /*                   tt-fg-bin.job-no  eq fg-bin.job-no AND  */
    /*                   tt-fg-bin.job-no2 eq fg-bin.job-no2     */
    /*                   NO-ERROR.                               */
    /*                                                           */
    /*              IF NOT AVAIL tt-fg-bin THEN                  */
    /*              DO:                                          */
    /*                 CREATE tt-fg-bin.                         */
    /*                 ASSIGN                                    */
    /*                   tt-fg-bin.i-no    = fg-bin.i-no         */
    /*                   tt-fg-bin.loc     = fg-bin.loc          */
    /*                   tt-fg-bin.loc-bin = fg-bin.loc-bin      */
    /*                   tt-fg-bin.tag     = fg-bin.tag          */
    /*                   tt-fg-bin.job-no  = fg-bin.job-no       */
    /*                   tt-fg-bin.job-no2 = fg-bin.job-no2      */
    /*                   tt-fg-bin.seq-no = v-seq-no             */
    /*                   v-seq-no = v-seq-no + 1.                */
    /*              END.                                         */
    /*                                                           */
    /*              IF AVAIL tt-fg-bin THEN tt-fg-bin.on-hand-qty = fg-bin.qty.          */
    /*                                                           */
    /*              RELEASE tt-fg-bin.                           */
    /*          END. /*each fg-bin*/                             */

    END. /*for each fg-rctd*/

    FOR EACH tt-fg-bin 
        BREAK BY tt-fg-bin.i-no
        BY tt-fg-bin.seq-no:

        IF FIRST-OF(tt-fg-bin.i-no) THEN 
        DO:
            PUT SKIP(1).
            ASSIGN
                v-tot-price = 0
                v-cum-qty   = 0.
        END.
        /*IF tg_showOHCounted AND  THEN*/
        /*
        display tt-fg-bin.rct-date when first-of(tt-fg-bin.i-no)
                tt-fg-bin.i-no     when first-of(tt-fg-bin.i-no)
                tt-fg-bin.i-name
                tt-fg-bin.part-no
                tt-fg-bin.tag
                SUBSTR(tt-fg-bin.tag,16,8) WHEN SUBSTR(tt-fg-bin.tag,1,15) EQ tt-fg-bin.i-no @ tt-fg-bin.tag
                tt-fg-bin.on-hand-qty
                tt-fg-bin.counted-qty WHEN tt-fg-bin.counted-qty NE 0
                tt-fg-bin.sell-price
                /*tt-fg-bin.pur-uom*/
                tt-fg-bin.loc
                tt-fg-bin.loc-bin
                tt-fg-bin.std-cost WHEN tt-fg-bin.count-trans
                tt-fg-bin.tot-value WHEN tt-fg-bin.count-trans
                tt-fg-bin.job-no
                tt-fg-bin.job-no2
            with frame itemx2.
        down with frame itemx2.
        */
        BUFFER btt-fg-bin:FIND-BY-ROWID(ROWID(tt-fg-bin), NO-LOCK) .

        ASSIGN 
            cDisplay   = ""
            cTmpField  = ""
            cVarValue  = ""
            v-variance = btt-fg-bin.on-hand-qty - btt-fg-bin.counted-qty.

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
            cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
            IF INDEX(cTmpField,".") > 0 THEN 
            DO:
                cFieldName = cTmpField.
                cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                hField = BUFFER btt-fg-bin:BUFFER-FIELD(cTmpField).
                IF hField <> ? THEN 
                DO: 
                    cTmpField = SUBSTRING(GetFieldValue(hField),1,int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                    IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = IF cTmpField <> "" THEN 
                                    TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', cTmpField, fg-rctd.job-no2))) ELSE "".                  

                    IF ENTRY(i,cSelectedList) BEGINS "TAG" AND LENGTH(tt-fg-bin.tag) GE 4 THEN 
                    DO:
                        cTmpField = SUBSTRING(tt-fg-bin.tag, LENGTH(tt-fg-bin.tag) - 4).
                    END.


                    cDisplay = cDisplay + 
                        IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                        (cTmpField + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                        ELSE IF LENGTH(cTmpField) <  int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                        (FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                        ELSE cTmpField.

                END.
                ELSE 
                DO:
                    cTmpField = SUBSTRING(cFieldName,1,int( ENTRY( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).

                END.
            END.
            ELSE 
            DO: 
                CASE cTmpField:               
                    WHEN "v-variance" THEN 
                        cVarValue = STRING(btt-fg-bin.on-hand-qty - btt-fg-bin.counted-qty,"->>>>>>>>9" ).
                    WHEN "over-under" THEN 
                        cVarValue =  STRING(- INT( btt-fg-bin.on-hand-qty - btt-fg-bin.counted-qty),"->>>>>>>>9" ) .
                    WHEN "v-tot-value" THEN 
                        cVarValue = STRING(v-tot-value).
                END CASE.

                cDisplay = cDisplay + cVarValue +
                    FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 

            END.
        END.
        IF tg_showOHCounted AND tg_showOHNotCounted THEN .
        ELSE 
        DO:
            IF tg_showOHCounted AND v-variance <> 0 THEN DO:
                tt-fg-bin.createGL = NO.
                NEXT.
            END.
            IF tg_showOHNotCounted AND v-variance = 0 THEN DO:
                tt-fg-bin.createGL = NO.
                NEXT.
            END.
        END.
        v-postable = YES.

        PUT UNFORMATTED cDisplay SKIP.

        ASSIGN
            v-tot-price = v-tot-price + tt-fg-bin.sell-price .

        IF tt-fg-bin.count-trans THEN
        DO:
            ASSIGN
                v-cum-qty  = v-cum-qty + tt-fg-bin.counted-qty
                v-item-tot = v-item-tot + tt-fg-bin.tot-value.
                
                cDescription = IF fg-rctd.job-no NE "" THEN "Job: " + 
                               TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', fg-rctd.job-no, fg-rctd.job-no2)))
                               ELSE IF fg-rctd.po-no NE "" THEN "PO: " + string(fg-rctd.po-no,"999999") + "-" + STRING(fg-rctd.po-line,"999") ELSE "" NO-ERROR. 

            /*Invoicing  - Post Invoicing Transactions - Job Costing*/
            RUN oe/invposty.p (0, tt-fg-bin.i-no, tt-fg-bin.v-adj-qty, tt-fg-bin.v-uom,
                tt-fg-bin.v-cost[1], tt-fg-bin.v-cost[2], tt-fg-bin.v-cost[3], tt-fg-bin.v-cost[4], cDescription).
        END.

        IF LAST-OF(tt-fg-bin.i-no) THEN 
        DO:
            IF  tg_PrintSubTotal THEN
                PUT "--------" TO 115 "---------------" TO 131  "---------------" TO 158 SKIP
                    "Item Count Total"                 TO 100
                    v-cum-qty                          TO 115
                    v-tot-price                        TO 131
                    v-item-tot                         TO 158 SKIP.

            v-item-tot = 0.
        END.
    END. /* each fg-rctd */

    IF v-gl THEN
        FOR EACH work-job BREAK BY work-job.actnum:

            FIND FIRST account
                WHERE account.company EQ cocode
                AND account.actnum  EQ work-job.actnum
                NO-LOCK NO-ERROR.

            ASSIGN
                v-dscr        = IF AVAILABLE account THEN account.dscr
                        ELSE "ACCOUNT NOT FOUND - " + work-job.actnum
                v-disp-actnum = work-job.actnum.

            IF work-job.fg THEN
                v-disp-amt = - work-job.amt.
            ELSE
                v-disp-amt = work-job.amt.

            DISPLAY v-disp-actnum v-dscr udate v-disp-amt
                WITH FRAME gldetail2.
            DOWN WITH FRAME gldetail2.
        END. /* each work-job */
    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    SESSION:SET-WAIT-STATE("").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetFieldValue C-Win 
FUNCTION GetFieldValue RETURNS CHARACTER
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

