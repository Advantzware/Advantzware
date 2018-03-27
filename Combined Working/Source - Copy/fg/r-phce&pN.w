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
DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.
DEF VAR tran-date AS DATE NO-UNDO.
DEF VAR tran-period AS INT NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/VAR.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def buffer b-fg-rctd   for fg-rctd.
def buffer b-itemfg     for itemfg.
def buffer b-fg-bin     for fg-bin.

def var v-post-date     as   date init TODAY NO-UNDO.
def var v-gl            as   log init NO NO-UNDO.

def var save_id         as   recid.
def var v-qty-onh       as   DEC NO-UNDO.
def var v-temp-cost     as   dec format "->>>>>9.99" NO-UNDO.
def var time_stamp      as   CHAR NO-UNDO.
def var v-cum-qty       as   dec format "->>>>>>9" NO-UNDO.
def var v-tot-value     as   dec format "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR v-sell-price    LIKE itemfg.sell-price .
def var v-tot-price     as   dec format "->>>,>>>,>>9.99".
def var v-item-tot      as   dec format "->>>,>>>,>>9.99" NO-UNDO.
def var v-std-cost      as   dec format ">>>,>>9.99<<" NO-UNDO.
def var v-q-adj-ytd     as   INT NO-UNDO.
def var v-adj-qty       as   INT NO-UNDO.
def var v-dscr          like account.dscr NO-UNDO.
def var v-disp-actnum   like account.actnum NO-UNDO.
def var v-disp-amt      as   dec format ">>,>>>,>>9.99cr" NO-UNDO.
def var v-cost          like itemfg.std-tot-cost extent 4 NO-UNDO.
def var v-uom           like itemfg.prod-uom NO-UNDO.

def new shared var v-trnum as int.

def TEMP-TABLE w-fg-rctd NO-UNDO like fg-rctd.

DEF TEMP-TABLE tt-fg-bin NO-UNDO
    FIELD rct-date AS DATE
    FIELD i-no AS CHAR    
    FIELD i-name AS CHAR
    FIELD part-no AS CHAR
    FIELD sell-price LIKE itemfg.sell-price
    FIELD job-no AS CHAR  
    FIELD job-no2 AS INT
    FIELD loc AS CHAR
    FIELD loc-bin AS CHAR
    FIELD tag AS CHAR
    FIELD on-hand-qty AS INT
    FIELD counted-qty AS INT
    FIELD pur-uom AS CHAR
    FIELD std-cost AS DEC FORMAT ">>>,>>9.99<<"
    FIELD tot-value AS DEC FORMAT "->>>,>>>,>>9.99"
    FIELD seq-no AS INT
    FIELD count-trans AS LOG
    FIELD v-cost like itemfg.std-tot-cost extent 4
    FIELD v-uom AS CHAR
    FIELD v-adj-qty AS INT
    INDEX i-no i-no job-no job-no2 loc loc-bin tag
    INDEX seq-no seq-no.

{oe/invwork.i new}
{sys/FORM/r-topw.f}

{fg/fullset.i NEW}

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR v-count-qty LIKE fg-rctd.t-qty NO-UNDO.
DEF VAR v-variance LIKE fg-rctd.t-qty NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Trans. Date,Item,Description,Customer Part#," +
                           "Job#,Whse,Bin,Tag,O/H Qty," +
                           "Sell Value,Cost/M,Total Cost,Count Qty,Variance,Overs/Unders"
       cFieldListToSelect = "tt-fg-bin.rct-date,tt-fg-bin.i-no,tt-fg-bin.i-name,tt-fg-bin.part-no," +
                            "tt-fg-bin.job-no,tt-fg-bin.loc,tt-fg-bin.loc-bin,tt-fg-bin.tag,tt-fg-bin.on-hand-qty," +
                            "tt-fg-bin.sell-price,tt-fg-bin.std-cost,tt-fg-bin.tot-value,tt-fg-bin.counted-qty,v-variance,over-under"    
       cFieldLength = "11,15,20,15," + "9,5,8,8,8," + "10,10,10,10,10,12"
       cFieldType   = "C,c,c,c," + "c,c,c,c,i," + "i,i,i,i,i,i".

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Trans. Date,Item,Description,Customer Part#," +
                           "Tag,Whse,Bin,Sell Value,Cost/M,Total Cost,Job#".

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
end_userid tg_account tg_ShowOHCounted tg_ShowOHNotCounted tg_TotalByItem ~
tg_PrintSubTotal sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS post-date begin_userid end_userid ~
tg_account tg_ShowOHCounted tg_ShowOHNotCounted tg_TotalByItem ~
tg_PrintSubTotal sl_avail sl_selected rd-dest lv-ornt lines-per-page ~
lv-font-no lv-font-name td-show-parm 

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
     SIZE 22 BY 3.81 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 6.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_account AS LOGICAL INITIAL no 
     LABEL "Create GL Accounts?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

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
     post-date AT ROW 3.14 COL 38 COLON-ALIGNED
     begin_userid AT ROW 4.57 COL 23 COLON-ALIGNED HELP
          "Enter the Beginning User ID"
     end_userid AT ROW 4.57 COL 64 COLON-ALIGNED HELP
          "Enter the Ending User ID"
     tg_account AT ROW 6 COL 38
     tg_ShowOHCounted AT ROW 6.95 COL 38 WIDGET-ID 48
     tg_ShowOHNotCounted AT ROW 7.91 COL 38 WIDGET-ID 50
     tg_TotalByItem AT ROW 8.81 COL 38 WIDGET-ID 52
     tg_PrintSubTotal AT ROW 9.81 COL 38 WIDGET-ID 54
     tg_show-inv AT ROW 9.81 COL 64 WIDGET-ID 2
     tg_CheckQty AT ROW 10.62 COL 64 WIDGET-ID 46
     sl_avail AT ROW 12.19 COL 6 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 12.19 COL 60 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 12.24 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.24 COL 40 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.24 COL 40 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.24 COL 40 WIDGET-ID 40
     btn_down AT ROW 16.24 COL 40 WIDGET-ID 42
     rd-dest AT ROW 18.62 COL 5 NO-LABEL
     lv-ornt AT ROW 18.86 COL 29 NO-LABEL
     lines-per-page AT ROW 18.86 COL 82 COLON-ALIGNED
     lv-font-no AT ROW 20.29 COL 32 COLON-ALIGNED
     lv-font-name AT ROW 21.24 COL 26 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.91 COL 5
     btn-ok AT ROW 24.62 COL 23
     btn-cancel AT ROW 24.62 COL 58
     "This procedure will post all finished goods physical count transactions." VIEW-AS TEXT
          SIZE 82 BY .95 AT ROW 2.14 COL 9
          FGCOLOR 4 FONT 6
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 59.4 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.67 COL 6
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.48 COL 2 WIDGET-ID 38
     RECT-6 AT ROW 17.43 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95 BY 25.52.


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
         HEIGHT             = 25.76
         WIDTH              = 96.4
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
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN 
       begin_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_userid:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       post-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tg_account:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

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
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
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
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DEF VAR lv-post AS LOG NO-UNDO.

  assign /*rd-dest
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

  IF tg_account THEN DO:
     RUN fg/d-fginvp.w (OUTPUT tran-date, OUTPUT tran-period).
     IF tran-date = ? THEN RETURN NO-APPLY.
  END.
  ASSIGN post-date = tran-date
         v-post-date = tran-date.

  /* gdm - 12050809*/
  ASSIGN
      begin_userid = begin_userid:SCREEN-VALUE
      end_userid   = end_userid:SCREEN-VALUE .

  /*IF NOT tg_show-inv THEN
     run run-report.
  ELSE */
     RUN run-report-inv.

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
  end case.

  IF v-postable THEN DO:

    lv-post = NO.

    MESSAGE "Post To Finished Goods?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lv-post.

    IF lv-post THEN do:
      RUN cpost.
      MESSAGE "Posting Complete" VIEW-AS ALERT-BOX.
    END.
  END.

  ELSE MESSAGE "Nothing available for posting..." VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME FRAME-A /* Add >> */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

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
  DEF VAR cSelectedList AS cha NO-UNDO.

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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
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
  assign {&self-name}.
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
  assign {&self-name}.
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
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:add-last({&SELF-NAME}:SCREEN-VALUE)
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
    assign {&self-name}.
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  post-date = TODAY.
  RUN DisplaySelectionList.
  RUN enable_UI.

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
SESSION:SET-WAIT-STATE("general").

DEF BUFFER b2-fg-bin FOR fg-bin.

EMPTY TEMP-TABLE w-fg-rctd.

postit:
  do transaction on error undo postit, leave postit:
    for each fg-rctd
        where fg-rctd.company   eq cocode
          and fg-rctd.rita-code eq "C"
          AND ((begin_userid    LE "" AND
                end_userid      GE "") OR
              (fg-rctd.created-by GE begin_userid 
               AND fg-rctd.created-by LE end_userid))      
        no-lock,  
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq fg-rctd.i-no
          and itemfg.isaset
          and itemfg.alloc   
        NO-LOCK:

      RUN fg/fullset.p (ROWID(itemfg)).

      FOR EACH tt-fg-set,
          FIRST b-itemfg
          WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
          NO-LOCK:

        x = 1.
        for each w-fg-rctd by w-fg-rctd.r-no desc:
          leave.
        end.
        if avail w-fg-rctd THEN x = w-fg-rctd.r-no + 1.
        FOR each b-fg-rctd NO-LOCK BY b-fg-rctd.r-no DESCENDING:
          IF b-fg-rctd.r-no GE X THEN X = b-fg-rctd.r-no + 1.
          LEAVE.
        END.

        find last fg-rcpth use-index r-no no-lock no-error.
        if avail fg-rcpth and fg-rcpth.r-no ge x THEN x = fg-rcpth.r-no + 1.

        create w-fg-rctd.
        buffer-copy fg-rctd to w-fg-rctd
        assign
         w-fg-rctd.i-no   = b-itemfg.i-no
         w-fg-rctd.i-name = b-itemfg.i-name
         w-fg-rctd.r-no   = x.

        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq itemfg.i-no
              and fg-bin.loc     eq fg-rctd.loc
              and fg-bin.loc-bin eq fg-rctd.loc-bin
              and fg-bin.tag     eq fg-rctd.tag
              and fg-bin.job-no  eq fg-rctd.job-no
              and fg-bin.job-no2 eq fg-rctd.job-no2
              and fg-bin.cust-no eq fg-rctd.cust-no
            use-index co-ino no-error.
        v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) * tt-fg-set.part-qty-dec.

        find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq b-itemfg.i-no
              and fg-bin.loc     eq fg-rctd.loc
              and fg-bin.loc-bin eq fg-rctd.loc-bin
              and fg-bin.tag     eq fg-rctd.tag
              and fg-bin.job-no  eq fg-rctd.job-no
              and fg-bin.job-no2 eq fg-rctd.job-no2
              and fg-bin.cust-no eq fg-rctd.cust-no
            use-index co-ino no-error.
        v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - v-adj-qty.

        if v-adj-qty lt 0 then v-adj-qty = 0.

        ASSIGN w-fg-rctd.t-qty = (fg-rctd.t-qty * tt-fg-set.part-qty-dec) + v-adj-qty.
      END.
    END.

    {fg/fg-cpost.i w-}

    {fg/fg-cpost.i}

    if v-gl then do:
      /** GET next G/L TRANS. POSTING # **/
      /* gdm - 11050906 */
      REPEAT:
        FIND FIRST gl-ctrl EXCLUSIVE-LOCK
          WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
        IF AVAIL gl-ctrl THEN DO:
          ASSIGN v-trnum       = gl-ctrl.trnum + 1
                 gl-ctrl.trnum = v-trnum.
          FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
          LEAVE.
        END. /* IF AVAIL gl-ctrl */
      END. /* REPEAT */
      /* gdm - 11050906 */

      for each work-job break by work-job.actnum:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = work-job.actnum
         gltrans.jrnl    = "OEINV"
         gltrans.tr-date = udate
         gltrans.period  = uperiod
         gltrans.trnum   = v-trnum.

        if work-job.fg then
          assign
           gltrans.tr-amt  = - work-job.amt
           gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
        else
          assign
           gltrans.tr-amt  = work-job.amt
           gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".
      end. /* each work-job */
    end.
  end. /* postit */

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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.

/*   MESSAGE "List to select: " NUM-ENTRIES(cTextListToSelect) ":" NUM-ENTRIES(cFieldListToSelect) */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:

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
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
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
  DEF VAR cListContents AS cha NO-UNDO.
  DEF VAR iCount AS INT NO-UNDO.
  DEF VAR cTmpList AS cha NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) <> NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect)   .
    CREATE ttRptList.
    ASSIGN ttRptList.TextList = ENTRY(iCount,cTextListToSelect)
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
  DISPLAY post-date begin_userid end_userid tg_account tg_ShowOHCounted 
          tg_ShowOHNotCounted tg_TotalByItem tg_PrintSubTotal sl_avail 
          sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 post-date begin_userid end_userid tg_account 
         tg_ShowOHCounted tg_ShowOHNotCounted tg_TotalByItem tg_PrintSubTotal 
         sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest lv-ornt lines-per-page lv-font-no td-show-parm btn-ok 
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
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.
 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
           iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
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
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF move = "Up" AND i NE 1 THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i - 1)
        ldummy = sl_selected:DELETE(i + 1)
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
     DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
     DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
     DEFINE VARIABLE result AS LOGICAL NO-UNDO.

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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF BUFFER bitemfg FOR itemfg.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF BUFFER bfg-rctd FOR fg-rctd .

/*{sys/form/r-top5DL.f} */

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "FINISHED GOODS PHYSICAL COUNT - POSTING REPORT"
 str-tit3 = ""
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (112 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.
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
form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down width 130 frame gldetail STREAM-IO.

time_stamp = string(time,"hh:mmam").

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

      if td-show-parm then run show-param.

display str-tit4 str-tit5  with WIDTH 210 STREAM-IO frame r-top. 
/*      VIEW FRAME r-top.*/

  v-postable = NO.

  /* gdm - 12050809 */
  FOR EACH fg-rctd NO-LOCK 
      WHERE fg-rctd.company   EQ cocode
        and fg-rctd.rita-code EQ "C"
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.created-by GE begin_userid 
        AND fg-rctd.created-by LE end_userid,
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      BREAK BY fg-rctd.i-no:

     ASSIGN
        v-sell-price = itemfg.sell-price .

    if first-of(fg-rctd.i-no) then do:
     /* put skip(1).*/
      ASSIGN
         v-cum-qty = 0
         v-tot-price = 0.
    end.

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq fg-rctd.i-no
          and fg-bin.loc     eq fg-rctd.loc
          and fg-bin.loc-bin eq fg-rctd.loc-bin
          and fg-bin.tag     eq fg-rctd.tag
          and fg-bin.job-no  eq fg-rctd.job-no
          and fg-bin.job-no2 eq fg-rctd.job-no2
        no-lock no-error.
    if not avail fg-bin and fg-rctd.tag ne "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq fg-rctd.i-no
          and fg-bin.loc     eq fg-rctd.loc
          and fg-bin.loc-bin eq fg-rctd.loc-bin
          and fg-bin.tag     eq ""
          and fg-bin.job-no  eq fg-rctd.job-no
          and fg-bin.job-no2 eq fg-rctd.job-no2
        no-lock no-error.

    IF AVAIL fg-bin AND fg-bin.pur-uom NE "" THEN
    DO:
       if fg-bin.pur-uom eq "M" then
          v-std-cost = fg-rctd.std-cost.
       else
          run sys/ref/convcuom.p(fg-bin.pur-uom, "M", 0, 0, 0, 0,
                                 fg-rctd.std-cost, output v-std-cost).
    END.
    ELSE
       if itemfg.prod-uom eq "M" then
          v-std-cost = fg-rctd.std-cost.
    else
       run sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                              fg-rctd.std-cost, output v-std-cost).

       IF AVAIL fg-bin THEN do: 
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ fg-bin.company AND
                 oe-ordl.job-no EQ fg-bin.job-no AND
                 oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                 oe-ordl.i-no EQ fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  v-sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) .
       END.

    assign
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
    ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           .

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cTmpField = "Part-no" THEN hField = BUFFER bitemfg:BUFFER-FIELD(cTmpField).
                 ELSE hField = BUFFER bfg-rctd:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-rctd.job-no2,"99") ELSE "".                  

                     IF ENTRY(i,cSelectedList) BEGINS "TAG" THEN 
                        cTmpField = substring(fg-rctd.tag, LENGTH(fg-rctd.tag) - 4).

                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .

                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).

                 END.
       END.
       ELSE DO: 
            CASE cTmpField:               
                 WHEN "v-std-cost" THEN cVarValue = string(v-std-cost).
                 WHEN "v-tot-value" THEN cVarValue = string(v-tot-value).
            END CASE.

            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 

       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.

    if avail fg-bin then
      assign
       v-cost[1] = fg-bin.std-lab-cost
       v-cost[2] = fg-bin.std-fix-cost
       v-cost[3] = fg-bin.std-var-cost
       v-cost[4] = fg-bin.std-mat-cost
       v-uom     = fg-bin.pur-uom.
    else   
      assign
       v-cost[1] = itemfg.std-lab-cost
       v-cost[2] = itemfg.std-fix-cost
       v-cost[3] = itemfg.std-var-cost
       v-cost[4] = itemfg.std-mat-cost
       v-uom     = itemfg.prod-uom.

    v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - fg-rctd.t-qty.

    /*Invoicing  - Post Invoicing Transactions - Job Costing*/
    run oe/invposty.p (0, itemfg.i-no, v-adj-qty, v-uom,
                       v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

    v-item-tot = v-item-tot + v-tot-value.

    if last-of(fg-rctd.i-no) then do:
       IF  tg_totalByItem THEN
       put "--------" to 106 "--------------" to 122 "---------------" to 149 skip
          "Item Total"                       to 97
          v-cum-qty                          to 106
          v-tot-price                        TO 122
          v-item-tot                         to 149 skip.

      v-item-tot = 0.
    end.
    v-postable = YES.
  end. /* each fg-rctd */

  if v-gl then
  for each work-job break by work-job.actnum:

    find first account
        where account.company eq cocode
          and account.actnum  eq work-job.actnum
        no-lock no-error.

    assign
     v-dscr        = if avail account then account.dscr
                     else "ACCOUNT NOT FOUND - " + work-job.actnum
     v-disp-actnum = work-job.actnum.

    if work-job.fg then
      v-disp-amt = - work-job.amt.
    else
      v-disp-amt = work-job.amt.

    display v-disp-actnum v-dscr udate v-disp-amt
        with frame gldetail.
    down with frame gldetail.
  end. /* each work-job */

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

  SESSION:SET-WAIT-STATE("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-inv C-Win 
PROCEDURE run-report-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-seq-no AS INT INIT 1 NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF BUFFER bitemfg FOR itemfg.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF BUFFER btt-fg-bin FOR tt-fg-bin .

/*{sys/form/r-top5DL.f} */

cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.


assign
 str-tit  = coname + " - " + loname
 str-tit2 = "FINISHED GOODS PHYSICAL COUNT - POSTING REPORT"
 str-tit3 = ""
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (112 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (132 - length(str-tit3)) / 2
 str-tit3 = fill(" ",x) + str-tit3.

form tt-fg-bin.rct-date FORMAT "99/99/99" column-label "TRANS.!DATE"
     tt-fg-bin.i-no     label "ITEM"
     tt-fg-bin.i-name   format "x(20)" label "DESCRIPTION"
     tt-fg-bin.part-no  format "x(15)" label "Customer Part#"
     tt-fg-bin.job-no   label "   JOB" space(0) "-" space(0)
     tt-fg-bin.job-no2  label "# " format "99"
     tt-fg-bin.loc      label "WHSE"
     tt-fg-bin.loc-bin  label "BIN"
     tt-fg-bin.tag      label "TAG" FORM "x(8)"
     tt-fg-bin.on-hand-qty format "->>>>>>9" label "O/H Qty."
     tt-fg-bin.counted-qty format "->>>>>>9" LABEL "Cnt Qty."
     tt-fg-bin.sell-price format "->>>,>>>,>>9.99" LABEL "Selling Value."
     /*tt-fg-bin.pur-uom  label "UOM"*/
     tt-fg-bin.std-cost label "COST/UOM"
     tt-fg-bin.tot-value label "TOTAL COST"
     SKIP
     with frame itemx2 no-box down width 170 STREAM-IO.

form v-disp-actnum label "G/L ACCOUNT NUMBER"
     v-dscr        label "DESCRIPTION"
     udate         label "DATE"   
     v-disp-amt    label "AMOUNT" SKIP
    with down width 133 frame gldetail2 STREAM-IO.

time_stamp = string(time,"hh:mmam").

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
 if td-show-parm then run show-param.

  display str-tit4 str-tit5  with WIDTH 210 STREAM-IO frame r-top. 

  v-postable = NO.

  EMPTY TEMP-TABLE tt-fg-bin.

  FOR EACH fg-rctd NO-LOCK 
      WHERE fg-rctd.company   EQ cocode
        and fg-rctd.rita-code EQ "C"
        AND fg-rctd.loc-bin   NE ""
        AND fg-rctd.created-by GE begin_userid 
        AND fg-rctd.created-by LE end_userid, 
      FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
      BREAK BY fg-rctd.i-no:

      CREATE tt-fg-bin.
      ASSIGN tt-fg-bin.rct-date = fg-rctd.rct-date
             tt-fg-bin.i-no = fg-rctd.i-no
             tt-fg-bin.i-name = fg-rctd.i-name
             tt-fg-bin.job-no = fg-rctd.job-no  
             tt-fg-bin.job-no2 = fg-rctd.job-no2
             tt-fg-bin.loc = fg-rctd.loc
             tt-fg-bin.loc-bin = fg-rctd.loc-bin
             tt-fg-bin.tag = fg-rctd.tag
             tt-fg-bin.counted-qty = fg-rctd.t-qty
             tt-fg-bin.pur-uom = fg-rctd.pur-uom
             tt-fg-bin.seq-no = v-seq-no
             tt-fg-bin.count-trans = YES
             tt-fg-bin.part-no = itemfg.part-no
             tt-fg-bin.sell-price = itemfg.sell-price
             v-seq-no = v-seq-no + 1.

      find first fg-bin WHERE
           fg-bin.company eq cocode AND
           fg-bin.i-no    eq fg-rctd.i-no AND
           fg-bin.loc     eq fg-rctd.loc AND
           fg-bin.loc-bin eq fg-rctd.loc-bin AND
           fg-bin.tag     eq fg-rctd.tag AND
           fg-bin.job-no  eq fg-rctd.job-no AND
           fg-bin.job-no2 eq fg-rctd.job-no2
           no-lock no-error.

      if not avail fg-bin and fg-rctd.tag ne "" then
         find first fg-bin WHERE
              fg-bin.company eq cocode AND
              fg-bin.i-no    eq fg-rctd.i-no AND
              fg-bin.loc     eq fg-rctd.loc AND
              fg-bin.loc-bin eq fg-rctd.loc-bin AND
              fg-bin.tag     eq "" AND
              fg-bin.job-no  eq fg-rctd.job-no AND
              fg-bin.job-no2 eq fg-rctd.job-no2
              no-lock no-error.

      IF AVAIL fg-bin AND fg-bin.pur-uom NE "" THEN
      DO:
         if fg-bin.pur-uom eq "M" then
            v-std-cost = fg-rctd.std-cost.
         else
            run sys/ref/convcuom.p(fg-bin.pur-uom, "M", 0, 0, 0, 0,
                                   fg-rctd.std-cost, output v-std-cost).
      END.
      ELSE
         if itemfg.prod-uom eq "M" then
            v-std-cost = fg-rctd.std-cost.
      else
         run sys/ref/convcuom.p(itemfg.prod-uom, "M", 0, 0, 0, 0,
                                fg-rctd.std-cost, output v-std-cost).

      ASSIGN
         tt-fg-bin.std-cost = v-std-cost
         tt-fg-bin.tot-value = v-std-cost * (fg-rctd.t-qty / 1000).

      if avail fg-bin then
         assign
            tt-fg-bin.v-cost[1] = fg-bin.std-lab-cost
            tt-fg-bin.v-cost[2] = fg-bin.std-fix-cost
            tt-fg-bin.v-cost[3] = fg-bin.std-var-cost
            tt-fg-bin.v-cost[4] = fg-bin.std-mat-cost
            tt-fg-bin.v-uom     = fg-bin.pur-uom
            tt-fg-bin.on-hand-qty = fg-bin.qty.
      else   
         assign
            tt-fg-bin.v-cost[1] = itemfg.std-lab-cost
            tt-fg-bin.v-cost[2] = itemfg.std-fix-cost
            tt-fg-bin.v-cost[3] = itemfg.std-var-cost
            tt-fg-bin.v-cost[4] = itemfg.std-mat-cost
            tt-fg-bin.v-uom     = itemfg.prod-uom.

      IF AVAIL fg-bin THEN do: 
            FIND LAST oe-ordl WHERE
                 oe-ordl.company EQ fg-bin.company AND
                 oe-ordl.job-no EQ fg-bin.job-no AND
                 oe-ordl.job-no2 EQ fg-bin.job-no2 AND
                 oe-ordl.i-no EQ fg-bin.i-no AND
                 (oe-ordl.pr-uom NE "CS" OR oe-ordl.cas-cnt NE 0)
                 NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
               ASSIGN
                  tt-fg-bin.sell-price = oe-ordl.price * (1 - (oe-ordl.disc / 100)) .
       END.


      tt-fg-bin.v-adj-qty = (if avail fg-bin then fg-bin.qty else 0) - fg-rctd.t-qty.

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

  FOR EACH tt-fg-bin NO-LOCK 
      BREAK BY tt-fg-bin.i-no
            BY tt-fg-bin.seq-no:

    if first-of(tt-fg-bin.i-no) then do:
       put skip(1).
       ASSIGN
          v-tot-price = 0
          v-cum-qty   = 0.
    end.
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

    ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           v-variance = btt-fg-bin.on-hand-qty - btt-fg-bin.counted-qty.

    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 hField = BUFFER btt-fg-bin:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO: 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(fg-rctd.job-no2,"99") ELSE "".                  

                     IF ENTRY(i,cSelectedList) BEGINS "TAG" AND LENGTH(tt-fg-bin.tag) GE 4 THEN DO:
                         cTmpField = substring(tt-fg-bin.tag, LENGTH(tt-fg-bin.tag) - 4).
                     END.


                     cDisplay = cDisplay + 
                               IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldType) = "C" THEN
                                 (cTmpField + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField)))
                               ELSE IF LENGTH(cTmpField) <  int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) THEN
                                 (FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) - LENGTH(cTmpField)) + cTmpField) + " "
                               ELSE cTmpField.

                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).

                 END.
       END.
       ELSE DO: 
            CASE cTmpField:               
                 WHEN "v-variance" THEN cVarValue = string(btt-fg-bin.on-hand-qty - btt-fg-bin.counted-qty,"->>>>>>>>9" ).
                 WHEN "over-under" THEN cVarValue =  string(- INT( btt-fg-bin.on-hand-qty - btt-fg-bin.counted-qty),"->>>>>>>>9" ) .
                 WHEN "v-tot-value" THEN cVarValue = string(v-tot-value).
            END CASE.

            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 

       END.
    END.
    IF tg_showOHCounted AND tg_showOHNotCounted THEN .
    ELSE DO:
        IF tg_showOHCounted AND v-variance <> 0 THEN NEXT.
        IF tg_showOHNotCounted AND v-variance = 0 THEN NEXT.
    END.
    v-postable = YES.

    PUT UNFORMATTED cDisplay SKIP.

     ASSIGN
       v-tot-price = v-tot-price + tt-fg-bin.sell-price .

    IF tt-fg-bin.count-trans THEN
    DO:
       ASSIGN
          v-cum-qty = v-cum-qty + tt-fg-bin.counted-qty
          v-item-tot = v-item-tot + tt-fg-bin.tot-value.

       /*Invoicing  - Post Invoicing Transactions - Job Costing*/
       run oe/invposty.p (0, tt-fg-bin.i-no, tt-fg-bin.v-adj-qty, tt-fg-bin.v-uom,
                          tt-fg-bin.v-cost[1], tt-fg-bin.v-cost[2], tt-fg-bin.v-cost[3], tt-fg-bin.v-cost[4]).
    END.

    if last-of(tt-fg-bin.i-no) then do:
        IF  tg_PrintSubTotal THEN
        put "--------" to 115 "---------------" to 131  "---------------" to 158 skip
           "Item Count Total"                 to 100
           v-cum-qty                          to 115
           v-tot-price                        TO 131
           v-item-tot                         to 158 skip.

       v-item-tot = 0.
    end.
  end. /* each fg-rctd */

  if v-gl then
     for each work-job break by work-job.actnum:

       find first account
           where account.company eq cocode
             and account.actnum  eq work-job.actnum
           no-lock no-error.

       assign
        v-dscr        = if avail account then account.dscr
                        else "ACCOUNT NOT FOUND - " + work-job.actnum
        v-disp-actnum = work-job.actnum.

       if work-job.fg then
         v-disp-amt = - work-job.amt.
       else
         v-disp-amt = work-job.amt.

       display v-disp-actnum v-dscr udate v-disp-amt
           with frame gldetail2.
       down with frame gldetail2.
     end. /* each work-job */
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
  def var lv-frame-hdl as handle no-undo.
  def var lv-group-hdl as handle no-undo.
  def var lv-field-hdl as handle no-undo.
  def var lv-field2-hdl as handle no-undo.
  def var parm-fld-list as cha no-undo.
  def var parm-lbl-list as cha no-undo.
  def var i as int no-undo.
  def var lv-label as cha.

  lv-frame-hdl = frame {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:first-child.
  lv-field-hdl = lv-group-hdl:first-child .

  do while true:
     if not valid-handle(lv-field-hdl) then leave.
     if lookup(lv-field-hdl:private-data,"parm") > 0
        then do:
           if lv-field-hdl:label <> ? then 
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:label + "," 
                     .
           else do:  /* radio set */
              assign parm-fld-list = parm-fld-list + lv-field-hdl:screen-value + ","
                     .
              lv-field2-hdl = lv-group-hdl:first-child.
              repeat:
                  if not valid-handle(lv-field2-hdl) then leave. 
                  if lv-field2-hdl:private-data = lv-field-hdl:name then do:
                     parm-lbl-list = parm-lbl-list + lv-field2-hdl:screen-value + ",".
                  end.
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
              end.       
           end.                 
        end.            
     lv-field-hdl = lv-field-hdl:next-sibling.   
  end.

  put space(28)
      "< Selection Parameters >"
      skip(1).

  do i = 1 to num-entries(parm-fld-list,","):
    if entry(i,parm-fld-list) ne "" or
       entry(i,parm-lbl-list) ne "" then do:

      lv-label = fill(" ",34 - length(trim(entry(i,parm-lbl-list)))) +
                 trim(entry(i,parm-lbl-list)) + ":".

      put lv-label format "x(35)" at 5
          space(1)
          trim(entry(i,parm-fld-list)) format "x(40)"
          skip.              
    end.
  end.

  put fill("-",80) format "x(80)" skip.

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
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

