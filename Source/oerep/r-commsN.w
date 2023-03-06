&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-comms.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No.        */     

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

assign
 cocode = gcompany
 locode = gloc.

DEFINE VARIABLE v-per-rpt   AS   LOG FORMAT "PTD/YTD" INIT YES.
DEFINE VARIABLE v-period    AS   INT INIT 1.
DEFINE VARIABLE v-cat       LIKE itemfg.procat.
DEFINE VARIABLE v-sman      AS   CHAR FORMAT "x(3)" EXTENT 2 INIT ["", "zzz"].
DEFINE VARIABLE v-date      AS   DATE EXTENT 2 INIT [01/01/01, TODAY].
DEFINE VARIABLE v-cust      AS   CHARACTER EXTENT 2 INIT ["", "zzzzzzzz"].
DEFINE VARIABLE v-cust-typ  AS   CHARACTER EXTENT 2 INIT ["", "zzzzzzzz"].
DEFINE VARIABLE v-sumdet    AS   LOG FORMAT "Summary/Detail" INIT YES.
DEFINE VARIABLE v-cost1     AS   CHAR.
DEFINE VARIABLE v-year      AS   INTEGER.
DEFINE VARIABLE v-calc-cat  AS   CHAR.
DEFINE VARIABLE cSlsList    AS   CHAR NO-UNDO.
DEFINE VARIABLE deUseCost   AS   DEC  NO-UNDO.

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER c-itemfg FOR itemfg.

DEF TEMP-TABLE w-comm    NO-UNDO
    FIELD sman    AS   CHAR
    FIELD samt    LIKE ar-invl.amt
    FIELD camt    LIKE ar-invl.amt
    FIELD cost    LIKE ar-invl.amt
    INDEX sman sman.

DEF TEMP-TABLE tt-slsrp
    FIELD sman    AS   CHAR
    FIELD samt    LIKE ar-invl.amt
    FIELD camt    LIKE ar-invl.amt
    FIELD cost    LIKE ar-invl.amt
    FIELD scat    AS   CHAR
    INDEX i1 sman scat.


DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD row-id AS ROWID.

DEFINE VARIABLE v-print-fmt         AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form      AS LOGICAL.
DEFINE VARIABLE ls-fax-file         AS CHARACTER NO-UNDO.
DEF STREAM st-excell.

DEFINE VARIABLE ldummy              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFileName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutputProcs      AS HANDLE    NO-UNDO.

RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.

ASSIGN cTextListToSelect  = "Rep,Customer,Name,Type,FG Item#,Cust Part#,Order#,Inv#,Cat,Quantity,Sell Price,Total Cost," +
                            "GP %,Comm Amt,Comm %,Group,Currency,Invoice Date,Warehouse,Ship To,MSF," +
                            "Est. Freight,Est. Warehouse,Est. Inhouse,Est. Deviation,Estimate,Est Created"
       cFieldListToSelect = "sman,cust-no,cust-nam,type,i-no,part-no,ord,inv,cat,qty,sel-pric,totl-cst," +
                            "v-gp,v-camt,v-comm,grp,curr,inv-date,ware-house,ship-id,msf," +
                            "fre-cost,ware-cost,manu-cost,devi-cost,est-no,est-created"

       cFieldLength = "4,8,19,8,15,15,8,7,8,10,12,12," + "9,10,8,8,8,12,9,8,10," + "12,14,16,14,9,11"
       .

{sys/inc/ttRptSel.i}
 ASSIGN cTextListToDefault  = "Rep,Customer,Name,Type,FG Item#,Cust Part#,Order#,Inv#,Cat,Quantity,Sell Price,Total Cost," +
                            "GP %,Comm Amt,Comm %,Currency,Invoice Date,Warehouse"  .

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
end_date begin_slsmn end_slsmn tgChooseSalesReps begin_cust-no end_cust-no ~
begin_cust-type end_cust-type begin_group end_group fg-cat tb_prep tgCatSum ~
tgFullCost sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down ~
rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS lbl_ptd rd_ptd begin_period begin_date ~
end_date begin_slsmn end_slsmn tgChooseSalesReps begin_cust-no end_cust-no ~
begin_cust-type end_cust-type begin_group end_group fg-cat tb_prep tgCatSum ~
tgFullCost sl_avail sl_selected rd-dest td-show-parm fi_file tb_OpenCSV ~
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

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE begin_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE begin_group AS CHARACTER FORMAT "X(8)" 
     LABEL "From Group" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE begin_period AS INTEGER FORMAT ">>":U INITIAL 99 
     LABEL "For Period?" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE end_cust-type AS CHARACTER FORMAT "X(8)" 
     LABEL "Ending Cust Type" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Invoice Date" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE end_group AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "To Group" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.

DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "For Category" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-cusitm.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE
     SIZE 43 BY 1.

DEFINE VARIABLE lbl_ptd AS CHARACTER FORMAT "X(256)":U INITIAL "PTD / YTD?" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

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
"To Email", 5,
"To CSV", 3
     SIZE 16 BY 3.81 NO-UNDO.

DEFINE VARIABLE rd_ptd AS CHARACTER INITIAL "PTD" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PTD", "PTD",
"YTD", "YTD"
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 4.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 12.14.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.57 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.52 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prep AS LOGICAL INITIAL no 
     LABEL "Show Prep Charges?" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CS?" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.2 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tgCatSum AS LOGICAL INITIAL no 
     LABEL "Cat. Summary?" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE tgChooseSalesReps AS LOGICAL INITIAL no 
     LABEL "Choose Sales Reps" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE tgFullCost AS LOGICAL INITIAL no 
     LABEL "Use Full Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     lbl_ptd AT ROW 2.19 COL 10 COLON-ALIGNED NO-LABEL
     rd_ptd AT ROW 2.19 COL 26 NO-LABEL
     begin_period AT ROW 3.38 COL 24 COLON-ALIGNED
     begin_date AT ROW 4.57 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.57 COL 67 COLON-ALIGNED HELP
          "Enter Ending Date"
     begin_slsmn AT ROW 5.67 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slsmn AT ROW 5.67 COL 67 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tgChooseSalesReps AT ROW 6.67 COL 26 WIDGET-ID 58
     begin_cust-no AT ROW 7.57 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 7.57 COL 67 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-type AT ROW 8.71 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 6
     end_cust-type AT ROW 8.71 COL 67.2 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 8
     begin_group AT ROW 9.86 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 60
     end_group AT ROW 9.86 COL 67.2 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 62
     fg-cat AT ROW 11 COL 24 COLON-ALIGNED HELP
          "Enter Category, or leave blank for all"
     tb_prep AT ROW 12.38 COL 13.6
     tgCatSum AT ROW 12.38 COL 45 WIDGET-ID 4
     tgFullCost AT ROW 12.38 COL 68.8 WIDGET-ID 10
     sl_avail AT ROW 14.71 COL 3.4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 14.86 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 14.86 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 15.95 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 17.05 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 18.14 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 19.24 COL 40.4 WIDGET-ID 42
     lines-per-page AT ROW 21.19 COL 84 COLON-ALIGNED
     lv-font-name AT ROW 21.29 COL 28 COLON-ALIGNED NO-LABEL
     rd-dest AT ROW 21.38 COL 6 NO-LABEL
     lv-ornt AT ROW 21.43 COL 31 NO-LABEL
     lv-font-no AT ROW 21.57 COL 34 COLON-ALIGNED
     td-show-parm AT ROW 23.24 COL 40.2
     fi_file AT ROW 24.14 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     tb_OpenCSV AT ROW 24.19 COL 87.4 RIGHT-ALIGNED
     tbAutoClose AT ROW 26.1 COL 28.4 WIDGET-ID 16
     btn-ok AT ROW 27.05 COL 28.4
     btn-cancel AT ROW 27.05 COL 52
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 14.19 COL 60.2 WIDGET-ID 44
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.62 COL 4
          BGCOLOR 15 
     "(Leave Blank For all Categories)" VIEW-AS TEXT
          SIZE 31 BY .71 AT ROW 11.29 COL 46
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.19 COL 4.2
          BGCOLOR 15 
     "Available Columns" VIEW-AS TEXT
          SIZE 18.4 BY .62 AT ROW 14.1 COL 12 WIDGET-ID 38
     RECT-6 AT ROW 21.05 COL 3.4
     RECT-7 AT ROW 1.48 COL 3.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.19
         SIZE 95.2 BY 28.71
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
         TITLE              = "Commission Report"
         HEIGHT             = 28.91
         WIDTH              = 96.4
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 15
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
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_group:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_group:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slsmn:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fg-cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_ptd IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_ptd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_ptd".

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
       rd_ptd:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_prep:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tgCatSum:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Commission Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Commission Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  DELETE PROCEDURE hdOutputProcs.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON LEAVE OF begin_period IN FRAME FRAME-A /* For Period? */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN show-period-dates NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   DELETE PROCEDURE hdOutputProcs.
   apply "close" to this-procedure.
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
    ASSIGN fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE =  cFileName.
  END.
  RUN GetSelectionList.
  RUN run-report. 
  STATUS DEFAULT "Processing Complete". 
  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN DO:
           IF NOT tb_OpenCSV THEN DO:        
               MESSAGE  "CSV file have been created." SKIP(1)
               "~"OK~" to open CSV file?"
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
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &type= "Customer "
                            &begin_cust= "begin_cust-no"
                            &END_cust= "begin_cust-no" 
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer "
                             &begin_cust= "begin_cust-no"
                             &END_cust= "begin_cust-no"
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer "
                                  &begin_cust="begin_cust-no"
                                  &END_cust="begin_cust-no"
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

           END.
       END.
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* Name */
DO:
    fi_file = ''.
   //  assign {&self-name}.
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
  assign {&self-name}.

  if rd_ptd eq "YTD" then do:
    find first period
        where period.company eq gcompany
          and period.yr      eq v-year
        no-lock no-error.

    begin_date = if avail period then period.pst
                                 else date(1,1,year(today)).

    display begin_date WITH FRAME FRAME-A IN WINDOW C-Win.
  end.

  run show-period-dates.
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


&Scoped-define SELF-NAME tb_prep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_prep C-Win
ON VALUE-CHANGED OF tb_prep IN FRAME FRAME-A /* Show Prep Charges? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CS? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgChooseSalesReps
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgChooseSalesReps C-Win
ON VALUE-CHANGED OF tgChooseSalesReps IN FRAME FRAME-A /* Choose Sales Reps */
DO:
  assign {&self-name}.
  IF tgChooseSalesReps THEN DO:
    ASSIGN begin_slsmn:SCREEN-VALUE = "" end_slsmn:SCREEN-VALUE = "ZZZ"
           begin_slsmn:SENSITIVE = NO end_slsmn:SENSITIVE = NO.
    RUN windows/d-slsList.w (INPUT cocode, INPUT-OUTPUT cSlsList).
  END.
  ELSE 
     ASSIGN begin_slsmn:SENSITIVE = YES end_slsmn:SENSITIVE = YES.
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

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

/* security check need {methods/prgsecur.i} in definition section */
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  ASSIGN
   begin_date = TODAY
   end_date   = TODAY.

  find first period
      where period.company eq gcompany
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
      no-lock no-error.

  if available period then
    assign
     begin_period = period.pnum
     v-year       = period.yr
     begin_date   = period.pst.

  else
    assign
     begin_period = month(today)
     v-year       = year(today).

  fi_file = "c:\tmp\commrpt.csv".

  RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OR6" }
    {methods/nowait.i}
    ASSIGN
    td-show-parm:SENSITIVE = lShowParameters
    td-show-parm:HIDDEN = NOT lShowParameters
    td-show-parm:VISIBLE = lShowParameters
    .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}

    ASSIGN begin_slsmn:SENSITIVE = YES 
           end_slsmn:SENSITIVE = YES
           tgChooseSalesReps:SCREEN-VALUE = "NO".

    RUN DisplaySelectionList2.
    APPLY "entry" TO rd_ptd.

    FIND FIRST period
        WHERE period.company EQ gcompany
          AND period.pst     LE DATE(begin_date:SCREEN-VALUE)
          AND period.pend    GE DATE(end_date:SCREEN-VALUE)
          AND period.pstat
        NO-LOCK NO-ERROR.
    v-year = IF AVAIL period THEN period.yr ELSE YEAR(TODAY).

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
           ttRptlist.FieldList = ENTRY(iCount,cFieldListToSelect)
           .    
  END.

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
  DISPLAY lbl_ptd rd_ptd begin_period begin_date end_date begin_slsmn end_slsmn 
          tgChooseSalesReps begin_cust-no end_cust-no begin_cust-type 
          end_cust-type begin_group end_group fg-cat tb_prep tgCatSum tgFullCost 
          sl_avail sl_selected rd-dest td-show-parm fi_file tb_OpenCSV 
          tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 rd_ptd begin_period begin_date end_date begin_slsmn 
         end_slsmn tgChooseSalesReps begin_cust-no end_cust-no begin_cust-type 
         end_cust-type begin_group end_group fg-cat tb_prep tgCatSum tgFullCost 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest td-show-parm fi_file tb_OpenCSV tbAutoClose btn-ok btn-cancel 
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
    IF NOT AVAIL ttRptList THEN
        MESSAGE "no " i ENTRY(i,ctmplist) SKIP
        ctmplist
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/sa-comm.p 3/96 JLF */
/* Commission Summary / Detail Report                                         */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-top3w.f}*/
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.

{sys/form/r-top5DL3.f}

DEF BUFFER b-ar-invl FOR ar-invl.
DEF BUFFER b-ar-cashl FOR ar-cashl.
def var v-frst      as   log extent 2 NO-UNDO.
def var p-sman      as   char format "x(3)" NO-UNDO.
def var v-camt      like ar-invl.amt NO-UNDO.
def var v-prof      like ar-invl.amt NO-UNDO.
def var v-comm      as   dec format ">>9.99" NO-UNDO.
def var v-gp        as   dec format ">>9.99" NO-UNDO.
def var v-slsm      like ar-invl.sman extent 1 NO-UNDO.
def var v-slsc      like ar-invl.s-comm extent 1 NO-UNDO.
def var v-slsp      like ar-invl.s-pct extent 1 NO-UNDO.
def var v-inv-no    like ar-invl.inv-no NO-UNDO FORMAT ">>>>>>>9".
def var dFreightCost     AS DECIMAL NO-UNDO .
def var dWarehouseCost   AS DECIMAL NO-UNDO .
def var dManufactureCost AS DECIMAL NO-UNDO .
def var dDeviationCost   AS DECIMAL NO-UNDO .
DEFINE VARIABLE cEstimateNo AS CHARACTER NO-UNDO.

/*BV - 01071303*/
def var v-inv-date    like ar-invl.inv-date NO-UNDO.

def var v-procat    like itemfg.procat NO-UNDO.
def var v-qty       as   DEC NO-UNDO.
def var v-amt       like ar-invl.amt NO-UNDO.
def var v-cost      like ar-invl.t-cost NO-UNDO.
DEF VAR v-full-cost AS LOGICAL NO-UNDO.
def var v-cust-part like ar-invl.part-no no-undo.
def var v-ord-no    like ar-invl.ord-no NO-UNDO.
DEFINE VARIABLE v-ship-id like ar-inv.ship-id NO-UNDO.

def var v-job-no    like job.job-no NO-UNDO.
def var v-job-no2   like job.job-no2 NO-UNDO.
def var v-i-no      like ar-invl.i-no NO-UNDO.
DEF VAR v-bol-no   LIKE oe-boll.bol-no.
DEF VAR v-basis     LIKE sman.commbasis NO-UNDO.
DEF VAR v-show-sls-cat AS LOG INIT YES NO-UNDO.
DEF VAR ld-inv-pct  AS   DEC NO-UNDO.
DEF VAR ld-csh-pct  AS   DEC NO-UNDO.
DEF VAR ll-comp     AS   LOG NO-UNDO.
DEF VAR v-print-cost AS  LOG NO-UNDO.
DEF VAR v-srs-comm  AS DEC.
DEF VAR v-srs-cost AS DEC.
DEF VAR v-srs-gp   AS DEC.
DEF VAR cWhse AS CHAR NO-UNDO.

DEF VAR ll-secure    AS  LOG NO-UNDO INIT NO.

def var v-tot-samt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-camt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-cost  as   dec format "->>>>>>>9.99" extent 3.

def var v-head      as   character format "x(134)" extent 3.
DEF VAR v-exp-head AS cha FORM "x(132)" NO-UNDO.
DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.
DEF VAR v-part-fg LIKE v-cust-part NO-UNDO.

DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF BUFFER bar-inv FOR ar-inv.

DEFINE VARIABLE dMsfCalc AS DECIMAL NO-UNDO .

ASSIGN
 str-tit2 = TRIM(c-win:TITLE) + " (O-R-6)"
 {sys/inc/ctrtext.i str-tit2 112}

 v-per-rpt   = rd_ptd EQ "PTD"
 v-period    = begin_period
 v-date[1]   = begin_date
 v-date[2]   = end_date
 v-cat       = fg-cat
 v-sman[1]   = begin_slsmn
 v-sman[2]   = end_slsmn
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-cust-typ[1]  = begin_cust-type
 v-cust-typ[2]  = end_cust-type
 v-sumdet    = NOT 
 v-cost1     = ""
 v-print-cost = YES
 v-full-cost  = tgFullCost
 v-show-sls-cat = tgCatSum.

IF v-print-cost THEN DO: 
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
  v-print-cost = ll-secure. 
END.

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

    IF ttRptSelected.TextList = "Total Cost" AND NOT v-print-cost THEN NEXT. 

    ASSIGN str-tit4 = str-tit4 + 
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength + 1 - LENGTH(ttRptSelected.TextList))
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        

    IF LOOKUP(ttRptSelected.TextList, "Sell Price,Total Cost,GP %,Comm Amt,Comm %") <> 0    THEN
        ASSIGN
        str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
    ELSE
        str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
END.

EMPTY TEMP-TABLE tt-report.
EMPTY TEMP-TABLE w-comm.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF rd-dest EQ 3 THEN DO:
  OUTPUT STREAM st-excell TO VALUE(cFileName).
  PUT STREAM st-excell UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.
SESSION:SET-WAIT-STATE ("general").


if td-show-parm then run show-param.

{oe/rep/sa-commN.i}

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
IF rd-dest EQ 3 THEN DO:
   OUTPUT STREAM st-excell CLOSE.
END.                  
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
/*OUTPUT STREAM excel CLOSE. */
end procedure.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF rd_ptd:SCREEN-VALUE eq "PTD" then do:
      FIND FIRST period
          WHERE period.company EQ cocode
            AND period.yr      EQ v-year
            AND period.pnum    EQ INT(begin_period:SCREEN-VALUE)
          NO-LOCK NO-ERROR.

      IF AVAIL period THEN
        ASSIGN
         v-year                  = period.yr
         begin_date:SCREEN-VALUE = STRING(period.pst)
         end_date:SCREEN-VALUE   = STRING(IF period.pend LT TODAY THEN period.pend ELSE TODAY).

      ELSE DO: 
        MESSAGE begin_period "is not a valid period. "
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.
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
        fi_file:SENSITIVE = YES
        tb_OpenCSV:SENSITIVE = YES       
       .
      ELSE 
        ASSIGN
        tb_OpenCSV:SCREEN-VALUE = "NO"
        fi_file:SENSITIVE = NO
        tb_OpenCSV:SENSITIVE = NO       
       .
       
     ASSIGN fi_file:SCREEN-VALUE = "c:\tmp\commrpt.csv".
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
  RETURN string(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

