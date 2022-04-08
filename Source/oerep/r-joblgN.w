&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: oerep\r-joblog.w

  Description: Job Log

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
DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

{sys/ref/CustList.i NEW}

DEFINE VARIABLE v-program       AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form  AS LOGICAL NO-UNDO.
DEF {1} SHARED var v-print-fmt  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-fax-file     AS CHARACTER NO-UNDO.

DEFINE STREAM excel.

DEFINE VARIABLE ldummy              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTextListToSelect   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTextListToDefault  AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCustListActive    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFileName           AS CHARACTER NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .


ASSIGN cTextListToSelect = "Job#,Order#,Cust#,Cust Name#,Item Number,Item Name,Cust Po#,Qty Ordered,Order Date," +
                           "Due Date,Cust Part#,Est#,Die#,Cad#,CSR"
       cFieldListToSelect = "job,ord,cust,cust-name,item,item-name,cust-po,ord-qty,ord-date," +
                            "due-date,cust-part,est,die,cad,csr"
       cFieldLength = "13,8,8,30,15,30,15,11,10," + "10,32,8,15,15,8"
       cFieldType = "c,c,c,c,c,c,c,i,c," + "c,c,c,c,c,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Job#,Order#,Cust#,Item Number,Item Name,Cust Po#,Qty Ordered,Order Date," +
                           "Due Date,Cust Part#,Est#,Die#,Cad#" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_ord-date end_ord-date tb_sort-by-cust ~
tb_due-date rs-due-date sl_avail Btn_Def sl_selected Btn_Add Btn_Remove ~
btn_Up btn_down rd-dest td-show-parm tb_OpenCSV fi_file btn-ok btn-cancel ~
tb_cust-list btnCustList tbAutoClose RECT-6 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no end_cust-no begin_ord-no ~
end_ord-no begin_i-no end_i-no begin_ord-date end_ord-date tb_sort-by-cust ~
tb_due-date rs-due-date sl_avail sl_selected rd-dest td-show-parm ~
tb_OpenCSV fi_file tb_cust-list tbAutoClose 

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

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

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
     SIZE 17 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "From Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "To Order Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 
     LABEL "Ending Order#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(45)" INITIAL "c:~\tmp~\r-joblog.csv" 
     LABEL "Name" 
     VIEW-AS FILL-IN NATIVE
     SIZE 43 BY 1.

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

DEFINE VARIABLE rs-due-date AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "View Tab", "View",
"Item Tab", "Item"
     SIZE 28 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 4.91.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 8.33.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tbAutoClose AS LOGICAL INITIAL no 
     LABEL "Auto Close" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.8 BY .95 NO-UNDO.

DEFINE VARIABLE tb_due-date AS LOGICAL INITIAL no 
     LABEL "Print Due Date?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_OpenCSV AS LOGICAL INITIAL no 
     LABEL "Open CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_sort-by-cust AS LOGICAL INITIAL no 
     LABEL "Sort by Customer#/Name?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.8 BY .81 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust-no AT ROW 2.86 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 2.86 COL 69 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_ord-no AT ROW 3.95 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_ord-no AT ROW 3.95 COL 69 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     begin_i-no AT ROW 5.1 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 5.1 COL 69 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_ord-date AT ROW 6.24 COL 28 COLON-ALIGNED
     end_ord-date AT ROW 6.24 COL 69 COLON-ALIGNED HELP
          "Enter Ending Due Date"
     tb_sort-by-cust AT ROW 7.57 COL 30.2
     tb_due-date AT ROW 8.43 COL 30.2
     rs-due-date AT ROW 8.38 COL 51 NO-LABEL WIDGET-ID 2
     sl_avail AT ROW 10.67 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 10.67 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 10.67 COL 59.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 11.67 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.67 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.67 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 14.71 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 17.14 COL 6 NO-LABEL
     lv-ornt AT ROW 17.71 COL 31 NO-LABEL
     lines-per-page AT ROW 17.62 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 17.76 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 17.71 COL 28.2 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 19 COL 38.4
     tb_OpenCSV AT ROW 19.95 COL 87 RIGHT-ALIGNED
     fi_file AT ROW 19.91 COL 27 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23 COL 29.2
     btn-cancel AT ROW 23 COL 53.8
     tb_cust-list AT ROW 1.81 COL 30.2 WIDGET-ID 6
     btnCustList AT ROW 1.91 COL 63.8 WIDGET-ID 8
     tbAutoClose AT ROW 21.95 COL 29.6 WIDGET-ID 16
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.95 COL 5 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 16.29 COL 4.2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 4.4
          BGCOLOR 15 FGCOLOR 0 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.95 COL 59.6 WIDGET-ID 44
     RECT-6 AT ROW 16.57 COL 3.4
     RECT-7 AT ROW 1.48 COL 3.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 27.24
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
         TITLE              = "Job Log"
         HEIGHT             = 24.05
         WIDTH              = 95.8
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ord-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_OpenCSV IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_OpenCSV:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Job Log */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Job Log */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON LEAVE OF begin_cust-no IN FRAME FRAME-A /* Beginning Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no C-Win
ON LEAVE OF begin_i-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-date C-Win
ON LEAVE OF begin_ord-date IN FRAME FRAME-A /* From Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ord-no C-Win
ON LEAVE OF begin_ord-no IN FRAME FRAME-A /* Beginning Order# */
DO:
  assign {&self-name}.
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
  ASSIGN {&DISPLAYED-OBJECTS}.
  IF rd-dest EQ 3 THEN
  DO:
    ASSIGN fi_file = SUBSTRING(fi_file,1,INDEX(fi_file,"_") - 1) .
    RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .
    fi_file:SCREEN-VALUE = cFileName.
  END.
SESSION:SET-WAIT-STATE("general").
  RUN GetSelectionList.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
      EMPTY TEMP-TABLE ttCustList.
      RUN BuildCustList(INPUT cocode,
                        INPUT tb_cust-list AND glCustListActive,
                        INPUT begin_cust-no,
                        INPUT END_cust-no).
  END.
  run run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

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
           {custom/asifax.i &type= "Customer"
                            &begin_cust=begin_cust-no
                            &END_cust= begin_cust-no
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = "Customer"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Customer"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }

           END.
       END. 
  END CASE. 
  SESSION:SET-WAIT-STATE ("").
  IF tbAutoClose:CHECKED THEN 
     APPLY 'CLOSE' TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON LEAVE OF end_cust-no IN FRAME FRAME-A /* Ending Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_i-no C-Win
ON LEAVE OF end_i-no IN FRAME FRAME-A /* Ending Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-date C-Win
ON LEAVE OF end_ord-date IN FRAME FRAME-A /* To Order Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_ord-no C-Win
ON LEAVE OF end_ord-no IN FRAME FRAME-A /* Ending Order# */
DO:
  assign {&self-name}.
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
  /*IF {&self-name}:SCREEN-VALUE EQ "P" THEN tb_prt:SCREEN-VALUE = "NO".*/
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


&Scoped-define SELF-NAME tb_cust-list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-list C-Win
ON VALUE-CHANGED OF tb_cust-list IN FRAME FRAME-A /* Use Defined Customer List */
DO:
  assign {&self-name}.
  EMPTY TEMP-TABLE ttCustList.
  RUN SetCustRange(INPUT tb_cust-list).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_due-date C-Win
ON VALUE-CHANGED OF tb_due-date IN FRAME FRAME-A /* Print Due Date? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_OpenCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_OpenCSV C-Win
ON VALUE-CHANGED OF tb_OpenCSV IN FRAME FRAME-A /* Open CSV? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort-by-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort-by-cust C-Win
ON VALUE-CHANGED OF tb_sort-by-cust IN FRAME FRAME-A /* Sort by Customer#/Name? */
DO:
  assign {&self-name}.
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

  assign
   begin_ord-date = date(1,1,year(today))
   end_ord-date   = today.
  RUN DisplaySelectionList.
    btn-ok:load-image("Graphics/32x32/Ok.png").
    btn-cancel:load-image("Graphics/32x32/cancel.png").
    Btn_Def:load-image("Graphics/32x32/default.png").
    Btn_Add:load-image("Graphics/32x32/additem.png").
    Btn_Remove:load-image("Graphics/32x32/remove.png").
    btn_Up:load-image("Graphics/32x32/moveup.png").
    btn_down:load-image("Graphics/32x32/movedown.png").
    RUN enable_UI.
    {sys/inc/reportsConfigNK1.i "OR3" }
    {methods/nowait.i}
    ASSIGN
    td-show-parm:SENSITIVE = lShowParameters
    td-show-parm:HIDDEN = NOT lShowParameters
    td-show-parm:VISIBLE = lShowParameters
    .

  RUN sys/inc/CustListForm.p ( "OR3",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    tb_due-date:SCREEN-VALUE = "YES" .
    tb_due-date:SENSITIVE = NO .
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_cust-no.
  END.
  
  RUN pChangeDest.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'OR3',
                          INPUT NO,
                          OUTPUT glCustListActive).

 {sys/inc/chblankcust.i ""OR3""}

 IF ou-log THEN DO:
      ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes"
        tb_cust-list = YES 
        .
      RUN SetCustRange(INPUT tb_cust-list).
  END.
  ELSE
      ASSIGN
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        .
 IF ou-log AND ou-cust-int = 0 THEN do:
       ASSIGN 
        tb_cust-list:SENSITIVE IN FRAME {&FRAME-NAME} = YES
        btnCustList:SENSITIVE IN FRAME {&FRAME-NAME} = NO
        tb_cust-list:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "No"
        tb_cust-list = NO
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

DEFINE VARIABLE lActive AS LOGICAL     NO-UNDO.

IF iplList THEN DO:
    RUN sys/ref/CustList.p (INPUT ipcCompany,
                            INPUT 'OR3',
                            INPUT YES,
                            OUTPUT lActive).
END.
ELSE DO:
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
                                  INPUT 'OR3').


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
  DISPLAY begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
          begin_ord-date end_ord-date tb_sort-by-cust tb_due-date rs-due-date 
          sl_avail sl_selected rd-dest td-show-parm tb_OpenCSV fi_file 
          tb_cust-list tbAutoClose 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE begin_cust-no end_cust-no begin_ord-no end_ord-no begin_i-no end_i-no 
         begin_ord-date end_ord-date tb_sort-by-cust tb_due-date rs-due-date 
         sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up btn_down 
         rd-dest td-show-parm tb_OpenCSV fi_file btn-ok btn-cancel 
         tb_cust-list btnCustList tbAutoClose RECT-6 RECT-7 
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

     IF NOT OKpressed THEN  RETURN NO-APPLY.
  */
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
  RUN custom\d-print.w (list-name).
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
  run scr-rpt.w (list-name,c-win:title,INT(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* -------------------------------------------------oe/rep/joblog.p 04/95 DAR */
/* Job Log Report                                                             */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var v-fcust as char extent 2 init ["", "zzzzzzzz"] NO-UNDO.
def var v-ford-no as int format ">>>>>>>9" extent 2 init [0, 99999999] NO-UNDO.
def var v-fdate as date format "99/99/9999" extent 2 init [01/01/0001, today] NO-UNDO.
def var v-fitem as char format "x(15)" extent 2 init ["", "zzzzzzzzzzzzzzz"] NO-UNDO.
def var v-frst as log init NO NO-UNDO.
def var changed as log init NO NO-UNDO.
def var job-num as char format "x(13)" NO-UNDO.
def var v-i-no like oe-ordl.i-no no-undo.
def var v-i-name like oe-ordl.i-name no-undo.
DEF VAR v-cust AS CHAR FORMAT "X(17)" NO-UNDO.
DEF VAR est-num AS CHAR  NO-UNDO.
DEF VAR v-date AS DATE NO-UNDO.

DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
DEFINE VARIABLE lSelected AS LOGICAL INIT YES NO-UNDO.

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

//RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName) .

SESSION:SET-WAIT-STATE ("general").

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       oe-ord.due-date      COLUMN-LABEL "Due!Date"
                            FORMAT "99/99/99"
       SKIP
    WITH FRAME detail STREAM-IO WIDTH 156 DOWN. /*132*/

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       oe-ord.due-date      COLUMN-LABEL "Due!Date"
                            FORMAT "99/99/99"
       oe-ordl.part-no      COLUMN-LABEL "!Cust Part#"
       est-num            COLUMN-LABEL "!    Est#" FORMAT "X(8)"
       eb.die-no            COLUMN-LABEL "!Die#"
       eb.cad-no            COLUMN-LABEL "!Cad#"                         
       SKIP
    WITH FRAME detail-w STREAM-IO WIDTH 230 DOWN. /*180*/

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       SKIP
    WITH FRAME detail-y STREAM-IO WIDTH 156 DOWN. /*132*/

FORMAT job-num              COLUMN-LABEL "Job!Number"
       job-hdr.ord-no       COLUMN-LABEL "Order!Number"
       v-cust               COLUMN-LABEL "!Customer" FORMAT "X(17)"
       v-i-no               COLUMN-LABEL "!Item Number"
       v-i-name             COLUMN-LABEL "!Item Name"
       oe-ordl.po-no        COLUMN-LABEL "!Cust PO#"
       job-hdr.qty          COLUMN-LABEL "Quantity!Ordered"
       oe-ord.ord-date      COLUMN-LABEL "Order!Date"
                            FORMAT "99/99/99"
       oe-ordl.part-no      COLUMN-LABEL "!Cust Part#"
       est-num            COLUMN-LABEL "!    Est#" FORMAT "X(8)"
       eb.die-no            COLUMN-LABEL "!Die#"
       eb.cad-no            COLUMN-LABEL "!Cad#"                         
       SKIP
    WITH FRAME detail-z STREAM-IO WIDTH 230 DOWN. /*180*/

ASSIGN
 str-tit2 = c-win:TITLE + " by Job#"
 {sys/inc/ctrtext.i str-tit2 112}

 v-fcust[1]   = begin_cust-no
 v-fcust[2]   = end_cust-no
 v-ford-no[1] = begin_ord-no
 v-ford-no[2] = end_ord-no
 v-fitem[1]   = begin_i-no
 v-fitem[2]   = end_i-no
 v-fdate[1]   = begin_ord-date
 v-fdate[2]   = end_ord-date
 lSelected  = tb_cust-list .

changed = v-fdate[1] ne date(01,01,year(today)) or v-fdate[2] ne today.

IF lselected THEN DO:
    FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
    IF AVAIL ttCustList THEN ASSIGN v-fcust[1] = ttCustList.cust-no .
    FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
    IF AVAIL ttCustList THEN ASSIGN v-fcust[2] = ttCustList.cust-no .
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

/*IF rd-dest EQ 3 THEN 
DO:
  OUTPUT STREAM excel TO VALUE(cFileName).
  excelheader = "Job Num,Order Num,Customer,Itm Num,Itm Name,Cst PO,Ord Amt,Ord Dte,".
  IF tb_due-date THEN
     excelheader = excelheader + "Due Date,".
  IF tb_prt THEN
     excelheader = excelheader + "Cst Part,Est#,Die#,CAD#".

  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.*/

DEF VAR cslist AS cha NO-UNDO.
 FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

   IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
   THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
               str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
               excelheader = excelHeader + ttRptSelected.TextList + "," .        
   ELSE 
   ASSIGN str-tit4 = str-tit4 + 
            (IF ttRptSelected.HeadingFromLeft THEN
                ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
            ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
          str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
          excelheader = excelHeader + ttRptSelected.TextList + ","
          .        
          cSlist = cSlist + ttRptSelected.FieldList + ",".

        IF LOOKUP(ttRptSelected.TextList, "") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF rd-dest EQ 3 THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

DISPLAY "" WITH FRAME r-top.

    for each job-hdr
       where job-hdr.company eq cocode
         and job-hdr.opened  eq yes
         and job-hdr.ord-no  ge v-ford-no[1]
         and job-hdr.ord-no  le v-ford-no[2]
         and job-hdr.cust-no ge v-fcust[1]
         and job-hdr.cust-no le v-fcust[2]
         AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq job-hdr.cust-no
         AND ttCustList.log-fld no-lock) else true)
         and job-hdr.i-no    ge v-fitem[1]
         and job-hdr.i-no    le v-fitem[2]
        use-index opened no-lock,

        first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock,
        FIRST cust FIELDS(cust-no NAME) WHERE
              cust.company EQ cocode AND
              cust.cust-no EQ job-hdr.cust-no
              NO-LOCK

        break by (if tb_sort-by-cust /*AND
                     tb_cust-name EQ "#"*/ then job-hdr.cust-no
                  ELSE IF tb_sort-by-cust /*AND
                       tb_cust-name EQ "name"*/ THEN cust.NAME
                  else "")
              by job-hdr.job-no
              by job-hdr.job-no2:

       {custom/statusMsg.i "'Processing Order # ' + string(job-hdr.ord-no)"} 

        release oe-ord.
        if job-hdr.ord-no ne 0 then
          find first oe-ord
         where oe-ord.company eq cocode
           and oe-ord.ord-no  eq job-hdr.ord-no
          no-lock no-error.

        if avail oe-ord then
          if oe-ord.ord-date lt v-fdate[1] or
             oe-ord.ord-date gt v-fdate[2] or
             index("CZ",oe-ord.stat) ne 0  then next.
          else.
        else     
          if job.start-date lt v-fdate[1] or
             job.start-date gt v-fdate[2] or
             job.start-date eq ?          then next.

        ASSIGN
          v-i-name = ""
          v-i-no = "".

        RELEASE oe-ordl.

        if avail oe-ord then
        DO:
           find first oe-ordl WHERE
                oe-ordl.company eq cocode AND
                oe-ordl.ord-no  eq job-hdr.ord-no AND
                oe-ordl.i-no    eq job-hdr.i-no AND
                oe-ordl.job-no  EQ job-hdr.job-no AND
                oe-ordl.job-no2 EQ job-hdr.job-no2
                no-lock no-error.

           IF NOT AVAIL oe-ordl THEN
              find first oe-ordl WHERE
                   oe-ordl.company eq cocode AND
                   oe-ordl.ord-no  eq job-hdr.ord-no AND
                   oe-ordl.i-no    eq job-hdr.i-no
                   no-lock no-error.
        END.

        if avail oe-ordl then 
           v-i-name = oe-ordl.i-name.
        else
        do:
           find first itemfg
                where itemfg.company eq cocode
                  and itemfg.i-no eq job-hdr.i-no
               no-lock no-error.
           if avail itemfg then 
              v-i-name = itemfg.i-name.
           else 
              v-i-name = "".
        end.

        assign
          v-i-no  = job-hdr.i-no
          job-num = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job-hdr.job-no, job-hdr.job-no2))).

        /*IF tb_cust-name EQ "#" THEN
           v-cust = job-hdr.cust-no.
        ELSE*/
           v-cust = cust.NAME.

        RELEASE eb.
        IF job.est-no NE "" THEN
        FOR EACH eb
           WHERE eb.company EQ cocode
             AND eb.est-no  EQ job.est-no
            NO-LOCK
           BREAK BY eb.form-no DESC
                BY eb.blank-no DESC:
            IF LAST(eb.form-no) OR
               (eb.form-no EQ job-hdr.frm AND
                eb.blank-no EQ job-hdr.blank-no) THEN LEAVE.
        END.

        IF AVAIL eb THEN
             ASSIGN est-num =  TRIM( FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no))  .
        ELSE ASSIGN est-num = "" .

        v-date = IF rs-due-date EQ "View" AND AVAIL oe-ord THEN oe-ord.due-date 
                  ELSE IF  rs-due-date EQ "Item" AND AVAIL oe-ordl THEN oe-ordl.req-date ELSE job.due-date .


        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"    THEN cVarValue = string(job-num,"x(13)") .
                         WHEN "ord"   THEN cVarValue = string(job-hdr.ord-no,">>>>>>>>").
                         WHEN "cust"   THEN cVarValue = STRING(job-hdr.cust-no,"x(8)").
                         WHEN "cust-name"  THEN cVarValue = STRING(v-cust,"x(30)") .
                         WHEN "item"   THEN cVarValue = STRING(job-hdr.i-no,"x(15)") .
                         WHEN "item-name"   THEN cVarValue = STRING(v-i-name,"x(30)") .
                         WHEN "cust-po"  THEN cVarValue = IF AVAIL oe-ordl THEN STRING(oe-ordl.po-no,"x(15)") ELSE "" .
                         WHEN "ord-qty"   THEN cVarValue = STRING(job-hdr.qty,"->>,>>>,>>9") .
                         WHEN "ord-date"  THEN cVarValue = IF AVAIL oe-ord AND oe-ord.ord-date NE ? THEN STRING(oe-ord.ord-date,"99/99/9999") ELSE  "" .

                         WHEN "due-date"    THEN cVarValue = IF v-date NE ? THEN STRING(v-date,"99/99/9999") ELSE  "" .
                         WHEN "cust-part"   THEN cVarValue = IF AVAIL oe-ordl THEN string(oe-ordl.part-no,"x(32)") ELSE "".
                         WHEN "est"   THEN cVarValue =  STRING(est-num) .
                         WHEN "die"  THEN cVarValue = IF AVAIL eb AND eb.die-no NE "" THEN STRING(eb.die-no,"x(15)") ELSE "" .
                         WHEN "cad"   THEN cVarValue = IF AVAIL eb AND eb.cad-no NE "" THEN STRING(eb.cad-no,"x(15)") ELSE "" .
                         WHEN "csr"   THEN cVarValue = IF AVAIL oe-ord AND oe-ord.csrUser_id NE "" THEN STRING(oe-ord.csrUser_id,"x(8)") ELSE "" .


                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            PUT UNFORMATTED cDisplay SKIP.
            IF rd-dest EQ 3 THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.


       /* IF tb_prt THEN 
        DO:
           IF tb_due-date THEN
           DO:
              display 
                   job-num
                   job-hdr.ord-no
                   v-cust
                   job-hdr.i-no @ v-i-no
                   v-i-name
                   oe-ordl.po-no     when avail oe-ordl
                   job-hdr.qty
                   oe-ord.ord-date   when avail oe-ord
                     job.start       when not avail oe-ord @ oe-ord.ord-date
                   oe-ord.due-date   WHEN tb_due-date AND rs-due-date EQ "View" AND AVAIL oe-ord
                   oe-ordl.req-date WHEN tb_due-date AND rs-due-date EQ "Item" AND AVAIL oe-ordl @ oe-ord.due-date
                   job.due-date      WHEN tb_due-date AND NOT AVAIL oe-ord
                                     @ oe-ord.due-date
                   oe-ordl.part-no   when avail oe-ordl
                   est-num
                   eb.die-no         when avail eb
                   eb.cad-no         when avail eb
                 with FRAME detail-w.
              down with FRAME detail-w.
           END.
           ELSE
              display 
                   job-num
                   job-hdr.ord-no
                   v-cust
                   job-hdr.i-no @ v-i-no
                   v-i-name
                   oe-ordl.po-no     when avail oe-ordl
                   job-hdr.qty
                   oe-ord.ord-date   when avail oe-ord
                     job.start       when not avail oe-ord @ oe-ord.ord-date
                   oe-ord.due-date   WHEN tb_due-date AND rs-due-date EQ "View" AND AVAIL oe-ord
                   oe-ordl.req-date  WHEN tb_due-date AND rs-due-date EQ "Item" AND AVAIL oe-ordl @ oe-ord.due-date
                   oe-ordl.part-no   when avail oe-ordl
                   est-num
                   eb.die-no         when avail eb 
                   eb.cad-no         when avail eb
                 with FRAME detail-z.
              down with FRAME detail-z.

            IF rd-dest EQ 3 THEN 
            DO:
               PUT STREAM excel UNFORMATTED
                   '"' job-num                          '",'
                   '"' job-hdr.ord-no                   '",'
                   '"' v-cust                           '",'
                   '"' job-hdr.i-no                     '",'
                   '"' v-i-name                         '",'
                   '"' IF AVAIL oe-ordl THEN
                          STRING(oe-ordl.po-no) ELSE "" '",'
                   '"' job-hdr.qty                      '",'
                   '"' IF AVAIL oe-ord THEN STRING(oe-ord.ord-date)
                       ELSE STRING(job.START)           '",'.

               IF tb_due-date THEN
                  PUT STREAM excel UNFORMATTED
                      '"' IF AVAIL oe-ord AND rs-due-date EQ "View" THEN
                          STRING(oe-ord.due-date)
                          ELSE IF AVAIL oe-ordl AND rs-due-date EQ "Item" THEN
                          STRING(oe-ordl.req-date)
                          ELSE STRING(job.due-date) '",'.

               PUT STREAM excel UNFORMATTED
                   '"' IF AVAIL oe-ord THEN oe-ordl.part-no
                       ELSE ""                          '",'
                   '"' IF AVAIL eb THEN eb.est-no ELSE "" '",'
                   '"' IF AVAIL eb THEN eb.die-no ELSE "" '",'
                   '"' IF AVAIL eb THEN eb.cad-no ELSE "" '",'
                  SKIP.
            END.
        END.
        ELSE
        DO:
           IF tb_due-date THEN
           DO:
              display 
                  job-num
                  job-hdr.ord-no
                  v-cust
                  job-hdr.i-no @ v-i-no
                  v-i-name
                  oe-ordl.po-no     when avail oe-ordl
                  job-hdr.qty
                  oe-ord.ord-date   when avail oe-ord
                    job.start       when not avail oe-ord @ oe-ord.ord-date
                  oe-ord.due-date   WHEN tb_due-date AND rs-due-date EQ "View" AND AVAIL oe-ord
                  oe-ordl.req-date  WHEN tb_due-date AND rs-due-date EQ "Item" AND AVAIL oe-ordl @ oe-ord.due-date
                  job.due-date      WHEN tb_due-date AND NOT AVAIL oe-ord
                                    @ oe-ord.due-date
                with FRAME detail.
              down with FRAME detail.
           END.
           ELSE
           DO:
              display 
                  job-num
                  job-hdr.ord-no
                  v-cust
                  job-hdr.i-no @ v-i-no
                  v-i-name
                  oe-ordl.po-no     when avail oe-ordl
                  job-hdr.qty
                  oe-ord.ord-date   when avail oe-ord
                  job.start       when not avail oe-ord @ oe-ord.ord-date
                with FRAME detail-y.
              down with FRAME detail-y.
           END.

            IF rd-dest EQ 3 THEN 
            DO:
               PUT STREAM excel UNFORMATTED
                   '"' job-num                          '",'
                   '"' job-hdr.ord-no                   '",'
                   '"' v-cust                           '",'
                   '"' job-hdr.i-no                     '",'
                   '"' v-i-name                         '",'
                   '"' IF AVAIL oe-ordl THEN
                          STRING(oe-ordl.po-no) ELSE "" '",'
                   '"' job-hdr.qty                      '",'
                   '"' IF AVAIL oe-ord THEN STRING(oe-ord.ord-date)
                       ELSE STRING(job.START)           '",'.

               IF tb_due-date THEN
                  PUT STREAM excel UNFORMATTED
                      '"' IF AVAIL oe-ord AND rs-due-date EQ "View" THEN
                          STRING(oe-ord.due-date)
                          ELSE IF AVAIL oe-ordl AND rs-due-date EQ "Item" THEN
                          STRING(oe-ordl.req-date)
                          ELSE STRING(job.due-date) '",'.

               PUT STREAM excel UNFORMATTED SKIP.
            END.
        END.*/

    end.  /* for each */

    IF rd-dest EQ 3 THEN 
    DO:
       OUTPUT STREAM excel CLOSE.
    END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-ornt C-Win 
PROCEDURE set-ornt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
   /* IF tb_prt:SCREEN-VALUE EQ "YES" THEN
      ASSIGN
       lv-ornt:SCREEN-VALUE = "L"
       lv-ornt:SENSITIVE    = NO.

    ELSE*/
      ASSIGN
       lv-ornt:SCREEN-VALUE = "P"
       lv-ornt:SENSITIVE    = YES.

    {custom/chgfont.i}
  END.

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
        begin_cust-no:SENSITIVE = NOT iplChecked
        end_cust-no:SENSITIVE = NOT iplChecked
        begin_cust-no:VISIBLE = NOT iplChecked
        end_cust-no:VISIBLE = NOT iplChecked
        btnCustList:SENSITIVE = iplChecked
       .
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
      ASSIGN fi_file:SCREEN-VALUE = "c:\tmp\r-joblog.csv" .
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

