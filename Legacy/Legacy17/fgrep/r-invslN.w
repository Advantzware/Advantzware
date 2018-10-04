&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fgrep\r-invslm.w

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
DEF VAR list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

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

/*{sys/inc/custlistform.i ""IR5"" }*/

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS   ROWID
    FIELD i-no        LIKE itemfg.i-no
    FIELD cust-no     LIKE itemfg.cust-no
    FIELD part-no     LIKE itemfg.part-no
    FIELD sales-rep   AS   CHAR
    FIELD procat      LIKE itemfg.procat .

DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "SRep,Rep Name,Item,Description,Cust #,Customer Name,Cust PO#," +
                           "Cust Job#,Qty Prod.,Qty Ship,Qty On Hand," +
                           "Total Cost,Total Value"
       cFieldListToSelect = "sman,sname,itm,dscr,cust,cust-nam,cust-po," +
                            "cust-job,qty-pro,qty-ship,qty-onh," +
                            "ttl-cst,ttl-val"
       cFieldLength = "4,20,15,20,8,30,9," + "9,11,11,11," + "14,14"
       cFieldType = "c,c,c,c,c,c,c," + "c,i,i,i," + "i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "SRep,Rep Name,Item,Description,Cust #,Cust PO#," +
                           "Cust Job#,Qty Prod.,Qty Ship,Qty On Hand," +
                           "Total Cost,Total Value".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-6 tb_cust-list btnCustList ~
begin_cust end_cust begin_cust-po end_cust-po begin_slm end_slm tb_inc-zer ~
tb_inc-cust rd_sort sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up ~
btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm tb_excel ~
tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_cust end_cust ~
begin_cust-po end_cust-po begin_slm end_slm tb_inc-zer tb_inc-cust lbl_sort ~
rd_sort sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel /*AUTO-END-KEY*/
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnCustList 
     LABEL "Preview" 
     SIZE 9.8 BY .81.

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

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE begin_cust-po AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_slm AS CHARACTER FORMAT "XXX":U 
     LABEL "Beginning Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE end_cust-po AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Customer PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_slm AS CHARACTER FORMAT "XXX":U INITIAL "zzz" 
     LABEL "Ending Sales Rep#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-invslm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort by SalesRep By?" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .91 NO-UNDO.

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
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 21 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer#", "Customer#",
"Item", "Item"
     SIZE 29 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 8.57.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.29.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 4.95 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 4.95 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE tb_inc-cust AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .91 NO-UNDO.

DEFINE VARIABLE tb_inc-zer AS LOGICAL INITIAL no 
     LABEL "Include Zero Quantity On Hand?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .91 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 2.05 COL 30.2 WIDGET-ID 6
     btnCustList AT ROW 2.1 COL 62.2 WIDGET-ID 8
     begin_cust AT ROW 3.14 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust AT ROW 3.14 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cust-po AT ROW 4.1 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Customer PO Number"
     end_cust-po AT ROW 4.1 COL 70 COLON-ALIGNED HELP
          "Enter Ending Customer PO Number"
     begin_slm AT ROW 5.05 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Sales Rep Number"
     end_slm AT ROW 5.05 COL 70 COLON-ALIGNED HELP
          "Enter Ending Sales Rep Number"
     tb_inc-zer AT ROW 6.24 COL 29
     tb_inc-cust AT ROW 7.14 COL 72 RIGHT-ALIGNED
     lbl_sort AT ROW 8.29 COL 22 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.29 COL 46 NO-LABEL
     sl_avail AT ROW 10.29 COL 3.8 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 10.29 COL 60.4 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 10.33 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 11.29 COL 40.4 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 12.24 COL 40.4 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 13.19 COL 40.4 WIDGET-ID 40
     btn_down AT ROW 14.14 COL 40.4 WIDGET-ID 42
     rd-dest AT ROW 16.29 COL 5 NO-LABEL
     lv-ornt AT ROW 16.29 COL 30 NO-LABEL
     lines-per-page AT ROW 16.29 COL 85 COLON-ALIGNED
     lv-font-no AT ROW 18.67 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 19.62 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 21.29 COL 30
     tb_excel AT ROW 22.24 COL 66.2 RIGHT-ALIGNED
     tb_runExcel AT ROW 22.24 COL 90 RIGHT-ALIGNED
     fi_file AT ROW 23.33 COL 44 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.1 COL 20
     btn-cancel AT ROW 25.1 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 9.57 COL 4.2 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.52 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 9.62 COL 60.6 WIDGET-ID 44
     RECT-11 AT ROW 1 COL 1
     RECT-6 AT ROW 15.33 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.57.


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
         TITLE              = "Finished Goods Inventory By Sales Rep"
         HEIGHT             = 25.81
         WIDTH              = 95.8
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

/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
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
       begin_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-po:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_slm:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_inc-cust IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_inc-cust:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_inc-zer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods Inventory By Sales Rep */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods Inventory By Sales Rep */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Beginning Customer# */
DO:
   ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-po C-Win
ON LEAVE OF begin_cust-po IN FRAME FRAME-A /* Beginning Customer PO# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_slm C-Win
ON LEAVE OF begin_slm IN FRAME FRAME-A /* Beginning Sales Rep# */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

  SESSION:SET-WAIT-STATE("general").
RUN GetSelectionList.
  FIND FIRST  ttCustList NO-LOCK NO-ERROR.
  IF NOT AVAIL ttCustList AND tb_cust-list THEN do:
  EMPTY TEMP-TABLE ttCustList.
  RUN BuildCustList(INPUT cocode,
                    INPUT tb_cust-list AND glCustListActive ,
                    INPUT begin_cust,
                    INPUT end_cust).
  END.
  RUN run-report. 

   STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=END_cust
                            &END_cust=END_cust
                            &fax-subject= c-win:TITLE 
                            &fax-body= c-win:title 
                            &fax-file=list-name }
       END.
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust= END_cust
                             &END_cust=END_cust
                             &mail-subject= c-win:TITLE 
                             &mail-body= c-win:TITLE 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=END_cust
                                  &END_cust=END_cust
                                  &mail-subject= c-win:TITLE 
                                  &mail-body= c-win:TITLE 
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN RUN output-to-port.
  END CASE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustList C-Win
ON CHOOSE OF btnCustList IN FRAME FRAME-A /* Preview */
DO:
  RUN CustList.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
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
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* Ending Customer# */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-po C-Win
ON LEAVE OF end_cust-po IN FRAME FRAME-A /* Ending Customer PO# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_slm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_slm C-Win
ON LEAVE OF end_slm IN FRAME FRAME-A /* Ending Sales Rep# */
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
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-fonts.w (FOCUS:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                  LV-FONT-NAME:SCREEN-VALUE = ENTRY(2,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON HELP OF begin_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON HELP OF end_cust IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val) .

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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-cust C-Win
ON VALUE-CHANGED OF tb_inc-cust IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_inc-zer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_inc-zer C-Win
ON VALUE-CHANGED OF tb_inc-zer IN FRAME FRAME-A /* Include Zero Quantity On Hand? */
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

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
RUN DisplaySelectionList. 
  RUN enable_UI.

  {methods/nowait.i}

  RUN sys/inc/CustListForm.p ( "IR5",cocode, 
                               OUTPUT ou-log,
                               OUTPUT ou-cust-int) .

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
     RUN DisplaySelectionList2.
    APPLY "entry" TO begin_cust IN FRAME {&FRAME-NAME}.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR5',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR5""}

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

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
                            INPUT 'IR5',
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
                                  INPUT 'IR5').


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
  DEF VAR cTmpList AS cha NO-UNDO.

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
  DISPLAY tb_cust-list begin_cust end_cust begin_cust-po end_cust-po begin_slm 
          end_slm tb_inc-zer tb_inc-cust lbl_sort rd_sort sl_avail sl_selected 
          rd-dest lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm 
          tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-11 RECT-6 tb_cust-list btnCustList begin_cust end_cust 
         begin_cust-po end_cust-po begin_slm end_slm tb_inc-zer tb_inc-cust 
         rd_sort sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up 
         btn_down rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
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
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
 iColumnLength = 0.

 DO i = 1 TO sl_selected:NUM-ITEMS /* IN FRAME {&FRAME-NAME}*/ :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.     

    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
           ttRptSelected.DisplayOrder = i
           ttRptSelected.HeadingFromLeft = IF ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
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
/* ------------------------------------------------ fg/rep/fg-ssman.p 3/94 RM */
/*  finished goods inventory by salesrep report                               */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

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
DEF VAR v-row-id AS ROWID NO-UNDO.
{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.

DEF VAR v-ext AS DEC FORMAT "->>,>>>,>>9.99".
DEF VAR fcust AS ch INIT " ".
DEF VAR tcust LIKE fcust INIT "ZZZZZZZZZ".
DEF VAR fcustpo AS ch INIT " ".
DEF VAR tcustpo LIKE fcustpo INIT "ZZZZZZZZZZZZZZZ".
DEF VAR fsman LIKE cust.sman INIT " ".
DEF VAR tsman LIKE cust.sman INIT "ZZZ".
DEF VAR zbal AS LOG FORMAT "Y/N".
DEF VAR v-sub-ext LIKE v-ext.
DEF VAR v-cust-ext LIKE v-ext.
DEF VAR tot-string AS CHAR FORMAT "x(5)" INIT "Total".
DEF VAR v-cost LIKE itemfg.total-std-cost.
DEF VAR v-qty-onh AS INT FORMAT "->>>,>>>,>>9".
DEF VAR v-totcost AS DEC FORMAT "->>>,>>9.99".
DEF VAR v-frst AS LOG.
DEF VAR v-sel-one AS CHAR FORMAT "x(30)".
DEF VAR v-sel-two AS CHAR FORMAT "x(33)".
DEF VAR v-sel-three AS CHAR FORMAT "x(20)".
DEF VAR v-custown AS LOG FORMAT "Y/N" INIT "N".
DEF VAR v-cust-qty AS INT FORMAT "->>>,>>>,>>9".
DEF VAR v-sales-rep AS CHAR NO-UNDO.
DEF VAR sort-opt AS CHAR NO-UNDO INIT "C" FORMAT "!".
DEF VAR lSelected AS LOG INIT YES NO-UNDO.
FORMAT
  itemfg.i-no LABEL "ITEM #"
  itemfg.i-name FORMAT "x(20)" LABEL "DESCRIPTION"
  itemfg.cust-no LABEL "CUST #"
  itemfg.cust-po-no COLUMN-LABEL "CUSTOMER!  PO #"
  itemfg.cust-job-no COLUMN-LABEL "CUSTOMER!  JOB #"
  itemfg.q-prod-ptd COLUMN-LABEL "QUANTITY!PRODUCED" FORMAT "->>,>>>,>>9"
  itemfg.q-ship-ptd COLUMN-LABEL "QUANTITY!SHIPPED" FORMAT "->>,>>>,>>9"
  v-qty-onh COLUMN-LABEL "QUANTITY! ON HAND" FORMAT "->>>,>>>,>>9"
  v-totcost COLUMN-LABEL "TOTAL!COST" FORMAT "->>,>>>,>>9.99"
  v-ext COLUMN-LABEL "TOTAL!VALUE"  FORMAT "->>,>>>,>>9.99"
  WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 132.

FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.


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

        IF LOOKUP(ttRptSelected.TextList, "Total Value") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 fcust        = begin_cust
 tcust        = end_cust
 fcustpo      = begin_cust-po
 tcustpo      = END_cust-po
 fsman        = begin_slm
 tsman        = end_slm
 zbal         = tb_inc-zer
 v-custown    = tb_inc-cust
 sort-opt     = SUBSTR(rd_sort,1,1)
 lSelected    = tb_cust-list.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF td-show-parm THEN RUN show-param.

DISPLAY "" WITH FRAME r-top.

SESSION:SET-WAIT-STATE ("general").

IF lselected THEN DO:
      FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
      IF AVAIL ttCustList THEN ASSIGN fcust = ttCustList.cust-no .
      FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
      IF AVAIL ttCustList THEN ASSIGN tcust = ttCustList.cust-no .
  END.

EMPTY TEMP-TABLE tt-itemfg.

  FOR EACH itemfg
        NO-LOCK
        USE-INDEX customer
        WHERE itemfg.company EQ cocode 
      AND itemfg.cust-no GE fcust 
      AND itemfg.cust-no LE tcust 
      AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
      AND ttCustList.log-fld no-lock) else true)
      AND itemfg.cust-po-no GE fcustpo 
      AND itemfg.cust-po-no LE tcustpo,
        FIRST cust
        NO-LOCK
        WHERE cust.company EQ itemfg.company
      /*and cust.sman ge fsman  and cust.sman le tsman*/ AND
              cust.cust-no EQ itemfg.cust-no
        BREAK
        BY cust.sman
        BY (IF sort-opt EQ "C" THEN itemfg.cust-no ELSE itemfg.i-no)
        BY (IF sort-opt EQ "C" THEN itemfg.i-no    ELSE itemfg.cust-no):

        {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"} 

      v-sales-rep = "" .

      RUN fg/fgSlsRep.p (INPUT itemfg.company,
          INPUT itemfg.cust-no,
          INPUT itemfg.part-no,
          INPUT itemfg.i-no,
          OUTPUT v-sales-rep).

      IF v-sales-rep GE fsman  AND v-sales-rep LE tsman THEN DO:
          CREATE tt-itemfg.
          ASSIGN
           tt-itemfg.row-id    = ROWID(itemfg)
           tt-itemfg.i-no      = itemfg.i-no
           tt-itemfg.cust-no   = itemfg.cust-no
           tt-itemfg.part-no   = itemfg.part-no
           tt-itemfg.sales-rep = v-sales-rep
          .
      END.
  END.



 /*for each itemfg
        no-lock
        use-index customer
        where itemfg.company eq cocode 
      AND itemfg.cust-no ge fcust 
      and itemfg.cust-no le tcust 
      AND itemfg.cust-po-no ge fcustpo 
      and itemfg.cust-po-no le tcustpo,*/
  FOR EACH tt-itemfg WHERE 
       tt-itemfg.sales-rep GE fsman  AND tt-itemfg.sales-rep LE tsman NO-LOCK,
       FIRST itemfg WHERE itemfg.company = cocode 
           AND itemfg.i-no = tt-itemfg.i-no
           AND itemfg.cust-no = itemfg.cust-no NO-LOCK,
        FIRST cust
        NO-LOCK
        WHERE cust.company EQ itemfg.company
          /*and cust.sman ge fsman  and cust.sman le tsman*/ AND
              cust.cust-no EQ itemfg.cust-no
        BREAK
        BY tt-itemfg.sales-rep
        BY (IF sort-opt EQ "C" THEN itemfg.cust-no ELSE itemfg.i-no)
        BY (IF sort-opt EQ "C" THEN itemfg.i-no    ELSE itemfg.cust-no):



      IF FIRST-OF(tt-itemfg.sales-rep) THEN
      ASSIGN v-frst = YES.

      ASSIGN v-totcost = 0
        v-ext = 0
        v-cost = 0
        v-qty-onh = 0
        v-cust-qty = 0.

      FOR EACH fg-bin WHERE fg-bin.company EQ cocode AND
          fg-bin.i-no EQ itemfg.i-no NO-LOCK USE-INDEX i-no
          BREAK BY fg-bin.i-no:

        IF fg-bin.loc NE "CUST" AND trim(fg-bin.cust-no) EQ "" THEN
        ASSIGN v-qty-onh = v-qty-onh + fg-bin.qty.
        ELSE
        ASSIGN v-cust-qty = v-cust-qty + fg-bin.qty.

        IF v-custown OR
          (NOT v-custown AND fg-bin.loc NE "CUST" AND trim(fg-bin.cust-no) EQ "") THEN
        DO:

          IF CAPS(fg-ctrl.inv-meth) EQ "L" THEN
          v-cost = itemfg.last-cost.
          ELSE
          ASSIGN v-cost = fg-bin.std-tot-cost.

          /* Calculate Cost */
          IF itemfg.pur-uom EQ "CS" AND itemfg.case-count NE 0 THEN
          ASSIGN v-totcost = v-totcost + (fg-bin.qty * v-cost)
            / itemfg.case-count.
          ELSE
          FIND FIRST uom WHERE uom.uom EQ itemfg.sell-uom AND
            uom.mult NE 0 NO-LOCK NO-ERROR.
          IF AVAIL uom THEN
          ASSIGN v-totcost = v-totcost + (fg-bin.qty * v-cost / uom.mult).
          ELSE
          ASSIGN v-totcost = v-totcost + (fg-bin.qty * v-cost) / 1000.

          IF itemfg.pur-uom EQ "L" THEN
          ASSIGN v-totcost = v-cost.

          FIND FIRST oe-ordl
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.job-no  EQ fg-bin.job-no
                AND oe-ordl.job-no2 EQ fg-bin.job-no2
                AND oe-ordl.i-no    EQ fg-bin.i-no
              USE-INDEX job NO-LOCK NO-ERROR.

          /* Calculate Selling Price */
          IF AVAIL oe-ordl THEN DO:
            IF (oe-ordl.t-price / oe-ordl.qty * fg-bin.qty) NE ? THEN
              v-ext = v-ext + (oe-ordl.t-price / oe-ordl.qty * fg-bin.qty).
          END.

          ELSE DO:
            IF itemfg.sell-uom EQ "CS" AND itemfg.case-count NE 0 THEN
              v-ext = v-ext + (fg-bin.qty * itemfg.sell-price / itemfg.case-count).
            ELSE 
            IF itemfg.sell-uom EQ "L" THEN v-ext = v-ext + itemfg.sell-price.
            ELSE DO:
              FIND FIRST uom
                  WHERE uom.uom EQ itemfg.sell-uom
                    AND uom.mult NE 0
                  NO-LOCK NO-ERROR.
              IF AVAIL uom THEN
                v-ext = v-ext + (fg-bin.qty * itemfg.sell-price / uom.mult).
              ELSE
                v-ext = v-ext + (fg-bin.qty * itemfg.sell-price / 1000).
            END.
          END.
        END.

        IF LAST-OF(fg-bin.i-no) THEN
        DO:
          /** include customer owned inventory in qty on hand
          if v-custown = true. **/
          IF v-custown THEN
          ASSIGN v-qty-onh = v-qty-onh + v-cust-qty.
          /**
          if v-ext < 0 then assign v-ext = 0.
          if v-totcost < 0 then assign v-totcost = 0.
          **/
          IF (itemfg.q-onh NE 0 ) OR (itemfg.q-onh EQ 0 AND zbal)  THEN
          DO:
            IF LINE-COUNTER GE 56 THEN
            PAGE.
          /*  if v-frst then
            do: */
              FIND FIRST sman WHERE sman.company EQ cust.company AND
                sman.sman EQ tt-itemfg.sales-rep NO-LOCK NO-ERROR.
           /*   if avail sman then
              put "SALESREP: " cust.sman SPACE(2) sman.sname skip(1).
              else
              put "SALESREP: " cust.sman SPACE(2) "UNKNOWN" skip(1).
              assign v-frst = NO.
            end.
            display
              itemfg.i-no itemfg.i-name itemfg.cust-po-no itemfg.cust-job-no
              itemfg.q-prod-ptd itemfg.q-ship-ptd v-qty-onh itemfg.cust-no
              v-totcost v-ext
              with frame itemx.
            down with frame itemx.

           IF tb_excel THEN 
             EXPORT STREAM excel DELIMITER ","
                    itemfg.i-no 
                    itemfg.i-name
                    itemfg.cust-no 
                    itemfg.cust-po-no 
                    itemfg.cust-job-no
                    itemfg.q-prod-ptd 
                    itemfg.q-ship-ptd 
                    v-qty-onh
                    v-totcost v-ext
                    SKIP. */

                        ASSIGN cDisplay = ""
                               cTmpField = ""
                               cVarValue = ""
                               cExcelDisplay = ""
                               cExcelVarValue = "".

                        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                           cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                                CASE cTmpField:             
                                     WHEN "sman"     THEN cVarValue = IF AVAIL sman THEN STRING(tt-itemfg.sales-rep) ELSE " " .
                                     WHEN "sname"    THEN cVarValue = IF AVAIL sman THEN STRING(sman.sname,"x(20)") ELSE " " .
                                     WHEN "itm"      THEN cVarValue = STRING(itemfg.i-no,"x(15)").
                                     WHEN "dscr"     THEN cVarValue = STRING(itemfg.i-name,"x(20)") .
                                     WHEN "cust"     THEN cVarValue = STRING(itemfg.cust-no,"x(8)") .
                                     WHEN "cust-nam" THEN cVarValue = STRING(cust.NAME,"x(30)")  .
                                     WHEN "cust-po"  THEN cVarValue = STRING(itemfg.cust-po-no,"x(9)").
                                     WHEN "cust-job" THEN cVarValue = STRING(itemfg.cust-job-no,"x(9)").
                                     WHEN "qty-pro"  THEN cVarValue = STRING(itemfg.q-prod-ptd,"->>,>>>,>>9")  .
                                     WHEN "qty-ship" THEN cVarValue = STRING(itemfg.q-ship-ptd,"->>,>>>,>>9") .
                                     WHEN "qty-onh"  THEN cVarValue = IF v-qty-onh NE ? THEN STRING(v-qty-onh,"->>,>>>,>>9") ELSE "". 
                                     WHEN "ttl-cst"  THEN cVarValue = IF v-totcost NE ? THEN STRING(v-totcost,"->>,>>>,>>9.99") ELSE "".
                                     WHEN "ttl-val"  THEN cVarValue = IF v-ext NE ? THEN STRING(v-ext,"->>,>>>,>>9.99") ELSE "".

                                END CASE.

                                cExcelVarValue = cVarValue.
                                cDisplay = cDisplay + cVarValue +
                                           FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                        END.

                        PUT UNFORMATTED cDisplay SKIP.
                        IF tb_excel THEN DO:
                             PUT STREAM excel UNFORMATTED  
                                   cExcelDisplay SKIP.
                         END.

               ASSIGN v-sub-ext = v-sub-ext + v-ext
                 v-cust-ext = v-cust-ext + v-ext /* 9508 CAH */
                 v-totcost = 0
                 v-ext = 0
                 v-qty-onh = 0.
          END.
   END. /* last-of fg-bin.i-no */
 END. /* each bin */
 IF LAST-OF
   (IF sort-opt EQ "C" THEN itemfg.cust-no ELSE itemfg.i-no)
   AND sort-opt EQ "C"  AND v-cust-ext NE 0
   THEN
 DO: /* 9508 CAH */
  /* put "--------------" TO 132 skip
     "CUSTOMER TOTAL" TO 116 v-cust-ext TO 132 skip(1). */
                    PUT    SKIP  str-line SKIP .
                    ASSIGN cDisplay = ""
                               cTmpField = ""
                               cVarValue = ""
                               cExcelDisplay = ""
                               cExcelVarValue = "".

                        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                           cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                                CASE cTmpField:             
                                     WHEN "sman"     THEN cVarValue = " " .
                                     WHEN "sname"    THEN cVarValue = " " .
                                     WHEN "itm"      THEN cVarValue = " " .
                                     WHEN "dscr"     THEN cVarValue = " " .
                                     WHEN "cust"     THEN cVarValue = " " .
                                     WHEN "cust-nam" THEN cVarValue = " " .
                                     WHEN "cust-po"  THEN cVarValue = " " .
                                     WHEN "cust-job" THEN cVarValue = " " .
                                     WHEN "qty-pro"  THEN cVarValue = " " .
                                     WHEN "qty-ship" THEN cVarValue = " " .
                                     WHEN "qty-onh"  THEN cVarValue = " " .
                                     WHEN "ttl-cst"  THEN cVarValue = " " .
                                     WHEN "ttl-val"  THEN cVarValue = STRING(v-cust-ext,"->>,>>>,>>9.99") .

                                END CASE.

                                cExcelVarValue = cVarValue.
                                cDisplay = cDisplay + cVarValue +
                                           FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                        END.

                        PUT UNFORMATTED "   CUSTOMER Totals" SUBSTRING(cDisplay,19,300) SKIP(1).
                       /* IF tb_excel THEN DO:
                             PUT STREAM excel UNFORMATTED  
                                   "CUSTOMER TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                         END.*/
   ASSIGN v-cust-ext = 0.
 END.

 IF LAST-OF(tt-itemfg.sales-rep) AND v-sub-ext NE 0 THEN
 DO:
  /* put "--------------" TO 132 skip
     "SALESREP TOTAL" TO 116 v-sub-ext TO 132 skip(1).*/
                        PUT    SKIP  str-line SKIP .
                        ASSIGN cDisplay = ""
                               cTmpField = ""
                               cVarValue = ""
                               cExcelDisplay = ""
                               cExcelVarValue = "".

                        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                           cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                                CASE cTmpField:             
                                     WHEN "sman"     THEN cVarValue = " " .
                                     WHEN "sname"    THEN cVarValue = " " .
                                     WHEN "itm"      THEN cVarValue = " " .
                                     WHEN "dscr"     THEN cVarValue = " " .
                                     WHEN "cust"     THEN cVarValue = " " .
                                     WHEN "cust-nam" THEN cVarValue = " " .
                                     WHEN "cust-po"  THEN cVarValue = " " .
                                     WHEN "cust-job" THEN cVarValue = " " .
                                     WHEN "qty-pro"  THEN cVarValue = " " .
                                     WHEN "qty-ship" THEN cVarValue = " " .
                                     WHEN "qty-onh"  THEN cVarValue = " " .
                                     WHEN "ttl-cst"  THEN cVarValue = " " .
                                     WHEN "ttl-val"  THEN cVarValue = STRING(v-sub-ext,"->>,>>>,>>9.99") .

                                END CASE.

                                cExcelVarValue = cVarValue.
                                cDisplay = cDisplay + cVarValue +
                                           FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                                cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                        END.

                        PUT UNFORMATTED "   SALESREP Totals" SUBSTRING(cDisplay,19,300) SKIP(1).
                        /*IF tb_excel THEN DO:
                             PUT STREAM excel UNFORMATTED  
                                   "SALESREP TOTALS " + substring(cExcelDisplay,3,300) SKIP.
                         END.*/
   ASSIGN v-sub-ext = 0.
 END.
 END. /* for each itemfg and each cust */

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
       OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

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
        begin_cust:SENSITIVE = NOT iplChecked
        end_cust:SENSITIVE = NOT iplChecked
        begin_cust:VISIBLE = NOT iplChecked
        end_cust:VISIBLE = NOT iplChecked
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
  DEF VAR lv-frame-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-group-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
  DEF VAR parm-fld-list AS cha NO-UNDO.
  DEF VAR parm-lbl-list AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR lv-label AS cha.

  lv-frame-hdl = FRAME {&frame-name}:handle.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .

  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0
        THEN DO:
           IF lv-field-hdl:LABEL <> ? THEN 
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + "," 
                     .
           ELSE DO:  /* radio set */
              ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","
                     .
              lv-field2-hdl = lv-group-hdl:FIRST-CHILD.
              REPEAT:
                  IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE. 
                  IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN DO:
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
       entry(i,parm-lbl-list) NE "" THEN DO:

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

