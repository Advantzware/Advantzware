&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd_fgu-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd_fgu-exp 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER lcSearch   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER lcsearchby AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{custom/gperiod.i}
{custom/persist.i}

{methods/defines/hndldefs.i}

DEFINE {&NEW} SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
{sys/inc/var.i new shared}

 v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
 v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

DEFINE VARIABLE v-bal AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-net AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-onh AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO. 
DEFINE VARIABLE v-ono AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-opo AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report
    FIELD required AS INT
    FIELD variance AS INT.

assign
 cocode = g_company
 /*locode = gloc*/ .
{sa/sa-sls01.i}
DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "FG Item,Release Date,Required,Variance,Ordered Qty,On Hand Qty," +
                            "Net Qty,Bal to Ship,Open PO's,Release"
      cFieldListToSelect = "item,rel-date,req,var,order,qty-hand," +
                            "net-qty,bal-ship,open-po,release".
{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "FG Item,Release Date,Required,Variance,Ordered Qty,On Hand Qty," +
                                "Net Qty,Bal to Ship,Open PO's,Release" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd_fgu-exp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_i-no begin_date ~
sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up btn_down tb_runExcel ~
fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_i-no begin_date sl_avail sl_selected ~
tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd_fgu-exp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd_fgu-exp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd_fgu-exp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd_fgu-exp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" 
     LABEL "From Ship Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "x(15)" 
     LABEL "From FG Item" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgu-exp.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.62.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 2.48.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 6.14 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd_fgu-exp
     begin_i-no AT ROW 4.24 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Customer Number" WIDGET-ID 146
     begin_date AT ROW 4.24 COL 71 COLON-ALIGNED HELP
          "Enter Ending Customer" WIDGET-ID 148
     sl_avail AT ROW 10.71 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 10.71 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 10.81 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 11.95 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 13.14 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 14.33 COL 44 WIDGET-ID 136
     btn_down AT ROW 15.52 COL 44 WIDGET-ID 132
     tb_excel AT ROW 17.38 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 17.38 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 18.33 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 20.19 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 20.19 COL 60.2 WIDGET-ID 12
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10 COL 64.4 WIDGET-ID 138
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 10 COL 9.4 WIDGET-ID 140
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 8.86 COL 3 WIDGET-ID 86
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     RECT-6 AT ROW 9.24 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 17.05 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(3.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Sched vs Qoh to Excel" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX rd_fgu-exp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd_fgu-exp:SCROLLABLE       = FALSE
       FRAME rd_fgu-exp:HIDDEN           = TRUE.

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME rd_fgu-exp     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME rd_fgu-exp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd_fgu-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd_fgu-exp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd_fgu-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd_fgu-exp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd_fgu-exp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd_fgu-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fgu-exp rd_fgu-exp
ON HELP OF FRAME rd_fgu-exp /* Export Sched vs Qoh to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :

       when "begin_cust-type" then do:
           ls-cur-val = lw-focus:screen-value.
           RUN windows/l-cust.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_cust-type" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-cust.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_fgu-exp rd_fgu-exp
ON WINDOW-CLOSE OF FRAME rd_fgu-exp /* Export Sched vs Qoh to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date rd_fgu-exp
ON LEAVE OF begin_date IN FRAME rd_fgu-exp /* From Ship Date */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_i-no rd_fgu-exp
ON LEAVE OF begin_i-no IN FRAME rd_fgu-exp /* From FG Item */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd_fgu-exp
ON CHOOSE OF btn-cancel IN FRAME rd_fgu-exp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd_fgu-exp
ON CHOOSE OF btn-ok IN FRAME rd_fgu-exp /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  RUN GetSelectionList.

  ASSIGN
      v-bal = 0
      v-net = 0
      v-onh = 0
      v-ono = 0
      v-opo = 0 .
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd_fgu-exp
ON CHOOSE OF Btn_Add IN FRAME rd_fgu-exp /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd_fgu-exp
ON CHOOSE OF Btn_Def IN FRAME rd_fgu-exp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd_fgu-exp
ON CHOOSE OF btn_down IN FRAME rd_fgu-exp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd_fgu-exp
ON CHOOSE OF Btn_Remove IN FRAME rd_fgu-exp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd_fgu-exp
ON CHOOSE OF btn_Up IN FRAME rd_fgu-exp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd_fgu-exp
ON LEAVE OF fi_file IN FRAME rd_fgu-exp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd_fgu-exp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd_fgu-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd_fgu-exp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd_fgu-exp
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd_fgu-exp
ON VALUE-CHANGED OF tb_excel IN FRAME rd_fgu-exp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd_fgu-exp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd_fgu-exp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd_fgu-exp 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i}
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
  RUN DisplaySelectionList.
  RUN enable_UI.
   {methods/nowait.i}
   DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    RUN Set-Sort-Data.



    APPLY "entry" TO begin_i-no.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile rd_fgu-exp 
PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-rel     LIKE v-ono NO-UNDO.
  DEF VAR v-var     LIKE v-ono NO-UNDO.
  DEF VAR v-hld-qty AS   DEC   NO-UNDO.
  DEF VAR v-loc     LIKE oe-boll.loc NO-UNDO.

  FOR EACH tt-report:
    DELETE tt-report.
  END.
  
  FIND FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ trim(begin_i-no)
      NO-LOCK NO-ERROR.
/*  fi_name = itemfg.i-name.*/

  v-onh = 0.
  IF AVAIL itemfg THEN
  for each fg-bin
      where fg-bin.company eq cocode
        and fg-bin.i-no    eq itemfg.i-no
      no-lock:
    v-onh = v-onh + fg-bin.qty.
  end.
  v-net = v-onh.

  assign
   v-rel = 0
   v-ono = 0
   v-bal = 0.

  for each oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.i-no    eq itemfg.i-no
      use-index item no-lock,

      first oe-ord of oe-ordl
      where oe-ord.stat     ne "D"
        and oe-ord.stat     ne "C"
        and oe-ord.stat     ne "Z"
      no-lock,

      each oe-rel
      where oe-rel.company  eq cocode
        and oe-rel.ord-no   eq oe-ord.ord-no
        and oe-rel.i-no     eq oe-ordl.i-no
        and oe-rel.line     eq oe-ordl.line
      no-lock

      break by oe-rel.rel-date:

    assign
     v-bal = v-bal + oe-rel.qty
     v-rel = v-rel + oe-rel.qty
     v-ono = v-ono + oe-rel.qty
     v-net = v-net - oe-rel.qty.

    RELEASE oe-bolh.

    IF oe-rel.link-no NE 0 THEN
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-rel.company
          AND oe-rell.ord-no  EQ oe-rel.ord-no
          AND oe-rell.r-no    EQ oe-rel.link-no
          AND oe-rell.i-no    EQ oe-rel.i-no
          AND oe-rell.line    EQ oe-rel.line
        NO-LOCK,
        FIRST oe-relh
        WHERE oe-relh.r-no   EQ oe-rell.r-no
          AND oe-relh.posted EQ YES
        NO-LOCK,
        EACH oe-boll
        WHERE oe-boll.company  EQ oe-rell.company
          AND oe-boll.ord-no   EQ oe-rell.ord-no
          AND oe-boll.line     EQ oe-rell.line
          AND oe-boll.i-no     EQ oe-rell.i-no
          AND oe-boll.r-no     EQ oe-rell.r-no
          AND oe-boll.rel-no   EQ oe-rell.rel-no
          AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
        NO-LOCK,
        FIRST oe-bolh
        WHERE oe-bolh.b-no   EQ oe-boll.b-no
          AND oe-bolh.posted EQ YES
        NO-LOCK:
      LEAVE.
    END.

    if avail oe-bolh then
      assign
       v-bal = v-bal - oe-rell.qty
       v-rel = v-rel - oe-rell.qty
       v-net = v-net + oe-rell.qty.
  
    if last-of(oe-rel.rel-date)  and
       oe-rel.rel-date ge begin_date AND
       v-rel gt 0               then do:
      create tt-report.
      assign
       tt-report.term-id = v-term
       tt-report.key-01   = string(year(oe-rel.rel-date),"9999") +
                            string(month(oe-rel.rel-date),"99")  +
                            string(day(oe-rel.rel-date),"99")
       tt-report.key-02   = string(month(oe-rel.rel-date),"99")  + "/" +
                            string(day(oe-rel.rel-date),"99")    + "/" +
                            string(year(oe-rel.rel-date),"9999")
       tt-report.key-03   = string(v-rel,"-9999999999")
       tt-report.key-04   = string(v-net,"-9999999999")
       tt-report.required = v-rel
       tt-report.variance = v-net
       tt-report.key-05   = string(oe-rel.i-no)
        tt-report.key-06 = IF AVAIL oe-relh THEN string(oe-relh.release#) ELSE "0" .
       v-rel = 0.
      
    end.
  end.

  v-opo = 0.
  for each po-ordl
      where po-ordl.company   eq itemfg.company
        and po-ordl.i-no      eq itemfg.i-no
        and po-ordl.item-type eq no
        and lookup(po-ordl.stat,"O,P,U") gt 0
        and po-ordl.t-rec-qty lt po-ordl.cons-qty
      no-lock,

      first po-ord WHERE
            po-ord.company EQ po-ordl.company AND
            po-ord.po-no   EQ po-ordl.po-no AND
            lookup(po-ord.stat,"N,O,R,U") gt 0
        and po-ord.po-date                le begin_date
      no-lock:

    if po-ordl.cons-uom eq "EA" then
      v-hld-qty = po-ordl.cons-qty.
    else
      run sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0,
                             po-ordl.cons-qty, output v-hld-qty).

    if v-hld-qty - po-ordl.t-rec-qty gt 0 then
      v-opo = v-opo + (v-hld-qty - po-ordl.t-rec-qty).
  end.

  v-net = v-net + v-opo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd_fgu-exp  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME rd_fgu-exp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd_fgu-exp 
PROCEDURE DisplaySelectionList2 :
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

  DO iCount = 1 TO sl_selected:NUM-ITEMS:
      ldummy = sl_avail:DELETE(sl_selected:ENTRY(iCount)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd_fgu-exp  _DEFAULT-ENABLE
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
  DISPLAY begin_i-no begin_date sl_avail sl_selected tb_excel tb_runExcel 
          fi_file 
      WITH FRAME rd_fgu-exp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_i-no begin_date sl_avail sl_selected 
         Btn_Def Btn_Add Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok 
         btn-cancel 
      WITH FRAME rd_fgu-exp.
  VIEW FRAME rd_fgu-exp.
  {&OPEN-BROWSERS-IN-QUERY-rd_fgu-exp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd_fgu-exp 
PROCEDURE GetSelectionList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR cTmpList AS cha NO-UNDO.

 EMPTY TEMP-TABLE ttRptSelected.
 cTmpList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.

 DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME} :
    FIND FIRST ttRptList WHERE ttRptList.TextList = ENTRY(i,cTmpList) NO-LOCK NO-ERROR.  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           /* ttRptSelected.FieldLength */
        .   
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd_fgu-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd_fgu-exp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF BUFFER b-cust FOR cust.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

 RUN create-tempfile .

    v-excel-detail-lines = "".

    FOR EACH tt-report  NO-LOCK:
        v-excel-detail-lines = "".
       FOR EACH ttRptSelected:
         /* IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email") <> 0    THEN do: */
               IF ttRptSelected.FieldList = "item" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(tt-report.key-05).
              IF ttRptSelected.FieldList = "rel-date" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(tt-report.key-02).
              IF ttRptSelected.FieldList = "req" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-report.required)).
              IF ttRptSelected.FieldList = "var" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-report.variance)).

              IF ttRptSelected.FieldList = "order" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-ono)).
              IF ttRptSelected.FieldList = "qty-hand" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-onh)).
              IF ttRptSelected.FieldList = "net-qty" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-net)).
              IF ttRptSelected.FieldList = "bal-ship" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-bal)).
              IF ttRptSelected.FieldList = "open-po" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(v-opo)).
              IF ttRptSelected.FieldList = "release" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-report.key-06)).
        END.  /* each ttrptse */

       PUT STREAM excel UNFORMATTED v-excel-detail-lines SKIP.

    END. /* for each phone */

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd_fgu-exp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    IF lcSearch <> "" THEN
        ASSIGN
        begin_i-no:SCREEN-VALUE = lcSearch .
     IF lcsearchby <> "" OR lcsearchby NE ? THEN
         begin_date:SCREEN-VALUE = STRING(lcsearchby) .
END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd_fgu-exp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Adds a value to a csv line
    Notes:  Protects agains commans and quotes.
------------------------------------------------------------------------------*/
    DEF VAR lc-line AS CHAR NO-UNDO.

    ipc-append = REPLACE(ipc-append, '"', '').
    ipc-append = REPLACE(ipc-append, ',', ' ').
    lc-line = lc-line + '"' + ipc-append + '",'.
    RETURN lc-line.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd_fgu-exp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lc-return AS CHAR.

    IF ipl-end THEN
        lc-return = ipc-param + "ZZZZZZZZZZZZZZZ".
    ELSE
        lc-return = ipc-param.

  RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd_fgu-exp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR lc-header AS CHAR NO-UNDO.

FOR EACH ttRptSelected:
    lc-header = lc-header + appendXLLine(ttRptSelected.TextList).
END.
/*     lc-header = lc-header + appendXLLine ("PO #").      */
/*     lc-header = lc-header + appendXLLine ("Vendor #").  */
/*     lc-header = lc-header + appendXLLine ("Due Date").  */
/*     lc-header = lc-header + appendXLLine ("Ship ID").   */
/*     lc-header = lc-header + appendXLLine ("Ship Name"). */
/*     lc-header = lc-header + appendXLLine ("Job #").     */
/*     lc-header = lc-header + appendXLLine ("Item #").    */
/*     lc-header = lc-header + appendXLLine ("Item Name"). */

  
    RETURN lc-header.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd_fgu-exp 
FUNCTION getValue-itemfg RETURNS CHARACTER
  ( BUFFER ipb-itemfg FOR cust, ipc-field AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Take a buffer and field name as string and return the value
    Notes:  
------------------------------------------------------------------------------*/
{custom/getperd.i} 
    DEF VAR h-field AS HANDLE.
    DEF VAR li-extent AS INT NO-UNDO.
    DEF VAR lc-return AS CHAR FORMAT "x(100)" NO-UNDO.
    DEF VAR ptd-profit1 AS CHAR NO-UNDO.
    DEF VAR ytd-profit1 AS CHAR NO-UNDO.
    DEF VAR lyr-profit1 AS CHAR NO-UNDO.
    DEF VAR ptd-sales1 AS DEC NO-UNDO.

    CASE ipc-field :
        WHEN "ptd-profit" THEN DO:
            lc-return = STRING(ipb-itemfg.sales[gperiod] - ipb-itemfg.cost[1]) .
        END.
        
        OTHERWISE DO:
            IF INDEX(ipc-field,"[") > 0 THEN DO:
                li-extent = INT(SUBSTRING(ipc-field,INDEX(ipc-field,"[") + 1, LENGTH(TRIM(ipc-field)) - INDEX(ipc-field,"[") - 1)).
                ipc-field = SUBSTRING(ipc-field,1,INDEX(ipc-field,"[") - 1).
            END.
            h-field = BUFFER ipb-itemfg:BUFFER-FIELD(ipc-field).
            IF h-field:EXTENT = 0 THEN
                lc-return = STRING(h-field:BUFFER-VALUE /*, h-field:FORMAT*/ ).
            ELSE
                lc-return = STRING(h-field:BUFFER-VALUE(li-extent) /*, h-field:FORMAT*/ ).
        END.
    END CASE.
    IF lc-return EQ ? THEN lc-return = "".
    RETURN lc-return.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

