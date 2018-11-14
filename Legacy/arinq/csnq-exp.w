&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS rd-fgnq-exp 
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
/*{methods/prgsecur.i}          */

/*{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}*/
DEFINE {&NEW} SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
{sys/inc/var.i new shared}
 v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1).
 v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

assign
 cocode = g_company
 /*locode = gloc*/ .


DEF TEMP-TABLE tt-arinq NO-UNDO
  FIELD ref-num AS CHAR FORM "x(15)" LABEL "Ck/Cr/Dr/Po#"
  FIELD inv-no LIKE ar-inv.inv-no LABEL "Invoice#"
  FIELD tr-date AS DATE FORM "99/99/9999" LABEL "Date"
  FIELD tr-dscr LIKE gltrans.tr-dscr LABEL "Description"
  FIELD tr-damt LIKE gltrans.tr-amt LABEL "Debits"
  FIELD tr-camt LIKE gltrans.tr-amt LABEL "Credits"
  FIELD ageapp AS CHAR FORM "x(5)" LABEL "Age App"
  FIELD tr-from AS CHAR LABEL "Inquiry From"
  FIELD balance AS dec FORM "->>>,>>>,>>9.99" LABEL "Balance"
  FIELD applied AS LOG
  FIELD seq AS INT
  FIELD printed LIKE ar-inv.printed
  FIELD posted LIKE ar-inv.posted
  INDEX seq seq
  INDEX applied IS PRIMARY applied seq
  INDEX ref-num ref-num seq
  INDEX inv-no inv-no seq
  INDEX tr-date tr-date seq
  INDEX tr-dscr tr-dscr seq
  INDEX tr-damt tr-damt seq
  INDEX tr-camt tr-camt seq
  INDEX ageapp ageapp seq
  INDEX tr-from tr-from seq
  INDEX balance balance seq.

DEF VAR lv-first AS LOG NO-UNDO.
DEF VAR lv-sort-by AS cha NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR v-format AS cha NO-UNDO.
DEF NEW SHARED VAR uperiod AS INT NO-UNDO.  /* for gl-open.p */
DEF VAR v-gltrans-desc AS CHAR NO-UNDO.

&SCOPED-DEFINE SORTBY-ASC ASCENDING
&SCOPED-DEFINE SORTBY-DES DESCENDING

DEF VAR lv-save-char AS CHAR INIT "" NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ARINQ"
    no-lock no-error.
if not avail sys-ctrl then do on error undo, retry transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ARINQ"
   sys-ctrl.descrip  = "AR Customer Activity Inquiry format"
   sys-ctrl.char-fld = "ASI".
  message "Enter" sys-ctrl.descrip " (ASI/Fibre)"
          update sys-ctrl.char-fld.
  if lookup(sys-ctrl.char-fld,"ASI,Fibre") eq 0 then undo, retry.
end.
v-format = sys-ctrl.char-fld.


DEFINE STREAM excel.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.

ASSIGN cTextListToSelect = "Ck/Cr/Dr/Po#,Invoice#,Date,Description,Age App,Debits,Credits,Balance"

      cFieldListToSelect = "ck-cr,inv,date,dscr,ag-ap,deb,cre,bal" .
{sys/inc/ttRptSel.i}

    ASSIGN cTextListToDefault  = "Ck/Cr/Dr/Po#,Invoice#,Date,Description,Age App,Debits,Credits,Balance".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME rd-fgnq-exp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-8 begin_cust end_cust ~
begin_chk end_chk begin_inv end_inv tb_open sl_avail sl_selected Btn_Def ~
Btn_Add Btn_Remove btn_Up btn_down tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust begin_chk end_chk ~
begin_inv end_inv tb_open sl_avail sl_selected tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD appendXLLine rd-fgnq-exp 
FUNCTION appendXLLine RETURNS CHARACTER
  ( ipc-append AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD assignParam rd-fgnq-exp 
FUNCTION assignParam RETURNS CHARACTER
  ( ipc-param AS CHAR , ipl-end AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildHeader rd-fgnq-exp 
FUNCTION buildHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue-itemfg rd-fgnq-exp 
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

DEFINE VARIABLE begin_chk AS DECIMAL FORMAT ">>>>>>>>>>":U INITIAL 0 
     LABEL "From Check#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "x(10)" 
     LABEL "From Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "From Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_chk AS DECIMAL FORMAT ">>>>>>>>>>":U INITIAL 2147483647
     LABEL "To Check#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(10)" INITIAL "zzzzzzzzzzz" 
     LABEL "To Customer#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE end_inv AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999  
     LABEL "To Invoice#" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-frmitm.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 9.29.

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

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open Invoices Only?" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.8 BY .81 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL yes 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME rd-fgnq-exp
     begin_cust AT ROW 3.33 COL 25.6 COLON-ALIGNED HELP
          "Enter Beginning Customer#" WIDGET-ID 142
     end_cust AT ROW 3.43 COL 67.6 COLON-ALIGNED HELP
          "Enter Ending Customer#" WIDGET-ID 144
     begin_chk AT ROW 4.71 COL 25.6 COLON-ALIGNED HELP
          "Enter Beginning Vendor#" WIDGET-ID 146
     end_chk AT ROW 4.81 COL 67.6 COLON-ALIGNED HELP
          "Enter Ending Vendor#" WIDGET-ID 148
     begin_inv AT ROW 6.19 COL 25.6 COLON-ALIGNED HELP
          "Enter Beginning Vendor#" WIDGET-ID 150
     end_inv AT ROW 6.29 COL 67.6 COLON-ALIGNED HELP
          "Enter Ending Vendor#" WIDGET-ID 152
     tb_open AT ROW 8.19 COL 66 RIGHT-ALIGNED WIDGET-ID 154
     sl_avail AT ROW 12.24 COL 9 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 12.24 COL 64 NO-LABEL WIDGET-ID 28
     Btn_Def AT ROW 12.43 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 13.62 COL 44 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 130
     Btn_Remove AT ROW 14.81 COL 44 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 134
     btn_Up AT ROW 16 COL 44 WIDGET-ID 136
     btn_down AT ROW 17.19 COL 44 WIDGET-ID 132
     tb_excel AT ROW 18.91 COL 36 WIDGET-ID 32
     tb_runExcel AT ROW 18.91 COL 78 RIGHT-ALIGNED WIDGET-ID 34
     fi_file AT ROW 19.86 COL 34 COLON-ALIGNED HELP
          "Enter File Name" WIDGET-ID 22
     btn-ok AT ROW 21.71 COL 30 WIDGET-ID 14
     btn-cancel AT ROW 21.71 COL 60.2 WIDGET-ID 12
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5 WIDGET-ID 36
          BGCOLOR 2 
     "Export Selection" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 10.52 COL 3 WIDGET-ID 86
     "Selected Columns" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.52 COL 64.4 WIDGET-ID 138
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.52 COL 9.4 WIDGET-ID 140
     RECT-6 AT ROW 10.76 COL 2 WIDGET-ID 30
     RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 38
     RECT-8 AT ROW 18.62 COL 2 WIDGET-ID 84
     SPACE(2.39) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Export Customer to Excel" WIDGET-ID 100.


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
/* SETTINGS FOR DIALOG-BOX rd-fgnq-exp
   FRAME-NAME                                                           */
ASSIGN 
       FRAME rd-fgnq-exp:SCROLLABLE       = FALSE
       FRAME rd-fgnq-exp:HIDDEN           = TRUE.

ASSIGN 
       begin_chk:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       begin_cust:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       begin_inv:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       end_chk:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       end_cust:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       end_inv:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME rd-fgnq-exp
   NO-ENABLE                                                            */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_open IN FRAME rd-fgnq-exp
   ALIGN-R                                                              */
ASSIGN 
       tb_open:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME rd-fgnq-exp
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME rd-fgnq-exp     = 
                "parm".

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME rd-fgnq-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON HELP OF FRAME rd-fgnq-exp /* Export Customer to Excel */
DO:
DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
DEF VAR ls-cur-val AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.

   lw-focus = FOCUS.

   case lw-focus:name :
      /* when "begin_i-no" then do:
           ls-cur-val = lw-focus:screen-value.
           RUN windows/l-itemfj.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg */
       when "end_i-no" then do:
           ls-cur-val = lw-focus:screen-value.
           run windows/l-itemfj.w (cocode, ls-cur-val, output char-val).
           if char-val <> "" then do:
              lw-focus:screen-value =  ENTRY(1,char-val).
           end.
           return no-apply.
       end.  /* itemfg*/*/

END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-fgnq-exp rd-fgnq-exp
ON WINDOW-CLOSE OF FRAME rd-fgnq-exp /* Export Customer to Excel */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_chk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_chk rd-fgnq-exp
ON LEAVE OF begin_chk IN FRAME rd-fgnq-exp /* From Check# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust rd-fgnq-exp
ON LEAVE OF begin_cust IN FRAME rd-fgnq-exp /* From Customer# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_inv rd-fgnq-exp
ON LEAVE OF begin_inv IN FRAME rd-fgnq-exp /* From Invoice# */
DO:
   assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel rd-fgnq-exp
ON CHOOSE OF btn-cancel IN FRAME rd-fgnq-exp /* Cancel */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok rd-fgnq-exp
ON CHOOSE OF btn-ok IN FRAME rd-fgnq-exp /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.
  RUN GetSelectionList.  
  run run-report.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add rd-fgnq-exp
ON CHOOSE OF Btn_Add IN FRAME rd-fgnq-exp /* Add >> */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def rd-fgnq-exp
ON CHOOSE OF Btn_Def IN FRAME rd-fgnq-exp /* Default */
DO:
  DEF VAR cSelectedList AS cha NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down rd-fgnq-exp
ON CHOOSE OF btn_down IN FRAME rd-fgnq-exp /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove rd-fgnq-exp
ON CHOOSE OF Btn_Remove IN FRAME rd-fgnq-exp /* << Remove */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up rd-fgnq-exp
ON CHOOSE OF btn_Up IN FRAME rd-fgnq-exp /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_chk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_chk rd-fgnq-exp
ON LEAVE OF end_chk IN FRAME rd-fgnq-exp /* To Check# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust rd-fgnq-exp
ON LEAVE OF end_cust IN FRAME rd-fgnq-exp /* To Customer# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_inv rd-fgnq-exp
ON LEAVE OF end_inv IN FRAME rd-fgnq-exp /* To Invoice# */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file rd-fgnq-exp
ON LEAVE OF fi_file IN FRAME rd-fgnq-exp /* If Yes, File Name */
DO:
     assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl_avail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_avail rd-fgnq-exp
ON DEFAULT-ACTION OF sl_avail IN FRAME rd-fgnq-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected rd-fgnq-exp
ON DEFAULT-ACTION OF sl_selected IN FRAME rd-fgnq-exp
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel rd-fgnq-exp
ON VALUE-CHANGED OF tb_excel IN FRAME rd-fgnq-exp /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_open rd-fgnq-exp
ON VALUE-CHANGED OF tb_open IN FRAME rd-fgnq-exp /* Open Invoices Only? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel rd-fgnq-exp
ON VALUE-CHANGED OF tb_runExcel IN FRAME rd-fgnq-exp /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK rd-fgnq-exp 


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

   end_chk:SCREEN-VALUE = "2147483647" .  /* Default value as given in AQ2 */

    APPLY "entry" TO begin_cust.
  END. 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-tempfile rd-fgnq-exp 
PROCEDURE create-tempfile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR xsum AS DEC NO-UNDO.
  def var v-open       as   log format "Y/N" init "N".
  def var t-check-no  as   char   format "x(8)".
  def var x-check-no  as   char   format "x(10)".
  def var t-credits   as   dec    format ">,>>>,>>>.99"  column-label "Credits".
  def var t-debits    as   dec    format ">,>>>,>>>.99"  column-label "Debits".
  def var t-balance   as   dec    format ">>,>>>,>>>.99" column-label "Balance".
  def var v-tot-due   as   dec    format "->,>>>,>>9.99".
  def var v-pay-stat1 as   log    format "Y/N".
  def var v-pay-stat2 as   char   format "x(4)".
  DEF VAR li-seq AS INT NO-UNDO.
  DEF VAR lv-cust-no LIKE ar-cashl.cust-no NO-UNDO.
  DEF VAR ll-valid AS LOG NO-UNDO.
  DEF VAR li-fchk AS INTEGER NO-UNDO.
  DEF VAR li-tchk AS INTEGER NO-UNDO. 

  def var num-day-old  as dec format ">>>9" init 9999.
  DEF VAR vp-custno AS CHAR INIT "" NO-UNDO.
  DEFINE VARIABLE fi_fchk AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0  NO-UNDO.
  DEFINE VARIABLE fi_tchk AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 2147483647  NO-UNDO .

  SESSION:SET-WAIT-STATE ("general").

  FOR EACH tt-arinq:
    DELETE tt-arinq.
  END.

   ASSIGN
   v-open  = tb_open
   li-seq  = 0
   xsum    = 0
   li-fchk = INT(begin_chk)
   li-tchk = INT(begin_chk)
   fi_fchk = li-fchk 
   fi_tchk = li-tchk    .

  IF NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ YES
                    AND ar-cash.check-no LT li-fchk) AND
     NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ NO
                    AND ar-cash.check-no LT li-fchk) THEN li-fchk = 0.

  IF NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ YES
                    AND ar-cash.check-no GT li-fchk) AND
     NOT CAN-FIND(FIRST ar-cash
                  WHERE ar-cash.company  EQ cocode
                    AND ar-cash.memo     EQ NO
                    AND ar-cash.check-no GT li-fchk) THEN li-fchk = 2147483647.

  for each ar-inv no-lock
      where ar-inv.company  eq cocode
        and ar-inv.cust-no  GE begin_cust /*or fi_cust eq "")*/
        and ar-inv.cust-no  LE end_cust 
        and ar-inv.inv-no   ge INT(begin_inv)
        and ar-inv.inv-no   le INT(end_inv)
        and ar-inv.posted   eq yes
        and ar-inv.terms    ne "CASH"
        and ar-inv.inv-date ge (today - num-day-old)      
      USE-INDEX ar-inv
      BY ar-inv.inv-date
      BY ar-inv.inv-no:
     FIND FIRST cust
            WHERE cust.company EQ cocode
              AND cust.cust-no EQ ar-inv.cust-no 
            NO-LOCK NO-ERROR.
    
    if v-format eq "ASI" then do:
      {ar/ar-iactxl.i 1}
    end.    
    else do:
      {ar/ar-iactxl.i 2}
    end.
  end. /* for each ar-inv record */
  
  /* display unapplied cr/db memos/payments */
  lv-cust-no = begin_cust.

  DO WHILE TRUE:
    FOR EACH ar-cashl NO-LOCK
        WHERE ar-cashl.company EQ cocode
          AND ar-cashl.posted  EQ YES
          AND ar-cashl.cust-no GE begin_cust
          and ar-cashl.cust-no  LE end_cust /*or fi_cust eq "")*/
          AND ar-cashl.inv-no  EQ 0,      
        FIRST ar-cash NO-LOCK
        WHERE ar-cash.c-no     EQ ar-cashl.c-no
          AND decimal(ar-cash.check-no) GE DECIMAL(begin_chk)
          AND decimal(ar-cash.check-no) LE decimal(end_chk)
        BY ar-cash.check-date
        BY ar-cash.c-no:
       
      DO WITH FRAME {&frame-name}:
        FIND cust
            WHERE cust.company EQ cocode
              AND cust.cust-no EQ ar-cashl.cust-no
            NO-LOCK NO-ERROR.
      END.
      
      IF v-format eq "ASI" THEN DO:
        {ar/ar-iact2.i 1}
      END.    
      ELSE DO:
        {ar/ar-iact2.i 2}
      END.  
    END. /* for each ar-cash record */

    RELEASE ar-cashl.

    IF begin_cust EQ "" THEN
    FIND FIRST ar-cashl
        WHERE ar-cashl.company EQ cocode
          AND ar-cashl.posted  EQ YES
          AND ar-cashl.company GT lv-cust-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ar-cashl THEN LEAVE.

    lv-cust-no = ar-cashl.cust-no.
  END.
  /*APPLY 'VALUE-CHANGED':U TO FRAME {&FRAME-NAME}.*/

  SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI rd-fgnq-exp  _DEFAULT-DISABLE
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
  HIDE FRAME rd-fgnq-exp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionDefault rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList2 rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI rd-fgnq-exp  _DEFAULT-ENABLE
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
  DISPLAY begin_cust end_cust begin_chk end_chk begin_inv end_inv tb_open 
          sl_avail sl_selected tb_excel tb_runExcel fi_file 
      WITH FRAME rd-fgnq-exp.
  ENABLE RECT-6 RECT-7 RECT-8 begin_cust end_cust begin_chk end_chk begin_inv 
         end_inv tb_open sl_avail sl_selected Btn_Def Btn_Add Btn_Remove btn_Up 
         btn_down tb_runExcel fi_file btn-ok btn-cancel 
      WITH FRAME rd-fgnq-exp.
  VIEW FRAME rd-fgnq-exp.
  {&OPEN-BROWSERS-IN-QUERY-rd-fgnq-exp}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectionList rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move-Field rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report rd-fgnq-exp 
PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.
DEF BUFFER b-cust FOR cust.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR op-qty-pal AS INT NO-UNDO.

v-excelheader = buildHeader().
SESSION:SET-WAIT-STATE ("general").

IF tb_excel THEN OUTPUT STREAM excel TO VALUE(fi_file).
IF v-excelheader NE "" THEN PUT STREAM excel UNFORMATTED v-excelheader SKIP.

v-excel-detail-lines = "".
op-qty-pal = 0 .
li-pallets = 0 .

RUN create-tempfile.

    FOR EACH tt-arinq NO-LOCK
        BY tt-arinq.seq :

        v-excel-detail-lines = "".

       FOR EACH ttRptSelected:
         /* IF LOOKUP(ttRptSelected.TextList, "Contact,Title,Email") <> 0    THEN do: */
              IF ttRptSelected.TextList = "Ck/Cr/Dr/Po#" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.ref-num)).
              IF ttRptSelected.TextList = "Invoice#" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.inv-no)).
              IF ttRptSelected.TextList = "Date" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.tr-date)).
              IF ttRptSelected.TextList = "Description" THEN do:
                  v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.tr-dscr)).
              END.
              IF ttRptSelected.TextList = "Age App" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.ageapp)).
              IF ttRptSelected.TextList = "Debits" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.tr-damt)).
              IF ttRptSelected.TextList = "Credits" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.tr-camt)).
               IF ttRptSelected.TextList = "Balance" THEN
              v-excel-detail-lines = v-excel-detail-lines + appendXLLine(string(tt-arinq.balance)).
              
              
              
        /*  END.

          ELSE do:
              v-excel-detail-lines = v-excel-detail-lines + 
                  appendXLLine(getValue-itemfg(BUFFER b-cust,ttRptSelected.FieldList)).
          END.  */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Sort-Data rd-fgnq-exp 
PROCEDURE Set-Sort-Data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

      /* If a customer number was entered, find first and last matching customers. */
 /*   IF begin_i-no:SCREEN-VALUE EQ "" THEN DO:
        FIND FIRST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
        begin_i-no:SCREEN-VALUE = fg-rcpth.i-no.
        FIND LAST fg-rcpth WHERE fg-rcpth.company EQ cocode NO-LOCK NO-ERROR.
        end_i-no:SCREEN-VALUE   = fg-rcpth.i-no .
    END. */
  /*  IF begin_title-cod:SCREEN-VALUE EQ "" THEN DO:
        FIND FIRST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
        begin_title-cod:SCREEN-VALUE = phone.titlcode .
        FIND LAST phone WHERE phone.company EQ cocode NO-LOCK NO-ERROR.
        end_title-cod:SCREEN-VALUE   = phone.titlcode .
    END. */

  /*  IF lcSearch NE "" THEN 
        begin_cust-type:SCREEN-VALUE = lcSearch.
    IF lcsearchby NE "" THEN 
        end_cust-type:SCREEN-VALUE = lcsearchby.*/

END.

RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION appendXLLine rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION assignParam rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildHeader rd-fgnq-exp 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue-itemfg rd-fgnq-exp 
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

