&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ce-ctrl.w.w

  Description: Cost Estimating Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

/* Local Variable Definitions ---  
                                     */

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

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

{sys/inc/custlistform.i ""IR3"" }

{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL     NO-UNDO.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
    
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "CUSTOMER,ITEM,DESCRIPTION,PROD,UOM,AVERAGE COST,SELLING PRICE," +
                           "ON HAND,ON ORDER,CUST ORDERS,QTY AVAIL,TOTAL COST,TOTAL VALUE,STAT"
       cFieldListToSelect = "cust,i-no,dscr,prod,uom,avrg-cst,sel-prc," +
                            "oh-qty,ord-qty,cust-ord,qty-avl,ttl-cst,ttl-val,stat"
       cFieldLength = "8,15,15,5,3,14,13," + "11,11,11,11,15,15,4"  
       cFieldType = "c,c,c,c,c,i,i," + "i,i,i,i,i,i,c" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "ITEM,DESCRIPTION,PROD,UOM,AVERAGE COST,SELLING PRICE," +
                           "ON HAND,ON ORDER,CUST ORDERS,QTY AVAIL,TOTAL COST,TOTAL VALUE,STAT" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 tb_cust-list btnCustList ~
begin_i-no end_i-no begin_cust-no end_cust-no begin_cat end_cat rd_pg-brk ~
tb_cust-whse tb_sort tb_zero rd_active sl_avail Btn_Def sl_selected Btn_Add ~
Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_cust-list begin_i-no end_i-no ~
begin_cust-no end_cust-no begin_cat end_cat lbl_pg-brk rd_pg-brk ~
tb_cust-whse tb_sort tb_zero rd_active sl_avail sl_selected rd-dest lv-ornt ~
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

DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" 
     LABEL "Beginning Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" 
     LABEL "Ending Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-alphls.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_pg-brk AS CHARACTER FORMAT "X(256)":U INITIAL "Page Break?" 
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

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_active AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Active", "A",
"Inactive", "I",
"All", "All"
     SIZE 35 BY .95 NO-UNDO.

DEFINE VARIABLE rd_pg-brk AS CHARACTER INITIAL "None" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "None", "None",
"Customer#", "Customer#",
"Product Category", "Product Category"
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 10.71.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_cust-list AS LOGICAL INITIAL no 
     LABEL "Use Defined Customer List" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .95 NO-UNDO.

DEFINE VARIABLE tb_cust-whse AS LOGICAL INITIAL no 
     LABEL "Include Customer Owned Warehouse?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .95
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sort AS LOGICAL INITIAL no 
     LABEL "Sort By Item#?" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE tb_zero AS LOGICAL INITIAL yes 
     LABEL "Show Only Items with Quantity Greater than Zero?" 
     VIEW-AS TOGGLE-BOX
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL yes 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     tb_cust-list AT ROW 1.91 COL 29.2 WIDGET-ID 6
     btnCustList AT ROW 1.95 COL 61.2 WIDGET-ID 8
     begin_i-no AT ROW 2.95 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Order Number"
     end_i-no AT ROW 2.95 COL 66 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_cust-no AT ROW 3.91 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Customer Number"
     end_cust-no AT ROW 3.91 COL 66 COLON-ALIGNED HELP
          "Enter Ending Customer Number"
     begin_cat AT ROW 4.86 COL 23 COLON-ALIGNED HELP
          "Enter Beginning Category"
     end_cat AT ROW 4.86 COL 66 COLON-ALIGNED HELP
          "Enter Ending Order Number"
     lbl_pg-brk AT ROW 6 COL 26 COLON-ALIGNED NO-LABEL
     rd_pg-brk AT ROW 6 COL 42 NO-LABEL
     tb_cust-whse AT ROW 7.19 COL 42
     tb_sort AT ROW 8.14 COL 42
     tb_zero AT ROW 9.1 COL 42
     rd_active AT ROW 10.38 COL 44 NO-LABEL WIDGET-ID 2
     sl_avail AT ROW 12.48 COL 4 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 12.48 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 12.48 COL 59.4 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 13.48 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 14.48 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 15.52 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 16.52 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 18.67 COL 5 NO-LABEL
     lv-ornt AT ROW 18.67 COL 29 NO-LABEL
     lines-per-page AT ROW 18.67 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 20.1 COL 28 COLON-ALIGNED
     lv-font-name AT ROW 21.29 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.48 COL 27
     tb_excel AT ROW 23.43 COL 69 RIGHT-ALIGNED
     tb_runExcel AT ROW 23.43 COL 91 RIGHT-ALIGNED
     fi_file AT ROW 24.38 COL 47 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 25.81 COL 18
     btn-cancel AT ROW 25.81 COL 56
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 11.76 COL 4.8 WIDGET-ID 38
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 17.95 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 11.71 COL 59 WIDGET-ID 44
     RECT-6 AT ROW 17.71 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 26.19.


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
         TITLE              = "Finished Goods By Description Report"
         HEIGHT             = 26.43
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
       begin_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_cust-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_pg-brk IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_pg-brk:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_pg-brk".

ASSIGN 
       rd_pg-brk:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-list:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_cust-whse:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zero:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Finished Goods By Description Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Finished Goods By Description Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cat C-Win
ON LEAVE OF begin_cat IN FRAME FRAME-A /* Beginning Category */
DO:
  assign {&self-name}.
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
                    INPUT begin_cust-no,
                    INPUT end_cust-no).
  END. 
  run run-report. 

   STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=END_cust-no
                            &fax-subject=c-win:TITLE 
                            &fax-body=c-win:TITLE 
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust-no
                             &END_cust=end_cust-no
                             &mail-subject=c-win:TITLE 
                             &mail-body=c-win:title 
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust-no
                                  &END_cust=end_cust-no
                                  &mail-subject=c-win:TITLE 
                                  &mail-body=c-win:TITLE 
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
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


&Scoped-define SELF-NAME end_cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cat C-Win
ON LEAVE OF end_cat IN FRAME FRAME-A /* Ending Category */
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     assign {&self-name}.
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


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME FRAME-A /* Font */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    RUN WINDOWS/l-cust.w (cocode, {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
    IF char-val <> "" THEN ASSIGN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val)
                                  .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust-no C-Win
ON HELP OF end_cust-no IN FRAME FRAME-A /* Font */
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
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active C-Win
ON VALUE-CHANGED OF rd_active IN FRAME FRAME-A
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_pg-brk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_pg-brk C-Win
ON VALUE-CHANGED OF rd_pg-brk IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_cust-whse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_cust-whse C-Win
ON VALUE-CHANGED OF tb_cust-whse IN FRAME FRAME-A /* Include Customer Owned Warehouse? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sort C-Win
ON VALUE-CHANGED OF tb_sort IN FRAME FRAME-A /* Sort By Item#? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_zero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero C-Win
ON VALUE-CHANGED OF tb_zero IN FRAME FRAME-A /* Show Only Items with Quantity Greater than Zero? */
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

  RUN DisplaySelectionList.
  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_i-no.
  END.

  RUN sys/ref/CustList.p (INPUT cocode,
                          INPUT 'IR3',
                          INPUT NO,
                          OUTPUT glCustListActive).
  {sys/inc/chblankcust.i ""IR3""}

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
                            INPUT 'IR3',
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
                                  INPUT 'IR3').
    

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
  DISPLAY tb_cust-list begin_i-no end_i-no begin_cust-no end_cust-no begin_cat 
          end_cat lbl_pg-brk rd_pg-brk tb_cust-whse tb_sort tb_zero rd_active 
          sl_avail sl_selected rd-dest lv-ornt lines-per-page lv-font-no 
          lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 tb_cust-list btnCustList begin_i-no end_i-no 
         begin_cust-no end_cust-no begin_cat end_cat rd_pg-brk tb_cust-whse 
         tb_sort tb_zero rd_active sl_avail Btn_Def sl_selected Btn_Add 
         Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no 
         lv-font-name td-show-parm tb_excel tb_runExcel fi_file btn-ok 
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
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.
 */
 {CUstom/out2file.i}

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ fg/rep/fg-aitem.p 9/91 cd */
/* finished goods - alphabetic item report.                                   */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

def var v-i-no     like itemfg.i-no     extent 2  init ["","zzzzzzzzzzzzzzz"].
def var v-cust     like itemfg.cust-no  extent 2  init ["","zzzzzzzz"].
def var v-procat   like itemfg.procat   extent 2  init ["","zzzzz"].
def var v-break    as   char format "!" init "N".
def var v-prt-cost as   log format "Cost/Value" init no.
def var v-custown  as   log format "Y/N" init no.
def var v-sort-by  as   log format "Y/N" init no.
def var v-zero     as   log format "Y/N" init no.
def var v-sho-cost as   log format "Y/N" init no.

def var v-first       as log extent 2 init yes.
def var v-page-break  as char.
def var v-label1      as char format "x(14)" extent 3.
def var v-label2      as char format "x(14)".
def var v-price       as dec.
def var v-price2      as dec.
def var v-cost        as dec.
def var v-tq-onh      as dec extent 2.
def var v-tq-ono      like v-tq-onh.
def var v-tq-alloc    like v-tq-onh.
def var v-tq-avail    like v-tq-onh.
def var v-tprice      like v-tq-onh.
def var v-tprice2      like v-tq-onh.
def var v-qty-onh     like itemfg.q-onh.
DEF VAR v-status      AS CHAR NO-UNDO FORMAT "x(1)".
DEF VAR v-qty-aval    AS INT NO-UNDO.

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

{sys/form/r-top5L3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR lSelected AS LOG INIT YES NO-UNDO.

form header skip(1)
            v-page-break format "x(132)"
            
    with frame r-top-2 STREAM-IO width 140 no-labels no-box no-underline page-top.

form header skip(1)
            "               "
            "             "
            "PROD "
            "   "
            v-label1[1]
            "     SELLING"
            "           "
            "           "
            "   CUSTOMER"
            "           "
            "         TOTAL"
            skip
           
            "ITEM           "
            "DESCRIPTION  "
            "CAT  "
            "UOM"
            v-label1[2]
            "       PRICE"
            "    ON HAND"
            "   ON ORDER"
            "     ORDERS"
            "  QTY AVAIL"
            v-label2
            "STAT"
            skip
            
            "---------------"
            "-------------"
            "-----"
            "---"
            v-label1[3]
            "------------"
            "-----------"
            "-----------"
            "-----------"
            "-----------"
            "--------------"
            "-----"
          skip
    with frame r-top-3 STREAM-IO width 140 no-labels no-box no-underline page-top.
    
form
    itemfg.i-no                                 
    itemfg.i-name           format "x(13)"          
    itemfg.procat                               
    itemfg.sell-uom 
    itemfg.total-std-cost   format "->>,>>>,>>9.99" 
    itemfg.sell-price       format ">,>>>,>>9.99"
    v-qty-onh               format "->>,>>>,>>9"    
    itemfg.q-ono            format "->>,>>>,>>9"    
    itemfg.q-alloc          format "->>,>>>,>>9"    
    itemfg.q-avail          format "->>,>>>,>>9"    
    v-price                 format "->>,>>>,>>9.99" 
    v-status                FORMAT "x(1)"
    SKIP  
   with frame itemx no-labels no-box down STREAM-IO WIDTH 140.

ASSIGN
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}

 v-i-no[1]    = begin_i-no
 v-i-no[2]    = end_i-no
 v-cust[1]    = begin_cust-no
 v-cust[2]    = end_cust-no
 v-procat[1]  = begin_cat
 v-procat[2]  = END_cat
 v-break      = SUBSTR(rd_pg-brk,1,1)
 /*v-sho-cost   = tb_cost*/
 /*v-prt-cost   = rd_ext-cst EQ "Cost"*/
 v-custown    = tb_cust-whse
 v-sort-by    = tb_sort
 v-zero       = tb_zero
 lSelected    = tb_cust-list .


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

        IF LOOKUP(ttRptSelected.TextList, "ON HAND,ON ORDER,CUST ORDERS,QTY AVAIL,TOTAL COST,TOTAL VALUE") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").

  find first fg-ctrl where fg-ctrl.company eq cocode no-lock.
  
  assign
   v-label1[1] = if fg-ctrl.inv-meth eq "A" then
                   "       AVERAGE" else "          LAST"
   v-label1[2] = "          COST"
   v-label1[3] = "--------------"
   v-label2    = if v-prt-cost then
                   "          COST" else "         VALUE".

 /* if not v-sho-cost then v-label1 = "".*/
  v-qty-aval = 0 .

  IF lselected THEN DO:
      FIND FIRST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no  NO-LOCK NO-ERROR  .
      IF AVAIL ttCustList THEN ASSIGN v-cust[1] = ttCustList.cust-no .
      FIND LAST ttCustList WHERE ttCustList.log-fld USE-INDEX cust-no NO-LOCK NO-ERROR .
      IF AVAIL ttCustList THEN ASSIGN v-cust[2] = ttCustList.cust-no .
  END.
  
  FOR each itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    ge v-i-no[1]
        and itemfg.i-no    le v-i-no[2]
        and itemfg.cust-no GE v-cust[1]
        and itemfg.cust-no le v-cust[2]
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq itemfg.cust-no
        AND ttCustList.log-fld no-lock) else true)
        and itemfg.procat  ge v-procat[1]
        and itemfg.procat  le v-procat[2]
      no-lock
      
      break by (if v-break eq "C" then itemfg.cust-no else
                if v-break eq "P" then itemfg.procat  else "1")
            by (if v-sort-by then itemfg.i-no else itemfg.i-name)
            
      with frame itemx.

/*       /* Check active status. */                   */
/*      FIND FIRST reftable NO-LOCK WHERE             */
/*          reftable.reftable = "FGSTATUS" AND        */
/*          reftable.company  = itemfg.company AND    */
/*          reftable.loc      = "" AND                */
/*          reftable.code     = itemfg.i-no NO-ERROR. */

/*      /* If no status record found and user selected Active or Inactive, */
/*         then skip.  (If all selected, then print it) */                 */
/*      IF NOT AVAILABLE reftable AND rd_active <> "All" THEN NEXT.        */
/*                                                                         */
     /* If user selected active or inactive and does not match selected
        status then skip. */
    IF rd_active <> "All" AND itemfg.stat NE rd_active THEN NEXT.
    v-status = itemfg.stat.
/*     IF rd_active <> "All" AND AVAILABLE reftable AND reftable.code2 <> rd_active THEN NEXT. */
/*     IF AVAIL reftable THEN                */
/*         ASSIGN v-status = reftable.code2. */
/*     ELSE                                  */
/*         ASSIGN v-status = "".             */
    

    {custom/statusMsg.i "'Processing Item # ' + itemfg.i-no"}
      
    if first-of(if v-break eq "C" then itemfg.cust-no else
                if v-break eq "P" then itemfg.procat  else "1") then
      v-first[2] = yes.
       
    v-qty-onh  = 0.
     
    for each fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq itemfg.i-no
        use-index i-no no-lock:
        
      if v-custown or (fg-bin.loc ne "CUST" and fg-bin.cust-no eq "") then
        v-qty-onh = v-qty-onh + fg-bin.qty.
    end. /* each bin */

    if v-qty-onh ne 0 or not v-zero then do:
      if v-first[2] then do:
        assign
         v-first[2]   = no
         v-page-break = if v-break eq "C" then
                          ("Customer: " + trim(itemfg.cust-no))
                        else
                        if v-break eq "P" then
                          ("Product Category: " + trim(itemfg.procat))
                        else "".  
        
        IF v-first[1] THEN DO:
          v-first[1] = NO.
          DISPLAY WITH frame r-top.
         /* IF v-break NE "N" THEN DISPLAY WITH FRAME r-top-2.
          DISPLAY WITH frame r-top-3.*/
        END.

        ELSE PAGE.
      end.
    
     /* if v-prt-cost then do:*/
        v-cost = itemfg.total-std-cost.
      
        if itemfg.prod-uom ne "EA" then
          run sys/ref/convcuom.p (itemfg.prod-uom, "EA", 0, 0, 0, 0,
                                  v-cost, output v-cost).
                                
        v-price2 = v-cost * v-qty-onh.  
     /* end.
    
      else do:*/
        find first uom
            where uom.uom  eq itemfg.sell-uom
              and uom.mult ne 0
            no-lock no-error.
        v-price = v-qty-onh * itemfg.sell-price /
                  (if avail uom then uom.mult else 1000).
                 
        if itemfg.sell-uom eq "CS" then
          v-price = v-qty-onh / itemfg.case-count * itemfg.sell-price.
        
        else
        /* gdm - 04130901 */
        IF itemfg.sell-uom EQ "L" AND v-qty-onh > 0
          THEN v-price =  itemfg.sell-price.
        IF itemfg.sell-uom EQ "L" AND v-qty-onh  < 1
          THEN v-price = (v-qty-onh * itemfg.sell-price).

     /* end.*/
      
      if v-price eq ? then v-price = 0.
      if v-price2 eq ? then v-price2 = 0.
              
     /* display itemfg.i-no
              itemfg.i-name
              itemfg.procat
              itemfg.sell-uom
              itemfg.prod-uom when v-prt-cost @ itemfg.sell-uom
              itemfg.total-std-cost when v-sho-cost
              itemfg.sell-price
              v-qty-onh
              itemfg.q-ono
              itemfg.q-alloc
              (v-qty-onh + itemfg.q-ono - itemfg.q-alloc) @ itemfg.q-avail
              v-price
              v-status
          with frame itemx.
      down with frame itemx.

      IF tb_excel THEN 
        PUT STREAM excel UNFORMATTED
            '"' itemfg.i-no                                               '",'
            '"' itemfg.i-name                                             '",'
            '"' itemfg.procat                                             '",'
            '"' (IF v-prt-cost THEN itemfg.prod-uom ELSE itemfg.sell-uom) '",'
            '"' (IF v-sho-cost THEN itemfg.total-std-cost ELSE 0)         '",'
            '"' itemfg.sell-price                                         '",'
            '"' v-qty-onh                                                 '",'
            '"' itemfg.q-ono                                              '",'
            '"' itemfg.q-alloc                                            '",'
            '"' (v-qty-onh + itemfg.q-ono - itemfg.q-alloc)               '",'
            '"' v-price                                                   '",'
            '"' v-status                                            '",'
            SKIP. */

      ASSIGN v-qty-aval = (v-qty-onh + itemfg.q-ono - itemfg.q-alloc) .

      ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".
      
      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
              CASE cTmpField:             
                   WHEN "i-no"       THEN cVarValue = STRING(itemfg.i-no,"x(15)") .  
                   WHEN "dscr"       THEN cVarValue = STRING(itemfg.i-nam,"x(15)")   .   
                   WHEN "prod"       THEN cVarValue = string(itemfg.procat)   .                
                   WHEN "uom"        THEN cVarValue = IF itemfg.prod-uom NE "" THEN string(itemfg.prod-uom) ELSE string(itemfg.sell-uom).
                   WHEN "avrg-cst"   THEN cVarValue = string(itemfg.total-std-cost,"->>,>>>,>>9.99") .              
                   WHEN "sel-prc"    THEN cVarValue = string(itemfg.sell-price,">,>>>,>>9.99")  .                           
                   WHEN "oh-qty"     THEN cVarValue = STRING(v-qty-onh,"->>,>>>,>>9").                                          
                   WHEN "ord-qty"    THEN cVarValue = STRING(itemfg.q-ono,"->>,>>>,>>9").             
                   WHEN "cust-ord"   THEN cVarValue = STRING(itemfg.q-alloc,"->>,>>>,>>9")   .         
                   WHEN "qty-avl"    THEN cVarValue = STRING(v-qty-aval,"->>,>>>,>>9") .
                   WHEN "ttl-cst"    THEN cVarValue = STRING(v-price2,"->>>,>>>,>>9.99").     
                   WHEN "ttl-val"    THEN cVarValue = STRING(v-price,"->>>,>>>,>>9.99").
                   WHEN "stat"       THEN cVarValue = STRING(v-status)   . 
                   WHEN "cust"       THEN cVarValue = STRING(itemfg.cust-no) .
                   
              END CASE.
                
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
      END.
      
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
           PUT STREAM excel UNFORMATTED  
                 cExcelDisplay SKIP.
       END.

    
      assign
       v-tq-onh[1]   = v-tq-onh[1]   + v-qty-onh
       v-tq-ono[1]   = v-tq-ono[1]   + itemfg.q-ono
       v-tq-alloc[1] = v-tq-alloc[1] + itemfg.q-alloc
       v-tq-avail[1] = v-tq-avail[1] + v-qty-aval
       v-tprice[1]   = v-tprice[1]   + v-price
       v-tprice2[1]   = v-tprice2[1]   + v-price2 .
    end.
     
    if not v-first[2]                                          and
       last-of(if v-break eq "C" then itemfg.cust-no else
               if v-break eq "P" then itemfg.procat  else "1") then do:
               
      if v-break ne "N" then do:
        put skip(1).
        
        /*display "Cust Total"                          @ itemfg.i-name
                  "ProdCat Total" when v-break eq "P" @ itemfg.i-name
                v-tq-onh[1]                           @ v-qty-onh
                v-tq-ono[1]                           @ itemfg.q-ono
                v-tq-alloc[1]                         @ itemfg.q-alloc
                v-tq-avail[1]                         @ itemfg.q-avail
                v-tprice[1]                           @ v-price
            with frame itemx.
        down with frame itemx. */
        PUT SKIP str-line SKIP .
        ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".
      
      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
              CASE cTmpField:             
                   WHEN "i-no"       THEN cVarValue = "" .  
                   WHEN "dscr"       THEN cVarValue = ""   .   
                   WHEN "prod"       THEN cVarValue = ""   .                
                   WHEN "uom"        THEN cVarValue = "" .
                   WHEN "avrg-cst"   THEN cVarValue = "" .              
                   WHEN "sel-prc"    THEN cVarValue = ""  .                           
                   WHEN "oh-qty"     THEN cVarValue = STRING(v-tq-onh[1],"->>,>>>,>>9").                                          
                   WHEN "ord-qty"    THEN cVarValue = STRING(v-tq-ono[1],"->>,>>>,>>9").             
                   WHEN "cust-ord"   THEN cVarValue = STRING(v-tq-alloc[1],"->>,>>>,>>9")  .         
                   WHEN "qty-avl"    THEN cVarValue = STRING(v-tq-avail[1],"->>,>>>,>>9") .
                   WHEN "ttl-cst"    THEN cVarValue = STRING(v-tprice2[1],"->>>,>>>,>>9.99").     
                   WHEN "ttl-val"    THEN cVarValue = STRING(v-tprice[1],"->>>,>>>,>>9.99").
                   WHEN "stat"       THEN cVarValue = ""   .
                   WHEN "cust"       THEN cVarValue = "" .
                   
              END CASE.
                
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
      END.
      
      IF v-break eq "P" THEN
      PUT UNFORMATTED "   ProdCat Total" substring(cDisplay,17,300) SKIP.
      ELSE 
          PUT UNFORMATTED "   Cust Total" substring(cDisplay,14,300) SKIP.
        
        put skip(1).
      end.
     
      assign
       v-tq-onh[2]   = v-tq-onh[2]   + v-tq-onh[1]
       v-tq-ono[2]   = v-tq-ono[2]   + v-tq-ono[1]
       v-tq-alloc[2] = v-tq-alloc[2] + v-tq-alloc[1]
       v-tq-avail[2] = v-tq-avail[2] + v-tq-avail[1]
       v-tprice[2]   = v-tprice[2]   + v-tprice[1]
       v-tprice2[2]   = v-tprice2[2]   + v-tprice2[1]
       
       v-tq-onh[1]   = 0
       v-tq-ono[1]   = 0
       v-tq-alloc[1] = 0
       v-tq-avail[1] = 0
       v-tprice[1]   = 0
       v-tprice2[1]   = 0   .
    end.
    
    if not v-first[1]                                       and
       last(if v-break eq "C" then itemfg.cust-no else
            if v-break eq "P" then itemfg.procat  else "1") then do:
               
      hide frame r-top-2 no-pause.
  
      if v-break ne "N" then page.
      else put skip(2).

      /*display "Grand Total"                         @ itemfg.i-name
              v-tq-onh[2]                           @ v-qty-onh
              v-tq-ono[2]                           @ itemfg.q-ono
              v-tq-alloc[2]                         @ itemfg.q-alloc
              v-tq-avail[2]                         @ itemfg.q-avail
              v-tprice[2]                           @ v-price
          with frame itemx.
      down with frame itemx.*/
      PUT SKIP str-line SKIP .
      ASSIGN cDisplay = ""
             cTmpField = ""
             cVarValue = ""
             cExcelDisplay = ""
             cExcelVarValue = "".
      
      DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
              CASE cTmpField:             
                   WHEN "i-no"       THEN cVarValue = "" .  
                   WHEN "dscr"       THEN cVarValue = ""   .   
                   WHEN "prod"       THEN cVarValue = ""   .                
                   WHEN "uom"        THEN cVarValue = "" .
                   WHEN "avrg-cst"   THEN cVarValue = "" .              
                   WHEN "sel-prc"    THEN cVarValue = ""  .                           
                   WHEN "oh-qty"     THEN cVarValue = STRING(v-tq-onh[2],"->>,>>>,>>9").                                          
                   WHEN "ord-qty"    THEN cVarValue = STRING(v-tq-ono[2],"->>,>>>,>>9").             
                   WHEN "cust-ord"   THEN cVarValue = STRING(v-tq-alloc[2],"->>,>>>,>>9")  .         
                   WHEN "qty-avl"    THEN cVarValue = STRING(v-tq-avail[2],"->>,>>>,>>9") .
                   WHEN "ttl-cst"    THEN cVarValue = STRING(v-tprice2[2],"->>>,>>>,>>9.99").     
                   WHEN "ttl-val"    THEN cVarValue = STRING(v-tprice[2],"->>>,>>>,>>9.99").
                   WHEN "stat"       THEN cVarValue = ""   . 
                   WHEN "cust"       THEN cVarValue = "" .
                   
              END CASE.
                
              cExcelVarValue = cVarValue.
              cDisplay = cDisplay + cVarValue +
                         FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
              cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
      END.
      
      PUT UNFORMATTED "   Grand Total" substring(cDisplay,15,300) SKIP.

    end.      
  end. /* each itemfg */

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

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

