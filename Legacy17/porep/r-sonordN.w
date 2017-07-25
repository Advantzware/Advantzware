&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-sonord.w

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
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

  def TEMP-TABLE wk-sh-ord NO-UNDO
    field due-date like po-ordl.due-date
    FIELD machine  AS CHAR
    FIELD key1 AS CHAR
    field rec-id as recid.

DEF VAR ll-secure AS LOG NO-UNDO.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF STREAM excel.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR vWidth AS DEC NO-UNDO.
DEF VAR vLength AS DEC NO-UNDO.
DEF VAR vFGItem# AS cha NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.
/*  DATE  ORDER        QUANTITY           QUANTITY  ------ VENDOR ------              P/O FG ITEM         RM ITEM           MSF        COST
     DUE     NO         ORDERED UOM       RECEIVED  ------- NAME -------  MACHINE  NUMBER NUMBER          NUMBER         REMAIN      REMAIN
---
display wk-sh-ord.due-date
            oe-ord.ord-no when avail oe-ord ">>>>>9"
            ld-oqty @ po-ordl.ord-qty   ->>>,>>>,>>9.9<<<<<
            lv-uom  @ po-ordl.pr-qty-uom
            ld-rqty @ po-ordl.t-rec-qty
            v-prt-name
            wk-sh-ord.machine
            po-ordl.po-no >>>>>9
            job-hdr.i-no when avail job-hdr
            po-ordl.i-no
            v-msf-rem
            v-cst-rem when v-cost     
            po-ordl.s-wid >>,>>9.99<<<
            po-ordl.s-len

*/
ASSIGN cTextListToSelect = "Due Date,Order#,Customer Name,Vendor Name,Order Qty,Rcpt Qty," + 
                           "Cost Due,MSF Due,Machine#,P/O#,FG Item#,RM Item#,Width,Length,UOM,Job#,Job Due" 
       cFieldListToSelect = "wk-sh-ord.due-date,v-ord-no,v-cust-name,v-vend-name,ld-oqty,ld-rqty," +
                            "v-cst-rem,v-msf-rem,wk-sh-ord.machine,po-ordl.po-no,vFGItem#,po-ordl.i-no,v-wid,v-len,lv-uom,v-job-no,v-jobDueDate"
       cFieldLength = "8,6,30,30,15,15," + "10,10,8,6,15,15,10,10,4,9,10" 
       cFieldType   = "i,i,c,c,i,i," + "i,i,c,i,c,c,i,i,c,c,c"
       .

{sys/inc/ttRptSel.i}
 ASSIGN cTextListToDefault  = "Due Date,Order#,Order Qty,UOM,Rcpt Qty,Customer Name,Vendor Name," + 
                              "Machine#,P/O#,FG Item#,RM Item#,MSF Due,Cost Due" .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_vend-no end_vend-no ~
begin_due-date end_due-date rd_uom rd_sort tb_date tb_closed sl_avail ~
Btn_Add sl_selected Btn_Remove btn_Up btn_down rd-dest lv-ornt ~
lines-per-page lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok ~
btn-cancel Btn_Def
&Scoped-Define DISPLAYED-OBJECTS begin_vend-no end_vend-no begin_due-date ~
end_due-date lbl_uom rd_uom lbl_sort rd_sort tb_date tb_closed sl_avail ~
sl_selected rd-dest lv-ornt lines-per-page lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-vendor-name C-Win 
FUNCTION get-vendor-name RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE begin_vend-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Due Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend-no AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-sonord.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_show AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_uom AS CHARACTER FORMAT "X(256)":U INITIAL "Print Qtys in Which UOM?" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Customer Name" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Customer Name", "Customer Name",
"Vendor Name", "Vendor Name"
     SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "D" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Machine", "M",
"Due Date", "D",
"Vendor Name", "V"
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE rd_uom AS CHARACTER INITIAL "Purchased" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Purchased", "Purchased",
"Consumption", "Consumption",
"MSF", "MSF"
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.48.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 9.05.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 34 BY 5.48 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 35 BY 5.48 NO-UNDO.

DEFINE VARIABLE tb_closed AS LOGICAL INITIAL no 
     LABEL "Show Closed PO's (already received)" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE tb_date AS LOGICAL INITIAL no 
     LABEL "Display Subtotal?" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .86 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_sheets AS LOGICAL INITIAL no 
     LABEL "Print Cost Of Remaining Sheets?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_vend-no AT ROW 2.43 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend-no AT ROW 2.43 COL 69 COLON-ALIGNED HELP
          "Enter Ending Vendor number"
     begin_due-date AT ROW 3.38 COL 27 COLON-ALIGNED HELP
          "Enter Beginning Due Date"
     end_due-date AT ROW 3.38 COL 69 COLON-ALIGNED HELP
          "Enter ending Due Date"
     lbl_uom AT ROW 4.81 COL 11 COLON-ALIGNED NO-LABEL
     rd_uom AT ROW 4.81 COL 40 NO-LABEL
     tb_sheets AT ROW 5.52 COL 104
     lbl_sort AT ROW 6 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     rd_sort AT ROW 6 COL 40 NO-LABEL WIDGET-ID 2
     lbl_show AT ROW 7.19 COL 107 COLON-ALIGNED NO-LABEL
     tb_date AT ROW 7.43 COL 36
     rd_show AT ROW 8.14 COL 98 NO-LABEL
     tb_closed AT ROW 8.38 COL 36 WIDGET-ID 8
     sl_avail AT ROW 11.24 COL 2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 11.29 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 12.38 COL 39 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     sl_selected AT ROW 11.24 COL 58 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 13.48 COL 39 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 14.57 COL 39 WIDGET-ID 40
     btn_down AT ROW 15.67 COL 39 WIDGET-ID 42
     rd-dest AT ROW 18.62 COL 3 NO-LABEL
     lv-ornt AT ROW 19.1 COL 30 NO-LABEL
     lines-per-page AT ROW 19.1 COL 83 COLON-ALIGNED
     lv-font-no AT ROW 20.76 COL 34 COLON-ALIGNED
     lv-font-name AT ROW 21.71 COL 28 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 22.86 COL 30
     tb_excel AT ROW 24.57 COL 50 RIGHT-ALIGNED
     tb_runExcel AT ROW 24.57 COL 71 RIGHT-ALIGNED
     fi_file AT ROW 25.38 COL 28 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 27.24 COL 18
     btn-cancel AT ROW 27.24 COL 57
     "Available Columns" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 10.52 COL 2 WIDGET-ID 38
     " Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 17.67 COL 2
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 10.52 COL 58.6 WIDGET-ID 44
     RECT-6 AT ROW 17.24 COL 1
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.05
         SIZE 138.6 BY 28.14.


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
         TITLE              = "Sheets On Order Report"
         HEIGHT             = 28.19
         WIDTH              = 96.8
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
       begin_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_due-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       lbl_show:HIDDEN IN FRAME FRAME-A           = TRUE
       lbl_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lbl_uom IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_uom:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_uom".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd_show IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rd_show:HIDDEN IN FRAME FRAME-A           = TRUE
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_uom:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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

/* SETTINGS FOR TOGGLE-BOX tb_sheets IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_sheets:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_sheets:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sheets On Order Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sheets On Order Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_due-date C-Win
ON LEAVE OF begin_due-date IN FRAME FRAME-A /* Beginning Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_vend-no C-Win
ON LEAVE OF begin_vend-no IN FRAME FRAME-A /* Beginning Vendor# */
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

  RUN GetSelectionList.
  IF iColumnLength > 300 THEN
   MESSAGE "Report may not show all selected columns appropriately. " SKIP
           "Use Excel Output for all selected column values. (" iColumnLength ")"
       VIEW-AS ALERT-BOX WARNING BUTTONS OK.

  run run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend-no
                            &END_cust=end_vend-no
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend-no
                             &END_cust=end_vend-no
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "Vendor"
                                  &begin_cust= begin_vend-no
                                  &END_cust=end_vend-no
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
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


&Scoped-define SELF-NAME end_due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_due-date C-Win
ON LEAVE OF end_due-date IN FRAME FRAME-A /* Ending Due Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend-no C-Win
ON LEAVE OF end_vend-no IN FRAME FRAME-A /* Ending Vendor# */
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


&Scoped-define SELF-NAME rd_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show C-Win
ON VALUE-CHANGED OF rd_show IN FRAME FRAME-A
DO:
  assign {&self-name}.
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


&Scoped-define SELF-NAME rd_uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_uom C-Win
ON VALUE-CHANGED OF rd_uom IN FRAME FRAME-A
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


&Scoped-define SELF-NAME tb_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_date C-Win
ON VALUE-CHANGED OF tb_date IN FRAME FRAME-A /* Display Subtotal? */
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


&Scoped-define SELF-NAME tb_sheets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_sheets C-Win
ON VALUE-CHANGED OF tb_sheets IN FRAME FRAME-A /* Print Cost Of Remaining Sheets? */
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

    assign            
    begin_due-date =  TODAY
    end_due-date   =  date(12,31,year(today)).

  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {methods/setButton.i btn-cancel "Cancel"} /* added by script _nonAdm1Images2.p */
    {methods/setButton.i btn-ok "OK"} /* added by script _nonAdm1Images2.p */
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_vend-no.
  END.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  /* sl_avail:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListContents. */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplaySelectionList C-Win 
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
  DISPLAY begin_vend-no end_vend-no begin_due-date end_due-date lbl_uom rd_uom 
          lbl_sort rd_sort tb_date tb_closed sl_avail sl_selected rd-dest 
          lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel 
          tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_vend-no end_vend-no begin_due-date end_due-date 
         rd_uom rd_sort tb_date tb_closed sl_avail Btn_Add sl_selected 
         Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page lv-font-no 
         td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel Btn_Def
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
    IF AVAIL ttRptList THEN DO:
        CREATE ttRptSelected.
        ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
               ttRptSelected.FieldList = ttRptList.FieldList
               ttRptSelected.FieldLength = int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
               ttRptSelected.DisplayOrder = i
               ttRptSelected.HeadingFromLeft = IF entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldType) = "C" THEN YES ELSE NO
               iColumnLength = iColumnLength + ttRptSelected.FieldLength + 1.
    END.                          
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
  /*   DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.

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
  run scr-rpt.w (list-name,c-win:title,int(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ po/rep/sh-ord.p 8/96 fwk  */
/* Sheets On Order Report                                                     */
/* -------------------------------------------------------------------------- */
DEF VAR str-tit4 AS cha NO-UNDO.
DEF VAR str-tit5 AS cha NO-UNDO.
DEF VAR str-line AS cha FORM "x(300)" NO-UNDO.
{sys/form/r-top5DL3.f}
def var v-msf-rem as dec format ">,>>>,>>9.999" no-undo.
def var v-cst-rem as dec format ">>>,>>>,>>9" no-undo.
def var v-s-vend like vend.vend-no init "" no-undo.
def var v-e-vend like vend.vend-no init "zzzzzzzz" no-undo.
def var v-s-date like po-ord.po-date format "99/99/9999" init today no-undo.
def var v-e-date like po-ord.po-date format "99/99/9999" init today no-undo.
def var v-name as char format "x(1)" init "C" no-undo.
def var v-cost as log format "Y/N" no-undo.
def var v-subtotal-flag as log format "Y/N" no-undo.
def var v-prt-name like oe-ord.cust-name.
def var v-wid like po-ordl.s-wid.
def var v-len like po-ordl.s-len.
def var v-dep like item.s-dep.
def var v-bwt like item.basis-w.
def var v-s-num   like po-ordl.s-num init 1 no-undo.
DEF VAR ld-oqty AS DEC FORM "->>>,>>>,>>9.9<<<<<" NO-UNDO.
DEF VAR ld-rqty AS DEC FORM "->>>,>>>,>>9.9<<<<<" NO-UNDO.
DEF VAR lv-uom LIKE po-ordl.pr-qty-uom NO-UNDO.
DEF VAR ld AS DEC EXTENT 2 NO-UNDO.
DEF VAR v-mach AS CHAR EXTENT 4 NO-UNDO.
DEF VAR v-sortby AS CHAR NO-UNDO.
DEF VAR v-closed AS LOGICAL NO-UNDO.
DEF VAR v-mch-rowid AS ROWID NO-UNDO.
def var v-cust-vend as char format "x(20)" init "------ VENDOR ------".
def var tot-cons-qty like po-ordl.cons-qty extent 2 no-undo.
def var tot-rec-qty as dec extent 2 no-undo.
def var tot-msf-rem as dec extent 2 no-undo.
def var tot-cst-rem as dec extent 2 no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-ord-no AS INT FORM ">>>>>9" NO-UNDO.
DEF VAR vFGItem# AS cha FORM "x(15)" NO-UNDO.
DEF BUFFER boe-ord FOR oe-ord.
DEF BUFFER bpo-ordl FOR po-ordl.
DEF BUFFER bwk-sh FOR wk-sh-ord.
DEF VAR cDisplay AS cha NO-UNDO.
DEF VAR cExcelDisplay AS cha NO-UNDO.
DEF VAR hField AS HANDLE NO-UNDO.
DEF VAR cTmpField AS CHA NO-UNDO.
DEF VAR cVarValue AS cha NO-UNDO.
DEF VAR cExcelVarValue AS cha NO-UNDO.
DEF VAR cFieldName AS cha NO-UNDO.
DEF VAR cSelectedList AS cha NO-UNDO.
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR v-vend-name AS cha NO-UNDO.
DEF VAR iSubCount AS INT NO-UNDO.
DEF VAR v-jobDueDate AS DATE NO-UNDO.
assign
 str-tit2 = TRIM(c-win:TITLE) + " (P-R-1)"
 {sys/inc/ctrtext.i str-tit2 112}
 v-s-vend         = begin_vend-no
 v-e-vend         = end_vend-no 
 v-s-date         = begin_due-date
 v-e-date         = end_due-date
 v-name           = SUBSTR(rd_show,1,1)  
 v-cost           = tb_sheets
 v-subtotal-flag  = tb_date
 v-sortby         = rd_sort
 v-closed         = tb_closed.

IF v-cost THEN DO: 
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
  v-cost = ll-secure. 
END.

FOR EACH ttRptSelected BY ttRptSelected.DisplayOrder:

  IF LENGTH(ttRptSelected.TextList) = ttRptSelected.FieldLength 
  THEN ASSIGN str-tit4 = str-tit4 + ttRptSelected.TextList + " "
              str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".        
  ELSE 
  ASSIGN str-tit4 = str-tit4 + 
           (IF ttRptSelected.HeadingFromLeft THEN
               ttRptSelected.TextList + FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList))
           ELSE FILL(" ",ttRptSelected.FieldLength - LENGTH(ttRptSelected.TextList)) + ttRptSelected.TextList) + " "
            str-tit5 = str-tit5 + FILL("-",ttRptSelected.FieldLength) + " "
            excelheader = excelHeader + ttRptSelected.TextList + ",".   

       IF LOOKUP(ttRptSelected.TextList, "Order Qty,Rcpt Qty,MSF Due,Cost Due") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " .
END.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.
SESSION:SET-WAIT-STATE ("general").
display "" with frame r-top.
  for each wk-sh-ord:
    delete wk-sh-ord.
  end.

  po-ord:
  for each po-ord
      where po-ord.company eq cocode
        and index("XF",po-ord.stat) eq 0 
        and po-ord.vend-no ge v-s-vend
        and po-ord.vend-no le v-e-vend
      no-lock:

      IF v-closed = NO AND po-ord.stat = "C" THEN NEXT po-ord.

    po-ordl:
    for each po-ordl WHERE
        po-ordl.company EQ po-ord.company AND
        po-ordl.po-no   EQ po-ord.po-no AND
        po-ordl.item-type
          and index("XF",po-ordl.stat) eq 0
          and po-ordl.due-date ge v-s-date
          and po-ordl.due-date le v-e-date
        no-lock:

        {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

        IF v-closed = NO AND po-ordl.stat = "C" THEN NEXT po-ordl.

      find first item
          where item.company eq cocode
            and item.i-no    eq po-ordl.i-no
            and po-ordl.item-type
          no-lock no-error.
      if avail item and index("1234BPR",item.mat-type) gt 0 then do:
        {po/po-fibr1.i v-mach[1] v-mach[2] v-mach[3] v-mach[4]}

        create wk-sh-ord.
        assign
         wk-sh-ord.due-date = po-ordl.due-date
         wk-sh-ord.machine  = v-mach[1]
         wk-sh-ord.rec-id   = recid(po-ordl).
        /* Task 11071308 */
        IF wk-sh-ord.machine = "" THEN do:
        FIND first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job-no  eq po-ordl.job-no
          and job-hdr.job-no2 eq po-ordl.job-no2
        no-lock no-error.

       if avail job-hdr THEN
         for first job
            where job.company EQ po-ordl.company
              and job.job-no  EQ po-ordl.job-no
              and job.job-no2 EQ po-ordl.job-no2
            no-lock,
          FIRST job-mch 
           WHERE job-mch.company EQ cocode
             and job-mch.job     EQ job.job
             AND job-mch.job-no  EQ job.job-no
             AND job-mch.job-no2 EQ job.job-no2
           NO-LOCK  USE-INDEX line-idx.
           wk-sh-ord.machine = job-mch.m-code .
           LEAVE.
         END.
        END.  /* Task 11071308 */

        IF v-sortby = "D" THEN 
            ASSIGN wk-sh-ord.key1 = string(year(po-ordl.due-date),"9999") +  
                                    string(MONTH(po-ordl.due-date),"99") + 
                                    string(DAY(po-ordl.due-date),"99").
        ELSE IF v-sortby = "M" THEN ASSIGN wk-sh-ord.key1 = wk-sh-ord.machine .
        ELSE IF v-sortby = "V" THEN ASSIGN wk-sh-ord.key1 = string(get-vendor-name(),"x(20)").
      end.
    end.
  end.

  for each wk-sh-ord,
      each po-ordl where recid(po-ordl) eq wk-sh-ord.rec-id no-lock,

      FIRST po-ord WHERE
            po-ord.company EQ po-ordl.company and
            po-ord.po-no   EQ po-ordl.po-no
            no-lock,

      first item
      where item.company eq cocode
        and item.i-no    eq po-ordl.i-no
      no-lock

      break by wk-sh-ord.key1 
            BY wk-sh-ord.due-date:

      {custom/statusMsg.i " 'Processing PO#  '  + string(po-ordl.po-no) "}

    ASSIGN
     lv-uom  = IF rd_uom BEGINS "P" THEN po-ordl.pr-qty-uom ELSE
               IF rd_uom BEGINS "C" THEN po-ordl.cons-uom   ELSE "MSF"
     v-len   = po-ordl.s-len
     v-wid   = po-ordl.s-wid
     v-dep   = item.s-dep
     v-bwt   = 0
     v-s-num = po-ordl.s-num.
    IF FIRST-OF(wk-sh-ord.key1) THEN
       ASSIGN iSubCount = 0.
    iSubCount = iSubCount + 1.
    {po/pol-dims.i}

    if v-wid eq 0 then v-wid = 12.
    if v-len eq 0 OR lv-uom EQ "ROLL" then v-len = 12.

    ld-oqty = po-ordl.ord-qty.

    IF po-ordl.pr-qty-uom NE lv-uom THEN
      RUN sys/ref/convquom.p (po-ordl.pr-qty-uom, lv-uom,
                              v-bwt, v-len, v-wid, v-dep,
                              ld-oqty,
                              OUTPUT ld-oqty).
    ld-rqty = po-ordl.t-rec-qty.

    IF po-ordl.cons-uom NE lv-uom THEN
      RUN sys/ref/convquom.p (po-ordl.cons-uom, lv-uom,
                              v-bwt, v-len, v-wid, v-dep,
                              ld-rqty,
                              OUTPUT ld-rqty).

    IF po-ordl.item-type EQ YES   AND
       lv-uom NE po-ordl.cons-uom THEN DO:

      ld[2] = 0.

      FOR EACH rm-rcpth NO-LOCK
          WHERE rm-rcpth.company   EQ po-ordl.company
            AND rm-rcpth.po-no     EQ STRING(po-ordl.po-no)
            AND rm-rcpth.i-no      EQ po-ordl.i-no
            AND rm-rcpth.rita-code EQ "R",
          EACH rm-rdtlh NO-LOCK
          WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
            AND rm-rdtlh.job-no  EQ po-ordl.job-no
            AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
            AND rm-rdtlh.s-num   EQ po-ordl.s-num:

        IF lv-uom EQ "ROLL" AND rm-rdtlh.tag NE "" THEN ld[1] = 1.
        ELSE DO:
          ld[1] = rm-rdtlh.qty.

          IF rm-rcpth.pur-uom NE lv-uom THEN
            RUN sys/ref/convquom.p(rm-rcpth.pur-uom, lv-uom,
                                   v-bwt, v-len, v-wid, v-dep,
                                   ld[1],
                                   OUTPUT ld[1]).
        END.

        ld[2] = ld[2] + ld[1].
      END.
    END.

    ELSE ld[2] = ld-rqty.

    v-msf-rem = ld-oqty - ld-rqty.

    IF lv-uom NE "MSF" THEN
      RUN sys/ref/convquom.p (lv-uom, "MSF",
                              v-bwt, v-len, v-wid, v-dep,
                              v-msf-rem,
                              OUTPUT v-msf-rem).
    ld-rqty = ld[2].
    RUN sys/ref/convcuom.p (po-ordl.cons-uom, "MSF",
                            v-bwt, v-len, v-wid, v-dep,
                            po-ordl.cons-cost,
                            OUTPUT v-cst-rem).

    ASSIGN
     v-cst-rem  = v-msf-rem * v-cst-rem
     v-prt-name = ""
     v-vend-name = ""
     .

    find first vend
          where vend.company = cocode
            and vend.vend-no = po-ord.vend-no
          no-lock no-error.
      if avail vend then ASSIGN v-vend-name = vend.name.

    release oe-ord.

    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job-no  eq po-ordl.job-no
          and job-hdr.job-no2 eq po-ordl.job-no2
        no-lock no-error.

    if avail job-hdr then v-jobDueDate = job-hdr.due-date.
    IF wk-sh-ord.machine = "" and avail job-hdr THEN DO:

      v-mch-rowid = ?.
      FOR FIRST job-mch 
          WHERE job-mch.company = cocode
            AND job-mch.job-no  = job-hdr.job-no
            AND job-mch.job-no2 = job-hdr.job-no2
            NO-LOCK  USE-INDEX line-idx.
          v-mch-rowid = ROWID(job-mch).
          LEAVE.
      END.
      IF v-mch-rowid <> ? THEN
          FIND job-mch WHERE ROWID(job-mch) = v-mch-rowid NO-LOCK NO-ERROR.
      IF AVAIL job-mch THEN
          wk-sh-ord.machine = job-mch.m-code.
    END.

    if po-ordl.ord-no ne 0 then
      find first oe-ord
           where oe-ord.company eq cocode
            and oe-ord.ord-no  eq po-ordl.ord-no
          no-lock no-error.

    if not avail oe-ord and avail job-hdr then
    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq job-hdr.ord-no
        no-lock no-error.

    if avail oe-ord  then ASSIGN v-prt-name = oe-ord.cust-name
                                 .
    IF v-prt-name EQ "" THEN DO:
        FIND FIRST cust WHERE cust.company EQ cocode
        AND cust.cust-no EQ po-ordl.cust-no NO-LOCK NO-ERROR.
        IF AVAIL cust THEN
            v-prt-name = cust.NAME .                                /* Task# 10081307 */
    END.
    if v-prt-name EQ "" THEN do:
        IF AVAIL job-hdr
             THEN do:
            FIND FIRST itemfg WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ job-hdr.i-no NO-LOCK NO-ERROR.

            IF AVAIL itemfg THEN
                v-prt-name = itemfg.cust-name .
        END.
    END.                                                        /* Task# 10081307 */

    if v-msf-rem lt 0 then v-msf-rem = 0.
    if v-cst-rem lt 0 then v-cst-rem = 0.
    ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".  
    IF AVAIL oe-ord THEN BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
    BUFFER bpo-ordl:FIND-BY-ROWID(ROWID(po-ordl),NO-LOCK) .
    BUFFER bwk-sh:FIND-BY-ROWID(ROWID(wk-sh-ord), NO-LOCK) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).         
         IF INDEX(cTmpField,".") > 0 THEN DO:
                 cFieldName = cTmpField.
                 cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
                 IF cFieldName BEGINS "po-ordl" THEN hField = BUFFER bpo-ordl:BUFFER-FIELD(cTmpField) .
                 ELSE IF cFieldName BEGINS "oe-ord" THEN DO:
                    IF AVAIL boe-ord THEN hField = BUFFER boe-ord:BUFFER-FIELD(cTmpField).
                    ELSE hField = ?.
                 END.
                 ELSE hfield = BUFFER bwk-sh:BUFFER-FIELD(cTmpField).
                 IF hField <> ? THEN DO:                 
                     cTmpField = substring(GetFieldValue(hField),1,int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
                     IF ENTRY(i,cSelectedList) = "Job#" THEN
                        cTmpField = cTmpField + IF cTmpField <> "" THEN "-" + string(job-hdr.job-no2,"99") ELSE "".                  

                     cDisplay = cDisplay + cTmpField + 
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                               .
                     cExcelDisplay = cExcelDisplay + quoter(GetFieldValue(hField)) + ",".                    
                 END.
                 ELSE DO:
                    cTmpField = substring(cFieldName,1,int( entry( getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength) ) ).                  
                    cDisplay = cDisplay + FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 ).
                    cExcelDisplay = cExcelDisplay + quoter(" ") + ",".
                 END.
         END.
         ELSE DO:       
            CASE cTmpField:                                   
                {porep/r-sonordN1.i}
            END CASE.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         END.
      END.
      PUT UNFORMATTED cDisplay SKIP.
      IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
      END.
      /*===== end of new ===== */


    assign
     tot-cons-qty[1] = tot-cons-qty[1] + ld-oqty
     tot-rec-qty[1]  = tot-rec-qty[1] + ld-rqty
     tot-msf-rem[1]  = tot-msf-rem[1] + v-msf-rem
     tot-cst-rem[1]  = tot-cst-rem[1] + v-cst-rem.

/*===========*/
    if last-of(wk-sh-ord.key1) then do:
      if v-subtotal-flag  /*AND iSubCount > 1*/ then do: 
           /* task 12311303  */
          PUT    SKIP  str-line SKIP .

          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".  
    IF AVAIL oe-ord THEN BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
    BUFFER bpo-ordl:FIND-BY-ROWID(ROWID(po-ordl),NO-LOCK) .
    BUFFER bwk-sh:FIND-BY-ROWID(ROWID(wk-sh-ord), NO-LOCK) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).         
            CASE cTmpField:                                   
               WHEN "vFGItem#" THEN cVarValue =  "".
               WHEN "wk-sh-ord.due-date" THEN cVarValue =  "".
               WHEN "wk-sh-ord.machine" THEN cVarValue =  "".
               WHEN "po-ordl.po-no" THEN cVarValue =  "".
               WHEN "po-ordl.i-no" THEN cVarValue =  "".
               WHEN "ld-oqty" THEN cVarValue = string(tot-cons-qty[1],"->>>,>>>,>>9.9<<<<<").
                   WHEN "ld-rqty" THEN cVarValue = string(tot-rec-qty[1],"->>>,>>>,>>9.9<<<<<").
                   WHEN "v-cst-rem" THEN cVarValue = string(tot-cst-rem[1],">>,>>>,>>9").
                   WHEN "v-msf-rem" THEN cVarValue = string(tot-msf-rem[1],">>,>>9.999").
                   WHEN "v-ord-no" THEN cVarValue = "".
                   WHEN "v-cust-name" THEN cVarValue = "".
                   WHEN "v-vend-name" THEN cVarValue = "".
                   WHEN "lv-uom" THEN cVarValue = "".
                   WHEN "v-wid" THEN cVarValue = "".
                   WHEN "v-len" THEN cVarValue = "".
                   WHEN "v-job-no" THEN cVarValue =  "".
                   WHEN "v-jobDueDate" THEN cVarValue =  "".

               END CASE.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         /*END.*/
      END.
      PUT UNFORMATTED "        Sub Total:"  substring(cDisplay,19,300) SKIP(1).
      IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
            "SUB TOTAL" +   substring(cExcelDisplay,3,300) SKIP.
      END.
            /* task 12311303  */
        /*PUT UNFORMATTED space(10) FILL('-',80) SKIP.
        PUT "Sub Total:" SPACE(30)    "Qty Ordered:" tot-cons-qty[1] 
                "    Qty Received:" tot-rec-qty[1]              
                "    MSF Remain:" tot-msf-rem[1]              
                /*"    Cost Remain: " 
                IF v-cost THEN tot-cst-rem[1]  ELSE 0*/
            SKIP(1).

        IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
               SKIP(1)
               '"' "SUB TOTAL"                                      '",'
               '"' "Qty Ordered"                                    '",'
               '"' STRING(tot-cons-qty[1],"->>>,>>>,>>9.9<<<<<")    '",'
               '"' "Qty Received"                                   '",'
               '"' STRING(tot-rec-qty[1],"->>>,>>>,>>9.9<<<<<")     '",'
               '"' "MSF Remain"                                     '",'
               '"' STRING(tot-msf-rem[1],">,>>>,>>9.99")                '",'
               /*'"' "Cost Remain"                                    '",'
               '"' (IF v-cost THEN STRING(tot-cst-rem[1],">>>,>>9")
                    ELSE "")                                        '",'*/
               SKIP.*/
      end.

      assign
       tot-cons-qty[2] = tot-cons-qty[2] + tot-cons-qty[1]
       tot-rec-qty[2]  = tot-rec-qty[2]  + tot-rec-qty[1]
       tot-msf-rem[2]  = tot-msf-rem[2]  + tot-msf-rem[1]
       tot-cst-rem[2]  = tot-cst-rem[2]  + tot-cst-rem[1]

       tot-cons-qty[1] = 0
       tot-rec-qty[1]  = 0
       tot-msf-rem[1]  = 0
       tot-cst-rem[1]  = 0.
    end.

    if LAST(wk-sh-ord.key1) then do:
    /* task 12311303  */        
        PUT   
            SKIP  str-line SKIP .

          ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".  
    IF AVAIL oe-ord THEN BUFFER boe-ord:FIND-BY-ROWID(ROWID(oe-ord),NO-LOCK) .
    BUFFER bpo-ordl:FIND-BY-ROWID(ROWID(po-ordl),NO-LOCK) .
    BUFFER bwk-sh:FIND-BY-ROWID(ROWID(wk-sh-ord), NO-LOCK) .
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
         cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).         
            CASE cTmpField:                                   
               WHEN "vFGItem#" THEN cVarValue =  "".
               WHEN "wk-sh-ord.due-date" THEN cVarValue =  "".
               WHEN "wk-sh-ord.machine" THEN cVarValue =  "".
               WHEN "po-ordl.po-no" THEN cVarValue =  "".
               WHEN "po-ordl.i-no" THEN cVarValue =  "".
               WHEN "ld-oqty" THEN cVarValue = string(tot-cons-qty[2],"->>>,>>>,>>9.9<<<<<").
                   WHEN "ld-rqty" THEN cVarValue = string(tot-rec-qty[2],"->>>,>>>,>>9.9<<<<<").
                   WHEN "v-cst-rem" THEN cVarValue = string(tot-cst-rem[2],">>,>>>,>>9").
                   WHEN "v-msf-rem" THEN cVarValue = string(tot-msf-rem[2],">>,>>9.999").
                   WHEN "v-ord-no" THEN cVarValue = "".
                   WHEN "v-cust-name" THEN cVarValue = "".
                   WHEN "v-vend-name" THEN cVarValue = "".
                   WHEN "lv-uom" THEN cVarValue = "".
                   WHEN "v-wid" THEN cVarValue = "".
                   WHEN "v-len" THEN cVarValue = "".
                   WHEN "v-job-no" THEN cVarValue =  "".
                   WHEN "v-jobDueDate" THEN cVarValue =  "".

               END CASE.
            cExcelVarValue = cVarValue.
            cDisplay = cDisplay + cVarValue +
                       FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
            cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
         /*END.*/
      END.
      PUT UNFORMATTED "            Total:"  substring(cDisplay,19,300) SKIP(1).
      IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
            "TOTAL " + SUBSTRING(cExcelDisplay,3,300) SKIP.
      END.

           /* task 12311303  */
        /*if v-subtotal-flag then do:      */
        /*PUT UNFORMATTED space(10) FILL('-',80) SKIP.
        PUT "Total:    " SPACE(30) "Qty Ordered:" tot-cons-qty[2] 
                "    Qty Received:" tot-rec-qty[2]              
                "    MSF Remain:" tot-msf-rem[2]              
                /*"    Cost Remain: " 
                IF v-cost THEN tot-cst-rem[2]  ELSE 0*/
            SKIP.

        IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
           SKIP(1)
           '"' "TOTAL"                                            '",'
           '"' "Qty Ordered"                                    '",'
           '"' ""                                            '",'
           '"' STRING(tot-cons-qty[2],"->>>,>>>,>>9.9<<<<<")    '",'
           '"' "Qty Received"                                   '",'
           '"' ""                                            '",'
           '"' STRING(tot-rec-qty[2],"->>>,>>>,>>9.9<<<<<")     '",'
           '"' "MSF Remain"                                     '",'
           '"' STRING(tot-msf-rem[2],">,>>>,>>9.99")                '",'
           /*'"' "Cost Remain"                                    '",'
           '"' (IF v-cost THEN STRING(tot-cst-rem[2],">>>,>>9")
                ELSE "")                                        '",' */
           SKIP.*/
      END.
    /*end.*/
    /* =========   */
  END.

IF tb_excel THEN DO:
  OUTPUT STREAM excel CLOSE.
  IF tb_runExcel THEN
    OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-vendor-name C-Win 
FUNCTION get-vendor-name RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

     find first vend
        where vend.company = cocode
          and vend.vend-no = po-ord.vend-no
        no-lock no-error.
    if avail vend then 
        RETURN vend.name.
    ELSE
        RETURN "".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

