&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: porep\r-pofghs.w

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
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

    {sys/inc/var.i new shared}
  
assign
 cocode = gcompany
 locode = gloc.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEF STREAM excel.
DEF VAR excelcol AS INT NO-UNDO.

DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg .
DEF VAR cTextListToDefault AS cha NO-UNDO.
DEF VAR cColumnInit AS LOG INIT YES NO-UNDO.


ASSIGN cTextListToSelect = "PO #,Vendor,Item #,Item Description,Control #,Packed,Qty Ord,Trans Date," +
                           "Trans Qty,Amount,UOM,Balance Due"
                         
       cFieldListToSelect = "po,vend,item,desc,cont,pack,qty-ord,tran-date," +
                            "tran-qty,amt,uom,bal-due"

       cFieldLength = "6,8,15,25,15,10,13,10," + "11,10,8,11"
       cFieldType = "i,c,c,c,c,c,i,c," + "i,i,c,i"  
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "PO #,Vendor,Item #,Item Description,Control #,Packed,Qty Ord,Trans Date," +
                           "Trans Qty,Amount,UOM,Balance Due" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_po-no end_po-no ~
begin_po-date end_po-date begin_vend end_vend begin_po-i-no end_po-i-no ~
begin_ctrl-no end_ctrl-no begin_buyer end_buyer rd_sort select-mat rd_show ~
tb_fg tb_rm rd-dest lv-ornt lines-per-page lv-font-no td-show-parm ~
tb_runExcel tb_excel fi_file btn-ok btn-cancel btn_SelectColumns
&Scoped-Define DISPLAYED-OBJECTS begin_po-no end_po-no begin_po-date ~
end_po-date begin_vend end_vend begin_po-i-no end_po-i-no begin_ctrl-no ~
end_ctrl-no begin_buyer end_buyer lbl_sort rd_sort select-mat lbl_show ~
rd_show tb_fg mat-types tb_rm rd-dest lv-ornt lines-per-page lv-font-no ~
lv-font-name td-show-parm tb_runExcel tb_excel fi_file 

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
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-ok 
     LABEL "&OK" 
     SIZE 15 BY 1.14.


DEFINE BUTTON btn_SelectColumns 
     LABEL "Select Columns" 
     SIZE 43 BY 1.19.


DEFINE VARIABLE begin_buyer AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Buyer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_ctrl-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning Vendor Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Beginning PO Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Beginning PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)":U 
     LABEL "Beginning Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_buyer AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Buyer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_ctrl-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending Vendor Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending PO Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" 
     LABEL "Ending PO Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 999999 
     LABEL "Ending PO#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" 
     LABEL "Ending Vendor#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-pofghs.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_show AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Sort By?" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE mat-types AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material Types" 
     VIEW-AS FILL-IN 
     SIZE 1 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "L" 
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

DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Open" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Open", "Open",
"Closed", "Closed",
"All PO's", "All PO's"
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Vendor" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vendor", "Vendor",
"Item", "Item",
"Control#", "Control#"
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 9.05.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 14.05.

DEFINE VARIABLE select-mat AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 28 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 5.19 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_fg AS LOGICAL INITIAL yes 
     LABEL "Print FGs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_rm AS LOGICAL INITIAL yes 
     LABEL "Print RMs?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_po-no AT ROW 2.19 COL 24 COLON-ALIGNED HELP
          "Enter Beginning PO Number"
     end_po-no AT ROW 2.19 COL 65 COLON-ALIGNED HELP
          "Enter Ending PO Number"
     begin_po-date AT ROW 3.14 COL 24 COLON-ALIGNED HELP
          "Enter Beginning PO Date"
     end_po-date AT ROW 3.14 COL 65 COLON-ALIGNED HELP
          "Enter Ending PO Date"
     begin_vend AT ROW 4.1 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Vendor Number"
     end_vend AT ROW 4.1 COL 65 COLON-ALIGNED HELP
          "Enter Ending Vendor Number"
     begin_po-i-no AT ROW 5.05 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_po-i-no AT ROW 5.05 COL 65 COLON-ALIGNED HELP
          "Enter Ending Item Number"
     begin_ctrl-no AT ROW 6 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Control Number"
     end_ctrl-no AT ROW 6 COL 65 COLON-ALIGNED HELP
          "Enter Ending Control Number"
     begin_buyer AT ROW 6.95 COL 24 COLON-ALIGNED HELP
          "Enter Beginning Buyer" WIDGET-ID 2
     end_buyer AT ROW 6.95 COL 65 COLON-ALIGNED HELP
          "Enter Ending Buyer" WIDGET-ID 4
     sl_avail AT ROW 14.81 COL 3 NO-LABEL WIDGET-ID 26
     sl_selected AT ROW 15.19 COL 15.8 NO-LABEL WIDGET-ID 28
     lbl_sort AT ROW 8.62 COL 2 COLON-ALIGNED NO-LABEL
     rd_sort AT ROW 8.62 COL 15 NO-LABEL
     select-mat AT ROW 9.1 COL 61 NO-LABEL
     btn_SelectColumns AT ROW 13.50 COL 12 WIDGET-ID 10
     lbl_show AT ROW 9.57 COL 5 COLON-ALIGNED NO-LABEL
     rd_show AT ROW 9.57 COL 15 NO-LABEL
     tb_fg AT ROW 10.52 COL 11
     mat-types AT ROW 11 COL 56 COLON-ALIGNED
     tb_rm AT ROW 11.48 COL 11
     rd-dest AT ROW 16.48 COL 7 NO-LABEL
     lv-ornt AT ROW 16.48 COL 31 NO-LABEL
     lines-per-page AT ROW 16.48 COL 84 COLON-ALIGNED
     lv-font-no AT ROW 18.62 COL 35 COLON-ALIGNED
     lv-font-name AT ROW 19.57 COL 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.76 COL 31
     tb_runExcel AT ROW 22.1 COL 72 RIGHT-ALIGNED
     tb_excel AT ROW 22.1 COL 51 RIGHT-ALIGNED
     fi_file AT ROW 22.91 COL 29 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 24.57 COL 19
     btn-cancel AT ROW 24.57 COL 57
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.52 COL 4
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 5
          BGCOLOR 2 
     "Select/Deselect RM Types" VIEW-AS TEXT
          SIZE 31 BY 1 AT ROW 8.14 COL 59
          FONT 6
     RECT-6 AT ROW 15.29 COL 2
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.2 BY 25.24.


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
         TITLE              = "PO RM/FG History by Vendor"
         HEIGHT             = 25.29
         WIDTH              = 95.4
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
       begin_buyer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_ctrl-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_buyer:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_ctrl-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-i-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_po-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_vend:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_show IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_show".

/* SETTINGS FOR FILL-IN lbl_sort IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_sort".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mat-types IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       mat-types:HIDDEN IN FRAME FRAME-A           = TRUE
       mat-types:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".


ASSIGN 
       sl_avail:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl_selected IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       sl_selected:HIDDEN IN FRAME FRAME-A           = TRUE.


ASSIGN 
       rd_show:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       rd_sort:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       select-mat:AUTO-RESIZE IN FRAME FRAME-A      = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_fg:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_rm:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* PO RM/FG History by Vendor */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PO RM/FG History by Vendor */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_buyer C-Win
ON LEAVE OF begin_buyer IN FRAME FRAME-A /* Beginning Buyer */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_ctrl-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_ctrl-no C-Win
ON LEAVE OF begin_ctrl-no IN FRAME FRAME-A /* Beginning Vendor Item# */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-date C-Win
ON LEAVE OF begin_po-date IN FRAME FRAME-A /* Beginning PO Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_po-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-i-no C-Win
ON LEAVE OF begin_po-i-no IN FRAME FRAME-A /* Beginning PO Item# */
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
  RUN GetSelectionList.
  run run-report. 
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_vend
                            &END_cust=END_vend
                            &fax-subject=c-win:title
                            &fax-body=c-win:title
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = "Vendor"
                             &begin_cust= begin_vend
                             &END_cust=end_vend
                             &mail-subject=c-win:title
                             &mail-body=c-win:title
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = "vendor"
                                  &begin_cust= begin_vend
                                  &END_cust=end_vend
                                  &mail-subject=c-win:title
                                  &mail-body=c-win:title
                                  &mail-file=list-name }

           END.
 
       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_SelectColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SelectColumns C-Win
ON CHOOSE OF btn_SelectColumns IN FRAME FRAME-A /* Select Columns */
DO:
    DEF VAR cTextSelected AS cha NO-UNDO.
    DEF VAR cTextListed AS cha NO-UNDO.

    RUN displaySelectionList2.

    ASSIGN cTextSelected = sl_selected:LIST-ITEMS
           cTextListed = sl_avail:LIST-ITEMS.
 
    IF NOT cColumnInit THEN RUN custom/d-rptsel.w (INPUT-OUTPUT cTextListed, INPUT-OUTPUT cTextSelected, INPUT-OUTPUT cTextListToDefault, INPUT-OUTPUT cTextListToSelect).

    ASSIGN sl_selected:LIST-ITEMS = cTextSelected
           sl_avail:LIST-ITEMS = cTextListed.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_buyer C-Win
ON LEAVE OF end_buyer IN FRAME FRAME-A /* Ending Buyer */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_vend C-Win
ON LEAVE OF end_vend IN FRAME FRAME-A /* Ending Vendor# */
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
  {custom/chgfont2.i "12" }
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


&Scoped-define SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fg C-Win
ON VALUE-CHANGED OF tb_fg IN FRAME FRAME-A /* Print FGs? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_rm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_rm C-Win
ON VALUE-CHANGED OF tb_rm IN FRAME FRAME-A /* Print RMs? */
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
DEF VAR v-mat-list AS CHAR NO-UNDO.
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
   begin_po-date = date(1,1,year(today))
   end_po-date   = today.
  RUN DisplaySelectionList.
  RUN enable_UI.
  
 for each mat:
    v-mat-list = v-mat-list + string(mat.mat,"x(5)") + " " + mat.dscr + ",".
  end.
  if substr(v-mat-list,length(trim(v-mat-list)),1) eq "," then
    substr(v-mat-list,length(trim(v-mat-list)),1) = "".
  
  select-mat:list-items = v-mat-list.
        
  do i = 1 to select-mat:num-items:
    if trim(substr(select-mat:entry(i),1,5)) eq "B" then do:
      select-mat:screen-value = entry(i,v-mat-list).
      leave.
    end.
  end.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    APPLY "entry" TO begin_po-no.
  END.
cColumnInit   = NO .
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-report-recs C-Win 
PROCEDURE build-report-recs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ipv-s-pono like po-ord.po-no format ">>>>>>" NO-UNDO.
def input parameter ipv-e-pono like ipv-s-pono init 999999 NO-UNDO.
def input parameter ipv-s-date like po-ord.po-date format "99/99/9999" init "01/01/0001" NO-UNDO.
def input parameter ipv-e-date like ipv-s-date init TODAY NO-UNDO.
def input parameter ipv-s-vend like po-ord.vend-no NO-UNDO.
def input parameter ipv-e-vend like ipv-s-vend init "zzzzzzzz" NO-UNDO.
def input parameter ipv-s-item like po-ordl.i-no NO-UNDO.
def input parameter ipv-e-item like ipv-s-item init "zzzzzzzzzzzzzzz" NO-UNDO.
def input parameter ipv-s-vitm like po-ordl.vend-i-no NO-UNDO.
def input parameter ipv-e-vitm like ipv-s-vitm init "zzzzzzzzzzzzzzz" NO-UNDO.
def input parameter ipv-s-buyer like po-ord.buyer NO-UNDO.
def input parameter ipv-e-buyer like ipv-s-buyer init "zzzzzzzzzzzzzzz" NO-UNDO.

def input parameter ipv-stat   as   char format "!" init "A" NO-UNDO.
def input parameter ipv-type   as   char format "!" init "B" NO-UNDO.
def input parameter ipv-sort   as   char format "!" init "V" NO-UNDO.
def input parameter ipv-term   AS   CHAR NO-UNDO.
def input parameter ipv-mattype-list as   char format "x(36)" NO-UNDO.

DEF BUFFER xjob-mat FOR job-mat.
     for each po-ord
        where po-ord.company eq cocode
          and po-ord.po-no   ge ipv-s-pono
          and po-ord.po-no   le ipv-e-pono
          and po-ord.po-date ge ipv-s-date
          and po-ord.po-date le ipv-e-date
          and po-ord.vend-no ge ipv-s-vend
          and po-ord.vend-no le ipv-e-vend
          AND po-ord.buyer   GE ipv-s-buyer
          AND po-ord.buyer   LE ipv-e-buyer
          and ((po-ord.stat eq "C" and ipv-stat eq "C") or
               (po-ord.stat ne "C" and ipv-stat eq "O") or ipv-stat eq "A")
        no-lock,
          
        each po-ordl
        where po-ordl.company   EQ po-ord.company
          AND po-ordl.po-no     EQ po-ord.po-no
          AND po-ordl.i-no      ge ipv-s-item
          and po-ordl.i-no      le ipv-e-item
          and po-ordl.vend-i-no ge ipv-s-vitm
          and po-ordl.vend-i-no le ipv-e-vitm
          and ((po-ordl.stat eq "C" and ipv-stat eq "C") or
               (po-ordl.stat ne "C" and ipv-stat eq "O") or ipv-stat eq "A")
          and ((po-ordl.item-type eq yes and ipv-type eq "R") or
               (po-ordl.item-type eq no  and ipv-type eq "F") or ipv-type eq "B")
        no-lock:
      
      release item.
      release job-hdr.
      release job-mat.
      
      find first item
          where item.company eq cocode
            and item.i-no    eq po-ordl.i-no
          no-lock no-error.
      
      if po-ordl.item-type and not avail item then next.
      
      if index(ipv-mattype-list,"A") ne 0 and
         po-ordl.job-no ne ""           and 
         avail item                     and
         item.mat-type eq "B"           then
      find first job-hdr
          where job-hdr.company eq cocode
            and job-hdr.job-no  eq po-ordl.job-no
            and job-hdr.job-no2 eq po-ordl.job-no2
          no-lock no-error.
       
      if avail job-hdr then
      find first job-mat
          where job-mat.company  eq cocode
            and job-mat.job-no   eq job-hdr.job-no
            and job-mat.job-no2  eq job-hdr.job-no2
            and job-mat.job      eq job-hdr.job
            and job-mat.rm-i-no  eq po-ordl.i-no
            and job-mat.frm      eq po-ordl.s-num
          no-lock no-error. 
      
      if avail job-mat then
      for each xjob-mat
          where xjob-mat.company  eq cocode
            and xjob-mat.job      eq job-mat.job
            and xjob-mat.frm      eq job-mat.frm
            and xjob-mat.job-no   eq job-mat.job-no
            and xjob-mat.job-no2  eq job-mat.job-no2
            and can-find(first item where item.company  eq cocode
                                      and item.i-no     eq xjob-mat.i-no
                                      and item.mat-type eq "A")
          no-lock:
        
        create report.
        assign
         report.term-id = ipv-term
         report.key-01  = string(int(po-ordl.item-type),"9")
         report.key-02  = if ipv-sort eq "V" then po-ord.vend-no else ""
         report.key-03  = if ipv-sort ne "C" then xjob-mat.i-no   else ""
         report.key-04  = ""
         report.key-05  = xjob-mat.i-no
         report.key-06  = po-ord.vend-no
         report.key-07  = xjob-mat.qty-uom
         report.rec-id  = recid(po-ordl).
      end.
      
      if avail item and index(ipv-mattype-list,item.mat-type) eq 0 then next.

      create report.
      assign
       report.term-id = ipv-term
       report.key-01  = string(int(po-ordl.item-type),"9")
       report.key-02  = if ipv-sort eq "V" then po-ord.vend-no else ""
       report.key-03  = if ipv-sort ne "C" then po-ordl.i-no   else ""
       report.key-04  = po-ordl.vend-i-no
       report.key-05  = po-ordl.i-no
       report.key-06  = po-ord.vend-no
       report.rec-id  = recid(po-ordl).
    end.

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
  DISPLAY begin_po-no end_po-no begin_po-date end_po-date begin_vend end_vend 
          begin_po-i-no end_po-i-no begin_ctrl-no end_ctrl-no begin_buyer 
          end_buyer lbl_sort rd_sort select-mat lbl_show rd_show tb_fg mat-types 
          tb_rm rd-dest lv-ornt lines-per-page lv-font-no lv-font-name 
          td-show-parm tb_runExcel tb_excel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_po-no end_po-no begin_po-date end_po-date 
         begin_vend end_vend begin_po-i-no end_po-i-no begin_ctrl-no 
         end_ctrl-no begin_buyer end_buyer rd_sort select-mat rd_show tb_fg 
         tb_rm rd-dest lv-ornt lines-per-page lv-font-no td-show-parm 
         tb_runExcel tb_excel fi_file btn-ok btn-cancel btn_SelectColumns
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-1-proc C-Win 
PROCEDURE excel-1-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-date AS DATE NO-UNDO.
  DEF INPUT PARAMETER ip-dec-1 AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dec-2 AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-uom AS CHAR NO-UNDO.

  PUT STREAM excel UNFORMATTED
     '"' (IF ip-date NE ? THEN STRING(ip-date)
         ELSE "")                             '",'
     '"' STRING(ip-dec-1,"->>,>>>,>>9.9<<<<<")       '",'
     '"' STRING(ip-dec-2,"->,>>>,>>9.99")      '",'
     '"' ip-uom                               '",'.
   excelcol = 11.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-2-proc C-Win 
PROCEDURE excel-2-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-balance AS DEC NO-UNDO.

  DEF VAR viIndex AS INT NO-UNDO.

  DO viIndex = excelcol TO 10:
     PUT STREAM excel UNFORMATTED
         '"' "" '",'.
  END.

  PUT STREAM excel UNFORMATTED
      '"' STRING(ip-balance,"->,>>>,>>9.9<<<<<") '",' SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-3-proc C-Win 
PROCEDURE excel-3-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-text AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-dec1 AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dec2 AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dec3 AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-dec4 AS DEC NO-UNDO.
  
  PUT STREAM excel UNFORMATTED
      SKIP
      '"' ""                                    '",' 
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' ip-text                               '",'
      '"' ""                                    '",'
      '"' ""                                    '",'
      '"' STRING(ip-dec1,"->>,>>>,>>9.9<<<<<")  '",'
      '"' ""                                    '",'
      '"' STRING(ip-dec2,"->,>>>,>>9.9<<<<<")    '",'
      '"' STRING(ip-dec4,"->,>>>,>>9.99")        '",'
      '"' ""                                    '",'
      '"' STRING(ip-dec3,"->>,>>>,>>9.9<<<<<")  '",'
      SKIP(1).

  IF ip-text EQ "Vendor Totals" THEN
     PUT STREAM excel UNFORMATTED SKIP.

  excelcol = 12.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel-skip-proc C-Win 
PROCEDURE excel-skip-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR i AS INT NO-UNDO.

  PUT STREAM excel UNFORMATTED SKIP.
  DO i = 1 TO 7:
     PUT STREAM excel UNFORMATTED
         '"' "" '",'.
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
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.*/
     
 {custom/out2file.i}.

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
/* ------------------------------------------------ po/rep/sh-pur.p 05/98 JLF */
/* PO FG/RM History                                                           */
/* -------------------------------------------------------------------------- */

/*{sys/form/r-topw.f}*/

 {sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.

form  header
      skip(1)
      day_str
      str-tit  format "x(112)"
      "Page" at 123
      page-number format ">>9"
      skip
      tim_str
      str-tit2 format "x(112)"   "{1}" at 123
      skip(1)
      
     with frame r-top row 1 column 1 stream-io width 250
	   no-labels no-box no-underline page-top.
  
def buffer xjob-mat for job-mat.

def var v-foot-rem like ap-invl.amt-msf no-undo.
def var v-msf-cal as log no-undo.

def var v-s-pono like po-ord.po-no format ">>>>>>" NO-UNDO.
def var v-e-pono like v-s-pono init 999999 NO-UNDO.
def var v-s-date like po-ord.po-date format "99/99/9999" init "01/01/0001" NO-UNDO.
def var v-e-date like v-s-date init TODAY NO-UNDO.
def var v-s-vend like po-ord.vend-no NO-UNDO.
def var v-e-vend like v-s-vend init "zzzzzzzz" NO-UNDO.
def var v-s-item like po-ordl.i-no NO-UNDO.
def var v-e-item like v-s-item init "zzzzzzzzzzzzzzz" NO-UNDO.
def var v-s-vitm like po-ordl.vend-i-no NO-UNDO.
def var v-e-vitm like v-s-vitm init "zzzzzzzzzzzzzzz" NO-UNDO.
def var v-s-buyer like po-ord.buyer NO-UNDO.
def var v-e-buyer like v-s-buyer init "zzzzzzzzzzzzzzz" NO-UNDO.

def var v-stat   as   char format "!" init "A" NO-UNDO.
def var v-type   as   char format "!" init "B" NO-UNDO.
def var v-sort   as   char format "!" init "V" NO-UNDO.

def var v-mattype-list          as   char format "x(36)" NO-UNDO.
def var v-mat-dscr              as   char format "x(20)" extent 21 NO-UNDO.

def var v-balance               like rm-rdtlh.qty NO-UNDO.
def var v-cons-qty              like po-ordl.cons-qty NO-UNDO.
def var bf-v-cons-qty              like po-ordl.cons-qty NO-UNDO.
def var v-trans-date              like rm-rcpth.trans-date NO-UNDO.
def var v-trans-qty              like rm-rdtlh.qty NO-UNDO.

def var v-first                 like report.key-02 extent 4 NO-UNDO.
def var v-ord                   like rm-rdtlh.qty extent 4 NO-UNDO.
def var v-qty                   like rm-rdtlh.qty extent 4 NO-UNDO.
def var v-bal                   like rm-rdtlh.qty extent 4 NO-UNDO.
DEF VAR v-amt AS DEC EXTENT 4 NO-UNDO.
DEF VAR v-amount AS DEC NO-UNDO.
DEF VAR v-uom AS cha NO-UNDO.

def var str-tit6                like str-tit3.
def var v-item-type             as   char format "x(14)" init "Finished Goods".
DEF VAR excel-skip   AS LOG NO-UNDO.
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

/*{sys/form/r-top5DL3.f} */
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF BUFFER b-rm-rcpth FOR rm-rcpth .
DEF BUFFER b-fg-rcpth FOR fg-rcpth .
DEF BUFFER b-rm-rdtlh FOR rm-rdtlh .
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh .

DEF VAR v-first-rec AS RECID NO-UNDO .

form header str-tit6 format "x(130)" skip
            v-item-type skip(1)
            str-tit4 SKIP
            str-tit5 SKIP

    with frame r-top.

FORM po-ord.po-no          column-label "PO #"
     po-ord.vend-no        column-label "Vendor"
     report.key-05         column-label "Item #"
                           format "x(15)"
     item.i-name           column-label "Item Description"
                           format "x(23)"
     po-ordl.vend-i-no     column-label "Control #"
     itemfg.prod-notes     column-label "Packed"
                           format "x(10)"
     v-cons-qty            column-label "Qty Ord"
     rm-rcpth.trans-date   column-label "Trans Date"
     rm-rdtlh.qty          column-label "Trans Qty"
     v-amount COLUMN-LABEL "Amount"
     v-uom    COLUMN-LABEL "UOM"
     v-balance             column-label "Balance Due"     
     with frame main no-box no-attr-space down STREAM-IO width 165 /*131*/ .
  
 {ce/msfcalc.i} 

assign
 str-tit2 = c-win:title
 {sys/inc/ctrtext.i str-tit2 112}
   
 v-s-pono    = begin_po-no
 v-e-pono    = END_po-no
 v-s-date    = begin_po-date
 v-e-date    = end_po-date
 v-s-vend    = begin_vend
 v-e-vend    = end_vend
 v-s-item    = begin_po-i-no
 v-e-item    = end_po-i-no
 v-s-vitm    = begin_ctrl-no
 v-e-vitm    = END_ctrl-no
 v-s-buyer   = begin_buyer
 v-e-buyer   = end_buyer
 v-sort      = SUBSTR(rd_sort,1,1)
 v-stat      = SUBSTR(rd_show,1,1)
 v-type      = IF tb_fg THEN
                 IF tb_rm THEN "B" ELSE "F"
               ELSE
                 IF tb_rm THEN "R" ELSE "".

do with frame {&frame-name}:          
  do i = 1 to select-mat:num-items:
    if select-mat:is-selected(i) then
      v-mattype-list = v-mattype-list + trim(substr(select-mat:entry(i),1,5)) + ",".
  end.
  
  if LENGTH(TRIM(v-mattype-list)) NE 0 AND substr(v-mattype-list,length(trim(v-mattype-list)),1) eq "," then
    substr(v-mattype-list,length(trim(v-mattype-list)),1) = "".
    
  mat-types = v-mattype-list.
  
  do i = 1 to length(mat-types):
    if substr(mat-types,i,1) eq "," then substr(mat-types,i,1) = " ".
  end.
  
  display mat-types.
end.

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

        IF LOOKUP(ttRptSelected.TextList, "Qty Ord,Trans Qty,Amount,Balance Due") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.

{sa/sa-sls01.i}
 
{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  /*excelheader = "PO #,Vendor,Item #,Item Description,Control #,Packed,"
              + "Qty Ord,Trans Date,Trans Qty,Amount,UOM,Balance Due".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

if td-show-parm then run show-param.

SESSION:SET-WAIT-STATE ("general").
  
DISPLAY WITH frame r-top.

{custom/statusMsg.i " 'Processing...'"}

RUN build-report-recs (
    v-s-pono, 
    v-e-pono, 
    v-s-date, 
    v-e-date, 
    v-s-vend, 
    v-e-vend, 
    v-s-item, 
    v-e-item,
    v-s-vitm, 
    v-e-vitm, 
    v-s-buyer,
    v-e-buyer,
    v-stat, 
    v-type, 
    v-sort,     
    v-term,
    v-mattype-list).

    for each report
        where report.term-id eq v-term,

        first po-ordl
        where recid(po-ordl) eq report.rec-id
        no-lock,

        first po-ord WHERE
              po-ord.company EQ po-ordl.company AND
              po-ord.po-no   EQ po-ordl.po-no no-lock
        break by report.key-01
              by report.key-02
              by report.key-03
              by report.key-04

        transaction:

         {custom/statusMsg.i " 'Processing PO#  '  + string(po-ord.po-no) "}

      if first-of(report.key-02) then v-first[2] = report.key-02.
      if first-of(report.key-03) then v-first[3] = report.key-03.
      if first-of(report.key-04) then v-first[4] = report.key-04.

      release item.
      release itemfg.

      if first-of(report.key-01) then do:
        if report.key-01 eq "1" then v-item-type = "Raw Materials".

        if first(report.key-01) then DISPLAY WITH frame r-top.
        else page.

        assign
         v-ord = 0
         v-qty = 0
         v-amt = 0   .
      end.

      v-cons-qty = po-ordl.cons-qty.
      
      if po-ordl.item-type then do:
        find first item
            where item.company eq cocode
              and item.i-no    eq report.key-05
            no-lock no-error.
            
        if po-ordl.i-no ne report.key-05 and
           report.key-07 ne ""           then do:
           
          find first item
              where item.company eq cocode
                and item.i-no    eq po-ordl.i-no
              no-lock no-error. 
           
          if po-ordl.cons-uom ne "EA" then
            run sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                                    po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    v-cons-qty, output v-cons-qty).
        
          find first item
              where item.company eq cocode
                and item.i-no    eq report.key-05
              no-lock no-error.
            
          if "EA" ne report.key-07 then
            run sys/ref/convquom.p ("EA", report.key-07, item.basis-w,
                                    po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    v-cons-qty, output v-cons-qty).
        end.
      end.
 
      else do:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq po-ordl.i-no
            no-lock no-error.
        if po-ordl.cons-uom ne "EA" then
          run sys/ref/convquom.p(po-ordl.cons-uom, "EA", 0, 0, 0, 0, 
                                 v-cons-qty, output v-cons-qty).
      end.      

      assign
       v-ord[4]  = v-ord[4] + v-cons-qty
       v-balance = v-cons-qty.

      ASSIGN v-trans-date = ? 
              v-trans-qty = 0 .
      
     IF po-ordl.item-type THEN
         FIND FIRST b-rm-rcpth
          where b-rm-rcpth.company    eq cocode
            and b-rm-rcpth.po-no      eq trim(string(po-ordl.po-no,">>>>>9"))
            and b-rm-rcpth.i-no       eq po-ordl.i-no            
            and (b-rm-rcpth.rita-code eq "R" or b-rm-rcpth.rita-code eq "A")
          NO-LOCK NO-ERROR. 
         FIND FIRST b-rm-rdtlh
          where b-rm-rdtlh.r-no eq b-rm-rcpth.r-no  
            AND b-rm-rdtlh.job-no  EQ po-ordl.job-no
            AND b-rm-rdtlh.job-no2 EQ po-ordl.job-no2
            AND b-rm-rdtlh.s-num   EQ po-ordl.s-num
          NO-LOCK NO-ERROR.
     
     IF AVAIL b-rm-rcpth THEN DO:
        bf-v-cons-qty = b-rm-rdtlh.qty.
        
        if po-ordl.i-no ne report.key-05 and
           report.key-07 ne ""           then do:
           
          find first item
              where item.company eq cocode
                and item.i-no    eq po-ordl.i-no
              no-lock no-error. 
           
          if po-ordl.cons-uom ne "EA" then
            run sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                                    po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    bf-v-cons-qty, output bf-v-cons-qty).
        
          find first item
              where item.company eq cocode
                and item.i-no    eq report.key-05
              no-lock no-error.
            
          if "EA" ne report.key-07 then
            run sys/ref/convquom.p ("EA", report.key-07, item.basis-w,
                                    po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    bf-v-cons-qty, output bf-v-cons-qty).
        end.
        ASSIGN v-amount = bf-v-cons-qty *  b-rm-rdtlh.cost
               v-uom = b-rm-rcpth.pur-uom.

              v-trans-date = b-rm-rcpth.trans-date .
              v-trans-qty  = b-rm-rdtlh.qty .

        ASSIGN v-qty[4]  = v-qty[4] + bf-v-cons-qty
               v-balance = v-balance - bf-v-cons-qty
               v-amt[4] = v-amt[4] + v-amount.
        ASSIGN v-first-rec = RECID(b-rm-rdtlh) .

      END. /* avail buffer */

      ELSE DO:
          FIND FIRST b-fg-rcpth
          where b-fg-rcpth.company    eq cocode
            and b-fg-rcpth.i-no       eq report.key-05
            and b-fg-rcpth.po-no      eq trim(string(po-ordl.po-no,">>>>>9"))
            and b-fg-rcpth.job-no     eq po-ordl.job-no
            and b-fg-rcpth.job-no2    eq po-ordl.job-no2
            and (b-fg-rcpth.rita-code eq "R" or
                 b-fg-rcpth.rita-code eq "A")
          NO-LOCK NO-ERROR.

          FIND FIRST b-fg-rdtlh
          where b-fg-rdtlh.r-no eq b-fg-rcpth.r-no
          NO-LOCK NO-ERROR.
           IF AVAIL b-fg-rcpth THEN do:
               assign
                   v-qty[4]  = v-qty[4] + b-fg-rdtlh.qty
                   v-balance = v-balance - b-fg-rdtlh.qty
                   v-amount  = b-fg-rdtlh.qty * b-fg-rdtlh.cost /
                               (IF b-fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1)
                   v-amt[4]  = v-amt[4] + v-amount
                   v-uom     = "M".

               ASSIGN v-first-rec = RECID(b-fg-rdtlh) 
                   v-trans-date =  b-fg-rcpth.trans-date 
                   v-trans-qty  =  b-fg-rdtlh.qty  .
           END.

      END.     /* not avail buffer b-rm-rdtlh */

    ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
     
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "po"    THEN cVarValue = string(po-ord.po-no,">>>>>>") .
                         WHEN "vend"   THEN cVarValue = string(po-ord.vend-no,"x(8)").
                         WHEN "item"   THEN cVarValue = STRING(report.key-05,"x(15)").
                         WHEN "desc"  THEN cVarValue = IF AVAIL ITEM THEN STRING(item.i-name,"x(25)") ELSE IF AVAIL itemfg THEN STRING(itemfg.i-name,"x(25)") ELSE "".
                         WHEN "cont"   THEN cVarValue = STRING(po-ordl.vend-i-no,"x(15)") .
                         WHEN "pack"  THEN cVarValue = IF AVAIL itemfg THEN STRING(itemfg.prod-notes,"x(10)") ELSE "" .
                         WHEN "qty-ord"   THEN cVarValue = STRING(v-cons-qty,"->,>>>,>>9.99") .
                         WHEN "tran-date"  THEN cVarValue = IF v-trans-date NE ? THEN STRING(v-trans-date,"99/99/9999") ELSE "" .
                         WHEN "tran-qty"  THEN cVarValue = STRING(v-trans-qty,"->>>,>>9.99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amount,"->>,>>9.99") .
                         WHEN "uom"  THEN cVarValue = STRING(v-uom,"x(8)") .
                         WHEN "bal-due"  THEN cVarValue = STRING(v-balance,"->>>,>>9.99") .
                         
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

      /*display po-ord.po-no
              po-ord.vend-no
              report.key-05
              item.i-name when avail item
              itemfg.i-name when avail itemfg @ item.i-name
              po-ordl.vend-i-no
              itemfg.prod-notes when avail itemfg
              v-cons-qty
          with frame main.*/
      excel-skip = NO.

      if po-ordl.item-type then
      for each rm-rcpth
          where rm-rcpth.company    eq cocode
            and rm-rcpth.po-no      eq trim(string(po-ordl.po-no,">>>>>9"))
            and rm-rcpth.i-no       eq po-ordl.i-no            
            /*and rm-rcpth.job-no     eq po-ordl.job-no
            and rm-rcpth.job-no2    eq po-ordl.job-no2*/
            and (((rm-rcpth.rita-code eq "R" or rm-rcpth.rita-code eq "A") /*and
                  item.i-code eq "R") or
                 (rm-rcpth.rita-code eq "I" and item.i-code eq "E"*/))
          no-lock,
          each rm-rdtlh
          where rm-rdtlh.r-no eq rm-rcpth.r-no  
            AND rm-rdtlh.job-no  EQ po-ordl.job-no
            AND rm-rdtlh.job-no2 EQ po-ordl.job-no2
            AND rm-rdtlh.s-num   EQ po-ordl.s-num
           AND RECID(rm-rdtlh) NE v-first-rec 
          no-lock

          break by rm-rcpth.trans-date by rm-rcpth.r-no:

        if not first(rm-rcpth.trans-date) then do:
          v-first[4] = "".
          /*down with frame main.
          clear frame main.*/
        end.
        
        v-cons-qty = rm-rdtlh.qty.
        
        if po-ordl.i-no ne report.key-05 and
           report.key-07 ne ""           then do:
           
          find first item
              where item.company eq cocode
                and item.i-no    eq po-ordl.i-no
              no-lock no-error. 
           
          if po-ordl.cons-uom ne "EA" then
            run sys/ref/convquom.p (po-ordl.cons-uom, "EA", item.basis-w,
                                    po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    v-cons-qty, output v-cons-qty).
        
          find first item
              where item.company eq cocode
                and item.i-no    eq report.key-05
              no-lock no-error.
            
          if "EA" ne report.key-07 then
            run sys/ref/convquom.p ("EA", report.key-07, item.basis-w,
                                    po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                                    v-cons-qty, output v-cons-qty).
        end.
        ASSIGN v-amount = v-cons-qty *  rm-rdtlh.cost
               v-uom = rm-rcpth.pur-uom.

        ASSIGN v-qty[4]  = v-qty[4] + v-cons-qty
               v-balance = v-balance - v-cons-qty
               v-amt[4] = v-amt[4] + v-amount.

        /*display rm-rcpth.trans-date
                v-cons-qty @ rm-rdtlh.qty
                v-amount v-uom
            with frame main.*/
        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "po"    THEN cVarValue = "" .
                         WHEN "vend"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue =  "" .
                         WHEN "cont"   THEN cVarValue = "" .
                         WHEN "pack"  THEN cVarValue =  "" .
                         WHEN "qty-ord"   THEN cVarValue = "" .
                         WHEN "tran-date"  THEN cVarValue = STRING(rm-rcpth.trans-date,"99/99/9999") .
                         WHEN "tran-qty"  THEN cVarValue = STRING(v-cons-qty,"->>>,>>9.99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amount,"->>,>>9.99") .
                         WHEN "uom"  THEN cVarValue = STRING(v-uom,"x(8)") .
                         WHEN "bal-due"  THEN cVarValue = /*STRING(v-balance,"->>>,>>9.99")*/ "" .
                         
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
       
      end.

      else
      for each fg-rcpth
          where fg-rcpth.company    eq cocode
            and fg-rcpth.i-no       eq report.key-05
            and fg-rcpth.po-no      eq trim(string(po-ordl.po-no,">>>>>9"))
            and fg-rcpth.job-no     eq po-ordl.job-no
            and fg-rcpth.job-no2    eq po-ordl.job-no2
            and (fg-rcpth.rita-code eq "R" or
                 fg-rcpth.rita-code eq "A")
          no-lock,

          each fg-rdtlh
          where fg-rdtlh.r-no eq fg-rcpth.r-no
          AND RECID(fg-rdtlh) NE v-first-rec
          no-lock

          break by fg-rcpth.trans-date BY fg-rdtlh.trans-time by fg-rcpth.r-no:

        if not first(fg-rcpth.trans-date) then do:
          v-first[4] = "".
          /*down with frame main.
          clear frame main.*/
        end.

        assign
         v-qty[4]  = v-qty[4] + fg-rdtlh.qty
         v-balance = v-balance - fg-rdtlh.qty
         v-amount  = fg-rdtlh.qty * fg-rdtlh.cost /
                     (IF fg-rcpth.pur-uom EQ "M" THEN 1000 ELSE 1)
         v-amt[4]  = v-amt[4] + v-amount
         v-uom     = "M".
         
        /*display fg-rcpth.trans-date @ rm-rcpth.trans-date
                fg-rdtlh.qty        @ rm-rdtlh.qty
                v-amount v-uom
            with frame main.*/
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "po"    THEN cVarValue = "" .
                         WHEN "vend"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue =  "" .
                         WHEN "cont"   THEN cVarValue = "" .
                         WHEN "pack"  THEN cVarValue =  "" .
                         WHEN "qty-ord"   THEN cVarValue = "" .
                         WHEN "tran-date"  THEN cVarValue = STRING(fg-rcpth.trans-date,"99/99/9999") .
                         WHEN "tran-qty"  THEN cVarValue = STRING(fg-rdtlh.qty,"->>>,>>9.99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amount,"->>,>>9.99") .
                         WHEN "uom"  THEN cVarValue = STRING(v-uom,"x(8)") .
                         WHEN "bal-due"  THEN cVarValue = /*STRING(v-balance,"->>>,>>9.99")*/ "" .
                         
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

        
      end.
      
     /* display v-balance with frame main.

      IF tb_excel THEN
        RUN excel-2-proc(INPUT v-balance).

      down with frame main.
      clear frame main.*/

      v-bal[4] = v-bal[4] + v-balance.
      
      if last-of(report.key-04) then do:
        put skip(1).

        if report.key-04 ne ""         and
           report.key-04 ne v-first[4] then do:

         /* display "Control # Totals" @ item.i-name
                  v-ord[4]           @ v-cons-qty
                  v-qty[4]           @ rm-rdtlh.qty
                  v-bal[4]           @ v-balance
                  v-amt[4]           @ v-amount
              with frame main.
          down with frame main.*/

           ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "po"    THEN cVarValue = "" .
                         WHEN "vend"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue =  "" .
                         WHEN "cont"   THEN cVarValue = "" .
                         WHEN "pack"  THEN cVarValue =  "" .
                         WHEN "qty-ord"   THEN cVarValue = STRING(v-ord[4],"->,>>>,>>9.99") .
                         WHEN "tran-date"  THEN cVarValue = "" .
                         WHEN "tran-qty"  THEN cVarValue = STRING(v-qty[4],"->>>,>>9.99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amt[4],"->>,>>9.99") .
                         WHEN "uom"  THEN cVarValue = "" .
                         WHEN "bal-due"  THEN cVarValue = STRING(v-bal[4],"->>>,>>9.99")  .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "    Control # Totals "  substring(cDisplay,22,300) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED 'Control # Totals ,' 
                       substring(cExcelDisplay,4,300) SKIP.
             END.

          put skip(1).

          /*IF tb_excel THEN
             RUN excel-3-proc(INPUT "Control # Totals",
                              INPUT v-ord[4], INPUT v-qty[4],
                              INPUT v-bal[4], INPUT v-amt[4]).

          if not last-of(report.key-03) then put skip(1).*/
        end.

        assign
         v-ord[3] = v-ord[3] + v-ord[4]
         v-qty[3] = v-qty[3] + v-qty[4]
         v-bal[3] = v-bal[3] + v-bal[4]
         v-amt[3] = v-amt[3] + v-amt[4]
         v-ord[4] = 0
         v-qty[4] = 0
         v-bal[4] = 0
         v-amt[4] = 0.
      end.

      if last-of(report.key-03) then do:
        if report.key-03 ne ""         and
           report.key-03 ne v-first[3] then do:

         /* display "Item Totals" @ item.i-name
                  v-ord[3]      @ v-cons-qty
                  v-qty[3]      @ rm-rdtlh.qty
                  v-bal[3]      @ v-balance
                  v-amt[3]      @ v-amount
              with frame main.
          down with frame main.*/

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "po"    THEN cVarValue = "" .
                         WHEN "vend"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue =  "" .
                         WHEN "cont"   THEN cVarValue = "" .
                         WHEN "pack"  THEN cVarValue =  "" .
                         WHEN "qty-ord"   THEN cVarValue = STRING(v-ord[3],"->,>>>,>>9.99") .
                         WHEN "tran-date"  THEN cVarValue = "" .
                         WHEN "tran-qty"  THEN cVarValue = STRING(v-qty[3],"->>>,>>9.99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amt[3],"->>,>>9.99") .
                         WHEN "uom"  THEN cVarValue = "" .
                         WHEN "bal-due"  THEN cVarValue = STRING(v-bal[3],"->>>,>>9.99")  .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT str-line SKIP .
            PUT UNFORMATTED "    Item # Totals "  substring(cDisplay,19,300) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED 'Item # Totals ,' 
                       substring(cExcelDisplay,4,300) SKIP.
             END.

          put skip(1).

          /*IF tb_excel THEN
             RUN excel-3-proc(INPUT "Item Totals",
                              INPUT v-ord[3], INPUT v-qty[3],
                              INPUT v-bal[3], INPUT v-amt[3]).*/

          if not last-of(report.key-02) then put skip(1).
        end.

        assign
         v-ord[2] = v-ord[2] + v-ord[3]
         v-qty[2] = v-qty[2] + v-qty[3]
         v-bal[2] = v-bal[2] + v-bal[3]
         v-amt[2] = v-amt[2] + v-amt[3]
         v-ord[3] = 0
         v-qty[3] = 0
         v-bal[3] = 0
         v-amt[3] = 0.
      end.

      if last-of(report.key-02) then do:
        if report.key-02 ne ""         and
           report.key-02 ne v-first[2] then do:

          /*display "Vendor Totals" @ item.i-name
                  v-ord[2]        @ v-cons-qty
                  v-qty[2]        @ rm-rdtlh.qty
                  v-bal[2]        @ v-balance
                  v-amt[2]        @ v-amount
              with frame main.
          down with frame main.*/

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

        DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "po"    THEN cVarValue = "" .
                         WHEN "vend"   THEN cVarValue = "".
                         WHEN "item"   THEN cVarValue = "".
                         WHEN "desc"  THEN cVarValue =  "" .
                         WHEN "cont"   THEN cVarValue = "" .
                         WHEN "pack"  THEN cVarValue =  "" .
                         WHEN "qty-ord"   THEN cVarValue = STRING(v-ord[2],"->,>>>,>>9.99") .
                         WHEN "tran-date"  THEN cVarValue = "" .
                         WHEN "tran-qty"  THEN cVarValue = STRING(v-qty[2],"->>>,>>9.99") .
                         WHEN "amt"   THEN cVarValue = STRING(v-amt[2],"->>,>>9.99") .
                         WHEN "uom"  THEN cVarValue = "" .
                         WHEN "bal-due"  THEN cVarValue = STRING(v-bal[2],"->>>,>>9.99")  .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "    Vender # Totals "  substring(cDisplay,21,300) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED 'Vender # Totals ,' 
                       substring(cExcelDisplay,4,300) SKIP.
             END.

          put skip(2).

          /*IF tb_excel THEN
             RUN excel-3-proc(INPUT "Vendor Totals",
                              INPUT v-ord[2], INPUT v-qty[2],
                              INPUT v-bal[2], INPUT v-amt[2]).*/
        end.

        assign
         v-ord[2] = 0
         v-qty[2] = 0
         v-bal[2] = 0
         v-amt[2] = 0   .
      end.

      v-first = "".

      delete report.
    end.

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
