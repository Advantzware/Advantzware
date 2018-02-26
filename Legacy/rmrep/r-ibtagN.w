&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&SCOPED-DEFINE WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: r-ibtagN.w

  Description:

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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecdt.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

 ASSIGN
 cocode = gcompany
 locode = gloc.

DEFINE VARIABLE v-print-fmt AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOGICAL.
DEFINE VARIABLE ls-fax-file AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-roll-multp AS DEC DECIMALS 4 NO-UNDO.

DEFINE TEMP-TABLE tt-rm-bin NO-UNDO LIKE rm-bin
                                 FIELD trans-date LIKE rm-rcpth.trans-date
                                 FIELD tag2 LIKE rm-rdtlh.tag2.
    

DEFINE STREAM excel.

DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE cTextListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldLength AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldType AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumnLength AS INTEGER NO-UNDO.
DEFINE VARIABLE str-line AS CHARACTER FORMAT "x(300)" NO-UNDO.
DEFINE VARIABLE cTextListToDefault AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-lstdt AS   CHARACTER   NO-UNDO.
DEFINE VARIABLE lv-fistdt AS   CHARACTER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO .
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEFINE VARIABLE lTagFormat AS LOGICAL NO-UNDO .

ASSIGN cTextListToSelect = "Whse,Item,Description,Bin,Tag,Rolls," +
                           "Last Trans Date,Quantity,Unit Cost,Cost Value,MSF,Tons,Cost/MSF,Vendor Tag,Vendor Po#,Cert/Lot/Mill#,Vendor,Last Recd,Caliper," +
                           "Wt/Msf,PO GL Account,Item Name,Job#,Sheet Size"
       cFieldListToSelect = "tt-rm-bin.loc,tt-rm-bin.i-no,v-itemname,loc-bin,tag,rolls," +
                            "trans-date,qty,v-cost,v-total,v-msf,v-tons,v-costMSF,cVendTag,cVendPo,crtlot,cVendCode,cLstRcd,cali," +
                            "wt-msf,po-gl-act,cItemName,job-no,sht-size"
       cFieldLength = "5,10,30,8,22,5," + "15,16,10,13,11,11,11,30,10,30,8,9,7," + "6,25,30,10,20"
       cFieldType = "c,c,c,c,c,i," + "c,i,i,i,i,i,i,c,i,c,c,c,i," + "i,c,c,c,c"
       .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Whse,Item,Description,Bin,Tag," +
                           "Last Trans Date,Quantity,Unit Cost,Cost Value,Item Name" .

RUN sys/ref/nk1look.p (INPUT cocode, "TagFormat", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
lTagFormat = LOGICAL(cRtnChar) NO-ERROR .
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE PROCEDURE-TYPE Window
&SCOPED-DEFINE DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&SCOPED-DEFINE FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&SCOPED-DEFINE ENABLED-OBJECTS RECT-6 RECT-7 as-of-date begin_rm-no ~
end_rm-no begin_whs end_whs begin_procat end_procat begin_mat-type ~
end_mat-type begin_date end_date rd_item tb_zero-bal tb_total-rolls tb_subt ~
tb_grdt tb_detail sl_avail Btn_Def Btn_Add sl_selected Btn_Remove btn_Up btn_down ~
lv-ornt lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel ~
fi_file btn-ok btn-cancel 
&SCOPED-DEFINE DISPLAYED-OBJECTS as-of-date begin_rm-no end_rm-no begin_whs ~
end_whs begin_procat end_procat begin_mat-type end_mat-type begin_date ~
end_date rd_item lbl_itm-code-2 tb_zero-bal tb_total-rolls tb_subt tb_grdt ~
tb_detail sl_avail sl_selected lv-ornt lines-per-page rd-dest lv-font-no lv-font-name ~
td-show-parm tb_excel tb_runExcel fi_file 

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPrepareCSV B-table-Win 
FUNCTION fPrepareCSV RETURNS CHARACTER
  (ipBal AS CHARACTER)  FORWARD.

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

DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "As of" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .95 NO-UNDO.

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Beginning Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_mat-type AS CHARACTER FORMAT "X":U 
     LABEL "Beginning Material Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_procat AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning  Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_rm-no AS CHARACTER FORMAT "X(10)":U 
     LABEL "Beginning Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_whs AS CHARACTER FORMAT "X(5)":U 
     LABEL "Beginning Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Receipt Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_mat-type AS CHARACTER FORMAT "X":U INITIAL "z" 
     LABEL "Ending Material Type" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_procat AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Category" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_rm-no AS CHARACTER FORMAT "X(10)":U INITIAL "zzzzzzzzzz" 
     LABEL "Ending Item#" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE end_whs AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Ending Warehouse" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-ibtag.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_itm-code-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Item Code?" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .95 NO-UNDO.

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
     SIZE 20 BY 6.67 NO-UNDO.

DEFINE VARIABLE rd_item AS CHARACTER INITIAL "Both" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Estimated", "Estimated",
"Real", "Real",
"Both", "Both"
     SIZE 33 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 11.67.

DEFINE VARIABLE sl_avail AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE sl_selected AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 31 BY 5.71 NO-UNDO.

DEFINE VARIABLE tb_detail AS LOGICAL INITIAL NO 
     LABEL "Show Detail?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES 
     LABEL "Export To Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_grdt AS LOGICAL INITIAL NO 
     LABEL "Print Grand Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .95 NO-UNDO.

DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL NO 
     LABEL "Auto Run Excel?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81
     BGCOLOR 3  NO-UNDO.

DEFINE VARIABLE tb_subt AS LOGICAL INITIAL YES 
     LABEL "Print Sub Totals?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tb_tagask AS LOGICAL INITIAL NO 
     LABEL "Print * on Tag?" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY .95 NO-UNDO.

DEFINE VARIABLE tb_total-rolls AS LOGICAL INITIAL NO 
     LABEL "Print Total Rolls?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.4 BY .95 NO-UNDO.

DEFINE VARIABLE tb_zero-bal AS LOGICAL INITIAL NO 
     LABEL "Include Zero Balances?" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .95 NO-UNDO.

DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO 
     LABEL "Show Parameters?" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     as-of-date AT ROW 1.52 COLUMN 72 COLON-ALIGNED
     begin_rm-no AT ROW 2.71 COLUMN 28 COLON-ALIGNED HELP
          "Enter Beginning Item Number"
     end_rm-no AT ROW 2.71 COLUMN 72 COLON-ALIGNED HELP
          "Enter Ending Item number"
     begin_whs AT ROW 3.91 COLUMN 28 COLON-ALIGNED HELP
          "Enter Beginng Warehouse"
     end_whs AT ROW 3.91 COLUMN 72 COLON-ALIGNED HELP
          "Enter Endng Warehouse"
     begin_procat AT ROW 5.1 COLUMN 28 COLON-ALIGNED HELP
          "Enter Begining Category"
     end_procat AT ROW 5.1 COLUMN 72 COLON-ALIGNED HELP
          "Enter Ending Category"
     begin_mat-type AT ROW 6.29 COLUMN 28 COLON-ALIGNED HELP
          "Enter Beginning Material Type"
     end_mat-type AT ROW 6.29 COLUMN 72 COLON-ALIGNED HELP
          "Enter ending Material Type"
     begin_date AT ROW 7.48 COLUMN 28 COLON-ALIGNED HELP
          "Enter Beginning Receipt Date"
     end_date AT ROW 7.48 COLUMN 72 COLON-ALIGNED HELP
          "Enter Ending Receipt Date"
     rd_item AT ROW 8.57 COLUMN 51.4 NO-LABEL WIDGET-ID 52
     lbl_itm-code-2 AT ROW 8.62 COLUMN 36.2 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     tb_zero-bal AT ROW 9.76 COLUMN 30
     tb_total-rolls AT ROW 9.76 COLUMN 58.2
     tb_tagask AT ROW 9.76 COLUMN 64
     tb_subt AT ROW 10.71 COLUMN 30
     tb_grdt AT ROW 10.71 COLUMN 58
     tb_detail AT ROW 11.67 COLUMN 30 WIDGET-ID 46
     sl_avail AT ROW 13.62 COLUMN 7 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 13.67 COLUMN 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     Btn_Add AT ROW 14.81 COLUMN 41 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     sl_selected AT ROW 13.62 COLUMN 61 NO-LABEL WIDGET-ID 28
     Btn_Remove AT ROW 15.95 COLUMN 41 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 17.10 COLUMN 41 WIDGET-ID 40
     btn_down AT ROW 18.24 COLUMN 41 WIDGET-ID 42
     lv-ornt AT ROW 20.52 COLUMN 31 NO-LABEL
     lines-per-page AT ROW 20.52 COLUMN 84 COLON-ALIGNED
     rd-dest AT ROW 21.24 COLUMN 6 NO-LABEL
     lv-font-no AT ROW 21.95 COLUMN 35 COLON-ALIGNED
     lv-font-name AT ROW 22.91 COLUMN 29 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 24.1 COLUMN 31
     tb_excel AT ROW 25.52 COLUMN 67 RIGHT-ALIGNED
     tb_runExcel AT ROW 25.52 COLUMN 90.8 RIGHT-ALIGNED
     fi_file AT ROW 26.48 COLUMN 45 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 28.62 COLUMN 23
     btn-cancel AT ROW 28.62 COLUMN 62
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 12.91 COLUMN 3 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COLUMN 5
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 20.29 COLUMN 3
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 12.91 COLUMN 60.4 WIDGET-ID 44
     RECT-6 AT ROW 20.05 COLUMN 1
     RECT-7 AT ROW 1 COLUMN 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COLUMN 1 ROW 1
         SIZE 96.4 BY 29.05.


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
         TITLE              = "RM Inventory By Bin/Tag"
         HEIGHT             = 29.05
         WIDTH              = 98.6
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
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
       as-of-date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_mat-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_mat-type:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_procat:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_rm-no:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       end_whs:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       fi_file:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR FILL-IN lbl_itm-code-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       lbl_itm-code-2:PRIVATE-DATA IN FRAME FRAME-A     = 
                "rd_item".

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_grdt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_runExcel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_runExcel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_subt:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_tagask IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_tagask:HIDDEN IN FRAME FRAME-A           = TRUE
       tb_tagask:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_total-rolls:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_zero-bal:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&SCOPED-DEFINE SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* RM Inventory By Bin/Tag */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* RM Inventory By Bin/Tag */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME as-of-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL as-of-date C-Win
ON LEAVE OF as-of-date IN FRAME FRAME-A /* As of */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Receipt Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_mat-type C-Win
ON LEAVE OF begin_mat-type IN FRAME FRAME-A /* Beginning Material Type */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_procat C-Win
ON LEAVE OF begin_procat IN FRAME FRAME-A /* Beginning  Category */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_rm-no C-Win
ON LEAVE OF begin_rm-no IN FRAME FRAME-A /* Beginning Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME begin_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_whs C-Win
ON LEAVE OF begin_whs IN FRAME FRAME-A /* Beginning Warehouse */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
   APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok C-Win
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.
  SESSION:SET-WAIT-STATE("general").
  RUN GetSelectionList.
  RUN run-report. 

  STATUS DEFAULT "Processing Complete". 
  SESSION:SET-WAIT-STATE("").

  CASE rd-dest:
       WHEN 1 THEN RUN output-to-printer.
       WHEN 2 THEN RUN output-to-screen.
       WHEN 3 THEN RUN output-to-file.
       WHEN 4 THEN DO:
           /*run output-to-fax.*/
           {custom/asifax.i &TYPE= ''
                            &begin_cust= "begin_procat"
                            &END_cust= "begin_procat" 
                            &fax-subject=c-win:TITLE
                            &fax-body=c-win:TITLE
                            &fax-file=list-name }
       END. 
       WHEN 5 THEN DO:
           IF is-xprint-form THEN DO:
              {custom/asimail.i &TYPE = ''
                             &begin_cust= "begin_procat"
                             &END_cust= "begin_procat"
                             &mail-subject=c-win:TITLE
                             &mail-body=c-win:TITLE
                             &mail-file=list-name }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust="begin_procat"
                                  &END_cust="begin_procat"
                                  &mail-subject=c-win:TITLE
                                  &mail-body=c-win:TITLE
                                  &mail-file=list-name }
           END.
       END.
       WHEN 6 THEN RUN OUTPUT-to-port.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&SCOPED-DEFINE SELF-NAME Btn_Add
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

&SCOPED-DEFINE SELF-NAME Btn_Def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Def C-Win
ON CHOOSE OF Btn_Def IN FRAME FRAME-A /* Default */
DO:
  DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.

  RUN DisplaySelectionDefault.  /* task 04041406 */ 
  RUN DisplaySelectionList2 .
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME btn_down
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_down C-Win
ON CHOOSE OF btn_down IN FRAME FRAME-A /* Move Down */
DO:
  RUN Move-Field ("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME Btn_Remove
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


&SCOPED-DEFINE SELF-NAME btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Up C-Win
ON CHOOSE OF btn_Up IN FRAME FRAME-A /* Move Up */
DO:
  RUN Move-Field ("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Receipt Date */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_mat-type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_mat-type C-Win
ON LEAVE OF end_mat-type IN FRAME FRAME-A /* Ending Material Type */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_procat C-Win
ON LEAVE OF end_procat IN FRAME FRAME-A /* Ending Category */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_rm-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_rm-no C-Win
ON LEAVE OF end_rm-no IN FRAME FRAME-A /* Ending Item# */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME end_whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_whs C-Win
ON LEAVE OF end_whs IN FRAME FRAME-A /* Ending Warehouse */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME fi_file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_file C-Win
ON LEAVE OF fi_file IN FRAME FRAME-A /* If Yes, File Name */
DO:
     ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page C-Win
ON LEAVE OF lines-per-page IN FRAME FRAME-A /* Lines Per Page */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no C-Win
ON HELP OF lv-font-no IN FRAME FRAME-A /* Font */
DO:
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

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


&SCOPED-DEFINE SELF-NAME lv-ornt
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


&SCOPED-DEFINE SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest C-Win
ON VALUE-CHANGED OF rd-dest IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME sl_avail
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


&SCOPED-DEFINE SELF-NAME sl_selected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl_selected C-Win
ON DEFAULT-ACTION OF sl_selected IN FRAME FRAME-A
DO:
   DO i = 1 TO {&SELF-NAME}:NUM-ITEMS:
    IF {&SELF-NAME}:IS-SELECTED(i) THEN DO:
       ASSIGN ldummy = sl_Avail:ADD-LAST({&SELF-NAME}:SCREEN-VALUE)
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


&SCOPED-DEFINE SELF-NAME tb_excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_excel C-Win
ON VALUE-CHANGED OF tb_excel IN FRAME FRAME-A /* Export To Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_grdt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_grdt C-Win
ON VALUE-CHANGED OF tb_grdt IN FRAME FRAME-A /* Print Grand Totals? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_runExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_runExcel C-Win
ON VALUE-CHANGED OF tb_runExcel IN FRAME FRAME-A /* Auto Run Excel? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_subt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_subt C-Win
ON VALUE-CHANGED OF tb_subt IN FRAME FRAME-A /* Print Sub Totals? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_tagask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_tagask C-Win
ON VALUE-CHANGED OF tb_tagask IN FRAME FRAME-A /* Print * on Tag? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_total-rolls
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_total-rolls C-Win
ON VALUE-CHANGED OF tb_total-rolls IN FRAME FRAME-A /* Print Total Rolls? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME tb_zero-bal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_zero-bal C-Win
ON VALUE-CHANGED OF tb_zero-bal IN FRAME FRAME-A /* Include Zero Balances? */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&SCOPED-DEFINE SELF-NAME td-show-parm
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
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  as-of-date = TODAY.

  FIND FIRST uom  NO-LOCK WHERE
       uom.uom = "ROLL"
       NO-ERROR.

  IF AVAILABLE uom THEN
  DO:
     v-roll-multp = uom.mult.
     RELEASE uom.
  END.
  RUN DisplaySelectionList.
  RUN enable_UI.
  
  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    as-of-date:SCREEN-VALUE = STRING(TODAY).
    APPLY "entry" TO as-of-date.
  END.

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
 DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:
     
     RETURN.
  END.

  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    /* (IF cListContents = "" THEN ""  ELSE ",") +
                     ENTRY(iCount,cTextListToSelect) + "," +
                     ENTRY(1,cFieldListToSelect)
                     paris */
                     
                    (IF cListContents EQ "" THEN ""  ELSE ",") +
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
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE cTmpList AS CHARACTER NO-UNDO.

  IF NUM-ENTRIES(cTextListToSelect) NE NUM-ENTRIES(cFieldListToSelect) THEN DO:
    RETURN.
  END.
        
  EMPTY TEMP-TABLE ttRptList.

  DO iCount = 1 TO NUM-ENTRIES(cTextListToSelect):

     cListContents = cListContents +
                    (IF cListContents EQ "" THEN ""  ELSE ",") +
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
  DEFINE VARIABLE cListContents AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  
  DO iCount = 1 TO NUM-ENTRIES(cTextListToDefault):

     cListContents = cListContents +                   
                    (IF cListContents EQ "" THEN ""  ELSE ",") +
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
  DISPLAY as-of-date begin_rm-no end_rm-no begin_whs end_whs begin_procat 
          end_procat begin_mat-type end_mat-type begin_date end_date rd_item 
          lbl_itm-code-2 tb_zero-bal tb_total-rolls tb_subt tb_grdt tb_detail 
          sl_avail sl_selected lv-ornt lines-per-page rd-dest lv-font-no lv-font-name 
          td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 as-of-date begin_rm-no end_rm-no begin_whs end_whs 
         begin_procat end_procat begin_mat-type end_mat-type begin_date 
         end_date rd_item tb_zero-bal tb_total-rolls tb_subt tb_grdt tb_detail 
         sl_avail Btn_Def Btn_Add sl_selected Btn_Remove btn_Up btn_down lv-ornt 
         lines-per-page rd-dest lv-font-no td-show-parm tb_excel tb_runExcel 
         fi_file btn-ok btn-cancel 
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
    FIND FIRST ttRptList NO-LOCK WHERE ttRptList.TextList = ENTRY(i,cTmpList)  NO-ERROR.     
  
    CREATE ttRptSelected.
    ASSIGN ttRptSelected.TextList =  ENTRY(i,cTmpList)
           ttRptSelected.FieldList = ttRptList.FieldList
           ttRptSelected.FieldLength = INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cTmpList)), cFieldLength))
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
 DEFINE INPUT PARAMETER MOVE AS CHARACTER NO-UNDO.

  DO i = 1 TO sl_selected:NUM-ITEMS IN FRAME {&FRAME-NAME}
      WITH FRAME {&FRAME-NAME}:
    IF sl_selected:IS-SELECTED(i) THEN
    DO:
      IF MOVE EQ "Down" AND i NE sl_selected:NUM-ITEMS THEN
      ASSIGN
        ldummy = sl_selected:INSERT(sl_selected:SCREEN-VALUE,i + 2)
        ldummy = sl_selected:DELETE(i)
        sl_selected:SCREEN-VALUE = sl_selected:ENTRY(i + 1)
        .
      ELSE
      IF MOVE EQ "Up" AND i NE 1 THEN
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
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.  */
     
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
  
  RUN custom/prntproc.p (list-name,INTEGER(lv-font-no),lv-ornt).
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
  RUN scr-rpt.w (list-name,c-win:TITLE,INTEGER(lv-font-no),lv-ornt). /* open file-name, title */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rm-mkbin C-Win 
PROCEDURE rm-mkbin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE BUFFER b-rm-bin FOR rm-bin.

DEFINE VARIABLE v-r-qty AS   DECIMAL    NO-UNDO.
DEFINE VARIABLE v-i-qty AS   DECIMAL    NO-UNDO.
DEFINE VARIABLE v-t-qty AS   DECIMAL    NO-UNDO.
DEFINE VARIABLE ld-qty  AS   DECIMAL    NO-UNDO.
DEFINE VARIABLE ld-cst  AS   DECIMAL    NO-UNDO.
DEFINE VARIABLE lv-uom  AS   CHARACTER  NO-UNDO.

  IF as-of-date GE TODAY THEN
  FOR EACH rm-bin NO-LOCK
      WHERE rm-bin.company EQ item.company
        AND rm-bin.i-no    EQ item.i-no:

    CREATE tt-rm-bin.
    BUFFER-COPY rm-bin TO tt-rm-bin.
  END.

  ELSE DO:
    {rm/rmmkbin1.i as-of-date tt-}
  END.

  FOR EACH tt-rm-bin
      WHERE tt-rm-bin.company EQ item.company
        AND tt-rm-bin.i-no    EQ item.i-no:

    RELEASE rm-rcpth.
    RELEASE rm-rcpth.

    tt-rm-bin.trans-date = ?.
    tt-rm-bin.tag2 = "".

    IF TRIM(tt-rm-bin.tag) EQ "" THEN
    FOR EACH rm-rcpth NO-LOCK
        WHERE rm-rcpth.company      EQ tt-rm-bin.company
          AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
          AND rm-rcpth.rita-code    NE "S"
        USE-INDEX i-no,

        EACH rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.r-no         EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code    EQ rm-rcpth.rita-code
          AND rm-rdtlh.loc          EQ tt-rm-bin.loc
          AND rm-rdtlh.loc-bin      EQ tt-rm-bin.loc-bin
          AND rm-rdtlh.tag          EQ tt-rm-bin.tag
        USE-INDEX rm-rdtl
    
        BY rm-rcpth.trans-date
        BY rm-rcpth.r-no:

      tt-rm-bin.trans-date = rm-rcpth.trans-date.
      tt-rm-bin.tag2 = rm-rdtlh.tag2.
      LEAVE.
    END.

    ELSE
    FOR EACH rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.company      EQ tt-rm-bin.company
          AND rm-rdtlh.loc          EQ tt-rm-bin.loc
          AND rm-rdtlh.loc-bin      EQ tt-rm-bin.loc-bin
          AND rm-rdtlh.tag          EQ tt-rm-bin.tag
          AND rm-rdtlh.rita-code    NE "S"
        USE-INDEX tag,
        
        EACH rm-rcpth NO-LOCK 
        WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
          AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
          AND rm-rcpth.i-no         EQ ITEM.i-no
        USE-INDEX r-no
    
        BY rm-rcpth.trans-date
        BY rm-rcpth.r-no:

      tt-rm-bin.trans-date = rm-rcpth.trans-date.
      tt-rm-bin.tag2 = rm-rdtlh.tag2.
      LEAVE.
    END.

    IF tt-rm-bin.trans-date EQ ? THEN DO:
      FIND FIRST rm-bin NO-LOCK
          WHERE rm-bin.company EQ tt-rm-bin.company
            AND rm-bin.i-no    EQ tt-rm-bin.i-no
            AND rm-bin.loc     EQ tt-rm-bin.loc
            AND rm-bin.loc-bin EQ tt-rm-bin.loc-bin
            AND rm-bin.tag     EQ tt-rm-bin.tag
          USE-INDEX loc-bin NO-ERROR.
      tt-rm-bin.trans-date = DATE(SUBSTRING(rm-bin.rec_key,1,8)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN tt-rm-bin.trans-date = TODAY.
    END.
    

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report :
/* ------------------------------------------------ rm/rep/rm-ibtag.p 9/93 cd */
/* raw materials - inventory by bin/tag report                                */
/* -------------------------------------------------------------------------- */

DEFINE VARIABLE cDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit4 AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5 AS CHARACTER FORMAT "x(200)" NO-UNDO.

{sys/form/r-top5DL.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF BUFFER bttrmbin FOR tt-rm-bin.
DEFINE VARIABLE save_id AS RECID.
DEFINE VARIABLE v-price AS DECIMAL FORMAT "->>>>9.99".
DEFINE VARIABLE v-tot-price AS DECIMAL FORMAT "$->>,>>>,>>9.99".
DEFINE VARIABLE v-cum-qty AS DECIMAL FORMAT "->>>>>9.999".
DEFINE VARIABLE v-cum-price AS DECIMAL FORMAT "->>>,>>9.99".
DEFINE VARIABLE v-cum-qty2 AS DECIMAL FORMAT "->>>>>9.999".  /* item totals */
DEFINE VARIABLE v-gt-qty2 AS DECIMAL FORMAT "->>>,>>>,>>9.999".
DEFINE VARIABLE v-cum-price2 AS DECIMAL FORMAT "->>>,>>9.99".  /* item totals */
DEFINE VARIABLE v-cum-rolls AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-item-rolls AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-tot-rolls AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-rolls-dec AS DECIMAL DECIMALS 4 NO-UNDO.
DEFINE VARIABLE fitm LIKE rm-bin.i-no FORMAT "X(10)" INITIAL "".
DEFINE VARIABLE titm LIKE fitm INITIAL "zzzzzzzzzz".
DEFINE VARIABLE floc LIKE rm-bin.loc INITIAL "".
DEFINE VARIABLE tloc LIKE floc INITIAL "zzzzz".
DEFINE VARIABLE fcat AS CHARACTER INITIAL "".
DEFINE VARIABLE tcat LIKE fcat INITIAL "zzzzzz".
DEFINE VARIABLE TYPE AS LOGICAL FORMAT "R/E" INITIAL YES.
DEFINE VARIABLE ftyp LIKE ITEM.mat-type INITIAL "".
DEFINE VARIABLE ttyp LIKE ftyp INITIAL "z".
DEFINE VARIABLE zbal AS LOGICAL FORMAT "Y/N" INITIAL NO.
DEFINE VARIABLE v-fst-loc AS LOGICAL.
DEFINE VARIABLE v-fst-ino AS LOGICAL.
DEFINE VARIABLE v-lst-ino AS LOGICAL.
DEFINE VARIABLE v-prnt-line AS INTEGER.
DEFINE VARIABLE v-cost LIKE rm-bin.cost.
DEFINE VARIABLE psubtot AS LOGICAL INITIAL YES.
DEFINE VARIABLE pgtot AS LOGICAL INITIAL NO.
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE tagask AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-lf-qty LIKE rm-bin.qty NO-UNDO.
DEFINE VARIABLE v-MSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cum-MSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cum-MSF2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-tot-MSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Tons AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cum-tons AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cum-tons2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-tot-tons AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-CostMSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE cVendTag AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendPo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendor AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE cShtSize AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cJobNo LIKE po-ordl.job-no NO-UNDO.

DEF BUFFER bf-loadtag FOR loadtag.

    {custom/statusMsg.i "'Processing...'"} 

/* rdb 02/06/07 02050701 */
DEFINE VARIABLE chrTotCostVal AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrRmBinTag AS CHARACTER FORMAT "x(22)" NO-UNDO.
DEFINE VARIABLE vpo-gl-act AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ctype AS CHARACTER FORMAT "!"   NO-UNDO INITIAL "B".
FIND FIRST ce-ctrl NO-LOCK WHERE ce-ctrl.company EQ cocode  NO-ERROR.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 fitm    = begin_rm-no
 titm    = end_rm-no
 floc    = begin_whs
 tloc    = end_whs
 fcat    = begin_procat
 tcat    = end_procat
 ftyp    = begin_mat-type
 ttyp    = end_mat-type
 zbal    = tb_zero-bal
 psubtot = tb_subt
 pgtot   = tb_grdt
 tagask  = tb_tagask
 ctype   = substr(rd_item,1,1) .

ASSIGN str-line  = "".

DEFINE VARIABLE cslist AS CHARACTER NO-UNDO.
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
        
        IF LOOKUP(ttRptSelected.TextList, "Quantity,Cost Value,Rolls,Tons,MSF") NE 0    THEN  /* */
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
{sys/inc/outprint.i VALUE(lines-per-page)}

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

  DISPLAY "" WITH FRAME r-top.

  EMPTY TEMP-TABLE tt-rm-bin.

  IF NOT tb_detail THEN DO:
     RUN run-report-summary.
  END.
  /* ======= detial ===== */
  ELSE DO:
  
  FOR EACH ITEM NO-LOCK
      WHERE ITEM.company           EQ cocode
        AND ITEM.i-no              GE fitm
        AND ITEM.i-no              LE titm
        AND ITEM.i-no              NE ""
        AND ITEM.procat            GE fcat
        AND ITEM.procat            LE tcat
        AND ITEM.mat-type          GE ftyp
        AND ITEM.mat-type          LE ttyp
        and (item.i-code  eq ctype or ctype eq "B")  :
     

  {custom/statusMsg.i "'Processing Item # ' + string(item.i-no)"} 

    RUN rm-mkbin.

    IF zbal AND ITEM.q-onh EQ 0 AND
       NOT CAN-FIND(FIRST tt-rm-bin WHERE
       tt-rm-bin.company EQ ITEM.company AND
       tt-rm-bin.i-no EQ ITEM.i-no) THEN
       DO:
          CREATE tt-rm-bin.
          ASSIGN tt-rm-bin.company = ITEM.company
                 tt-rm-bin.i-no = ITEM.i-no
                 tt-rm-bin.trans-date = TODAY.
          RELEASE tt-rm-bin.
       END.
  END.
 
   ASSIGN 
      vpo-gl-act = "" .

  FOR EACH tt-rm-bin  NO-LOCK
      WHERE tt-rm-bin.loc          GE floc
        AND tt-rm-bin.loc          LE tloc
        AND tt-rm-bin.trans-date   GE begin_date
        AND tt-rm-bin.trans-date   LE end_date
        AND (zbal or tt-rm-bin.qty NE 0),
    
      FIRST ITEM NO-LOCK
      WHERE ITEM.company EQ tt-rm-bin.company
        AND ITEM.i-no    EQ tt-rm-bin.i-no
     
      BREAK BY tt-rm-bin.loc
            BY tt-rm-bin.i-no
            BY tt-rm-bin.loc-bin
            BY tt-rm-bin.tag

      WITH FRAME itemx:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-rm-bin.i-no)"} 

    IF FIRST-OF(tt-rm-bin.loc) OR
       LINE-COUNTER GT PAGE-SIZE - 10 THEN DO:
      IF NOT FIRST(tt-rm-bin.loc) THEN PAGE.
      v-prnt-line = 0.
    END.

    ELSE v-prnt-line = 1.
  
    lv-lstdt = "" .
    lv-fistdt = "" .
   
   IF STRING(tt-rm-bin.po-no) NE "0"  THEN DO:
     
     FOR EACH rm-rcpth  NO-LOCK                            
         WHERE rm-rcpth.company      EQ tt-rm-bin.company
           AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
           AND rm-rcpth.rita-code    NE "S"
           AND (rm-rcpth.po-no       EQ STRING(tt-rm-bin.po-no)  )
         USE-INDEX i-no                                                                                  
         BREAK BY rm-rcpth.trans-date DESCENDING:
  
       IF FIRST(rm-rcpth.trans-date) THEN 
         lv-lstdt = STRING(rm-rcpth.trans-date).
  
       IF LAST(rm-rcpth.trans-date) THEN 
         lv-fistdt = STRING(rm-rcpth.trans-date).
      
     END.
   END.
   ELSE IF tt-rm-bin.tag NE "" THEN  DO:
      DEFINE VARIABLE lReceiptFound AS LOGICAL NO-UNDO.
      lReceiptFound = NO.

      /* Find without transfers */
      FOR EACH rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.company      EQ tt-rm-bin.company
          AND rm-rdtlh.loc          EQ tt-rm-bin.loc
          AND rm-rdtlh.loc-bin      EQ tt-rm-bin.loc-bin
          AND rm-rdtlh.tag          EQ tt-rm-bin.tag
          AND rm-rdtlh.rita-code    NE "S"
          AND rm-rdtlh.rita-code    NE "T"
        USE-INDEX tag,
        
        EACH rm-rcpth NO-LOCK 
        WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
          AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
          AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
        USE-INDEX r-no
        BREAK BY rm-rcpth.trans-date DESC:
         IF FIRST(rm-rcpth.trans-date) THEN
             lv-lstdt = STRING(rm-rcpth.trans-date).
         IF LAST(rm-rcpth.trans-date) THEN
             lv-fistdt = STRING(rm-rcpth.trans-date).
         lReceiptFound = TRUE.
      END.


      IF NOT lReceiptFound THEN DO:
        FOR EACH rm-rdtlh NO-LOCK
          WHERE rm-rdtlh.company      EQ tt-rm-bin.company
            AND rm-rdtlh.tag          EQ tt-rm-bin.tag
            AND rm-rdtlh.rita-code    NE "S"
            AND rm-rdtlh.rita-code    NE "T"
          USE-INDEX tag,
          
          EACH rm-rcpth NO-LOCK 
          WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
            AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
            AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
          USE-INDEX r-no
          BREAK BY rm-rcpth.trans-date DESCENDING:
                   IF FIRST(rm-rcpth.trans-date) THEN
             lv-lstdt = STRING(rm-rcpth.trans-date).
         IF LAST(rm-rcpth.trans-date) THEN
             lv-fistdt = string(rm-rcpth.trans-date).
         lReceiptFound = TRUE.

        END.
      END.

      /* If not found, find with transfers */
      IF NOT lReceiptFound THEN DO:
        FOR EACH rm-rdtlh NO-LOCK
          WHERE rm-rdtlh.company      EQ tt-rm-bin.company
            AND rm-rdtlh.loc          EQ tt-rm-bin.loc
            AND rm-rdtlh.loc-bin      EQ tt-rm-bin.loc-bin
            AND rm-rdtlh.tag          EQ tt-rm-bin.tag
            AND rm-rdtlh.rita-code    NE "S"
          USE-INDEX tag,
          
          EACH rm-rcpth NO-LOCK 
          WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
            AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
            AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
          USE-INDEX r-no
          BREAK BY rm-rcpth.trans-date DESCENDING:

           IF FIRST(rm-rcpth.trans-date) THEN
               lv-lstdt = string(rm-rcpth.trans-date).
           IF LAST(rm-rcpth.trans-date) THEN
               lv-fistdt = string(rm-rcpth.trans-date).
        
        END.
      END.
   END. /* if tag <> "" */
   ELSE DO:
       
      FOR EACH rm-rcpth 
        WHERE rm-rcpth.company      EQ tt-rm-bin.company
          AND rm-rcpth.i-no         EQ tt-rm-bin.i-no
          AND rm-rcpth.rita-code    NE "S" NO-LOCK
        USE-INDEX i-no                                                                                  
        BREAK BY rm-rcpth.trans-date DESCENDING:

      IF FIRST(rm-rcpth.trans-date) THEN 
      lv-lstdt = STRING(rm-rcpth.trans-date).
      
       IF LAST(rm-rcpth.trans-date) THEN 
      lv-fistdt = STRING(rm-rcpth.trans-date).
       
     END.

   END.


    IF lv-lstdt = "" THEN ASSIGN lv-lstdt = STRING(tt-rm-bin.trans-date) .
    
    v-cost = IF ce-ctrl.r-cost THEN ITEM.avg-cost ELSE tt-rm-bin.cost.

    IF v-cost EQ ? THEN v-cost = 0.
    
    IF tagask AND tt-rm-bin.tag NE "" THEN
      tt-rm-bin.tag = "*" + tt-rm-bin.tag + "*".
    IF tt-rm-bin.tag NE "" THEN DO:
        FIND FIRST bf-loadtag  NO-LOCK
            WHERE bf-loadtag.company EQ cocode
              AND bf-loadtag.item-type EQ YES /*rm*/
              AND bf-loadtag.tag-no EQ tt-rm-bin.tag
            NO-ERROR.
        IF AVAILABLE bf-loadtag THEN
            cVendTag = bf-loadtag.misc-char[1].
    END.

    ASSIGN
     v-cum-qty   = v-cum-qty   + tt-rm-bin.qty
     v-cum-price = v-cum-price + (tt-rm-bin.qty * v-cost).

    IF /*tb_total-rolls AND*/ ITEM.r-wid GT 0 THEN
    DO:
       v-lf-qty = tt-rm-bin.qty.
       IF tt-rm-bin.tag NE "" AND tt-rm-bin.qty NE 0 THEN
          ASSIGN
            v-cum-rolls = v-cum-rolls + 1
            v-item-rolls = v-item-rolls + 1.
       ELSE
       DO:
          IF ITEM.cons-uom NE "LF" THEN
             RUN sys/ref/convquom.p(ITEM.cons-uom, "LF", ITEM.basis-w,
                                   (IF ITEM.r-wid eq 0 THEN ITEM.s-len
                                                       ELSE 12),
                                   (IF ITEM.r-wid eq 0 THEN ITEM.s-wid
                                                       ELSE ITEM.r-wid),
                                   ITEM.s-dep,                    
                                   tt-rm-bin.qty, OUTPUT v-lf-qty).
          ELSE
             v-lf-qty = tt-rm-bin.qty.
          
          IF ITEM.s-len NE 0 THEN
          DO:
             v-rolls-dec = v-lf-qty / ITEM.s-len.
             {sys/inc/roundup.i v-rolls-dec}
             ASSIGN
               v-cum-rolls = v-cum-rolls + v-rolls-dec
               v-item-rolls = v-item-rolls + v-rolls-dec.
          END.
          ELSE IF v-roll-multp NE 0 THEN
          DO:
             v-rolls-dec = v-lf-qty / v-roll-multp.
             {sys/inc/roundup.i v-rolls-dec}
             ASSIGN
               v-cum-rolls = v-cum-rolls + v-rolls-dec
               v-item-rolls = v-item-rolls + v-rolls-dec.
          END.
       END.
    END.
   ASSIGN cVendor = ""
    cJobNo = ""
    cShtSize = "" .
    FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ tt-rm-bin.company 
        AND po-ord.po-no EQ tt-rm-bin.po-no NO-ERROR.

    IF AVAILABLE po-ord THEN
        ASSIGN cVendor = po-ord.vend-no .

    IF tt-rm-bin.po-no NE 0 AND AVAILABLE po-ord THEN DO:
        FIND FIRST po-ordl  NO-LOCK WHERE po-ordl.company EQ tt-rm-bin.company 
            AND po-ordl.po-no EQ po-ord.po-no
            AND po-ordl.i-no EQ tt-rm-bin.i-no NO-ERROR.
        
        IF AVAILABLE po-ordl THEN DO:
            ASSIGN vpo-gl-act = po-ordl.actnum
                    cJobNo     = IF po-ordl.job-no NE "" THEN string(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2) ELSE "".

        END.
        ELSE
            ASSIGN vpo-gl-act = ""
                   cJobNo = ""  .
    END.
    ELSE
            ASSIGN vpo-gl-act = ""
                   cJobNo = "" .


    ASSIGN 
        v-msf = IF ITEM.r-wid GT 0 THEN v-lf-qty * ITEM.r-wid / 12 / 1000
                ELSE tt-rm-bin.qty * ITEM.s-wid * ITEM.s-len / 144 / 1000
        v-tons = v-MSF * ITEM.basis-w / 2000 /*Lbs*/
        v-CostMsf = tt-rm-bin.qty * v-cost / v-msf 
        v-cum-tons = v-cum-tons + v-tons
        v-cum-MSF = v-cum-MSF + v-msf
        cShtSize = IF ITEM.r-wid GT 0 THEN trim(STRING(ITEM.r-wid,">,>>9.99"))
                   ELSE (trim(string(ITEM.s-len,">,>>9.99")) + " X " + trim(string(ITEM.s-wid,">,>>9.99")) ).
     IF v-CostMsf EQ ? THEN
         ASSIGN v-CostMsf = 0.   /* task 10251310  */

   ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
    BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") GT 0 THEN DO:
          cFieldName = cTmpField.
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          hField = BUFFER bttrmbin:BUFFER-FIELD(cTmpField).
          cTmpField = SUBSTRING(GetFieldValue(hField),1,INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
          IF cFieldName = "tt-rm-bin.qty"
                  THEN cTmpField = STRING(DECIMAL(cTmpField),"->>>,>>9.99<<").
          IF cFieldName = "tt-rm-bin.loc"
                  THEN cTmpField = /*IF FIRST-OF(tt-rm-bin.loc) THEN*/ STRING((cTmpField),"x(5)") /*ELSE ""*/ .

          IF cFieldName = "tt-rm-bin.i-no"
                  THEN cTmpField = /*IF FIRST-OF(tt-rm-bin.i-no) THEN*/ STRING((cTmpField),"x(10)") /*ELSE ""*/ .

          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + QUOTER(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
          CASE cTmpField: 
                WHEN "rolls" THEN cVarValue = "" .
                WHEN "v-itemname" THEN cVarValue = /*IF FIRST-OF(tt-rm-bin.i-no) THEN*/ string(item.i-dscr,"x(30)") /*ELSE ""*/ .
                WHEN "v-cost" THEN cvarValue = STRING(v-cost,">>>,>>9.99<<<<").
                WHEN "v-total" THEN cVarValue = STRING(tt-rm-bin.qty * v-cost,"->,>>>,>>9.99").
                WHEN "v-MSF" THEN cVarValue = STRING(v-MSF,"->>>,>>9.99").
                WHEN "v-Tons" THEN cVarValue = STRING(v-Tons,"->>>,>>9.99").
                WHEN "v-CostMSF" THEN cVarValue = STRING(v-costMSF,"->>>,>>9.99").
                WHEN "cVendTag" THEN cVarValue = cVendTag.
                WHEN "trans-date" THEN cVarValue = STRING(lv-lstdt) /*string(lv-fistdt)*/ .
                WHEN "loc-bin" THEN cVarValue = STRING(tt-rm-bin.loc-bin).
                WHEN "tag" THEN cVarValue = fPrepareCSV(STRING(tt-rm-bin.tag)) .
                WHEN "qty" THEN cVarValue = STRING(tt-rm-bin.qty,"->>>,>>>,>>9.999").
                WHEN "cVendPo" THEN cVarValue = STRING(tt-rm-bin.po-no,"->>>>>>>>"). /* task 02261404 */
                WHEN "crtlot" THEN cVarValue = IF tt-rm-bin.tag2 NE "" THEN STRING(tt-rm-bin.tag2,"x(30)") ELSE "".
                WHEN "cVendCode" THEN cVarValue = STRING(cVendor).
                WHEN "cLstRcd" THEN cVarValue = STRING(lv-fistdt)  /*string(lv-lstdt)*/ .
                WHEN "cali" THEN cVarValue = STRING(ITEM.cal,"9.99999"). 
                WHEN "wt-msf" THEN cVarValue = STRING(item.basis-w,">>9.99").
                WHEN "po-gl-act" THEN cVarValue = STRING(vpo-gl-act) .
                WHEN "cItemName" THEN cVarValue = /*IF FIRST-OF(tt-rm-bin.i-no) THEN*/ string(ITEM.i-name,"x(30)") /*ELSE ""*/ .
                WHEN "job-no" THEN cVarValue = STRING(cJobNo,"x(10)") .
                WHEN "sht-size" THEN cVarValue = STRING(cShtSize,"x(20)"). 
          END CASE.
          cExcelVarValue = cVarValue.  
          IF cTmpField = "tag" THEN do:
              cVarValue = REPLACE(cVarValue,'"','')  .
              cVarValue = REPLACE(cVarValue,'=','')  .
          END.
          cDisplay = cDisplay + cVarValue +
                       FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
          cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
    IF LAST-OF(tt-rm-bin.loc-bin) THEN DO:
      IF NOT FIRST-OF(tt-rm-bin.loc-bin) AND psubtot THEN
         DO:
           IF NOT(tb_total-rolls AND ITEM.r-wid > 0) THEN DO:  /* task 12041301 */
          PUT   
            SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField:  
                 WHEN "rolls" THEN cVarValue =  (IF tb_total-rolls THEN STRING(v-cum-rolls,">>>>9") ELSE "") .
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  ITEM.i-dscr .
                 WHEN "v-cost" THEN cvarValue = "".
                 WHEN "v-total" THEN cVarValue = STRING(v-cum-price,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-cum-MSF,"->>>,>>9.99").
                 WHEN "v-Tons" THEN cVarValue = STRING(v-cum-tons,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-cum-qty,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = string(ITEM.i-name,"x(30)") .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "". 
                     
           END CASE.

           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
        
     END.
     PUT UNFORMATTED "           Bin Sub-total "   SUBSTRING(cDisplay,26,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Bin Sub-total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
     

           END.   /* not NOT(tb_total-rolls AND item.r-wid > 0) */
           ELSE DO:

          PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField:   
                 WHEN "rolls" THEN cVarValue = (IF tb_total-rolls THEN STRING(v-cum-rolls,">>>>9") ELSE "").
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-cum-price,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-cum-MSF,"->>>,>>9.99").
                 WHEN "v-Tons" THEN cVarValue = STRING(v-cum-tons,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-cum-qty,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "". 
           END CASE.
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
       
     END.
     PUT UNFORMATTED  "           Bin Sub-total"  SUBSTRING(cDisplay,25,300) SKIP.  /* task 12041301 */
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Bin Sub-total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
     

           END. /* else do*/
         END.
      
      IF NOT LAST-OF(tt-rm-bin.i-no) THEN PUT SKIP(1).

      ASSIGN
       v-cum-qty2   = v-cum-qty2   + v-cum-qty
       v-cum-price2 = v-cum-price2 + v-cum-price
       v-cum-tons2 = v-cum-tons2 + v-cum-tons 
       v-cum-MSF2 = v-cum-MSF2 + v-cum-MSF 
       v-cum-qty    = 0
       v-cum-price  = 0
       v-cum-rolls  = 0
       v-cum-tons   = 0
       v-cum-msf    = 0 .
    END.

    IF LAST-OF(tt-rm-bin.i-no) THEN DO:
      IF psubtot THEN
      DO:
        IF NOT FIRST-OF(tt-rm-bin.i-no) AND
           NOT(tb_total-rolls AND ITEM.r-wid > 0) THEN do:
            /* task 12041301 */
            PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField:      
                 WHEN "rolls" THEN cVarValue = (IF tb_total-rolls THEN STRING(v-cum-rolls,">>>>9") ELSE "") .
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-cum-price2,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-cum-MSF2,"->>>,>>9.99").
                 WHEN "v-Tons" THEN cVarValue = STRING(v-cum-tons2,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-cum-qty2,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "".
                 WHEN "sht-size" THEN cVarValue = "". 
           END CASE.
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
        
     END.
     PUT UNFORMATTED "           Item Total "   SUBSTRING(cDisplay,23,300) SKIP.  /* task 12041301 */
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Item Total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
     
    
        END. /*not first-of(tt-rm-bin.i-no) AND NOT(tb_total-rolls AND item.r-wid > 0)*/
        ELSE IF tb_total-rolls AND ITEM.r-wid GT 0 THEN
        DO:
           
            PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField: 
                 WHEN "rolls" THEN cVarValue = (IF tb_total-rolls THEN STRING(v-item-rolls,">>>>9") ELSE "") .
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-cum-price2,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-cum-MSF2,"->>>,>>9.99").
                 WHEN "v-Tons" THEN cVarValue = STRING(v-cum-tons2,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-cum-qty2,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "".
           END CASE.
        
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
        
     END.
     PUT UNFORMATTED  "           ITEM TOTAL"  SUBSTRING(cDisplay,22,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Item Total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
   
        END.
      END.

      PUT SKIP(1).
      
      ASSIGN
       v-tot-price  = v-tot-price + v-cum-price2
       v-tot-rolls = v-tot-rolls + v-item-rolls
       v-gt-qty2 = v-gt-qty2 + v-cum-qty2
       v-tot-tons = v-tot-tons + v-cum-tons2 
       v-tot-MSF = v-tot-MSF + v-cum-MSF2 
       v-cum-qty2   = 0
       v-item-rolls = 0
       v-cum-price2 = 0
       v-cum-tons2  = 0
       v-cum-MSF2   = 0.
    END.

    IF LAST-OF(tt-rm-bin.loc) THEN DO:
      IF pgtot THEN
      DO:
        IF tb_total-rolls THEN DO:
           PUT SKIP(1) .
                PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField:  
                 WHEN "rolls" THEN cVarValue =   (IF tb_total-rolls THEN STRING(v-tot-rolls,">>>>9") ELSE "") . 
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-tot-price,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-tot-MSF,"->>>,>>9.99").
                 WHEN "v-Tons" THEN cVarValue = STRING(v-tot-tons,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-gt-qty2,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".    
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "".
           END CASE.
        
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
      
     END.
     PUT UNFORMATTED "           Grand Totals   "  SUBSTRING(cDisplay,27,300) SKIP.
     PUT   str-line SKIP .
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Grand Total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
  
        END.

        ELSE DO:
           PUT SKIP(1).

                PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField: 
                 WHEN "rolls" THEN cVarValue =  (IF tb_total-rolls THEN STRING(v-tot-rolls,">>>>9") ELSE "") . 
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-tot-price,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-tot-MSF,"->>>,>>9.99").
                 WHEN "v-Tons" THEN cVarValue = STRING(v-tot-tons,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-gt-qty2,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "". 
           END CASE.
        
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
        
     END.
     PUT UNFORMATTED "           Grand Totals       " +   "            " SUBSTRING(cDisplay,43,300) SKIP.
     PUT   str-line SKIP .
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Grand Total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
   
        END.
      END.

      ASSIGN
      v-tot-price = 0
      v-tot-rolls = 0
      v-gt-qty2 = 0
      v-tot-tons = 0
      v-tot-MSF = 0.
    END.
  END.
END.  /* detail */

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

  IF tb_excel THEN DO:
         OUTPUT STREAM excel CLOSE.
         IF tb_runExcel THEN
             OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).
     END.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-summary C-Win 
PROCEDURE run-report-summary :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelDisplay AS CHARACTER NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE cTmpField AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExcelVarValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
DEFINE VARIABLE str-tit4 AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5 AS CHARACTER FORMAT "x(200)" NO-UNDO.

{sys/form/r-top5DL.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEF BUFFER bttrmbin FOR tt-rm-bin.
DEFINE VARIABLE save_id AS RECID.
DEFINE VARIABLE v-price AS DECIMAL FORMAT "->>>>9.99".
DEFINE VARIABLE v-tot-price AS DECIMAL FORMAT "$->>,>>>,>>9.99".
DEFINE VARIABLE v-cum-qty   AS DECIMAL FORMAT "->>>>>9.999".
DEFINE VARIABLE v-cum-price AS DECIMAL FORMAT "->>>,>>9.99".
DEFINE VARIABLE v-cum-qty2   AS DECIMAL FORMAT "->>>>>9.999".  /* item totals */
DEFINE VARIABLE v-gt-qty2 AS DECIMAL FORMAT "->>>,>>>,>>9.999".
DEFINE VARIABLE v-cum-price2 AS DECIMAL FORMAT "->>>,>>9.99".  /* item totals */
DEFINE VARIABLE v-cum-rolls AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-item-rolls AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-tot-rolls AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE v-rolls-dec AS DECIMAL DECIMALS 4 NO-UNDO.
DEFINE VARIABLE fitm LIKE rm-bin.i-no FORMAT "X(10)"INITIAL "".
DEFINE VARIABLE titm LIKE fitm INITIAL "zzzzzzzzzz".
DEFINE VARIABLE floc LIKE rm-bin.loc INITIAL "".
DEFINE VARIABLE tloc LIKE floc INITIAL "zzzzz".
DEFINE VARIABLE fcat AS CHARACTER INITIAL "".
DEFINE VARIABLE tcat LIKE fcat INITIAL "zzzzzz".
DEFINE VARIABLE TYPE AS LOGICAL FORMAT "R/E" INITIAL YES.
DEFINE VARIABLE ftyp LIKE ITEM.mat-type INITIAL "".
DEFINE VARIABLE ttyp like ftyp INITIAL "z".
DEFINE VARIABLE zbal AS LOGICAL FORMAT "Y/N" INITIAL NO.
DEFINE VARIABLE v-fst-loc AS LOGICAL.
DEFINE VARIABLE v-fst-ino AS LOGICAL.
DEFINE VARIABLE v-lst-ino AS LOGICAL.
DEFINE VARIABLE v-prnt-line AS INTEGER.
DEFINE VARIABLE v-cost LIKE rm-bin.cost.
DEFINE VARIABLE psubtot AS LOGICAL INITIAL YES.
DEFINE VARIABLE pgtot AS LOGICAL INITIAL NO.
DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE VARIABLE tagask AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-lf-qty LIKE rm-bin.qty NO-UNDO.
DEFINE VARIABLE v-MSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cum-MSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-tot-MSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-Tons AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cum-ton AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-tot-ton AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-CostMSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE cVendor AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE vpo-gl-act AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cShtSize AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cJobNo LIKE po-ordl.job-no NO-UNDO.

/* rdb 02/06/07 02050701 */
DEFINE VARIABLE chrTotCostVal AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrRmBinTag AS CHARACTER FORMAT "x(22)" NO-UNDO.
DEFINE VARIABLE ctype AS CHARACTER FORMAT "!"   NO-UNDO INITIAL "B".
FIND FIRST ce-ctrl NO-LOCK WHERE ce-ctrl.company EQ cocode NO-ERROR.

ASSIGN
 str-tit2 = c-win:TITLE
 {sys/inc/ctrtext.i str-tit2 112}

 fitm    = begin_rm-no
 titm    = end_rm-no
 floc    = begin_whs
 tloc    = end_whs
 fcat    = begin_procat
 tcat    = end_procat
 ftyp    = begin_mat-type
 ttyp    = end_mat-type
 zbal    = tb_zero-bal
 psubtot = tb_subt
 pgtot   = tb_grdt
 tagask  = tb_tagask
 ctype   = substr(rd_item,1,1) .

  
 FOR EACH ITEM  NO-LOCK 
    WHERE ITEM.company           EQ cocode
    AND ITEM.i-no              GE fitm
    AND ITEM.i-no              LE titm
    AND ITEM.i-no              NE ""
    AND ITEM.procat            GE fcat
    AND ITEM.procat            LE tcat
    AND ITEM.mat-type          GE ftyp
    AND ITEM.mat-type          LE ttyp
    AND (item.i-code  EQ ctype OR ctype EQ "B")  :
  

       {custom/statusMsg.i "'Processing Item # ' + string(item.i-no)"} 

    RUN rm-mkbin.

    IF zbal AND ITEM.q-onh EQ 0 AND
       NOT CAN-FIND(FIRST tt-rm-bin WHERE
       tt-rm-bin.company EQ ITEM.company AND
       tt-rm-bin.i-no EQ ITEM.i-no) THEN
       DO:
          CREATE tt-rm-bin.
          ASSIGN tt-rm-bin.company = ITEM.company
                 tt-rm-bin.i-no = ITEM.i-no
                 tt-rm-bin.trans-date = TODAY.
          RELEASE tt-rm-bin.
       END.
    END.
  


  
  FOR EACH tt-rm-bin NO-LOCK
      WHERE tt-rm-bin.loc          GE floc
        AND tt-rm-bin.loc          LE tloc
        AND tt-rm-bin.trans-date   GE begin_date
        AND tt-rm-bin.trans-date   LE end_date
        AND (zbal OR tt-rm-bin.qty NE 0) ,
     

      FIRST ITEM NO-LOCK
      WHERE ITEM.company EQ tt-rm-bin.company
        AND ITEM.i-no    EQ tt-rm-bin.i-no
     
      BREAK BY tt-rm-bin.loc
            BY tt-rm-bin.i-no
            BY tt-rm-bin.loc-bin
            BY tt-rm-bin.tag

      WITH FRAME itemx:

       {custom/statusMsg.i "'Processing Item # ' + string(tt-rm-bin.i-no)"} 

    IF FIRST-OF(tt-rm-bin.loc) OR
       LINE-COUNTER GT PAGE-SIZE - 10 THEN DO:
      IF NOT FIRST(tt-rm-bin.loc) THEN PAGE.
      v-prnt-line = 0.
    END.

    ELSE v-prnt-line = 1.

    v-cost = IF ce-ctrl.r-cost THEN ITEM.avg-cost ELSE tt-rm-bin.cost.

    IF v-cost EQ ? THEN v-cost = 0.
    
    IF tagask AND tt-rm-bin.tag NE "" THEN
      tt-rm-bin.tag = "*" + tt-rm-bin.tag + "*".

    ASSIGN 
     v-cum-qty   = v-cum-qty   + tt-rm-bin.qty
     v-cum-price = v-cum-price + (tt-rm-bin.qty * v-cost).

    IF /*tb_total-rolls AND*/ ITEM.r-wid GT 0 THEN
    DO:
       v-lf-qty = tt-rm-bin.qty.
       IF tt-rm-bin.tag NE "" THEN
          ASSIGN
            v-cum-rolls = v-cum-rolls + 1
            v-item-rolls = v-item-rolls + 1.
       ELSE
       DO:
          IF ITEM.cons-uom NE "LF" THEN
             RUN sys/ref/convquom.p(ITEM.cons-uom, "LF", ITEM.basis-w,
                                   (IF ITEM.r-wid eq 0 THEN ITEM.s-len
                                                       ELSE 12),
                                   (IF ITEM.r-wid eq 0 THEN ITEM.s-wid
                                                       ELSE ITEM.r-wid),
                                   ITEM.s-dep,                    
                                   tt-rm-bin.qty, OUTPUT v-lf-qty).
          ELSE
             v-lf-qty = tt-rm-bin.qty.
          
          IF ITEM.s-len NE 0 THEN
          DO:
             v-rolls-dec = v-lf-qty / ITEM.s-len.
             {sys/inc/roundup.i v-rolls-dec}
             ASSIGN
               v-cum-rolls = v-cum-rolls + v-rolls-dec
               v-item-rolls = v-item-rolls + v-rolls-dec.
          END.
          ELSE IF v-roll-multp NE 0 THEN
          DO:
             v-rolls-dec = v-lf-qty / v-roll-multp.
             {sys/inc/roundup.i v-rolls-dec}
             ASSIGN
               v-cum-rolls = v-cum-rolls + v-rolls-dec
               v-item-rolls = v-item-rolls + v-rolls-dec.
          END.
       END.
    END. 
    ASSIGN cShtSize = "".

    ASSIGN 
        v-msf = IF item.r-wid GT 0 THEN v-lf-qty * ITEM.r-wid / 12 / 1000
                ELSE tt-rm-bin.qty * ITEM.s-wid * ITEM.s-len / 144 / 1000
        v-tons = v-MSF * ITEM.basis-w / 2000 /*Lbs*/ 
        cShtSize = IF ITEM.r-wid GT 0 THEN trim(STRING(ITEM.r-wid,">,>>9.99"))
                   ELSE (trim(string(ITEM.s-len,">,>>9.99")) + " X " + trim(string(ITEM.s-wid,">,>>9.99")) )     .

    /*if last-of(tt-rm-bin.loc-bin) then do:  */
       
     /* if not first-of(tt-rm-bin.loc-bin) and psubtot then 
         DO:                                              
           IF NOT(tb_total-rolls AND item.r-wid > 0) THEN 
             put "-----------"          to 100            
                 "-----------"          to 123 skip       
                 "Bin Sub-total"        at 61             
                 v-cum-qty              to 100            
                 v-cum-price            to 123.           
           ELSE                                           
           DO:                                            
              put "-----------"          to 100           
                 "-----------"          to 123 skip       
                 "Total Rolls"          AT 41             
                 v-cum-rolls            TO 58             
                 "Bin Sub-total"        at 61             
                 v-cum-qty              to 100            
                 v-cum-price            to 123.           
           END.                                           
         END.        */                                     
                                                          
     /* if not last-of(tt-rm-bin.i-no) then put skip(1).    */

      ASSIGN 
       v-cum-qty2   = v-cum-qty2   + v-cum-qty
       v-cum-price2 = v-cum-price2 + v-cum-price
       v-cum-ton = v-cum-ton + v-Tons 
       v-cum-msf = v-cum-msf + v-MSF 
       v-cum-qty    = 0
       v-cum-price  = 0
       v-cum-rolls  = 0
       v-Tons = 0
       v-MSF = 0
        .
   /* end.*/
       

IF LAST-OF(tt-rm-bin.i-no) THEN DO:

    ASSIGN cVendor = ""
    cJobNo = "" .
    FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ tt-rm-bin.company 
        AND po-ord.po-no EQ tt-rm-bin.po-no NO-ERROR.

    IF AVAILABLE po-ord THEN
        ASSIGN cVendor = po-ord.vend-no .

    IF tt-rm-bin.po-no NE 0 AND AVAILABLE po-ord THEN DO:
        FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ tt-rm-bin.company 
            AND po-ordl.po-no EQ po-ord.po-no
            AND po-ordl.i-no EQ tt-rm-bin.i-no NO-ERROR.
        
        IF AVAILABLE po-ordl THEN
            ASSIGN vpo-gl-act = po-ordl.actnum
                   cJobNo     = IF po-ordl.job-no NE "" THEN string(po-ordl.job-no) + "-" + STRING(po-ordl.job-no2) ELSE "" .
        ELSE
            ASSIGN vpo-gl-act = ""
                   cJobNo     = "" .
    END.
    ELSE
            ASSIGN vpo-gl-act = ""
                   cJobNo     = "" .
    
    ASSIGN cDisplay = ""
           cTmpField = ""
           cVarValue = ""
           cExcelDisplay = ""
           cExcelVarValue = "".
    BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
    DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
       cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
       IF INDEX(cTmpField,".") GT 0 THEN DO:
          cFieldName = cTmpField.
          cTmpField = SUBSTRING(cTmpField,INDEX(cTmpField,".") + 1).
          hField = BUFFER bttrmbin:BUFFER-FIELD(cTmpField).
          cTmpField = SUBSTRING(GetFieldValue(hField),1,INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength))).
          IF cFieldName = "tt-rm-bin.qty"
                  THEN cTmpField = STRING(DECIMAL(cTmpField),"->>>,>>9.99<<").
          cDisplay = cDisplay + cTmpField + 
                           FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cTmpField))
                           .
          cExcelDisplay = cExcelDisplay + QUOTER(GetFieldValue(hField)) + ",".       
       END.
       ELSE DO:            
          CASE cTmpField:  
              WHEN "rolls" THEN cVarValue =  "" .
                WHEN "v-itemname" THEN cVarValue = string(ITEM.i-dscr,"x(30)").
                WHEN "v-cost" THEN cvarValue = "".
                WHEN "v-total" THEN cVarValue = STRING(v-cum-price2,"->,>>>,>>9.99").
                WHEN "v-MSF" THEN cVarValue = STRING(v-cum-MSF,"->>>,>>9.99").   
                WHEN "v-Tons" THEN cVarValue = STRING(v-cum-Ton,"->>>,>>9.99"). 
                WHEN "v-CostMSF" THEN cVarValue = "".
                WHEN "cVendTag" THEN cVarValue =  "".
                WHEN "trans-date" THEN cVarValue = "".
                WHEN "loc-bin" THEN cVarValue = "".
                WHEN "tag" THEN cVarValue = "".
                WHEN "qty" THEN cVarValue = STRING(v-cum-qty2,"->>>,>>>,>>9.999").
                WHEN "cVendPo" THEN cVarValue = STRING(tt-rm-bin.po-no,"->>>>>>>>").
                WHEN "crtlot" THEN cVarValue = "".
                WHEN "cVendCode" THEN cVarValue = STRING(cVendor).
                WHEN "cLstRcd" THEN cVarValue = "".
                WHEN "cali" THEN cVarValue = "".
                WHEN "wt-msf" THEN cVarValue = "".
                WHEN "po-gl-act" THEN cVarValue = STRING(vpo-gl-act) .
                WHEN "cItemName" THEN cVarValue = string(ITEM.i-name,"x(30)") .
                WHEN "job-no" THEN cVarValue = STRING(cJobNo,"x(10)") .
                WHEN "sht-size" THEN cVarValue = STRING(cShtSize,"x(20)"). 
          END CASE.
          cExcelVarValue = cVarValue.  
          cDisplay = cDisplay + cVarValue +
                       FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
          cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
       END.
    END.
    PUT UNFORMATTED cDisplay SKIP.
    IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
               cExcelDisplay SKIP.
    END.
        
END.



  /*   if last-of(tt-rm-bin.loc-bin) then do:                
  /*    if not first-of(tt-rm-bin.loc-bin) and psubtot then 
         DO:                                              
           IF NOT(tb_total-rolls AND item.r-wid > 0) THEN 
             put "-----------"          to 100            
                 "-----------"          to 123 skip       
                 "Bin Sub-total"        at 61             
                 v-cum-qty              to 100            
                 v-cum-price            to 123.           
           ELSE                                           
           DO:                                            
              put "-----------"          to 100           
                 "-----------"          to 123 skip       
                 "Total Rolls"          AT 41             
                 v-cum-rolls            TO 58             
                 "Bin Sub-total"        at 61             
                 v-cum-qty              to 100            
                 v-cum-price            to 123.           
           END.                                           
         END.        */                                     
                                                          
      if not last-of(tt-rm-bin.i-no) then put skip(1).    

      assign
       v-cum-qty2   = v-cum-qty2   + v-cum-qty
       v-cum-price2 = v-cum-price2 + v-cum-price
       v-cum-qty    = 0
       v-cum-price  = 0
       v-cum-rolls  = 0.
    end. */

   IF LAST-OF(tt-rm-bin.i-no) THEN DO:
    /*  if psubtot then*/
      DO:        

      /*  IF /*not first-of(tt-rm-bin.i-no) AND*/
           NOT(tb_total-rolls AND item.r-wid > 0) THEN
           PUT /* "-----------"          to 100
               "-----------"          to 123 skip
               "Item Total"           at 63*/
               tt-rm-bin.loc
               tt-rm-bin.i-no
               ITEM.i-name
               v-cum-qty2             to 100
               v-cum-price2           to 123.
        ELSE*/
        IF tb_total-rolls AND ITEM.r-wid GT 0 THEN
        DO:
           /*put /*"------"               TO 58
               "-----------"          to 100
               "-----------"          to 123 skip */
           /*    tt-rm-bin.loc
               tt-rm-bin.i-no
               ITEM.i-name */
               "Item Total Rolls"     AT 36      
               v-item-rolls           TO 58
               /*"Item Total"           at 63*/
               "Item Total Qty"            AT 75
               v-cum-qty2             to 100
               "Total Cost"          AT 102
               v-cum-price2           to 123.*/

            PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField: 
                 WHEN "rolls" THEN cVarValue =  (IF tb_total-rolls THEN STRING(v-item-rolls,">>>>9") ELSE "") .
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-cum-price2,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-cum-MSF,"->>>,>>9.99").  
                 WHEN "v-Tons" THEN cVarValue = STRING(v-cum-Ton,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-cum-qty2,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "".
           END CASE.
        
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
        
     END.
     PUT UNFORMATTED  "           ITEM TOTAL"  SUBSTRING(cDisplay,22,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Item Total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
        END.
      END.

      PUT SKIP(1).
      
      ASSIGN
       v-tot-price  = v-tot-price + v-cum-price2
       v-tot-rolls = v-tot-rolls + v-item-rolls
       v-gt-qty2 = v-gt-qty2 + v-cum-qty2
       v-tot-ton =  v-tot-ton + v-cum-ton 
       v-tot-msf = v-tot-msf + v-cum-msf
       v-cum-qty2   = 0
       v-item-rolls = 0
       v-cum-price2 = 0
       v-cum-ton  = 0
       v-cum-msf  = 0.
    END.

    IF LAST-OF(tt-rm-bin.loc) THEN DO:
      IF pgtot THEN
      DO:
        /*IF tb_total-rolls THEN
           put skip(1)
               "------"                    TO 58
               "---------------"           to 100
               "--------------"            to 123
               "Grand Totals Rolls"        AT 35
               v-tot-rolls                 TO 58
               " Qty"                      TO 84
               v-gt-qty2                   TO 100
               "Cost"                      TO 108
               v-tot-price                 to 123
               "------"                    TO 58
               "---------------"           to 100
               "--------------"            to 123.
        ELSE
           put skip(1)
               "---------------"           to 77
               "--------------"            to 100
               "Grand Totals       Qty"          AT 40
               v-gt-qty2                   TO 77
               "Cost"                      TO 85
               v-tot-price                 to 100
               "---------------"           to 77
               "--------------"            to 100.*/

          PUT   SKIP  str-line SKIP .
     ASSIGN cDisplay = ""
            cTmpField = ""
            cVarValue = ""
            cExcelDisplay = ""
            cExcelVarValue = "".
     BUFFER bttrmbin:FIND-BY-ROWID(ROWID(tt-rm-bin), NO-LOCK) .        
     DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
        cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                   
           CASE cTmpField: 
                 WHEN "rolls" THEN cVarValue = (IF tb_total-rolls THEN STRING(v-tot-rolls,">>>>9") ELSE "") .
                 WHEN "tt-rm-bin.loc" THEN cVarValue =  "" . 
                 WHEN "tt-rm-bin.i-no" THEN cVarValue =  "" .
                 WHEN "tt-rm-bin.tag" THEN cVarValue =  "" .
                 WHEN "v-itemname" THEN cVarValue =  "" .
                 WHEN "v-cost" THEN cvarValue =  "" .
                 WHEN "v-total" THEN cVarValue = STRING(v-tot-price,"->,>>>,>>9.99").
                 WHEN "v-MSF" THEN cVarValue = STRING(v-tot-MSF,"->>>,>>9.99").  
                 WHEN "v-Tons" THEN cVarValue = STRING(v-tot-Ton,"->>>,>>9.99").
                 WHEN "v-CostMSF" THEN cVarValue = "" .
                 WHEN "cVendTag" THEN cVarValue =  "".
                 WHEN "trans-date" THEN cVarValue = "".
                 WHEN "loc-bin" THEN cVarValue = "" .
                 WHEN "tag" THEN cVarValue = "" .
                 WHEN "qty" THEN cVarValue = STRING(v-gt-qty2,"->>>,>>>,>>9.999").
                 WHEN "cVendPo" THEN cVarValue = "".
                 WHEN "crtlot" THEN cVarValue = "".
                 WHEN "cVendCode" THEN cVarValue = "".
                 WHEN "cLstRcd" THEN cVarValue = "".
                 WHEN "cali" THEN cVarValue = "".
                 WHEN "wt-msf" THEN cVarValue = "".
                 WHEN "po-gl-act" THEN cVarValue = "" .
                 WHEN "cItemName" THEN cVarValue = "" .
                 WHEN "job-no" THEN cVarValue = "" .
                 WHEN "sht-size" THEN cVarValue = "".
           END CASE.
        
           cExcelVarValue = cVarValue.  
           cDisplay = cDisplay + cVarValue +
                        FILL(" ",INTEGER(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)).             
           cExcelDisplay = cExcelDisplay + QUOTER(cExcelVarValue) + ",". 
        
     END.
     PUT UNFORMATTED  "           GRAND TOTAL"  SUBSTRING(cDisplay,23,300) SKIP.
     IF tb_excel THEN DO:
         PUT STREAM excel UNFORMATTED  
             "Grand Total " + SUBSTRING(cExcelDisplay,3,300) SKIP.
     END.
      END.

      ASSIGN
      v-tot-price = 0
      v-tot-rolls = 0
      v-gt-qty2 = 0
      v-tot-ton = 0 
      v-tot-msf = 0.
    END. 
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
  DEFINE VARIABLE lv-frame-hdl AS handle no-undo.
  DEFINE VARIABLE lv-group-hdl AS handle no-undo.
  DEFINE VARIABLE lv-field-hdl AS handle no-undo.
  DEFINE VARIABLE lv-field2-hdl AS handle no-undo.
  DEFINE VARIABLE parm-fld-list AS cha no-undo.
  DEFINE VARIABLE parm-lbl-list AS cha no-undo.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-label AS CHARACTER.
  
  lv-frame-hdl = FRAME {&FRAME-NAME}:HANDLE.
  lv-group-hdl = lv-frame-hdl:FIRST-CHILD.
  lv-field-hdl = lv-group-hdl:FIRST-CHILD .
  
  DO WHILE TRUE:
     IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.
     IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") GT 0
        THEN DO:
           IF lv-field-hdl:label NE ? THEN 
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
                  lv-field2-hdl = lv-field2-hdl:next-sibling.                 
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
       ENTRY(i,parm-lbl-list) NE "" THEN DO:
       
      lv-label = FILL(" ",34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list)))) +
                 TRIM(ENTRY(i,parm-lbl-list)) + ":".
                 
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

FUNCTION fPrepareCSV RETURNS CHARACTER 
(ipcValue AS CHARACTER) :
/*------------------------------------------------------------------------------
Purpose: Tests for an integer value in a character and adds a ' to force Text formatting
Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE iTester AS INT64 NO-UNDO.

 iTester = INT64(ipcValue) NO-ERROR.
 IF iTester NE 0 AND lTagFormat THEN 
      cReturn = '="' + ipcValue + '"'.
 ELSE DO:
     cReturn = ipcValue.
  END.

RETURN cReturn.

END FUNCTION.

