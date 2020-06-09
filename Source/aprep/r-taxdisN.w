&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aprep\r-taxdis.w

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

DEF TEMP-TABLE tt-report LIKE report.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF STREAM excel.

DEFINE TEMP-TABLE ttRawData
    FIELD riInvoice AS ROWID
    FIELD riCashReceipt AS ROWID
    FIELD cTaxGroup AS CHARACTER
    FIELD cAccount AS CHARACTER
.


DEF VAR ldummy AS LOG NO-UNDO.
DEF VAR cTextListToSelect AS cha NO-UNDO.
DEF VAR cFieldListToSelect AS cha NO-UNDO.
DEF VAR cFieldLength AS cha NO-UNDO.
DEF VAR cFieldType AS cha NO-UNDO.
DEF VAR iColumnLength AS INT NO-UNDO.
DEF VAR cTextListToDefault AS cha NO-UNDO.


ASSIGN cTextListToSelect = "Tax Code,Tax Name,Tax Rate,Gross Sales$,Sales Taxable$,"
                           + "Sales Exempt $,Freight $,Freight Taxable$,Freight Exempt $,Tax $,"
                           + "Gross + Freight,Taxable Revenue"
       cFieldListToSelect = "tax-code,tax-name,tax-rat,gro-sal,sal-tax," +
                            "sal-exe,fright,fri-tax,fri-exe,tax," +
                            "gross-freight,tax-revenue"
       cFieldLength = "8,20,8,14,14," + "14,14,16,16,14," + "15,15"
       cFieldType = "c,c,i,i,i," + "i,i,i,i,i," + "i,i" 
    .

{sys/inc/ttRptSel.i}
ASSIGN cTextListToDefault  = "Tax Code,Tax Name,Tax Rate,Gross Sales$,Sales Taxable$,"
                           + "Sales Exempt $,Freight $,Freight Taxable$,Freight Exempt $,Tax $" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 begin_year begin_period ~
begin_date end_date rd_print tb_page-break sl_avail Btn_Def sl_selected ~
Btn_Add Btn_Remove btn_Up btn_down rd-dest lv-ornt lines-per-page ~
lv-font-no td-show-parm tb_excel tb_runExcel fi_file btn-ok btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_year begin_period begin_date ~
end_date lbl_sort rd_print tb_page-break sl_avail sl_selected rd-dest ~
lv-ornt lines-per-page lv-font-no lv-font-name td-show-parm tb_excel ~
tb_runExcel fi_file 

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

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Beginning Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE begin_period AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "For Period" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE begin_year AS INTEGER FORMAT ">>>>":U INITIAL 9999 
     LABEL "For Year" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Ending Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-taxdis.csv" 
     LABEL "If Yes, File Name" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE lbl_sort AS CHARACTER FORMAT "X(256)":U INITIAL "Print?" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

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

DEFINE VARIABLE rd_print AS CHARACTER INITIAL "Summary" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Summary", "Summary",
"Detail", "Detail"
     SIZE 35.2 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 8.57.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.62.

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

DEFINE VARIABLE tb_page-break AS LOGICAL INITIAL no 
     LABEL "Page break by tax group?" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .67 NO-UNDO.

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
     begin_year AT ROW 2.19 COL 28 COLON-ALIGNED HELP
          "Enter Fiscal Year"
     begin_period AT ROW 3.38 COL 28 COLON-ALIGNED HELP
          "Enter Reporting Period"
     begin_date AT ROW 4.57 COL 28 COLON-ALIGNED HELP
          "Enter Beginning Date"
     end_date AT ROW 4.57 COL 63 COLON-ALIGNED HELP
          "Enter Ending Date"
     lbl_sort AT ROW 6.1 COL 20.8 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     rd_print AT ROW 6.1 COL 30.6 NO-LABEL WIDGET-ID 60
     tb_page-break AT ROW 7.52 COL 30.2 WIDGET-ID 66
     sl_avail AT ROW 9.48 COL 4.2 NO-LABEL WIDGET-ID 26
     Btn_Def AT ROW 9.48 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 56
     sl_selected AT ROW 9.48 COL 59.6 NO-LABEL WIDGET-ID 28
     Btn_Add AT ROW 10.48 COL 40.2 HELP
          "Add Selected Table to Tables to Audit" WIDGET-ID 32
     Btn_Remove AT ROW 11.48 COL 40.2 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 34
     btn_Up AT ROW 12.52 COL 40.2 WIDGET-ID 40
     btn_down AT ROW 13.52 COL 40.2 WIDGET-ID 42
     rd-dest AT ROW 15.76 COL 6.4 NO-LABEL
     lv-ornt AT ROW 16.71 COL 30.4 NO-LABEL
     lines-per-page AT ROW 16.71 COL 83.4 COLON-ALIGNED
     lv-font-no AT ROW 18.14 COL 33.4 COLON-ALIGNED
     lv-font-name AT ROW 19.1 COL 27.4 COLON-ALIGNED NO-LABEL
     td-show-parm AT ROW 20.19 COL 29.6
     tb_excel AT ROW 21.33 COL 49.4 RIGHT-ALIGNED
     tb_runExcel AT ROW 21.33 COL 70.4 RIGHT-ALIGNED
     fi_file AT ROW 22.14 COL 27.4 COLON-ALIGNED HELP
          "Enter File Name"
     btn-ok AT ROW 23.62 COL 18.4
     btn-cancel AT ROW 23.62 COL 58.4
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.76 COL 5 WIDGET-ID 38
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .71 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 15.05 COL 4.4
     "Selected Columns(In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 8.76 COL 59.6 WIDGET-ID 44
     RECT-6 AT ROW 14.81 COL 2.4
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.24
         SIZE 95.2 BY 25.19.


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
         TITLE              = "Tax Distribution Schedule"
         HEIGHT             = 26.14
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
       begin_date:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_period:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       begin_year:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       btn-ok:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".

ASSIGN 
       end_date:PRIVATE-DATA IN FRAME FRAME-A     = 
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
       rd_print:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

/* SETTINGS FOR TOGGLE-BOX tb_excel IN FRAME FRAME-A
   ALIGN-R                                                              */
ASSIGN 
       tb_excel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "parm".

ASSIGN 
       tb_page-break:PRIVATE-DATA IN FRAME FRAME-A     = 
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
ON END-ERROR OF C-Win /* Tax Distribution Schedule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tax Distribution Schedule */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_date C-Win
ON LEAVE OF begin_date IN FRAME FRAME-A /* Beginning Date */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_period
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_period C-Win
ON VALUE-CHANGED OF begin_period IN FRAME FRAME-A /* For Period */
DO:
  RUN show-period-dates.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_year
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_year C-Win
ON VALUE-CHANGED OF begin_year IN FRAME FRAME-A /* For Year */
DO:
  RUN show-period-dates.
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
  IF rd_print EQ "Summary" THEN
  run run-report-summary(INPUT cocode,
                INPUT begin_date,
                INPUT end_date). 
  ELSE              
  RUN RunReport(INPUT cocode,
                INPUT begin_date,
                INPUT end_date).
  STATUS DEFAULT "Processing Complete".

  case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_date
                            &END_cust=begin_date
                            &fax-subject="Tax Distribution Schedule"
                            &fax-body="Tax Distribution Schedule"
                            &fax-file=list-name }
       END.
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail.i &TYPE = ''
                             &begin_cust=''
                             &END_cust=''
                             &mail-subject="Tax Distribution Schedule"
                             &mail-body="Tax Distribution Schedule"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimailr.i &TYPE = ''
                                  &begin_cust=''
                                  &END_cust=''
                                  &mail-subject="Tax Distribution Schedule"
                                  &mail-body="Tax Distribution Schedule"
                                  &mail-file=list-name }
           END.

       END. 
       WHEN 6 THEN run output-to-port.
  end case. 
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


&Scoped-define SELF-NAME end_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_date C-Win
ON LEAVE OF end_date IN FRAME FRAME-A /* Ending Date */
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


&Scoped-define SELF-NAME rd_print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_print C-Win
ON VALUE-CHANGED OF rd_print IN FRAME FRAME-A
DO:
  ASSIGN {&self-name}.
  IF rd_print:SCREEN-VALUE EQ "Summary" THEN
  DO:
      tb_page-break:SENSITIVE = FALSE.
      tb_page-break:SCREEN-VALUE = "No".
  END.
  ELSE tb_page-break:SENSITIVE = TRUE.
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


&Scoped-define SELF-NAME tb_page-break
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_page-break C-Win
ON VALUE-CHANGED OF tb_page-break IN FRAME FRAME-A /* Page break by tax group? */
DO:
  ASSIGN {&self-name}.
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
   begin_year   = year(today)
   begin_period = month(today)
   begin_date   = today
   end_date     = today.

  find first period
      where period.company eq cocode
        and period.yr      eq begin_year
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
      no-lock no-error.

  if avail period then
    assign
     begin_period = period.pnum
     begin_year   = period.yr
     begin_date   = period.pst.
  RUN DisplaySelectionList.
  RUN enable_UI.

  {methods/nowait.i}

  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
    RUN DisplaySelectionList2.
    IF rd_print:SCREEN-VALUE EQ "Summary" THEN
    DO:
      tb_page-break:SENSITIVE = FALSE.
      tb_page-break:SCREEN-VALUE = "No".
    END.
    ELSE tb_page-break:SENSITIVE = TRUE.
    
    APPLY "entry" TO begin_year.
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
  DISPLAY begin_year begin_period begin_date end_date lbl_sort rd_print 
          tb_page-break sl_avail sl_selected rd-dest lv-ornt lines-per-page 
          lv-font-no lv-font-name td-show-parm tb_excel tb_runExcel fi_file 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-6 RECT-7 begin_year begin_period begin_date end_date rd_print 
         tb_page-break sl_avail Btn_Def sl_selected Btn_Add Btn_Remove btn_Up 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report-summary C-Win 
PROCEDURE run-report-summary :
/* ---------------------------------------------- ap/rep/taxsched.p 07/99 JLF */
/* Tax Distribution Schedule by Customer                                      */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtBegin AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipdtEnd AS DATE NO-UNDO.

DEFINE VARIABLE cTaxDescription AS CHARACTER FORMAT "x(24)" NO-UNDO.
DEFINE VARIABLE dAmountSales AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreight AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountSalesTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreightTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountSalesTaxable AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreightTaxable AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountSalesTaxableTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreightTaxableTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountTax AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountTaxTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE dRateTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dRateFreightTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iLevel AS INTEGER     NO-UNDO.

/*{sys/form/r-top3w.f}*/


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

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dTexRate AS DECIMAL NO-UNDO .
DEFINE VARIABLE cFileName2 LIKE fi_file NO-UNDO .

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName2) . 

ASSIGN
    str-tit2 = c-win:TITLE
    {sys/inc/ctrtext.i str-tit2 112}
    str-tit3 = "(" + STRING(ipdtBegin) + "-" + STRING(ipdtEnd) + ")"
    {sys/inc/ctrtext.i str-tit3 132}. 

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

        IF LOOKUP(ttRptSelected.TextList, "Gross Sales$,Sales Taxable$,Sales Exempt $,Freight $,Freight Taxable$,Freight Exempt $,Tax $,Gross + Freight,Taxable Revenue") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.


{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName2).     
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

FOR EACH cust 
    WHERE cust.company EQ ipcCompany 
    NO-LOCK:
    FOR EACH ar-inv
        WHERE ar-inv.company        EQ cust.company
          AND ar-inv.cust-no        EQ cust.cust-no
          AND ar-inv.inv-date       GE ipdtBegin
          AND ar-inv.inv-date       LE ipdtEnd
          AND ar-inv.tax-code       NE ''
          AND ar-inv.posted         
        USE-INDEX inv-date NO-LOCK:

        {custom/statusMsg.i " 'Processing Tax Code:  '  + string(ar-inv.tax-code) "}

        CREATE ttRawData.
        ASSIGN
            ttRawData.riInvoice = ROWID(ar-inv)
            ttRawData.cTaxGroup = ar-inv.tax-code
            .
    END. /*each ar-inv*/
    FOR EACH ar-cash
        WHERE ar-cash.company    EQ cust.company
          AND ar-cash.cust-no    EQ cust.cust-no
          AND ar-cash.check-date GE ipdtBegin
          AND ar-cash.check-date LE ipdtEnd
          AND ar-cash.posted
        USE-INDEX ar-cash NO-LOCK:

        FOR EACH ar-cashl
            WHERE ar-cashl.c-no   EQ ar-cash.c-no
              AND ar-cashl.posted
              AND ar-cashl.memo
            USE-INDEX c-no NO-LOCK,
            FIRST ar-inv
                WHERE ar-inv.company  EQ ar-cashl.company
                  AND ar-inv.inv-no   EQ ar-cashl.inv-no
                  AND ar-inv.tax-code NE ''
            NO-LOCK,
            FIRST stax
                WHERE stax.company EQ ar-cashl.company
                  AND stax.tax-group  eq ar-inv.tax-code
            NO-LOCK:

            {custom/statusMsg.i " 'Processing Tax Code:  '  + string(ar-inv.tax-code) "}

            CREATE ttRawData.
            ASSIGN
                ttRawData.riCashReceipt = ROWID(ar-cashl)
                ttRawData.cTaxGroup = ar-inv.tax-code
                ttRawData.cAccount = stax.tax-acc1[1]
                .
        END.  /*each ar-cashl*/
    END.  /*each ar-cash*/
END. /*each cust*/

VIEW FRAME r-top.

FOR EACH stax
    WHERE stax.company EQ ipcCompany
    NO-LOCK
    BY stax.tax-group:

    ASSIGN
        dRateTotal = 0
        dRateFreightTotal = 0
        dAmountSales = 0
        dAmountFreight = 0
        dAmountSalesTaxable = 0
        dAmountFreightTaxable = 0
        dAmountTax = 0
        cTaxDescription = ''.

    FOR EACH ttRawData
        WHERE ttRawData.cTaxGroup EQ stax.tax-group
        BREAK BY ttRawData.cTaxGroup:

        {custom/statusMsg.i " 'Processing Tax Code:  '  + string(ttRawData.cTaxGroup) "}

        IF FIRST-OF(ttRawData.cTaxGroup) THEN DO:
            DO iLevel = 1 to EXTENT(stax.tax-code1):
                IF stax.tax-code1[iLevel] EQ stax.tax-group THEN DO:
                    cTaxDescription = stax.tax-dscr1[iLevel].
                END. /* primary tax code line */
                dRateTotal = dRateTotal + stax.tax-rate1[iLevel].
                IF stax.tax-frt1[iLevel] THEN 
                    dRateFreightTotal = dRateFreightTotal + stax.tax-rate1[iLevel].
            END. /* iLevel = 1 to extent of stax*/
            IF cTaxDescription EQ '' THEN
                cTaxDescription = stax.tax-dscr1[1].
        END.  /*first of ttRawData.cTaxGroup*/
        FIND FIRST ar-inv 
            WHERE ROWID(ar-inv) EQ ttRawData.riInvoice
            NO-LOCK NO-ERROR.
        IF AVAIL ar-inv THEN DO:
            FOR EACH ar-invl
                WHERE ar-invl.company       EQ ar-inv.company
                  AND ar-invl.cust-no       EQ ar-inv.cust-no
                  AND ar-invl.inv-no        EQ ar-inv.inv-no
                  AND ar-invl.posted
                NO-LOCK:
                dAmountSales = dAmountSales + ar-invl.amt.
                IF ar-invl.tax THEN do: 
                    dTexRate = 0 .  
                    DO i = 1 to 5:
                        if stax.tax-code1[i] ne "" then do:
                            assign
                                dTexRate       = dTexRate +  stax.tax-rate1[i]  .
                        END.
                    END.
                    IF dTexRate GT 0 THEN
                    dAmountSalesTaxable = dAmountSalesTaxable + ar-invl.amt.
                END.
            END. /*each ar-invl*/
            ASSIGN
                dAmountFreight = dAmountFreight + 
                     (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                dAmountFreightTaxable = dAmountFreightTaxable + 
                    (IF ar-inv.f-bill AND dRateFreightTotal GT 0 THEN ar-inv.freight ELSE 0)
                dAmountTax = dAmountTax + ar-inv.tax-amt
                .
        END.  /*avail ar-inv*/
        ELSE DO: /*cash receipts*/ 
            FIND ar-cashl 
                WHERE ROWID(ar-cashl) EQ ttRawData.riCashReceipt
                NO-LOCK NO-ERROR.
            IF AVAIL ar-cashl THEN DO:
                dAmountSales = dAmountSales + (ar-cashl.amt-paid - ar-cashl.amt-disc). 
                IF ar-cashl.actnum EQ stax.tax-acc1[1] THEN
                     dAmountSalesTaxable = dAmountSalesTaxable + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END. /*avail ar-cashl*/
        END. /*not avail ar-inv*/                                                                             
        IF LAST-OF(ttRawData.cTaxGroup) THEN DO:            
             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "tax-code"    THEN cVarValue = string(ttRawData.cTaxGroup,"x(3)") .
                         WHEN "tax-name"   THEN cVarValue = string(cTaxDescription,"x(20)").
                         WHEN "tax-rat"   THEN cVarValue = STRING(dRateTotal,"->>>9.99").
                         WHEN "gro-sal"  THEN cVarValue = STRING(dAmountSales,"->>,>>>,>>9.99") .
                         WHEN "sal-tax"   THEN cVarValue = STRING(dAmountSalesTaxable,"->>,>>>,>>9.99") .
                         WHEN "sal-exe"  THEN cVarValue = STRING(dAmountSales - dAmountSalesTaxable,"->>,>>>,>>9.99") .
                         WHEN "fright"   THEN cVarValue = STRING(dAmountFreight,"->>,>>>,>>9.99") .
                         WHEN "fri-tax"  THEN cVarValue = STRING(dAmountFreightTaxable,"->>>>,>>>,>>9.99") .
                         WHEN "fri-exe"   THEN cVarValue = STRING(dAmountFreight - dAmountFreightTaxable,"->>>>,>>>,>>9.99") .
                         WHEN "tax"  THEN cVarValue = STRING(dAmountTax,"->>,>>>,>>9.99") .
                         WHEN "gross-freight"  THEN cVarValue = STRING(dAmountSales + dAmountFreight,"->>,>>>,>>9.99") .
                         WHEN "tax-revenue"  THEN cVarValue = STRING(dAmountSalesTaxable + dAmountFreightTaxable,"->>,>>>,>>9.99") .

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

            ASSIGN
                dAmountSalesTotal = dAmountSalesTotal + dAmountSales
                dAmountSalesTaxableTotal = dAmountSalesTaxableTotal + dAmountSalesTaxable
                dAmountFreightTotal = dAmountFreightTotal + dAmountFreight
                dAmountFreightTaxableTotal = dAmountFreightTaxableTotal + dAmountFreightTaxable
                dAmountTaxTotal = dAmountTaxTotal + dAmountTax
                .

        END. /*last of cTaxGroup*/
    END.  /*each ttRawData*/
END.  /* for each stax */  

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "tax-code"    THEN cVarValue = "" .
                         WHEN "tax-name"   THEN cVarValue = "".
                         WHEN "tax-rat"   THEN cVarValue = "".
                         WHEN "gro-sal"  THEN cVarValue = STRING(dAmountSalesTotal,"->>,>>>,>>9.99") .
                         WHEN "sal-tax"   THEN cVarValue = STRING(dAmountSalesTaxableTotal,"->>,>>>,>>9.99") .
                         WHEN "sal-exe"  THEN cVarValue = STRING(dAmountSalesTotal - dAmountSalesTaxableTotal,"->>,>>>,>>9.99") .
                         WHEN "fright"   THEN cVarValue = STRING(dAmountFreightTotal,"->>,>>>,>>9.99") .
                         WHEN "fri-tax"  THEN cVarValue = STRING(dAmountFreightTaxableTotal,"->>>>,>>>,>>9.99") .
                         WHEN "fri-exe"   THEN cVarValue = STRING(dAmountFreightTotal - dAmountFreightTaxableTotal,"->>>>,>>>,>>9.99") .
                         WHEN "tax"  THEN cVarValue = STRING(dAmountTaxTotal,"->>,>>>,>>9.99") .
                         WHEN "gross-freight"  THEN cVarValue = STRING(dAmountSalesTotal + dAmountFreightTotal,"->>,>>>,>>9.99") .
                         WHEN "tax-revenue"  THEN cVarValue = STRING(dAmountSalesTaxableTotal + dAmountFreightTaxableTotal,"->>,>>>,>>9.99") .

                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
           PUT str-line SKIP .
            PUT UNFORMATTED "      TOTALS: " substring(cDisplay,15,250) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' TOTALS: ,'
                       substring(cExcelDisplay,4,250) SKIP.
             END.

EMPTY TEMP-TABLE ttRawData.

IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName2)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunReport C-Win 
PROCEDURE RunReport :
/*------------------------------------------------------------------------------
  Purpose:  Replace the old run-report with a report that works    
  Parameters:  period, begin date and end date
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtBegin AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipdtEnd AS DATE NO-UNDO.

DEFINE VARIABLE cTaxDescription AS CHARACTER FORMAT "x(24)" NO-UNDO.
DEFINE VARIABLE dAmountSales AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreight AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountSalesTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreightTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountSalesTaxable AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreightTaxable AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountSalesTaxableTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountFreightTaxableTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountTax AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountTaxTotal AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dAmountTaxSub AS DECIMAL FORMAT "->>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE dRateTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dRateFreightTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iLevel AS INTEGER     NO-UNDO.

/*{sys/form/r-top3w.f}*/


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

{sys/form/r-top5DL3.f} 
cSelectedList = sl_selected:LIST-ITEMS IN FRAME {&FRAME-NAME}.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dTexRate AS DECIMAL NO-UNDO .
DEFINE VARIABLE cFileName2 LIKE fi_file NO-UNDO .
DEFINE VARIABLE v-t-price AS DECIMAL NO-UNDO.

RUN sys/ref/ExcelNameExt.p (INPUT fi_file,OUTPUT cFileName2) . 

ASSIGN
    str-tit2 = c-win:TITLE
    {sys/inc/ctrtext.i str-tit2 112}
    str-tit3 = "(" + STRING(ipdtBegin) + "-" + STRING(ipdtEnd) + ")"
    {sys/inc/ctrtext.i str-tit3 132}. 

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

        IF LOOKUP(ttRptSelected.TextList, "Tax Rate,Gross Sales$,Sales Taxable$,Sales Exempt $,Freight $,Freight Taxable$,Freight Exempt $,Tax $,Gross + Freight,Taxable Revenue") <> 0    THEN
         ASSIGN
         str-line = str-line + FILL("-",ttRptSelected.FieldLength) + " " .
        ELSE
         str-line = str-line + FILL(" ",ttRptSelected.FieldLength) + " " . 
 END.


{sys/inc/print1.i}

{sys/inc/outprint.i VALUE(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(cFileName2).
  /*excelheader = "Tax Code,Tax Name,Tax Rate,Gross Sales $,"
              + "Sales Taxable $,Sales Exempt $,Freight $,Freight Taxable $,Freight Exempt $,Tax $".*/
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

IF td-show-parm THEN RUN show-param.

SESSION:SET-WAIT-STATE ("general").

FOR EACH cust 
    WHERE cust.company EQ ipcCompany 
    NO-LOCK:
    FOR EACH ar-inv
        WHERE ar-inv.company        EQ cust.company
          AND ar-inv.cust-no        EQ cust.cust-no
          AND ar-inv.inv-date       GE ipdtBegin
          AND ar-inv.inv-date       LE ipdtEnd
          AND ar-inv.tax-code       NE ''
          AND ar-inv.posted         
        USE-INDEX inv-date NO-LOCK:

        {custom/statusMsg.i " 'Processing Tax Code:  '  + string(ar-inv.tax-code) "}

        CREATE ttRawData.
        ASSIGN
            ttRawData.riInvoice = ROWID(ar-inv)
            ttRawData.cTaxGroup = ar-inv.tax-code
            .
    END. /*each ar-inv*/
    FOR EACH ar-cash
        WHERE ar-cash.company    EQ cust.company
          AND ar-cash.cust-no    EQ cust.cust-no
          AND ar-cash.check-date GE ipdtBegin
          AND ar-cash.check-date LE ipdtEnd
          AND ar-cash.posted
        USE-INDEX ar-cash NO-LOCK:

        FOR EACH ar-cashl
            WHERE ar-cashl.c-no   EQ ar-cash.c-no
              AND ar-cashl.posted
              AND ar-cashl.memo
            USE-INDEX c-no NO-LOCK,
            FIRST ar-inv
                WHERE ar-inv.company  EQ ar-cashl.company
                  AND ar-inv.inv-no   EQ ar-cashl.inv-no
                  AND ar-inv.tax-code NE ''
            NO-LOCK,
            FIRST stax
                WHERE stax.company EQ ar-cashl.company
                  AND stax.tax-group  eq ar-inv.tax-code
            NO-LOCK:

            {custom/statusMsg.i " 'Processing Tax Code:  '  + string(ar-inv.tax-code) "}

            CREATE ttRawData.
            ASSIGN
                ttRawData.riCashReceipt = ROWID(ar-cashl)
                ttRawData.cTaxGroup = ar-inv.tax-code
                ttRawData.cAccount = stax.tax-acc1[1]
                .
        END.  /*each ar-cashl*/
    END.  /*each ar-cash*/
END. /*each cust*/

VIEW FRAME r-top.

FOR EACH stax
    WHERE stax.company EQ ipcCompany
    NO-LOCK
    BY stax.tax-group:

    ASSIGN
        dRateTotal = 0
        dRateFreightTotal = 0
        dAmountSales = 0
        dAmountFreight = 0
        dAmountSalesTaxable = 0
        dAmountFreightTaxable = 0
        dAmountTax = 0
        cTaxDescription = ''.

    FOR EACH ttRawData
        WHERE ttRawData.cTaxGroup EQ stax.tax-group
        BREAK BY ttRawData.cTaxGroup:

        {custom/statusMsg.i " 'Processing Tax Code:  '  + string(ttRawData.cTaxGroup) "}
        
         FIND FIRST ar-inv 
            WHERE ROWID(ar-inv) EQ ttRawData.riInvoice
            NO-LOCK NO-ERROR.  
        IF AVAIL ar-inv THEN DO:
            FOR EACH ar-invl
                WHERE ar-invl.company       EQ ar-inv.company
                  AND ar-invl.cust-no       EQ ar-inv.cust-no
                  AND ar-invl.inv-no        EQ ar-inv.inv-no
                  AND ar-invl.posted
                NO-LOCK:
                dAmountSales = dAmountSales + ar-invl.amt.
                IF ar-invl.tax THEN do: 
                    dTexRate = 0 .  
                    DO i = 1 to 5:
                        if stax.tax-code1[i] ne "" then do:
                            assign
                                dTexRate       = dTexRate +  stax.tax-rate1[i]  .
                        END.
                    END.
                    IF dTexRate GT 0 THEN
                    dAmountSalesTaxable = dAmountSalesTaxable + ar-invl.amt.
                END.
            END. /*each ar-invl*/
            ASSIGN
                dAmountFreight = dAmountFreight + 
                     (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                dAmountFreightTaxable = dAmountFreightTaxable + 
                    (IF ar-inv.f-bill AND dRateFreightTotal GT 0 THEN ar-inv.freight ELSE 0)
                dAmountTax = dAmountTax + ar-inv.tax-amt
                .
        END.  /*avail ar-inv*/
        ELSE DO: /*cash receipts*/ 
            FIND ar-cashl 
                WHERE ROWID(ar-cashl) EQ ttRawData.riCashReceipt
                NO-LOCK NO-ERROR.
            IF AVAIL ar-cashl THEN DO:
                dAmountSales = dAmountSales + (ar-cashl.amt-paid - ar-cashl.amt-disc). 
                IF ar-cashl.actnum EQ stax.tax-acc1[1] THEN
                     dAmountSalesTaxable = dAmountSalesTaxable + (ar-cashl.amt-paid - ar-cashl.amt-disc).
            END. /*avail ar-cashl*/
        END. /*not avail ar-inv*/ 

        IF LAST-OF(ttRawData.cTaxGroup) THEN DO:  
            v-t-price = dAmountSalesTaxable.
            DO iLevel = 1 to EXTENT(stax.tax-code1):
              IF stax.tax-code1[iLevel] NE "" THEN do:
                dAmountTaxSub      = round((if stax.accum-tax EQ YES then v-t-price
                                                                  else dAmountSalesTaxable) *
                                        stax.tax-rate1[iLevel] / 100,2).
                v-t-price  = v-t-price + dAmountTaxSub .
                cTaxDescription = stax.tax-dscr1[iLevel].
                dRateTotal = dRateTotal + stax.tax-rate1[iLevel] .
                
                 ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "tax-code"    THEN cVarValue = string(stax.tax-code1[iLevel],"x(3)") .
                         WHEN "tax-name"   THEN cVarValue = string(cTaxDescription,"x(20)").
                         WHEN "tax-rat"   THEN cVarValue = STRING(stax.tax-rate1[iLevel],"->>>9.99").
                         WHEN "gro-sal"  THEN cVarValue = STRING(dAmountSales,"->>,>>>,>>9.99") .
                         WHEN "sal-tax"   THEN cVarValue = STRING(dAmountSalesTaxable,"->>,>>>,>>9.99") .
                         WHEN "sal-exe"  THEN cVarValue = STRING(dAmountSales - dAmountSalesTaxable,"->>,>>>,>>9.99") .
                         WHEN "fright"   THEN cVarValue = STRING(dAmountFreight,"->>,>>>,>>9.99") .
                         WHEN "fri-tax"  THEN cVarValue = STRING(dAmountFreightTaxable,"->>>>,>>>,>>9.99") .
                         WHEN "fri-exe"   THEN cVarValue = STRING(dAmountFreight - dAmountFreightTaxable,"->>>>,>>>,>>9.99") .
                         WHEN "tax"  THEN cVarValue = STRING(dAmountTaxSub,"->>,>>>,>>9.99") .
                         WHEN "gross-freight"  THEN cVarValue = STRING(dAmountSales + dAmountFreight,"->>,>>>,>>9.99") .
                         WHEN "tax-revenue"  THEN cVarValue = STRING(dAmountSalesTaxable + dAmountFreightTaxable,"->>,>>>,>>9.99") .
                         
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
                
             END.
             
            END. /* iLevel = 1 to extent of stax*/
            IF cTaxDescription EQ '' THEN
                cTaxDescription = stax.tax-dscr1[1]. 
        END.  /*first of ttRawData.cTaxGroup*/
                                                                                   
        IF LAST-OF(ttRawData.cTaxGroup) THEN DO:  
        
             ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "tax-code"    THEN cVarValue = "".
                         WHEN "tax-name"   THEN cVarValue = "".
                         WHEN "tax-rat"   THEN cVarValue = STRING(dRateTotal,"->>>9.99").
                         WHEN "gro-sal"  THEN cVarValue = STRING(dAmountSales,"->>,>>>,>>9.99") .
                         WHEN "sal-tax"   THEN cVarValue = STRING(dAmountSalesTaxable,"->>,>>>,>>9.99") .
                         WHEN "sal-exe"  THEN cVarValue = STRING(dAmountSales - dAmountSalesTaxable,"->>,>>>,>>9.99") .
                         WHEN "fright"   THEN cVarValue = STRING(dAmountFreight,"->>,>>>,>>9.99") .
                         WHEN "fri-tax"  THEN cVarValue = STRING(dAmountFreightTaxable,"->>>>,>>>,>>9.99") .
                         WHEN "fri-exe"   THEN cVarValue = STRING(dAmountFreight - dAmountFreightTaxable,"->>>>,>>>,>>9.99") .
                         WHEN "tax"  THEN cVarValue = STRING(dAmountTax,"->>,>>>,>>9.99") .
                         WHEN "gross-freight"  THEN cVarValue = STRING(dAmountSales + dAmountFreight,"->>,>>>,>>9.99") .
                         WHEN "tax-revenue"  THEN cVarValue = STRING(dAmountSalesTaxable + dAmountFreightTaxable,"->>,>>>,>>9.99") .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.

            /*PUT UNFORMATTED cDisplay SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP(1).
             END.*/
             PUT str-line SKIP .
            PUT UNFORMATTED "      TOTALS " ttRawData.cTaxGroup FORMAT "x(3)" ":" substring(cDisplay,18,250) SKIP(1).
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  ' TOTALS ' ttRawData.cTaxGroup ': ,'
                       substring(cExcelDisplay,4,250) SKIP(1).
             END.
             IF tb_page-break THEN
              PAGE.

            ASSIGN
                dAmountSalesTotal = dAmountSalesTotal + dAmountSales
                dAmountSalesTaxableTotal = dAmountSalesTaxableTotal + dAmountSalesTaxable
                dAmountFreightTotal = dAmountFreightTotal + dAmountFreight
                dAmountFreightTaxableTotal = dAmountFreightTaxableTotal + dAmountFreightTaxable
                dAmountTaxTotal = dAmountTaxTotal + dAmountTax
                .

        END. /*last of cTaxGroup*/
    END.  /*each ttRawData*/
END.  /* for each stax */   


            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".

            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "tax-code"    THEN cVarValue = "" .
                         WHEN "tax-name"   THEN cVarValue = "".
                         WHEN "tax-rat"   THEN cVarValue = "".
                         WHEN "gro-sal"  THEN cVarValue = STRING(dAmountSalesTotal,"->>,>>>,>>9.99") .
                         WHEN "sal-tax"   THEN cVarValue = STRING(dAmountSalesTaxableTotal,"->>,>>>,>>9.99") .
                         WHEN "sal-exe"  THEN cVarValue = STRING(dAmountSalesTotal - dAmountSalesTaxableTotal,"->>,>>>,>>9.99") .
                         WHEN "fright"   THEN cVarValue = STRING(dAmountFreightTotal,"->>,>>>,>>9.99") .
                         WHEN "fri-tax"  THEN cVarValue = STRING(dAmountFreightTaxableTotal,"->>>>,>>>,>>9.99") .
                         WHEN "fri-exe"   THEN cVarValue = STRING(dAmountFreightTotal - dAmountFreightTaxableTotal,"->>>>,>>>,>>9.99") .
                         WHEN "tax"  THEN cVarValue = STRING(dAmountTaxTotal,"->>,>>>,>>9.99") .
                         WHEN "gross-freight"  THEN cVarValue = STRING(dAmountSalesTotal + dAmountFreightTotal,"->>,>>>,>>9.99")  .
                         WHEN "tax-revenue"  THEN cVarValue = STRING(dAmountSalesTaxableTotal + dAmountFreightTaxableTotal,"->>,>>>,>>9.99") .
                    END CASE.

                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            IF NOT tb_page-break THEN 
               PUT str-line SKIP .
            PUT UNFORMATTED " GRAND TOTALS: " substring(cDisplay,16,250) SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  'GRAND TOTALS: ,'
                       substring(cExcelDisplay,4,250) SKIP.
             END.

EMPTY TEMP-TABLE ttRawData.

IF tb_excel THEN DO:
    OUTPUT STREAM excel CLOSE.
    IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(cFileName2)).
END.

RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-period-dates C-Win 
PROCEDURE show-period-dates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST period
        WHERE period.company EQ gcompany
          AND period.yr      EQ INT(begin_year:SCREEN-VALUE)
          AND period.pnum    EQ INT(begin_period:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

    IF AVAIL period THEN
      ASSIGN
       begin_date:SCREEN-VALUE = STRING(period.pst)
       end_date:SCREEN-VALUE   = STRING(IF period.pend LT TODAY THEN period.pend
                                                                ELSE TODAY).
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

